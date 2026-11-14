# lake_status.R - user-facing progress/status utilities

#' quick inventory of the data lake
#'
#' Lightweight - reads parquet footers only, no full scans. Run any time to
#' see ingest progress.
#'
#' @param lake project root of the data lake
#' @return a tibble: source, files, rows, MB
#' @export
lake_status <- function(lake) {
  lake <- default_lake(lake)
  dirs <- list.dirs(file.path(lake, "data", "raw"), recursive = FALSE)
  out <- lapply(dirs, function(d) {
    fs <- list.files(d, pattern = "parquet$", recursive = TRUE, full.names = TRUE)
    if (!length(fs)) return(data.frame(
      source = basename(d), files = 0L, rows = 0L, MB = 0))
    # lazy footer-only row counts (arrow exposes num_rows as an active binding)
    rows <- tryCatch(
      sum(vapply(fs, function(f)
        tryCatch(as.integer(arrow::open_dataset(f)$num_rows), error = function(e) -1L),
        integer(1), USE.NAMES = FALSE)), na.rm = TRUE)
    data.frame(source = basename(d), files = length(fs), rows = as.integer(rows),
               MB = round(sum(file.size(fs)) / 1e6, 1))
  })
  res <- do.call(rbind, out)
  res <- res[order(-res$MB), ]
  rownames(res) <- NULL
  cat(sprintf("%-16s %6s %14s %10s\n", "source", "files", "rows", "MB"))
  for (i in seq_len(nrow(res)))
    cat(sprintf("%-16s %6d %14s %10.1f\n", res$source[i], res$files[i],
                format(res$rows[i], big.mark = ","), res$MB[i]))
  invisible(res)
}

#' full integrity audit of the data lake (read-only)
#'
#' Checks exact-cap hits and short files in Statcast, interior day gaps,
#' retrosheet per-year play/game counts, and inventory of every source.
#' Read-only; safe to run any time.
#'
#' @param lake project root of the data lake
#' @return invisibly the statcast audit tibble (or NULL)
#' @export
validate_lake <- function(lake) {
  lake <- default_lake(lake)
  cat("================ LAKE AUDIT ", format(Sys.time()), " ================\n")
  for (d in list.dirs(file.path(lake, "data", "raw"), recursive = FALSE)) {
    files <- list.files(d, pattern = "parquet$", recursive = TRUE, full.names = TRUE)
    cat(sprintf("%-45s %4d files  %8.1f MB\n", d, length(files),
                sum(file.size(files)) / 1e6))
  }

  ## ---- statcast: per-file integrity ----
  cat("\n== STATCAST integrity ==\n")
  sc <- tibble::tibble(file = list.files(file.path(lake, "data/raw/statcast"),
                                         pattern = "parquet$", full.names = TRUE))
  if (nrow(sc)) {
    sc <- sc |>
      dplyr::mutate(data = purrr::map(file, arrow::read_parquet,
                                      col_select = c(game_date, game_pk))) |>
      dplyr::mutate(rows  = purrr::map_int(data, nrow),
                    ndays = purrr::map_int(data, ~ length(unique(.x$game_date))),
                    mind  = purrr::map_chr(data, ~ format(min(.x$game_date))),
                    maxd  = purrr::map_chr(data, ~ format(max(.x$game_date))),
                    dates = purrr::map(data, ~ format(unique(.x$game_date)))) |>
      dplyr::select(-data) |>
      dplyr::mutate(month = gsub("statcast-(\\d{4}-\\d{2}).parquet", "\\1",
                                 basename(file)),
                    exact_cap = rows == 25000)
    season_core <- function(m) {
      y <- substr(m, 1, 4); mo <- as.integer(substr(m, 6, 7))
      mo >= 4 && mo <= 9
    }
    sc <- sc |>
      dplyr::mutate(core = sapply(month, season_core),
                    flag = dplyr::case_when(
                      exact_cap ~ "EXACT-25K-CAP",
                      core & rows < 100000 ~ "SUSPECT-SHORT",
                      month >= "2021-04" & rows < 50000 & month <= "2025-09"
                        ~ "SUSPECT-SHORT",
                      TRUE ~ "ok"))
    print(sc |>
            dplyr::select(month, rows, ndays, mind, maxd, flag) |>
            dplyr::arrange(month), n = 60)
    cat("\nsummary: ", sum(sc$flag != "ok"), "of", nrow(sc), "files flagged\n")
    gaps <- sc |>
      dplyr::filter(ndays > 1) |>
      dplyr::mutate(gap_days = purrr::pmap_int(
        list(mind, maxd, dates),
        function(a, b, d) length(setdiff(
          format(seq(as.Date(a), as.Date(b), by = "day")), d))))
    if (any(gaps$gap_days > 0)) {
      cat("DAY GAPS detected (interior missing days):\n")
      print(gaps |> dplyr::filter(gap_days > 0) |> dplyr::select(month, gap_days))
    } else cat("no interior day gaps detected\n")
  }

  ## ---- retrosheet: per-year per-play coverage ----
  cat("\n== RETROSHEET (github mirror) ==\n")
  rs_files <- list.files(file.path(lake, "data/raw/retrosheet"),
                         pattern = "^plays-\\d{4}\\.parquet$", full.names = TRUE)
  if (length(rs_files)) {
    yrs <- as.integer(sub(".*-(\\d{4})\\.parquet$", "\\1", basename(rs_files)))
    rs <- tibble::tibble(file = rs_files, year = yrs) |>
      dplyr::mutate(data = purrr::map(file, arrow::read_parquet,
                                      col_select = GAME_ID)) |>
      dplyr::mutate(plays = purrr::map_int(data, nrow),
                    games = purrr::map_int(data, ~ length(unique(.x$GAME_ID)))) |>
      dplyr::select(-data) |>
      dplyr::arrange(year)
    print(rs, n = 100)
    cat("retrosheet years present:", length(yrs), "(", min(yrs), "-", max(yrs),
        "); missing in range:",
        paste(setdiff(min(yrs):max(yrs), yrs), collapse = ","), "\n")
  }

  ## ---- inventory of everything else ----
  for (src in c("espn", "ncaa", "mlbmeta", "milb", "bbref", "chadwick",
                "lahman", "fangraphs")) {
    cat(sprintf("\n== %s ==\n", toupper(src)))
    fs <- list.files(file.path(lake, "data/raw", src), pattern = "parquet$",
                     full.names = TRUE)
    if (!length(fs)) { cat("  (empty)\n"); next }
    inv <- tibble::tibble(file = fs) |>
      dplyr::mutate(rows = purrr::map_int(file, ~ tryCatch(
        as.integer(arrow::open_dataset(.x)$num_rows), error = function(e) -1L))) |>
      dplyr::mutate(file = basename(file)) |>
      dplyr::arrange(file)
    print(inv, n = 40)
  }
  cat("audit done\n")
  invisible(if (nrow(sc)) sc else NULL)
}
