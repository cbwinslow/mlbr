# validate_lake.R - read-only integrity audit of data/raw/
# Reports: inventory, row counts, exact-cap detection, date gaps in statcast,
# retrosheet per-year play/game counts vs known history, coverage of every
# other source. Run any time: Rscript R/validate_lake.R
suppressPackageStartupMessages({ library(arrow); library(dplyr); library(purrr) })
options(dplyr.summarise.inform = FALSE)

cat("================ LAKE AUDIT ", format(Sys.time()), " ================\n")
for (d in list.dirs("data/raw", recursive = FALSE)) {
  files <- list.files(d, pattern = "parquet$", recursive = TRUE, full.names = TRUE)
  cat(sprintf("%-45s %4d files  %8.1f MB\n", d, length(files),
              sum(file.size(files)) / 1e6))
}

## ---- statcast: per-file integrity ------------------------------------------
cat("\n== STATCAST integrity ==\n")
sc <- tibble(file = list.files("data/raw/statcast", pattern = "parquet$",
                               full.names = TRUE))
if (nrow(sc)) {
  sc <- sc |>
    mutate(data  = map(file, read_parquet, col_select = c(game_date, game_pk))) |>
    mutate(rows  = map_int(data, nrow),
           ndays = map_int(data, ~ length(unique(.x$game_date))),
           mind  = map_chr(data, ~ format(min(.x$game_date))),
           maxd  = map_chr(data, ~ format(max(.x$game_date))),
           dates = map(data, ~ format(unique(.x$game_date)))) |>
    select(-data) |>
    mutate(month = gsub("statcast-(\\d{4}-\\d{2}).parquet", "\\1", basename(file)),
           exact_cap = rows == 25000)
  # calendar coverage: expected ~4,500 pitches/day in season; flag short files
  season_core <- function(m) { y <- substr(m, 1, 4); mo <- as.integer(substr(m, 6, 7)); mo >= 4 && mo <= 9 }
  sc <- sc |>
    mutate(core = sapply(month, season_core),
           flag = case_when(
             exact_cap                       ~ "EXACT-25K-CAP",
             core & rows < 100000            ~ "SUSPECT-SHORT",
             month >= "2021-04" & rows < 50000 & month <= "2025-09" ~ "SUSPECT-SHORT",
             TRUE                            ~ "ok"))
  print(sc |> select(month, rows, ndays, mind, maxd, flag) |> arrange(month), n = 60)
  cat("\nsummary: ", sum(sc$flag != "ok"), "of", nrow(sc), "files flagged\n")
  print(sc |> filter(flag != "ok") |> select(month, rows, ndays, flag))
  # day-gap check: any month file missing interior days?
  gaps <- sc |> filter(ndays > 1) |>
    mutate(gap_days = pmap_int(list(mind, maxd, dates), function(a, b, d) {
      length(setdiff(format(seq(as.Date(a), as.Date(b), by = "day")), d))
    }))
  if (any(gaps$gap_days > 0)) {
    cat("DAY GAPS detected (interior missing days):\n")
    print(gaps |> filter(gap_days > 0) |> select(month, ndays, gap_days))
  } else cat("no interior day gaps detected\n")
}

## ---- retrosheet: per-year per-play coverage --------------------------------
cat("\n== RETROSHEET (github mirror) ==\n")
rs_files <- list.files("data/raw/retrosheet", pattern = "^plays-\\d{4}\\.parquet$",
                       full.names = TRUE)
if (length(rs_files)) {
  yrs <- as.integer(sub(".*-(\\d{4})\\.parquet$", "\\1", basename(rs_files)))
  rs <- tibble(file = rs_files, year = yrs) |>
    mutate(data = map(file, read_parquet, col_select = GAME_ID)) |>
    mutate(plays = map_int(data, nrow),
           games = map_int(data, ~ length(unique(.x$GAME_ID)))) |>
    select(-data) |>
    arrange(year)
  print(rs, n = 100)
  # sanity anchors: known totals (plays for recent full seasons)
  anchors <- c("2024" = 192384)
  for (y in names(anchors)) {
    got <- rs$plays[rs$year == as.integer(y)]
    if (length(got) && got != anchors[[y]])
      cat("MISMATCH", y, "expected", anchors[[y]], "got", got, "\n")
  }
  cat("retrosheet years present:", length(yrs),
      "(", min(yrs), "-", max(yrs), "); missing in range:",
      paste(setdiff(min(yrs):max(yrs), yrs), collapse = ","), "\n")
}

## ---- fangraphs: table x year coverage --------------------------------------
cat("\n== FANGRAPH coverage ==\n")
fg <- tibble(file = list.files("data/raw/fangraphs", pattern = "-\\d{4}\\.parquet$",
                               full.names = TRUE)) |>
  mutate(tbl = sub("-\\d{4}\\.parquet$", "", basename(file)),
         yr  = as.integer(sub(".*-(\\d{4})\\.parquet$", "\\1", basename(file)))) |>
  count(tbl, name = "years_present")
print(fg, n = 20)
cat("one-off ancillary files:\n")
print(setdiff(list.files("data/raw/fangraphs"),
              list.files("data/raw/fangraphs", pattern = "-\\d{4}\\.parquet$")))

## ---- espn / ncaa / mlbmeta / milb / bbref inventory ------------------------
for (src in c("espn", "ncaa", "mlbmeta", "milb", "bbref", "chadwick", "lahman")) {
  cat(sprintf("\n== %s ==\n", toupper(src)))
  fs <- list.files(file.path("data/raw", src), pattern = "parquet$",
                   full.names = TRUE)
  if (!length(fs)) { cat("  (empty)\n"); next }
  inv <- tibble(file = fs) |>
    mutate(rows = map_int(file, ~ tryCatch(nrow(read_parquet(.x, col_select = 1)),
                                           error = function(e) -1L)))
  print(inv |> mutate(file = basename(file)) |> arrange(file), n = 40)
}

## ---- lahman / chadwick sanity ----------------------------------------------
cat("\n== LAHMAN detail ==\n")
for (t in c("Batting", "Pitching", "People", "Fielding", "Teams", "Salaries")) {
  p <- sprintf("data/raw/lahman/%s.parquet", t)
  if (file.exists(p)) {
    cols <- if (t == "People") "playerID" else if (t == "Teams") c("teamID", "yearID") else c("playerID", "yearID")
    d <- read_parquet(p, col_select = any_of(cols))
    y_min <- if ("yearID" %in% names(d)) min(d$yearID, na.rm = TRUE) else "N/A"
    y_max <- if ("yearID" %in% names(d)) max(d$yearID, na.rm = TRUE) else "N/A"
    cat(sprintf("  %-10s %8d rows  years %s-%s\n", t, nrow(d), y_min, y_max))
  } else cat("  ", t, "MISSING\n")
}
cat("\n== CHADWICK ==\n")
lu <- read_parquet("data/raw/chadwick/register.parquet")
cat(sprintf("  register: %d players; non-NA key_mlbam=%d key_retro=%d key_fangraphs=%d\n",
            nrow(lu), sum(!is.na(lu$key_mlbam)), sum(!is.na(lu$key_retro)),
            sum(!is.na(lu$key_fangraphs))))
cat("audit done\n")
