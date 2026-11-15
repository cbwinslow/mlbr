# statcast_ingest.R - MLB Statcast per-pitch
#
# Savant search endpoint caps a single request at 25,000 rows. In-season
# months exceed it, so this walks a tier ladder: month -> week -> day. A DAY
# is always far below the cap (~4,500 pitches), so the ladder guarantees
# completeness. Every chunk is trimmed to its requested [start,end] window
# before binding, so month files never bleed into neighbors or duplicate rows.
#
# Output: <lake>/data/raw/statcast/statcast-<year>-<month>.parquet
#         one row per pitch (the finest granularity that exists)

harmonize_types <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  # Coerce all-NA logical columns to character to prevent bind_rows type clash
  for (col in names(df)) {
    if (is.logical(df[[col]]) && all(is.na(df[[col]]))) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  if ("bat_speed" %in% names(df)) df$bat_speed <- suppressWarnings(as.numeric(df$bat_speed))
  if ("swing_length" %in% names(df)) df$swing_length <- suppressWarnings(as.numeric(df$swing_length))
  df
}

robust_bind <- function(parts) {
  if (length(parts) == 0) return(tibble::tibble())
  if (length(parts) == 1) return(parts[[1]])
  
  all_cols <- unique(unlist(lapply(parts, names)))
  for (col in all_cols) {
    types <- unique(vapply(parts, function(p) {
      if (col %in% names(p)) class(p[[col]])[1] else NA_character_
    }, character(1)))
    types <- setdiff(types, NA_character_)
    if (length(types) > 1) {
      if (any(types %in% c("numeric", "double", "integer"))) {
        for (i in seq_along(parts)) {
          if (col %in% names(parts[[i]])) {
            parts[[i]][[col]] <- suppressWarnings(as.numeric(parts[[i]][[col]]))
          }
        }
      } else {
        for (i in seq_along(parts)) {
          if (col %in% names(parts[[i]])) {
            parts[[i]][[col]] <- as.character(parts[[i]][[col]])
          }
        }
      }
    }
  }
  dplyr::bind_rows(parts)
}

#' Ingest MLB Statcast per-pitch
#'
#' @rdname statcast_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive, default: 2008 to 2026)
#' @param months integer months to walk each year (default Mar-Nov)
#' @return invisible TRUE; writes parquet files
#' @export
statcast_ingest <- function(lake = NULL, from = 2008L, to = 2026L, months = 3:11) {
  lake <- default_lake(lake)
  SAVANT_CAP <- 25000L

  fetch <- function(start, end) {
    d <- retry(3, 10, baseballr::scrape_statcast_savant(start_date = start,
                                                       end_date = end))
    if (is.null(d) || nrow(d) == 0) return(tibble::tibble())
    d <- dplyr::filter(d, game_date >= start, game_date <= end)  # enforce exact window
    harmonize_types(d)
  }

  # tier ladder: try progressively finer windows until each one is uncapped
  fetch_range <- function(start, end, depth = "month") {
    d <- tryCatch(fetch(start, end), error = function(e) NULL)
    if (is.null(d)) stop("fetch failed at ", depth, " level for ", start, "..", end)
    if (nrow(d) < SAVANT_CAP) return(d)
    message("    ", depth, " ", start, "..", end, " hit cap (", nrow(d), ") -> going finer")
    parts <- list()
    cur <- start
    step <- if (depth == "month") 6L else 0L   # month -> 6d windows -> days
    while (cur <= end) {
      wend <- min(cur + step, end)
      parts[[length(parts) + 1L]] <-
        if (depth == "month") fetch_range(cur, wend, "week") else fetch(cur, wend)
      cur <- wend + 1L
      throttle(0.8)                              # politeness between requests
    }
    parts <- lapply(parts, harmonize_types)
    robust_bind(parts)
  }

  out_dir <- file.path(lake, "data/raw/statcast")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (yr in from:to) for (m in months) {
    start <- as.Date(sprintf("%d-%02d-01", yr, m))
    if (yr == 2008L && m == 3L) start <- as.Date("2008-03-25")
    end   <- as.Date(sprintf("%d-%02d-01", yr, m + 1L)) - 1L
    p <- file.path(out_dir, sprintf("statcast-%d-%02d.parquet", yr, m))
    if (end < start || file.exists(p)) next
    tryCatch({
      throttle(0.5)
      d <- fetch_range(start, end)
      if (nrow(d) > 0) {
        got <- sort(unique(d$game_date))
        if (length(got)) {
          holes <- got[which(c(0, diff(as.integer(got))) > 1)[-1] - 1]
          holes <- holes[holes > min(got) & holes < max(got)]
          if (length(holes))
            warning("statcast ", yr, "-", sprintf("%02d", m),
                    ": possible day gaps between ", min(holes), " and ",
                    max(holes), " - inspect before trusting")
        }
        write_df(d, p)
        message("  ", yr, "-", sprintf("%02d", m), ": ", nrow(d), " pitches, ",
                length(got), " game days")
      } else message("  empty ", yr, "-", m)
    }, error = function(e) message("statcast: FAILED ", yr, "-", m, " - ",
                                   substr(conditionMessage(e), 1, 90)))
    throttle(1.5)
  }
  invisible(TRUE)
}
