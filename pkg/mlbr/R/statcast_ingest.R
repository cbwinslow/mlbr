# statcast_ingest.R - MLB Statcast per-pitch
#
# Savant's search endpoint caps a single request at 25,000 rows. In-season
# months exceed it, so this walks a tier ladder: month -> week -> day. A DAY
# is always far below the cap (~4,500 pitches), so the ladder guarantees
# completeness. Every chunk is trimmed to its requested [start,end] window
# before binding, so month files never bleed into neighbors or duplicate rows.
#
# Output: <lake>/data/raw/statcast/statcast-<year>-<month>.parquet
#         one row per pitch (the finest granularity that exists)

#' Ingest MLB Statcast per-pitch
#'
#' @rdname statcast_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @param months integer months to walk each year (default Mar-Nov)
#' @return invisible TRUE; writes parquet files
#' @export
statcast_ingest <- function(lake, from = as.integer(format(Sys.Date(), "%Y")) - 1L,
                            to = as.integer(format(Sys.Date(), "%Y")), months = 3:11) {
  lake <- default_lake(lake)
  SAVANT_CAP <- 25000L

  fetch <- function(start, end) {
    d <- retry(3, 15, baseballr::scrape_statcast_savant(start_date = start,
                                                       end_date = end))
    dplyr::filter(d, game_date >= start, game_date <= end)  # enforce exact window
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
      throttle(2)                              # politeness between requests
    }
    dplyr::bind_rows(parts)
  }

  for (yr in from:to) for (m in months) {
    start <- as.Date(sprintf("%d-%02d-01", yr, m))
    end   <- as.Date(sprintf("%d-%02d-01", yr, m + 1L)) - 1L
    p <- lake_raw(lake, "statcast", sprintf("statcast-%d-%02d.parquet", yr, m))
    if (end < start || file.exists(p)) next
    tryCatch({
      throttle(1)
      d <- fetch_range(start, end)
      if (nrow(d) > 0) {
        got <- sort(unique(d$game_date))
        # only a gap BETWEEN covered days is suspicious (off-days are fine)
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
    throttle(5)
  }
  invisible(TRUE)
}
