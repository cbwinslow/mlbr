# espn_ingest.R - ESPN scoreboards (MLB + NCAA) -> <lake>/data/raw/espn/
# CDN-backed public API; keep 2s spacing anyway.

#' Ingest ESPN scoreboards
#'
#' @rdname espn_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @param leagues which scoreboards to pull: college, mlb
#' @return invisible TRUE; writes espn-<league>-scoreboard-<year>.parquet
#' @export
espn_ingest <- function(lake, from = 2002L,
                        to = as.integer(format(Sys.Date(), "%Y")),
                        leagues = c("college", "mlb")) {
  lake <- default_lake(lake)
  for (yr in from:to) {
    for (nm in leagues) {
      p <- lake_raw(lake, "espn",
                    sprintf("espn-%s-scoreboard-%d.parquet", nm, yr))
      if (file.exists(p)) next
      tryCatch({
        throttle(2)
        d <- if (nm == "college")
          retry(2, 10, baseballr::espn_college_baseball_scoreboard(season = yr))
        else
          retry(2, 10, baseballr::espn_mlb_scoreboard(season = yr))
        if (!is.null(d) && nrow(d) > 0) write_df(as.data.frame(d), p)
      }, error = function(e) message("espn: FAILED ", nm, "-", yr, " - ",
                                     substr(conditionMessage(e), 1, 80)))
      throttle(2)
    }
  }
  message("espn: done")
  invisible(TRUE)
}

# bbref_ingest.R - Baseball-Reference daily player logs -> <lake>/data/raw/bbref/
# POLITE BY CONTRACT: baseball-reference.com bans aggressive scrapers fast.
# One request per table per year, 12s minimum spacing (never tighten this),
# and never run concurrently with anything else.

#' Ingest Baseball-Reference daily player logs
#'
#' @rdname bbref_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @return invisible TRUE; writes bbref-bat-<year>.parquet / bbref-pit-<year>.parquet
#'         (one row per PLAYER-DAY)
#' @export
bbref_ingest <- function(lake, from = 2015L,
                         to = as.integer(format(Sys.Date(), "%Y")) - 1L) {
  lake <- default_lake(lake)
  for (yr in from:to) {
    for (nm in c("bat", "pit")) {
      p <- lake_raw(lake, "bbref", sprintf("bbref-%s-%d.parquet", nm, yr))
      if (file.exists(p)) next
      tryCatch({
        throttle(12)
        d <- retry(2, 30, if (nm == "bat")
          baseballr::bref_daily_batter(t1 = sprintf("%d-03-01", yr),
                                       t2 = sprintf("%d-11-05", yr))
          else
            baseballr::bref_daily_pitcher(t1 = sprintf("%d-03-01", yr),
                                          t2 = sprintf("%d-11-05", yr)))
        if (!is.null(d) && nrow(d) > 0) write_df(as.data.frame(d), p)
      }, error = function(e) message("bbref: FAILED ", nm, "-", yr, " - ",
                                     substr(conditionMessage(e), 1, 80)))
      throttle(12)  # bbref is strict - never tighten this
    }
  }
  message("bbref: done")
  invisible(TRUE)
}

# milb_ingest.R - Minor League Statcast (per-pitch) -> <lake>/data/raw/milb/
# Uses Savant's minors search (same endpoint family as MLB statcast) with the
# SAME tier ladder and 25k cap handling. MUST be serialized with
# statcast_ingest (same backend) - never run both concurrently.

#' Ingest Minor League Statcast per-pitch
#'
#' @rdname milb_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @param months integer months to walk each year (default Apr-Sep)
#' @return invisible TRUE; writes milb-<year>-<month>.parquet (one row per pitch)
#' @export
milb_ingest <- function(lake, from = 2021L,
                        to = as.integer(format(Sys.Date(), "%Y")),
                        months = 4:9) {
  lake <- default_lake(lake)
  SAVANT_CAP <- 25000L

  fetch <- function(start, end) {
    d <- retry(3, 15,
               baseballr::statcast_search_minors(start_date = start,
                                                 end_date = end))
    dplyr::filter(d, game_date >= start, game_date <= end)
  }
  fetch_range <- function(start, end, depth = "month") {
    d <- tryCatch(fetch(start, end), error = function(e) NULL)
    if (is.null(d)) stop("fetch failed at ", depth, " for ", start, "..", end)
    if (nrow(d) < SAVANT_CAP) return(d)
    message("    ", depth, " ", start, "..", end, " hit cap -> finer")
    parts <- list(); cur <- start
    step <- if (depth == "month") 6L else 0L
    while (cur <= end) {
      wend <- min(cur + step, end)
      parts[[length(parts) + 1L]] <-
        if (depth == "month") fetch_range(cur, wend, "week") else fetch(cur, wend)
      cur <- wend + 1L; throttle(2)
    }
    dplyr::bind_rows(parts)
  }
  for (yr in from:to) for (m in months) {
    start <- as.Date(sprintf("%d-%02d-01", yr, m))
    end   <- as.Date(sprintf("%d-%02d-01", yr, m + 1L)) - 1L
    p <- lake_raw(lake, "milb", sprintf("milb-%d-%02d.parquet", yr, m))
    if (end < start || file.exists(p)) next
    tryCatch({
      throttle(1); d <- fetch_range(start, end)
      if (nrow(d) > 0) write_df(d, p)
    }, error = function(e) message("milb: FAILED ", yr, "-", m, " - ",
                                   substr(conditionMessage(e), 1, 90)))
    throttle(5)
  }
  message("milb: done")
  invisible(TRUE)
}
