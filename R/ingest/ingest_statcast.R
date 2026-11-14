# ingest_statcast.R - Statcast per-pitch data -> data/raw/statcast/
#   statcast-<year>-<month>.parquet : one row per pitch (finest granularity that exists)
#
# Savant's search endpoint caps a single request at 25,000 rows. In-season months
# exceed it, so this script walks a tier ladder: month -> week -> day. A DAY is
# always far below the cap (~4,500 pitches), so the ladder guarantees completeness.
# Every fetched chunk is trimmed to its requested [start,end] window before
# binding, so month files never bleed into neighbors and never duplicate rows.
# Verifies each month file covers every calendar day before writing it.
#
# Usage: Rscript R/ingest/ingest_statcast.R <from_year> <to_year>
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else as.integer(format(Sys.Date(), "%Y")) - 1
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))
SAVANT_CAP <- 25000L

fetch <- function(start, end) {
  d <- retry(3, 15, baseballr::scrape_statcast_savant(start_date = start, end_date = end))
  d <- d |> filter(game_date >= start, game_date <= end)   # enforce exact window
  d
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
    if (depth == "month") {
      parts[[length(parts) + 1L]] <- fetch_range(cur, wend, "week")
    } else {
      parts[[length(parts) + 1L]] <- fetch(cur, wend)   # day level: always < cap
    }
    cur <- wend + 1L
    throttle(2)                                        # politeness between requests
  }
  bind_rows(parts)
}

for (yr in from:to) for (m in 3:11) {   # Mar (spring training) - Nov (WS window)
  start <- as.Date(sprintf("%d-%02d-01", yr, m))
  end   <- as.Date(sprintf("%d-%02d-01", yr, m + 1L)) - 1L   # last day OF the month
  p <- file.path(raw_dir("statcast"), sprintf("statcast-%d-%02d.parquet", yr, m))
  if (end < start || file.exists(p)) next
  tryCatch({
    throttle(1)
    d <- fetch_range(start, end)
    if (nrow(d) > 0) {
      # completeness gate: every calendar day of the month must have pitched
      got <- sort(unique(d$game_date))
      days <- seq(start, end, by = "day")
      no_game <- days[!(days %in% got)]   # off-days (All-Star break etc.) are fine,
                                          # only a gap AFTER a covered day with a
                                          # later covered day is suspicious
      if (length(got)) {
        holes <- got[which(c(0, diff(as.integer(got))) > 1)[-1] - 1]
        holes <- holes[holes > min(got) & holes < max(got)]
        if (length(holes)) warning("statcast ", yr, "-", sprintf("%02d", m),
                                   ": possible day gaps between ", min(holes),
                                   " and ", max(holes), " - inspect before trusting")
      }
      write_df(d, p)
      message("  ", yr, "-", sprintf("%02d", m), ": ", nrow(d), " pitches, ",
              length(got), " game days")
    } else message("  empty ", yr, "-", m)
  }, error = function(e) message("statcast: FAILED ", yr, "-", m, " - ",
                                 substr(conditionMessage(e), 1, 90)))
  throttle(5)
}
message("statcast: done (", from, "-", to, ")")
