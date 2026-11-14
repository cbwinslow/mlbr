# ingest_milb.R - Minor League Statcast (per-pitch!) -> data/raw/milb/
# Uses Savant's minors search (same endpoint family as MLB statcast) with the
# SAME tier ladder (month -> week -> day) and the 25k cap handling.
# MUST be serialized with ingest_statcast.R (same backend).
# Usage: Rscript R/ingest/ingest_milb.R <from_year> <to_year>
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2021
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))
SAVANT_CAP <- 25000L

fetch <- function(start, end) {
  d <- retry(3, 15, baseballr::statcast_search_minors(start_date = start, end_date = end))
  d <- d |> filter(game_date >= start, game_date <= end)
  d
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
  bind_rows(parts)
}
for (yr in from:to) for (m in 4:9) {   # MiLB season Apr-Sep
  start <- as.Date(sprintf("%d-%02d-01", yr, m))
  end   <- as.Date(sprintf("%d-%02d-01", yr, m + 1L)) - 1L
  p <- file.path(raw_dir("milb"), sprintf("milb-%d-%02d.parquet", yr, m))
  if (end < start || file.exists(p)) next
  tryCatch({
    throttle(1); d <- fetch_range(start, end)
    if (nrow(d) > 0) write_df(d, p)
  }, error = function(e) message("milb: FAILED ", yr, "-", m, " - ",
                                 substr(conditionMessage(e), 1, 90)))
  throttle(5)
}
message("milb: done")
