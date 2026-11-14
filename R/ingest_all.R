# ingest_all.R - run the full ingest pipeline (all sources, all history)
# Usage:
#   Rscript R/ingest_all.R                     # everything, default ranges
#   Rscript R/ingest_all.R statcast 2008 2025  # single source + year range
#
# ORDER & SERIALIZATION (rate-limit safety):
#   - cheap local/no-HTTP sources first
#   - savant-backed sources (statcast, milb) never run concurrently
#   - bbref last (strictest scraper; 12s spacing)
suppressPackageStartupMessages(library(arrow))
args <- commandArgs(trailingOnly = TRUE)
run <- function(script, ...) {
  message("========== ", script, " ==========")
  source(file.path("R", "ingest", script), local = new.env(), echo = FALSE)
}
if (length(args) == 0) {
  run("ingest_lahman.R")
  run("ingest_chadwick.R")
  run("ingest_retrosheet_github.R", "1871", "2025")   # GitHub mirror (no retrosheet.org)
  run("ingest_ncaa.R")                                # pre-cleaned NCAA datasets
  run("ingest_mlb_meta.R", "2008", "2026")
  run("ingest_fangraphs.R", "1901", "2025")
  run("ingest_statcast.R", "2008", "2025")
  run("ingest_milb.R", "2021", "2025")                # after statcast (same backend)
  run("ingest_espn.R", "2002", "2026")
  run("ingest_bbref.R", "2015", "2025")               # politest, last
  # run("ingest_retrosheet.R", "1955", "2025")        # only when retrosheet.org reachable
} else {
  src <- paste0("ingest_", args[1], ".R")
  run(src, args[-1])
}
message("INGEST_ALL_DONE")
