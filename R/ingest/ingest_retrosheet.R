# ingest_retrosheet.R - Retrosheet play-by-play (event-level) -> data/raw/retrosheet/
#   plays-<year>.parquet  : parsed event data (one row per play, via chadwick)
#   retrosheet_raw/       : raw downloads (zips + fields.csv) kept as cache
# Retrosheet has event-level data back to ~1955; earlier decades are partial.
# Usage: Rscript R/ingest/ingest_retrosheet.R <from_year> <to_year>
# NOTE: baseballr::retrosheet_data() setwd()s internally - always pass an ABSOLUTE path.
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2015
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))

work_dir <- normalizePath(file.path(getwd(), "data", "raw", "retrosheet_raw"), mustWork = FALSE)
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

# preflight: retrosheet.org is a hobbyist server and is regularly unreachable for
# hours at a time. Fail fast instead of burning retries on every year.
ok <- tryCatch({ system("curl -sI --max-time 30 https://www.retrosheet.org/ > /dev/null",
                       timeout = 40) == 0 }, error = function(e) FALSE)
if (!ok) stop("retrosheet.org unreachable right now - run this script later (idempotent).\n")

for (yr in from:to) {
  p <- file.path(raw_dir("retrosheet"), sprintf("plays-%d.parquet", yr))
  if (file.exists(p)) { message("retrosheet: skip existing ", yr); next }
  message("retrosheet: acquiring ", yr, " ...")
  tryCatch({
    res <- retry(3, 20, {
      baseballr::retrosheet_data(path_to_directory = work_dir,
                                 years_to_acquire = c(yr, yr),
                                 sequence_years = TRUE)
    })
    # res: list of event data frames (per team-year) -> bind to play-level
    d <- if (is.list(res)) bind_rows(res[names(res) != "" ]) else res
    if (is.data.frame(d) && nrow(d) > 0) write_df(d, p) else message("  empty result ", yr)
  }, error = function(e) message("retrosheet: FAILED ", yr, " - ",
                                 substr(conditionMessage(e), 1, 100)))
  Sys.sleep(5)
}
message("retrosheet: done (", from, "-", to, ")")
