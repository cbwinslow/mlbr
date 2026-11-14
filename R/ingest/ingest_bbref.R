# ingest_bbref.R - Baseball-Reference daily player logs -> data/raw/bbref/
#   bbref-bat-<year>.parquet / bbref-pit-<year>.parquet : one row per PLAYER-DAY
# POLITE BY CONTRACT: baseball-reference.com bans aggressive scrapers fast.
# One request per table per year, 12s spacing, small years first.
# Usage: Rscript R/ingest/ingest_bbref.R <from_year> <to_year>
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2015
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y")) - 1
for (yr in from:to) {
  for (nm in c("bat", "pit")) {
    p <- file.path(raw_dir("bbref"), sprintf("bbref-%s-%d.parquet", nm, yr))
    if (file.exists(p)) next
    tryCatch({
      d <- retry(2, 30, if (nm == "bat")
        baseballr::bref_daily_batter(t1 = sprintf("%d-03-01", yr), t2 = sprintf("%d-11-05", yr))
        else
        baseballr::bref_daily_pitcher(t1 = sprintf("%d-03-01", yr), t2 = sprintf("%d-11-05", yr)))
      if (!is.null(d) && nrow(d) > 0) write_df(as.data.frame(d), p)
    }, error = function(e) message("bbref: FAILED ", nm, "-", yr, " - ",
                                   substr(conditionMessage(e), 1, 80)))
    throttle(12)   # bbref is strict - never tighten this
  }
}
message("bbref: done")
