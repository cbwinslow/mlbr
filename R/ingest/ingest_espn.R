# ingest_espn.R - ESPN endpoints -> data/raw/espn/
#   college: per-season scoreboards (every game ESPN has for NCAA baseball)
#   mlb: per-season scoreboards (game inventory + scores, complements statsapi)
# ESPN is a CDN-backed public API; keep 2s spacing anyway.
# Usage: Rscript R/ingest/ingest_espn.R <from_year> <to_year>
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2002
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))
for (yr in from:to) {
  for (nm in c("college", "mlb")) {
    p <- file.path(raw_dir("espn"), sprintf("espn-%s-scoreboard-%d.parquet", nm, yr))
    if (file.exists(p)) next
    tryCatch({
      d <- if (nm == "college") retry(2, 10, baseballr::espn_college_baseball_scoreboard(season = yr))
           else                 retry(2, 10, baseballr::espn_mlb_scoreboard(season = yr))
      if (!is.null(d) && nrow(d) > 0) write_df(as.data.frame(d), p)
    }, error = function(e) message("espn: FAILED ", nm, "-", yr, " - ",
                                   substr(conditionMessage(e), 1, 80)))
    throttle(2)
  }
}
message("espn: done")
