# ingest_chadwick.R - Chadwick player-ID crosswalk -> data/raw/chadwick/register.parquet
# Granularity: one row per player. Maps MLBAM/bbref/retro/FanGraphs IDs.
# Usage: Rscript R/ingest/ingest_chadwick.R
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow) })
p <- file.path(raw_dir("chadwick"), "register.parquet")
if (file.exists(p)) { message("chadwick: register exists, skipping") } else {
  lu <- retry(3, 15, baseballr::get_chadwick_lu())
  write_df(lu, p)
}
message("chadwick: done")
