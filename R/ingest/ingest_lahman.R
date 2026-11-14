# ingest_lahman.R - all Lahman package tables -> data/raw/lahman/*.parquet
# Granularity: player-season (finest Lahman provides; covers 1871-present).
# Usage: Rscript R/ingest/ingest_lahman.R
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow) })
library(Lahman)
items <- data(package = "Lahman")$results[, "Item"]
for (ds in items) {
  p <- file.path(raw_dir("lahman"), paste0(ds, ".parquet"))
  if (file.exists(p)) { message("lahman: skip existing ", ds); next }
  write_df(get(ds), p)
}
message("lahman: done (", length(items), " tables)")
