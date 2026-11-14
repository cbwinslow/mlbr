#!/usr/bin/env Rscript
# proof_lake.R - Standalone CLI runner for the baseballLake proof suite
# Usage: Rscript R/proof_lake.R [/path/to/lake]

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(baseballLake)
})

args <- commandArgs(trailingOnly = TRUE)
lake <- if (length(args) >= 1) args[1] else "/mnt/storage/Rprojects/mlb"

res <- proof_lake(lake, verbose = TRUE)
failed <- sum(res$status == "FAIL")

if (failed > 0) {
  quit(status = 1)
} else {
  quit(status = 0)
}
