#!/usr/bin/env Rscript
# R/generate_agent_manifest.R - Generate AI Agent Data Lake Manifest and Run Semantic Invariant Audit
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(dplyr)
})

cat("==============================================================================\n")
cat("          MLBR AI AGENT MANIFEST & SEMANTIC INTEGRITY RUNNER                  \n")
cat(sprintf("          Timestamp: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
cat("==============================================================================\n\n")

res <- mlbr::generate_agent_manifest()

cat("\n--- Semantic Invariant Audit Results ---\n")
print(as.data.frame(res$audit), row.names = FALSE)

all_pass <- all(res$audit$status == "PASS")
if (all_pass) {
  cat("\n*** ALL SEMANTIC INVARIANTS PASSED - LAKE 100% HEALTHY FOR AI AGENTS ***\n\n")
  quit(status = 0)
} else {
  cat("\n*** WARNING: ONE OR MORE INVARIANTS FAILED ***\n\n")
  quit(status = 1)
}
