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

# Mechanical 1:1 Q-CRISP Study <-> Student Work-Along Lab parity guardrail
lake_root <- mlbr::default_lake()
study_files <- sort(Sys.glob(file.path(lake_root, "site/analyses/[0-9][0-9]_*.qmd")))
study_nums  <- sub("^([0-9][0-9])_.*$", "\\1", basename(study_files))
missing_labs <- character(0)
for (sn in study_nums) {
  r_hits   <- Sys.glob(file.path(lake_root, sprintf("workbooks/lab%s_*.R", sn)))
  qmd_hits <- Sys.glob(file.path(lake_root, sprintf("site/workbooks/lab%s_*.qmd", sn)))
  if (length(r_hits) == 0L)   missing_labs <- c(missing_labs, sprintf("workbooks/lab%s_*.R", sn))
  if (length(qmd_hits) == 0L) missing_labs <- c(missing_labs, sprintf("site/workbooks/lab%s_*.qmd", sn))
}
audit_df <- as.data.frame(res$audit, stringsAsFactors = FALSE)
lab_row <- audit_df[1, , drop = FALSE]
for (cn in names(lab_row)) lab_row[[cn]] <- NA
if ("category" %in% names(lab_row)) lab_row$category <- "Curriculum Parity"
lab_row$test   <- "Q-CRISP Study <-> Work-Along Lab 1:1 Parity"
lab_row$status <- if (length(missing_labs) == 0L) "PASS" else "FAIL"
lab_row$detail <- if (length(missing_labs) == 0L) {
  sprintf("All %d numbered Q-CRISP studies (%s) have matching workbooks/labXX_*.R & site/workbooks/labXX_*.qmd",
          length(study_nums), paste(study_nums, collapse = ", "))
} else {
  paste("Missing companion labs:", paste(missing_labs, collapse = ", "))
}
audit_df <- rbind(audit_df, lab_row)

cat("\n--- Semantic & Consistency Invariant Audit Results ---\n")
print(audit_df, row.names = FALSE)

all_pass <- all(audit_df$status == "PASS")
if (all_pass) {
  cat("\n*** ALL SEMANTIC & CONSISTENCY INVARIANTS PASSED - LAKE 100% HEALTHY FOR AI AGENTS ***\n\n")
  quit(status = 0)
} else {
  cat("\n*** WARNING: ONE OR MORE INVARIANTS FAILED ***\n\n")
  quit(status = 1)
}
