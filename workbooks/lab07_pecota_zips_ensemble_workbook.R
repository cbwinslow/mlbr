# ==============================================================================
# mlbr Student Work-Along Lab 07: PECOTA k-NN Comps, ZiPS 5-Year Aging &
#                                 Non-Pitch Defense/Baserunning EDA
# ==============================================================================
# Companion to: site/workbooks/lab07_pecota_zips_ensemble_workbook.qmd
# Reference Study: site/analyses/07_pecota_zips_ensemble.qmd (EXP-007)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Era-Neutral Z-Scores & PECOTA Similarity Math
# ------------------------------------------------------------------------------
era_zscore <- function(x, lg_mean, lg_sd) (x - lg_mean) / lg_sd

pecota_similarity <- function(z_target, z_comp, weights) {
  d <- sqrt(sum(weights * (z_target - z_comp)^2))
  list(distance = d, similarity_score = 100.0 * exp(-0.22 * d))
}

cat("--- Part 1: Era-Neutralized PECOTA Similarity Toy Example ---\n")
# Compare 2025 Juan Soto (Age 26) vs. 1995 Frank Thomas (Age 27) in z-score space
# Features: [Age_z, wOBA_z, K%_z, BB%_z, ISO_z]
w_vec    <- c(1.40, 1.20, 1.00, 1.00, 1.10)^2
soto_z   <- c(-0.30, 2.50, -0.50, 2.80, 1.80)
thomas_z <- c(-0.15, 2.65, -0.45, 2.70, 2.05)
sim_ex   <- pecota_similarity(soto_z, thomas_z, w_vec)
cat(sprintf("Weighted Euclidean Distance = %.3f | PECOTA Similarity Score = %.1f / 100\n\n",
            sim_ex$distance, sim_ex$similarity_score))

# ------------------------------------------------------------------------------
# Part 2: Exploring EXP-007 Gold Tables (Defense EDA, Comps, 5-Yr ZiPS, Shootout)
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

make_lab07_plot <- function(df) {
  force(df)
  ggplot(df, aes(x = age_2026, y = survival_prob * 100, color = group)) +
    geom_point(alpha = 0.72, size = 2.6) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
    scale_color_manual(values = c("C" = "#f43f5e", "Middle" = "#38bdf8", "Corner_DH" = "#10b981"),
                       name = "Position Archetype") +
    labs(
      title = "Lab 07 Diagnostic: Empirical PECOTA Attrition / Survival Curve by Position Archetype (2026)",
      subtitle = "Probability of remaining active with >= 50 PA in the following season based on 1960-2025 nearest historical comps",
      x = "2026 Baseball Age (July 1 Convention)",
      y = "Empirical 1-Year Survival Probability (%)",
      caption = "Source: mlbr Gold pecota_knn_comps_2026.parquet (EXP-007) | Student Lab 07"
    ) +
    mlbr::theme_cyber(base_size = 10.5)
}

run_lab07 <- function(lake = NULL) {
  if (is.null(lake)) lake <- mlbr::default_lake()
  proc <- file.path(lake, "data/processed")
  req <- c("defense_baserunning_eda_2002_2025.parquet", "pecota_knn_comps_2026.parquet",
           "zips_5yr_trajectories_2026_2030.parquet", "ensemble_backtest_metrics.parquet")
  missing <- req[!file.exists(file.path(proc, req))]
  if (length(missing) > 0L) {
    stop(sprintf("Missing EXP-007 Gold files (%s). Run 'Rscript models/07_pecota_zips_ensemble.R --full' first.",
                 paste(missing, collapse = ", ")))
  }

  eda   <- as.data.table(arrow::read_parquet(file.path(proc, "defense_baserunning_eda_2002_2025.parquet")))
  comps <- as.data.table(arrow::read_parquet(file.path(proc, "pecota_knn_comps_2026.parquet")))
  z5    <- as.data.table(arrow::read_parquet(file.path(proc, "zips_5yr_trajectories_2026_2030.parquet")))
  sh    <- as.data.table(arrow::read_parquet(file.path(proc, "ensemble_backtest_metrics.parquet")))

  cat("--- Part 2A: Non-Pitch Defense & Baserunning YoY Reliability (2002-2025) ---\n")
  print(eda[order(-yoy_r), .(domain_group, metric_label, n_pairs, yoy_r)])

  cat("\n--- Part 2B: 5-Paradigm Locked 2024 -> 2025 Out-of-Sample Shootout ---\n")
  print(sh)

  cat("\n--- Part 2C: Top 3 Historical Comps for Marquee 2026 Pitchers ---\n")
  p_stars <- c("Tarik Skubal", "Paul Skenes", "Garrett Crochet", "Zack Wheeler")
  print(comps[domain == "Pitcher" & player_name %in% p_stars & comp_rank <= 3L,
              .(player_name, age_2026, comp_rank, comp_name, comp_year, comp_age, similarity)])

  cat("\n--- Part 2D: 5-Year ZiPS Trajectory (2026-2030) for Bobby Witt Jr. & Aaron Judge ---\n")
  print(z5[player_name %in% c("Bobby Witt Jr.", "Aaron Judge"),
           .(player_name, group, horizon_year, age_at_yr, proj_woba = round(proj_woba, 3))])

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Attrition/Survival Diagnostic Chart (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  b_surv <- unique(comps[domain == "Batter", .(mlbam, player_name, age_2026, group, survival_prob)])
  p_lab07 <- make_lab07_plot(b_surv)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  mlbr::save_plot_assets(p_lab07, "lab07_pecota_survival_curves", data = b_surv,
                         width = 11.0, height = 6.5, dir = models_dir)
  file.copy(file.path(models_dir, "lab07_pecota_survival_curves.png"),
            file.path(assets_dir, "lab07_pecota_survival_curves.png"), overwrite = TRUE)
  cat("\nLab 07 completed cleanly! Inspect models/lab07_pecota_survival_curves.png\n")
  invisible(b_surv)
}

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Try these exercises in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Defense Stabilization):
#   Compare the YoY r of Statcast OAA (0.506) vs. DRS (0.431) vs. UZR/150 (0.374).
#   Why does OAA stabilize faster than legacy box-score / zone-grid fielding metrics?
#
# CHECKPOINT 2 (PECOTA Breakout Hunters):
#   Query `comps[domain == "Batter" & comp_rank == 1 & age_2026 <= 25]` sorted by
#   descending `breakout_prob`. Which young 2026 hitters have the highest empirical
#   breakout probability based on their 1960-2025 historical comps?
#
# CHECKPOINT 3 (Convex Stacking vs. Statcast Physics):
#   Inspect how the exact KKT M5_Consensus_Ensemble blends 68.1% M4_Statcast + 31.9% PECOTA
#   on batters (w_r = 0.5782) and 81.8% M4 + 11.1% Marcel + 7.1% ZiPS on pitchers
#   (w_rmse = 0.71565, beating every individual projection paradigm).
# ------------------------------------------------------------------------------

if (!interactive()) {
  run_lab07()
}
