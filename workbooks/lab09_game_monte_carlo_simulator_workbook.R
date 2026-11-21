# ==============================================================================
# mlbr Student Work-Along Lab 09: Level 4 Plate-Appearance 24 Base-Out Markov
#                                 Game Simulator & Empirical Win Expectancy (WPA)
# ==============================================================================
# Companion to: site/workbooks/lab09_game_monte_carlo_simulator_workbook.qmd
# Reference Study: site/analyses/09_game_monte_carlo_simulator.qmd (EXP-009)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Multi-Category Log5 Matchup & Park Multiplier Math
# ------------------------------------------------------------------------------
log5_matchup_hand <- function(p_bat, p_pit, p_lg, pf_mult = rep(1.0, length(p_lg))) {
  raw <- (p_bat * p_pit / p_lg) * pf_mult
  raw / sum(raw)
}

cat("--- Part 1: Multi-Category Log5 Matchup (Elite Power Hitter vs. Ace SP at Coors) ---\n")
# Events: [SO, BB, HR, 1B, 2B, 3B, OUT]
p_lg  <- c(0.222, 0.094, 0.031, 0.143, 0.044, 0.004, 0.462)
p_bat <- c(0.240, 0.130, 0.062, 0.135, 0.052, 0.003, 0.378) # Judge/Ohtani-caliber bat
p_pit <- c(0.305, 0.060, 0.022, 0.128, 0.038, 0.003, 0.444) # Skubal/Skenes-caliber ace
pf_coors <- c(0.95, 1.02, 1.15, 1.14, 1.14, 1.14, 1.00)     # Coors Field multipliers

p_neut  <- log5_matchup_hand(p_bat, p_pit, p_lg)
p_coors <- log5_matchup_hand(p_bat, p_pit, p_lg, pf_coors)
names(p_neut) <- names(p_coors) <- c("SO", "BB", "HR", "1B", "2B", "3B", "OUT")
print(round(rbind(Neutral_Park = p_neut, Coors_Field = p_coors), 4))
cat("\n")

# ------------------------------------------------------------------------------
# Part 2: Inspecting Gold EXP-009 2024-2025 Game Backtest & Win Expectancy Matrix
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

make_lab09_plot <- function(df) {
  force(df)
  ggplot(df, aes(x = bin_label, y = actual_mean, group = 1)) +
    geom_col(fill = "#38bdf8", alpha = 0.55, width = 0.65) +
    geom_line(aes(y = pred_mean), color = "#10b981", linewidth = 1.3) +
    geom_point(aes(y = pred_mean), color = "#10b981", size = 3.2) +
    labs(
      title = "Lab 09 Diagnostic: Simulated Expected Total Runs vs. Actual Total Runs (2024-2025, n = 4,859)",
      subtitle = "Cyan bars = Observed Average Total Runs | Emerald line = Markov Simulator Expected Total Runs across deciles",
      x = "Simulated Expected Total Runs Decile",
      y = "Average Combined Runs Scored Per Game",
      caption = "Source: mlbr Gold game_simulation_backtest_2024_2025.parquet (EXP-009) | Student Lab 09"
    ) +
    mlbr::theme_cyber(base_size = 10.5) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

run_lab09 <- function(lake = NULL) {
  if (is.null(lake)) lake <- mlbr::default_lake()
  proc <- file.path(lake, "data/processed")
  req <- c("game_simulation_backtest_2024_2025.parquet",
           "game_simulator_benchmark_metrics.parquet",
           "win_expectancy_re24_matrix.parquet")
  missing <- req[!file.exists(file.path(proc, req))]
  if (length(missing) > 0L) {
    stop(sprintf("Missing EXP-009 Gold files (%s). Run 'Rscript models/09_game_monte_carlo_simulator.R --full' first.",
                 paste(missing, collapse = ", ")))
  }

  games <- as.data.table(arrow::read_parquet(file.path(proc, "game_simulation_backtest_2024_2025.parquet")))
  bench <- as.data.table(arrow::read_parquet(file.path(proc, "game_simulator_benchmark_metrics.parquet")))
  we    <- as.data.table(arrow::read_parquet(file.path(proc, "win_expectancy_re24_matrix.parquet")))

  cat("--- Part 2A: Out-of-Sample Game Prediction Benchmark (2024-2025, n = 4,859) ---\n")
  print(bench)

  cat("\n--- Part 2B: Highest-Leverage Walk-Off States (Bot 9th, Tied, 2 Outs) ---\n")
  print(we[inning == 9L & bat_home == 1L & outs == 2L & score_diff == 0L][order(bases),
           .(inning, outs, bases, n_plays, home_we = round(home_we, 4))])

  # Part 3: Dark Cyber Total-Runs Calibration Chart (4-Asset Preservation)
  games[, dec := cut(exp_total_runs, breaks = stats::quantile(exp_total_runs, probs = seq(0, 1, 0.1)), include.lowest = TRUE)]
  tot_df <- games[, .(n = .N, pred_mean = mean(exp_total_runs), actual_mean = mean(actual_total_runs)), by = dec][order(pred_mean)]
  tot_df[, bin_label := factor(sprintf("D%d (%.1f)", seq_len(.N), pred_mean), levels = sprintf("D%d (%.1f)", seq_len(.N), pred_mean))]

  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)

  p_lab09 <- make_lab09_plot(tot_df)
  mlbr::save_plot_assets(p_lab09, "lab09_game_total_calibration", data = tot_df,
                         width = 11.5, height = 6.5, dir = models_dir)
  file.copy(file.path(models_dir, "lab09_game_total_calibration.png"),
            file.path(assets_dir, "lab09_game_total_calibration.png"), overwrite = TRUE)
  cat("\nLab 09 completed cleanly! Inspect models/lab09_game_total_calibration.png\n")
  invisible(tot_df)
}

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Try these exercises in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Run-Line -1.5 Pricing):
#   In `games`, compute the empirical cover rate of Home -1.5 (`home_score - away_score >= 2`)
#   when `p_m4_rl15 >= 0.45` vs. `< 0.30`. How closely does the Markov joint score matrix
#   match actual 2+ run margins?
#
# CHECKPOINT 2 (Win Expectancy Leverage):
#   Compare `home_we` in `we` for Top 9th, 0 outs, bases empty, tied (`score_diff = 0`)
#   vs. Bottom 9th, 0 outs, bases loaded (`bases = 7`), tied (`score_diff = 0`).
#   How much win probability is created simply by loading the bases with 0 outs?
#
# CHECKPOINT 3 (Why M3 + M4 Beat Coin Flip & HFA):
#   Inspect the Brier Score gap between `B1_HomeField_Constant` (0.24897) and
#   `M4_PA_Markov_Simulator` (0.24451) / `M3_Starter_Team_Log5` (0.24387). Why is a
#   0.005 Brier improvement massive in MLB game forecasting?
# ------------------------------------------------------------------------------

if (!interactive()) {
  run_lab09()
}
