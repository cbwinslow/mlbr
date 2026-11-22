# ==============================================================================
# mlbr Student Work-Along Lab 13: Pitcher Stamina, In-Game Velocity Decay &
#                                 Dynamic Rolling Bullpen Fatigue Engine
# ==============================================================================
# Companion to: site/workbooks/lab13_pitcher_stamina_bullpen_fatigue_workbook.qmd
# Reference Study: site/analyses/13_pitcher_stamina_bullpen_fatigue.qmd (EXP-013)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Bullpen Fatigue Penalty Calculation
# ------------------------------------------------------------------------------
bp_penalty_hand <- function(pitches_3d, mean_3d = 140.0, slope_per_100 = 0.0516) {
  slope_per_100 * ((pitches_3d - mean_3d) / 100.0)
}
cat("--- Part 1: Rested (60 pitches / 3d) vs. Taxed (240 pitches / 3d) Bullpen ---\n")
cat(sprintf("Rested (60p): %+.3f R/G | Taxed (240p): %+.3f R/G | Matchup Swing: %.3f R/G\n\n",
            bp_penalty_hand(60), bp_penalty_hand(240), bp_penalty_hand(240) - bp_penalty_hand(60)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit Over EXP-013 Gold Tables
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

bp_p <- file.path(lake, "data/processed/team_bullpen_fatigue_daily_2018_2025.parquet")
sql_bp <- sprintf("
  SELECT
    load_bin,
    COUNT(*) AS n_team_games,
    ROUND(AVG(bp_pitches_3d), 1) AS avg_3d_pitches,
    ROUND(AVG(late_runs_allowed), 3) AS avg_late_runs
  FROM read_parquet('%s')
  WHERE load_bin IS NOT NULL
  GROUP BY load_bin
  ORDER BY avg_3d_pitches
", bp_p)
cat("--- Part 2A: Zero-Copy DuckDB SQL Bullpen Workload vs. Late-Inning Runs ---\n")
print(DBI::dbGetQuery(con, sql_bp))

stam <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/pitcher_stamina_leaderboard_2024_2025.parquet")))
cat("\n--- Part 2B: Top 6 2025 Starters by Late-Game Velocity Retention (Pitch 75+ vs 1-25) ---\n")
print(head(stam[season == 2025L][order(-velo_retention_mph),
      .(player_name, n_starts, early_v = round(mean_early_velo, 1),
        late_v = round(mean_late_velo, 1), ret_mph = round(velo_retention_mph, 2),
        late_rv100 = round(late_rv_per_100, 2))], 6))

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Distribution of Starter Late-Game Velocity Retention
# ------------------------------------------------------------------------------
st25 <- stam[season == 2025L]
p_diag <- ggplot(st25, aes(x = velo_retention_mph)) +
  geom_histogram(bins = 24, fill = "#38bdf8", color = "#090d16", alpha = 0.88) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#f59e0b", linewidth = 0.9) +
  geom_vline(xintercept = mean(st25$velo_retention_mph), linetype = "solid", color = "#10b981", linewidth = 0.9) +
  labs(
    title = "Lab 13 Diagnostic: 2025 Starter Velocity Retention Distribution (Pitch 75+ minus 1-25)",
    subtitle = sprintf("n = %d qualified 2025 starters | Mean = %+.2f mph (emerald) | Rare 'ramping' starters sit right of 0.0 (amber)",
                       nrow(st25), mean(st25$velo_retention_mph)),
    x = "Late-Game Fastball Velocity Retention (mph)",
    y = "Number of Starting Pitchers",
    caption = "Source: mlbr Lab 13 (EXP-013 Starter Stamina Engine)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab13_starter_stamina_dist.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 13 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Survivorship Bias Past Pitch 80):
#   In `starter_velocity_decay_curve_2018_2025.parquet`, explain why mean velocity
#   rebounds slightly from `-0.43 mph` (61-80) to `-0.04 mph` (101+) while pitcher
#   Run Value/100 simultaneously worsens to `+0.35`.
#
# CHECKPOINT 2 (Zero-Leakage Rolling Windows):
#   Inspect the SQL `LEFT JOIN` in `models/13_pitcher_stamina_bullpen_fatigue.R`.
#   Why is `g2.game_date < g1.game_date` mandatory to prevent same-day doubleheader leakage?
#
# CHECKPOINT 3 (Bullpen Taxed vs. Light Load):
#   Compare `mean(late_runs_allowed)` for `Taxed (181+)` vs. `Light (61-100)` bullpens
#   specifically in the locked 2024-2025 OOS seasons.
