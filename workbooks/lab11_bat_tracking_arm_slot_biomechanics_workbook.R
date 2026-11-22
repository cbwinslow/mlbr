# ==============================================================================
# mlbr Student Work-Along Lab 11: Hawk-Eye Bat-Tracking & Pitcher Arm-Slot
#                                 (arm_angle) Biomechanics
# ==============================================================================
# Companion to: site/workbooks/lab11_bat_tracking_arm_slot_biomechanics_workbook.qmd
# Reference Study: site/analyses/11_bat_tracking_arm_slot_biomechanics.qmd (EXP-011)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Collision Physics (MaxEV, Squared-Up & Blast Rate)
# ------------------------------------------------------------------------------
calc_max_ev <- function(bat_speed, rel_speed) {
  1.23 * bat_speed + 0.2116 * rel_speed
}
is_blast <- function(ev, bat_speed, rel_speed) {
  sq <- ev / calc_max_ev(bat_speed, rel_speed)
  (sq >= 0.80) & ((bat_speed + 100.0 * sq) >= 164.0)
}

cat("--- Part 1: Collision Physics Hand-Check (Judge 76.4 mph swing vs 95 mph FF) ---\n")
max_ev <- calc_max_ev(76.4, 95.0)
ev_obs <- 108.5
sq_pct <- 100.0 * ev_obs / max_ev
cat(sprintf("Max Theoretical EV = %.1f mph | Observed EV = %.1f mph | Squared-Up = %.1f%% | Blast? %s\n\n",
            max_ev, ev_obs, sq_pct, is_blast(ev_obs, 76.4, 95.0)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit Over Hawk-Eye Swings & Gold EXP-011 Tables
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

sql_swings <- "
  SELECT
    game_year,
    COUNT(*) AS n_comp_swings,
    ROUND(AVG(TRY_CAST(bat_speed AS DOUBLE)), 2) AS mean_bs_mph,
    ROUND(AVG(TRY_CAST(swing_length AS DOUBLE)), 2) AS mean_sl_ft,
    ROUND(100.0 * AVG(CASE WHEN TRY_CAST(bat_speed AS DOUBLE) >= 75.0 THEN 1.0 ELSE 0.0 END), 1) AS fast_swing_pct
  FROM v_statcast
  WHERE game_year BETWEEN 2023 AND 2026
    AND game_type = 'R'
    AND TRY_CAST(bat_speed AS DOUBLE) >= 60.0
  GROUP BY game_year
  ORDER BY game_year
"
cat("--- Part 2A: Zero-Copy DuckDB SQL Hawk-Eye Competitive Swings (2023-2026) ---\n")
print(DBI::dbGetQuery(con, sql_swings))

bat <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/batter_bat_tracking_biomechanics_2023_2026.parquet")))
pit <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/pitcher_arm_angle_biomechanics_2020_2026.parquet")))
yoy <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/biomechanics_yoy_reliability_2024_2025.parquet")))

cat("\n--- Part 2B: Top 6 Hitters (2025) by SwingPower+ ---\n")
print(head(bat[season == 2025L & n_comp_swings >= 250L][order(-swing_power_plus),
      .(player_name, bs = round(mean_bat_speed, 1), sl = round(mean_swing_length, 2),
        blast_pct = round(100 * blast_rate, 1), swing_power_plus = round(swing_power_plus, 1))], 6))

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Bat Speed vs. 2025 ISO Across Qualified Hitters
# ------------------------------------------------------------------------------
b25 <- bat[season == 2025L & n_comp_swings >= 200L & !is.na(iso)]
p_diag <- ggplot(b25, aes(x = mean_bat_speed, y = iso, color = 100 * blast_rate)) +
  geom_point(alpha = 0.85, size = 2.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#38bdf8", fill = "#38bdf8", alpha = 0.18) +
  scale_color_viridis_c(option = "plasma", name = "Blast %") +
  labs(
    title = "Lab 11 Diagnostic: Competitive Bat Speed (mph) vs. Isolated Power (ISO, 2025)",
    subtitle = sprintf("n = %d qualified hitters | Pearson r = %.3f | Bat Speed stabilizes at r = 0.923 YoY vs. ISO r = 0.591",
                       nrow(b25), stats::cor(b25$mean_bat_speed, b25$iso)),
    x = "Mean Competitive Bat Speed (mph, >= 60 mph swings)",
    y = "2025 Isolated Power (ISO = SLG - AVG)",
    caption = "Source: mlbr Lab 11 (EXP-011 Hawk-Eye Bat-Tracking)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab11_bat_speed_vs_iso.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 11 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Compact vs. Long Power Swings):
#   Compare Juan Soto (`bs=73.5, sl=7.03 ft`) vs. Giancarlo Stanton (`bs=79.9, sl=8.76 ft`).
#   Compute `bat_speed / swing_length` (angular whip efficiency) across `b25`. Who leads MLB?
#
# CHECKPOINT 2 (Why Arm Angle Matters for 4-Seam Whiffs):
#   In `pit[season == 2025L & n_ff_pitches >= 250L]`, compare mean `ff_whiff_rate`
#   for pitchers with `ivb_above_slot >= +1.5` vs. `ivb_above_slot <= -1.5`.
#
# CHECKPOINT 3 (Process vs. Outcome Reliability):
#   Inspect `yoy`. Why does `bat_speed` (`r = 0.923`) improve locked 2025 OOS ISO
#   RMSE by 3.65% even when prior-year ISO is already in the regression?
