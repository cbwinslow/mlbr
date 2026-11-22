# ==============================================================================
# mlbr Student Work-Along Lab 17: Batter Home Run (HR >= 1) & Total Bases
#                                 (TB >= 2) Prop Pricing via v_statcast_biomechanics
# ==============================================================================
# Companion to: site/workbooks/lab17_batter_hr_total_bases_props_workbook.qmd
# Reference Study: site/analyses/17_batter_hr_total_bases_props.qmd (EXP-017)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Home Run Prop Fair American Odds Conversion
# ------------------------------------------------------------------------------
prob_to_am <- function(p) ifelse(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p)
cat("--- Part 1: Fair American Odds for MLB Home Run & Total Bases Props ---\n")
cat(sprintf("Aaron Judge (32.1%% HR): Fair %+.0f | League Avg Starter (11.7%% HR): Fair %+.0f\n\n",
            prob_to_am(0.321), prob_to_am(0.117)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Query Using Native v_statcast_biomechanics View
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

cat("--- Part 2A: Native v_statcast_biomechanics View Check (17th Lake View) ---\n")
print(DBI::dbGetQuery(con, "
  SELECT game_year, COUNT(*) AS n_pitches, SUM(is_blast) AS n_blasts,
         ROUND(AVG(CASE WHEN bat_speed_dbl >= 60 THEN bat_speed_dbl END), 2) AS mean_bs
  FROM v_statcast_biomechanics
  WHERE game_year >= 2023
  GROUP BY game_year ORDER BY game_year
"))

ab <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/batter_hr_tb_ablation_metrics_2025.parquet")))
cat("\n--- Part 2B: Locked 2025 OOS Batter HR & TB>=2 Ablation ---\n")
print(ab)

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Batting-Order Slot (1..9) vs. HR & TB>=2 Rates
# ------------------------------------------------------------------------------
bg <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/batter_hr_tb_prop_backtest_2024_2025.parquet")))
slot_df <- bg[season == 2025L, .(
  n_obs    = .N,
  hr_pct   = 100 * mean(y_hr),
  tb2_pct  = 100 * mean(y_tb2)
), by = slot][order(slot)]

p_diag <- ggplot(slot_df, aes(x = factor(slot), y = hr_pct, fill = tb2_pct)) +
  geom_col(width = 0.68, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.1f%% HR\n(TB>=2: %.1f%%)", hr_pct, tb2_pct)),
            vjust = -0.30, color = "#f8fafc", size = 2.9, fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", name = "TB >= 2\nRate (%)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Lab 17 Diagnostic: Batting-Order Slot (1-9) vs. 2025 HR & Total Bases Prop Hit Rates",
    subtitle = "Cleanup/Heart-of-Order (#2-#4) hits HR in ~15% of games vs. #9 slot (~8%) due to both talent and extra PA exposure",
    x = "Starting Batting-Order Slot (1 = Leadoff, 9 = Bottom)",
    y = "Observed 2025 Game Home Run Rate (%)",
    caption = "Source: mlbr Lab 17 (EXP-017 Batter HR & Total Bases Prop Engine)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab17_slot_hr_tb2_rates.png")
ggsave(out_png, p_diag, width = 10.8, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 17 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Handedness Park + Weather Multiplier on HR Props):
#   In `bg[season == 2025L]`, compare `mean(y_hr)` when `pf_hr_hand >= 115 & temp_f >= 80`
#   vs. `pf_hr_hand <= 85 & temp_f <= 60`. How wide is the environmental HR swing?
#
# CHECKPOINT 2 (Why Batting-Order Slot Matters Twice):
#   Explain why `slot` (1..9) predicts both `HR >= 1` and `TB >= 2` even after
#   controlling for batter `ISO`, `SLG`, and `bat_speed`.
#
# CHECKPOINT 3 (Calibrating Rare Events):
#   Inspect `models/bprop01_hr_prop_calibration_2025.parquet`. How closely does
#   the top decile (`mean_pred`) match `obs_rate` across 4,374 locked-2025 starts?
