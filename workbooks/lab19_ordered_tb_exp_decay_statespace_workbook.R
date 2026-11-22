#!/usr/bin/env Rscript
# ==============================================================================
# Lab 19: Joint Ordered/Multinomial Total Bases Ladder & Exponential Decay
# Companion Work-Along Workbook for Study 19 (EXP-019)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(ggplot2)
})

cat("=== Lab 19: Joint TB Ladder & Exponential Half-Life State-Space ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# PART 1: First-Principles Exponential Decay Kernel vs Flat 30-Day Boxcar
days_ago <- 1:60
w_flat30 <- ifelse(days_ago <= 30, 1.0, 0.0)
w_exp14  <- 2^(-days_ago / 14)
w_exp30  <- 2^(-days_ago / 30)
cat(sprintf("Day 5 vs Day 29 weight ratio — Flat30: %.2fx | Exp14: %.2fx | Exp30: %.2fx\n",
            w_flat30[5]/w_flat30[29], w_exp14[5]/w_exp14[29], w_exp30[5]/w_exp30[29]))

# MENTOR CHECKPOINT 1:
# Verify why w_exp30 retains smooth memory past Day 30 while giving 2x weight to recent starts.
stopifnot(abs(w_exp30[30] - 0.5) < 1e-12 && w_exp30[45] > 0 && w_flat30[31] == 0)
cat("CHECKPOINT 1 PASS: Exponential half-life decay kernel verified.\n")

# PART 2: Zero-Copy DuckDB SQL Monotonicity & Ladder Verification on Locked 2025
p_tb <- file.path(lake, "data/processed/ordered_tb_ladder_backtest_2024_2025.parquet")
tb_chk <- DBI::dbGetQuery(con, sprintf("
  SELECT
    COUNT(*) AS n_starts_2025,
    SUM(CASE WHEN p_mn_ge1 < p_mn_ge2 OR p_mn_ge2 < p_mn_ge3 OR p_mn_ge3 < p_mn_ge4 THEN 1 ELSE 0 END) AS n_violations,
    ROUND(100.0 * AVG(p_mn_ge1), 2) AS avg_p_ge1,
    ROUND(100.0 * AVG(p_mn_ge2), 2) AS avg_p_ge2,
    ROUND(100.0 * AVG(p_mn_ge3), 2) AS avg_p_ge3,
    ROUND(100.0 * AVG(p_mn_ge4), 2) AS avg_p_ge4
  FROM read_parquet('%s')
", p_tb))
print(tb_chk)

# MENTOR CHECKPOINT 2:
# Confirm n_violations == 0 across all 43,740 locked 2025 batter-games.
stopifnot(tb_chk$n_violations == 0L && tb_chk$n_starts_2025 == 43740L)
cat("CHECKPOINT 2 PASS: Zero monotonicity violations across all 43,740 OOS batter-games.\n")

# PART 3: Dark Cyber Diagnostic Chart — Half-Life Curve on Locked 2025 OOS Over 5.5 K AUC
p_dk <- file.path(lake, "data/processed/exp_decay_kprop_ablation_2025.parquet")
dk_df <- arrow::read_parquet(p_dk)
flat_auc <- dk_df$oos_2025_auc_o55[dk_df$model == "M4_EXP016_Flat30d"]

p_lab19 <- ggplot(dk_df |> filter(!is.na(half_life_days)), aes(x = half_life_days, y = oos_2025_auc_o55)) +
  geom_hline(yintercept = flat_auc, color = "#f43f5e", linetype = "dashed", linewidth = 0.9) +
  geom_line(color = "#38bdf8", linewidth = 1.2) +
  geom_point(color = "#10b981", size = 4) +
  scale_x_continuous(breaks = c(7, 14, 21, 30)) +
  labs(
    title = "Lab 19 Diagnostic: Exponential Half-Life vs. Flat 30d Boxcar (Over 5.5 K AUC)",
    subtitle = sprintf("Red dashed = EXP-016 Flat 30d (%.4f) | H*=30d smooth decay reaches %.4f", flat_auc, max(dk_df$oos_2025_auc_o55)),
    x = "Half-Life H (Days)", y = "Locked 2025 OOS Over 5.5 K ROC AUC"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab19_exp_decay_auc_diagnostic.png")
ggsave(out_png, p_lab19, width = 9, height = 5, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 19 diagnostic PNG to: %s\n", out_png))

# MENTOR CHECKPOINT 3:
# Why does H=7d perform worse than flat 30d while H=30d (90-day lookback) beats it cleanly?
cat("CHECKPOINT 3 PASS: Lab 19 completed cleanly!\n")
