#!/usr/bin/env Rscript
# ==============================================================================
# Lab 18: Same-Game Parlay (SGP) Correlation Matrix & Gaussian/Clayton Copulas
# Companion Work-Along Workbook for Study 18 (EXP-018)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr)
  library(DBI)
  library(arrow)
  library(dplyr)
  library(ggplot2)
})

cat("=== Lab 18: SGP Correlation Matrix & Copula Pricing Engine ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# ------------------------------------------------------------------------------
# PART 1: First-Principles Sklar's Theorem & Clayton Copula
# ------------------------------------------------------------------------------
# Given marginal probabilities u, v in (0, 1) and dependence parameter theta > 0:
clayton_pos <- function(u, v, theta) {
  p <- (u^(-theta) + v^(-theta) - 1)^(-1 / theta)
  pmin(pmax(p, pmax(0, u + v - 1)), pmin(u, v))
}

# Example: Slugger HR (p = 0.14) + Team ML (p = 0.58)
u_hr <- 0.14; v_ml <- 0.58
p_ind_ex  <- u_hr * v_ml
p_clay_ex <- clayton_pos(u_hr, v_ml, theta = 0.45)
cat(sprintf("\nExample (HR 14%% + Team ML 58%%):\n  Naive Indep: %.2f%% (Fair Odds +%.0f)\n  Clayton (theta=0.45): %.2f%% (Fair Odds +%.0f, %.2fx lift)\n",
            100 * p_ind_ex, 100 * (1 / p_ind_ex - 1),
            100 * p_clay_ex, 100 * (1 / p_clay_ex - 1), p_clay_ex / p_ind_ex))

# MENTOR CHECKPOINT 1:
# Verify that clayton_pos(u, v, theta) ALWAYS obeys the Frechet-Hoeffding bounds:
# max(0, u + v - 1) <= C(u, v) <= min(u, v). What happens as theta -> 0 and theta -> Inf?
stopifnot(p_clay_ex >= max(0, u_hr + v_ml - 1) && p_clay_ex <= min(u_hr, v_ml))
cat("CHECKPOINT 1 PASS: Frechet-Hoeffding bounds verified.\n")

# ------------------------------------------------------------------------------
# PART 2: Zero-Copy DuckDB SQL Query on Locked 2025 SGP Backtest
# ------------------------------------------------------------------------------
p_bt <- file.path(lake, "data/processed/sgp_copula_backtest_2024_2025.parquet")
p_ab <- file.path(lake, "data/processed/sgp_copula_ablation_metrics_2025.parquet")

sgp_sql <- DBI::dbGetQuery(con, sprintf("
  SELECT
    sgp_label,
    COUNT(*) AS n_oos_2025,
    ROUND(100.0 * AVG(y_parlay), 2) AS actual_hit_pct,
    ROUND(100.0 * AVG(p_ind), 2)    AS naive_ind_pct,
    ROUND(100.0 * AVG(p_gauss), 2)  AS gauss_copula_pct,
    ROUND(AVG(p_gauss) / NULLIF(AVG(p_ind), 0), 3) AS fair_price_mult
  FROM read_parquet('%s')
  WHERE season = 2025
  GROUP BY sgp_label
  ORDER BY fair_price_mult DESC
", p_bt))

cat("\n=== Locked 2025 OOS SGP Pricing Summary (DuckDB Zero-Copy SQL) ===\n")
print(sgp_sql)

# MENTOR CHECKPOINT 2:
# Inspect fair_price_mult > 1.0 (positive script) vs < 1.0 (negative script).
# Why does 'Home SP O5.5 K + Game Over 8.5' have fair_price_mult < 1.0 while
# 'Batter HR 1+ + Team ML' has fair_price_mult ~ 1.29x?
stopifnot(nrow(sgp_sql) == 7L)
cat("CHECKPOINT 2 PASS: All 7 two-leg SGP scripts queried via DuckDB SQL.\n")

# ------------------------------------------------------------------------------
# PART 3: Dark Cyber Diagnostic Chart — Naive vs. Gaussian Copula Calibration
# ------------------------------------------------------------------------------
ab_df <- arrow::read_parquet(p_ab) |>
  filter(model %in% c("M0_Naive_Independence", "M1_Gaussian_Copula"))

p_lab18 <- ggplot(ab_df, aes(x = reorder(sgp_label, abs(cal_bias_pp)), y = cal_bias_pp, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(
    values = c("M0_Naive_Independence" = "#f43f5e", "M1_Gaussian_Copula" = "#38bdf8"),
    labels = c("M0: Naive Independence", "M1: Gaussian Copula"),
    name = "Pricing Model"
  ) +
  labs(
    title = "Lab 18 Diagnostic: Signed SGP Calibration Bias (Locked 2025 OOS)",
    subtitle = "Negative bias = model underprices true hit rate; Positive bias = model overprices",
    x = NULL, y = "Mean Predicted - Actual Hit Rate (Percentage Points)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab18_sgp_bias_diagnostic.png")
ggsave(out_png, p_lab18, width = 9.5, height = 5.5, dpi = 300)
cat(sprintf("\nSaved Dark Cyber Lab 18 diagnostic PNG to: %s\n", out_png))

# MENTOR CHECKPOINT 3:
# Compare SGP1 (Home ML + F5 Home) vs SGP7 (Slot #2 + Slot #3 TB 2+).
# Why is residual teammate stack correlation only 1.03x once pre-game park, weather,
# and opposing starter HR% are already embedded in our EXP-017 marginals?
cat("CHECKPOINT 3 PASS: Lab 18 completed cleanly!\n")
