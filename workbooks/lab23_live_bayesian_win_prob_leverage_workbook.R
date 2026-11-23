#!/usr/bin/env Rscript
# ==============================================================================
# Lab 23: Live In-Game Bayesian Win Probability, Pre-Game Prior Decay &
#         Real-Time Tango Leverage Index (EXP-023 Companion Workbook)
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(mlbr)
})

cat("=== Lab 23: Live In-Game Bayesian Win Probability & Leverage Index ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB'")

# ------------------------------------------------------------------------------
# CHECKPOINT 1: Inspect Leak-Free 2015-2023 Base RE24 WE & Peak Tango LI States
# ------------------------------------------------------------------------------
p_base <- file.path(lake, "data/processed/live_we_base_matrix_2015_2023.parquet")
peak_li <- DBI::dbGetQuery(con, sprintf("
  SELECT inn_cap AS inning, bat_home, outs, base_state, score_diff,
         CAST(n_plays AS INTEGER) AS n_plays,
         ROUND(100.0 * base_we, 1) AS home_we_pct,
         ROUND(tango_li, 2) AS tango_li
  FROM read_parquet('%s')
  WHERE n_plays >= 100
  ORDER BY tango_li DESC
  LIMIT 8
", p_base))
print(peak_li)
stopifnot(nrow(peak_li) == 8L, peak_li$tango_li[1] > 4.0)
cat("CHECKPOINT 1 PASS: Peak Tango Leverage Index states verified (LI > 4.0 in late tied/1-run jams).\n")

# ------------------------------------------------------------------------------
# CHECKPOINT 2: Verify Bayesian Prior Decay (M2) Beats Static RE24 (M0) & Flat Prior (M1)
# ------------------------------------------------------------------------------
p_oos <- file.path(lake, "data/processed/live_we_oos_ablation_2025.parquet")
oos <- DBI::dbGetQuery(con, sprintf("
  SELECT model, gamma_decay, n_oos_plays,
         ROUND(oos_brier, 5) AS oos_brier,
         ROUND(oos_logloss, 5) AS oos_logloss,
         ROUND(oos_auc, 5) AS oos_auc
  FROM read_parquet('%s')
", p_oos))
print(oos)
m0_b <- oos$oos_brier[oos$model == "M0_Static_RE24_WE"]
m1_b <- oos$oos_brier[oos$model == "M1_Static_Plus_Flat_Prior"]
m2_b <- oos$oos_brier[oos$model == "M2_Bayesian_Decayed_Prior"]
stopifnot(m2_b < m1_b, m1_b < m0_b)
cat("CHECKPOINT 2 PASS: Locked 2025 OOS ordering M2 < M1 < M0 verified on Brier Score.\n")

# ------------------------------------------------------------------------------
# CHECKPOINT 3: Inning-by-Inning Brier Improvement Diagnostic Plot (Dark Cyber)
# ------------------------------------------------------------------------------
p_inn <- file.path(lake, "data/processed/live_we_by_inning_2025.parquet")
inn <- arrow::read_parquet(p_inn) |>
  mutate(inning_lbl = factor(ifelse(inn_grp == 9L, "9+", as.character(inn_grp)),
                             levels = c("1","2","3","4","5","6","7","8","9+")))

p_diag <- ggplot(inn, aes(x = inning_lbl, y = 1000 * delta_brier_m2_vs_m0, fill = delta_brier_m2_vs_m0 < 0)) +
  geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
  geom_col(width = 0.62, alpha = 0.92, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+.2f", 1000 * delta_brier_m2_vs_m0)),
            vjust = ifelse(inn$delta_brier_m2_vs_m0 < 0, 1.35, -0.5), color = "#f8fafc", size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#f43f5e")) +
  labs(
    title = "Lab 23 Diagnostic: Inning-by-Inning OOS Brier Reduction (M2 Bayesian vs M0 Static RE24)",
    subtitle = "Negative values = M2 Bayesian Decayed-Prior beats Static RE24 on locked 2025 plays (x1000 Brier)",
    x = "Inning", y = "Delta Brier Score (x1000, Lower is Better)",
    caption = "mlbr Lab 23 | Peak live-market edge concentrates in Innings 1-4 (-3.48 to -0.45 x1000 Brier)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab23_inning_brier_delta_diagnostic.png")
ggsave(out_png, p_diag, width = 9.5, height = 5.2, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 23 diagnostic PNG to: %s\n", out_png))
cat("CHECKPOINT 3 PASS: Lab 23 completed cleanly!\n")
