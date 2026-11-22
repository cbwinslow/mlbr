# ==============================================================================
# mlbr Student Work-Along Lab 15: Daily 9-Man Starting Lineups, Platoon Park
#                                 Asymmetry & Pitcher Strikeout (SO) Props
# ==============================================================================
# Companion to: site/workbooks/lab15_daily_lineups_platoon_k_props_workbook.qmd
# Reference Study: site/analyses/15_daily_lineups_platoon_k_props.qmd (EXP-015)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Poisson Strikeout Prop Pricing (Over 4.5 / 5.5 / 6.5)
# ------------------------------------------------------------------------------
k_prop_fair <- function(lambda) {
  c(P_Over_4_5 = 1 - ppois(4, lambda),
    P_Over_5_5 = 1 - ppois(5, lambda),
    P_Over_6_5 = 1 - ppois(6, lambda))
}
cat("--- Part 1: Poisson Strikeout Prop Fair Probabilities (lambda = 6.0 K Ace) ---\n")
print(round(100 * k_prop_fair(6.0), 1))
cat("\n")

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit Over EXP-015 Gold Tables
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

kp_p <- file.path(lake, "data/processed/sp_strikeout_prop_backtest_2024_2025.parquet")
sql_kp <- sprintf("
  SELECT
    season,
    COUNT(*) AS n_starts,
    ROUND(AVG(exp_so_m2), 2) AS mean_exp_so,
    ROUND(AVG(sp_actual_so), 2) AS mean_act_so,
    ROUND(100.0 * AVG(p_over_5_5), 1) AS pred_o55_pct,
    ROUND(100.0 * AVG(y_over_5_5), 1) AS obs_o55_pct
  FROM read_parquet('%s')
  GROUP BY season
  ORDER BY season
", kp_p)
cat("--- Part 2A: Zero-Copy DuckDB SQL Summary of Starter K-Props (2024-2025) ---\n")
print(DBI::dbGetQuery(con, sql_kp))

ab <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/sp_k_prop_ablation_metrics_2025.parquet")))
cat("\n--- Part 2B: Locked 2025 OOS Strikeout Prop Ablation ---\n")
print(ab)

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Opposing 9-Man Lineup K% vs. Realized SP Strikeouts
# ------------------------------------------------------------------------------
sp <- as.data.table(arrow::read_parquet(kp_p))[season == 2025L]
sp[, lu_k_bin := cut(100 * opp_lineup_k, breaks = c(15, 20.5, 22.0, 23.5, 25.0, 35),
                     labels = c("<20.5%", "20.5-22.0%", "22.0-23.5%", "23.5-25.0%", "25.0%+"))]
bin_df <- sp[!is.na(lu_k_bin), .(n_starts = .N, mean_so = mean(sp_actual_so),
                                 o55_rate = 100 * mean(y_over_5_5)), by = lu_k_bin]

p_diag <- ggplot(bin_df, aes(x = lu_k_bin, y = mean_so, fill = o55_rate)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.2f K\n(O5.5: %.1f%%)", mean_so, o55_rate)),
            vjust = -0.35, color = "#f8fafc", size = 3.1, fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", name = "Over 5.5 K\nHit Rate (%)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "Lab 15 Diagnostic: Opposing 9-Man Starting Lineup K% vs. Realized SP Strikeouts (2025)",
    subtitle = "Strictly pre-game prior-season lineup K% cleanly separates low-K contact lineups from high-K strikeout targets",
    x = "Opposing 9-Man Starting Lineup Prior-Season K% Bucket",
    y = "Mean Realized Starting Pitcher Strikeouts per Start (2025)",
    caption = "Source: mlbr Lab 15 (EXP-015 Daily Lineups & SP Strikeout Prop Engine)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab15_lineup_k_vs_sp_so.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 15 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Poisson Overdispersion Check):
#   In `sp` (2025), compare `var(sp_actual_so)` vs. `mean(sp_actual_so)`.
#   Is MLB starter strikeout count slightly overdispersed relative to a pure Poisson
#   distribution, and how would a Negative Binomial model adjust tail probabilities (`Over 8.5 K`)?
#
# CHECKPOINT 2 (Opposing Lineup K% Alpha):
#   In `ab`, quantify the locked-2025 Over 5.5 K ROC AUC lift from `M1_SP_Prior_Only`
#   (`0.6409`) to `M2_Pro_K_Prop_Engine` (`0.6638`). Why is player-prop AUC so much
#   higher than full-game Moneyline AUC (`0.574`)?
#
# CHECKPOINT 3 (Yankee Stadium Short Porch vs. Fenway Green Monster):
#   Inspect `models/kprop03_platoon_park_lineup_asymmetry.parquet`. Which 3 MLB
#   ballparks exhibit the strongest LHB vs. RHB home-run asymmetry?
