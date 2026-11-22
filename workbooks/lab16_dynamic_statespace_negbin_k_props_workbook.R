# ==============================================================================
# mlbr Student Work-Along Lab 16: Dynamic In-Season State-Space Updates &
#                                 Negative Binomial Overdispersed K-Props
# ==============================================================================
# Companion to: site/workbooks/lab16_dynamic_statespace_negbin_k_props_workbook.qmd
# Reference Study: site/analyses/16_dynamic_statespace_negbin_k_props.qmd (EXP-016)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Poisson vs. Negative Binomial Tail Comparison
# ------------------------------------------------------------------------------
mu_ex <- 6.0; theta_ex <- 46.89
cat("--- Part 1: Poisson vs. Negative Binomial Tail Probabilities (mu = 6.0 K) ---\n")
cat(sprintf("P(SO >= 8 / Over 7.5 K): Poisson = %.2f%% | NegBin (theta=%.1f) = %.2f%%\n",
            100 * (1 - ppois(7, mu_ex)), theta_ex, 100 * (1 - pnbinom(7, size = theta_ex, mu = mu_ex))))
cat(sprintf("P(SO <= 2 / Under 2.5 K): Poisson = %.2f%% | NegBin (theta=%.1f) = %.2f%%\n\n",
            100 * ppois(2, mu_ex), theta_ex, 100 * pnbinom(2, size = theta_ex, mu = mu_ex)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit Over Silver statcast_biomechanics & EXP-016
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

silv_p <- file.path(lake, "data/features/statcast_biomechanics/**/*.parquet")
sql_silv <- sprintf("
  SELECT
    game_year,
    COUNT(*) AS n_pitches,
    COUNT(ivb_above_slot) AS n_slot,
    COUNT(bat_speed_dbl) AS n_swings,
    SUM(is_blast) AS n_blasts,
    ROUND(AVG(csaa_pitch), 5) AS mean_csaa
  FROM read_parquet('%s', hive_partitioning = true)
  GROUP BY game_year
  ORDER BY game_year
", silv_p)
cat("--- Part 2A: Zero-Copy DuckDB SQL Audit of Silver statcast_biomechanics (2020-2026) ---\n")
print(DBI::dbGetQuery(con, sql_silv))

ab <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/dynamic_negbin_ablation_metrics_2025.parquet")))
cat("\n--- Part 2B: Locked 2025 OOS Dynamic State-Space & NegBin Ablation ---\n")
print(ab)

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Trailing 30-Day In-Season CSW% vs. Realized SO/GS
# ------------------------------------------------------------------------------
kp <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet")))
kp25 <- kp[season == 2025L]
kp25[, csw_bin := cut(100 * sp_csw_30d, breaks = c(15, 26, 28, 30, 32, 45),
                      labels = c("<26%", "26-28%", "28-30%", "30-32%", "32%+"))]
c_df <- kp25[!is.na(csw_bin), .(n_starts = .N, mean_so = mean(sp_actual_so),
                                o55 = 100 * mean(y_over_5_5)), by = csw_bin]

p_diag <- ggplot(c_df, aes(x = csw_bin, y = mean_so, fill = o55)) +
  geom_col(width = 0.65, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.2f K\n(O5.5: %.1f%%)", mean_so, o55)),
            vjust = -0.35, color = "#f8fafc", size = 3.1, fontface = "bold") +
  scale_fill_viridis_c(option = "viridis", name = "Over 5.5 K\nHit Rate (%)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "Lab 16 Diagnostic: Strictly Pre-Game Trailing 30-Day CSW% vs. 2025 Strikeouts",
    subtitle = "Starters entering with 32%+ trailing 30-day CSW% average 6+ K/start and dominate Over 5.5 K prop markets",
    x = "Starter Trailing 30-Day Pre-Game CSW% (EB-Shrunk M=150p)",
    y = "Observed Strikeouts per Start (2025 Regular Season)",
    caption = "Source: mlbr Lab 16 (EXP-016 Dynamic State-Space Engine)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab16_csw30d_vs_sp_so.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 16 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Overdispersion Physics):
#   In `kp25`, verify `var(sp_actual_so) / mean(sp_actual_so) == 1.28`. Why is
#   MLB starter strikeout variance 28% wider than a pure Poisson distribution?
#
# CHECKPOINT 2 (Why 30d Pitcher CSW% Works While 30d Team xwOBA Adds Noise):
#   Compare the `+0.0141` Over 5.5 K AUC lift from trailing 30d pitcher CSW%/K%
#   against the `+0.00095` Full-Game ML Brier degradation from 30d club xwOBA.
#
# CHECKPOINT 3 (Silver Tier Speed & Zero-Sum Framing Audit):
#   In `sql_silv`, confirm `mean_csaa == 0` across all 7 seasons (`2020-2026`,
#   4.55M pitches) in `data/features/statcast_biomechanics/`.
