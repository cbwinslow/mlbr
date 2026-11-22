# ==============================================================================
# mlbr Student Work-Along Lab 14: Unified Pro Multi-Market Pricing (ML, F5,
#                                 Totals) & Exact 12-Team Playoff Bracket MC
# ==============================================================================
# Companion to: site/workbooks/lab14_unified_pro_game_f5_totals_bracket_workbook.qmd
# Reference Study: site/analyses/14_unified_pro_game_f5_totals_bracket.qmd (EXP-014)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Best-of-3 vs. Best-of-7 Log5 Series Probability Math
# ------------------------------------------------------------------------------
bo3_prob <- function(p) p^2 + 2 * (p^2) * (1 - p)
bo7_prob <- function(p) sum(dbinom(4:7, size = 7, prob = p))

cat("--- Part 1: Why Skipping the Best-of-3 Wild Card Round Is Huge (p = 0.56 Favorite) ---\n")
cat(sprintf("Single-Game p = 56.0%% | Bo3 Series Win Prob = %.1f%% | Bo7 Series Win Prob = %.1f%%\n\n",
            100 * bo3_prob(0.56), 100 * bo7_prob(0.56)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit Over EXP-014 Gold Multi-Market Tables
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

br_p <- file.path(lake, "data/processed/mlb_2026_12team_bracket_ws_odds.parquet")
sql_br <- sprintf("
  SELECT
    league,
    COUNT(*) AS n_clubs,
    ROUND(SUM(bye_prob), 3) AS sum_byes,
    ROUND(SUM(playoff_prob_12), 3) AS sum_playoffs,
    ROUND(SUM(pennant_prob), 4) AS sum_pennant,
    ROUND(SUM(ws_prob_12team), 4) AS sum_ws
  FROM read_parquet('%s')
  GROUP BY league
  ORDER BY league
", br_p)
cat("--- Part 2A: Zero-Copy DuckDB SQL Verification of Exact 12-Team Bracket Invariants ---\n")
print(DBI::dbGetQuery(con, sql_br))

br <- as.data.table(arrow::read_parquet(br_p))
cat("\n--- Part 2B: Top 8 Clubs — Exact 12-Team Bracket vs. EXP-010 Heuristic Proxy ---\n")
print(head(br[, .(team = team_statcast, lg = league, bye = round(100 * bye_prob, 1),
                  pennant = round(100 * pennant_prob, 1), ws_12 = round(100 * ws_prob_12team, 2),
                  ws_proxy = round(100 * exp010_ws_proxy, 2),
                  bye_boost_pp = round(100 * (ws_prob_12team - exp010_ws_proxy), 2))], 8))

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — First-Round Bye Boost (Exact 12-Team vs. Heuristic Proxy)
# ------------------------------------------------------------------------------
br[, bye_boost_pp := 100.0 * (ws_prob_12team - exp010_ws_proxy)]
p_diag <- ggplot(br, aes(x = 100 * bye_prob, y = bye_boost_pp, color = league)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.7) +
  geom_point(size = 3.2, alpha = 0.90) +
  geom_text(data = br[abs(bye_boost_pp) > 0.6], aes(label = team_statcast),
            vjust = -0.8, size = 2.9, color = "#f8fafc", fontface = "bold", check_overlap = TRUE) +
  scale_color_manual(values = c("AL" = "#38bdf8", "NL" = "#10b981"), name = "League") +
  labs(
    title = "Lab 14 Diagnostic: How MLB's Top-2 First-Round Bye Reshapes World Series Odds",
    subtitle = "Clubs with ~40% Bye probability (NYY, LAD) gain +2.8 to +4.1 pp in WS probability vs. a bye-blind heuristic proxy",
    x = "2026 First-Round Wild Card Bye Probability (%)",
    y = "Exact 12-Team Bracket minus EXP-010 Proxy (WS Prob pp)",
    caption = "Source: mlbr Lab 14 (EXP-014 50,000-Season Exact 12-Team Playoff Bracket Simulator)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab14_bye_boost_ws_odds.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 14 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Why F5 Beats Full-Game on ROC AUC):
#   In `unified_pro_game_f5_totals_2024_2025.parquet` (`season == 2025L`), compare
#   locked-2025 ROC AUC of `p_m6_f5` (`0.5846`) vs. `p_m6_ml` (`0.5742`). Why does
#   isolating innings 1-5 sharpen starter-level discrimination?
#
# CHECKPOINT 2 (Weather + Umpire + Bullpen Fatigue Totals Lift):
#   Compare locked-2025 Total-Runs RMSE of `exp_total_runs` (`4.5862`) vs.
#   `exp_total_m6` (`4.5732`) in `unified_pro_multi_market_metrics.parquet`.
#
# CHECKPOINT 3 (AL vs. NL Structural Pennant Value):
#   Why does Seattle (`87.2 W`, `AL`) have a higher Pennant probability (`13.9%`)
#   than Atlanta (`88.7 W`, `NL`, `13.8%`) under the exact 12-team bracket?
