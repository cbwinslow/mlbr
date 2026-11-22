# ==============================================================================
# mlbr Student Work-Along Lab 10: Level 5 Market Edge, Closing Line Value (CLV),
#                                 Fractional Kelly Sizing & Prediction Markets
# ==============================================================================
# Companion to: site/workbooks/lab10_market_edge_prediction_valuation_workbook.qmd
# Reference Study: site/analyses/10_market_edge_prediction_valuation.qmd (EXP-010)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles EV, Fractional Kelly & CLV Hand-Calculation
# ------------------------------------------------------------------------------
ev_hand <- function(p, dec_odds) p * (dec_odds - 1.0) - (1.0 - p)
kelly_hand <- function(p, dec_odds, frac = 0.25) {
  b <- dec_odds - 1.0
  max(0.0, frac * ((p * b - (1.0 - p)) / b))
}
clv_hand <- function(o_entry, o_close) (o_entry / o_close) - 1.0

cat("--- Part 1: Binary Contract Pricing (Yankees YES @ 55c vs. Fair 60%) ---\n")
p_fair  <- 0.60
o_entry <- 1.0 / 0.55   # 1.8182 decimal odds (55c on Kalshi/Polymarket)
o_close <- 1.0 / 0.58   # Market closes at 58c -> 1.7241 decimal odds

cat(sprintf("Fair Prob = %.1f%% | Entry Price = 55.0c (O=%.4f) | Close = 58.0c (O=%.4f)\n",
            100 * p_fair, o_entry, o_close))
cat(sprintf("Expected Value (EV) = %+.2f%% | CLV = %+.2f%%\n",
            100 * ev_hand(p_fair, o_entry), 100 * clv_hand(o_entry, o_close)))
cat(sprintf("Full Kelly = %.2f%% | Quarter Kelly (c=0.25) = %.2f%% | Eighth Kelly (c=0.125) = %.2f%%\n\n",
            100 * kelly_hand(p_fair, o_entry, 1.0),
            100 * kelly_hand(p_fair, o_entry, 0.25),
            100 * kelly_hand(p_fair, o_entry, 0.125)))

# ------------------------------------------------------------------------------
# Part 2: Inspecting Gold EXP-010 Market Backtest & 2026 Futures Tables
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(arrow)
  library(data.table)
  library(DBI)
  library(duckdb)
  library(ggplot2)
})

lake <- mlbr::default_lake()

# Zero-Copy DuckDB SQL Audit over Gold EXP-010 Backtest Parquet
con <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")
bt_parquet <- file.path(lake, "data/processed/market_valuation_backtest_2024_2025.parquet")
sql_summary <- DBI::dbGetQuery(con, sprintf("
  SELECT
    season,
    COUNT(*) AS n_games,
    SUM(CASE WHEN ev_best >= 0.02 THEN 1 ELSE 0 END) AS n_ev2_bets,
    ROUND(100.0 * AVG(CASE WHEN ev_best >= 0.02 THEN bet_won END), 2) AS ev2_win_pct,
    ROUND(100.0 * SUM(CASE WHEN ev_best >= 0.02 THEN pnl_flat_1u ELSE 0 END) /
          NULLIF(SUM(CASE WHEN ev_best >= 0.02 THEN 1 ELSE 0 END), 0), 2) AS ev2_flat_roi_pct
  FROM read_parquet('%s')
  GROUP BY season
  ORDER BY season
", bt_parquet))
cat("--- Part 2A: Zero-Copy DuckDB SQL Summary (EXP-010 Backtest by Season) ---\n")
print(sql_summary)
cat("\n")
gm   <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/market_valuation_backtest_2024_2025.parquet")))
st   <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/market_staking_summary_2024_2025.parquet")))
fut  <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/market_2026_futures_valuation.parquet")))

cat("--- Part 2: Locked 2025 OOS Staking Regimes (Sportsbook -105, EV >= +2%) ---\n")
print(st[split_name == "Locked 2025 OOS" & grepl("105", vig_tier) & abs(ev_min - 0.02) < 1e-9,
         .(regime, n_bets, roi_pct = round(roi_pct, 2), final_bankroll = round(final_bankroll, 1),
           mean_clv_pct = round(mean_clv_pct, 2), daily_sharpe = round(daily_sharpe, 2),
           max_drawdown_pct = round(max_drawdown_pct, 1))])

cat("\n--- Top 6 Clubs: 2026 Playoff & World Series Fair Prices vs. Polymarket ---\n")
print(head(fut[, .(team = team_statcast, proj_wins = round(mean_wins, 1),
                   playoff_c = round(fair_playoff_cents, 1),
                   mlbr_ws_c = round(fair_ws_cents, 2),
                   pm_ws_c = round(100 * pm_ws_mid, 2),
                   edge_pp = round(ws_edge_pct, 2))], 6))

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Visualization — Expected Log-Growth Curve g(f) vs. Leverage
# ------------------------------------------------------------------------------
p_ex <- 0.56; b_ex <- 1.0 / 0.52 - 1.0  # +7.7% EV binary contract
f_star <- (p_ex * b_ex - (1.0 - p_ex)) / b_ex
f_grid <- seq(0.0, 2.3 * f_star, length.out = 200)
g_grid <- p_ex * log(1.0 + f_grid * b_ex) + (1.0 - p_ex) * log(1.0 - f_grid)

df_curve <- data.table(
  f_pct = 100.0 * f_grid,
  g_bps = 10000.0 * g_grid
)
pts <- data.table(
  label = c("Eighth Kelly (c=0.125)", "Quarter Kelly (c=0.25)", "Full Kelly (c=1.0)", "Over-Betting (2.0x)"),
  f_pct = 100.0 * c(0.125, 0.25, 1.0, 2.0) * f_star
)
pts[, g_bps := 10000.0 * (p_ex * log(1.0 + (f_pct / 100) * b_ex) + (1.0 - p_ex) * log(1.0 - (f_pct / 100)))]

p_diag <- ggplot(df_curve, aes(x = f_pct, y = g_bps)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#f43f5e", linewidth = 0.7) +
  geom_line(color = "#38bdf8", linewidth = 1.4) +
  geom_point(data = pts, aes(x = f_pct, y = g_bps), color = "#10b981", size = 3.5) +
  geom_text(data = pts, aes(x = f_pct, y = g_bps, label = label),
            vjust = -1.0, hjust = 0.5, color = "#f8fafc", size = 3.1, fontface = "bold") +
  labs(
    title = "Lab 10 Diagnostic: Kelly Expected Log-Growth Curve g(f) & Over-Betting Cliff",
    subtitle = sprintf("Example: Fair p = 56%% vs. Market q = 52%% (f* = %.1f%%) | Quarter-Kelly captures ~44%% of growth with 1/16th variance!",
                       100 * f_star),
    x = "Bet Fraction of Bankroll f (%)",
    y = "Expected Log-Growth per Bet (bps)",
    caption = "Source: mlbr Lab 10 (EXP-010) — Over-betting beyond 2f* yields strictly negative long-run compounding!"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab10_kelly_growth_curve.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 10 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete these exercises in RStudio!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Overround & Vig Stripping):
#   Verify sum(fut$pm_ws_mid) vs. sum(fut$pm_ws_novig). Why does an early-season
#   Polymarket futures book sum to ~191.7%, and how does vig stripping change
#   which clubs appear undervalued?
#
# CHECKPOINT 2 (Drawdown vs. Growth Trade-Off):
#   In `st` for Locked 2025 OOS (`SB-105`, `ev_min == 0.02`), compare `Full_Kelly`
#   vs. `Eighth_Kelly` on `final_bankroll`, `roi_pct`, and `max_drawdown_pct`.
#   Why can Eighth-Kelly beat Full-Kelly on ROI% while cutting max drawdown in half?
#
# CHECKPOINT 3 (Fee Sensitivity in Prediction Markets):
#   Compare `Quarter_Kelly` on Locked 2025 OOS at `ev_min == 0.02` between
#   Sportsbook `-105` (2.38% vig) and Exchange `-102` (1.00% fee). How many extra
#   units of bankroll are unlocked simply by reducing transaction costs by 1.38%?
