# Specification 10: Level 5 Market Edge, Pricing Efficiency & Prediction Market Valuation

- **Experiment ID:** `EXP-010-MARKET-EDGE-PREDICTION-VALUATION`
- **Architecture Tier:** Level 5 (Market Edge, CLV, Prediction Market Pricing & Bankroll Optimization)
- **Status:** `VALIDATED`
- **Upstream Dependencies:**
  - Level 4 Simulation Probabilities: `data/processed/game_simulation_backtest_2024_2025.parquet` (`EXP-009`)
  - Level 3 2026 Season Projections: `data/processed/team_standings_projections_2026.parquet` (`EXP-008`)
  - Market Archives: `data/raw/markets/` (Polymarket / Kalshi orderbook snapshots via `R/ingest/archive_market_snapshots.py`)
- **Downstream Consumers:** Automated execution, portfolio allocation, closing line value (CLV) performance tracking.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Expected Value (EV) & Edge Calculation
For any binary outcome market (e.g. Kalshi "Will Yankees win on 2025-06-15?" or standard moneyline with decimal price $O$):
$$\text{Market Implied Probability } q = \frac{1}{O}$$
$$\text{Expected Value } \text{EV} = p \cdot (O - 1) - (1 - p) = p \cdot O - 1 = \frac{p - q}{q}$$
where $p$ is the chronologically calibrated Level 4 fair-value probability (`p_fair = w_M3 * p_m3 + (1 - w_M3) * p_m4`, with `w_M3` fit strictly on `2024`).

### 1.2 Fractional Kelly Criterion Bankroll Growth
To maximize the expected logarithmic growth of bankroll $W$ while compressing drawdowns, bets are sized using fractional Kelly ($f^* = \min(f_{\max}, c \cdot \text{Kelly})$, typically $c = 0.25$ or $0.125$, $f_{\max} = 0.05$, daily slate cap $35\%$):
$$f^* = \min\!\left(f_{\max},\; c \cdot \max\!\left(0,\; \frac{p \cdot b - (1 - p)}{b}\right)\right)$$
where $b = O - 1$ is the net decimal odds.

### 1.3 Closing Line Value (CLV) & Market Overround
The gold standard of predictive edge in quantitative finance and sports modeling is beating the closing price:
$$\text{CLV} = \frac{O_{\text{entry}}}{O_{\text{close}}} - 1$$
For multi-outcome futures markets (e.g., 30-team World Series prediction markets), the structural vig/overround is:
$$\nu = \sum_{i=1}^{30} q_i^{\text{mid}} - 1$$

---

## 2. Validation & Benchmark Strategy
- **Strictly Chronological Split (`2024` Train -> Locked `2025` OOS):**
  - Fit Level 4 fair-value blend weight strictly on `2024` (`n = 2,429` games); evaluate strictly out-of-sample on locked `2025` (`n = 2,430` games) as well as full `2024–2025` (`4,859` games).
  - Compare four staking regimes: Flat 1-unit, Full Kelly ($c = 1.0$), Quarter Kelly ($c = 0.25$), and Eighth Kelly ($c = 0.125$) across four EV thresholds (`+1%`, `+2%`, `+3%`, `+5%`) and two fee tiers (Sportsbook `-105` `2.38%` vig vs. Exchange `-102` `1.00%` fee).
  - Audit live archived Polymarket/Kalshi orderbook overround and bid-ask spreads against 30-club 2026 fair futures probabilities.

---

## 3. Implementation Deliverables
- Model: `models/10_market_edge_prediction_valuation.R` (`--smoke` / `--full`)
- Gold Outputs: `data/processed/market_valuation_backtest_2024_2025.parquet`, `data/processed/market_staking_summary_2024_2025.parquet`, `data/processed/market_2026_futures_valuation.parquet`, `data/processed/market_oos_model_calibration.parquet`
- Study: `site/analyses/10_market_edge_prediction_valuation.qmd`
- Lab: `workbooks/lab10_market_edge_prediction_valuation_workbook.R` & `site/workbooks/lab10_market_edge_prediction_valuation_workbook.qmd`
- Visuals: Dark Cyber 4-asset suites (`models/mkt01_*`, `models/mkt02_*`, `models/mkt03_*`).
