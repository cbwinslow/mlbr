# Specification 09: Level 5 Market Edge, Pricing Efficiency & Prediction Market Valuation

- **Experiment ID:** `EXP-009-MARKET-EDGE-PREDICTION-VALUATION`
- **Architecture Tier:** Level 5 (Market Edge, CLV, Prediction Market Pricing & Bankroll Optimization)
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - Level 4 Simulation Probabilities: `data/processed/game_simulation_backtest_2024_2025.parquet` (`EXP-008`)
  - Market Archives: `data/raw/markets/` (Polymarket / Kalshi orderbook snapshots via `R/ingest/archive_market_snapshots.py`)
  - Historical Betting Feeds: Game closing lines, moneylines, run lines, and totals
- **Downstream Consumers:** Automated execution, portfolio allocation, closing line value (CLV) performance tracking.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Expected Value (EV) & Edge Calculation
For any binary outcome market (e.g. Kalshi "Will Yankees win on 2025-06-15?" or standard moneyline with decimal price $O$):
$$\text{Market Implied Probability } q = \frac{1}{O}$$
$$\text{Expected Value } \text{EV} = p \cdot (O - 1) - (1 - p) = \frac{p - q}{q}$$
where $p$ is the Level 4 Monte Carlo simulated model probability.

### 1.2 Fractional Kelly Criterion Bankroll Growth
To maximize the expected logarithmic growth of bankroll $W$ while minimizing drawdowns, bets are sized using fractional Kelly ($f^* = c \cdot \text{Kelly}$, typically $c = 0.25$ or $0.125$):
$$f^* = c \cdot \frac{p \cdot (O - 1) - (1 - p)}{O - 1} = c \cdot \frac{p \cdot b - (1 - p)}{b}$$
where $b = O - 1$ is the net decimal odds.

### 1.3 Closing Line Value (CLV) Metric
The gold standard of predictive edge in quantitative finance and sports modeling is beating the closing price:
$$\text{CLV} = \frac{O_{\text{entry}}}{O_{\text{close}}} - 1$$
Consistently achieving positive CLV ($\text{mean}(\text{CLV}) > 0$) proves predictive alpha independent of short-term variance.

---

## 2. Validation & Benchmark Strategy
- **Historical Out-of-Time Backtest (2024–2025 seasons):**
  - Simulate dynamic bankroll evolution under:
    - Fixed 1-unit staking
    - Full Kelly ($c = 1.0$)
    - Quarter Kelly ($c = 0.25$)
    - Eighth Kelly ($c = 0.125$)
  - Evaluate:
    - Brier score vs. consensus market implied probability
    - ROI (% return on capital deployed)
    - Sharpe ratio of daily PnL
    - Maximum drawdown duration and depth.

---

## 3. Implementation Deliverables
- Model: `models/09_market_edge_prediction_valuation.R`
- Output: `data/processed/market_valuation_backtest_2024_2025.parquet`
- Study: `site/analyses/09_market_edge_prediction_valuation.qmd`
- Lab: `workbooks/lab09_market_edge_prediction_valuation_workbook.R` & `site/workbooks/lab09_market_edge_prediction_valuation_workbook.qmd`
- Visuals: Dark Cyber 4-asset suites (`models/mkt01_*`, `models/mkt02_*`).

