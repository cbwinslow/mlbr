# CLAUDE.md - Operating Contract & Quantitative Research Protocol for mlbr

This file defines the mandatory working contract, behavioral guidelines, and engineering standards for all AI agents working in `/mnt/storage/Rprojects/mlbr`.

---

## 1. Core Mission & Grand Architecture

`mlbr` is a production-grade Sabermetrics research platform, data lake, and quantitative game prediction engine.

The ultimate objective is a **5-tier hierarchical modeling system** building from atomic pitch physics to predictive game simulation and prediction market valuation (Kalshi, Polymarket):

```
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 5: Market Edge & Prediction Valuation                            │
│  • Sources: Polymarket, Kalshi, traditional sportsbook implied odds   │
│  • Evaluation: Expected Value (EV), Brier Score calibration, ROI       │
│  • Staking: Fractional Kelly Criterion bankroll management             │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Evaluates Mispricings
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 4: Game-Level Win Probability & Simulation Engine                │
│  • Monte Carlo Inning-by-Inning & Plate-Appearance Simulator           │
│  • Matchup Mechanics: Log5 odds ratio, Bradley-Terry, ML Classifiers   │
│  • Accounts for: Starting pitcher stamina, bullpen usage, home bias    │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Simulates From
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 3: Team Run Creation & Prevention (True Talent)                  │
│  • BaseRuns (David Smyth) & Pythagenpat Expected Win Matrix            │
│  • Park Factors (3-year regressed) & Defensive Efficiency (OAA, UZR)  │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Aggregates From
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 2: Player-Level Projections & Aging Curves                       │
│  • Marcel the Monkey (Tango): 5/4/3 weighting + league mean regression │
│  • ZiPS (Szymborski): Multi-year aging curves & career trajectory comps│
│  • PECOTA (Silver): Nearest-Neighbor k-NN comps in similarity space   │
│  • Scikit-learn / R equivalents: Mahalanobis distance, GMMs, PCA       │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Grounded In
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 1: Atomic Pitch & Batted-Ball Physics (Tracked Quality)          │
│  • xWhiff / Stuff+: Pitch quality isolated from defense & luck         │
│  • xwOBA / Exit Velocity Surface: True damage on contact               │
│  • RE24 Run Expectancy: Contextual equity of every ball, strike, out  │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Mandatory Research Protocol: Q-CRISP & 7-Section Study Template

All analytical investigations and models MUST follow the **Q-CRISP (Quantitative CRISP-DM)** framework and be structured into the canonical **7-Section Study Template** for both research documentation and Quarto website publication:

1. **Section 1: Executive Summary & Abstract (TL;DR)** — Bottom line up front, key findings, headline visualization.
2. **Section 2: Domain Problem & Theoretical Physics** — Perceptual/biomechanical mechanisms, historical context, why linear models fail.
3. **Section 3: Mathematical Formulation & Lake-Wide Pipeline** — Formal KaTeX formulas and full-population DuckDB SQL queries.
4. **Section 4: Exploratory Data Analysis (EDA) & Feature Diagnostics** — 2D density surfaces (hexbins), correlation matrices, mutual information ranking.
5. **Section 5: Model Architecture & Temporal Validation** — Out-of-season temporal validation (train past -> test future), calibration curves, benchmark comparisons.
6. **Section 6: Leaderboards & Player Case Studies** — Model rankings, outlier detection (undervalued/overvalued arms and hitters).
7. **Section 7: Downstream Integration & Market Applications** — Connection to player projections (Level 2) and game betting edge (Levels 4–5).

---

## 3. Data Lake Invariants & Population Mandate

1. **Full-Population Invariant (NO Arbitrary Subsampling)**:
   - Feature engineering and model scoring must be calculated and materialized **lake-wide across all 14,643,543 pitches and 125 historical seasons**.
   - Subsampling or small slices are **strictly restricted** to interactive prototyping probes or rendering raw vector scatter plots that would otherwise crash web browsers.
2. **Zero-Drop Raw Invariant**:
   - Raw Parquet partitions must retain 100% of upstream columns. Never drop fields during ingestion or storage.
3. **Strict Temporal Validation (No Look-Ahead Leakage)**:
   - Models must NEVER train on future game dates to predict the past. Training splits must be strictly chronological.
4. **Dynamic Path Resolution**:
   - Always resolve paths via `mlbr::default_lake()`; never hardcode absolute paths in library or analysis code.

---

## 4. Teaching & Mentoring Protocol (MANDATORY)

1. **Collaborative Tutoring Tone**:
   - The user is using this project as a masterclass in modern sabermetrics, statistical modeling, and quantitative prediction.
   - Explain the *why*, mathematical foundations, and historical sabermetric context (e.g. why Tangotiger chose 5/4/3, why BaseRuns outperforms Linear Weights, the aerodynamics of Magnus lift vs Seam-Shifted Wake).
   - Write didactic, readable code with Google-style documentation and clear comments.
2. **Mandatory Response Structure**:
   Every response MUST include:
   - **Concrete Recommendations** (Actionable immediate next steps)
   - **Pros & Cons Analysis** (Trade-offs for analytical and technical options)
   - **Ways to Improve** (Specific enhancements to current code and next steps)

---

## 5. Key Server Endpoints

* **RStudio Server:** `http://192.168.10.1:8787` (Persistent session; 365-day keepalive; zero idle timeout)
* **Quarto Documentation Site:** `http://192.168.10.1:8088` (Serving `site/docs/`)
* **Host Server:** `cbwdellr720` (40 CPU cores, 125 GB RAM, RAID volume `/mnt/storage/Rprojects/mlbr`)
