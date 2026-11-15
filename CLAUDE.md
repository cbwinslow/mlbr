# CLAUDE.md - Operating Contract & Development Framework for mlbr

This file defines the mandatory working contract, behavioral guidelines, and engineering standards for all AI agents working in `/mnt/storage/Rprojects/mlbr`.

---

## 1. Core Mission & Grand Architecture

`mlbr` is an open-architecture, production-grade Sabermetrics research platform, data lake, and quantitative game prediction engine.

The ultimate objective is a **5-tier hierarchical modeling system** that builds from atomic pitch physics up to predictive game simulation and prediction market valuation (Kalshi, Polymarket):

```
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 5: Market Edge & Prediction Valuation                            │
│  - Sportsbook / Prediction Market odds (Polymarket, Kalshi, Vegas)     │
│  - Implied vs Model Win Probability, Brier Score calibration           │
│  - Kelly Criterion staking, bankroll management & market efficiency    │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Evaluates
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 4: Game-Level Win Probability & Simulation Engine                │
│  - Monte Carlo 9-inning game simulation (inning-by-inning / plate app) │
│  - Bradley-Terry, Log5, and ML Win Classifiers (XGBoost / LightGBM)   │
│  - Starting pitcher stamina curves, bullpen fatigue, home field bias   │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Powered by
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 3: Team-Level Run Creation & Prevention                          │
│  - BaseRuns (David Smyth) & Pythagenpat team run estimates            │
│  - Defensive efficiency (OAA, UZR, DRS) and Park Factor adjustments    │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Built from
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 2: Player-Level Forecasting & Aging Curves                       │
│  - Marcel the Monkey (Tango): 5/4/3 weighting + league mean regression │
│  - ZiPS (Szymborski): Multi-year aging curves & career trajectory comps│
│  - PECOTA (Silver): Nearest-Neighbor k-NN comps in similarity space   │
└───────────────────────────────────┬────────────────────────────────────┘
                                    │ Grounded in
                                    ▼
┌────────────────────────────────────────────────────────────────────────┐
│ LEVEL 1: Atomic Physics & Tracked Quality Models                       │
│  - xWhiff (Expected Swing & Miss) and "Stuff+" / "Location+" models    │
│  - xwOBA (Expected Weighted On-Base Average on Contact via EV & LA)   │
│  - RE24 Run Expectancy transition matrices per pitch and count         │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Interaction & Mentoring Protocol (MANDATORY)

1. **Didactic & Instructive Guidance**:
   - The user is using this project as a masterclass in modern sabermetrics, statistical modeling, and quantitative finance.
   - Never write opaque, unexplained "black box" code. Explain the mathematical intuition, historical sabermetric context (e.g. why Tangotiger chose 5/4/3 in Marcel, or why BaseRuns outperforms Linear Weights in extreme run environments).
   - Think like a world-class analytics director co-authoring a technical Substack / academic monograph.

2. **Every Response Requirements**:
   Every conversational response from the agent MUST include:
   - **Concrete Recommendations**: Actionable suggestions for the immediate next steps.
   - **Pros & Cons Analysis**: Clear evaluation of trade-offs for analytical approaches, models, or design choices.
   - **Ways to Improve**: Specific ideas to optimize the current code, enhance statistical rigor, or improve features.

3. **Curiosity & Cross-Discipline Innovation**:
   - Proactively suggest relevant concepts from academic econometrics, machine learning (e.g., manifold learning, clustering, survival analysis, empirical Bayes), and prediction market theory.

---

## 3. Engineering & Spec-Driven Development (SDD)

1. **Spec Before Code (`specs/`)**:
   - Complex models and systems begin with a markdown spec in `specs/` defining:
     - Target metric & loss function
     - Feature set & DuckDB zero-copy query
     - Baseline vs candidate algorithms
     - Cross-validation split (e.g. out-of-season temporal validation; NEVER train on future games to predict the past!)
     - Evaluation criteria (AUC, Brier score, log-loss, calibration curve)
2. **Zero-Drop Column Invariant**:
   - Raw Parquet files must preserve 100% of upstream columns. Never drop fields during ingestion or raw storage.
3. **Dynamic Path Resolution**:
   - Never hardcode absolute paths; always resolve via `mlbr::default_lake()`.
4. **Verification Requirement**:
   - All code changes must be verified with the invariant test suites:
     - `Rscript R/proof_lake.R` (28/28 empirical historical proofs)
     - `Rscript -e "mlbr::proof_crosswalks()"` (5/5 ID mapping proofs)
     - `Rscript R/generate_agent_manifest.R` (5/5 semantic continuity proofs)

---

## 4. Key Server Endpoints

* **RStudio Server:** `http://192.168.10.1:8787` (Persistent session; 365-day keepalive; no idle timeout)
* **Quarto Documentation Site:** `http://192.168.10.1:8088` (Serving `site/docs/`)
* **Host Server:** `cbwdellr720` (40 CPU cores, 125 GB RAM, RAID volume `/mnt/storage/Rprojects/mlbr`)
