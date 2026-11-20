# CLAUDE.md - Operating Contract & Quantitative Research Protocol for mlbr

This file defines the mandatory working contract, behavioral guidelines, and engineering standards for all AI agents working in `/mnt/storage/Rprojects/mlbr` on `cbwdellr720`.

---

## 1. Core Mission & Grand Architecture

`mlbr` is a production-grade Sabermetrics research platform, data lake, and quantitative game prediction engine.

The platform executes a **5-tier hierarchical modeling system** building from atomic pitch physics to predictive game simulation and prediction market valuation (Kalshi, Polymarket):

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

## 2. Medallion Data Architecture & Full-Population Mandate

To preserve pure historical origins while providing instant (sub-50ms) query speeds, `mlbr` strictly enforces a 3-tier Medallion architecture:

1. **Bronze Tier (`data/raw/`)**:
   - Machine-managed, partitioned Parquet files (Statcast 2008–2026, Retrosheet 1900–2025, FanGraphs 1901–2025, Chadwick, Lahman, NCAA, MLB Meta).
   - **Zero-Drop Raw Invariant**: 100% column fidelity. Never drop fields, never overwrite raw partitions.
2. **Silver Tier (`data/features/`)**:
   - Lake-wide materialized feature datasets partitioned by `game_year` (e.g. `data/features/statcast_features/`).
   - Every row MUST retain composite primary keys linking directly to raw data: `game_pk`, `at_bat_number`, `pitch_number`.
   - Mounted dynamically via `v_statcast_features` in DuckDB (scanning 14.64M rows in 39ms).
3. **Gold Tier (`data/processed/` & `data/models/`)**:
   - Aggregated leaderboards, true talent baselines, run value matrices, and simulation lookup tables.
4. **Full-Population Materialization Mandate (NO Arbitrary Subsampling)**:
   - When a new physics metric, feature, or formula is validated, it MUST be materialized across the entire 14.64M pitch lake across all 19 seasons (2008–2026) using reproducible pipeline scripts (`R/materialize_*.R`).
   - Arbitrary subsamples are strictly prohibited for production data layers and are only permitted for local interactive sandbox probes or browser canvas rendering safety.

---

## 3. Visualization-First & Rich Asset Mandate

1. **Visual Storytelling Requirement**:
   - In quantitative research, charts and plots are primary discovery and communication tools, not secondary decorations.
   - Every study, analysis, and student lab workbook MUST produce clear, compelling, publication-grade figures illustrating distributions, non-linearities, and physical flight geometry.
   - Combine statistical summaries (GAM splines, empirical bins) with density contours and marginal distributions.
   - Incorporate interactive plots (`plotly`, `htmlwidgets`) on the Quarto site to enable hover-over pitch and player inspection.
2. **Saved Object Invariant (Asset Preservation)**:
   Whenever an exploratory or production plot is generated:
   - **Static Image**: Save high-res raster (`.png`, 300 DPI) styled with `theme_cyber()` to `models/` or `site/assets/`.
   - **Serialized Object**: Save the native R `ggplot` or `plotly` object as `.rds` in `models/` so downstream workflows can reload and restyle it without querying the lake.
   - **Audit Dataset**: Save the aggregated plot data as `.parquet` or `.csv` in `models/` for auditability.
   - **Interactive Widget**: Export standalone interactive `.html` widgets when applicable.

---

## 4. Institutional Knowledge: Experiment Registry (`experiments/registry.json`)

To prevent loss of discoveries, ungrounded code drift, and duplicated effort:
1. Every validated metric, baseline equation, invariant, or negative/rejected result MUST be cataloged in `experiments/registry.json` using `mlbr::lake_log_experiment()`.
2. Logging automatically updates `experiments/README.md` and syncs `data/manifest/agent_context.json`.
3. AI agents and student workbooks must check `experiments/registry.json` before proposing or deriving duplicate features.

---

## 5. Directory Structure & Naming Conventions

* `data/raw/`: Bronze tier raw Parquet lakes.
* `data/features/`: Silver tier partitioned Parquet feature tables.
* `data/processed/`: Gold tier aggregated model outputs and simulation artifacts.
* `data/manifest/`: Continuous machine-readable AI agent manifests (`lake_manifest.json`, `agent_context.json`).
* `experiments/`: Experiment registry (`registry.json`, `README.md`).
* `sql/`: Standalone, reusable, pure DuckDB SQL scripts (`01_statcast_features.sql`, `02_pitch_tunneling.sql`).
* `models/`: Self-contained, reproducible R model training and research scripts, along with saved figure assets (`.png`, `.rds`, `.parquet`).
* `workbooks/`: Interactive student RStudio Server code-along scripts (`lab01`–`lab05_*.R`). **1:1 Parity Mandate:** Every numbered Q-CRISP study in `site/analyses/0X_*.qmd` MUST have a companion runnable `workbooks/lab0X_*.R` and rendered guide in `site/workbooks/lab0X_*.qmd` (enforced by `R/generate_agent_manifest.R`).
* `specs/`: Mathematical and system architecture specifications (SDD / Q-CRISP).
* `pkg/mlbr/`: The core R package. Must pass clean build and installation.
* `site/`: Quarto multi-page research gallery and documentation (`site/analyses/`, `site/workbooks/`), deployed via GitHub Pages.

---

## 6. Mandatory Research Protocol: Q-CRISP & 7-Section Study Template

All analytical studies MUST follow the canonical **7-Section Study Template** for both research documentation and Quarto publication:
1. **Section 1: Executive Summary & Abstract (TL;DR)** — Bottom line up front, key findings, headline visualization.
2. **Section 2: Domain Problem & Theoretical Physics** — Perceptual/biomechanical mechanisms, why linear models fail.
3. **Section 3: Mathematical Formulation & Lake-Wide Pipeline** — Formal KaTeX formulas and full-population DuckDB SQL queries.
4. **Section 4: Exploratory Data Analysis (EDA) & Feature Diagnostics** — 2D density surfaces (hexbins), correlation matrices.
5. **Section 5: Model Architecture & Temporal Validation** — Out-of-season temporal validation (train past -> test future).
6. **Section 6: Leaderboards & Player Case Studies** — Model rankings, outlier detection.
7. **Section 7: Downstream Integration & Market Applications** — Connection to player projections and game betting edge.

---

## 7. Teaching, Mentoring & Review Protocol (MANDATORY)

1. **Collaborative Tutoring & Review**:
   - The user writes code and executes experiments in RStudio Server.
   - AI agents review student code at explicit checkpoints, checking for:
     - Correct joins (e.g. using `v_chadwick_register` instead of broken joins).
     - Proper aggregation syntax (e.g. `SUM(is_whiff)::NUMERIC / NULLIF(SUM(is_swing), 0)`).
     - Scalable plotting methods (GAM splines / binning instead of 150GB LOESS allocations).
     - Execution speed and sample completeness.
2. **Mandatory Output Structure**:
   Every response MUST include:
   - **Concrete Recommendations** (Actionable immediate next steps)
   - **Pros & Cons Analysis** (Trade-offs for analytical and technical options)
   - **Ways to Improve** (Specific enhancements to current code and next steps)

---

## 8. Aesthetic & Visual Standards

* **Dark Cyber / Antigravity Theme**:
  - Background Canvas: `#090d16`
  - Surface Cards: `#111827`
  - Borders: `#1f2937` with cyan/emerald glowing focus
  - Palette: `#38bdf8` (Cyan), `#10b981` (Emerald), `#f59e0b` (Amber), `#f43f5e` (Rose)
  - Typography: Google Sans / Inter for prose, Consolas / JetBrains Mono for code.
  - Zero harsh white backgrounds on website or in saved `ggplot2` figures.

---

## 9. Server Endpoints & Remote Rules

* **Remote Canonical Host:** `cbwdellr720` (IP: `192.168.10.1`, Ubuntu 24.04, 40 cores, 125 GB RAM, `/mnt/storage/Rprojects/mlbr`).
* **Zero Local Files on Windows:** Windows client machines must remain 100% free of local project files or cloned data.
* **RStudio Server:** `http://192.168.10.1:8787` (Persistent session; 365-day keepalive).
* **Quarto Documentation Site:** `http://192.168.10.1:8088` and `https://cbwinslow.github.io/mlbr/`.

---

## Storage Format & Multi-Engine Protocol (ADR-001)

- **Canonical Storage:** **Apache Parquet (`ZSTD` compression, sorted row groups)** across Bronze (`data/raw/`), Silver (`data/features/`), and Gold (`data/processed/`).
- **SQL & DataFrame Access:** Query via **DuckDB SQL** (`v_statcast`, `v_statcast_features`) or zero-copy **Apache Arrow / Polars** (`scan_parquet`).
- **Why Not `.sql` or `.duckdb` Files:** `.sql` text dumps lack columnar compression and projection pushdown; `.duckdb` single-file storage introduces single-writer file locks and slower multi-language reads compared to open ZSTD Parquet.
- **Passive Market & Lineup Archiving:** `R/ingest/archive_market_snapshots.py` snapshots MLB Stats API lineups and Polymarket/Kalshi public prices into ZSTD Parquet (`data/raw/mlb_lineups/`, `data/raw/markets/`) for future CLV backtesting.
