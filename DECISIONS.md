# Architecture Decision Records (`mlbr`)

Concise log of lasting architectural, storage, and methodological decisions for `mlbr`. Add a new ADR whenever a non-obvious design choice is settled so future sessions do not re-litigate it.

---

## ADR-001: ZSTD-Compressed, Sorted, Hive-Partitioned Apache Parquet + DuckDB/Arrow Compute
- **Status:** Accepted (2026-11-18)
- **Context:** The `mlbr` lake stores 14.64M+ Statcast pitches and 125+ seasons across Retrosheet, FanGraphs, Chadwick, Lahman, and NCAA. Monolithic `.duckdb` files introduce single-writer lock contention across R, Python, and Quarto processes, while `.sql` dumps are slow and bloated.
- **Decision:** Standardize on Apache Parquet with **ZSTD compression** (`ROW_GROUP_SIZE = 100000`), Hive-partitioned by `game_year`, and sorted by `(pitcher, game_date, game_pk, at_bat_number, pitch_number)`. Mount via zero-copy DuckDB views (`mlbr::lake_duckdb()`) and Arrow/Polars (`pyarrow` / `polars.scan_parquet`). Reserve Arrow IPC/Feather strictly for ephemeral scratch IPC.
- **Consequences:** Sub-50ms analytical queries (~39ms on `v_statcast_features`) via min-max row-group pruning, concurrent multi-engine reads (R + Python), and compact disk footprint.

## ADR-002: Medallion Architecture & Full-Population Silver Materialization
- **Status:** Accepted
- **Context:** Ad-hoc feature engineering inside notebooks causes formula drift, inconsistent sample filters, and slow repeated recomputation.
- **Decision:** Enforce strict Bronze (`data/raw/`, 100% zero-drop schema fidelity), Silver (`data/features/`, full-population materialized features keyed by `game_pk`, `at_bat_number`, `pitch_number`), and Gold (`data/processed/`, `data/models/`) tiers. Validated metrics must be codified in `R/materialize_*.R` and `sql/`.
- **Consequences:** Every downstream study, workbook, and model inherits identical, audited feature definitions over the full population.

## ADR-003: Strict Chronological Validation & 4-Asset Preservation
- **Status:** Accepted
- **Context:** Baseball forecasting models are highly vulnerable to temporal leakage (training on future seasons/dates) and unreproducible figures.
- **Decision:** All predictive models must use strictly chronological train/validation/test splits. Every published figure/model must preserve four assets (`.png` at 300 DPI Dark Cyber, `.rds` native R object, `.parquet`/`.csv` aggregated plot data, and `.html` interactive widget where applicable) and log results into `experiments/registry.json`.
- **Consequences:** Full auditability, restyle-without-requery capability, and zero silent regressions in reported findings.

## ADR-004: Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry Decomposition
- **Status:** Accepted (2026-11-20)
- **Context:** Single-season home-vs-road scoring and home run ratios suffer from severe sample noise (~81 home games per season) and schedule imbalance. Furthermore, symmetric park factors mask critical handedness biases caused by asymmetric outfield dimensions (e.g. Yankee Stadium short right porch vs. Oracle Park deep right-center triples alley).
- **Decision:** Derive 3-year rolling weighted (3/2/1) empirical park factors from `v_retrosheet_games` and `v_retrosheet_plays` (`BAT_EVENT_FL = 'T'`, 16.2M+ PAs across 2000-2025) with Empirical Bayes pseudo-count smoothing and event-specific shrinkage denominators ($M = 40$ games for runs, $M = 1500$ PAs for HR, $M = 800$ PAs for LHB/RHB HR splits, $M = 2000$ PAs for SO, BB, and BABIP). Enrich with exact physical geometry, elevation, and roof type from `park_dimensions.parquet`.
- **Consequences:** Provides stable, non-noisy environmental priors (`pf_runs`, `pf_hr`, `pf_hr_rhb`, `pf_hr_lhb`, `pf_so`, `pf_bb`, `pf_babip`) across 720 park-seasons (2002-2025) stored as Gold ADR-001 ZSTD Parquet (`data/processed/ballpark_factors_2002_2025.parquet`) for Level 3 team run creation/prevention and Level 4 Monte Carlo simulation.

## ADR-005: Full-Spectrum True-Talent WAR Decomposition & Residual Sensor Ridge Blending
- **Status:** Accepted (2026-11-20)
- **Context:** True-talent player projections require combining long-term historical aging and stabilization baselines (Tango Marcel + Carleton reliability M) with high-frequency modern physical tracking sensors (Hawk-Eye mlbr Stuff+, Decomposed Pitching+, and Batter Process Engine). Blending sensors of radically different sample lengths directly in a single regression causes L2 over-shrinkage, while standard WAR accounting can inadvertently double-count positional adjustments if FanGraphs `Defense` is combined naively with positional runs.
- **Decision:** Employ a 2-stage orthogonal architecture: Stage 1 establishes multi-year Marcel rates and zero-sum midpoint Delta-Method aging curves; Stage 2 applies a compact zero-mean Residual Ridge learning orthogonal adjustments from modern physics sensors. In WAR accounting, isolate pure fielding runs `b.Fielding AS fld_runs` so that $\text{proj\_def\_runs} = \text{proj\_fld\_runs} + \text{proj\_pos\_runs}$ and $\text{proj\_war} = (\text{proj\_wraa} + \text{proj\_bsr\_runs} + \text{proj\_fld\_runs} + \text{proj\_pos\_runs} + \text{proj\_repl\_runs}) / 9.77$ strictly adheres to FanGraphs WAR without double-counting.
- **Consequences:** Out-of-sample weighted RMSE significantly outperforms Marcel and naive persistences on both pitchers ($p = 1.000$) and batters ($p = 0.983$), while producing calibrated exposure-dependent 80% uncertainty bands and mathematically coherent True-Talent WAR.

---

## Template for New ADRs
```markdown
## ADR-NNN: <Short Title>
- **Status:** Proposed | Accepted | Superseded by ADR-XXX
- **Context:** <Problem and constraints>
- **Decision:** <What we chose>
- **Consequences:** <Trade-offs, invariants, and follow-up work>
```
