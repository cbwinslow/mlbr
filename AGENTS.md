# mlbr Data Lake, Sabermetrics & Quantitative Prediction Platform (AGENTS.md)

- DOX is a performant AGENTS.md hierarchy installed across this workspace.
- Agents must follow DOX instructions across all edits.

## Core Contract & Behavioral Mandates
- **Project Name:** `mlbr` (Production Sabermetrics Data Lake & Quantitative Machine Learning Engine).
- **Canonical Root:** Strictly on homelab server `cbwdellr720` at `/mnt/storage/Rprojects/mlbr`. Windows client machines must remain 100% free of local data/repo clones.
- **Core Mission:** A 5-tier hierarchical prediction platform (Atomic Pitch Telemetry -> Player Projections [Marcel/ZiPS/PECOTA] -> Team BaseRuns -> Game Simulation -> Prediction Market Valuation [Polymarket/Kalshi]).
- **Research Framework (Q-CRISP):** All modeling investigations must follow the Quantitative CRISP-DM lifecycle and publish studies following the **7-Section Study Template** to the Quarto site.
- **Medallion Data Architecture Invariant:**
  - **Bronze Tier (`data/raw/`):** 100% schema fidelity raw Parquet partitions. Never drop columns, never mutate or overwrite raw data.
  - **Silver Tier (`data/features/`):** Full-population materialized feature datasets (partitioned Parquet) linked to raw data via immutable composite keys (`game_pk`, `at_bat_number`, `pitch_number`). Mounted via `v_statcast_features` in DuckDB for 39ms query speeds.
  - **Gold Tier (`data/processed/` & `data/models/`):** Lake-wide aggregated player talent levels, True Talent baselines, run values, aging curves, and simulation matrices.
- **Full-Population Materialization Mandate (NO Arbitrary Subsampling):** Feature engineering and physics derivations must apply across the entire data lake (all 14.64 million pitches and 125 historical seasons). Subsampling is restricted to interactive sandbox probing and browser canvas safety. Validated metrics must be codified into `R/materialize_*.R` and written to Silver Parquet.
- **Visualization-First & Rich Asset Mandate:**
  - Every study, lab workbook, and analysis MUST produce rich, informative, publication-grade visualizations. Tabular outputs alone are never sufficient.
  - Plots must convey intuitive stories (perceptual mechanisms, non-linear break points, risk/reward trade-offs).
  - Use interactive tools (`plotly`, `htmlwidgets`, `echarts4r`) on the website to allow hover inspection of player names, pitch velocities, and sample sizes.
- **Saved Plot & Object Invariant (Asset Preservation):**
  - Whenever a plot or model is generated, always preserve its assets:
    1. High-resolution raster image (`.png` at 300 DPI, styled in Dark Cyber) in `models/` or `site/assets/`.
    2. Serialized native R object (`.rds`) in `models/` containing the `ggplot` or `plotly` object so downstream scripts can reload, inspect, or restyle without re-querying the database.
    3. Aggregated plot data (`.parquet` or `.csv`) alongside the figure for auditability.
    4. Interactive standalone HTML widget (`.html`) when applicable.
- **Curated Knowledge Base & Experiment Registry:**
  - Every analytical finding, formula, invariant, or negative/rejected result must be logged into `experiments/registry.json` via `mlbr::lake_log_experiment()`.
  - Machine manifests (`data/manifest/agent_context.json`) and `experiments/README.md` must auto-sync so all agents and workbooks inherit verified formulas.
- **Instructional & Mentoring Protocol:** Work collaboratively with the user as an expert tutor and quantitative mentor. Review student workbook outputs at designated checkpoints, verify statistical validity, check for sample bias, and explain mathematical foundations and sabermetric history.
- **Mandatory Output Structure:** Every response MUST include:
  1. Concrete Recommendations
  2. Pros & Cons of decisions and approaches
  3. Ways to improve current implementations and next steps
- **Zero-Drop Column Invariant:** Raw datasets MUST preserve 100% of available columns. Never drop fields during ingestion or raw storage.
- **Spec-Driven Development (SDD):** Complex modeling features and estimators must be specified in `specs/` with defined loss functions, temporal validation splits, and mathematical invariants.
- **Dynamic Path Resolution:** Never hardcode absolute paths; always resolve via `mlbr::default_lake()`.
- **Dark Cyber / Antigravity Visual Standard:** Canvas `#090d16`, surface `#111827`, gridlines `#1e293b`, glowing cyber accents (`#38bdf8` cyan, `#10b981` emerald, `#f59e0b` amber, `#f43f5e` rose), Google Sans / Inter for prose, Consolas for code. Zero harsh white backgrounds.

## Purpose
The `mlbr` workspace on `cbwdellr720` is a comprehensive, production-grade Sabermetrics data lake and machine learning platform. It stores raw baseball data at pitch-level and play-level granularity (Statcast 2008-2026, Retrosheet 1900-2025, FanGraphs 1901-2025, Chadwick, Lahman, NCAA, MLB Meta) in partitioned Apache Parquet format, mounted via zero-copy DuckDB views, with an R package (`mlbr`) and a Quarto analytics gallery published to GitHub Pages and local doc server (`http://192.168.10.1:8088`).

## Conventions & Directory Taxonomy
- `data/raw/`: Bronze tier. Machine-managed, partitioned Parquet, 100% schema fidelity.
- `data/features/`: Silver tier. Partitioned Parquet feature tables (e.g., `statcast_features/`), keyed by `game_pk`, `at_bat_number`, `pitch_number`.
- `data/processed/`: Gold tier. Aggregated leaderboards, aging curves, and game simulator assets.
- `data/manifest/`: Continuous machine-readable AI agent manifests (`lake_manifest.json`, `agent_context.json`, `LAKE_AGENT_PROMPT.md`).
- `experiments/`: Curated experiment registry (`registry.json`, `README.md`) tracking all hypotheses, equations, metrics, and validation status.
- `sql/`: Reusable, standalone, pure DuckDB SQL scripts for feature materialization and queries.
- `models/`: Self-contained, reproducible model training and exploratory research scripts in R, including saved figure artifacts (`.png`, `.rds`, `.parquet`).
- `workbooks/`: Student-facing interactive lab workbooks for RStudio Server (`lab01_aerodynamics_workbook.R`, etc.).
- `specs/`: Engineering and sabermetric model specifications following SDD principles and Q-CRISP.
- `pkg/mlbr/`: The core R package. Must pass `R CMD check` with 0 ERRORS and 0 WARNINGS.
- `site/`: Quarto multi-page documentation, interactive data dictionary, and analysis gallery website, outputting to `site/docs/`.
- Temporal Leakage Rule: NEVER train on future game dates to predict the past. Validation splits must be strictly chronological.

## Closeout & Verification
1. Re-check changed paths against the DOX chain.
2. Run data proof suite: `Rscript R/proof_lake.R` (all 28 invariant checks must pass).
3. Run crosswalk proof suite: `Rscript -e "mlbr::proof_crosswalks()"`.
4. Run semantic agent manifest audit: `Rscript R/generate_agent_manifest.R`.
5. If R package modified: `R CMD INSTALL pkg/mlbr` and verify views with `mlbr::lake_duckdb()`.
6. If site modified: `quarto render site/` (verify exit code 0 and output in `site/docs/`).
7. Update nearest owning `AGENTS.md` and refresh Child DOX Index.

## Child DOX Index
- [pkg/mlbr/AGENTS.md](pkg/mlbr/AGENTS.md): R package source code, R6 `LakeManager`, DuckDB views, data access, and test suite.
- [R/ingest/AGENTS.md](R/ingest/AGENTS.md): Standalone acquisition scripts, API limits, and rate-limiting policies for raw data sources.
- [site/AGENTS.md](site/AGENTS.md): Quarto multi-page website, layout, scikit-learn inspired analysis gallery, and publication to GitHub Pages.
- [specs/AGENTS.md](specs/AGENTS.md): Model specifications, loss functions, and architectural design contracts.