# mlbr Data Lake, Sabermetrics & Quantitative Prediction Platform (AGENTS.md)

- DOX is a performant AGENTS.md hierarchy installed across this workspace.
- Agents must follow DOX instructions across all edits.

## Core Contract & Behavioral Mandates
- **Project Name:** `mlbr` (Production Sabermetrics Data Lake & Quantitative Machine Learning Engine).
- **Core Mission:** A 5-tier hierarchical prediction platform (Atomic Pitch Telemetry -> Player Projections [Marcel/ZiPS/PECOTA] -> Team BaseRuns -> Game Simulation -> Prediction Market Valuation [Polymarket/Kalshi]).
- **Research Framework (Q-CRISP):** All modeling investigations must follow the Quantitative CRISP-DM lifecycle and publish studies following the **7-Section Study Template** to the Quarto site.
- **Full-Population Invariant:** Feature engineering and model scoring must apply across the entire data lake (all 14.64 million pitches and 125 historical seasons). Subsampling is restricted to interactive sandbox probing and browser canvas safety.
- **Instructional & Mentoring Protocol:** Work collaboratively with the user as an expert tutor and quantitative mentor. Explain mathematical foundations, sabermetric historical context, and architectural choices.
- **Mandatory Output Structure:** Every response MUST include:
  1. Concrete Recommendations
  2. Pros & Cons of decisions and approaches
  3. Ways to improve current implementations and next steps
- **Zero-Drop Column Invariant:** Raw datasets MUST preserve 100% of available columns. Never drop fields during ingestion or raw storage.
- **Spec-Driven Development (SDD):** Complex modeling features and estimators must be specified in `specs/` with defined loss functions, temporal validation splits, and mathematical invariants.
- **Dynamic Path Resolution:** Never hardcode absolute paths; always resolve via `mlbr::default_lake()`.

## Purpose
The `mlbr` workspace on `cbwdellr720` is a comprehensive, production-grade Sabermetrics data lake and machine learning platform. It stores raw baseball data at pitch-level and play-level granularity (Statcast 2008-2026, Retrosheet 1900-2025, FanGraphs 1901-2025, Chadwick, Lahman, NCAA, MLB Meta) in partitioned Apache Parquet format, mounted via zero-copy DuckDB views, with an R package (`mlbr`) and a Quarto analytics gallery published to `cloudcurio.cc` (served locally at `http://192.168.10.1:8088`).

## Conventions & Rules
- `data/raw/`: Machine-managed, partitioned Parquet, 100% schema fidelity.
- `data/processed/`: Derived tables and lake-wide engineered features.
- `data/manifest/`: Continuous machine-readable AI agent manifests (`lake_manifest.json`, `agent_context.json`, `LAKE_AGENT_PROMPT.md`).
- `specs/`: Engineering and sabermetric model specifications following SDD principles and Q-CRISP.
- `models/`: Self-contained, reproducible model training and exploratory scripts in R.
- `pkg/mlbr/`: The core R package. Must pass `R CMD check` with 0 ERRORS and 0 WARNINGS.
- `site/`: Quarto multi-page documentation, interactive data dictionary, and analysis gallery website, outputting to `docs/`.
- Temporal Leakage Rule: NEVER train on future game dates to predict the past. Validation splits must be strictly chronological.

## Closeout & Verification
1. Re-check changed paths against the DOX chain.
2. Run data proof suite: `Rscript R/proof_lake.R` (all 28 invariant checks must pass).
3. Run crosswalk proof suite: `Rscript -e "mlbr::proof_crosswalks()"`.
4. Run semantic agent manifest audit: `Rscript R/generate_agent_manifest.R`.
5. If R package modified: `R CMD check pkg/mlbr --no-manual`.
6. If site modified: `quarto render site/` (verify exit code 0 and output in `docs/`).
7. Update nearest owning `AGENTS.md` and refresh Child DOX Index.

## Child DOX Index
- [pkg/mlbr/AGENTS.md](pkg/mlbr/AGENTS.md): R package source code, R6 `LakeManager`, DuckDB views, data access, and test suite.
- [R/ingest/AGENTS.md](R/ingest/AGENTS.md): Standalone acquisition scripts, API limits, and rate-limiting policies for raw data sources.
- [site/AGENTS.md](site/AGENTS.md): Quarto multi-page website, layout, scikit-learn inspired analysis gallery, and publication to `cloudcurio.cc`.
- [specs/AGENTS.md](specs/AGENTS.md): Model specifications, loss functions, and architectural design contracts.
