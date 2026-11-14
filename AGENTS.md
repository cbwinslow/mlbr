# mlbr Data Lake & Sabermetrics Platform (AGENTS.md)

- DOX is a performant AGENTS.md hierarchy installed across this workspace.
- Agents must follow DOX instructions across all edits.

## Core Contract
- AGENTS.md files are binding work contracts for their subtrees.
- Work products, source materials, instructions, records, assets, and durable docs must stay understandable from the nearest applicable AGENTS.md plus every parent AGENTS.md above it.
- **Project Name:** `mlbr` (formerly `baseballLake`). Update references accordingly.
- **Zero-Drop Column Invariant:** Raw datasets MUST preserve 100% of available columns. Never drop fields during ingestion or raw storage.
- **Dynamic Path Resolution:** Never hardcode absolute paths in library or analysis code; always resolve via `mlbr::default_lake()`.

## Purpose
The `mlbr` workspace on `cbwdellr720` is a comprehensive, production-grade Sabermetrics data lake and machine learning platform. It stores raw baseball data at pitch-level and play-level granularity (Statcast, Retrosheet, FanGraphs, Chadwick, Lahman, NCAA, MLB Meta) in partitioned Apache Parquet format, mounted via zero-copy DuckDB views, with an R package (`mlbr`) and a Quarto analytics gallery published to `cloudcurio.cc`.

## Conventions & Rules
- `data/raw/`: Machine-managed by `R/ingest/*`, partitioned Parquet, append-by-replacement only per file. Deleting a raw file is the trigger to re-download only that file.
- `data/processed/`: Derived tables and analytical features, safe to regenerate.
- `pkg/mlbr/`: The core R package. Must pass `R CMD check` with 0 ERRORS and 0 WARNINGS.
- `site/`: Quarto multi-page documentation and analysis gallery website, outputting to `docs/`.
- `site/analyses/`: Individual machine learning and empirical sabermetric research studies.
- Serialization: Multi-hour ingest runs log to `/tmp/baseball-ingest.log`; verify `ps aux | grep ingest` before starting runs.

## Closeout & Verification
1. Re-check changed paths against the DOX chain.
2. Run data proof suite: `Rscript R/proof_lake.R` (all 28 invariant checks must pass).
3. If R package modified: `R CMD check pkg/mlbr --no-manual`.
4. If site modified: `quarto render site/` (verify exit code 0 and output in `docs/`).
5. Update nearest owning `AGENTS.md` and refresh Child DOX Index.

## User Preferences
- Name is `mlbr`.
- Data sets must retain all available columns.
- Reusable, expandable verification with human-readable report card (`proof_lake()`).
- High download performance with rate-limit respect and adaptive tier ladder (`month -> week -> day`).
- Multi-page documentation website with left sidebar, search, sitemap, and scikit-learn style gallery.
- Target domain is `cloudcurio.cc` and GitHub Pages.

## Child DOX Index
- [pkg/mlbr/AGENTS.md](pkg/mlbr/AGENTS.md): R package source code, R6 `LakeManager`, DuckDB views, data access, and test suite.
- [R/ingest/AGENTS.md](R/ingest/AGENTS.md): Standalone acquisition scripts, API limits, and rate-limiting policies for raw data sources.
- [site/AGENTS.md](site/AGENTS.md): Quarto multi-page website, layout, scikit-learn inspired analysis gallery, and publication to `cloudcurio.cc`.
