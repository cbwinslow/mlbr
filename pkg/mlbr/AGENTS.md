# pkg/mlbr — R Package (AGENTS.md)

## Purpose
`mlbr` is an R package and analytical engine providing high-performance, type-safe, and zero-copy access to the local MLB Data Lake via DuckDB and Apache Parquet. It exposes dataset ingestion, lake management (`LakeManager` R6), data proofing/verification (`proof_lake`), and analytical query interfaces.

## Ownership
- Owned by `cbwinslow`.
- Maintained as a production-grade R package eligible for CRAN / GitHub publication.
- Source path: `/mnt/storage/Rprojects/mlb/pkg/mlbr/`.
- Target installation library: `/mnt/storage/R/library/mlbr`.

## Local Contracts
- **Package Name:** `mlbr` (MUST NOT reference `baseballLake`).
- **Dynamic Pathing:** NEVER hardcode absolute file paths inside functions. Always resolve via `default_lake(lake)` which queries:
  1. Explicit `lake` argument
  2. Environment variable `MLBR_DATA_PATH`
  3. Option `options("mlbr.path")`
  4. Local project root detection (`/mnt/storage/Rprojects/mlb` or `getwd()`)
- **Schema Preservation:** 0 columns dropped. Ingestion and extraction preserve 100% of raw fields.
- **R CMD Check Cleanliness:** Must pass with 0 ERRORS and 0 WARNINGS.
- **Documentation:** All exported functions and methods must have complete roxygen2 documentation with `@export`, `@param`, `@return`, and `@examples`.

## Work Guidance
- Use `R6` for manager classes (`LakeManager`).
- Use `duckdb` and `arrow` for zero-copy views over parquet files in `data/raw/`.
- Exported functions:
  - `LakeManager`: R6 controller class with methods `$status()`, `$proof()`, `$duckdb()`, `$find()`, `$describe()`, `$monitor()`.
  - `lake_status(lake)`: Data lake health and inventory audit table.
  - `lake_duckdb(lake, read_only=TRUE)`: DuckDB connection pre-mounted with views across all parquet datasets.
  - `proof_lake(lake, verbose=TRUE)`: Comprehensive test suite of 28 benchmark, schema, and sabermetric invariants.
  - Ingestion wrappers: `statcast_ingest`, `retrosheet_github_ingest`, `fangraphs_ingest`, `lahman_ingest`, `chadwick_ingest`, etc.
- When adding or modifying functions, regenerate docs with `roxygen2::roxygenise()` and reinstall with `R CMD INSTALL pkg/mlbr --library=/mnt/storage/R/library`.

## Verification
- Run test suite: `Rscript -e 'devtools::test("pkg/mlbr")'` or `Rscript R/proof_lake.R`.
- Check package: `R CMD check pkg/mlbr --no-manual --as-cran`.

## Child DOX Index
None.
