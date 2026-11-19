---
name: silver-materialization
description: >-
  Use when adding, modifying, or backfilling Silver tier features in mlbr (R/materialize_features.R,
  sql/01_statcast_features.sql, data/features/) to enforce ADR-001 Parquet layout and DuckDB view invariants.
---

# mlbr Silver Feature Materialization Playbook

## 1. Contract & Key Invariants
- Never mutate `data/raw/` (Bronze tier, 100% column fidelity).
- Link every Silver row back to Bronze via immutable composite keys: `(game_pk, at_bat_number, pitch_number)`.
- Resolve all paths dynamically via `mlbr::default_lake()`.

## 2. ADR-001 Parquet Layout
- Format: Apache Parquet with **ZSTD** compression and `ROW_GROUP_SIZE = 100000`.
- Partitioning: Hive-partitioned by `game_year`.
- Sorting: Sort within partitions by `(pitcher, game_date, game_pk, at_bat_number, pitch_number)` for DuckDB min-max row-group pruning.

## 3. Code & SQL Sync
1. Update canonical SQL in `sql/01_statcast_features.sql` (and `sql/02_pitch_tunneling.sql` if tunneling-related).
2. Codify full-population materialization in `R/materialize_features.R`.
3. Update `pkg/mlbr/` DuckDB view wiring if new Silver tables/columns are exposed.

## 4. Verification Closeout
1. `Rscript R/proof_lake.R` (all 28 invariants pass)
2. `Rscript -e "mlbr::proof_crosswalks()"`
3. If `pkg/mlbr/` changed: `R CMD INSTALL pkg/mlbr` and `R CMD check --no-manual pkg/mlbr` (0 ERRORS, 0 WARNINGS, 0 code NOTEs).
4. Run `mlbr-pipeline-auditor` and `mlbr-duckdb-optimizer` to confirm zero regressions.
