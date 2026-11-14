# R/ingest — Data Acquisition Scripts (AGENTS.md)

## Purpose
`R/ingest` contains independent, standalone data acquisition scripts for downloading, converting, and writing raw MLB data sources into partitioned Apache Parquet format under `data/raw/<source>/`.

## Ownership
- Owned by `cbwinslow`.
- Maintained as idempotent, rate-limit safe ingestion processes for the `mlbr` data lake.

## Local Contracts
1. **Never run two ingest scripts concurrently against the same source** — MLB Savant, Retrosheet, and FanGraphs rate-limit; serialize multi-source ingestion.
2. **Idempotent by contract.** Existing files are never re-downloaded. To refresh one file, delete that exact file and re-run the script (never bulk-delete raw data).
3. **APIs are named-args.** `baseballr` 2.0 dev changed signatures: `fg_*` takes `startseason`/`endseason`; retrosheet takes `path_to_directory` + `c(from,to)` + `sequence_years=TRUE`; Savant caps at 25,000 rows/request (adaptive tier ladder `month -> week -> day` mandatory).
4. **Zero-drop column policy.** All original columns must be preserved in parquet files (0 `select()` calls during ingestion).
5. **Polite rate limiting:** Mandatory `Sys.sleep` between requests, exponential backoff retries, and high timeout options (`options(timeout=900)`).
6. Multi-hour ingest runs log to `/tmp/baseball-ingest.log` and must be checked via `ps aux | grep ingest` before starting.

## Work Guidance
- Source scripts:
  - `ingest_statcast.R`: MLB Statcast pitch-by-pitch data via Savant adaptive tier ladder.
  - `ingest_retrosheet_github.R`: Full play-by-play, games, substitutions, and rosters from `chadwickbureau/retrosheet` parsed via local Chadwick C tools.
  - `ingest_fangraphs.R`: Batting, pitching, fielding, team season tables, park factors, and guts constants.
  - `ingest_chadwick.R`: Chadwick Bureau player register (universal crosswalk).
  - `ingest_lahman.R`: Lahman historical baseball database (1871+).
  - `ingest_ncaa.R`: College baseball play-by-play and schedules.
  - `ingest_mlb_meta.R`: MLB draft, schedules, park dimensions, and advanced metric tables.
  - `ingest_espn.R`: ESPN schedules and boxscores.

## Verification
- Inspect output counts: `Rscript -e 'mlbr::lake_status()'`
- Check integrity invariants: `Rscript R/proof_lake.R`
- Audit background process: `tail -n 25 /tmp/baseball-ingest.log`

## Child DOX Index
None.
