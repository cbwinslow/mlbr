# mlb project (AGENTS.md)

## Purpose
User's personal MLB analysis workspace on cbwdellr720. Raw data at the finest available
granularity (per-pitch Statcast, per-play Retrosheet); aggregation happens in R sessions,
NEVER by rewriting raw files.

## Conventions
- data/raw/: machine-managed by R/ingest/*, parquet, append-by-replacement only per-file.
  Deleting a raw file is how you request a re-download of exactly that file.
- data/processed/: outputs of R/prepare.R, regenerable - safe to delete and rebuild.
- R/ingest/: acquisition scripts - see R/ingest/AGENTS.md for API gotchas and rules.
- No secrets anywhere in this tree.
- Multi-hour ingest runs log to /tmp/baseball-ingest.log and are serialized; check
  `ps aux | grep ingest` before starting one.
