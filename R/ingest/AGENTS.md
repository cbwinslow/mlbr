# R/ingest — data acquisition scripts (AGENTS.md)

Created 2026-11-14. One script per source; each is standalone-reusable via Rscript and
reads/writes ONLY under data/raw/<source>/ with parquet (rare list objects -> rds).

## Ground rules

1. **Never run two ingest scripts concurrently against the same source** — MLB Savant,
   Retrosheet and FanGraphs rate-limit; serialize via one ingest_all.R run.
2. **Idempotent by contract.** Existing files are never re-downloaded. To refresh one file,
   delete that exact file and re-run the script (never bulk-delete raw data).
3. **APIs are named-args.** baseballr 2.0 dev changed signatures: fg_* takes
   startseason/endseason (positional calls bind to `age` and silently fail); retrosheet
   takes path_to_directory + c(from,to) + sequence_years=TRUE and setwd()s internally
   (absolute paths only); Savant caps at 25,000 rows/request (weekly fallback mandatory).
4. **Politeness is not optional:** Sys.sleep between requests, retries with backoff on
   failure, options(timeout=900) set in 00_common.R (retrosheet.org is slow).
5. Granularity targets: Statcast = per pitch, Retrosheet = per play/event, FG game logs =
   per player-game (per-player API, add a curated-player script if needed), Lahman/FG
   leaderboards = player-season (as fine as these sources provide).
6. Update the ingest log in the changelog if you change acquisition logic.

## Source roster (2026-11-14 deep-dive audit)

| script | source | granularity | note |
|---|---|---|---|
| ingest_lahman.R | Lahman pkg | player-season | 31 tables, 1871+ |
| ingest_chadwick.R | Chadwick register | per-player ID crosswalk | refresh occasionally |
| ingest_retrosheet_github.R | chadwickbureau/retrosheet (GitHub) | **per play / per game / per player-game / per sub / rosters** | 1871+; clone is ONE git connection, parsing is local; retrosheet.org NOT needed |
| ingest_retrosheet.R | retrosheet.org | per play | BANNED network - keep for future VPN/manual use; preflight-guarded |
| ingest_fangraphs.R | FanGraphs | player-season/team-season + guts/parks/projections | team tables only ~2002+ (API limit); use named args! |
| ingest_statcast.R | MLB Savant | **per pitch** | month->week->day tier ladder; day never hits 25k cap; window-trimmed + day-gap check |
| ingest_milb.R | Savant minors | **per pitch (MiLB)** | same backend as statcast - NEVER run concurrently with it |
| ingest_ncaa.R | sportsdataverse data repo | NCAA teams/schedules/**848k play rows**/groups | pre-cleaned, no scraping |
| ingest_mlb_meta.R | statsapi + data repo | meta, drafts, schedules, standings, park dims, re24/WE/WPA/xERA/stuff+/OAA/framing | cheap one-time pulls |
| ingest_bbref.R | baseball-reference | **per player-day** | STRICT: 12s spacing, do not tighten, run alone |
| ingest_espn.R | ESPN | per-season scoreboards (MLB + NCAA) | CDN-friendly, still throttle |

## Rate-limit ground truth
- The baseballr fetchers DO NOT throttle themselves. Every script calls throttle()
  (min spacing) from 00_common.R between requests. Never remove those calls.
- Savant caps single requests at 25,000 rows -> tier ladder (month/week/day);
  every window is trimmed to [start,end] so months never bleed into neighbors.
- bbref is the ban-happiest site on the internet; 12s minimum spacing, sequential.
- One connection per source at a time; two DIFFERENT sources may run in parallel.

## Known limitations
- cwevent 0.10 silently skips the repo's "version 3" exhibition/ASG files
  (.EBA/.EBE/.EBN) - regular season + playoffs + ASG (.EVN/.EVA/.EVE) parse fine.
- FanGraphs team_* tables unavailable pre-~2002 (API); Lahman Teams covers back to 1871.
- FanGraphs/ESPN per-player or per-game fan-outs (game logs) are NOT yet ingested -
  add as a curated phase; do not brute-force them (rate limits + request counts).
