# ingest_ncaa.R - College baseball (NCAA D1/D2/D3) -> data/raw/ncaa/
# Primary path: baseballr's PRE-CLEANED NCAA datasets hosted in the sportsdataverse
# data repo (load_ncaa_baseball_*) - no scraping, no rate limits. These cover
# teams, group/conference memberships, season ids, and cleaned schedules + pbp.
# Optional deeper scraping (ncaa_pbp per game) is a separate phase - stats.ncaa.org
# is Cloudflare-hostile; ESPN college endpoints (espn_college_baseball_*) are the
# friendlier per-game path and are added in ingest_espn.R.
# Usage: Rscript R/ingest/ingest_ncaa.R
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow) })
d <- raw_dir("ncaa")
grab <- function(nm, fun) {
  p <- file.path(d, sprintf("%s.parquet", nm))
  if (file.exists(p)) return(invisible())
  tryCatch({
    x <- retry(2, 8, fun())
    if (!is.null(x) && is.data.frame(x) && nrow(x) > 0) write_df(as.data.frame(x), p)
  }, error = function(e) message("ncaa: FAILED ", nm, " - ",
                                 substr(conditionMessage(e), 1, 80)))
}
grab("teams",                 baseballr::load_ncaa_baseball_teams)
grab("season_ids",            baseballr::load_ncaa_baseball_season_ids)
grab("schedules",             baseballr::load_ncaa_baseball_schedule)
grab("play_by_play",          baseballr::load_ncaa_baseball_pbp)
grab("groups",                baseballr::load_ncaa_baseball_groups)
grab("group_aliases",         baseballr::load_ncaa_baseball_group_aliases)
grab("group_seasons",         baseballr::load_ncaa_baseball_group_seasons)
grab("team_group_seasons",    baseballr::load_ncaa_baseball_team_group_seasons)
# live lookup table (divisions I/II/III school ids)
grab("school_id_lu",          function() baseballr::ncaa_school_id_lu(division = c(1, 2, 3)))
message("ncaa: done")
