# ingest_mlb_meta.R - MLB ancillary/metadata tables -> data/raw/mlbmeta/
# Cheap, mostly one-time pulls (statsapi + sportsdataverse data repo). Idempotent.
# Usage: Rscript R/ingest/ingest_mlb_meta.R [<from_year> <to_year>]
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2008
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))
d <- raw_dir("mlbmeta")

grab <- function(nm, fun, ...) {
  p <- file.path(d, sprintf("%s.parquet", nm))
  if (file.exists(p)) return(invisible())
  tryCatch({
    throttle(2)
    x <- retry(2, 8, fun(...))
    if (!is.null(x) && length(x) > 0 && (is.data.frame(x) && nrow(x) > 0))
      write_df(as.data.frame(x), p)
  }, error = function(e) message("mlbmeta: FAILED ", nm, " - ",
                                 substr(conditionMessage(e), 1, 80)))
}

## one-time reference tables
grab("teams",            baseballr::mlb_teams)
grab("venues",           baseballr::mlb_venues)
grab("positions",        baseballr::mlb_positions)
grab("sports",           baseballr::mlb_sports)
grab("award_types",      baseballr::mlb_awards)
grab("game_types",       baseballr::mlb_game_types)
grab("schedule_event_types", baseballr::mlb_schedule_event_types)
grab("standings_types",  baseballr::mlb_standings_types)

## sportsdataverse data-repo loads (pre-computed datasets; no scraping)
grab("park_dimensions",  baseballr::load_mlb_park_dimensions)
grab("mlb_groups",       baseballr::load_mlb_groups)
grab("mlb_group_aliases",baseballr::load_mlb_group_aliases)
grab("game_info_sup",    baseballr::load_game_info_sup)
grab("umpires",          baseballr::load_umpire_ids)
grab("re24_matrix",      baseballr::load_mlb_re24_matrix)
grab("we_table",         baseballr::load_mlb_we_table)
grab("wpa",              baseballr::load_mlb_wpa)
grab("xera",             baseballr::load_mlb_xera)
grab("stuff_plus",       baseballr::load_mlb_stuff_plus)
grab("command_plus",     baseballr::load_mlb_command_plus)
grab("oaa",              baseballr::load_mlb_oaa)
grab("catcher_framing",  baseballr::load_mlb_catcher_framing)
grab("expected_stats",   baseballr::load_mlb_expected_stats)
grab("expected_hr",      baseballr::load_mlb_expected_hr)
grab("batter_projection",baseballr::load_mlb_batter_projection)

## yearly: schedules (game_pk inventory), draft, standings snapshots
for (yr in from:to) {
  grab(sprintf("schedule-%d", yr),  baseballr::mlb_schedule,  season = yr, level_ids = "1")
  grab(sprintf("draft-%d", yr),     baseballr::mlb_draft,     year = yr)
  grab(sprintf("standings-%d", yr), baseballr::mlb_standings, season = yr,
       standings_type = "regularSeason", league_id = "103,104")
}
message("mlbmeta: done")
