# small_ingests.R - Lahman, Chadwick register, NCAA, MLB metadata

#' ingest every Lahman package table (player-season, 1871-present)
#' @rdname lahman_ingest
#' @param lake project root of the data lake
#' @return invisible TRUE
#' @export
lahman_ingest <- function(lake) {
  lake <- default_lake(lake)
  items <- utils::data(package = "Lahman")$results[, "Item"]
  for (ds in items) {
    p <- lake_raw(lake, "lahman", paste0(ds, ".parquet"))
    if (file.exists(p)) { message("lahman: skip existing ", ds); next }
    write_df(get(ds, envir = asNamespace("Lahman")), p)
  }
  message("lahman: done (", length(items), " tables)")
  invisible(TRUE)
}

#' ingest the Chadwick player-ID crosswalk (MLBAM/bbref/retro/FanGraphs)
#' @rdname chadwick_ingest
#' @param lake project root of the data lake
#' @param force re-download even if the register already exists
#' @return invisible TRUE
#' @export
chadwick_ingest <- function(lake, force = FALSE) {
  lake <- default_lake(lake)
  p <- lake_raw(lake, "chadwick", "register.parquet")
  if (file.exists(p) && !force) {
    message("chadwick: register exists, skipping (force=TRUE to refresh)")
  } else {
    throttle(2)
    lu <- retry(3, 15, baseballr::get_chadwick_lu())
    write_df(lu, p)
  }
  message("chadwick: done")
  invisible(TRUE)
}

#' ingest pre-cleaned NCAA baseball datasets (teams, schedules, play-by-play,
#' conference groups) from the sportsdataverse data repo - no scraping
#' @rdname ncaa_ingest
#' @param lake project root of the data lake
#' @return invisible TRUE
#' @export
ncaa_ingest <- function(lake) {
  lake <- default_lake(lake)
  grab <- function(nm, fun) {
    p <- lake_raw(lake, "ncaa", sprintf("%s.parquet", nm))
    if (file.exists(p)) return(invisible())
    tryCatch({
      throttle(2)
      x <- retry(2, 8, fun())
      if (!is.null(x) && is.data.frame(x) && nrow(x) > 0) write_df(x, p)
    }, error = function(e) message("ncaa: FAILED ", nm, " - ",
                                   substr(conditionMessage(e), 1, 80)))
  }
  grab("teams",              baseballr::load_ncaa_baseball_teams)
  grab("season_ids",         baseballr::load_ncaa_baseball_season_ids)
  grab("schedules",          baseballr::load_ncaa_baseball_schedule)
  grab("play_by_play",       baseballr::load_ncaa_baseball_pbp)
  grab("groups",             baseballr::load_ncaa_baseball_groups)
  grab("group_aliases",      baseballr::load_ncaa_baseball_group_aliases)
  grab("group_seasons",      baseballr::load_ncaa_baseball_group_seasons)
  grab("team_group_seasons", baseballr::load_ncaa_baseball_team_group_seasons)
  message("ncaa: done")
  invisible(TRUE)
}

#' ingest MLB metadata + pre-computed model datasets (schedules, drafts,
#' standings, park dimensions, re24/WE/WPA/xERA/stuff+/OAA/framing...)
#' @rdname mlb_meta_ingest
#' @param lake project root of the data lake
#' @param from,to year range for schedules/drafts/standings
#' @return invisible TRUE
#' @export
mlb_meta_ingest <- function(lake, from = 2008L,
                            to = as.integer(format(Sys.Date(), "%Y"))) {
  lake <- default_lake(lake)
  grab <- function(nm, fun, ...) {
    p <- lake_raw(lake, "mlbmeta", sprintf("%s.parquet", nm))
    if (file.exists(p)) return(invisible())
    tryCatch({
      throttle(2)
      x <- retry(2, 8, fun(...))
      if (!is.null(x) && length(x) > 0 && (is.data.frame(x) && nrow(x) > 0))
        write_df(x, p)
    }, error = function(e) message("mlbmeta: FAILED ", nm, " - ",
                                   substr(conditionMessage(e), 1, 80)))
  }
  # one-time reference tables
  grab("teams",            baseballr::mlb_teams)
  grab("venues",           baseballr::mlb_venues)
  grab("positions",        baseballr::mlb_positions)
  grab("sports",           baseballr::mlb_sports)
  grab("award_types",      baseballr::mlb_awards)
  grab("game_types",       baseballr::mlb_game_types)
  grab("schedule_event_types", baseballr::mlb_schedule_event_types)
  grab("standings_types",  baseballr::mlb_standings_types)
  # sportsdataverse data-repo loads (pre-computed datasets; no scraping)
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
  # yearly: schedules (game_pk inventory), draft, standings snapshots
  for (yr in from:to) {
    grab(sprintf("schedule-%d", yr),  baseballr::mlb_schedule,
         season = yr, level_ids = "1")
    grab(sprintf("draft-%d", yr),     baseballr::mlb_draft, year = yr)
    grab(sprintf("standings-%d", yr), baseballr::mlb_standings, season = yr,
         standings_type = "regularSeason", league_id = "103,104")
  }
  message("mlbmeta: done")
  invisible(TRUE)
}
