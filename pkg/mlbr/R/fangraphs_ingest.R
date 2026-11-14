# fangraphs_ingest.R - FanGraphs leaderboards -> <lake>/data/raw/fangraphs/
#
# Granularity: player-season leaderboards (bat/pitch/fielder, full pages),
# team-season tables, plus one-time guts/park/projections tables.
#
# GOTCHA: baseballr 2.0 dev API requires NAMED args (startseason/endseason) -
# positional calls silently bind to the wrong parameters. fg_* returns
# xMLBAMID (join key to the Chadwick register).

#' Ingest FanGraphs season leaderboards
#'
#' @rdname fangraphs_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @param include_team include team_bat/team_pit/team_fld (only ~2002+ exist)
#' @return invisible TRUE; writes parquet files
#' @export
fangraphs_ingest <- function(lake, from = 2015L,
                             to = as.integer(format(Sys.Date(), "%Y")),
                             include_team = TRUE) {
  lake <- default_lake(lake)

  # leaderboard pages: pageitems=2000, loop pagenum until a short page
  fg_all <- function(fun, year) {
    parts <- list(); pn <- 1
    repeat {
      d <- fun(startseason = as.character(year), endseason = as.character(year),
               pageitems = "2000", pagenum = as.character(pn))
      if (is.null(d) || nrow(d) == 0) break
      parts[[pn]] <- d
      if (nrow(d) < 2000) break
      pn <- pn + 1; Sys.sleep(2)
    }
    if (!length(parts)) NULL else dplyr::bind_rows(parts)
  }

  tables <- list(
    bat      = baseballr::fg_bat_leaders,
    pitch    = baseballr::fg_pitcher_leaders,
    fielder  = baseballr::fg_fielder_leaders
  )
  if (include_team) tables <- c(tables, list(
    team_bat = baseballr::fg_team_batter,
    team_pit = baseballr::fg_team_pitcher,
    team_fld = baseballr::fg_team_fielder
  ))

  for (yr in from:to) for (nm in names(tables)) {
    p <- lake_raw(lake, "fangraphs", sprintf("%s-%d.parquet", nm, yr))
    if (file.exists(p)) next
    tryCatch({
      throttle(2)
      d <- retry(2, 10, fg_all(tables[[nm]], yr))
      if (!is.null(d) && nrow(d) > 0) write_df(d, p)
    }, error = function(e) message("fangraphs: FAILED ", nm, "-", yr, " - ",
                                   substr(conditionMessage(e), 1, 90)))
    Sys.sleep(2)
  }

  # one-time ancillary tables
  once <- list(
    guts         = function() baseballr::fg_guts(),
    park         = function() baseballr::fg_park(),
    proj_steamer = function() baseballr::fg_projections(type = "steamer",
                                                        stats = "bat"),
    proj_zips    = function() baseballr::fg_projections(type = "zips",
                                                        stats = "bat")
  )
  for (nm in names(once)) {
    p <- lake_raw(lake, "fangraphs", sprintf("%s.parquet", nm))
    if (file.exists(p)) next
    tryCatch({
      throttle(2)
      d <- retry(2, 10, once[[nm]]())
      if (is.data.frame(d) && nrow(d) > 0) write_df(d, p)
    }, error = function(e) message("fangraphs: FAILED ", nm, " - ",
                                   substr(conditionMessage(e), 1, 90)))
    Sys.sleep(2)
  }
  message("fangraphs: done (", from, "-", to, ")")
  invisible(TRUE)
}
