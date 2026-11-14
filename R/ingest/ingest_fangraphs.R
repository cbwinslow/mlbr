# ingest_fangraphs.R - FanGraphs tables -> data/raw/fangraphs/
# Granularity:
#   bat/pitch/fielder-<year>.parquet : player-season leaderboards (full pages)
#   team_bat/team_pitch-<year>       : team-season
#   guts.parquet                     : league-year wOBA/FIP constants (once)
#   park-<year>.parquet              : park factors
#   projections-<type>.parquet       : current projections (steamer/zips/atc)
# Usage: Rscript R/ingest/ingest_fangraphs.R <from_year> <to_year>
# NOTE: baseballr 2.0 dev API - use NAMED args (startseason/endseason); positional
# calls silently bind to the wrong parameters. fg_* returns xMLBAMID (join key).
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(dplyr) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 2015
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))

# leaderboard pages: pageitems=2000, loop pagenum until a short page
fg_all <- function(fun, year, tag) {
  parts <- list(); pn <- 1
  repeat {
    d <- fun(startseason = as.character(year), endseason = as.character(year),
             pageitems = "2000", pagenum = as.character(pn))
    if (is.null(d) || nrow(d) == 0) break
    parts[[pn]] <- d
    if (nrow(d) < 2000) break
    pn <- pn + 1; Sys.sleep(2)
  }
  if (!length(parts)) NULL else bind_rows(parts)
}

tables <- list(
  bat      = baseballr::fg_bat_leaders,
  pitch    = baseballr::fg_pitcher_leaders,
  fielder  = baseballr::fg_fielder_leaders,
  team_bat = baseballr::fg_team_batter,
  team_pit = baseballr::fg_team_pitcher,
  team_fld = baseballr::fg_team_fielder
)

for (yr in from:to) for (nm in names(tables)) {
  p <- file.path(raw_dir("fangraphs"), sprintf("%s-%d.parquet", nm, yr))
  if (file.exists(p)) next
  tryCatch({
    d <- retry(2, 10, fg_all(tables[[nm]], yr, nm))
    if (!is.null(d) && nrow(d) > 0) write_df(d, p)
  }, error = function(e) message("fangraphs: FAILED ", nm, "-", yr, " - ",
                                 substr(conditionMessage(e), 1, 90)))
  Sys.sleep(2)
}

# one-time ancillary tables
once <- list(
  guts         = function() baseballr::fg_guts(),
  park         = function() baseballr::fg_park(),
  proj_steamer = function() baseballr::fg_projections(type = "steamer", stats = "bat"),
  proj_zips    = function() baseballr::fg_projections(type = "zips", stats = "bat")
)
for (nm in names(once)) {
  p <- file.path(raw_dir("fangraphs"), sprintf("%s.parquet", nm))
  if (file.exists(p)) next
  tryCatch({ d <- retry(2, 10, once[[nm]]())
             if (is.data.frame(d) && nrow(d) > 0) write_df(d, p) },
           error = function(e) message("fangraphs: FAILED ", nm, " - ",
                                       substr(conditionMessage(e), 1, 90)))
  Sys.sleep(2)
}
message("fangraphs: done (", from, "-", to, ")")
