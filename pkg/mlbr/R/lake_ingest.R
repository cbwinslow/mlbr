# lake_ingest.R - one-call orchestrator for the whole lake
#
# ORDER & SERIALIZATION (rate-limit safety):
#   - cheap local/no-HTTP sources first
#   - savant-backed sources (statcast, milb) NEVER run concurrently
#   - bbref last (strictest scraper; 12s spacing)
# All steps are idempotent: existing files are skipped, so re-running only
# fills in what is missing.

#' Ingest Everything (All Sources, All History)
#'
#' @param lake project root of the data lake
#' @param sources character vector subset to run (default: all)
#' @param statcast_years,milb_years,fg_years,bbref_years,espn_years,meta_years
#'   year ranges per source
#' @return invisible TRUE
#' @export
lake_ingest <- function(lake = NULL,
                        sources = c("lahman", "chadwick", "retrosheet",
                                    "ncaa", "mlbmeta", "fangraphs",
                                    "statcast", "milb", "espn", "bbref"),
                        statcast_years = c(2008L, 2026L),
                        milb_years = c(2021L, 2026L),
                        fg_years = c(1901L, 2026L),
                        bbref_years = c(2015L, 2026L),
                        espn_years = c(2002L, 2026L),
                        meta_years = c(2008L, 2026L)) {
  lake <- default_lake(lake)
  if ("lahman" %in% sources)      lahman_ingest(lake)
  if ("chadwick" %in% sources)    chadwick_ingest(lake)
  if ("retrosheet" %in% sources)  retrosheet_github_ingest(lake, 1871, 2026)
  if ("ncaa" %in% sources)        ncaa_ingest(lake)
  if ("mlbmeta" %in% sources)     mlb_meta_ingest(lake, meta_years[1], meta_years[2])
  if ("fangraphs" %in% sources)   fangraphs_ingest(lake, fg_years[1], fg_years[2])
  if ("statcast" %in% sources)    statcast_ingest(lake, statcast_years[1], statcast_years[2])
  if ("milb" %in% sources)        milb_ingest(lake, milb_years[1], milb_years[2])
  if ("espn" %in% sources)        espn_ingest(lake, espn_years[1], espn_years[2])
  if ("bbref" %in% sources)       bbref_ingest(lake, bbref_years[1], bbref_years[2])
  message("lake_ingest: done")
  invisible(TRUE)
}
