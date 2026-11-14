# ingest.R - download baseball data into the project's data/raw/ tree (parquet)
#
# Usage (from the project root):
#   Rscript R/ingest.R lahman
#   Rscript R/ingest.R chadwick
#   Rscript R/ingest.R retrosheet 2015 2025
#   Rscript R/ingest.R fangraphs 2015 2025
#   Rscript R/ingest.R statcast  2021 2025      # per-pitch: BIG, chunked monthly
#   Rscript R/ingest.R all       2021 2025
#
# Design: idempotent (skips files that already exist), chunked, per-chunk
# tryCatch (one bad download never kills the loop), Sys.sleep between
# requests to be polite to MLB/Retrosheet/FanGraphs servers.

suppressPackageStartupMessages({
  library(arrow)
  library(purrr)
})

raw_dir <- function(...) {
  d <- file.path("data", "raw", ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

write_df <- function(df, path) {
  if (is.data.frame(df)) arrow::write_parquet(df, path)
  else saveRDS(df, sub("\\.parquet$", ".rds", path))
  message(sprintf("  wrote %s (%s rows)", path, nrow(df)))
}

ingest_lahman <- function() {
  library(Lahman)
  items <- data(package = "Lahman")$results[, "Item"]
  for (ds in items) write_df(get(ds), file.path(raw_dir("lahman"), paste0(ds, ".parquet")))
}

ingest_chadwick <- function() {
  lu <- baseballr::get_chadwick_lu()
  write_df(lu, file.path(raw_dir("chadwick"), "register.parquet"))
}

ingest_retrosheet <- function(years) {
  for (yr in years) {
    p <- file.path(raw_dir("retrosheet"), paste0(yr, ".parquet"))
    if (file.exists(p)) { message("retrosheet: skip existing ", yr); next }
    tryCatch({
      d <- baseballr::get_retrosheet_data(yr)
      write_df(d, p)
    }, error = function(e) message("retrosheet: FAILED ", yr, " - ", conditionMessage(e)))
    Sys.sleep(3)
  }
}

ingest_fangraphs <- function(years) {
  fams <- list(
    bat     = baseballr::fg_bat_leaders,
    pitch   = baseballr::fg_pitcher_leaders,
    team_b  = baseballr::fg_team_batter,
    team_p  = baseballr::fg_team_pitcher,
    guts    = function(y) baseballr::fg_guts()   # wOBA/FIP constants (not per-year)
  )
  for (yr in years) for (nm in names(fams)) {
    p <- file.path(raw_dir("fangraphs"), sprintf("%s-%d.parquet", nm, yr))
    if (nm == "guts") p <- file.path(raw_dir("fangraphs"), "guts.parquet")
    if (file.exists(p)) { message("fangraphs: skip existing ", basename(p)); next }
    tryCatch({
      d <- fams[[nm]](yr)
      write_df(d, p)
    }, error = function(e) message("fangraphs: FAILED ", nm, "-", yr, " - ", conditionMessage(e)))
    Sys.sleep(3)
  }
}

ingest_statcast <- function(years) {
  # per-pitch Statcast via Savant: monthly chunks keep each request small
  for (yr in years) for (m in 3:11) {   # spring training (Mar) through WS (Oct/Nov)
    start <- as.Date(sprintf("%d-%02d-01", yr, m))
    end   <- min(seq(as.Date(sprintf("%d-%02d-01", yr, m + 1L)), by = "-1 day", length.out = 1),
                 Sys.Date())
    p <- file.path(raw_dir("statcast"), sprintf("statcast-%d-%02d.parquet", yr, m))
    if (end < start || file.exists(p)) next
    tryCatch({
      d <- baseballr::scrape_statcast_savant(start_date = start, end_date = end)
      write_df(d, p)
    }, error = function(e) message("statcast: FAILED ", yr, "-", m, " - ", conditionMessage(e)))
    Sys.sleep(5)  # politeness
  }
}

# ---- main -------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
what  <- if (length(args) >= 1) args[1] else "help"
years <- if (length(args) >= 3) as.integer(args[2]:args[3]) else
  as.integer(format(Sys.Date(), "%Y")):as.integer(format(Sys.Date(), "%Y"))

message("ingest: ", what, if (what != "lahman" && what != "chadwick")
  sprintf(" (%d-%d)", min(years), max(years)))

switch(what,
  lahman     = ingest_lahman(),
  chadwick   = ingest_chadwick(),
  retrosheet = ingest_retrosheet(years),
  fangraphs  = ingest_fangraphs(years),
  statcast   = ingest_statcast(years),
  all        = { ingest_lahman(); ingest_chadwick();
                 ingest_retrosheet(years); ingest_fangraphs(years);
                 ingest_statcast(years) },
  message("unknown source. use: lahman | chadwick | retrosheet | fangraphs | statcast | all ",
          "[start-year end-year]")
)
