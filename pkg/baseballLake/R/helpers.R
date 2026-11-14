# helpers.R - shared plumbing for baseballLake ingest functions
#
# LAKE ROOT: every function takes `lake` (path to the data-lake project root).
# Raw files land in <lake>/data/raw/<source>/. Atomic writes (tmp + rename).
#' @importFrom arrow open_dataset write_parquet read_parquet
#' @importFrom dplyr filter bind_rows mutate select arrange case_when
#' @importFrom data.table fread rbindlist setnames
#' @importFrom tibble tibble
#' @importFrom purrr map map_int map_chr pmap_int
#' @importFrom utils data
# Rate limiting: the baseballr fetchers do NOT throttle themselves, so every
# network touch goes through throttle() with a source-appropriate minimum.

lake_dir <- function(lake, ...) {
  d <- file.path(lake, "data", "raw", ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

lake_raw <- function(lake, source, ...) {
  file.path(lake_dir(lake, source), ...)
}

.bbl_state <- new.env(parent = emptyenv())
.bbl_state$last_request_time <- NULL

#' enforce a minimum spacing between HTTP requests
#'
#' The upstream fetchers (baseballr) perform no rate limiting of their own.
#' Every ingest function calls this between requests with a per-source
#' minimum: FanGraphs 2s, ESPN 2s, MLB Savant 1-5s, Baseball-Reference 12s.
#'
#' @param min_seconds minimum seconds since the previous request
#' @return invisible NULL
#' @export
throttle <- function(min_seconds = 2) {
  if (!is.null(.bbl_state$last_request_time)) {
    elapsed <- as.numeric(difftime(Sys.time(), .bbl_state$last_request_time,
                                   units = "secs"))
    if (elapsed < min_seconds) Sys.sleep(min_seconds - elapsed)
  }
  .bbl_state$last_request_time <<- Sys.time()
  invisible()
}

#' retry an expression with backoff
#'
#' @param n number of attempts
#' @param wait seconds between attempts
#' @param expr expression to evaluate
#' @return the value of expr, or errors after the final attempt
#' @export
retry <- function(n = 3, wait = 10, expr) {
  for (i in 1:n) {
    out <- tryCatch(force(expr), error = function(e) {
      if (i < n) {
        message("    attempt ", i, " failed: ",
                substr(conditionMessage(e), 1, 80), " - retrying in ", wait, "s")
        Sys.sleep(wait)
      }
      NULL
    })
    if (!is.null(out)) return(out)
  }
  stop("all ", n, " attempts failed")
}

#' atomically write a data frame to parquet
#' @param df data frame to write
#' @param path destination parquet path
#' @return invisible NULL; emits a message with the row count
#' @export
write_df <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  if (is.data.frame(df)) arrow::write_parquet(df, tmp) else saveRDS(df, tmp)
  file.rename(tmp, path)
  message(sprintf("  wrote %s (%s)", path,
                  if (is.data.frame(df)) sprintf("%d rows", nrow(df)) else "list"))
  invisible()
}

#' Determine data lake root path from arguments, options, env, or defaults
#' @param lake explicit path or NULL
#' @return absolute or normalized directory path
#' @export
default_lake <- function(lake = NULL) {
  if (!missing(lake) && !is.null(lake) && nzchar(lake)) return(normalizePath(lake, mustWork = FALSE))
  env_p <- Sys.getenv("BASEBALL_LAKE_PATH", "")
  if (nzchar(env_p)) return(normalizePath(env_p, mustWork = FALSE))
  opt_p <- getOption("baseballLake.path", NULL)
  if (!is.null(opt_p) && nzchar(opt_p)) return(normalizePath(opt_p, mustWork = FALSE))
  if (dir.exists("/mnt/storage/Rprojects/mlb")) return("/mnt/storage/Rprojects/mlb")
  getwd()
}

utils::globalVariables(c(
  "GAME_ID", "dates", "flag", "game_date", "game_pk",
  "gap_days", "maxd", "mind", "month", "ndays", "rows", "year",
  "playerID", "yearID", "PlayerName", "HR", "SB", "RBI", "SO", "W",
  "key_person", "key_retro", "key_bbref", "key_mlbam",
  "release_speed", "plate_x", "plate_z", "balls", "strikes", "outs_when_up",
  "EVENT_RUNS_CT", "INN_CT", "OUTS_CT"
))
