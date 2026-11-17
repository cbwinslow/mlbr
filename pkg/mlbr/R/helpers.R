# Internal helper utilities for mlbr

#' Throttle execution by sleeping
#' @param seconds Number of seconds to sleep
#' @keywords internal
throttle <- function(seconds = 1) {
  Sys.sleep(seconds)
}

#' Retry expression on failure
#' @param expr Expression to evaluate
#' @param max_attempts Maximum attempts
#' @param delay Initial delay
#' @param backoff Exponential multiplier
#' @keywords internal
retry <- function(expr, max_attempts = 3, delay = 2, backoff = 2) {
  attempt <- 1
  cur_delay <- delay
  while (attempt <= max_attempts) {
    res <- tryCatch(
      list(val = expr, err = NULL),
      error = function(e) list(val = NULL, err = e)
    )
    if (is.null(res$err)) return(res$val)
    if (attempt == max_attempts) stop(res$err)
    message(sprintf("Attempt %d failed: %s. Retrying in %.1fs...", attempt, res$err$message, cur_delay))
    Sys.sleep(cur_delay)
    cur_delay <- cur_delay * backoff
    attempt <- attempt + 1
  }
}

#' Write a data frame or tibble to parquet or rds safely
#' @param df Data frame to write
#' @param path Target path (.parquet or .rds)
#' @param partition_cols Optional character vector of partition columns
#' @keywords internal
write_df <- function(df, path, partition_cols = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    warning("write_df received empty data frame; skipping write to ", path)
    return(invisible(FALSE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(if (file.exists(tmp)) unlink(tmp, recursive = TRUE), add = TRUE)
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to write parquet files.")
    }
    if (!is.null(partition_cols) && length(partition_cols) > 0) {
      arrow::write_dataset(df, tmp, partitioning = partition_cols, format = "parquet")
    } else {
      arrow::write_parquet(df, tmp)
    }
  } else if (ext == "rds") {
    saveRDS(df, tmp, compress = "xz")
  } else {
    stop("Unsupported extension: ", ext)
  }
  if (file.exists(path)) unlink(path, recursive = TRUE)
  file.rename(tmp, path)
  invisible(TRUE)
}

#' Check basic directory structure of baseball data lake
#' @param lake_path Root path of the data lake
#' @return Logical TRUE if valid, FALSE otherwise
has_lake_dirs <- function(lake_path = default_lake()) {
  expected <- c("data/raw", "data/processed")
  all(dir.exists(file.path(lake_path, expected)))
}

#' Determine data lake root path from arguments, options, env, or defaults
#' @param lake explicit path or NULL
#' @return absolute or normalized directory path
#' @export
default_lake <- function(lake = NULL) {
  if (!missing(lake) && !is.null(lake) && nzchar(lake)) return(normalizePath(lake, mustWork = FALSE))
  env_p <- Sys.getenv("MLBR_DATA_PATH", unset = "")
  if (nzchar(env_p)) return(normalizePath(env_p, mustWork = FALSE))
  opt_p <- getOption("mlbr.path", NULL)
  if (!is.null(opt_p) && nzchar(opt_p)) return(normalizePath(opt_p, mustWork = FALSE))
  if (dir.exists("/mnt/storage/Rprojects/mlbr")) return("/mnt/storage/Rprojects/mlbr")
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
