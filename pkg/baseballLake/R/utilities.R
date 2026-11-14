# utilities.R - High-value workflow and exploration utilities for baseballLake

#' Find which datasets in the lake contain a specific column
#'
#' Searches across all Parquet schema metadata across all sources in the data lake
#' without loading the data into memory.
#'
#' @param pattern regex pattern for column name (e.g. "attack_angle", "wOBA", "fld")
#' @param lake project root of the data lake
#' @return a tibble of matching sources, sample files, and exact column names
#' @export
lake_find_column <- function(pattern, lake = NULL) {
  lake <- default_lake(lake)
  raw_dir <- file.path(lake, "data", "raw")
  sources <- list.dirs(raw_dir, recursive = FALSE)
  
  results <- list()
  for (src_path in sources) {
    src_name <- basename(src_path)
    # Get distinct parquet files (or prefixes)
    fs <- list.files(src_path, pattern = "parquet$", recursive = TRUE, full.names = TRUE)
    if (!length(fs)) next
    
    # Check sample files per unique prefix to cover all sub-tables
    basenames <- basename(fs)
    prefixes <- unique(sub("-\\d{4}.*parquet$", "", basenames))
    prefixes <- unique(sub("\\.parquet$", "", prefixes))
    
    for (pfx in prefixes) {
      match_files <- fs[grepl(paste0("^", pfx), basename(fs))]
      if (!length(match_files)) match_files <- fs[1]
      sample_f <- match_files[1]
      
      tryCatch({
        cols <- arrow::open_dataset(sample_f)$schema$names
        matched_cols <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
        if (length(matched_cols)) {
          results[[length(results) + 1L]] <- data.frame(
            source = src_name,
            table_prefix = pfx,
            matched_column = matched_cols,
            sample_file = basename(sample_f),
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) NULL)
    }
  }
  
  if (!length(results)) {
    message(sprintf("No columns matching pattern '%s' found in the data lake.", pattern))
    return(invisible(tibble::tibble()))
  }
  
  out <- tibble::as_tibble(do.call(rbind, results))
  out
}

#' Connect DuckDB and register zero-copy views over the baseball data lake
#'
#' Instantly mounts SQL views over the entire Parquet data lake using DuckDB.
#' Allows direct, sub-second analytical SQL queries across Statcast, Retrosheet,
#' FanGraphs, and Lahman without copying data.
#'
#' @param lake project root of the data lake
#' @param dbdir DuckDB database directory or ":memory:" (default)
#' @param read_only whether the DuckDB connection is read-only (default TRUE)
#' @return a DuckDB DBI connection object with registered views
#' @export
lake_duckdb <- function(lake = NULL, dbdir = ":memory:", read_only = TRUE) {
  if (!requireNamespace("duckdb", quietly = TRUE) || !requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'duckdb' and 'DBI' must be installed to use lake_duckdb().")
  }
  lake <- default_lake(lake)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only)
  
  # Register views
  views <- list(
    v_statcast            = file.path(lake, "data/raw/statcast/*.parquet"),
    v_retrosheet_plays    = file.path(lake, "data/raw/retrosheet/plays-*.parquet"),
    v_retrosheet_games    = file.path(lake, "data/raw/retrosheet/games-*.parquet"),
    v_retrosheet_subs     = file.path(lake, "data/raw/retrosheet/subs-*.parquet"),
    v_retrosheet_rosters  = file.path(lake, "data/raw/retrosheet/rosters-*.parquet"),
    v_fangraphs_bat       = file.path(lake, "data/raw/fangraphs/bat-*.parquet"),
    v_fangraphs_pitch     = file.path(lake, "data/raw/fangraphs/pitch-*.parquet"),
    v_fangraphs_fielder   = file.path(lake, "data/raw/fangraphs/fielder-*.parquet"),
    v_chadwick_register   = file.path(lake, "data/raw/chadwick/register.parquet"),
    v_lahman_batting      = file.path(lake, "data/raw/lahman/Batting.parquet"),
    v_lahman_pitching     = file.path(lake, "data/raw/lahman/Pitching.parquet"),
    v_lahman_people       = file.path(lake, "data/raw/lahman/People.parquet"),
    v_ncaa_pbp            = file.path(lake, "data/raw/ncaa/play_by_play.parquet")
  )
  
  registered <- 0L
  for (vname in names(views)) {
    p <- views[[vname]]
    # Check if files matching the pattern exist
    matching <- Sys.glob(p)
    if (length(matching) > 0) {
      sql <- sprintf("CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s');", vname, p)
      DBI::dbExecute(con, sql)
      registered <- registered + 1L
    }
  }
  
  message(sprintf("Mounted %d zero-copy DuckDB views over %s.", registered, lake))
  active_views <- names(views)[vapply(views, function(p) length(Sys.glob(p)) > 0, logical(1))]
  message("Available views: ", paste(active_views, collapse = ", "))
  con
}

#' Describe schema and null rate summary for any table in the lake
#'
#' @param source dataset name (e.g. "statcast", "retrosheet", "fangraphs")
#' @param file_pattern optional specific file pattern (e.g. "plays-2024.parquet")
#' @param lake project root of the data lake
#' @return a tibble of column names, data types, and null rates
#' @export
lake_describe <- function(source, file_pattern = NULL, lake = NULL) {
  lake <- default_lake(lake)
  dir_path <- file.path(lake, "data", "raw", source)
  if (!dir.exists(dir_path)) stop(sprintf("Source '%s' not found under %s/data/raw", source, lake))
  
  fs <- list.files(dir_path, pattern = "parquet$", recursive = TRUE, full.names = TRUE)
  if (!length(fs)) stop(sprintf("No parquet files found in %s", dir_path))
  
  target_file <- if (!is.null(file_pattern)) {
    matching <- grep(file_pattern, fs, value = TRUE)
    if (!length(matching)) stop("No file matching pattern: ", file_pattern)
    matching[1]
  } else fs[length(fs)]  # latest file by default
  
  schema <- arrow::open_dataset(target_file)$schema
  ds <- arrow::open_dataset(target_file)$schema
  
  data.frame(
    column_name = schema$names,
    type = vapply(schema$fields, function(f) f$type$ToString(), character(1)),
    stringsAsFactors = FALSE
  ) |> tibble::as_tibble()
}
