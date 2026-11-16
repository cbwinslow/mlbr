# General utility functions for mlbr data lake

#' Find column names across all parquet tables in the lake
#'
#' @param pattern regular expression pattern to match column names
#' @param lake project root of the data lake
#' @return a tibble of matching tables, column names, and types
#' @export
lake_find_column <- function(pattern, lake = NULL) {
  lake <- default_lake(lake)
  raw_dir <- file.path(lake, "data", "raw")
  if (!dir.exists(raw_dir)) stop("Raw data directory not found at: ", raw_dir)
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for lake_find_column().")
  }
  
  sources <- list.dirs(raw_dir, recursive = FALSE, full.names = FALSE)
  matches <- list()
  
  for (src in sources) {
    src_dir <- file.path(raw_dir, src)
    sample_files <- list.files(src_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(sample_files) == 0) next
    
    # Read schema from first file
    schema <- tryCatch(
      arrow::open_dataset(sample_files[1])$schema,
      error = function(e) NULL
    )
    if (is.null(schema)) next
    
    cnames <- names(schema)
    ctypes <- vapply(schema$fields, function(f) f$type$ToString(), character(1))
    
    hits <- grep(pattern, cnames, ignore.case = TRUE)
    if (length(hits) > 0) {
      matches[[src]] <- data.frame(
        source = src,
        column = cnames[hits],
        type = ctypes[hits],
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(matches) == 0) {
    message("No columns matching pattern '", pattern, "' were found.")
    return(invisible(data.frame(source = character(), column = character(), type = character())))
  }
  
  do.call(rbind, matches)
}

#' Connect to DuckDB and register zero-copy views over the data lake
#'
#' @param lake project root of the data lake
#' @param dbdir duckdb database location (default: ":memory:")
#' @param read_only logical, whether connection should be read only
#' @return a DBIConnection to DuckDB with pre-registered views
#' @export
lake_duckdb <- function(lake = NULL, dbdir = ":memory:", read_only = TRUE) {
  if (!requireNamespace("duckdb", quietly = TRUE) || !requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'duckdb' and 'DBI' must be installed to use lake_duckdb().")
  }
  lake <- default_lake(lake)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only)
  
  # Register primary views
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
    v_ncaa_pbp            = file.path(lake, "data/raw/ncaa/play_by_play.parquet"),
    v_mlbmeta_schedule    = file.path(lake, "data/raw/mlbmeta/schedule-*.parquet")
  )
  
  registered <- 0L
  for (vname in names(views)) {
    p <- views[[vname]]
    matching <- Sys.glob(p)
    if (length(matching) > 0) {
      sql <- sprintf("CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s', union_by_name = true);", vname, p)
      DBI::dbExecute(con, sql)
      registered <- registered + 1L
    }
  }

  # Register convenient non-prefixed aliases
  aliases <- list(
    statcast            = "v_statcast",
    retrosheet_plays    = "v_retrosheet_plays",
    retro_plays         = "v_retrosheet_plays",
    retrosheet_games    = "v_retrosheet_games",
    retro_games         = "v_retrosheet_games",
    retrosheet_subs     = "v_retrosheet_subs",
    retro_subs          = "v_retrosheet_subs",
    retrosheet_rosters  = "v_retrosheet_rosters",
    retro_rosters       = "v_retrosheet_rosters",
    fangraphs_bat       = "v_fangraphs_bat",
    fangraphs_pitch     = "v_fangraphs_pitch",
    fangraphs_fielder   = "v_fangraphs_fielder",
    chadwick_register   = "v_chadwick_register",
    chadwick            = "v_chadwick_register",
    lahman_batting      = "v_lahman_batting",
    lahman_pitching     = "v_lahman_pitching",
    lahman_people       = "v_lahman_people",
    ncaa_pbp            = "v_ncaa_pbp"
  )
  for (aname in names(aliases)) {
    target <- aliases[[aname]]
    p <- views[[target]]
    if (!is.null(p) && length(Sys.glob(p)) > 0) {
      tryCatch(
        DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW %s AS SELECT * FROM %s;", aname, target)),
        error = function(e) NULL
      )
    }
  }
  
  # Register derived features if statcast is mounted
  if ("v_statcast" %in% names(views) && length(Sys.glob(views[["v_statcast"]])) > 0) {
    tryCatch({
      lake_register_features(con)
      registered <- registered + 1L
    }, error = function(e) NULL)
  }

  message(sprintf("Mounted %d zero-copy DuckDB views over %s.", registered, lake))
  active_views <- names(views)[vapply(views, function(p) length(Sys.glob(p)) > 0, logical(1))]
  if ("v_statcast" %in% active_views) active_views <- c(active_views, "v_statcast_features")
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
  src_dir <- file.path(lake, "data", "raw", source)
  if (!dir.exists(src_dir)) stop("Source directory does not exist: ", src_dir)
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for lake_describe().")
  }
  
  if (is.null(file_pattern)) {
    files <- list.files(src_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0) stop("No parquet files found in ", src_dir)
    target_file <- files[length(files)] # use most recent file
  } else {
    target_file <- file.path(src_dir, file_pattern)
    if (!file.exists(target_file)) stop("Target file does not exist: ", target_file)
  }
  
  # Read schema and row count
  ds <- arrow::open_dataset(target_file)
  schema <- ds$schema
  row_count <- ds$num_rows
  
  cnames <- names(schema)
  ctypes <- vapply(schema$fields, function(f) f$type$ToString(), character(1))
  
  # Sample 5000 rows to calculate null rates efficiently
  sample_df <- head(ds, 5000) %>% as.data.frame()
  null_rates <- vapply(sample_df, function(col) mean(is.na(col)), numeric(1))
  
  data.frame(
    column = cnames,
    type = ctypes,
    sample_null_pct = round(null_rates[cnames] * 100, 1),
    stringsAsFactors = FALSE
  )
}
