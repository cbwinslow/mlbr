# manager.R - R6 LakeManager object-oriented interface for mlbr

#' R6 Class Representing a Baseball Data Lake
#'
#' Provides an object-oriented, chained interface for inspecting, validating,
#' querying, ingesting, and monitoring the baseball data lake.
#'
#' @importFrom R6 R6Class
#' @export
LakeManager <- R6::R6Class(
  "LakeManager",
  public = list(
    #' @field path Root directory path of the data lake
    path = NULL,

    #' @description
    #' Initialize a new LakeManager instance.
    #' @param path root directory of the lake (defaults to configured or environment path)
    #' @return A new `LakeManager` object.
    initialize = function(path = NULL) {
      self$path <- default_lake(path)
      message(sprintf("LakeManager initialized at: %s", self$path))
    },

    #' @description
    #' Display row count and size inventory across all raw datasets.
    #' @return Invisibly a data frame of lake status.
    status = function() {
      lake_status(self$path)
    },

    #' @description
    #' Execute the full 28-check validation and proof suite.
    #' @param verbose print formatted report to console (default TRUE)
    #' @return Invisibly a tibble of test results.
    proof = function(verbose = TRUE) {
      proof_lake(self$path, verbose = verbose)
    },

    #' @description
    #' Connect DuckDB and register zero-copy SQL views over the data lake.
    #' @param dbdir DuckDB database directory or ":memory:" (default)
    #' @param read_only read-only flag (default TRUE)
    #' @return A DBI DuckDB connection object.
    duckdb = function(dbdir = ":memory:", read_only = TRUE) {
      lake_duckdb(self$path, dbdir = dbdir, read_only = read_only)
    },

    #' @description
    #' Search schemas across all datasets for matching column names.
    #' @param pattern regex string for column search (e.g. "attack_angle", "wOBA")
    #' @return A tibble of matching tables and columns.
    find = function(pattern) {
      lake_find_column(pattern, self$path)
    },

    #' @description
    #' Describe columns and types for a specific dataset or file.
    #' @param source dataset name (e.g. "statcast", "retrosheet", "fangraphs")
    #' @param pattern optional file pattern
    #' @return A tibble of column metadata.
    describe = function(source, pattern = NULL) {
      lake_describe(source, file_pattern = pattern, lake = self$path)
    },

    #' @description
    #' Ingest or update data sources into the lake.
    #' @param source dataset name: "statcast", "retrosheet", "fangraphs", "lahman", "chadwick", "mlbmeta", "milb", "espn", "bbref", "ncaa", or "all".
    #' @param ... arguments passed to underlying source ingestion functions (e.g. from = 2008, to = 2026).
    #' @return Invisibly TRUE on success.
    ingest = function(source = "all", ...) {
      if (source == "all") {
        lake_ingest(self$path, ...)
      } else if (source == "statcast") {
        statcast_ingest(self$path, ...)
      } else if (source == "retrosheet") {
        retrosheet_github_ingest(self$path, ...)
      } else if (source == "fangraphs") {
        fangraphs_ingest(self$path, ...)
      } else if (source == "lahman") {
        lahman_ingest(self$path)
      } else if (source == "chadwick") {
        chadwick_ingest(self$path)
      } else if (source == "mlbmeta") {
        mlb_meta_ingest(self$path, ...)
      } else if (source == "milb") {
        milb_ingest(self$path, ...)
      } else if (source == "espn") {
        espn_ingest(self$path, ...)
      } else if (source == "bbref") {
        bbref_ingest(self$path, ...)
      } else if (source == "ncaa") {
        ncaa_ingest(self$path)
      } else {
        stop(sprintf("Unknown source: '%s'. Valid options: statcast, retrosheet, fangraphs, lahman, chadwick, mlbmeta, milb, espn, bbref, ncaa, all", source))
      }
    },

    #' @description
    #' Check the tail of running background ingest processes.
    #' @param n number of tail lines to display (default 20)
    #' @return Invisibly the log lines.
    monitor = function(n = 20L) {
      log_file <- "/tmp/baseball-ingest.log"
      if (!file.exists(log_file)) {
        message("No background ingest log found at ", log_file)
        return(invisible(character()))
      }
      lines <- tail(readLines(log_file, warn = FALSE), n)
      cat(paste0("--- INGEST LOG TAIL (/tmp/baseball-ingest.log) ---\n"))
      cat(paste(lines, collapse = "\n"), "\n")
      invisible(lines)
    }
  )
)
