# ==============================================================================
# mlbr Data Lake: Full-Population Commit-Point Tunneling Materializer (Silver)
# ==============================================================================
# Framework: Medallion Architecture (Silver v_statcast_features -> Silver Tunneling)
# Invariant: 100% Full-Population Scoring (Zero Subsampling, 2008-2026)
# Storage:   ADR-001 ZSTD Parquet, ROW_GROUP_SIZE = 100000, Hive by game_year,
#            strictly sorted by (pitcher, game_date, game_pk, at_bat_number, pitch_number)
#
# Destination: data/features/statcast_tunneling/ partitioned by game_year
# ==============================================================================

suppressPackageStartupMessages({
  library(mlbr)
  library(DBI)
  library(duckdb)
})

run_materialize_tunneling <- function() {
  cat("==============================================================================\n")
  cat("       MLBR FULL-POPULATION PITCH TUNNELING MATERIALIZER (SILVER TIER)        \n")
  cat("==============================================================================\n\n")

  lake <- mlbr::default_lake()
  sql_path <- file.path(lake, "sql", "02_pitch_tunneling.sql")
  if (!file.exists(sql_path)) {
    stop("Canonical tunneling SQL not found at: ", sql_path)
  }

  out_dir <- file.path(lake, "data", "features", "statcast_tunneling")
  stage_dir <- file.path(lake, "data", "features", "statcast_tunneling_staging")
  backup_dir <- file.path(lake, "data", "features", "statcast_tunneling_prev")

  if (dir.exists(stage_dir)) {
    if (unlink(stage_dir, recursive = TRUE) != 0L || dir.exists(stage_dir)) {
      stop("Failed to clear staging directory: ", stage_dir)
    }
  }
  if (!dir.create(stage_dir, recursive = TRUE)) {
    stop("Failed to create staging directory: ", stage_dir)
  }

  cat("-> Connecting to DuckDB Data Lake (reading Silver v_statcast_features)...\n")
  con <- mlbr::lake_duckdb(lake = lake)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads TO 16; SET memory_limit = '16GB'; SET preserve_insertion_order = true;")

  raw_sql <- paste(readLines(sql_path, warn = FALSE), collapse = "\n")
  tunnel_select <- sub(";\\s*$", "", raw_sql)

  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TEMP VIEW tmp_tunnel_src AS %s;", tunnel_select))

  seasons <- DBI::dbGetQuery(con, "SELECT DISTINCT game_year FROM v_statcast_features WHERE game_year IS NOT NULL ORDER BY game_year")$game_year
  if (length(seasons) < 19L) {
    stop("Expected >= 19 seasons in v_statcast_features, got ", length(seasons))
  }

  cat(sprintf("-> Materializing %d strictly sorted Hive partitions (ADR-001 ZSTD, ROW_GROUP_SIZE=100000)...\n", length(seasons)))
  t0 <- proc.time()

  for (yr in seasons) {
    yr_dir <- file.path(stage_dir, sprintf("game_year=%d", yr))
    if (!dir.create(yr_dir, recursive = TRUE)) stop("Failed to create partition dir: ", yr_dir)
    out_file <- file.path(yr_dir, "data_0.parquet")

    DBI::dbExecute(con, "SET threads TO 16;")
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tmp_yr_sorted AS
      SELECT * EXCLUDE (game_year)
      FROM tmp_tunnel_src
      WHERE game_year = %d
      ORDER BY pitcher, game_date, game_pk, at_bat_number, pitch_number;
    ", as.integer(yr)))

    DBI::dbExecute(con, "SET threads TO 1; SET preserve_insertion_order = true;")
    DBI::dbExecute(con, sprintf("
      COPY (SELECT * FROM tmp_yr_sorted ORDER BY pitcher, game_date, game_pk, at_bat_number, pitch_number)
      TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000);
    ", out_file))
    DBI::dbExecute(con, "DROP TABLE tmp_yr_sorted;")
  }

  DBI::dbExecute(con, "SET threads TO 16;")
  t1 <- proc.time()
  elapsed <- round((t1 - t0)[3], 2)
  cat(sprintf("-> Staging materialization complete in %s seconds!\n", elapsed))

  cat("-> Verifying staged Silver tunneling dataset (fail-fast invariants & ADR-001 sort order)...\n")
  summary_sql <- sprintf("
    SELECT
      COUNT(*) AS total_rows,
      COUNT(DISTINCT game_year) AS seasons,
      MIN(game_date) AS min_date,
      MAX(game_date) AS max_date,
      ROUND(AVG(tunnel_dist_in), 2) AS mean_tunnel_in,
      ROUND(MEDIAN(tunnel_dist_in), 2) AS med_tunnel_in,
      ROUND(AVG(plate_sep_in), 2) AS mean_plate_sep_in,
      ROUND(AVG(post_tunnel_break_in), 2) AS mean_post_break_in,
      ROUND(SUM(post_tunnel_break_in) / NULLIF(SUM(tunnel_dist_in), 0), 3) AS agg_break_tunnel_ratio,
      ROUND(SUM(plate_sep_in) / NULLIF(SUM(tunnel_dist_in), 0), 3) AS agg_plate_tunnel_ratio,
      SUM(CASE WHEN tunnel_dist_in IS NULL OR ISNAN(tunnel_dist_in)
                    OR tunnel_dist_in < 0 OR tunnel_dist_in > 240
                    OR x_dec IS NULL OR ISNAN(x_dec)
                    OR z_dec IS NULL OR ISNAN(z_dec)
                    OR plate_sep_in IS NULL OR ISNAN(plate_sep_in)
                    OR post_tunnel_break_in IS NULL OR ISNAN(post_tunnel_break_in)
                    OR break_to_tunnel_ratio IS NULL OR ISNAN(break_to_tunnel_ratio)
                    OR plate_to_tunnel_ratio IS NULL OR ISNAN(plate_to_tunnel_ratio)
                    OR delta_velo_mph IS NULL OR ISNAN(delta_velo_mph)
                    OR delta_flight_ms IS NULL OR ISNAN(delta_flight_ms)
                    OR pitch_sequence IS NULL OR pitch_type IS NULL
               THEN 1 ELSE 0 END) AS bad_rows
    FROM read_parquet('%s/*/*.parquet', hive_partitioning = true);
  ", stage_dir)

  summary_df <- DBI::dbGetQuery(con, summary_sql)
  print(as.data.frame(summary_df))

  dup_df <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) AS dup_groups
    FROM (
      SELECT game_pk, at_bat_number, pitch_number
      FROM read_parquet('%s/*/*.parquet', hive_partitioning = true)
      GROUP BY game_pk, at_bat_number, pitch_number
      HAVING COUNT(*) > 1
    ) d;
  ", stage_dir))
  cat(sprintf("-> Composite key duplicate groups: %d\n", dup_df$dup_groups))

  DBI::dbExecute(con, "SET threads TO 1; SET preserve_insertion_order = true;")
  total_inversions <- 0L
  rg_overlaps <- 0L
  for (yr in seasons) {
    f_yr <- file.path(stage_dir, sprintf("game_year=%d", yr), "data_0.parquet")
    inv <- DBI::dbGetQuery(con, sprintf("
      WITH seq AS (
        SELECT pitcher, LAG(pitcher) OVER () AS prev_pitcher
        FROM read_parquet('%s')
      )
      SELECT COUNT(*) AS n_inv FROM seq WHERE prev_pitcher IS NOT NULL AND pitcher < prev_pitcher;
    ", f_yr))$n_inv
    total_inversions <- total_inversions + as.integer(inv)

    rg <- DBI::dbGetQuery(con, sprintf("
      SELECT row_group_id,
             CAST(stats_min_value AS BIGINT) AS min_p,
             CAST(stats_max_value AS BIGINT) AS max_p
      FROM parquet_metadata('%s')
      WHERE path_in_schema = 'pitcher'
      ORDER BY row_group_id;
    ", f_yr))
    if (nrow(rg) > 1L) {
      for (k in 2:nrow(rg)) {
        if (rg$min_p[k] < rg$max_p[k - 1L]) rg_overlaps <- rg_overlaps + 1L
      }
    }
  }
  cat(sprintf("-> ADR-001 sort check across %d seasons: pitcher inversions = %d | row-group min-max overlaps = %d\n",
              length(seasons), total_inversions, rg_overlaps))

  if (is.na(summary_df$total_rows) || summary_df$total_rows < 9500000) {
    stop("Staging verification failed: unexpected row count ", summary_df$total_rows)
  }
  if (is.na(summary_df$seasons) || summary_df$seasons < 19) {
    stop("Staging verification failed: expected >= 19 seasons, got ", summary_df$seasons)
  }
  if (is.na(summary_df$bad_rows) || summary_df$bad_rows > 0) {
    stop("Staging verification failed: detected ", summary_df$bad_rows, " invalid/NULL tunneling rows")
  }
  if (is.na(dup_df$dup_groups) || dup_df$dup_groups > 0) {
    stop("Staging verification failed: detected ", dup_df$dup_groups, " duplicate composite keys")
  }
  if (total_inversions > 0L || rg_overlaps > 0L) {
    stop(sprintf("ADR-001 sort verification failed: inversions=%d, rg_overlaps=%d", total_inversions, rg_overlaps))
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  if (dir.exists(backup_dir) && unlink(backup_dir, recursive = TRUE) != 0L) {
    stop("Failed to remove old backup directory: ", backup_dir)
  }
  if (dir.exists(out_dir)) {
    if (!isTRUE(file.rename(out_dir, backup_dir))) {
      stop("Failed to move existing output dir to backup: ", out_dir)
    }
  }
  if (!isTRUE(file.rename(stage_dir, out_dir))) {
    if (dir.exists(backup_dir)) file.rename(backup_dir, out_dir)
    stop("Failed to swap staging dir into canonical output dir: ", out_dir)
  }
  if (dir.exists(backup_dir) && unlink(backup_dir, recursive = TRUE) != 0L) {
    warning("Swap succeeded, but could not remove backup directory: ", backup_dir)
  }

  cat("\n*** FULL-POPULATION SILVER TUNNELING MATERIALIZATION COMPLETE (ADR-001 VERIFIED) ***\n")
}

run_materialize_tunneling()
