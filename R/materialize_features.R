# ==============================================================================
# mlbr Data Lake: Full-Population Feature Materializer (Silver Tier)
# ==============================================================================
# Framework: Medallion Architecture (Bronze -> Silver Materialization)
# Invariant: 100% Full-Population Scoring (Zero Subsampling, 2008-2026)
# Storage:   ADR-001 ZSTD Parquet, ROW_GROUP_SIZE = 100000, Hive by game_year,
#            strictly sorted by (pitcher, game_date, game_pk, at_bat_number, pitch_number)
#
# Destination: data/features/statcast_features/ partitioned by game_year
# ==============================================================================

suppressPackageStartupMessages({
  library(mlbr)
  library(DBI)
  library(duckdb)
})

run_materialize_features <- function() {
  cat("==============================================================================\n")
  cat("          MLBR FULL-POPULATION FEATURE MATERIALIZER (SILVER TIER)             \n")
  cat("==============================================================================\n\n")

  lake <- mlbr::default_lake()
  out_dir <- file.path(lake, "data", "features", "statcast_features")
  stage_dir <- file.path(lake, "data", "features", "statcast_features_staging")
  backup_dir <- file.path(lake, "data", "features", "statcast_features_prev")

  if (dir.exists(stage_dir)) {
    if (unlink(stage_dir, recursive = TRUE) != 0L || dir.exists(stage_dir)) {
      stop("Failed to clear staging directory: ", stage_dir)
    }
  }
  if (!dir.create(stage_dir, recursive = TRUE)) {
    stop("Failed to create staging directory: ", stage_dir)
  }

  cat("-> Connecting to DuckDB Data Lake (forcing dynamic CTE over Bronze v_statcast)...\n")
  con <- mlbr::lake_duckdb(lake = lake)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET threads TO 16; SET memory_limit = '16GB'; SET preserve_insertion_order = true;")
  mlbr::lake_register_features(con, lake = lake, force_dynamic = TRUE)

  seasons <- DBI::dbGetQuery(con, "
    SELECT DISTINCT EXTRACT(YEAR FROM game_date)::INTEGER AS game_year
    FROM v_statcast_features
    WHERE game_date IS NOT NULL
    ORDER BY game_year;
  ")$game_year
  if (length(seasons) < 19L) {
    stop("Expected >= 19 seasons in Bronze v_statcast, got ", length(seasons))
  }

  cat(sprintf("-> Materializing %d strictly sorted Silver Hive partitions (ADR-001 ZSTD, ROW_GROUP_SIZE=100000)...\n", length(seasons)))
  t0 <- proc.time()

  for (yr in seasons) {
    yr_dir <- file.path(stage_dir, sprintf("game_year=%d", yr))
    if (!dir.create(yr_dir, recursive = TRUE)) stop("Failed to create partition dir: ", yr_dir)
    out_file <- file.path(yr_dir, "data_0.parquet")

    DBI::dbExecute(con, "SET threads TO 16;")
    DBI::dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tmp_feat_yr AS
      SELECT
        -- 1. Composite Primary / Foreign Keys (Links 1:1 back to Bronze raw data)
        game_pk,
        game_date,
        at_bat_number,
        pitch_number,
        pitcher,
        batter,
        pitch_type,
        pitch_name,
        home_team,
        away_team,
        game_type,
        tracking_era,

        -- 2. Batter / Pitcher Matchup & Leverage Context
        stand,
        p_throws,
        matchup,
        is_same_hand,
        balls,
        strikes,
        outs_when_up,
        inning,
        inning_topbot,
        n_thruorder_pitcher,
        n_priorpa_thisgame_player_at_bat,
        age_pit,
        age_bat,

        -- 3. Release Dynamics & Arm Slot
        release_speed,
        effective_speed,
        release_spin_rate,
        spin_axis,
        release_pos_x,
        release_pos_x_arm,
        release_pos_y,
        release_pos_z,
        release_extension,
        arm_angle_deg,

        -- 4. Raw Trajectory Tracking Parameters (50 ft to plate)
        vx0, vy0, vz0, ax, ay, az,

        -- 5. Plate Crossing Coordinates & Rulebook Zone
        plate_x,
        plate_z,
        sz_top,
        sz_bot,
        zone,

        -- 6. Silver Derived Aerodynamics, Platoon Movement & Approach Angles
        hb_in,
        ivb_in,
        hb_glove_in,
        hb_away_in,
        plate_x_norm,
        attack_zone,
        vy_f,
        t_flight,
        vaa,
        haa,
        haa_glove,
        haa_away,

        -- 7. Plate Discipline, Contact Quality & Run Value Outcomes
        is_called_strike,
        is_swing,
        is_whiff,
        is_csw,
        is_ball,
        is_bip,
        is_in_zone,
        is_chase,
        xwoba,
        xba,
        xslg,
        woba_value,
        delta_run_exp,
        pitcher_run_exp,
        launch_speed,
        launch_angle,
        bb_type,
        hit_distance_sc,
        is_gb,
        is_hard_hit,
        bat_speed_mph,
        swing_length_ft,
        events,
        description
      FROM v_statcast_features
      WHERE EXTRACT(YEAR FROM game_date)::INTEGER = %d
      ORDER BY pitcher, game_date, game_pk, at_bat_number, pitch_number;
    ", as.integer(yr)))

    # Single-threaded sorted write preserves exact global ORDER BY across every 100k row group
    DBI::dbExecute(con, "SET threads TO 1; SET preserve_insertion_order = true;")
    DBI::dbExecute(con, sprintf("
      COPY (SELECT * FROM tmp_feat_yr ORDER BY pitcher, game_date, game_pk, at_bat_number, pitch_number)
      TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000);
    ", out_file))
    DBI::dbExecute(con, "DROP TABLE tmp_feat_yr;")
  }

  DBI::dbExecute(con, "SET threads TO 16;")
  t1 <- proc.time()
  elapsed <- round((t1 - t0)[3], 2)
  cat(sprintf("-> Staging materialization complete in %s seconds!\n", elapsed))

  cat("-> Verifying staged Silver dataset & ADR-001 sort order...\n")
  summary_sql <- sprintf("
    SELECT
      COUNT(*) AS total_rows,
      COUNT(DISTINCT game_year) AS seasons,
      MIN(game_date) AS min_date,
      MAX(game_date) AS max_date,
      ROUND(AVG(vaa), 2) AS mean_vaa,
      ROUND(100.0 * SUM(is_whiff)::DOUBLE / NULLIF(SUM(is_swing), 0), 2) AS whiff_on_swings_pct,
      ROUND(AVG(is_csw)::NUMERIC * 100, 2) AS csw_pct,
      ROUND(AVG(xwoba), 3) AS mean_xwoba
    FROM read_parquet('%s/*/*.parquet', hive_partitioning = true);
  ", stage_dir)
  summary_df <- DBI::dbGetQuery(con, summary_sql)
  print(as.data.frame(summary_df))

  # Verify ADR-001 sort monotonicity & disjoint row-group min-max stats across all seasons
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
      SELECT COUNT(*) AS n_inv
      FROM seq
      WHERE prev_pitcher IS NOT NULL AND pitcher IS NOT NULL AND pitcher < prev_pitcher;
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
        if (!is.na(rg$min_p[k]) && !is.na(rg$max_p[k - 1L]) && rg$min_p[k] < rg$max_p[k - 1L]) {
          rg_overlaps <- rg_overlaps + 1L
        }
      }
    }
  }
  cat(sprintf("-> ADR-001 sort check across %d seasons: pitcher inversions = %d | row-group min-max overlaps = %d\n",
              length(seasons), total_inversions, rg_overlaps))

  if (is.na(summary_df$total_rows) || summary_df$total_rows < 14000000) {
    stop("Staging verification failed: unexpected row count ", summary_df$total_rows)
  }
  if (is.na(summary_df$seasons) || summary_df$seasons < 19) {
    stop("Staging verification failed: expected >= 19 seasons, got ", summary_df$seasons)
  }
  if (total_inversions > 0L || rg_overlaps > 0L) {
    stop(sprintf("ADR-001 sort verification failed: inversions=%d, rg_overlaps=%d", total_inversions, rg_overlaps))
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  # Checked atomic swap
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

  cat("\n*** FULL-POPULATION SILVER FEATURE MATERIALIZATION COMPLETE (ADR-001 VERIFIED) ***\n")
}

run_materialize_features()
