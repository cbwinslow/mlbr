# ==============================================================================
# mlbr Data Lake: Full-Population Feature Materializer (Silver Tier)
# ==============================================================================
# Framework: Medallion Architecture (Bronze -> Silver Materialization)
# Invariant: 100% Full-Population Scoring (Zero Subsampling)
#
# Destination: data/features/statcast_features/ partitioned by game_year
# ==============================================================================

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
})

cat("==============================================================================\n")
cat("          MLBR FULL-POPULATION FEATURE MATERIALIZER (SILVER TIER)             \n")
cat("==============================================================================\n\n")

lake <- mlbr::default_lake()
out_dir <- file.path(lake, "data", "features", "statcast_features")
stage_dir <- file.path(lake, "data", "features", "statcast_features_staging")
backup_dir <- file.path(lake, "data", "features", "statcast_features_prev")

if (dir.exists(stage_dir)) unlink(stage_dir, recursive = TRUE)
dir.create(stage_dir, recursive = TRUE)

cat("-> Connecting to DuckDB Data Lake (forcing dynamic CTE over Bronze v_statcast)...\n")
con <- mlbr::lake_duckdb(lake = lake)
mlbr::lake_register_features(con, lake = lake, force_dynamic = TRUE)

cat("-> Materializing expanded Silver features across all pitches into staging Parquet...\n")
t0 <- proc.time()

export_sql <- sprintf("
COPY (
  SELECT
    -- 1. Composite Primary / Foreign Keys (Links 1:1 back to Bronze raw data)
    game_pk,
    game_date,
    EXTRACT(YEAR FROM game_date)::INTEGER AS game_year,
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
  ORDER BY game_year, pitcher, game_date, game_pk, at_bat_number, pitch_number
) TO '%s' (FORMAT PARQUET, PARTITION_BY (game_year), COMPRESSION ZSTD, ROW_GROUP_SIZE 100000, OVERWRITE_OR_IGNORE);
", stage_dir)

DBI::dbExecute(con, export_sql)
t1 <- proc.time()
elapsed <- round((t1 - t0)[3], 2)
cat(sprintf("-> Staging materialization complete in %s seconds!\n", elapsed))

cat("-> Verifying staged Silver dataset...\n")
summary_sql <- sprintf("
  SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT game_year) AS seasons,
    MIN(game_date) AS min_date,
    MAX(game_date) AS max_date,
    ROUND(AVG(vaa), 2) AS mean_vaa,
    ROUND(AVG(is_whiff)::NUMERIC * 100, 2) AS whiff_pct,
    ROUND(AVG(is_csw)::NUMERIC * 100, 2) AS csw_pct,
    ROUND(AVG(xwoba), 3) AS mean_xwoba
  FROM read_parquet('%s/*/*.parquet');
", stage_dir)

summary_df <- DBI::dbGetQuery(con, summary_sql)
print(as.data.frame(summary_df))

if (is.na(summary_df$total_rows) || summary_df$total_rows < 14000000) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop("Staging verification failed: unexpected row count ", summary_df$total_rows)
}

DBI::dbDisconnect(con, shutdown = TRUE)

# Atomic swap
if (dir.exists(backup_dir)) unlink(backup_dir, recursive = TRUE)
if (dir.exists(out_dir)) file.rename(out_dir, backup_dir)
file.rename(stage_dir, out_dir)
if (dir.exists(backup_dir)) unlink(backup_dir, recursive = TRUE)

cat("\n*** FULL-POPULATION SILVER MATERIALIZATION COMPLETE ***\n")
