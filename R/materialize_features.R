# ==============================================================================
# mlbr Data Lake: Full-Population Feature Materializer (Silver Tier)
# ==============================================================================
# Framework: Medallion Architecture (Bronze -> Silver Materialization)
# Invariant: 100% Full-Population Scoring (Zero Subsampling)
#
# Materializes all derived aerodynamic & trajectory metrics across all 14.64M pitches:
#   - vy_f (terminal plate forward speed at y = 1.417 ft)
#   - t_flight (flight duration in seconds)
#   - vaa (Vertical Approach Angle in degrees)
#   - haa (Horizontal Approach Angle in degrees)
#   - plate_x_norm (Platoon-normalized location; positive = away/glove-side)
#   - attack_zone (Heart, Shadow, Chase, Waste)
#   - is_swing, is_whiff
#   - hb_in, ivb_in
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
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("-> Connecting to DuckDB Data Lake...\n")
con <- mlbr::lake_duckdb(lake = lake)

cat("-> Materializing features across all 14.64M pitches into partitioned Parquet...\n")
t0 <- proc.time()

# We select the composite foreign keys plus all raw release/movement attributes and derived features
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
    
    -- 2. Batter / Pitcher Matchup Context
    stand,
    p_throws,
    balls,
    strikes,
    outs_when_up,
    inning,
    inning_topbot,
    
    -- 3. Release Dynamics (Imperial & Physical)
    release_speed,
    release_spin_rate,
    release_pos_x,
    release_pos_y,
    release_pos_z,
    release_extension,
    
    -- 4. Raw Trajectory Tracking Parameters (50 ft to plate)
    vx0, vy0, vz0, ax, ay, az,
    
    -- 5. Plate Crossing Coordinates
    plate_x,
    plate_z,
    sz_top,
    sz_bot,
    
    -- 6. Silver Derived Aerodynamics & Approach Angles
    hb_in,
    ivb_in,
    plate_x_norm,
    attack_zone,
    vy_f,
    t_flight,
    vaa,
    haa,
    
    -- 7. Behavioral Flags & Outcomes
    is_swing,
    is_whiff,
    events,
    description
  FROM v_statcast_features
) TO '%s' (FORMAT PARQUET, PARTITION_BY (game_year), OVERWRITE_OR_IGNORE);
", out_dir)

DBI::dbExecute(con, export_sql)
t1 <- proc.time()
elapsed <- round((t1 - t0)[3], 2)
cat(sprintf("-> Feature materialization complete in %s seconds!\n", elapsed))

# Verify the materialized dataset
cat("-> Verifying materialized Silver dataset...\n")
summary_sql <- sprintf("
  SELECT 
    COUNT(*) as total_rows,
    COUNT(DISTINCT game_year) as seasons,
    MIN(game_date) as min_date,
    MAX(game_date) as max_date,
    ROUND(AVG(vaa), 2) as mean_vaa,
    ROUND(AVG(is_whiff)::NUMERIC, 4) as whiff_rate
  FROM read_parquet('%s/*/*.parquet');
", out_dir)

summary_df <- DBI::dbGetQuery(con, summary_sql)
print(as.data.frame(summary_df))

DBI::dbDisconnect(con, shutdown = TRUE)

cat("\n*** FULL-POPULATION MATERIALIZATION COMPLETE ***\n")
