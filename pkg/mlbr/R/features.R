#' Register Lake-Wide Aerodynamic and Trajectory Features in DuckDB
#'
#' Mounts the pre-materialized Silver-tier feature table (data/features/statcast_features)
#' if present, providing sub-50ms queries over 14.64M rows. If unmaterialized or if
#' \code{force_dynamic = TRUE}, builds the canonical dynamic CTE derivation over Bronze \code{v_statcast}.
#'
#' @param con A DBI connection to DuckDB
#' @param lake Project root directory (default: mlbr default_lake)
#' @param force_dynamic Logical; if TRUE, bypass materialized Parquet and evaluate live SQL over \code{v_statcast}
#' @return Invisibly returns the connection
#' @export
lake_register_features <- function(con, lake = NULL, force_dynamic = FALSE) {
  if (!DBI::dbIsValid(con)) stop("Invalid DuckDB database connection.")
  lake <- default_lake(lake)

  feat_dir <- file.path(lake, "data", "features", "statcast_features")
  feat_files <- Sys.glob(file.path(feat_dir, "*", "*.parquet"))

  if (!force_dynamic && length(feat_files) > 0) {
    # Mount materialized Silver-tier parquet dataset
    sql <- sprintf(
      "CREATE OR REPLACE VIEW v_statcast_features AS SELECT * FROM read_parquet('%s/*/*.parquet', hive_partitioning = true);",
      feat_dir
    )
    DBI::dbExecute(con, sql)
  } else {
    # Dynamic CTE derivation over Bronze v_statcast
    views <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.views WHERE table_name = 'v_statcast'")
    if (nrow(views) == 0) return(invisible(con))

    sql <- "
    CREATE OR REPLACE VIEW v_statcast_features AS
    WITH flight AS (
      SELECT
        *,
        -- Tracking era segmentation across Statcast history
        CASE
          WHEN EXTRACT(YEAR FROM game_date) <= 2014 THEN 'PITCHf/x'
          WHEN EXTRACT(YEAR FROM game_date) <= 2019 THEN 'TrackMan'
          ELSE 'Hawk-Eye'
        END AS tracking_era,

        -- 4-Quadrant Platoon Matchup Context
        CASE
          WHEN p_throws IN ('R', 'L') AND stand IN ('R', 'L') THEN p_throws || 'v' || stand
          ELSE NULL
        END AS matchup,
        CASE
          WHEN p_throws IN ('R', 'L') AND stand IN ('R', 'L') THEN CASE WHEN p_throws = stand THEN 1 ELSE 0 END
          ELSE NULL
        END AS is_same_hand,

        -- Pitcher arm-side positive release offset (ft)
        release_pos_x * (CASE WHEN p_throws = 'L' THEN 1.0 ELSE -1.0 END) AS release_pos_x_arm,

        -- Imperial aerodynamic movement (inches)
        pfx_x * 12.0 AS hb_in,
        pfx_z * 12.0 AS ivb_in,

        -- Pitcher-perspective horizontal break: positive = glove-side, negative = arm-side
        (pfx_x * 12.0) * (CASE WHEN p_throws = 'L' THEN -1.0 ELSE 1.0 END) AS hb_glove_in,

        -- Batter-perspective horizontal break: positive = away from batter, negative = into batter
        (pfx_x * 12.0) * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END) AS hb_away_in,

        -- Platoon-normalized plate horizontal location: positive = away from batter
        ROUND(plate_x * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END), 3) AS plate_x_norm,

        -- Statcast 4-tier concentric attack zones
        CASE
          WHEN abs(plate_x) <= 0.55 AND plate_z BETWEEN 1.83 AND 3.17 THEN 'Heart'
          WHEN abs(plate_x) <= 1.11 AND plate_z BETWEEN 1.25 AND 3.75 THEN 'Shadow'
          WHEN abs(plate_x) <= 1.66 AND plate_z BETWEEN 0.67 AND 4.33 THEN 'Chase'
          ELSE 'Waste'
        END AS attack_zone,

        -- Plate discipline & outcome flags
        CASE WHEN description = 'called_strike' THEN 1 ELSE 0 END AS is_called_strike,
        CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff,
        CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play') THEN 1 ELSE 0 END AS is_swing,
        CASE WHEN description IN ('called_strike', 'swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_csw,
        CASE WHEN description IN ('ball', 'blocked_ball', 'intent_ball', 'pitchout', 'automatic_ball') THEN 1 ELSE 0 END AS is_ball,
        CASE WHEN description = 'hit_into_play' THEN 1 ELSE 0 END AS is_bip,
        CASE
          WHEN zone BETWEEN 1 AND 9 THEN 1
          WHEN plate_x IS NOT NULL AND plate_z IS NOT NULL AND sz_top IS NOT NULL AND sz_bot IS NOT NULL
               AND ABS(plate_x) <= 0.8308 AND plate_z BETWEEN sz_bot AND sz_top THEN 1
          ELSE 0
        END AS is_in_zone,

        -- Safely cast VARCHAR Statcast metrics
        TRY_CAST(NULLIF(estimated_woba_using_speedangle, 'null') AS DOUBLE) AS xwoba,
        TRY_CAST(NULLIF(estimated_ba_using_speedangle, 'null') AS DOUBLE) AS xba,
        TRY_CAST(NULLIF(estimated_slg_using_speedangle, 'null') AS DOUBLE) AS xslg,
        TRY_CAST(NULLIF(arm_angle, 'null') AS DOUBLE) AS arm_angle_deg,
        TRY_CAST(NULLIF(bat_speed, 'null') AS DOUBLE) AS bat_speed_mph,
        TRY_CAST(NULLIF(swing_length, 'null') AS DOUBLE) AS swing_length_ft,

        -- Pitcher-perspective run value (positive = good for pitcher)
        COALESCE(delta_pitcher_run_exp, -delta_run_exp) AS pitcher_run_exp,

        -- Batted ball quality flags
        CASE WHEN bb_type = 'ground_ball' THEN 1 ELSE 0 END AS is_gb,
        CASE WHEN launch_speed >= 95.0 THEN 1 ELSE 0 END AS is_hard_hit,

        -- Forward terminal velocity at front edge of plate (y = 1.417 ft)
        CASE
          WHEN vy0 IS NOT NULL AND ay IS NOT NULL AND (vy0*vy0 + 2.0 * ay * (1.417 - 50.0)) > 0
          THEN -SQRT(vy0*vy0 + 2.0 * ay * (1.417 - 50.0))
          ELSE NULL
        END AS vy_f
      FROM v_statcast
    ),
    terminal AS (
      SELECT
        *,
        CASE WHEN is_swing = 1 AND is_in_zone = 0 THEN 1 ELSE 0 END AS is_chase,
        CASE WHEN vy_f IS NOT NULL AND ay IS NOT NULL AND ay != 0 THEN (vy_f - vy0) / ay ELSE NULL END AS t_flight
      FROM flight
    ),
    angles AS (
      SELECT
        *,
        ROUND(ATAN2(vz0 + az * t_flight, -vy_f) * (180.0 / PI()), 2) AS vaa,
        ROUND(ATAN2(vx0 + ax * t_flight, -vy_f) * (180.0 / PI()), 2) AS haa
      FROM terminal
    )
    SELECT
      *,
      ROUND(haa * (CASE WHEN p_throws = 'L' THEN -1.0 ELSE 1.0 END), 2) AS haa_glove,
      ROUND(haa * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END), 2) AS haa_away
    FROM angles;
    "
    DBI::dbExecute(con, sql)
  }

  tryCatch(
    DBI::dbExecute(con, "CREATE OR REPLACE VIEW statcast_features AS SELECT * FROM v_statcast_features;"),
    error = function(e) NULL
  )
  invisible(con)
}
