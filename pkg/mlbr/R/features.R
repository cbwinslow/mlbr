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

  views_now <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.views WHERE table_name = 'v_statcast_features'")
  if (nrow(views_now) > 0) {
    DBI::dbExecute(con, "CREATE OR REPLACE VIEW statcast_features AS SELECT * FROM v_statcast_features;")
  }
  invisible(con)
}

#' Register Lake-Wide Commit-Point Pitch Tunneling Features in DuckDB
#'
#' Mounts the pre-materialized Silver-tier tunneling table
#' (`data/features/statcast_tunneling`) if present, exposing exact
#' `y0 = 50 ft -> y_commit = 23.8 ft` commit-point coordinates, tunnel distance,
#' plate separation, post-tunnel break, Pavlidis/Judge `break_to_tunnel_ratio`
#' (`post_tunnel_break_in / GREATEST(tunnel_dist_in, 1.0)`), and
#' `plate_to_tunnel_ratio` (`plate_sep_in / GREATEST(tunnel_dist_in, 1.0)`).
#' If unmaterialized or if `force_dynamic = TRUE`, evaluates the canonical SQL
#' over `v_statcast_features` dynamically.
#'
#' @param con A DBI connection to DuckDB
#' @param lake Project root directory (default: mlbr default_lake)
#' @param force_dynamic Logical; if TRUE, bypass materialized Parquet and evaluate live SQL over `v_statcast_features`
#' @return Invisibly returns the connection
#' @examples
#' \dontrun{
#' con <- mlbr::lake_duckdb()
#' mlbr::lake_register_tunneling(con)
#' }
#' @export
lake_register_tunneling <- function(con, lake = NULL, force_dynamic = FALSE) {
  if (!DBI::dbIsValid(con)) stop("Invalid DuckDB database connection.")
  lake <- default_lake(lake)

  tun_dir <- file.path(lake, "data", "features", "statcast_tunneling")
  tun_files <- Sys.glob(file.path(tun_dir, "*", "*.parquet"))

  if (!force_dynamic && length(tun_files) > 0) {
    sql <- sprintf(
      "CREATE OR REPLACE VIEW v_statcast_tunneling AS SELECT * FROM read_parquet('%s/*/*.parquet', hive_partitioning = true);",
      tun_dir
    )
    DBI::dbExecute(con, sql)
  } else {
    views <- DBI::dbGetQuery(
      con,
      "SELECT table_name FROM information_schema.views WHERE table_name = 'v_statcast_features'"
    )
    if (nrow(views) == 0) {
      stop("Cannot register dynamic v_statcast_tunneling: required view 'v_statcast_features' is not mounted.")
    }

    feat_cols <- DBI::dbGetQuery(con, "DESCRIBE v_statcast_features")$column_name
    game_type_expr <- if ("game_type" %in% feat_cols) "game_type" else "CAST(NULL AS VARCHAR) AS game_type"

    sql <- sprintf("
    CREATE OR REPLACE VIEW v_statcast_tunneling AS
    WITH valid_pitches AS (
      SELECT
        game_pk,
        EXTRACT(YEAR FROM game_date)::INTEGER AS game_year,
        game_date,
        %s,
        at_bat_number,
        pitch_number,
        pitcher,
        batter,
        stand,
        p_throws,
        matchup,
        is_same_hand,
        pitch_type,
        release_speed,
        plate_x,
        plate_z,
        attack_zone,
        is_swing,
        is_whiff,
        is_csw,
        is_chase,
        pitcher_run_exp,
        vx0, vy0, vz0,
        ax, ay, az,
        t_flight AS t_plate,
        (-SQRT(vy0 * vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay AS t_dec
      FROM v_statcast_features
      WHERE game_pk IS NOT NULL
        AND game_date IS NOT NULL
        AND at_bat_number IS NOT NULL
        AND pitch_number IS NOT NULL
        AND pitcher IS NOT NULL
        AND pitch_type IS NOT NULL
        AND release_speed IS NOT NULL
        AND vx0 IS NOT NULL
        AND vy0 IS NOT NULL
        AND vy0 < -40.0
        AND vz0 IS NOT NULL
        AND ax IS NOT NULL
        AND ay IS NOT NULL
        AND ay > 1e-6
        AND az IS NOT NULL
        AND t_flight IS NOT NULL
        AND t_flight BETWEEN 0.15 AND 1.2
        AND plate_x IS NOT NULL
        AND plate_x BETWEEN -10.0 AND 10.0
        AND plate_z IS NOT NULL
        AND plate_z BETWEEN -10.0 AND 15.0
        AND (vy0 * vy0 + 2.0 * ay * (23.8 - 50.0)) > 0
    ),
    commit_coords AS (
      SELECT
        *,
        (plate_x - vx0 * t_plate - 0.5 * ax * t_plate * t_plate)
          + vx0 * t_dec + 0.5 * ax * t_dec * t_dec AS x_dec,
        (plate_z - vz0 * t_plate - 0.5 * az * t_plate * t_plate)
          + vz0 * t_dec + 0.5 * az * t_dec * t_dec AS z_dec
      FROM valid_pitches
    ),
    sequences AS (
      SELECT
        *,
        LAG(pitch_number) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_pitch_number,
        LAG(pitcher) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_pitcher,
        LAG(pitch_type) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_pitch_type,
        LAG(release_speed) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_release_speed,
        LAG(t_plate) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_t_plate,
        LAG(x_dec) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_x_dec,
        LAG(z_dec) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_z_dec,
        LAG(plate_x) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_plate_x,
        LAG(plate_z) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_plate_z
      FROM commit_coords
    ),
    metrics AS (
      SELECT
        game_pk,
        game_year,
        game_date,
        game_type,
        at_bat_number,
        pitch_number,
        pitcher,
        batter,
        matchup,
        is_same_hand,
        prev_pitch_type,
        pitch_type,
        prev_pitch_type || ' -> ' || pitch_type AS pitch_sequence,
        CASE WHEN prev_pitch_type = pitch_type THEN 1 ELSE 0 END AS is_repeat_pitch,
        ROUND(x_dec, 4) AS x_dec,
        ROUND(z_dec, 4) AS z_dec,
        ROUND(release_speed - prev_release_speed, 2) AS delta_velo_mph,
        ROUND((t_plate - prev_t_plate) * 1000.0, 1) AS delta_flight_ms,
        ROUND(SQRT(POW(x_dec - prev_x_dec, 2) + POW(z_dec - prev_z_dec, 2)) * 12.0, 2) AS tunnel_dist_in,
        ROUND(SQRT(POW(plate_x - prev_plate_x, 2) + POW(plate_z - prev_plate_z, 2)) * 12.0, 2) AS plate_sep_in,
        ROUND(
          SQRT(POW((plate_x - x_dec) - (prev_plate_x - prev_x_dec), 2)
             + POW((plate_z - z_dec) - (prev_plate_z - prev_z_dec), 2)) * 12.0,
          2
        ) AS post_tunnel_break_in,
        attack_zone,
        is_swing,
        is_whiff,
        is_csw,
        is_chase,
        pitcher_run_exp
      FROM sequences
      WHERE prev_pitch_type IS NOT NULL
        AND pitch_type IS NOT NULL
        AND prev_pitcher = pitcher
        AND prev_pitch_number = pitch_number - 1
    )
    SELECT
      *,
      ROUND(post_tunnel_break_in / GREATEST(tunnel_dist_in, 1.0), 3) AS break_to_tunnel_ratio,
      ROUND(plate_sep_in / GREATEST(tunnel_dist_in, 1.0), 3) AS plate_to_tunnel_ratio
    FROM metrics;
    ", game_type_expr)
    DBI::dbExecute(con, sql)
  }

  DBI::dbExecute(con, "CREATE OR REPLACE VIEW statcast_tunneling AS SELECT * FROM v_statcast_tunneling;")
  invisible(con)
}

#' Register Pitch-Level Silver Biomechanics & Catcher Framing View
#'
#' Mounts the materialized Silver Parquet lake (`data/features/statcast_biomechanics`)
#' as `v_statcast_biomechanics` (and alias `statcast_biomechanics`) in an active
#' DuckDB connection, exposing `ivb_above_slot`, Alan Nathan `max_ev_mph`/`sq_ratio`/`is_blast`,
#' and zero-sum pitch-level catcher framing `csaa_pitch`.
#'
#' @param con Active DBI/DuckDB connection from [lake_duckdb()].
#' @param lake Optional path to lake root. Defaults to [default_lake()].
#' @return Invisibly returns `TRUE` if mounted, `FALSE` if Silver directory is absent.
#' @export
#' @examples
#' \dontrun{
#' con <- lake_duckdb()
#' lake_register_biomechanics(con)
#' }
lake_register_biomechanics <- function(con, lake = NULL) {
  if (is.null(lake)) lake <- default_lake()
  bio_dir <- file.path(lake, "data", "features", "statcast_biomechanics")
  if (!dir.exists(bio_dir)) return(invisible(FALSE))
  parq_files <- Sys.glob(file.path(bio_dir, "*", "*.parquet"))
  if (length(parq_files) == 0L) return(invisible(FALSE))
  sql <- sprintf(
    "CREATE OR REPLACE VIEW v_statcast_biomechanics AS SELECT * FROM read_parquet('%s/*/*.parquet', hive_partitioning = true);",
    bio_dir
  )
  DBI::dbExecute(con, sql)
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW statcast_biomechanics AS SELECT * FROM v_statcast_biomechanics;")
  invisible(TRUE)
}
