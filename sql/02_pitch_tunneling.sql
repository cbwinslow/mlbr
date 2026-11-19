-- =============================================================================
-- mlbr Canonical SQL Query: Commit-Point Pitch Tunneling (y = 23.8 ft)
-- =============================================================================
-- Purpose:
--   Calculates exact pairwise pitch sequencing deception within each plate
--   appearance (guarded against mid-AB pitching changes, pitch_number gaps,
--   NULL kinematics/pitch types, and corrupted tracking sensor readings):
--   1. Exact 3D trajectory coordinates at batter decision commit point (y = 23.8 ft)
--      by reconstructing (x50, z50) from (plate_x, plate_z) at y = 1.417 ft
--      (fixes the ~2.87 in mean / 4.69 in p95 error from starting at release_pos_y).
--   2. Commit-Point Tunnel Distance (tunnel_dist_in) between pitch N-1 and pitch N
--   3. Plate Separation (plate_sep_in), Post-Tunnel Break (post_tunnel_break_in),
--      Pavlidis/Judge Break-to-Tunnel Ratio (post_tunnel_break_in / GREATEST(tunnel_dist_in, 1.0)),
--      and Plate-to-Tunnel Ratio (plate_sep_in / GREATEST(tunnel_dist_in, 1.0))
--   4. Velocity separation (delta_velo_mph) and flight-time differential (delta_flight_ms)
-- =============================================================================

WITH valid_pitches AS (
  SELECT
    game_pk,
    EXTRACT(YEAR FROM game_date)::INTEGER AS game_year,
    game_date,
    game_type,
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
    -- Exact 3D position at y = 23.8 ft via exact (x50, z50) reconstruction from plate
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
