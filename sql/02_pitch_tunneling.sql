-- =============================================================================
-- mlbr Canonical SQL Query: Pitch Tunneling at Decision Point (y = 23.8 ft)
-- =============================================================================
-- Purpose:
--   Calculates pairwise pitch sequencing deception within each plate appearance:
--   1. Projectile position at the batter's decision commit point (y = 23.8 ft)
--   2. Tunnel Distance (d_tunnel) between previous pitch (N-1) and current pitch (N)
--   3. Velocity separation (delta_v) relative to the preceding pitch
-- =============================================================================

WITH decision_flight AS (
  SELECT 
    game_pk,
    at_bat_number,
    pitch_number,
    pitcher,
    batter,
    stand,
    p_throws,
    pitch_type,
    release_speed,
    plate_x,
    plate_z,
    attack_zone,
    is_swing,
    is_whiff,
    
    -- Forward velocity at decision commit point (y = 23.8 ft from plate)
    -SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) AS vy_dec,
    (-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay AS t_dec,
    
    -- 3D Spatial coordinates at commit point (y = 23.8 ft)
    release_pos_x + vx0 * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) 
      + 0.5 * ax * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) AS x_dec,
      
    release_pos_z + vz0 * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) 
      + 0.5 * az * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) * ((-SQRT(vy0*vy0 + 2.0 * ay * (23.8 - 50.0)) - vy0) / ay) AS z_dec
  FROM v_statcast_features
  WHERE vy0 IS NOT NULL AND ay IS NOT NULL
),
sequences AS (
  SELECT 
    *,
    -- Previous pitch attributes within the same plate appearance
    LAG(pitch_type) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_pitch_type,
    LAG(release_speed) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_release_speed,
    LAG(x_dec) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_x_dec,
    LAG(z_dec) OVER (PARTITION BY game_pk, at_bat_number ORDER BY pitch_number) AS prev_z_dec
  FROM decision_flight
)
SELECT 
  game_pk,
  at_bat_number,
  pitch_number,
  prev_pitch_type || ' -> ' || pitch_type AS pitch_sequence,
  ROUND(release_speed - prev_release_speed, 1) AS delta_velo,
  -- Tunnel distance in inches at y = 23.8 ft commit point
  ROUND(SQRT(POW(x_dec - prev_x_dec, 2) + POW(z_dec - prev_z_dec, 2)) * 12.0, 2) AS tunnel_dist_in,
  attack_zone,
  is_swing,
  is_whiff
FROM sequences
WHERE prev_pitch_type IS NOT NULL;
