-- =============================================================================
-- mlbr Canonical SQL View: v_statcast_features
-- =============================================================================
-- Purpose:
--   Derives full-population aerodynamic and trajectory metrics across all 14.64M pitches:
--   1. Terminal flight velocities at the front edge of home plate (y = 1.417 ft)
--   2. Vertical Approach Angle (VAA) & Horizontal Approach Angle (HAA)
--   3. Platoon-normalized horizontal coordinates (plate_x_norm)
--   4. Statcast Concentric Attack Zones (Heart, Shadow, Chase, Waste)
--   5. Swing and Whiff indicator flags
-- =============================================================================

CREATE OR REPLACE VIEW v_statcast_features AS
WITH flight AS (
  SELECT 
    *,
    -- Imperial movement units (inches)
    pfx_x * 12.0 AS hb_in,
    pfx_z * 12.0 AS ivb_in,
    
    -- Platoon-normalized plate coordinates:
    -- Positive is ALWAYS away / glove-side for both L and R batters
    ROUND(plate_x * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END), 2) AS plate_x_norm,
    
    -- Statcast 4-tier concentric attack zones
    CASE 
      WHEN abs(plate_x) <= 0.55 AND plate_z BETWEEN 1.83 AND 3.17 THEN 'Heart'
      WHEN abs(plate_x) <= 1.11 AND plate_z BETWEEN 1.25 AND 3.75 THEN 'Shadow'
      WHEN abs(plate_x) <= 1.66 AND plate_z BETWEEN 0.67 AND 4.33 THEN 'Chase'
      ELSE 'Waste'
    END AS attack_zone,
    
    -- Behavioral event flags
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff,
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play') THEN 1 ELSE 0 END AS is_swing,
    
    -- Forward terminal velocity at front edge of plate (y = 1.417 ft)
    -- Under constant longitudinal drag: v^2 = u^2 + 2 * a * (y_plate - y_50)
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
    -- Flight time from 50 ft release tracking point to 1.417 ft plate contact
    CASE WHEN vy_f IS NOT NULL AND ay IS NOT NULL AND ay != 0 THEN (vy_f - vy0) / ay ELSE NULL END AS t_flight
  FROM flight
)
SELECT 
  *,
  -- Approach Angles at the front edge of home plate (in degrees)
  ROUND(ATAN2(vz0 + az * t_flight, -vy_f) * (180.0 / PI()), 2) AS vaa,
  ROUND(ATAN2(vx0 + ax * t_flight, -vy_f) * (180.0 / PI()), 2) AS haa
FROM terminal;
