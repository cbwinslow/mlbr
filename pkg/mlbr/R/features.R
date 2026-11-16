#' Register Lake-Wide Aerodynamic and Trajectory Features in DuckDB
#'
#' Derives physics-based aerodynamic features across all 14.64M Statcast pitches:
#' - vy_f: Plate-plane forward velocity at y = 1.417 ft
#' - t_flight: Time of flight from release (50 ft) to plate front (1.417 ft)
#' - vaa: Vertical Approach Angle (degrees relative to horizontal plane)
#' - haa: Horizontal Approach Angle (degrees)
#' - plate_x_norm: Platoon-normalized horizontal location (positive = glove-side/away)
#' - attack_zone: Statcast concentric zones (Heart, Shadow, Chase, Waste)
#' - is_swing: Binary indicator of swing attempt
#' - is_whiff: Binary indicator of swinging strike
#' - hb_in: Horizontal break in inches
#' - ivb_in: Induced vertical break in inches
#'
#' @param con A DBI connection to DuckDB
#' @return Invisibly returns the connection
#' @export
lake_register_features <- function(con) {
  if (!DBI::dbIsValid(con)) stop("Invalid DuckDB database connection.")
  
  # Ensure v_statcast exists before deriving features
  views <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.views WHERE table_name = 'v_statcast'")
  if (nrow(views) == 0) {
    warning("v_statcast view not found. Skipping feature registration.")
    return(invisible(con))
  }
  
  sql <- "
  CREATE OR REPLACE VIEW v_statcast_features AS
  WITH flight AS (
    SELECT 
      *,
      pfx_x * 12.0 AS hb_in,
      pfx_z * 12.0 AS ivb_in,
      ROUND(plate_x * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END), 2) AS plate_x_norm,
      CASE 
        WHEN abs(plate_x) <= 0.55 AND plate_z BETWEEN 1.83 AND 3.17 THEN 'Heart'
        WHEN abs(plate_x) <= 1.11 AND plate_z BETWEEN 1.25 AND 3.75 THEN 'Shadow'
        WHEN abs(plate_x) <= 1.66 AND plate_z BETWEEN 0.67 AND 4.33 THEN 'Chase'
        ELSE 'Waste'
      END AS attack_zone,
      CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff,
      CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play') THEN 1 ELSE 0 END AS is_swing,
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
      CASE WHEN vy_f IS NOT NULL AND ay IS NOT NULL AND ay != 0 THEN (vy_f - vy0) / ay ELSE NULL END AS t_flight
    FROM flight
  )
  SELECT 
    *,
    ROUND(ATAN2(vz0 + az * t_flight, -vy_f) * (180.0 / PI()), 2) AS vaa,
    ROUND(ATAN2(vx0 + ax * t_flight, -vy_f) * (180.0 / PI()), 2) AS haa
  FROM terminal;
  "
  
  DBI::dbExecute(con, sql)
  tryCatch(
    DBI::dbExecute(con, "CREATE OR REPLACE VIEW statcast_features AS SELECT * FROM v_statcast_features;"),
    error = function(e) NULL
  )
  invisible(con)
}
