# ==============================================================================
# mlbr Student Work-Along Lab 01: Aerodynamics, Trajectory Math & VAA
# ==============================================================================
# Companion to: workbooks/lab01_aerodynamics_workbook.qmd
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Execute line-by-line using Ctrl + Enter to observe how the data transforms.
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Trajectory Physics
# ------------------------------------------------------------------------------
# Let's take real tracking parameters from a modern 98.5 mph Four-Seam Fastball:
vy0 <- -144.2   # ft/s (forward speed at y = 50 ft)
vz0 <- -3.8     # ft/s (vertical downward velocity)
vx0 <- 4.2      # ft/s (horizontal velocity toward first-base side)
ay  <- 31.4     # ft/s^2 (forward drag slowing the ball down)
az  <- -14.2    # ft/s^2 (gravity -32.2 plus upward backspin lift +18.0)
ax  <- -8.1     # ft/s^2 (lateral break toward third base)

# Step 1.1: Calculate forward velocity at the front edge of home plate (y = 1.417 ft)
# Formula: v^2 = u^2 + 2 * a * delta_y
delta_y <- 1.417 - 50.0  # -48.583 feet
vy_f <- -sqrt(vy0^2 + 2 * ay * delta_y)

cat("--- Trajectory Step 1: Forward Velocity ---\n")
cat(sprintf("Velocity at y = 50 ft:   %.1f ft/s (%.1f mph)\n", abs(vy0), abs(vy0) * 3600 / 5280))
cat(sprintf("Velocity at home plate:  %.1f ft/s (%.1f mph)\n", abs(vy_f), abs(vy_f) * 3600 / 5280))
cat(sprintf("Velocity lost to drag:   %.1f mph\n\n", (abs(vy0) - abs(vy_f)) * 3600 / 5280))

# Step 1.2: Calculate total flight time
# Formula: t = (v - u) / a
t_flight <- (vy_f - vy0) / ay
cat("--- Trajectory Step 2: Time of Flight ---\n")
cat(sprintf("Flight time to plate:    %.4f seconds (%.1f milliseconds)\n\n", t_flight, t_flight * 1000))

# Step 1.3: Calculate vertical and horizontal terminal velocities
vz_f <- vz0 + az * t_flight
vx_f <- vx0 + ax * t_flight

# Step 1.4: Calculate Approach Angles in degrees
# VAA: Angle relative to the horizontal ground plane
vaa <- atan2(vz_f, -vy_f) * (180 / pi)
haa <- atan2(vx_f, -vy_f) * (180 / pi)

cat("--- Trajectory Step 3: Terminal Approach Angles ---\n")
cat(sprintf("Vertical Approach Angle (VAA):   %.2f degrees\n", vaa))
cat(sprintf("Horizontal Approach Angle (HAA): %.2f degrees\n\n", haa))


# ------------------------------------------------------------------------------
# Part 2: Querying the Full 14.64M Lake via DuckDB
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

cat("Connecting to zero-copy DuckDB lake...\n")
con <- mlbr::lake_duckdb()

# Step 2.1: Compare real MLB pitchers in 2024
cat("\nPulling top Four-Seam Fastball pitchers by VAA in 2024...\n")
query_pitchers <- "
  SELECT 
    CONCAT(c.name_last, ', ', c.name_first) AS pitcher_name,
    COUNT(*) AS pitches,
    ROUND(AVG(f.release_speed), 1) AS avg_mph,
    ROUND(AVG(f.release_pos_z), 2) AS avg_rel_height,
    ROUND(AVG(f.ivb_in), 1) AS avg_ivb,
    ROUND(AVG(f.vaa), 2) AS avg_vaa,
    ROUND(SUM(f.is_whiff)::NUMERIC / NULLIF(SUM(f.is_swing), 0) * 100, 1) AS whiff_rate_swings
  FROM v_statcast_features f
  JOIN v_chadwick_register c ON f.pitcher = c.key_mlbam
  WHERE f.game_date >= '2024-01-01'
    AND f.pitch_type = 'FF'
  GROUP BY c.name_last, c.name_first
  HAVING COUNT(*) >= 500
  ORDER BY avg_vaa DESC
  LIMIT 15;
"

top_pitchers <- DBI::dbGetQuery(con, query_pitchers)
print(as.data.frame(top_pitchers))


# ------------------------------------------------------------------------------
# Part 3: Visualizing the Whiff Lift (Empirical Bins + GAM Splines)
# ------------------------------------------------------------------------------
cat("\nQuerying 2024 Four-Seam swings to plot VAA vs Whiff Probability...\n")
query_swings <- "
  SELECT 
    release_speed,
    ivb_in,
    plate_z,
    vaa,
    is_whiff
  FROM v_statcast_features
  WHERE game_date >= '2024-01-01'
    AND pitch_type = 'FF'
    AND is_swing = 1
    AND plate_z BETWEEN 1.5 AND 3.5;
"

swings_2024 <- DBI::dbGetQuery(con, query_swings)
cat(sprintf("Loaded %s competitive swings in the strike zone.\n", format(nrow(swings_2024), big.mark=",")))

p1 <- ggplot(swings_2024 |> filter(vaa >= -9.0 & vaa <= -1.0), aes(x = vaa, y = is_whiff)) +
  stat_summary_bin(bins = 20, geom = "point", size = 3.5, color = "#00f2fe") +
  stat_summary_bin(bins = 20, geom = "line", color = "#00e676", linewidth = 1.1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "#38bdf8", fill = "#38bdf8", alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  mlbr::theme_cyber(base_size = 13) +
  labs(
    title = "Work-Along Lab 01: Fastball Whiff Rate vs. VAA (2024 Season)",
    subtitle = "Cyan points = Empirical bins | Green line = Trajectory | Blue ribbon = 95% GAM Spline",
    x = "Vertical Approach Angle (degrees, higher/flatter is better)",
    y = "Observed Whiff Rate on Swings"
  )

print(p1)
# Save assets according to our mandatory Asset Preservation Mandate
mlbr::save_plot_assets(p1, "lab01_whiff_curve_dark", data = swings_2024 |> head(1000), dir = "models")


# ------------------------------------------------------------------------------
# Part 4: The Movement Arena (Horizontal Break vs Induced Vertical Break)
# ------------------------------------------------------------------------------
cat("\nPulling pitch movement profiles across major pitch types (2024)...\n")
query_movement <- "
  SELECT 
    pitch_type,
    ROUND(hb_in, 1) AS hb_in,
    ROUND(ivb_in, 1) AS ivb_in,
    release_speed
  FROM v_statcast_features
  WHERE game_date >= '2024-01-01'
    AND pitch_type IN ('FF', 'SI', 'FC', 'SL', 'ST', 'CH', 'CU')
    AND hb_in IS NOT NULL 
    AND ivb_in IS NOT NULL
  USING SAMPLE 50000;
"

movement_2024 <- DBI::dbGetQuery(con, query_movement)

p2 <- ggplot(movement_2024, aes(x = hb_in, y = ivb_in, color = pitch_type)) +
  geom_vline(xintercept = 0, color = "#334155", linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "#334155", linetype = "dashed", linewidth = 0.6) +
  geom_point(alpha = 0.25, size = 1.2) +
  stat_density_2d(aes(color = pitch_type), linewidth = 0.8, bins = 6) +
  scale_color_manual(
    values = c(
      "FF" = "#38bdf8", # Cyan
      "SI" = "#3b82f6", # Blue
      "FC" = "#f59e0b", # Amber
      "SL" = "#10b981", # Emerald
      "ST" = "#84cc16", # Lime
      "CH" = "#a855f7", # Purple
      "CU" = "#f43f5e"  # Rose
    ),
    labels = c(
      "FF" = "Four-Seam (FF)",
      "SI" = "Sinker (SI)",
      "FC" = "Cutter (FC)",
      "SL" = "Slider (SL)",
      "ST" = "Sweeper (ST)",
      "CH" = "Changeup (CH)",
      "CU" = "Curveball (CU)"
    )
  ) +
  coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
  mlbr::theme_cyber(base_size = 13) +
  labs(
    title = "The Pitch Movement Arena: HB vs. IVB (2024 MLB)",
    subtitle = "Aerodynamic lift (backspin) elevates FF to +18 inches; Sweepers create extreme glove-side break",
    x = "Horizontal Break (inches, catcher perspective: positive = arm-side)",
    y = "Induced Vertical Break (inches, relative to gravity)",
    color = "Pitch Type"
  )

print(p2)
mlbr::save_plot_assets(p2, "lab01_movement_arena", data = movement_2024 |> head(2000), dir = "models")


# ------------------------------------------------------------------------------
# Part 5: Strike Zone Whiff Heatmap: Flat VAA vs Steep VAA
# ------------------------------------------------------------------------------
cat("\nEvaluating Fastball Whiff Rate across spatial strike zones...\n")
query_zone <- "
  SELECT 
    CASE WHEN vaa >= -4.5 THEN 'Flat VAA (>= -4.5°)' ELSE 'Steep VAA (< -4.5°)' END AS vaa_tier,
    ROUND(plate_x_norm, 1) AS plate_x_norm,
    ROUND(plate_z, 1) AS plate_z,
    COUNT(*) AS total_swings,
    SUM(is_whiff) AS total_whiffs,
    ROUND(SUM(is_whiff)::NUMERIC / COUNT(*) * 100, 1) AS whiff_pct
  FROM v_statcast_features
  WHERE game_date >= '2024-01-01'
    AND pitch_type = 'FF'
    AND is_swing = 1
    AND plate_x_norm BETWEEN -1.5 AND 1.5
    AND plate_z BETWEEN 1.2 AND 4.0
  GROUP BY 1, 2, 3
  HAVING COUNT(*) >= 30;
"

zone_whiff <- DBI::dbGetQuery(con, query_zone)

p3 <- ggplot(zone_whiff, aes(x = plate_x_norm, y = plate_z, fill = whiff_pct)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("#090d16", "#1e3a8a", "#0284c7", "#06b6d4", "#10b981", "#f59e0b", "#ef4444"),
    values = scales::rescale(c(5, 12, 18, 25, 32, 40, 50)),
    labels = function(x) paste0(x, "%"),
    name = "Whiff %"
  ) +
  # Strike zone bounding box
  geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
            color = "#f8fafc", fill = NA, linewidth = 0.9, linetype = "dashed") +
  facet_wrap(~ vaa_tier) +
  coord_equal(xlim = c(-1.3, 1.3), ylim = c(1.0, 4.2)) +
  mlbr::theme_cyber(base_size = 13) +
  labs(
    title = "Strike Zone Whiff Rates: Flat vs. Steep Four-Seam Fastballs",
    subtitle = "Flat VAA generates 35-45% whiffs at the upper edge (z > 3.0 ft); Steep VAA loses effectiveness up",
    x = "Platoon Normalized Horizontal Location (ft, positive = away)",
    y = "Vertical Plate Height (ft)"
  )

print(p3)
mlbr::save_plot_assets(p3, "lab01_zone_whiff_heatmap", data = zone_whiff, dir = "models")


# ------------------------------------------------------------------------------
# Part 6: Kinematic Flight Path Simulation (Paul Skenes vs Colin Rea)
# ------------------------------------------------------------------------------
cat("\nSimulating 3D flight trajectories from release (y=50 ft) to home plate...\n")

simulate_trajectory <- function(name, vy0_mph, vz0_fps, az_fps2, y0 = 50.0, z0 = 5.5, ay = 30.0) {
  vy0_fps <- -vy0_mph * 5280 / 3600
  delta_y <- 1.417 - y0
  vy_f <- -sqrt(vy0_fps^2 + 2 * ay * delta_y)
  t_total <- (vy_f - vy0_fps) / ay
  
  times <- seq(0, t_total, length.out = 100)
  data.frame(
    pitcher = name,
    time = times,
    y = y0 + vy0_fps * times + 0.5 * ay * times^2,
    z = z0 + vz0_fps * times + 0.5 * az_fps2 * times^2
  )
}

# Paul Skenes: 99.8 mph, low release (5.5 ft), high carry (az = -12.5 ft/s^2), VAA ~ -4.2 deg
skenes <- simulate_trajectory("Paul Skenes (Flat VAA: -4.2°)", vy0_mph = 99.8, vz0_fps = -3.2, az_fps2 = -12.5, z0 = 5.5)

# Colin Rea: 92.5 mph, high release (6.3 ft), steep drop (az = -18.5 ft/s^2), VAA ~ -6.5 deg
rea <- simulate_trajectory("Colin Rea (Steep VAA: -6.5°)", vy0_mph = 92.5, vz0_fps = -5.8, az_fps2 = -18.5, z0 = 6.3)

traj_df <- rbind(skenes, rea)

p4 <- ggplot(traj_df, aes(x = y, y = z, color = pitcher)) +
  geom_line(linewidth = 1.3) +
  # Strike zone cross-section at home plate (y = 1.417 ft)
  geom_segment(x = 1.417, xend = 1.417, y = 1.5, yend = 3.5, color = "#f8fafc", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = 3.0, y = 3.6, label = "Strike Zone Top (3.5 ft)", color = "#94a3b8", size = 3.5, hjust = 0) +
  annotate("text", x = 3.0, y = 1.4, label = "Strike Zone Bottom (1.5 ft)", color = "#94a3b8", size = 3.5, hjust = 0) +
  scale_x_reverse() + # Ball flies from y = 50 ft to y = 1.417 ft
  scale_color_manual(values = c("Paul Skenes (Flat VAA: -4.2°)" = "#00f2fe", "Colin Rea (Steep VAA: -6.5°)" = "#f43f5e")) +
  mlbr::theme_cyber(base_size = 13) +
  labs(
    title = "Kinematic Trajectory Comparison: Flat 'Rising' Fastball vs. Steep Fastball",
    subtitle = "Skenes' flat plane enters the zone at a shallow angle, creating the optical illusion of rising",
    x = "Distance from Home Plate (ft, 50 ft = release, 1.4 ft = front edge)",
    y = "Vertical Height above Ground (ft)",
    color = "Pitcher Trajectory"
  )

print(p4)
mlbr::save_plot_assets(p4, "lab01_trajectory_simulation", data = traj_df, dir = "models")

DBI::dbDisconnect(con, shutdown = TRUE)
cat("\nLab 01 complete! All 4 diagnostic figures and assets preserved in models/!\n")