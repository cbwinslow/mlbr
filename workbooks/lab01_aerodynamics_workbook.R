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
    ROUND(AVG(CASE WHEN f.is_whiff = 1 THEN 1.0 ELSE 0.0 END)::NUMERIC * 100, 1) AS whiff_rate_all_pitches
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
# Part 3: Visualizing the Whiff Lift in Your RStudio Plot Viewer
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
DBI::dbDisconnect(con, shutdown = TRUE)

cat(sprintf("Loaded %s competitive swings in the strike zone.\n", format(nrow(swings_2024), big.mark=",")))

# Create an intuitive plot showing how flatter fastballs increase whiffs
# We use Generalized Additive Models (GAM) and empirical bins for sub-second, memory-efficient rendering
p <- ggplot(swings_2024 |> filter(vaa >= -9.0 & vaa <= -1.0), aes(x = vaa, y = is_whiff)) +
  stat_summary_bin(bins = 20, geom = "point", size = 3.5, color = "#00f2fe") +
  stat_summary_bin(bins = 20, geom = "line", color = "#00e676", linewidth = 1.1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "#38bdf8", fill = "#38bdf8", alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#090d16", color = NA),
    panel.background = element_rect(fill = "#111827", color = "#1e293b", linewidth = 0.6),
    panel.grid.major = element_line(color = "#1e293b", linewidth = 0.4),
    panel.grid.minor = element_line(color = "#162032", linewidth = 0.2),
    text             = element_text(color = "#e2e8f0"),
    plot.title       = element_text(color = "#f1f5f9", face = "bold", size = rel(1.1)),
    plot.subtitle    = element_text(color = "#94a3b8", size = rel(0.9)),
    axis.title       = element_text(color = "#e2e8f0", face = "bold"),
    axis.text        = element_text(color = "#94a3b8")
  ) +
  labs(
    title = "Work-Along Lab 01: Fastball Whiff Rate vs. VAA (2024 Season)",
    subtitle = "Cyan points = Empirical bins | Green line = Trajectory | Blue ribbon = 95% GAM Confidence Band",
    x = "Vertical Approach Angle (degrees, higher/flatter is better)",
    y = "Observed Whiff Rate on Swings"
  )

print(p)
cat("Plot rendered in Dark Cyber Theme in RStudio Plot Viewer!\n")
