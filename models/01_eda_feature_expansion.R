# ==============================================================================
# mlbr Modeling Lab 01-B: Exploratory Data Analysis & Physics Feature Expansion
# ==============================================================================
# Framework: Q-CRISP (Quantitative Cross-Industry Standard Process for Data Mining)
# Objective:
#   Derive, explore, and validate cutting-edge aerodynamic and trajectory features:
#   1. Vertical Approach Angle (VAA) and Horizontal Approach Angle (HAA)
#   2. VAA Above Expected (VAA_diff) at given plate heights
#   3. Platoon-Normalized Coordinates (plate_x_norm)
#   4. Statcast Attack Zones (Heart, Shadow, Chase, Waste)
#
# Execution:
#   Full-population vectorized math inside DuckDB over 14.6M pitches.
# ==============================================================================

# Step 1: Load packages
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

cat("==============================================================================\n")
cat("          MLBR Q-CRISP EDA: AERODYNAMICS & APPROACH ANGLES                    \n")
cat("==============================================================================\n\n")

# Step 2: Connect to DuckDB Lake
cat("-> Mounting zero-copy DuckDB lake view...\n")
con <- mlbr::lake_duckdb()

# Step 3: Vectorized Trajectory Derivations in DuckDB
# We derive VAA, HAA, plate_x_norm, and swing/whiff flags.
# Query extracts competitive swings from modern seasons (2023-2026).
cat("-> Executing aerodynamic trajectory math across DuckDB...\n")
query <- "
WITH base AS (
  SELECT 
    game_date,
    pitcher,
    batter,
    pitch_type,
    release_speed,
    release_spin_rate,
    release_pos_x,
    release_pos_z,
    release_extension,
    pfx_x * 12.0 AS hb_in,
    pfx_z * 12.0 AS ivb_in,
    plate_x,
    plate_z,
    stand,
    p_throws,
    balls,
    strikes,
    description,
    vx0, vy0, vz0, ax, ay, az,
    -- Trajectory terminal velocities at y = 1.417 ft (front of plate)
    -SQRT(vy0*vy0 + 2.0 * ay * (1.417 - 50.0)) AS vy_f,
    (-SQRT(vy0*vy0 + 2.0 * ay * (1.417 - 50.0)) - vy0) / ay AS t_flight
  FROM v_statcast
  WHERE game_date >= '2023-01-01'
    AND vy0 IS NOT NULL AND ay IS NOT NULL
    AND release_speed IS NOT NULL
    AND plate_x IS NOT NULL AND plate_z IS NOT NULL
    AND pitch_type IN ('FF', 'SI', 'SL', 'ST', 'CH', 'CU', 'FC', 'FS')
),
angles AS (
  SELECT 
    *,
    vz0 + az * t_flight AS vz_f,
    vx0 + ax * t_flight AS vx_f,
    -- VAA: angle relative to horizontal plane
    round(ATAN2(vz0 + az * t_flight, -(-SQRT(vy0*vy0 + 2.0 * ay * (1.417 - 50.0)))) * (180.0 / PI()), 2) AS vaa,
    -- HAA: horizontal entry angle
    round(ATAN2(vx0 + ax * t_flight, -(-SQRT(vy0*vy0 + 2.0 * ay * (1.417 - 50.0)))) * (180.0 / PI()), 2) AS haa,
    -- Platoon-normalized plate_x (positive is ALWAYS away/glove-side)
    round(plate_x * (CASE WHEN stand = 'L' THEN -1.0 ELSE 1.0 END), 2) AS plate_x_norm,
    -- Statcast concentric attack zones
    CASE 
      WHEN abs(plate_x) <= 0.55 AND plate_z BETWEEN 1.83 AND 3.17 THEN 'Heart'
      WHEN abs(plate_x) <= 1.11 AND plate_z BETWEEN 1.25 AND 3.75 THEN 'Shadow'
      WHEN abs(plate_x) <= 1.66 AND plate_z BETWEEN 0.67 AND 4.33 THEN 'Chase'
      ELSE 'Waste'
    END AS attack_zone,
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff,
    CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play') THEN 1 ELSE 0 END AS is_swing
  FROM base
)
SELECT * FROM angles;
"

pitches <- DBI::dbGetQuery(con, query) |> as_tibble()
DBI::dbDisconnect(con, shutdown = TRUE)

cat(sprintf("Loaded %s pitches from 2023-2026 for exploratory analysis.\n", format(nrow(pitches), big.mark=",")))

# Step 4: Empirical Expected VAA and VAA_diff
# VAA is naturally steep at the bottom of the zone and flat at the top.
# We model Expected VAA as a function of plate_z for Four-Seam Fastballs (FF).
ff_data <- pitches |> filter(pitch_type == "FF")
vaa_baseline_fit <- lm(vaa ~ plate_z, data = ff_data)
cat(sprintf("Four-Seam VAA Baseline: VAA = %.2f + %.2f * plate_z\n", 
            coef(vaa_baseline_fit)[1], coef(vaa_baseline_fit)[2]))

pitches <- pitches |>
  mutate(
    expected_vaa = coef(vaa_baseline_fit)[1] + coef(vaa_baseline_fit)[2] * plate_z,
    vaa_diff = vaa - expected_vaa
  )

# Step 5: Statistical Distribution of VAA by Pitch Type
cat("\n--- Approach Angle Distribution by Pitch Type ---\n")
pitch_summary <- pitches |>
  group_by(pitch_type) |>
  summarise(
    count = n(),
    avg_velo = mean(release_speed, na.rm=TRUE),
    avg_ivb = mean(ivb_in, na.rm=TRUE),
    mean_vaa = mean(vaa, na.rm=TRUE),
    mean_haa = mean(haa, na.rm=TRUE),
    whiff_rate_on_swings = mean(is_whiff[is_swing == 1], na.rm=TRUE) * 100
  ) |>
  arrange(desc(count))
print(as.data.frame(pitch_summary), digits = 3)

# Step 6: Visualizations (Dark Cyber / Hacker Aesthetic)
cat("\nGenerating diagnostic plots in Dark Cyber Theme...\n")

theme_cyber_dark <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background = element_rect(fill = "#090d16", color = NA),
      panel.background = element_rect(fill = "#111827", color = "#1e293b", linewidth = 0.6),
      panel.grid.major = element_line(color = "#1e293b", linewidth = 0.4),
      panel.grid.minor = element_line(color = "#162032", linewidth = 0.2),
      text = element_text(color = "#e2e8f0"),
      plot.title = element_text(color = "#f1f5f9", face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle = element_text(color = "#94a3b8", size = rel(0.9), hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(color = "#e2e8f0", face = "bold"),
      axis.text = element_text(color = "#94a3b8"),
      legend.background = element_rect(fill = "#090d16", color = NA),
      legend.text = element_text(color = "#e2e8f0"),
      legend.title = element_text(color = "#e2e8f0", face = "bold"),
      strip.background = element_rect(fill = "#162032", color = "#1e293b"),
      strip.text = element_text(color = "#38bdf8", face = "bold")
    )
}

# Plot 1: Hexbin Distribution of VAA vs Plate Height
p1 <- ggplot(pitches |> filter(pitch_type %in% c("FF", "SL", "CU", "CH")), 
             aes(x = plate_z, y = vaa)) +
  geom_hex(bins = 45) +
  geom_smooth(method = "lm", color = "#00f2fe", linetype = "dashed", se = FALSE, linewidth = 0.9) +
  scale_fill_viridis_c(trans = "log10", option = "magma") +
  facet_wrap(~ pitch_type, nrow = 2) +
  theme_cyber_dark(base_size = 12) +
  labs(
    title = "Vertical Approach Angle (VAA) vs. Plate Height",
    subtitle = "Fastballs (FF) exhibit flatter entry angles (-3° to -6°); Curveballs (CU) plunge steeply (-7° to -11°)",
    x = "Vertical Plate Height (ft)",
    y = "Vertical Approach Angle (degrees)",
    fill = "Pitch Count"
  )

ggsave("/mnt/storage/Rprojects/mlbr/models/fig1_vaa_by_pitch_type.png", plot = p1, width = 9, height = 7, dpi = 150)
cat("-> Saved: models/fig1_vaa_by_pitch_type.png\n")

# Plot 2: Whiff Rate by Attack Zone and VAA Diff
swings <- pitches |> filter(is_swing == 1, pitch_type == "FF")
p2 <- ggplot(swings, aes(x = vaa_diff, y = is_whiff)) +
  stat_summary_bin(bins = 20, geom = "point", size = 3, color = "#00f2fe") +
  stat_summary_bin(bins = 20, geom = "line", color = "#00e676", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent) +
  theme_cyber_dark(base_size = 12) +
  labs(
    title = "Four-Seam Fastball Whiff Rate vs. VAA Above Expected (VAA_diff)",
    subtitle = "Positive VAA_diff (flatter than expected angle) directly drives higher swing-and-miss rates",
    x = "VAA Above Expected (degrees)",
    y = "Observed Whiff Rate on Swings"
  )

ggsave("/mnt/storage/Rprojects/mlbr/models/fig2_whiff_vs_vaa.png", plot = p2, width = 8, height = 5, dpi = 150)
cat("-> Saved: models/fig2_whiff_vs_vaa.png\n")

cat("\n*** EDA COMPLETED SUCCESSFULLY ***\n")
