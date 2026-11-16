# ==============================================================================
# mlbr: My First Sabermetric Experiment
# ==============================================================================
# Follow along step-by-step: Highlight lines and press Ctrl + Enter to run!
# ==============================================================================

# Step 1: Load packages
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
})

# Step 2: Connect to the zero-copy DuckDB data lake
con <- mlbr::lake_duckdb()

# Step 3: Run SQL query over modern pitches
query <- "
  SELECT 
    pitch_type,
    COUNT(*) AS total_pitches,
    ROUND(AVG(release_speed), 1) AS avg_velo,
    ROUND(AVG(ivb_in), 1) AS avg_ivb,
    ROUND(AVG(vaa), 2) AS avg_vaa,
    ROUND(AVG(CASE WHEN is_whiff = 1 THEN 1.0 ELSE 0.0 END)::NUMERIC * 100, 1) AS whiff_rate
  FROM v_statcast_features
  WHERE game_date >= '2024-01-01'
    AND pitch_type IN ('FF', 'SI', 'SL', 'ST', 'CH', 'CU')
  GROUP BY pitch_type
  ORDER BY total_pitches DESC;
"

arsenals <- DBI::dbGetQuery(con, query)
print(arsenals)

# Step 4: Plot Velocity vs Whiff Rate
p <- ggplot(arsenals, aes(x = avg_velo, y = whiff_rate, label = pitch_type)) +
  geom_point(size = 5, color = "#2b5c8f") +
  geom_text(vjust = -1, fontface = "bold", size = 4.5) +
  scale_y_continuous(limits = c(10, 40)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "My First Sabermetric Chart: Velocity vs. Whiff Rate (2024)",
    subtitle = "Notice sliders and sweepers get high whiffs despite throwing 8-12 mph slower than fastballs!",
    x = "Average Release Velocity (mph)",
    y = "Whiff Rate on All Pitches (%)"
  )

print(p)

# Step 5: Always disconnect when finished
DBI::dbDisconnect(con, shutdown = TRUE)
