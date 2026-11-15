# ==============================================================================
# mlbr Modeling Lab 01: Empirical Whiff Probability Model
# ==============================================================================
# Objective:
#   Train a machine learning model to estimate the probability that a batter
#   swings and misses (whiffs) at a pitch given its velocity, movement, and location.
#
# Data Source:
#   Zero-copy DuckDB view over raw Statcast Parquet data lake (/mnt/storage/Rprojects/mlbr)
# ==============================================================================

# Step 1: Load packages from our high-performance library
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(ggplot2)
  library(tidymodels)
  library(ranger)
})

cat("Connecting to mlbr data lake via DuckDB...\n")
con <- lake_duckdb("/mnt/storage/Rprojects/mlbr")

# Step 2: Query a clean, representative sample of competitive pitches
# We filter for swings on fastballs (FF, SI) and breaking balls (SL, ST)
query <- "
SELECT 
  pitch_type,
  release_speed,
  release_spin_rate,
  pfx_x * 12.0 AS horiz_break_in,
  pfx_z * 12.0 AS induced_vert_break_in,
  plate_x,
  plate_z,
  balls,
  strikes,
  p_throws,
  stand,
  description,
  CASE WHEN description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') THEN 1 ELSE 0 END AS is_whiff
FROM v_statcast
WHERE description IN ('swinging_strike', 'swinging_strike_blocked', 'foul_tip', 'foul', 'hit_into_play')
  AND pitch_type IN ('FF', 'SL', 'SI', 'ST', 'CH')
  AND release_speed IS NOT NULL
  AND plate_x IS NOT NULL AND plate_z IS NOT NULL
  AND pfx_x IS NOT NULL AND pfx_z IS NOT NULL
USING SAMPLE 100000;
"

cat("Extracting 100,000 swing events from DuckDB...\n")
pitches <- dbGetQuery(con, query) |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

cat(sprintf("Loaded %s pitches. Overall Whiff Rate: %.1f%%\n",
            format(nrow(pitches), big.mark = ","),
            100 * mean(pitches$is_whiff)))

# Step 3: Feature Engineering
# Convert categorical features to factors and compute distance from center of zone
pitches <- pitches |>
  mutate(
    is_whiff = factor(is_whiff, levels = c(0, 1), labels = c("contact", "whiff")),
    pitch_type = factor(pitch_type),
    p_throws = factor(p_throws),
    stand = factor(stand),
    dist_from_center = sqrt(plate_x^2 + (plate_z - 2.5)^2)
  )

# Step 4: Train / Test Split using tidymodels (80% train, 20% test)
set.seed(42)
split <- initial_split(pitches, prop = 0.8, strata = is_whiff)
train_data <- training(split)
test_data  <- testing(split)

cat(sprintf("Training set: %s rows | Testing set: %s rows\n", 
            format(nrow(train_data), big.mark = ","), 
            format(nrow(test_data), big.mark = ",")))

# Step 5: Model Specification
# Model A: Logistic Regression Baseline
cat("\nFitting Logistic Regression baseline...\n")
lr_mod <- glm(
  is_whiff ~ release_speed + induced_vert_break_in + horiz_break_in + plate_x + plate_z + dist_from_center + strikes,
  data = train_data,
  family = binomial(link = "logit")
)
summary(lr_mod)

# Step 6: Model Evaluation on Out-of-Sample Test Set
test_preds <- test_data |>
  mutate(
    pred_whiff_prob = predict(lr_mod, newdata = test_data, type = "response")
  )

# Calculate ROC AUC
auc_metric <- roc_auc(test_preds, truth = is_whiff, pred_whiff_prob, event_level = "second")
cat(sprintf("\nTest Set ROC AUC: %.4f\n", auc_metric$.estimate))

# Step 7: Visualize Whiff Probability Surface in the Strike Zone
grid <- expand.grid(
  plate_x = seq(-1.5, 1.5, length.out = 80),
  plate_z = seq(1.0, 4.0, length.out = 80),
  release_speed = 96.0,          # 96 mph fastball
  induced_vert_break_in = 18.0,  # 18 inches ride (IVB)
  horiz_break_in = -5.0,
  strikes = 2
)
grid$dist_from_center <- sqrt(grid$plate_x^2 + (grid$plate_z - 2.5)^2)
grid$whiff_prob <- predict(lr_mod, newdata = grid, type = "response")

p <- ggplot(grid, aes(x = plate_x, y = plate_z, fill = whiff_prob)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "inferno", labels = scales::percent) +
  # Draw standard MLB rulebook strike zone
  geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
            fill = NA, color = "white", linewidth = 1, linetype = "dashed") +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Expected Whiff Probability Surface (4-Seam Fastball: 96 mph, 18\" IVB)",
    subtitle = "Higher whiff probability at the top edge and outside the strike zone (0-2 count)",
    x = "Horizontal Plate Location (ft, catcher perspective)",
    y = "Vertical Plate Location (ft)",
    fill = "Whiff Prob"
  )

# Save plot to artifacts
ggsave("/mnt/storage/Rprojects/mlbr/models/whiff_probability_surface.png", plot = p, width = 8, height = 7, dpi = 150)
cat("\nWhiff surface visualization saved to: models/whiff_probability_surface.png\n")
