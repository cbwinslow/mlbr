# model.R - starter experiments (run from project root; in R: source("R/model.R"))
#
# Experiment 1: predict next-season OPS from current-season stats
#   (the aging-curve problem - the canonical baseball modeling exercise)
# Experiment 2 (if Statcast is downloaded): pitch-type classification
#
# Uses tidymodels with three engines so you can compare model families.

suppressPackageStartupMessages({
  library(arrow)
  library(tidymodels)
})

bat <- read_parquet("data/processed/batting_seasons.parquet") |>
  filter(PA >= 150, !is.na(OPS), !is.na(next_OPS), !is.na(age)) |>
  select(yearID, OPS, OBP, SLG, PA, age, next_OPS)

train <- bat |> filter(yearID <= 2023)
test  <- bat |> filter(yearID >= 2024)
cat(sprintf("batting experiment: %d train / %d test player-seasons\n",
            nrow(train), nrow(test)))

specs <- list(
  "linear (lm)"        = linear_reg() |> set_engine("lm"),
  "random forest"      = rand_forest(mode = "regression") |>
                           set_engine("ranger", num.threads = 32),
  "gradient boosting"  = boost_tree(mode = "regression") |>
                           set_engine("xgboost")
)

res <- imap(specs, \(spec, nm) {
  spec |>
    fit(next_OPS ~ OPS + OBP + SLG + PA + age, data = train) |>
    predict(test) |>
    bind_cols(test) |>
    metrics(truth = next_OPS, estimate = .pred) |>
    filter(.metric %in% c("rmse", "rsq")) |>
    transmute(model = nm, .metric, .estimate)
}) |>
  list_rbind()

print(res |> pivot_wider(names_from = .metric, values_from = .estimate) |>
        arrange(rmse), n = 10)

# ---- Experiment 2: pitch-type classification (needs Statcast ingest) ---------
sc_files <- list.files("data/raw/statcast", pattern = "parquet$", full.names = TRUE)
if (length(sc_files) > 0) {
  cat("\n== pitch classification (Statcast) ==\n")
  sc <- open_dataset("data/raw/statcast") |>
    filter(game_year >= 2023, !is.na(pitch_name), !is.na(release_speed),
           !is.na(release_spin_rate), !is.na(pfx_x), !is.na(pfx_z)) |>
    select(pitch_name, release_speed, release_spin_rate, pfx_x, pfx_z,
           release_pos_x, release_pos_z) |>
    collect()

  # rare/unknown pitch types would break stratified CV - keep common ones
  keep <- sc |> count(pitch_name) |> filter(n >= 1000) |> pull(pitch_name)
  sc <- sc |> filter(pitch_name %in% keep) |> mutate(pitch_name = factor(pitch_name))

  set.seed(42)
  split <- initial_split(sc, prop = 0.8, strata = pitch_name)
  train_p <- training(split); test_p <- testing(split)

  pitch_fit <- boost_tree(mode = "classification", trees = 300) |>
    set_engine("xgboost") |>
    fit(pitch_name ~ ., data = train_p)

  pitch_fit |>
    predict(test_p) |>
    bind_cols(test_p |> select(pitch_name)) |>
    metrics(truth = pitch_name, estimate = .pred) |>
    print()

  cat("top confusion pairs:\n")
  print(pitch_fit |> predict(test_p) |> bind_cols(test_p |> select(pitch_name)) |>
          conf_mat(truth = pitch_name, estimate = .pred) |>
          (\(x) x$table)() |> (\(m) diag(m) - m[cbind(rownames(m), colnames(m))])() |>
          sort() |> head(6) |> (\(x) data.frame(from = names(x), misclassified_as = x))())
} else {
  cat("\n(statcast not ingested yet - run: Rscript R/ingest.R statcast 2021 2025)\n")
}
