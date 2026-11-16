# ==============================================================================
# mlbr: Curated Experiment & Knowledge Base Registry
# ==============================================================================
# Maintains a machine-readable, low-noise repository of validated sabermetric
# discoveries, formulas, and empirical invariants across the 5-tier architecture.
# ==============================================================================

TIER_NAMES <- c(
  "1" = "Level 1: Pitch Physics & Quality",
  "2" = "Level 2: Player Projections",
  "3" = "Level 3: Team BaseRuns & Run Environment",
  "4" = "Level 4: Game Simulation Engine",
  "5" = "Level 5: Betting Markets & Staking"
)

VALID_STATUSES <- c("PROPOSED", "IN_PROGRESS", "VALIDATED", "REJECTED", "DEPRECATED")

#' Register or Update a Quantitative Experiment in the Knowledge Base
#'
#' @param id Character string unique identifier (e.g. "EXP-001-AERODYNAMICS")
#' @param title Descriptive title
#' @param tier Integer 1 through 5 representing the predictive modeling tier
#' @param status Character status ("PROPOSED", "IN_PROGRESS", "VALIDATED", "REJECTED", "DEPRECATED")
#' @param summary 1-2 sentence executive summary of the discovery
#' @param hypotheses Character vector of tested scientific hypotheses
#' @param formulas Named list of mathematical equations established
#' @param metrics Named list of empirical findings and validation scores
#' @param features Character vector of engineered feature names
#' @param artifacts Named list of associated files (script, spec, study)
#' @param lake Project root directory (default: mlbr default_lake)
#' @return Invisibly returns the updated registry list
#' @export
lake_log_experiment <- function(id,
                                title,
                                tier,
                                status = "VALIDATED",
                                summary = "",
                                hypotheses = character(),
                                formulas = list(),
                                metrics = list(),
                                features = character(),
                                artifacts = list(),
                                lake = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for lake_log_experiment().")
  }
  
  lake <- default_lake(lake)
  exp_dir <- file.path(lake, "experiments")
  if (!dir.exists(exp_dir)) dir.create(exp_dir, recursive = TRUE)
  
  # Validation
  tier_str <- as.character(tier)
  if (!(tier_str %in% names(TIER_NAMES))) {
    stop("Invalid tier. Must be an integer between 1 and 5.")
  }
  status <- toupper(status)
  if (!(status %in% VALID_STATUSES)) {
    stop("Invalid status. Must be one of: ", paste(VALID_STATUSES, collapse = ", "))
  }
  
  reg_file <- file.path(exp_dir, "registry.json")
  registry <- if (file.exists(reg_file)) {
    tryCatch(jsonlite::fromJSON(reg_file, simplifyVector = FALSE), error = function(e) list())
  } else {
    list()
  }
  
  record <- list(
    id = id,
    title = title,
    tier = as.integer(tier),
    tier_name = TIER_NAMES[[tier_str]],
    status = status,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    summary = summary,
    hypotheses = as.list(hypotheses),
    formulas = formulas,
    metrics = metrics,
    features = as.list(features),
    artifacts = artifacts
  )
  
  # Upsert into registry
  existing_ids <- vapply(registry, function(x) x$id, character(1))
  match_idx <- which(existing_ids == id)
  
  if (length(match_idx) > 0) {
    registry[[match_idx[1]]] <- record
    message("Updated existing experiment: ", id)
  } else {
    registry[[length(registry) + 1]] <- record
    message("Logged new experiment: ", id)
  }
  
  # Sort by tier, then id
  order_keys <- vapply(registry, function(x) sprintf("%d_%s", x$tier, x$id), character(1))
  registry <- registry[order(order_keys)]
  
  # Write clean, formatted JSON
  jsonlite::write_json(registry, reg_file, pretty = TRUE, auto_unbox = TRUE)
  
  # Automatically generate clean markdown README
  readme_file <- file.path(exp_dir, "README.md")
  md_lines <- c(
    "# Quantitative Experiment & Knowledge Base Registry",
    "",
    "This curated registry catalogs all verified mathematical models, empirical formulas, and features across the 5-tier predictive architecture.",
    "",
    "| Experiment ID | Tier | Title | Status | Key Formula / Metric | Updated |",
    "|:---|:---:|:---|:---:|:---|:---|"
  )
  
  for (item in registry) {
    # Extract first formula or first metric for summary column
    key_info <- if (length(item$formulas) > 0) {
      paste0("`", names(item$formulas)[1], "`: ", item$formulas[[1]])
    } else if (length(item$metrics) > 0) {
      paste0(names(item$metrics)[1], ": ", item$metrics[[1]])
    } else {
      item$summary
    }
    # Truncate if long
    if (nchar(key_info) > 60) key_info <- paste0(substr(key_info, 1, 57), "...")
    
    md_lines <- c(md_lines, sprintf(
      "| **[%s](registry.json)** | %d | %s | `%s` | %s | %s |",
      item$id, item$tier, item$title, item$status, key_info, substr(item$updated_at, 1, 10)
    ))
  }
  
  md_lines <- c(md_lines, "", "---", "*Maintained by mlbr::lake_log_experiment()*")
  writeLines(md_lines, readme_file)
  
  invisible(registry)
}

#' Retrieve the Experiments Registry
#'
#' @param lake Project root directory
#' @return A list of registered experiments
#' @export
lake_get_experiments <- function(lake = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }
  lake <- default_lake(lake)
  reg_file <- file.path(lake, "experiments", "registry.json")
  if (!file.exists(reg_file)) return(list())
  jsonlite::fromJSON(reg_file, simplifyVector = FALSE)
}

#' Retrieve a Single Experiment by ID
#'
#' @param id Experiment ID (e.g. "EXP-001-AERODYNAMICS")
#' @param lake Project root directory
#' @return A list representing the experiment or NULL if not found
#' @export
lake_get_experiment <- function(id, lake = NULL) {
  reg <- lake_get_experiments(lake)
  for (item in reg) {
    if (identical(item$id, id)) return(item)
  }
  NULL
}
