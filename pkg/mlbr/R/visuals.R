#' Dark Cyber / Antigravity Visual Theme for ggplot2
#'
#' Provides an immersive dark aesthetic matching the mlbr Quarto site:
#' - Canvas: #090d16
#' - Panel surface: #111827
#' - Gridlines: #1e293b
#' - Typography: High-contrast legible text
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family
#' @return A ggplot2 theme object
#' @export
theme_cyber <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "#090d16", color = NA),
      panel.background  = ggplot2::element_rect(fill = "#111827", color = "#1e293b", linewidth = 0.6),
      panel.grid.major  = ggplot2::element_line(color = "#1e293b", linewidth = 0.4),
      panel.grid.minor  = ggplot2::element_line(color = "#162032", linewidth = 0.2),
      text              = ggplot2::element_text(color = "#e2e8f0"),
      plot.title        = ggplot2::element_text(color = "#f1f5f9", face = "bold", size = ggplot2::rel(1.15), margin = ggplot2::margin(b = 6)),
      plot.subtitle     = ggplot2::element_text(color = "#94a3b8", size = ggplot2::rel(0.92), margin = ggplot2::margin(b = 10)),
      plot.caption      = ggplot2::element_text(color = "#64748b", size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 10)),
      axis.title        = ggplot2::element_text(color = "#e2e8f0", face = "bold"),
      axis.text         = ggplot2::element_text(color = "#94a3b8"),
      legend.background = ggplot2::element_rect(fill = "#090d16", color = "#1e293b", linewidth = 0.4),
      legend.text       = ggplot2::element_text(color = "#cbd5e1"),
      legend.title      = ggplot2::element_text(color = "#f1f5f9", face = "bold"),
      strip.background  = ggplot2::element_rect(fill = "#1f2937", color = "#374151"),
      strip.text        = ggplot2::element_text(color = "#f8fafc", face = "bold")
    )
}

#' Cyber Color Palette
#'
#' @param n Number of colors or name of palette
#' @return Vector of hex colors
#' @export
cyber_palette <- function() {
  c(
    cyan    = "#38bdf8",
    emerald = "#10b981",
    amber   = "#f59e0b",
    rose    = "#f43f5e",
    purple  = "#a855f7",
    blue    = "#3b82f6",
    lime    = "#84cc16",
    indigo  = "#6366f1"
  )
}

#' Save Plot Assets (Image, Serialized Object, Audit Data)
#'
#' Adheres to the mlbr Asset Preservation Mandate by saving:
#' 1. High-resolution raster image (.png at 300 DPI)
#' 2. Serialized native R object (.rds)
#' 3. Underlying audit dataset (.parquet or .csv)
#' 4. Interactive HTML widget (.html if plotly object)
#'
#' @param plot A ggplot2 or plotly object
#' @param name Base filename without extension (e.g. "lab01_movement_arena")
#' @param data Optional underlying data frame to save as .parquet
#' @param width Plot width in inches (default: 10)
#' @param height Plot height in inches (default: 6)
#' @param dpi DPI resolution for raster image (default: 300)
#' @param dir Destination directory (default: "models")
#' @return Invisibly returns a named list of saved file paths
#' @export
save_plot_assets <- function(plot, name, data = NULL, width = 10, height = 6, dpi = 300, dir = "models") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  res <- list()
  
  # 1. High-resolution PNG
  png_path <- file.path(dir, paste0(name, ".png"))
  if (inherits(plot, "ggplot")) {
    ggplot2::ggsave(png_path, plot = plot, width = width, height = height, dpi = dpi)
    res$png <- png_path
  }
  
  # 2. Serialized Native R Object (.rds)
  rds_path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(plot, file = rds_path)
  res$rds <- rds_path
  
  # 3. Underlying Audit Data (.parquet / .csv)
  if (!is.null(data) && is.data.frame(data)) {
    if (requireNamespace("arrow", quietly = TRUE)) {
      parquet_path <- file.path(dir, paste0(name, ".parquet"))
      arrow::write_parquet(data, parquet_path)
      res$parquet <- parquet_path
    } else {
      csv_path <- file.path(dir, paste0(name, ".csv"))
      utils::write.csv(data, csv_path, row.names = FALSE)
      res$csv <- csv_path
    }
  }
  
  # 4. Interactive HTML widget if plotly
  if (inherits(plot, "plotly") && requireNamespace("htmlwidgets", quietly = TRUE)) {
    html_path <- file.path(dir, paste0(name, ".html"))
    htmlwidgets::saveWidget(plot, file = html_path, selfcontained = TRUE)
    res$html <- html_path
  }
  
  cat(sprintf("[mlbr] Preserved plot assets for '%s':\n", name))
  for (nm in names(res)) cat(sprintf("  - %s: %s\n", nm, res[[nm]]))
  
  invisible(res)
}