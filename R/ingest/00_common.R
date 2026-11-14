# 00_common.R - shared helpers for all ingest scripts
# DO NOT call directly; source()d by every ingest_* script.

options(timeout = 900)  # retrosheet.org in particular is slow; default 60s is not enough

raw_dir <- function(...) {
  d <- file.path("data", "raw", ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# write a data frame (or list, e.g. retrosheet year bundles) atomically to raw/
write_df <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  if (is.data.frame(df)) arrow::write_parquet(df, tmp) else saveRDS(df, tmp)
  file.rename(tmp, path)
  message(sprintf("  wrote %s (%s)", path,
                  if (is.data.frame(df)) sprintf("%d rows", nrow(df)) else "list"))
}

# retry helper: wraps an expression, retries up to n times with backoff
retry <- function(n = 3, wait = 10, expr) {
  for (i in 1:n) {
    out <- tryCatch(force(expr), error = function(e) {
      if (i < n) { message("    attempt ", i, " failed: ",
                           substr(conditionMessage(e), 1, 80), " - retrying in ", wait, "s")
                  Sys.sleep(wait) }
      NULL
    })
    if (!is.null(out)) return(out)
  }
  stop("all ", n, " attempts failed")
}

# throttle(min_seconds): enforce a minimum spacing between HTTP requests so we
# stay well inside every source's tolerance. The baseballr fetchers do NOT
# rate-limit themselves - every ingest script must call this between requests.
.last_request_time <- NULL
throttle <- function(min_seconds = 2) {
  if (!is.null(.last_request_time)) {
    elapsed <- as.numeric(difftime(Sys.time(), .last_request_time, units = "secs"))
    if (elapsed < min_seconds) Sys.sleep(min_seconds - elapsed)
  }
  .last_request_time <<- Sys.time()
  invisible()
}
