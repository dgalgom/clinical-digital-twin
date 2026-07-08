#!/usr/bin/env Rscript
# One-shot setup: install dependencies (via renv if available, else install.packages)
# and build the demo dataset + model.
#
# Usage:  Rscript setup.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) normalizePath(dirname(file_arg)) else normalizePath(getwd())
setwd(root)
Sys.setenv(CDT_PROJECT_ROOT = root)

core <- c("DBI", "RSQLite", "dplyr", "tidyr", "tibble", "lubridate",
  "jsonlite", "sodium")
frontend <- c("shiny", "bslib", "plotly", "DT", "plumber", "httr2", "testthat")
all_pkgs <- c(core, frontend)

if (requireNamespace("renv", quietly = TRUE) && file.exists("renv.lock")) {
  message("Restoring dependencies with renv...")
  renv::restore(prompt = FALSE)
} else {
  installed <- rownames(installed.packages())
  missing <- setdiff(all_pkgs, installed)
  if (length(missing) > 0) {
    message("Installing: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    message("All required packages already installed.")
  }
}

message("Building synthetic dataset + training model...")
source(file.path(root, "data-raw", "generate_synthetic_data.R"))
message("\nSetup complete. Next:")
message("  Rscript -e \"shiny::runApp('app.R', port=3838)\"   # dashboard")
message("  Rscript api/run_api.R                              # REST API + bot webhook")
message("  Rscript tests/run_tests.R                          # tests")
