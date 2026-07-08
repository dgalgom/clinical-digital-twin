#!/usr/bin/env Rscript
# Convenience runner: Rscript tests/run_tests.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) {
  normalizePath(file.path(dirname(file_arg), ".."))
} else {
  normalizePath(getwd())
}
Sys.setenv(CDT_PROJECT_ROOT = root)
library(testthat)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
res <- test_dir(file.path(root, "tests", "testthat"), reporter = "summary")
