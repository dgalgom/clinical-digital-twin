# Test runner. Sources the package R files then runs all tests.
library(testthat)

root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."))
if (!nzchar(root) || is.na(root)) root <- normalizePath(getwd())
Sys.setenv(CDT_PROJECT_ROOT = root)

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

test_dir(file.path(root, "tests", "testthat"))

`%||%` <- function(a, b) if (is.null(a)) b else a
