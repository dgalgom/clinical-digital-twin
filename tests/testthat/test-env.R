# Tests for the startup .env / .Renviron loader (R/env.R).
# All fixtures are temp files; the real project .env is never read here, and
# every environment mutation is reverted so global test state is untouched.

test_that("cdt_parse_dotenv handles comments, exports, quotes, and blanks", {
  path <- tempfile(fileext = ".env")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "# a comment",
    "",
    "  # indented comment",
    "PLAIN=value1",
    "  SPACED   =   value2  ",
    "export EXPORTED=value3",
    "DQUOTED=\"quoted value\"",
    "SQUOTED='single quoted'",
    "EQ_IN_VALUE=a=b=c",
    "EMPTY=",
    "NO_EQUALS_LINE",
    "=missingkey"
  ), path)

  kv <- cdt_parse_dotenv(path)

  expect_equal(kv[["PLAIN"]], "value1")
  expect_equal(kv[["SPACED"]], "value2")          # key + value trimmed
  expect_equal(kv[["EXPORTED"]], "value3")        # leading `export ` stripped
  expect_equal(kv[["DQUOTED"]], "quoted value")   # one matched quote pair removed
  expect_equal(kv[["SQUOTED"]], "single quoted")
  expect_equal(kv[["EQ_IN_VALUE"]], "a=b=c")      # split on FIRST `=` only

  expect_false("EMPTY" %in% names(kv))            # empty value skipped
  expect_false("NO_EQUALS_LINE" %in% names(kv))   # no `=` skipped
  expect_false("" %in% names(kv))                 # empty key skipped
})

test_that("cdt_parse_dotenv keeps the last value on duplicate keys", {
  path <- tempfile(fileext = ".env")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("DUP=first", "DUP=second"), path)

  kv <- cdt_parse_dotenv(path)
  expect_equal(sum(names(kv) == "DUP"), 1L)
  expect_equal(kv[["DUP"]], "second")
})

test_that("cdt_parse_dotenv returns empty for missing/NULL path", {
  expect_length(cdt_parse_dotenv(NULL), 0L)
  expect_length(cdt_parse_dotenv(tempfile(fileext = ".env")), 0L)
})

test_that("cdt_load_dotenv does not overwrite an already-set variable", {
  path <- tempfile(fileext = ".env")
  on.exit(unlink(path), add = TRUE)
  key <- "CDT_TEST_PRESET_KEY"
  writeLines(paste0(key, "=from_file"), path)

  old <- Sys.getenv(key, unset = NA)
  Sys.setenv(CDT_TEST_PRESET_KEY = "from_shell")
  on.exit({
    if (is.na(old)) Sys.unsetenv(key) else do.call(Sys.setenv, stats::setNames(list(old), key))
  }, add = TRUE)

  set <- cdt_load_dotenv(path, overwrite = FALSE)
  expect_false(key %in% set)
  expect_equal(Sys.getenv(key), "from_shell")     # shell precedence preserved
})

test_that("cdt_load_dotenv sets an unset variable and can overwrite", {
  path <- tempfile(fileext = ".env")
  on.exit(unlink(path), add = TRUE)
  key <- "CDT_TEST_UNSET_KEY"
  writeLines(paste0(key, "=from_file"), path)

  old <- Sys.getenv(key, unset = NA)
  Sys.unsetenv(key)
  on.exit({
    if (is.na(old)) Sys.unsetenv(key) else do.call(Sys.setenv, stats::setNames(list(old), key))
  }, add = TRUE)

  set <- cdt_load_dotenv(path, overwrite = FALSE)
  expect_true(key %in% set)
  expect_equal(Sys.getenv(key), "from_file")

  # overwrite = TRUE replaces an already-set value.
  writeLines(paste0(key, "=updated"), path)
  set2 <- cdt_load_dotenv(path, overwrite = TRUE)
  expect_true(key %in% set2)
  expect_equal(Sys.getenv(key), "updated")
})

test_that("cdt_load_env reads .env from a temp root and preserves auto-mock on empty keys", {
  root <- tempfile("cdt_root_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  key_real <- "CDT_TEST_ENV_REAL"
  key_empty <- "CDT_TEST_ENV_EMPTY"
  writeLines(c(
    paste0(key_real, "=present"),
    paste0(key_empty, "=")        # empty -> must be skipped (auto-mock preserved)
  ), file.path(root, ".env"))

  old_real <- Sys.getenv(key_real, unset = NA)
  old_empty <- Sys.getenv(key_empty, unset = NA)
  Sys.unsetenv(key_real)
  Sys.unsetenv(key_empty)
  on.exit({
    if (is.na(old_real)) Sys.unsetenv(key_real) else do.call(Sys.setenv, stats::setNames(list(old_real), key_real))
    if (is.na(old_empty)) Sys.unsetenv(key_empty) else do.call(Sys.setenv, stats::setNames(list(old_empty), key_empty))
  }, add = TRUE)

  set <- suppressMessages(cdt_load_env(root = root))
  expect_true(key_real %in% set)
  expect_false(key_empty %in% set)
  expect_equal(Sys.getenv(key_real), "present")
  expect_false(nzchar(Sys.getenv(key_empty)))     # stayed unset
})
