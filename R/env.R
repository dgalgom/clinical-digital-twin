#' Startup environment loader (.env / .Renviron)
#'
#' R does not auto-load a `.env` file. This module provides a small, dependency-
#' free loader so that launching the Shiny app or the plumber API picks up the
#' project's secrets (`ANTHROPIC_API_KEY`, `TELEGRAM_BOT_TOKEN`) and optional
#' overrides from a git-ignored `.env` (or `.Renviron`), enabling live Claude /
#' Telegram calls automatically when those files are present.
#'
#' Safety properties (by design):
#' - **Values are never printed or logged** — only key names and booleans.
#' - **Empty values are skipped**, so placeholder lines like `KEY=` in a copied
#'   `.Renviron.example` never set `""` and thus never disable the auto-mock
#'   fallback in the Claude / Telegram clients.
#' - **Shell / CI precedence**: an already-set environment variable is not
#'   overwritten (unless `overwrite = TRUE`), so `CDT_MOCK_*` and any keys
#'   injected by the shell or CI always win.
#' - The loader **never clears** variables and never forces live mode; absence
#'   of a key simply leaves the client in its existing mock behavior.

#' Parse a dotenv-style file into a named character vector
#'
#' Parse-only: reads the file and returns key/value pairs without touching the
#' process environment. Blank lines and `#` comments are skipped; an optional
#' leading `export ` is stripped; the line is split on the FIRST `=`; key and
#' value are trimmed; one matched surrounding quote pair (`"` or `'`) is
#' removed from the value. Lines with an empty key, no `=`, or an empty value
#' are ignored.
#'
#' @param path Path to a `.env`-style file.
#' @return Named character vector of parsed key/value pairs (possibly empty).
#' @export
cdt_parse_dotenv <- function(path) {
  empty <- character(0)
  if (is.null(path) || !file.exists(path)) {
    return(empty)
  }
  lines <- tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) character(0)
  )
  keys <- character(0)
  vals <- character(0)
  for (ln in lines) {
    s <- trimws(ln)
    if (!nzchar(s) || startsWith(s, "#")) {
      next
    }
    # Strip an optional leading `export `.
    if (grepl("^export[[:space:]]+", s)) {
      s <- sub("^export[[:space:]]+", "", s)
    }
    eq <- regexpr("=", s, fixed = TRUE)
    if (eq < 1) {
      next # no '=' -> not a key/value line
    }
    key <- trimws(substr(s, 1, eq - 1))
    val <- trimws(substr(s, eq + 1, nchar(s)))
    if (!nzchar(key)) {
      next # empty key
    }
    # Strip one matched surrounding quote pair.
    if (nchar(val) >= 2) {
      first <- substr(val, 1, 1)
      last <- substr(val, nchar(val), nchar(val))
      if ((first == "\"" && last == "\"") || (first == "'" && last == "'")) {
        val <- substr(val, 2, nchar(val) - 1)
      }
    }
    if (!nzchar(val)) {
      next # empty value -> skip (preserves auto-mock)
    }
    keys <- c(keys, key)
    vals <- c(vals, val)
  }
  if (length(keys) == 0) {
    return(empty)
  }
  # On duplicate keys, keep the last occurrence (dotenv convention).
  keep <- !duplicated(keys, fromLast = TRUE)
  stats::setNames(vals[keep], keys[keep])
}

#' Load a dotenv-style file into the process environment
#'
#' Parses `path` via [cdt_parse_dotenv()] and sets each pair with `Sys.setenv`.
#' By default an already-set variable is left untouched (shell / CI precedence).
#'
#' @param path Path to a `.env`-style file.
#' @param overwrite If `FALSE` (default), do not clobber variables already set
#'   in the environment. If `TRUE`, always set.
#' @return Invisibly, the character vector of key names that were actually set.
#' @export
cdt_load_dotenv <- function(path, overwrite = FALSE) {
  kv <- cdt_parse_dotenv(path)
  set <- character(0)
  for (key in names(kv)) {
    already <- nzchar(Sys.getenv(key))
    if (!overwrite && already) {
      next
    }
    args <- stats::setNames(list(kv[[key]]), key)
    do.call(Sys.setenv, args)
    set <- c(set, key)
  }
  invisible(set)
}

#' Load the project's environment at startup
#'
#' Precedence: existing shell/CI variables > `.env` > `.Renviron`. The `.env`
#' file is loaded non-destructively (never overwriting already-set variables).
#' `.Renviron` is only consulted as a fallback when no `.env` is present, via
#' base R's [readRenviron()] (note: `readRenviron` DOES overwrite, so it is used
#' only in the fallback case).
#'
#' This function only SETS non-empty variables found in those files; it never
#' clears anything and never forces mock or live mode. Missing keys simply leave
#' the Claude / Telegram clients in their existing (mock) behavior.
#'
#' @param root Project root to resolve the files against (default
#'   [cdt_project_root()]).
#' @return Invisibly, the character vector of key names set from `.env` (empty
#'   when only the `.Renviron` fallback ran or nothing was loaded).
#' @export
cdt_load_env <- function(root = cdt_project_root()) {
  dotenv_path <- file.path(root, ".env")
  renviron_path <- file.path(root, ".Renviron")

  if (file.exists(dotenv_path)) {
    set <- cdt_load_dotenv(dotenv_path, overwrite = FALSE)
    if (length(set)) {
      message("cdt_load_env: set ", length(set), " variable(s) from .env: ",
        paste(set, collapse = ", "))
    } else {
      message("cdt_load_env: .env present; no new variables set ",
        "(already-set or empty values skipped).")
    }
    return(invisible(set))
  }

  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
    message("cdt_load_env: loaded .Renviron fallback.")
  }
  invisible(character(0))
}
