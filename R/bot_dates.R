#' Date + relative-time-window parsing for bot chart queries
#'
#' The synthetic sensor timeline is ingested daily and ends at "today"
#' (see [cdt_data_end_date()]), so relative queries such as "the previous two
#' months" or "last 30 days" are anchored to the real system date and resolve
#' onto real rows. This module turns free-text time phrases into a concrete
#' `[from, to]` date window and filters a readings tibble to it.
#'
#' Nothing here touches the model, features, or schema; it only slices the
#' timeline for display.

#' The current date the bot reasons about ("today")
#'
#' Delegates to [cdt_data_end_date()] so that a frozen build
#' (`CDT_DATA_END_DATE=YYYY-MM-DD`) and the live system clock stay consistent
#' between data generation and query-time windowing.
#'
#' @return A `Date`.
#' @export
cdt_bot_today <- function() {
  cdt_data_end_date()
}

#' Extract the calendar date (YYYY-MM-DD) from a stored timestamp
#'
#' `sensor_readings.ts` is ISO-8601 with an explicit offset
#' (`2026-01-01T06:00:00+0100`) and `fall_events.ts` is a bare date; both share
#' a `YYYY-MM-DD` prefix, so the date is the first 10 characters.
#'
#' @param ts Character vector of timestamps.
#' @return A `Date` vector.
#' @export
cdt_ts_to_date <- function(ts) {
  as.Date(substr(as.character(ts), 1, 10))
}

# Small word-number map for phrases like "previous two months".
.cdt_word_numbers <- function() {
  c(
    a = 1, an = 1, one = 1, two = 2, three = 3, four = 4, five = 5,
    six = 6, seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11,
    twelve = 12
  )
}

# Resolve a count token that may be digits ("30") or a word ("two"); NULL if
# neither is present in `token`.
.cdt_resolve_count <- function(token) {
  if (is.null(token) || !nzchar(token)) {
    return(NULL)
  }
  token <- tolower(trimws(token))
  if (grepl("^[0-9]+$", token)) {
    return(as.integer(token))
  }
  wn <- .cdt_word_numbers()
  if (token %in% names(wn)) {
    return(as.integer(wn[[token]]))
  }
  NULL
}

#' Parse a relative time window from free text, anchored to "today"
#'
#' Recognizes phrasings like:
#' * "previous/last/past N days|weeks|months" (N as digits or words)
#' * "previous/last/past week|month|year" (implicit N = 1)
#' * "two months", "30 days" (bare duration, treated as a look-back)
#' * "this week|month", "today", "yesterday"
#'
#' The window is `[to - span + 1 day, to]` where `to` is `today` (the latest
#' data date). Returns `NULL` when no time phrase is found, so the caller can
#' fall back to a default window (e.g. the whole timeline or the last N days).
#'
#' @param text Free-text query.
#' @param today Anchor date (default [cdt_bot_today()]).
#' @return A list `list(from, to, label)` of `Date`s + a human label, or `NULL`.
#' @export
cdt_parse_relative_window <- function(text, today = cdt_bot_today()) {
  t <- tolower(text %||% "")
  to <- as.Date(today)

  # "today" / "yesterday"
  if (grepl("\\btoday\\b", t)) {
    return(list(from = to, to = to, label = "today"))
  }
  if (grepl("\\byesterday\\b", t)) {
    return(list(from = to - 1, to = to - 1, label = "yesterday"))
  }

  # "this week" / "this month" / "this year"
  if (grepl("\\bthis\\s+week\\b", t)) {
    return(list(from = to - 6, to = to, label = "this week"))
  }
  if (grepl("\\bthis\\s+month\\b", t)) {
    return(list(from = to - 29, to = to, label = "this month"))
  }
  if (grepl("\\bthis\\s+year\\b", t)) {
    return(list(from = to - 364, to = to, label = "this year"))
  }

  # "previous/last/past <n> <unit>" or "<n> <unit>" (unit: day/week/month/year).
  # Capture an optional count token (digits or a number-word) before the unit.
  unit_pat <- paste0(
    "(?:previous|last|past)?\\s*",
    "([0-9]+|a|an|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)?",
    "\\s*(day|days|week|weeks|month|months|year|years)\\b"
  )
  m <- regmatches(t, regexec(unit_pat, t, perl = TRUE))[[1]]
  if (length(m) == 3 && nzchar(m[3])) {
    n <- .cdt_resolve_count(m[2])
    unit <- sub("s$", "", m[3])
    # Bare unit with no count means 1 (e.g. "last week").
    if (is.null(n)) n <- 1L
    per <- switch(unit, day = 1L, week = 7L, month = 30L, year = 365L)
    span_days <- n * per
    from <- to - (span_days - 1L)
    label <- sprintf("previous %d %s%s", n, unit, if (n == 1) "" else "s")
    return(list(from = from, to = to, label = label))
  }

  NULL
}

#' Filter a readings tibble to a `[from, to]` date window
#'
#' @param readings A tibble with a `ts` column.
#' @param from,to Inclusive `Date` bounds.
#' @return The subset of `readings` whose date falls in `[from, to]`, ordered by
#'   `ts`. May be zero rows if the window predates or postdates the data.
#' @export
cdt_filter_readings_window <- function(readings, from, to) {
  if (is.null(readings) || nrow(readings) == 0) {
    return(readings)
  }
  d <- cdt_ts_to_date(readings$ts)
  keep <- !is.na(d) & d >= as.Date(from) & d <= as.Date(to)
  out <- readings[keep, , drop = FALSE]
  out[order(out$ts), , drop = FALSE]
}

# Null-coalescing helper (defined in bot.R; redefined defensively so this file
# is self-contained when sourced in isolation, e.g. a targeted unit test).
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}
