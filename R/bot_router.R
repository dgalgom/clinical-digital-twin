#' Claude-assisted query routing with a classify -> grade -> retry loop
#'
#' This is the richer, LLM-assisted path that sits ON TOP of the deterministic
#' `cdt_bot_classify_query()` fallback (R/bot_viz.R). The flow mirrors the
#' `viz-query-router` + `viz-classification-grader` subagents:
#'
#'   1. CLASSIFY  — ask Claude to map the free-text query onto one of the known
#'      chart intents + parameters, returning a small JSON spec.
#'   2. GRADE     — ask Claude (as an independent grader) whether that spec is a
#'      correct, renderable classification of the query. Pass -> render; fail ->
#'      feed the grader's reason back and retry the classification.
#'   3. RETRY     — bounded by `max_retries`; on exhaustion (or any error, or
#'      mock mode, or no API key) fall back to the DETERMINISTIC classifier so
#'      the bot always produces a usable route offline.
#'
#' No model/feature/schema changes: the router only chooses WHICH existing chart
#' to draw and with WHICH stored-data parameters. Every LLM step degrades safely
#' to the offline classifier. The API key is read only via the client in
#' R/claude_client.R and is never logged.

# The JSON contract the classifier LLM must emit. Kept in one place so the
# prompt, the grader, and the parser stay in sync with cdt_bot_intents().
.cdt_router_schema_text <- function() {
  paste0(
    "Return ONLY a compact JSON object with these keys:\n",
    '  "intent": one of [', paste(sprintf('"%s"', cdt_bot_intents()), collapse = ", "),
    ", \"none\"]\n",
    '  "patient_id": a patient id like "P004", or null\n',
    '  "window_phrase": the relative-time phrase verbatim (e.g. "previous two months"), or null\n',
    '  "metric": for *_over_time intents one of ',
    '["steps","resting_hr","sbp","sedentary"], else null\n',
    '  "rationale": one short sentence of reasoning\n',
    "No prose, no code fences, JSON only."
  )
}

# Extract the first JSON object from an LLM reply (tolerant of stray prose or
# fenced code blocks). Returns a parsed list or NULL.
.cdt_extract_json <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) {
    return(NULL)
  }
  s <- regmatches(txt, regexpr("\\{(?:[^{}]|\\n)*\\}", txt, perl = TRUE))
  if (length(s) == 0 || !nzchar(s[1])) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(s[1], simplifyVector = TRUE),
    error = function(e) NULL
  )
}

# Turn a parsed LLM spec into the SAME shape cdt_bot_classify_query() returns,
# resolving the window phrase into concrete [from,to] and filling chat focus.
.cdt_spec_from_llm <- function(parsed, text, chat_id = NULL) {
  if (is.null(parsed) || is.null(parsed$intent)) {
    return(NULL)
  }
  intent <- as.character(parsed$intent)[1]
  if (identical(intent, "none") || !intent %in% cdt_bot_intents()) {
    return(NULL)
  }
  pid <- parsed$patient_id
  if (is.null(pid) || is.na(pid) || identical(as.character(pid), "null")) {
    pid <- cdt_bot_extract_patient(text)
    if (is.null(pid) && !is.null(chat_id)) pid <- cdt_bot_focus(chat_id)
  } else {
    pid <- as.character(pid)[1]
  }

  wp <- parsed$window_phrase
  window <- NULL
  if (!is.null(wp) && !is.na(wp) && nzchar(as.character(wp)[1])) {
    window <- cdt_parse_relative_window(as.character(wp)[1])
  }
  if (is.null(window)) window <- cdt_parse_relative_window(text)

  metric <- parsed$metric
  metric <- if (is.null(metric) || is.na(metric) ||
    identical(as.character(metric), "null")) {
    NULL
  } else {
    as.character(metric)[1]
  }

  overrides <- if (identical(intent, "whatif")) cdt_bot_parse_whatif(text) else NULL

  list(
    intent = intent, patient_id = pid, window = window,
    metric = metric, whatif_overrides = overrides,
    rationale = if (!is.null(parsed$rationale)) as.character(parsed$rationale)[1] else "",
    source = "llm"
  )
}

#' LLM classification step (single attempt)
#'
#' @param text Free-text query.
#' @param chat_id Optional chat id for focus fallback.
#' @param feedback Optional grader feedback to steer a retry.
#' @param mock Explicit mock override (passed through to the client).
#' @return A spec list (shape of [cdt_bot_classify_query()]) or NULL on failure.
#' @export
cdt_bot_classify_query_llm <- function(text, chat_id = NULL,
                                       feedback = NULL, mock = NULL) {
  if (cdt_llm_is_mock(mock)) {
    return(NULL)
  }
  sys <- paste(
    "You are a visualization query router for a clinical fall-risk digital-twin",
    "bot (all data synthetic). Classify the clinician's request into exactly one",
    "supported chart intent and its parameters.", .cdt_router_schema_text()
  )
  usr <- text
  if (!is.null(feedback) && nzchar(feedback)) {
    usr <- paste0(
      text, "\n\n(Previous attempt was rejected by the grader: ", feedback,
      ". Correct it.)"
    )
  }
  reply <- tryCatch(
    cdt_claude_reply(usr, context = NULL, system_prompt = sys, mock = mock,
      max_tokens = 250),
    error = function(e) NULL
  )
  parsed <- .cdt_extract_json(reply)
  .cdt_spec_from_llm(parsed, text, chat_id)
}

#' Deterministic grade of a classification spec (offline grader)
#'
#' Validates the spec against the taxonomy and the parameters each intent needs
#' to render. Returns pass/fail + a short reason. This is the fallback the loop
#' uses when the LLM grader is unavailable, and the final gate on every spec.
#'
#' @param text The original query (for light keyword corroboration).
#' @param spec A classification spec.
#' @return `list(pass = logical, reason = character)`.
#' @export
cdt_bot_grade_classification <- function(text, spec) {
  if (is.null(spec) || is.null(spec$intent) || is.na(spec$intent)) {
    return(list(pass = FALSE, reason = "no intent"))
  }
  if (!spec$intent %in% cdt_bot_intents()) {
    return(list(pass = FALSE, reason = sprintf("unknown intent '%s'", spec$intent)))
  }
  if (is.null(spec$patient_id) || is.na(spec$patient_id) ||
    !grepl("^P\\d{3}$", spec$patient_id)) {
    return(list(pass = FALSE, reason = "missing/invalid patient id"))
  }
  # *_over_time intents must carry a renderable metric.
  needs_metric <- grepl("_over_time$", spec$intent)
  if (needs_metric) {
    if (is.null(spec$metric) ||
      !spec$metric %in% c("steps", "resting_hr", "sbp", "sedentary")) {
      return(list(pass = FALSE, reason = "series intent lacks a valid metric"))
    }
  }
  list(pass = TRUE, reason = "ok")
}

# LLM grader (single attempt). Returns list(pass, reason) or NULL on failure so
# the caller can fall back to the deterministic grader.
.cdt_grade_classification_llm <- function(text, spec, mock = NULL) {
  if (cdt_llm_is_mock(mock) || is.null(spec)) {
    return(NULL)
  }
  sys <- paste(
    "You are an independent grader. Decide whether the proposed chart",
    "classification correctly and renderably answers the clinician query.",
    "Supported intents:", paste(cdt_bot_intents(), collapse = ", "), ".",
    'Return ONLY JSON: {"pass": true|false, "reason": "one short sentence"}.'
  )
  spec_json <- jsonlite::toJSON(
    list(intent = spec$intent, patient_id = spec$patient_id,
      metric = spec$metric,
      window = if (is.null(spec$window)) NA else spec$window$label),
    auto_unbox = TRUE, null = "null"
  )
  usr <- paste0("QUERY:\n", text, "\n\nPROPOSED CLASSIFICATION:\n", spec_json)
  reply <- tryCatch(
    cdt_claude_reply(usr, context = NULL, system_prompt = sys, mock = mock,
      max_tokens = 120),
    error = function(e) NULL
  )
  parsed <- .cdt_extract_json(reply)
  if (is.null(parsed) || is.null(parsed$pass)) {
    return(NULL)
  }
  list(
    pass = isTRUE(as.logical(parsed$pass)),
    reason = if (!is.null(parsed$reason)) as.character(parsed$reason)[1] else ""
  )
}

#' Route a query to a chart spec via classify -> grade -> retry, with fallback
#'
#' The one entry point the bot should call. Tries the LLM classifier+grader loop
#' (bounded retries), then always falls back to the deterministic classifier so
#' a usable spec is returned even offline. The returned spec is guaranteed to
#' pass [cdt_bot_grade_classification()] OR to be the deterministic result.
#'
#' @param text Free-text clinician query.
#' @param chat_id Optional chat id (focus fallback).
#' @param max_retries Max LLM re-classification attempts after a grader failure.
#' @param mock Explicit mock override.
#' @return A spec list with an added `source` ("llm", "llm-graded", or
#'   "fallback") and `graded` (the grade result).
#' @export
cdt_bot_route_query <- function(text, chat_id = NULL, max_retries = 2,
                                mock = NULL) {
  feedback <- NULL
  if (!cdt_llm_is_mock(mock)) {
    for (attempt in seq_len(max_retries + 1L)) {
      spec <- cdt_bot_classify_query_llm(text, chat_id, feedback, mock = mock)
      if (is.null(spec)) break # LLM unavailable/unparseable -> stop looping.

      grade <- .cdt_grade_classification_llm(text, spec, mock = mock)
      if (is.null(grade)) {
        grade <- cdt_bot_grade_classification(text, spec)
      }
      if (isTRUE(grade$pass)) {
        spec$source <- "llm-graded"
        spec$graded <- grade
        return(spec)
      }
      feedback <- grade$reason
    }
  }

  # Deterministic fallback (offline path or LLM exhausted/unusable).
  spec <- cdt_bot_classify_query(text, chat_id)
  spec$source <- "fallback"
  spec$graded <- cdt_bot_grade_classification(text, spec)
  spec
}
