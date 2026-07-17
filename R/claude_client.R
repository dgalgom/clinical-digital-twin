#' Claude API client (httr2)
#'
#' Thin wrapper around the Anthropic Messages API. The API key is read ONLY from
#' the `ANTHROPIC_API_KEY` environment variable and is never logged or hardcoded.
#'
#' Mock mode: if the key is absent, or `CDT_MOCK_LLM=1`, or `mock = TRUE`, the
#' client returns a deterministic templated reply built from the grounded
#' context. This lets the bot's prompt-grounding and routing logic be tested
#' without network access or credentials.

#' Default Claude model id used by the bot
#' @return Character scalar model id.
#' @export
cdt_claude_model <- function() {
  Sys.getenv("CDT_CLAUDE_MODEL", unset = "claude-sonnet-4-6")
}

#' Which LLM backend the bot should use
#'
#' Selects the inference backend for [cdt_claude_reply()]. Motivated by the
#' Telegram bot's perceived latency: a hosted low-latency backend (Groq's
#' Llama 3.3 70B, ~0.3-0.8s) is a drop-in alternative to Claude (~2-5s) that
#' mirrors the sibling human-digital-twin project's `telegram_bot_llama.py`.
#'
#' Resolution order:
#'   1. `CDT_LLM_BACKEND` if set to `"claude"` or `"groq"` (explicit wins).
#'   2. `"groq"` if a `GROQ_API_KEY` is present (opt-in by credential).
#'   3. `"claude"` otherwise (the original default).
#'
#' The choice only affects the LIVE path; mock mode is backend-agnostic and the
#' grounded prompt, PII handling, and safe fallbacks are identical either way.
#'
#' @return `"claude"` or `"groq"`.
#' @export
cdt_llm_backend <- function() {
  choice <- tolower(Sys.getenv("CDT_LLM_BACKEND", unset = ""))
  if (choice %in% c("claude", "groq")) {
    return(choice)
  }
  if (nzchar(Sys.getenv("GROQ_API_KEY"))) {
    return("groq")
  }
  "claude"
}

#' Default Groq model id used by the bot
#'
#' Llama 3.3 70B on Groq is fast and inexpensive; override with `CDT_GROQ_MODEL`.
#'
#' @return Character scalar model id.
#' @export
cdt_groq_model <- function() {
  Sys.getenv("CDT_GROQ_MODEL", unset = "llama-3.3-70b-versatile")
}

#' Is the client running in mock mode?
#'
#' @param mock Explicit override; if non-NULL it wins.
#' @return Logical.
#' @export
cdt_llm_is_mock <- function(mock = NULL) {
  if (!is.null(mock)) {
    return(isTRUE(mock))
  }
  if (identical(Sys.getenv("CDT_MOCK_LLM"), "1")) {
    return(TRUE)
  }
  # Live only if the SELECTED backend has a usable key; otherwise fall back to
  # the deterministic mock so the bot always answers offline.
  if (identical(cdt_llm_backend(), "groq")) {
    return(!nzchar(Sys.getenv("GROQ_API_KEY")))
  }
  !nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
}

#' Build a deterministic mock reply from the grounded context
#'
#' @param system_prompt The system prompt.
#' @param user_message The clinician's message.
#' @param context Grounded patient/model context text (may be NULL).
#' @return Character scalar reply.
#' @keywords internal
.cdt_mock_reply <- function(system_prompt, user_message, context) {
  if (is.null(context) || !nzchar(context)) {
    return(paste0(
      "[MOCK] No patient in focus. Ask about a specific patient, e.g. ",
      "'How is patient P042 trending?'"
    ))
  }
  # Pull the baseline risk line out of the grounded context to echo it back.
  risk_line <- grep("fall risk", strsplit(context, "\n")[[1]],
    value = TRUE, ignore.case = TRUE
  )
  paste0(
    "[MOCK CLINICAL SUMMARY] Based only on the provided synthetic data:\n",
    if (length(risk_line)) paste(risk_line, collapse = "\n") else context,
    "\n(This is a deterministic mock response; set ANTHROPIC_API_KEY for live Claude replies.)"
  )
}

#' Call Claude to produce a grounded clinical summary
#'
#' @param user_message The clinician's message text.
#' @param context Grounded facts (patient data + model outputs) to inject.
#' @param system_prompt Optional system prompt override.
#' @param mock Optional explicit mock override (`TRUE`/`FALSE`).
#' @param max_tokens Max response tokens.
#' @param temperature Optional sampling temperature passed to the live backend.
#'   `NULL` (default) leaves the backend default untouched; provided for the
#'   simulation's retry-at-lower-temperature idiom. Ignored in mock mode.
#' @return Character scalar reply text.
#' @export
cdt_claude_reply <- function(user_message,
                             context = NULL,
                             system_prompt = NULL,
                             mock = NULL,
                             max_tokens = 400,
                             temperature = NULL) {
  if (is.null(system_prompt)) {
    system_prompt <- paste(
      "You are a trusted clinical colleague helping nursing-home staff make",
      "sense of fall-risk for a specific patient. Talk like a sharp, caring",
      "clinician briefing a peer at the bedside: warm, direct, and human -- not",
      "a form letter or a bulleted robot.",
      "",
      "How to answer:",
      "- Lead with the bottom line in the first sentence (the risk tier and what",
      "  it means for this shift), then give the one or two reasons that matter.",
      "- Use plain, confident language a busy nurse can act on. Prefer 'her",
      "  activity has been dropping' over 'steps_mean_7d exhibits a negative",
      "  trend'. No jargon dumps, no restating every number.",
      "- Keep it tight: 2-5 sentences. Sound like a person, not a template.",
      "- When a what-if changed the risk, say so plainly and in human terms",
      "  ('getting her walking a bit more would pull the 7-day risk down').",
      "",
      "Hard rules (never break):",
      "- ALL patient data is synthetic. Use ONLY the facts in the context block;",
      "  never invent clinical values, diagnoses, medications, or risk numbers.",
      "- Refer to the patient by their coded id (e.g. 'P042') or as 'the",
      "  patient'/'she'/'he'; you are never given a name and must never guess one.",
      "- Use the risk tier language (Low/Moderate/High) for the headline.",
      "- If the context doesn't contain the answer, say so plainly rather than",
      "  guessing."
    )
  }

  grounded_user <- if (!is.null(context) && nzchar(context)) {
    paste0(
      "CONTEXT (synthetic, authoritative facts):\n", context,
      "\n\nCLINICIAN MESSAGE:\n", user_message
    )
  } else {
    user_message
  }

  if (cdt_llm_is_mock(mock)) {
    return(.cdt_mock_reply(system_prompt, user_message, context))
  }

  # --- Live call: dispatch to the selected backend -------------------------
  # Both backends receive the identical grounded prompt and PII-safe context;
  # only the transport (endpoint + auth + response shape) differs. Groq's
  # Llama 3.3 70B is a low-latency alternative to Claude for the Telegram bot.
  if (identical(cdt_llm_backend(), "groq")) {
    return(.cdt_groq_call(system_prompt, grounded_user, max_tokens, temperature))
  }
  .cdt_claude_call(system_prompt, grounded_user, max_tokens, temperature)
}

# Retry policy for live LLM calls. Rate limits (HTTP 429) and transient server
# errors (500/502/503/529) are retried with exponential backoff; the provider's
# `Retry-After` header is honored when present. Tunable via env so a dense batch
# job (the simulation) can retry more patiently than the latency-sensitive bot:
#   CDT_LLM_MAX_TRIES  (default 5)
#   CDT_LLM_BACKOFF_CAP seconds, the max single backoff (default 30)
# Applied identically to the Claude and Groq pipelines so behaviour is uniform.
.cdt_llm_max_tries <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("CDT_LLM_MAX_TRIES", "5")))
  if (is.na(n) || n < 1) 5L else n
}

.cdt_req_with_retry <- function(req) {
  cap <- suppressWarnings(as.numeric(Sys.getenv("CDT_LLM_BACKOFF_CAP", "30")))
  if (is.na(cap) || cap <= 0) cap <- 30
  httr2::req_retry(
    req,
    max_tries = .cdt_llm_max_tries(),
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 529L)
    },
    backoff = function(i) min(cap, 2^i)
  )
}

# Live Anthropic Messages API call. Returns reply text or a safe error string.
.cdt_claude_call <- function(system_prompt, grounded_user, max_tokens,
                             temperature = NULL) {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  body <- list(
    model = cdt_claude_model(),
    max_tokens = max_tokens,
    system = system_prompt,
    messages = list(list(role = "user", content = grounded_user))
  )
  if (!is.null(temperature)) {
    body$temperature <- as.numeric(temperature)
  }

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    .cdt_req_with_retry() |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(sprintf(
      "[LLM error %d] Could not generate a summary. Underlying data is still available in the dashboard.",
      status
    ))
  }
  parsed <- httr2::resp_body_json(resp)
  txt <- tryCatch(parsed$content[[1]]$text, error = function(e) NULL)
  if (is.null(txt)) {
    return("[LLM error] Unexpected response format.")
  }
  txt
}

# Live Groq (OpenAI-compatible chat completions) call. Groq maps the system
# prompt to a `system` message and the grounded prompt to a `user` message.
# Returns reply text or a safe error string, mirroring the Claude path so the
# bot's error handling is identical regardless of backend.
.cdt_groq_call <- function(system_prompt, grounded_user, max_tokens,
                           temperature = NULL) {
  key <- Sys.getenv("GROQ_API_KEY")
  body <- list(
    model = cdt_groq_model(),
    max_tokens = max_tokens,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = grounded_user)
    )
  )
  if (!is.null(temperature)) {
    body$temperature <- as.numeric(temperature)
  }

  resp <- httr2::request("https://api.groq.com/openai/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", key),
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    .cdt_req_with_retry() |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    return(sprintf(
      "[LLM error %d] Could not generate a summary. Underlying data is still available in the dashboard.",
      status
    ))
  }
  parsed <- httr2::resp_body_json(resp)
  txt <- tryCatch(parsed$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt)) {
    return("[LLM error] Unexpected response format.")
  }
  txt
}
