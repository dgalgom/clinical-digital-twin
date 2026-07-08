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
#' @return Character scalar reply text.
#' @export
cdt_claude_reply <- function(user_message,
                             context = NULL,
                             system_prompt = NULL,
                             mock = NULL,
                             max_tokens = 400) {
  if (is.null(system_prompt)) {
    system_prompt <- paste(
      "You are a clinical decision-support assistant for a fall-risk digital",
      "twin prototype. ALL patient data is synthetic. Use ONLY the facts in the",
      "provided context block; never invent clinical values, diagnoses, or risk",
      "numbers. Be concise (2-5 sentences), use the risk tier language",
      "(Low/Moderate/High), and note when a what-if simulation changed risk.",
      "If the context lacks the answer, say so plainly."
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

  # --- Live call (requires httr2 + ANTHROPIC_API_KEY) ---------------------
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  body <- list(
    model = cdt_claude_model(),
    max_tokens = max_tokens,
    system = system_prompt,
    messages = list(list(role = "user", content = grounded_user))
  )

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
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
