#' Telegram bot conversation logic
#'
#' Parses clinician messages, maintains per-chat "patient in focus" state,
#' detects simple what-if intents, grounds a prompt with real (synthetic) data +
#' model outputs, and returns a concise reply via Claude (or the mock).

# Per-chat conversation state: chat_id -> list(patient_id = ...).
.cdt_bot_env <- new.env(parent = emptyenv())
.cdt_bot_env$state <- list()

#' Get/set the patient in focus for a chat
#' @param chat_id Telegram chat id (coerced to character).
#' @param patient_id If provided, set focus; otherwise just read.
#' @return The current focus patient_id (character) or NULL.
#' @export
cdt_bot_focus <- function(chat_id, patient_id = NULL) {
  key <- as.character(chat_id)
  if (!is.null(patient_id)) {
    .cdt_bot_env$state[[key]] <- list(patient_id = patient_id)
    return(patient_id)
  }
  st <- .cdt_bot_env$state[[key]]
  if (is.null(st)) NULL else st$patient_id
}

#' Reset all bot conversation state (useful for tests)
#' @return Invisibly TRUE.
#' @export
cdt_bot_reset <- function() {
  .cdt_bot_env$state <- list()
  invisible(TRUE)
}

#' Extract a patient id from free text
#'
#' Recognizes forms like "P042", "patient 042", "patient 42", "042".
#'
#' @param text Message text.
#' @return A normalized patient id ("P042") or NULL.
#' @export
cdt_bot_extract_patient <- function(text) {
  m <- regmatches(text, regexpr("[Pp]\\s*0*\\d{1,3}", text))
  if (length(m) == 1 && nzchar(m)) {
    num <- as.integer(gsub("\\D", "", m))
    return(sprintf("P%03d", num))
  }
  m2 <- regmatches(text, regexpr("patient\\s+0*\\d{1,3}", text, ignore.case = TRUE))
  if (length(m2) == 1 && nzchar(m2)) {
    num <- as.integer(gsub("\\D", "", m2))
    return(sprintf("P%03d", num))
  }
  NULL
}

#' Detect what-if counterfactual intents from free text
#'
#' Very lightweight keyword/number extraction sufficient for the demo. Returns a
#' named list compatible with [cdt_apply_overrides()], or NULL if none found.
#'
#' @param text Message text.
#' @return Named list of overrides or NULL.
#' @export
cdt_bot_parse_whatif <- function(text) {
  t <- tolower(text)
  ov <- list()

  # "increase mobility/steps by 20%" or "more steps"
  pct <- regmatches(t, regexpr("\\d{1,3}\\s*%", t))
  pct_val <- if (length(pct)) as.numeric(gsub("\\D", "", pct)) else NULL

  if (grepl("increase|more|improve|boost|raise", t) &&
    grepl("mobil|step|walk|activity", t)) {
    ov$steps_pct <- if (!is.null(pct_val)) pct_val else 20
  }
  if (grepl("decrease|less|reduce|cut", t) &&
    grepl("mobil|step|walk|activity", t)) {
    ov$steps_pct <- if (!is.null(pct_val)) -pct_val else -20
  }
  # "lower systolic/BP by 10" (mmHg)
  if (grepl("bp|blood pressure|systolic", t) &&
    grepl("lower|reduce|decrease|drop", t)) {
    mm <- regmatches(t, regexpr("\\d{1,3}\\s*(mmhg)?", t))
    delta <- if (length(mm)) as.numeric(gsub("\\D", "", mm)) else 10
    ov$sbp_delta <- -abs(delta)
  }
  # "reduce sedentary time" / "less sitting"
  if (grepl("sedentary|sitting|lying", t) &&
    grepl("reduce|less|decrease|cut", t)) {
    ov$sedentary_hours_mean_7d <- 12
  }
  if (length(ov) == 0) NULL else ov
}

#' Handle one incoming clinician message and produce a reply
#'
#' Pipeline: resolve patient (explicit in message, else chat focus) -> detect
#' what-if -> build grounded context -> Claude/mock -> reply text.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param chat_id Telegram chat id.
#' @param text Message text.
#' @param llm_mock Optional explicit LLM mock override.
#' @return Character scalar reply.
#' @export
cdt_bot_handle_message <- function(con, model, chat_id, text, llm_mock = NULL) {
  text <- trimws(text %||% "")

  if (grepl("^/start", text)) {
    return(paste(
      "Fall-risk digital twin bot (synthetic data). Ask e.g.:",
      "\n- 'How is patient P042 trending?'",
      "\n- 'What if we increase patient P042's mobility by 25%?'"
    ))
  }

  pid <- cdt_bot_extract_patient(text)
  if (!is.null(pid)) {
    cdt_bot_focus(chat_id, pid)
  } else {
    pid <- cdt_bot_focus(chat_id)
  }

  if (is.null(pid)) {
    return(paste(
      "No patient specified. Mention one, e.g. 'How is patient P042 doing?'"
    ))
  }

  patient <- cdt_get_patient(con, pid)
  if (nrow(patient) == 0) {
    return(sprintf("Patient %s not found in the (synthetic) database.", pid))
  }

  overrides <- cdt_bot_parse_whatif(text)
  context <- cdt_patient_context(con, model, pid, modified_inputs = overrides)

  cdt_claude_reply(text, context = context, mock = llm_mock)
}

# Null-coalescing helper.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
