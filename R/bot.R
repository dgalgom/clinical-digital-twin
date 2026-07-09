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
    st <- .cdt_bot_env$state[[key]] %||% list()
    st$patient_id <- patient_id
    .cdt_bot_env$state[[key]] <- st
    return(patient_id)
  }
  st <- .cdt_bot_env$state[[key]]
  if (is.null(st)) NULL else st$patient_id
}

#' Get/set the authenticated username for a chat (username gate)
#'
#' The bot's access model (synthetic data): a clinician unlocks a chat by
#' stating a known username (verified read-only via [cdt_user_exists()]); NO
#' password ever travels over Telegram. This stores/returns that username.
#'
#' @param chat_id Telegram chat id (coerced to character).
#' @param username If provided, set the chat's authenticated username; otherwise
#'   just read it.
#' @return The authenticated username (character) or `NULL` if the chat is not
#'   yet authenticated.
#' @export
cdt_bot_authed <- function(chat_id, username = NULL) {
  key <- as.character(chat_id)
  if (!is.null(username)) {
    st <- .cdt_bot_env$state[[key]] %||% list()
    st$authed_username <- username
    .cdt_bot_env$state[[key]] <- st
    return(username)
  }
  st <- .cdt_bot_env$state[[key]]
  if (is.null(st)) NULL else st$authed_username
}

#' Extract a stated username from a login message
#'
#' Recognizes "/login <name>", "login as <name>", "log in as <name>",
#' "sign in as <name>", "i am <name>", "this is <name>". Returns the raw token
#' (existence is checked separately via [cdt_user_exists()]).
#'
#' @param text Message text.
#' @return The username token (character) or `NULL` if no login phrasing.
#' @export
cdt_bot_parse_login <- function(text) {
  t <- trimws(text %||% "")
  pat <- paste0(
    "(?i)^\\s*(?:/login|login(?:\\s+as)?|log\\s*in(?:\\s+as)?|",
    "sign\\s*in(?:\\s+as)?|i\\s*am|this\\s+is)\\s+([A-Za-z0-9_.-]+)\\s*$"
  )
  m <- regmatches(t, regexec(pat, t, perl = TRUE))[[1]]
  if (length(m) == 2 && nzchar(m[2])) {
    return(m[2])
  }
  NULL
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
#' Lightweight keyword/number extraction sufficient for the demo. Maps a
#' clinician's natural-language what-if onto the CLOSED set of levers the single
#' pooled model can actually ingest (see [cdt_apply_overrides()]); it never
#' invents a lever the model does not have. Returns a named list of overrides
#' compatible with [cdt_apply_overrides()], or `NULL` if no modeled lever is
#' mentioned.
#'
#' Supported levers (all map to existing model inputs; no model change):
#' * activity/steps/walking + percent -> `steps_pct` (relative)
#' * activity in minutes ("30 min more activity") -> `steps_pct` requires a
#'   baseline, so it is emitted as a raw `steps_minutes_delta` hint that
#'   [cdt_bot_reply()] converts against the patient's own `steps_mean_7d`.
#' * sitting/sedentary/lying to N hours (or "N hours less") -> absolute
#'   `sedentary_hours_mean_7d` (patient-relative deltas resolved in the caller).
#' * BP/systolic lower by N mmHg -> `sbp_delta`
#' * resting heart rate to N -> `resting_hr_mean_7d`
#' * HR variability to N -> `hr_variability_7d`
#' * "no prior falls" / "prior falls = 0" -> `prior_falls`
#' * "treat/resolve orthostatic hypotension" -> `orthostatic_hypotension = 0`
#'
#' The named-drug lever (decrement `n_medications`) needs the patient's
#' medication list, so it is resolved in [cdt_bot_reply()] via
#' [cdt_bot_resolve_drug_override()], not here.
#'
#' @param text Message text.
#' @return Named list of overrides or NULL.
#' @export
cdt_bot_parse_whatif <- function(text) {
  t <- tolower(text %||% "")
  ov <- list()

  # A signed number immediately followed by "%".
  pct <- regmatches(t, regexpr("\\d{1,3}\\s*%", t))
  pct_val <- if (length(pct)) as.numeric(gsub("\\D", "", pct)) else NULL

  activity <- grepl("mobil|step|walk|activity|active|exercis", t)
  up_word <- grepl("increase|more|improve|boost|raise|higher|up\\b", t)
  down_word <- grepl("decrease|less|reduce|cut|lower|fewer|down\\b", t)

  # --- Activity / steps -----------------------------------------------------
  if (activity && (up_word || down_word)) {
    # Minutes-of-activity phrasing ("30 min more activity") is patient-relative;
    # emit a hint the caller converts against the patient's steps baseline.
    mins <- regmatches(t, regexpr("\\d{1,3}\\s*(?:min|minute)", t))
    if (length(mins)) {
      m <- as.numeric(gsub("\\D", "", mins))
      ov$steps_minutes_delta <- if (down_word && !up_word) -m else m
    } else if (up_word && !down_word) {
      ov$steps_pct <- if (!is.null(pct_val)) pct_val else 20
    } else if (down_word) {
      ov$steps_pct <- if (!is.null(pct_val)) -pct_val else -20
    }
  }

  # --- Sedentary / sitting time --------------------------------------------
  if (grepl("sedentary|sitting|lying|sit\\b", t) && down_word) {
    # "to N hours" -> absolute; "N hours/hr less" -> caller-resolved delta.
    to_hrs <- regmatches(t, regexpr("to\\s+\\d{1,2}\\s*(?:h|hr|hour)", t))
    by_hrs <- regmatches(t, regexpr("\\d{1,2}\\s*(?:h|hr|hour)s?\\s*(?:less|fewer)", t))
    if (length(to_hrs)) {
      ov$sedentary_hours_mean_7d <- as.numeric(gsub("\\D", "", to_hrs))
    } else if (length(by_hrs)) {
      ov$sedentary_hours_delta <- -as.numeric(gsub("\\D", "", by_hrs))
    } else {
      ov$sedentary_hours_mean_7d <- 12
    }
  }

  # --- Systolic BP ----------------------------------------------------------
  if (grepl("bp|blood pressure|systolic|sbp", t) &&
    grepl("lower|reduce|decrease|drop", t)) {
    mm <- regmatches(t, regexpr("\\d{1,3}\\s*(mmhg)?", t))
    delta <- if (length(mm)) as.numeric(gsub("\\D", "", mm)) else 10
    ov$sbp_delta <- -abs(delta)
  }

  # --- Resting heart rate ("resting HR to 60") ------------------------------
  if (grepl("resting\\s*(?:hr|heart\\s*rate)", t)) {
    to_hr <- regmatches(t, regexpr("(?:to|=|at)\\s*\\d{2,3}", t))
    if (length(to_hr)) ov$resting_hr_mean_7d <- as.numeric(gsub("\\D", "", to_hr))
  }

  # --- HR variability ("HR variability to 8") -------------------------------
  if (grepl("variabilit|hrv", t)) {
    to_v <- regmatches(t, regexpr("(?:to|=|at)\\s*\\d{1,3}", t))
    if (length(to_v)) ov$hr_variability_7d <- as.numeric(gsub("\\D", "", to_v))
  }

  # --- Prior falls ("no prior falls" / "prior falls = 0") -------------------
  if (grepl("prior\\s*falls?", t) &&
    grepl("\\bno\\b|=\\s*0|zero|remove|clear", t)) {
    ov$prior_falls <- 0
  }

  # --- Orthostatic hypotension ("treat orthostatic hypotension") ------------
  if (grepl("orthostatic|postural\\s*hypotension", t) &&
    grepl("treat|resolve|correct|manage|fix|no\\b", t)) {
    ov$orthostatic_hypotension <- 0
  }

  if (length(ov) == 0) NULL else ov
}

# Factors a clinician might raise that the twin does NOT model. Detecting one
# lets the bot decline honestly (never fabricating an effect) and list what it
# CAN simulate. Returns the matched human-readable factor name, or NULL.
# Keys are case-insensitive regex patterns; values are the human-readable name.
.cdt_unmodeled_factors <- c(
  "sleep|slept|sleeping" = "sleep",
  "diet|dietary" = "diet",
  "nutrition|nutritional" = "nutrition",
  "hydrat" = "hydration",
  "vision|eyesight|glasses" = "vision/eyesight",
  "hearing" = "hearing",
  "alcohol" = "alcohol",
  "smoking|smoke" = "smoking",
  "\\bweight\\b|\\bbmi\\b" = "weight/BMI",
  "home hazard|trip hazard|rug|grab bar|handrail" = "home hazards",
  "footwear|shoes|slippers" = "footwear",
  "physical therapy|physio" = "physical therapy",
  "vitamin d" = "vitamin D",
  "balance training|balance exercis" = "balance training"
)

#' Detect a what-if that names an UNMODELED factor (e.g. sleep)
#'
#' The single pooled model has a fixed feature set; factors like sleep, diet, or
#' vision are not inputs. This scans for such factors so [cdt_bot_reply()] can
#' decline honestly and list the levers it can actually simulate, rather than
#' inventing an effect.
#'
#' @param text Message text.
#' @return A human-readable factor name (character) or `NULL`.
#' @export
cdt_bot_unmodeled_factor <- function(text) {
  t <- tolower(text %||% "")
  for (key in names(.cdt_unmodeled_factors)) {
    if (grepl(key, t, perl = TRUE)) {
      return(unname(.cdt_unmodeled_factors[[key]]))
    }
  }
  NULL
}

# The modeled levers a clinician can simulate, phrased for a decline message.
.cdt_supported_levers_text <- function() {
  paste(
    "activity/steps, sedentary (sitting) time, resting heart rate,",
    "systolic blood pressure, heart-rate variability, medications",
    "(deprescribing), prior falls, and orthostatic hypotension"
  )
}

#' Resolve a named-drug what-if against a patient's medication list
#'
#' Detects "remove/stop/deprescribe/discontinue <drug>" and checks the drug
#' token against the patient's `medications` string (split on `[;,|]`,
#' case-insensitive). If the drug is present, returns overrides that decrement
#' `n_medications` (floored at 0) and recompute `polypharmacy` (1 if the new
#' count is >= 5, else 0). If the query names a drug that the patient is NOT on,
#' returns a `not_found` marker so the caller can say so honestly instead of
#' fabricating an effect. Returns `NULL` when the message is not a drug what-if.
#'
#' @param patient A one-row patient tibble (must have `medications`,
#'   `n_medications`).
#' @param text Message text.
#' @return `NULL`, or a list with `overrides` (named list) and `drug`
#'   (character), or a list with `not_found = TRUE` and `drug`.
#' @export
cdt_bot_resolve_drug_override <- function(patient, text) {
  t <- tolower(text %||% "")
  if (!grepl("remove|stop|deprescrib|discontinu|take\\s+off|come\\s+off|without", t)) {
    return(NULL)
  }
  # Grab the token after the action verb (a drug-like word).
  m <- regmatches(
    t,
    regexec(
      "(?:remove|stop|deprescrib\\w*|discontinu\\w*|take off|come off|without)\\s+(?:the\\s+|their\\s+|patient'?s?\\s+)?([a-z][a-z0-9+.-]{2,})",
      t
    )
  )[[1]]
  if (length(m) < 2 || !nzchar(m[2])) {
    return(NULL)
  }
  drug <- m[2]

  meds_raw <- as.character(patient$medications %||% "")
  meds <- tolower(trimws(unlist(strsplit(meds_raw, "[;,|]"))))
  meds <- meds[nzchar(meds)]

  # Match if the named token is (a prefix of) any of the patient's meds.
  on_it <- any(vapply(meds, function(md) {
    identical(md, drug) || startsWith(md, drug) || startsWith(drug, md)
  }, logical(1)))

  if (!on_it) {
    return(list(not_found = TRUE, drug = drug))
  }

  cur_n <- suppressWarnings(as.numeric(patient$n_medications))
  if (is.na(cur_n)) cur_n <- length(meds)
  new_n <- max(0, cur_n - 1)
  list(
    overrides = list(
      n_medications = new_n,
      polypharmacy = as.integer(new_n >= 5)
    ),
    drug = drug
  )
}

#' Render the chart PNG that matches a routed query spec (or NULL)
#'
#' Given a classified spec (from [cdt_bot_route_query()] / the fallback
#' classifier) and a resolved patient, fetch the stored (synthetic) rows for the
#' spec's time window and dispatch to the matching `cdt_bot_plot_*` renderer.
#' Returns a temp PNG path or `NULL` when nothing is plottable, so the caller can
#' still send a text-only reply.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param pid Resolved patient id.
#' @param spec A classification spec (list with `intent`, `window`, `metric`,
#'   `whatif_overrides`).
#' @return A PNG file path, or `NULL`.
#' @keywords internal
cdt_bot_render_spec <- function(con, model, pid, spec) {
  intent <- spec$intent %||% NA_character_
  if (is.na(intent)) {
    return(NULL)
  }

  win <- spec$window
  window_label <- if (!is.null(win)) win$label else NULL

  # What-if: baseline vs simulated risk bars (no readings needed).
  if (identical(intent, "whatif")) {
    risk <- cdt_patient_risk(con, model, pid,
      modified_inputs = spec$whatif_overrides, include_baseline = TRUE
    )
    caption <- if (!is.null(spec$rationale)) spec$rationale else NULL
    return(cdt_bot_plot_whatif(risk, pid, caption = caption))
  }

  # Series/history intents draw from the (windowed) readings timeline.
  readings <- cdt_get_patient_timeline(con, pid)
  if (!is.null(win)) {
    readings <- cdt_filter_readings_window(readings, win$from, win$to)
  }

  if (intent %in% c("fall_history", "functional_history")) {
    falls <- cdt_get_fall_events(con, pid)
    fall_dates <- if (nrow(falls) > 0) falls$ts else NULL
    return(cdt_bot_plot_history(readings, pid,
      fall_dates = fall_dates, window_label = window_label
    ))
  }

  metric <- spec$metric %||% .cdt_intent_metric(intent)
  if (!is.null(metric)) {
    return(cdt_bot_plot_series(readings, pid, metric,
      window_label = window_label
    ))
  }

  NULL
}

# Map a *_over_time intent to its metric key (mirrors .cdt_metric_spec keys).
.cdt_intent_metric <- function(intent) {
  switch(intent,
    steps_over_time = "steps",
    resting_hr_over_time = "resting_hr",
    sbp_over_time = "sbp",
    sedentary_over_time = "sedentary",
    NULL
  )
}

#' One-line and full descriptions of the assistant
#'
#' A warm-but-professional framing used in `/start` (short) and `/help` (full).
#' The assistant is a clinical decision-SUPPORT aid grounded in each patient's
#' monitoring data and an interpretable risk model; it is not a substitute for
#' clinical judgment, and this demo runs on synthetic data only.
#'
#' @return A character scalar.
#' @export
cdt_bot_tagline <- function() {
  "Fall-Risk Digital Twin \u2014 a clinical decision-support assistant."
}

#' @rdname cdt_bot_tagline
#' @export
cdt_bot_description <- function() {
  paste(
    "Fall-Risk Digital Twin \u2014 Clinical Decision-Support Assistant",
    "",
    "I help your care team monitor fall risk and reason through care decisions.",
    "Ask in plain language and I'll surface each patient's current 24-hour and",
    "7-day fall-risk estimate, chart how their activity, heart rate, blood",
    "pressure, and sedentary time are trending, and simulate 'what-if' scenarios",
    "(more activity, less sitting, blood-pressure or medication changes) so you",
    "can weigh options before acting.",
    "",
    "Every answer is grounded in the patient's own monitoring data and an",
    "interpretable risk model \u2014 I won't invent clinical details, and I'll tell",
    "you plainly when a factor is outside what the model can simulate. I'm a",
    "decision-support aid, not a substitute for clinical judgment.",
    "(This demo runs on fully synthetic data.)",
    sep = "\n"
  )
}

#' The bot's command menu (for Telegram setMyCommands + /help)
#'
#' Returns a data frame of `command` (without the leading slash) and
#' `description`, suitable for Telegram's `setMyCommands`. Registration itself is
#' a deployment step (documented), not performed here.
#'
#' @return A data frame with columns `command`, `description`.
#' @export
cdt_bot_commands <- function() {
  data.frame(
    command = c(
      "start", "help", "risk", "history", "whatif",
      "triage", "drivers", "explain", "dashboard"
    ),
    description = c(
      "Greeting and how to get started",
      "Full description and command list",
      "Current 24h/7d fall risk for a patient (numbers only)",
      "Functional/fall-history chart for a patient",
      "Simulate a what-if scenario and show baseline vs simulated risk",
      "Top patients by 7-day fall risk (worklist)",
      "Top model drivers for a patient (why risk is what it is)",
      "Plain-language explanation of the model and its limits",
      "Open the patient in the web dashboard (deep link)"
    ),
    stringsAsFactors = FALSE
  )
}

# Render the command menu as a text block for /help.
.cdt_commands_help_text <- function() {
  cmds <- cdt_bot_commands()
  lines <- sprintf("/%s \u2014 %s", cmds$command, cmds$description)
  paste(lines, collapse = "\n")
}

# Parse a slash command into list(cmd = "history", rest = "P042 last month").
# Returns NULL when the text is not a slash command.
.cdt_parse_command <- function(text) {
  m <- regmatches(text, regexec("^/([A-Za-z]+)(?:@\\S+)?\\s*(.*)$", text))[[1]]
  if (length(m) < 2 || !nzchar(m[2])) {
    return(NULL)
  }
  list(cmd = tolower(m[2]), rest = trimws(m[3]))
}

# Build a deep link to the Shiny dashboard, optionally focused on a patient.
.cdt_dashboard_url <- function(pid = NULL) {
  base <- Sys.getenv("CDT_APP_URL", unset = "http://127.0.0.1:3838")
  if (!is.null(pid) && nzchar(pid)) paste0(base, "/?patient=", pid) else base
}

# A short plain-language model explanation for /explain.
.cdt_explain_text <- function() {
  paste(
    "How the estimate works:",
    "A single interpretable logistic model estimates the probability of a fall",
    "over the next 24 hours and 7 days from static risk factors (e.g. age,",
    "Parkinson's, osteoporosis, orthostatic hypotension, medications, prior",
    "falls) and recent wearable trends (steps, resting heart rate, systolic",
    "blood pressure, sedentary hours, HR variability).",
    "",
    "Limits: it is a risk-stratification aid, not a diagnosis. It only knows the",
    "factors above \u2014 it does NOT model sleep, diet, vision, or home hazards. It",
    "is trained on synthetic data for this demo and must not drive care alone.",
    sep = "\n"
  )
}

# Format a patient's numeric risk (for /risk).
.cdt_risk_line <- function(pid, risk) {
  sprintf(
    "%s fall risk: 24h=%.1f%% (%s), 7d=%.1f%% (%s).",
    pid, 100 * risk$p_24h, risk$tier_24h, 100 * risk$p_7d, risk$tier_7d
  )
}

#' Handle one incoming clinician message and produce a structured reply
#'
#' Pipeline: resolve patient (explicit in message, else chat focus) -> classify
#' the visualization intent (via [cdt_bot_route_query()], with the deterministic
#' fallback) -> render the matching chart to a PNG -> build grounded context ->
#' Claude/mock text. Returns both the text and an optional chart path so the
#' webhook can deliver a `sendMessage` + `sendPhoto` pair.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param chat_id Telegram chat id.
#' @param text Message text.
#' @param llm_mock Optional explicit LLM mock override.
#' @return A list `list(text = <character>, photo = <path or NULL>)`.
#' @export
cdt_bot_reply <- function(con, model, chat_id, text, llm_mock = NULL) {
  text <- trimws(text %||% "")

  # --- Open commands (no auth required) -------------------------------------
  cmd <- .cdt_parse_command(text)
  if (!is.null(cmd) && cmd$cmd == "start") {
    return(list(text = paste(
      cdt_bot_tagline(),
      "\nFirst, identify yourself: send 'login as <username>'.",
      "\nThen try:",
      "\n- 'How is patient P042 trending?'",
      "\n- 'What if we increase patient P042's mobility by 25%?'",
      "\nSend /help for the full command list.",
      "\n(Synthetic-data demo.)"
    ), photo = NULL))
  }
  if (!is.null(cmd) && cmd$cmd == "help") {
    return(list(text = paste(
      cdt_bot_description(),
      "",
      "Commands:",
      .cdt_commands_help_text(),
      "",
      "What-if examples: 'more activity 20%', 'sit 2 hours less',",
      "'lower BP by 10', 'resting HR to 60', 'remove furosemide'.",
      sep = "\n"
    ), photo = NULL))
  }
  if (!is.null(cmd) && cmd$cmd == "explain") {
    return(list(text = .cdt_explain_text(), photo = NULL))
  }

  # --- Username gate --------------------------------------------------------
  # Access requires a known username (no password over Telegram; synthetic
  # data). A login message unlocks the chat; until then, only /start and login
  # are accepted.
  login_name <- cdt_bot_parse_login(text)
  if (!is.null(login_name)) {
    if (cdt_user_exists(con, login_name)) {
      cdt_bot_authed(chat_id, login_name)
      return(list(text = sprintf(
        "Signed in as '%s'. Ask about a patient, e.g. 'How is patient P042 trending?'",
        login_name
      ), photo = NULL))
    }
    return(list(text = sprintf(
      "Unknown username '%s'. Send 'login as <username>' with a valid clinician username.",
      login_name
    ), photo = NULL))
  }

  if (is.null(cdt_bot_authed(chat_id))) {
    return(list(text = paste(
      "Please identify yourself first: send 'login as <username>'.",
      "\n(No password needed here \u2014 this is a synthetic-data demo bot.)"
    ), photo = NULL))
  }

  # --- Cohort-level command (no single patient needed) ----------------------
  if (!is.null(cmd) && cmd$cmd == "triage") {
    n <- suppressWarnings(as.integer(gsub("\\D", "", cmd$rest)))
    if (is.na(n) || n <= 0) n <- 5L
    snap <- cdt_cohort_snapshot(con, model)
    if (nrow(snap) == 0) {
      return(list(text = "No patients in the (synthetic) database.", photo = NULL))
    }
    top <- utils::head(snap, n)
    rows <- sprintf(
      "%d. %s \u2014 7d=%.1f%% (%s), 24h=%.1f%% (%s)",
      seq_len(nrow(top)), top$patient_id,
      100 * top$p_7d, top$tier_7d, 100 * top$p_24h, top$tier_24h
    )
    return(list(text = paste(
      c(sprintf("Top %d patients by 7-day fall risk:", nrow(top)), rows),
      collapse = "\n"
    ), photo = NULL))
  }

  pid <- cdt_bot_extract_patient(text)
  if (!is.null(pid)) {
    cdt_bot_focus(chat_id, pid)
  } else {
    pid <- cdt_bot_focus(chat_id)
  }

  if (is.null(pid)) {
    return(list(text = paste(
      "No patient specified. Mention one, e.g. 'How is patient P042 doing?'"
    ), photo = NULL))
  }

  patient <- cdt_get_patient(con, pid)
  if (nrow(patient) == 0) {
    return(list(
      text = sprintf("Patient %s not found in the (synthetic) database.", pid),
      photo = NULL
    ))
  }

  # --- Patient-scoped commands ----------------------------------------------
  if (!is.null(cmd) && cmd$cmd == "risk") {
    r <- cdt_patient_risk(con, model, pid)
    return(list(text = .cdt_risk_line(pid, r), photo = NULL))
  }
  if (!is.null(cmd) && cmd$cmd == "dashboard") {
    return(list(text = sprintf(
      "Open %s in the dashboard:\n%s", pid, .cdt_dashboard_url(pid)
    ), photo = NULL))
  }
  if (!is.null(cmd) && cmd$cmd == "drivers") {
    imp <- utils::head(cdt_feature_importance(model, "7d"), 5)
    rows <- sprintf("- %s (%+.2f)", imp$feature, imp$coefficient)
    r <- cdt_patient_risk(con, model, pid)
    # Suggested, evidence-based interventions for the top drivers (P0-2).
    di <- cdt_driver_interventions(model, top_n = 3L)
    sugg <- character(0)
    if (nrow(di) > 0) {
      sugg <- c("", "Suggested interventions:")
      for (i in seq_len(nrow(di))) {
        sugg <- c(sugg, sprintf("* %s (%s):", di$label[i], di$urgency[i]),
          sprintf("   - %s", di$interventions[[i]]))
      }
    }
    return(list(text = paste(c(
      sprintf("Top model drivers for %s (standardized coefficients):", pid),
      rows, "", .cdt_risk_line(pid, r),
      "(Coefficients are cohort-level model drivers, not patient-specific attributions.)",
      sugg, "",
      "(Interventions are illustrative decision-support on synthetic data, not clinical guidance.)"
    ), collapse = "\n"), photo = NULL))
  }
  if (!is.null(cmd) && cmd$cmd == "history") {
    readings <- cdt_get_patient_timeline(con, pid)
    falls <- cdt_get_fall_events(con, pid)
    fall_dates <- if (nrow(falls) > 0) falls$ts else NULL
    photo <- tryCatch(
      cdt_bot_plot_history(readings, pid, fall_dates = fall_dates),
      error = function(e) NULL
    )
    context <- cdt_patient_context(con, model, pid)
    reply_text <- cdt_claude_reply(
      sprintf("Summarize the functional history of patient %s.", pid),
      context = context, mock = llm_mock
    )
    return(list(text = reply_text, photo = photo))
  }

  # --- What-if levers -------------------------------------------------------
  # Parse the modeled levers, resolve patient-relative deltas + the named-drug
  # lever against this patient's own baseline, then decide whether the query is
  # a what-if that only names UNMODELED factors (honest decline).
  overrides <- cdt_bot_parse_whatif(text)
  drug <- cdt_bot_resolve_drug_override(patient, text)

  # A drug the patient is NOT on: decline honestly, do not fabricate an effect.
  if (!is.null(drug) && isTRUE(drug$not_found)) {
    return(list(text = sprintf(
      "%s is not in %s's current medication list, so I can't simulate deprescribing it. I can simulate: %s.",
      drug$drug, pid, .cdt_supported_levers_text()
    ), photo = NULL))
  }

  overrides <- .cdt_resolve_relative_overrides(con, patient, pid, overrides)
  if (!is.null(drug) && !is.null(drug$overrides)) {
    overrides <- utils::modifyList(overrides %||% list(), drug$overrides)
  }

  # If it reads as a what-if but names ONLY an unmodeled factor (e.g. sleep) and
  # produced no usable override, decline honestly and list what CAN be simulated.
  unmodeled <- cdt_bot_unmodeled_factor(text)
  if (!is.null(unmodeled) && (is.null(overrides) || length(overrides) == 0)) {
    return(list(text = sprintf(
      "The digital twin doesn't model %s, so I can't simulate that. I can simulate: %s.",
      unmodeled, .cdt_supported_levers_text()
    ), photo = NULL))
  }

  # Classify the query into a chart intent (LLM-graded when available, else the
  # deterministic fallback) and render the matching PNG. For what-if charts, use
  # the fully-resolved overrides (incl. the named-drug lever) so the plotted
  # simulation matches the grounded text. Failures are non-fatal: a NULL photo
  # just yields a text-only reply.
  photo <- tryCatch({
    spec <- cdt_bot_route_query(text, chat_id = chat_id, mock = llm_mock)
    if (identical(spec$intent %||% NA_character_, "whatif") && !is.null(overrides)) {
      spec$whatif_overrides <- overrides
    }
    cdt_bot_render_spec(con, model, pid, spec)
  }, error = function(e) NULL)

  context <- cdt_patient_context(con, model, pid, modified_inputs = overrides)
  reply_text <- cdt_claude_reply(text, context = context, mock = llm_mock)

  list(text = reply_text, photo = photo)
}

# Resolve patient-relative what-if hints (minutes of activity, sedentary-hours
# deltas) into concrete overrides against this patient's own baseline features.
# Pure model inputs only -- no model/feature change. Returns the override list
# (possibly NULL) with the hint keys removed.
.cdt_resolve_relative_overrides <- function(con, patient, pid, overrides) {
  if (is.null(overrides) || length(overrides) == 0) {
    return(overrides)
  }
  needs_baseline <- !is.null(overrides$steps_minutes_delta) ||
    !is.null(overrides$sedentary_hours_delta)
  feats <- if (needs_baseline) {
    readings <- cdt_get_patient_timeline(con, pid)
    cdt_assemble_features(patient, readings)
  } else {
    NULL
  }

  # "30 min more activity": convert minutes to a steps % against the patient's
  # own steps baseline using a transparent 100 steps/min walking proxy, capped
  # so a single lever can't drive an implausible multiplier.
  if (!is.null(overrides$steps_minutes_delta)) {
    base_steps <- as.numeric(feats$steps_mean_7d)
    if (is.finite(base_steps) && base_steps > 0) {
      delta_steps <- overrides$steps_minutes_delta * 100
      pct <- 100 * delta_steps / base_steps
      pct <- max(-90, min(200, pct))
      overrides$steps_pct <- (overrides$steps_pct %||% 0) + pct
    }
    overrides$steps_minutes_delta <- NULL
  }

  # "sit 2 hours less": subtract from the patient's sedentary baseline (floored).
  if (!is.null(overrides$sedentary_hours_delta)) {
    base_sed <- as.numeric(feats$sedentary_hours_mean_7d)
    if (is.finite(base_sed)) {
      overrides$sedentary_hours_mean_7d <-
        max(0, base_sed + overrides$sedentary_hours_delta)
    }
    overrides$sedentary_hours_delta <- NULL
  }

  if (length(overrides) == 0) NULL else overrides
}

#' Handle one incoming clinician message and produce a text reply (back-compat)
#'
#' Thin wrapper over [cdt_bot_reply()] that returns only the reply text, so
#' existing text-only callers keep working. New callers that want to deliver the
#' chart should call [cdt_bot_reply()] and send `$photo` via
#' [cdt_telegram_send_photo()].
#'
#' @inheritParams cdt_bot_reply
#' @return Character scalar reply.
#' @export
cdt_bot_handle_message <- function(con, model, chat_id, text, llm_mock = NULL) {
  cdt_bot_reply(con, model, chat_id, text, llm_mock = llm_mock)$text
}

# Null-coalescing helper.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
