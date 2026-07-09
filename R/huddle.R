#' Post-fall huddle drafting (P0-4)  -- flagship Claude moment
#'
#' Best practice after every fall is a structured post-fall huddle: what
#' happened, what the sensor data showed in the days before, likely contributing
#' factors, and the plan. Historically `fall_events` was used ONLY as model
#' labels; this module turns each fall into an actionable workflow.
#'
#' `cdt_draft_huddle_summary()` grounds an LLM prompt on the 72h of pre-fall
#' sensor data (mirroring the `cdt_patient_context()` recipe: inject real
#' synthetic facts + a "don't invent details" instruction). It only DRAFTS text;
#' the clinician reviews/edits and saves via `cdt_complete_huddle()`. In mock
#' mode (no key / CDT_MOCK_LLM=1) it returns a deterministic template built from
#' the same grounded facts, so the demo works offline.
#'
#' No model / feature / schema-label change: the fall's `patient_id`/`ts`/
#' `severity` are untouched; drafting is read-only over the stored timeline.

# How many days of pre-fall sensor history to summarise.
.cdt_huddle_window_days <- function() 3L

# Coerce a stored fall `ts` (ISO date or datetime with offset) to a Date.
.cdt_fall_date <- function(ts) {
  as.Date(substr(as.character(ts), 1, 10))
}

# Build the grounded pre-fall context block for one fall event. Returns a list
# with `text` (the prompt context) and `facts` (structured values the mock
# template and tests can assert on). Patient NAME is never included -- only the
# coded id crosses into the prompt, consistent with cdt_patient_context().
cdt_huddle_context <- function(con, model, event_id) {
  fall <- cdt_get_fall_event(con, event_id)
  if (nrow(fall) == 0) {
    return(NULL)
  }
  pid <- fall$patient_id[1]
  patient <- cdt_get_patient(con, pid)
  if (nrow(patient) == 0) {
    return(NULL)
  }

  fall_date <- .cdt_fall_date(fall$ts[1])
  window <- .cdt_huddle_window_days()

  tl <- cdt_get_patient_timeline(con, pid)
  tl_date <- if (nrow(tl) > 0) .cdt_fall_date(tl$ts) else as.Date(character(0))

  # 72h before (and up to) the fall vs a prior baseline of equal length.
  pre_idx <- which(tl_date <= fall_date & tl_date > (fall_date - window))
  base_idx <- which(tl_date <= (fall_date - window) &
    tl_date > (fall_date - 2L * window))

  mean_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
  pre <- tl[pre_idx, , drop = FALSE]
  base <- tl[base_idx, , drop = FALSE]

  facts <- list(
    patient_id = pid,
    age = patient$age[1],
    sex = patient$sex[1],
    fall_date = as.character(fall_date),
    severity = fall$severity[1] %||% NA_character_,
    n_pre_days = nrow(pre),
    steps_pre = mean_or_na(pre$step_count),
    steps_base = mean_or_na(base$step_count),
    resting_hr_pre = mean_or_na(pre$resting_hr),
    resting_hr_base = mean_or_na(base$resting_hr),
    hours_lying_pre = mean_or_na(pre$hours_lying),
    hours_lying_base = mean_or_na(base$hours_lying),
    sbp_pre = mean_or_na(pre$sbp)
  )

  pct_change <- function(now, was) {
    if (!is.finite(now) || !is.finite(was) || was == 0) return(NA_real_)
    100 * (now - was) / was
  }
  facts$steps_change_pct <- pct_change(facts$steps_pre, facts$steps_base)
  facts$hr_change_bpm <- if (is.finite(facts$resting_hr_pre) &&
    is.finite(facts$resting_hr_base)) {
    facts$resting_hr_pre - facts$resting_hr_base
  } else {
    NA_real_
  }
  facts$lying_change_hrs <- if (is.finite(facts$hours_lying_pre) &&
    is.finite(facts$hours_lying_base)) {
    facts$hours_lying_pre - facts$hours_lying_base
  } else {
    NA_real_
  }

  # Static risk flags for grounding (coded, no name).
  fmt <- function(v, digits = 0) if (is.finite(v)) formatC(v, format = "f",
    digits = digits) else "n/a"
  lines <- c(
    sprintf("Fall event for patient %s (age %d, sex %s) on %s; recorded severity: %s.",
      pid, as.integer(facts$age), facts$sex, facts$fall_date,
      facts$severity %||% "unspecified"),
    sprintf("Static risk factors: parkinsons=%d, osteoporosis=%d, orthostatic_hypotension=%d, polypharmacy=%d, prior_falls=%d, n_medications=%d.",
      patient$parkinsons[1], patient$osteoporosis[1],
      patient$orthostatic_hypotension[1], patient$polypharmacy[1],
      patient$prior_falls[1], patient$n_medications[1]),
    sprintf("Pre-fall %d-day sensor window (%d daily read-outs before the fall):",
      window, facts$n_pre_days),
    sprintf("  - steps/day: %s (baseline %s; change %s%%).",
      fmt(facts$steps_pre), fmt(facts$steps_base),
      if (is.finite(facts$steps_change_pct)) sprintf("%+.0f", facts$steps_change_pct) else "n/a"),
    sprintf("  - resting HR: %s bpm (baseline %s; change %s bpm).",
      fmt(facts$resting_hr_pre, 1), fmt(facts$resting_hr_base, 1),
      if (is.finite(facts$hr_change_bpm)) sprintf("%+.1f", facts$hr_change_bpm) else "n/a"),
    sprintf("  - hours lying/day: %s (baseline %s; change %s h).",
      fmt(facts$hours_lying_pre, 1), fmt(facts$hours_lying_base, 1),
      if (is.finite(facts$lying_change_hrs)) sprintf("%+.1f", facts$lying_change_hrs) else "n/a"),
    sprintf("  - mean systolic BP: %s mmHg.", fmt(facts$sbp_pre))
  )

  list(text = paste(lines, collapse = "\n"), facts = facts)
}

# Deterministic mock huddle draft built from the grounded facts. Mentions the
# windowed metrics so the demo (and tests) can see the grounding is real.
.cdt_mock_huddle <- function(ctx) {
  f <- ctx$facts
  trend_bits <- character(0)
  if (is.finite(f$steps_change_pct) && f$steps_change_pct <= -10) {
    trend_bits <- c(trend_bits, sprintf("activity fell %.0f%% (to ~%.0f steps/day)",
      abs(f$steps_change_pct), f$steps_pre))
  }
  if (is.finite(f$hr_change_bpm) && f$hr_change_bpm >= 2) {
    trend_bits <- c(trend_bits, sprintf("resting HR rose %+.1f bpm", f$hr_change_bpm))
  }
  if (is.finite(f$lying_change_hrs) && f$lying_change_hrs >= 0.5) {
    trend_bits <- c(trend_bits, sprintf("time lying down increased %+.1f h/day",
      f$lying_change_hrs))
  }
  trend <- if (length(trend_bits) == 0) {
    "no marked change in the tracked sensor signals in the 72h before the fall"
  } else {
    paste(trend_bits, collapse = "; ")
  }

  paste0(
    "[MOCK HUDDLE DRAFT - synthetic data; review and edit before saving]\n",
    sprintf("Patient %s, age %d, had a %s-severity fall on %s.\n",
      f$patient_id, as.integer(f$age), f$severity %||% "unspecified", f$fall_date),
    sprintf("In the %d-day pre-fall window, %s.\n",
      .cdt_huddle_window_days(), trend),
    "Likely contributing factors to review with the team: recent mobility/",
    "activity change, medication load, and any orthostatic BP drop.\n",
    "Suggested plan: confirm injury check, review contributing meds, and set ",
    "targeted precautions (e.g. rounding/toileting schedule, bed-exit alarm).\n",
    "(Deterministic mock; set an LLM key for a live Claude/Groq draft.)"
  )
}

#' Draft a post-fall huddle narrative for a fall event (P0-4)
#'
#' Grounds an LLM on the 72h pre-fall sensor window and asks for a concise,
#' structured huddle draft. DRAFT ONLY -- never writes to the DB. In mock mode
#' returns a deterministic template built from the same grounded facts.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model` (reserved for future risk grounding; unused today).
#' @param event_id Fall event identifier.
#' @param mock Optional explicit mock override (passed to the LLM client).
#' @return A character scalar draft, or `NULL` if the event/patient is unknown.
#' @export
cdt_draft_huddle_summary <- function(con, model, event_id, mock = NULL) {
  ctx <- cdt_huddle_context(con, model, event_id)
  if (is.null(ctx)) {
    return(NULL)
  }
  if (cdt_llm_is_mock(mock)) {
    return(.cdt_mock_huddle(ctx))
  }

  sys <- paste(
    "You are a clinical colleague helping nursing-home staff run a structured",
    "post-fall huddle. ALL data is synthetic. Using ONLY the grounded facts",
    "provided, draft a concise huddle note a nurse can review and edit. Cover:",
    "(1) a one-line what-happened, (2) what the 72h pre-fall sensor trend showed",
    "in plain language, (3) likely contributing factors to discuss, and (4) a",
    "short suggested plan. Never invent clinical values, medications, or a",
    "patient name; refer to the patient by coded id only. Keep it under ~120",
    "words. Mark it clearly as a draft for clinician review."
  )
  usr <- paste0(
    "GROUNDED FALL CONTEXT (synthetic, authoritative):\n", ctx$text,
    "\n\nDraft the post-fall huddle note."
  )
  cdt_claude_reply(usr, context = NULL, system_prompt = sys, mock = mock,
    max_tokens = 350)
}
