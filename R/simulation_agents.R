#' Agent LLM path + mock mode (Phase 4)
#'
#' Each resident is an agent that, once per simulation day, emits a compact JSON
#' behavioural decision (never raw sensors). This module builds the prompt,
#' obtains a reply (live via [cdt_claude_reply()] or deterministic mock), extracts
#' and validates the JSON (reusing `.cdt_extract_json` and `validate_agent_json`),
#' and on invalid output retries once at a lower temperature before falling back
#' to the prior day's decision (flagged `agent_output_invalid`). Mock mode emits
#' REAL JSON so mock and live share one parse/validate path.

# A stable marker the mock detects to know it should emit an agent decision
# (rather than a clinical summary). Embedded verbatim in every agent prompt.
.cdt_agent_marker <- function() "<<CDT_AGENT_DECISION>>"

#' The JSON contract an agent must emit (mirrors the router-schema idiom)
#'
#' @return A single character block describing the required JSON.
#' @export
.cdt_agent_schema_text <- function() {
  paste0(
    "Return ONLY a compact JSON object with these keys:\n",
    '  "patient_id": the patient id (e.g. "P08")\n',
    '  "day": the integer simulation day\n',
    '  "mobility_pct_of_baseline": number in [0,2], 1 = your usual activity\n',
    '  "participated_group_activity": 0 or 1\n',
    '  "medication_adherence": 0 or 1\n',
    '  "meaningful_social_interaction": 0 or 1\n',
    '  "mood_fatigue": one of ["good","ok","tired","low","agitated","anxious","pain"]\n',
    '  "notable_event": a short string or null\n',
    '  "confidence": number in [0,1]\n',
    "No prose, no code fences, JSON only."
  )
}

#' Build the per-day agent prompt (persisted verbatim)
#'
#' @param patient A one-row tibble (canonical cols + `system_prompt`).
#' @param day Integer simulation day.
#' @param ctx A named list of grounded day context (institution, weekend, flu,
#'   social summary, prior mood, etc.).
#' @return A character scalar prompt.
#' @export
cdt_agent_prompt <- function(patient, day, ctx = list()) {
  persona <- patient$system_prompt %||% ""
  bits <- c(
    .cdt_agent_marker(),
    persona,
    "",
    sprintf("Today is simulation day %d.", as.integer(day)),
    if (!is.null(ctx$weekend) && isTRUE(ctx$weekend)) "It is the weekend (fewer staff, no group activity)." else NULL,
    if (!is.null(ctx$physio_today) && isTRUE(ctx$physio_today)) "The physiotherapist is in today." else NULL,
    if (!is.null(ctx$flu_active) && isTRUE(ctx$flu_active)) "A flu outbreak is affecting the module (reduced staffing)." else NULL,
    if (!is.null(ctx$social_summary)) sprintf("Social note: %s", ctx$social_summary) else NULL,
    if (!is.null(ctx$prior_mood)) sprintf("Yesterday your mood was: %s.", ctx$prior_mood) else NULL,
    "",
    "Decide today's behaviour as YOUR character would, honestly reflecting your",
    "condition and the day's context.",
    "",
    .cdt_agent_schema_text()
  )
  paste(bits[!vapply(bits, is.null, logical(1))], collapse = "\n")
}

# Deterministic mock decision derived from the patient + day, emitted as JSON so
# it exercises the same extract/validate path as a live reply. Reproducible.
.cdt_mock_agent_json <- function(patient, day, ctx = list()) {
  # Seed off patient id + day so the same (patient, day) is stable.
  pid_num <- suppressWarnings(as.integer(sub("^P", "", patient$patient_id)))
  if (is.na(pid_num)) pid_num <- 1L
  set.seed(1000L * pid_num + as.integer(day))

  frailty <- .cdt_sim_frailty(patient)
  # More frail -> lower baseline mobility; add small daily jitter.
  mob <- max(0, min(2, 1.0 - 0.10 * frailty + stats::rnorm(1, 0, 0.08)))
  weekend <- isTRUE(ctx$weekend)
  group <- if (weekend) 0L else as.integer(stats::rbinom(1, 1, 0.7))
  if (isTRUE(ctx$flu_active)) mob <- mob * 0.7

  moods <- c("good", "ok", "tired", "low")
  mood <- moods[[1 + (pid_num + as.integer(day)) %% length(moods)]]

  obj <- list(
    patient_id = patient$patient_id,
    day = as.integer(day),
    mobility_pct_of_baseline = round(mob, 3),
    participated_group_activity = group,
    medication_adherence = as.integer(stats::rbinom(1, 1, 0.95)),
    meaningful_social_interaction = as.integer(stats::rbinom(1, 1, 0.6)),
    mood_fatigue = mood,
    notable_event = NULL,
    confidence = round(0.7 + stats::runif(1, 0, 0.25), 3)
  )
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")
}

#' Call one agent for one day, returning a validated decision
#'
#' @param patient A one-row tibble (canonical cols + `system_prompt`).
#' @param day Integer simulation day.
#' @param ctx Grounded day context (see [cdt_agent_prompt()]).
#' @param prior_decision The previous day's decision (named list) or NULL, reused
#'   if this day's output is invalid after a retry.
#' @param mock Optional explicit mock override.
#' @param temperature Sampling temperature for the first attempt.
#' @return A list with `decision` (named list), `invalid` (logical), `prompt`,
#'   `raw`, and `temperature`.
#' @export
cdt_call_agent <- function(patient, day, ctx = list(), prior_decision = NULL,
                           mock = NULL, temperature = 0.7) {
  prompt <- cdt_agent_prompt(patient, day, ctx)

  attempt <- function(temp) {
    if (cdt_llm_is_mock(mock)) {
      raw <- .cdt_mock_agent_json(patient, day, ctx)
    } else {
      raw <- cdt_claude_reply(prompt, context = NULL,
        system_prompt = "You are a resident-simulation agent. Output JSON only.",
        mock = mock, max_tokens = 300, temperature = temp)
    }
    parsed <- .cdt_extract_json(raw)
    list(raw = raw, parsed = parsed, valid = validate_agent_json(parsed)$status != "fail")
  }

  first <- attempt(temperature)
  if (first$valid) {
    return(list(decision = first$parsed, invalid = FALSE, prompt = prompt,
      raw = first$raw, temperature = temperature))
  }

  # Retry once at a lower temperature.
  retry_temp <- max(0, temperature - 0.4)
  second <- attempt(retry_temp)
  if (second$valid) {
    return(list(decision = second$parsed, invalid = FALSE, prompt = prompt,
      raw = second$raw, temperature = retry_temp))
  }

  # Fall back to the prior day's decision; flag as invalid for the audit trail.
  fallback <- prior_decision
  if (is.null(fallback)) {
    # No prior: synthesise a conservative neutral decision so the day proceeds.
    fallback <- list(
      patient_id = patient$patient_id, day = as.integer(day),
      mobility_pct_of_baseline = 1.0, participated_group_activity = 0L,
      medication_adherence = 1L, meaningful_social_interaction = 0L,
      mood_fatigue = "ok", notable_event = NA_character_, confidence = 0.3
    )
  }
  fallback$day <- as.integer(day)
  list(decision = fallback, invalid = TRUE, prompt = prompt,
    raw = second$raw, temperature = retry_temp)
}

#' Generate the day's social interactions from the affinity matrix
#'
#' Deterministic weighted sampling of 2-4 interactions from the affinity matrix
#' (respecting the P09 receive-only rule via the zero row). One light LLM/template
#' call summarises each interaction. Returns interaction rows plus a per-patient
#' social-context lookup.
#'
#' @param day Integer simulation day.
#' @param affinity A 10x10 affinity matrix ([cdt_sim_affinity_matrix()]).
#' @param institution_ctx The institution profile.
#' @param mock Optional explicit mock override.
#' @param seed Optional integer for reproducible sampling.
#' @return A list with `rows` (data frame) and `context` (named list keyed by
#'   patient id).
#' @export
cdt_sim_social_day <- function(day, affinity, institution_ctx, mock = NULL,
                               seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ids <- rownames(affinity)
  # Candidate initiator-target pairs weighted by affinity (row = initiator, so
  # P09's zero row means it never initiates).
  pairs <- list()
  weights <- numeric(0)
  for (a in ids) {
    for (b in ids) {
      w <- affinity[a, b]
      if (a != b && w > 0) {
        pairs[[length(pairs) + 1]] <- c(a, b)
        weights <- c(weights, w)
      }
    }
  }
  n_int <- if (length(pairs) == 0) 0L else sample(2:4, 1)
  rows <- data.frame(
    day = integer(0), participants = character(0),
    interaction_type = character(0), initiated_by = character(0),
    summary_text = character(0), stringsAsFactors = FALSE
  )
  context <- stats::setNames(vector("list", length(ids)), ids)

  if (n_int > 0) {
    idx <- sample(seq_along(pairs), size = min(n_int, length(pairs)),
      prob = weights / sum(weights), replace = FALSE)
    for (i in idx) {
      p <- pairs[[i]]
      summary_text <- if (cdt_llm_is_mock(mock)) {
        sprintf("[MOCK] %s and %s spent time together on day %d.", p[1], p[2], day)
      } else {
        cdt_claude_reply(
          sprintf("Summarise in one sentence a brief social interaction between residents %s and %s.",
            p[1], p[2]),
          context = NULL, mock = mock, max_tokens = 60)
      }
      rows <- rbind(rows, data.frame(
        day = as.integer(day),
        participants = jsonlite::toJSON(p, auto_unbox = FALSE),
        interaction_type = "shared_activity",
        initiated_by = p[1],
        summary_text = summary_text,
        stringsAsFactors = FALSE
      ))
      for (pp in p) {
        context[[pp]] <- c(context[[pp]] %||% character(0), summary_text)
      }
    }
  }
  list(rows = rows, context = context)
}
