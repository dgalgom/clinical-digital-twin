#' Simulation validation gate (Phase 2)
#'
#' Five standalone, LLM-free validators that guard each stage of the daily
#' pipeline, plus an aggregator that records their outcomes into
#' `daily_checkpoint_log`. The biological validator deliberately CODIFIES the
#' implicit invariants already enforced inline by `synthetic_sensors.R` — it
#' invents no new clinical thresholds. In particular it does NOT cap heart rate
#' or blood pressure (the engine does not), so flu-elevated vitals pass (a large
#' day-to-day jump is a WARN, never a FAIL).
#'
#' Each validator returns a list `list(status, issues)` where `status` is one of
#' "pass", "warn", "fail" and `issues` is a character vector of messages.

# Allowed categorical values for the agent decision.
.cdt_agent_mood_levels <- function() {
  c("good", "ok", "tired", "low", "agitated", "anxious", "pain")
}

# The 9 required keys of an agent decision (mirrors the router-schema idiom).
.cdt_agent_required_keys <- function() {
  c(
    "patient_id", "day", "mobility_pct_of_baseline",
    "participated_group_activity", "medication_adherence",
    "meaningful_social_interaction", "mood_fatigue", "notable_event",
    "confidence"
  )
}

# Combine per-check results into a single status (fail > warn > pass).
.cdt_worst_status <- function(statuses) {
  if ("fail" %in% statuses) {
    return("fail")
  }
  if ("warn" %in% statuses) {
    return("warn")
  }
  "pass"
}

#' Validate a single agent decision object
#'
#' @param obj A parsed decision (named list) as produced by the agent path.
#' @return `list(status, issues)`.
#' @export
validate_agent_json <- function(obj) {
  issues <- character(0)
  if (!is.list(obj)) {
    return(list(status = "fail", issues = "decision is not a JSON object"))
  }
  missing <- setdiff(.cdt_agent_required_keys(), names(obj))
  if (length(missing) > 0) {
    issues <- c(issues, sprintf("missing keys: %s", paste(missing, collapse = ", ")))
    return(list(status = "fail", issues = issues))
  }

  mob <- suppressWarnings(as.numeric(obj$mobility_pct_of_baseline))
  if (is.na(mob) || mob < 0 || mob > 2) {
    issues <- c(issues, "mobility_pct_of_baseline must be numeric in [0, 2]")
  }

  conf <- suppressWarnings(as.numeric(obj$confidence))
  if (is.na(conf) || conf < 0 || conf > 1) {
    issues <- c(issues, "confidence must be numeric in [0, 1]")
  }

  # Binary participation/adherence/interaction flags: accept 0/1 or logical.
  for (k in c("participated_group_activity", "medication_adherence",
    "meaningful_social_interaction")) {
    v <- obj[[k]]
    ok <- (is.logical(v) && !is.na(v)) ||
      (length(v) == 1 && !is.na(suppressWarnings(as.integer(v))) &&
        as.integer(v) %in% c(0L, 1L))
    if (!ok) {
      issues <- c(issues, sprintf("%s must be 0/1 or logical", k))
    }
  }

  mood <- as.character(obj$mood_fatigue)[1]
  if (is.na(mood) || !mood %in% .cdt_agent_mood_levels()) {
    issues <- c(issues, sprintf("mood_fatigue '%s' not in allowed set", mood))
  }

  if (length(issues) > 0) {
    return(list(status = "fail", issues = issues))
  }
  list(status = "pass", issues = character(0))
}

#' Validate the day's social interactions
#'
#' @param df A data frame of interactions with an `initiated_by` and a
#'   `participants` list/JSON column.
#' @param valid_ids Character vector of valid patient ids.
#' @param max_per_day Maximum interactions allowed in a day.
#' @return `list(status, issues)`.
#' @export
validate_social_interactions <- function(df, valid_ids, max_per_day = 12L) {
  issues <- character(0)
  if (is.null(df) || nrow(df) == 0) {
    return(list(status = "pass", issues = character(0)))
  }
  if (nrow(df) > max_per_day) {
    issues <- c(issues, sprintf("%d interactions exceeds max %d",
      nrow(df), max_per_day))
  }
  for (i in seq_len(nrow(df))) {
    parts <- df$participants[[i]]
    if (is.character(parts) && length(parts) == 1 && grepl("^\\s*\\[", parts)) {
      parts <- tryCatch(jsonlite::fromJSON(parts), error = function(e) parts)
    }
    parts <- as.character(unlist(parts))
    bad <- setdiff(parts, valid_ids)
    if (length(bad) > 0) {
      issues <- c(issues, sprintf("row %d: unknown ids %s", i,
        paste(bad, collapse = ", ")))
    }
    if (length(unique(parts)) < length(parts)) {
      issues <- c(issues, sprintf("row %d: self/duplicate participant", i))
    }
    ib <- df$initiated_by[i]
    if (!is.na(ib) && !ib %in% parts) {
      issues <- c(issues, sprintf("row %d: initiator not among participants", i))
    }
  }
  if (length(issues) > 0) {
    return(list(status = "fail", issues = issues))
  }
  list(status = "pass", issues = character(0))
}

#' Validate biological plausibility of a day's sensor readings
#'
#' Codifies the invariants `synthetic_sensors.R` enforces inline: non-negative
#' steps/accelerometry, posture hours summing to 24 (standing is the residual),
#' `hours_lying <= 20`. Whole-row missingness (non-wear) is ALLOWED and surfaced
#' as a wear-time WARN, not an error. A large day-to-day change (e.g. a flu HR
#' bump) is a WARN so legitimate acute events survive the gate.
#'
#' @param readings A data frame of one or more daily readings.
#' @param prior A one-row data frame of the previous day's reading, or NULL, used
#'   only for the (soft) day-jump WARN.
#' @return `list(status, issues)`.
#' @export
validate_biological_plausibility <- function(readings, prior = NULL) {
  issues <- character(0)
  status <- "pass"
  if (is.null(readings) || nrow(readings) == 0) {
    return(list(status = "fail", issues = "no readings"))
  }

  for (i in seq_len(nrow(readings))) {
    r <- readings[i, , drop = FALSE]
    sensor_cols <- c("step_count", "accel_counts", "hours_sitting",
      "hours_lying", "hours_standing")
    all_na <- all(vapply(sensor_cols, function(c) is.na(r[[c]]), logical(1)))
    if (all_na) {
      # Non-wear day: allowed, but flag low wear-time.
      issues <- c(issues, sprintf("row %d: non-wear (all sensors NA)", i))
      status <- .cdt_worst_status(c(status, "warn"))
      next
    }

    # Hard invariants (FAIL) — these mirror pmax(0,)/pmin(20,) guards.
    if (!is.na(r$step_count) && r$step_count < 0) {
      issues <- c(issues, sprintf("row %d: negative step_count", i))
      status <- "fail"
    }
    if (!is.na(r$accel_counts) && r$accel_counts < 0) {
      issues <- c(issues, sprintf("row %d: negative accel_counts", i))
      status <- "fail"
    }
    if (!is.na(r$hours_lying) && r$hours_lying > 20) {
      issues <- c(issues, sprintf("row %d: hours_lying > 20", i))
      status <- "fail"
    }
    hrs <- c(r$hours_sitting, r$hours_lying, r$hours_standing)
    if (all(!is.na(hrs))) {
      if (any(hrs < 0)) {
        issues <- c(issues, sprintf("row %d: negative posture hours", i))
        status <- "fail"
      }
      if (abs(sum(hrs) - 24) > 0.05) {
        issues <- c(issues, sprintf("row %d: posture hours sum to %.2f (!=24)",
          i, sum(hrs)))
        status <- "fail"
      }
    }

    # Soft day-jump (WARN) — never a FAIL, so flu-elevated HR survives.
    if (!is.null(prior) && nrow(prior) == 1 &&
      !is.na(r$resting_hr) && !is.na(prior$resting_hr)) {
      if (abs(r$resting_hr - prior$resting_hr) > 20) {
        issues <- c(issues, sprintf("row %d: large resting_hr jump", i))
        status <- .cdt_worst_status(c(status, "warn"))
      }
    }
  }

  list(status = status, issues = issues)
}

#' Validate a model prediction row
#'
#' @param pred A named list / one-row data frame with `p_24h`, `p_7d`.
#' @param prior_p7d Optional previous-day 7d probability for a jump WARN.
#' @return `list(status, issues)`.
#' @export
validate_model_output <- function(pred, prior_p7d = NULL) {
  issues <- character(0)
  status <- "pass"
  p24 <- suppressWarnings(as.numeric(pred$p_24h))
  p7 <- suppressWarnings(as.numeric(pred$p_7d))

  for (nm in list(c("p_24h", p24), c("p_7d", p7))) {
    label <- nm[[1]]
    val <- as.numeric(nm[[2]])
    if (length(val) == 0 || is.na(val) || is.nan(val) || is.infinite(val)) {
      issues <- c(issues, sprintf("%s is NA/NaN/Inf", label))
      status <- "fail"
    } else if (val < 0 || val > 1) {
      issues <- c(issues, sprintf("%s outside [0,1]", label))
      status <- "fail"
    }
  }

  if (status != "fail") {
    if (p7 < p24) {
      issues <- c(issues, "p_7d < p_24h (unusual horizon ordering)")
      status <- .cdt_worst_status(c(status, "warn"))
    }
    if (!is.null(prior_p7d) && !is.na(prior_p7d)) {
      if (abs(p7 - as.numeric(prior_p7d)) > 0.5) {
        issues <- c(issues, "large unexplained p_7d jump")
        status <- .cdt_worst_status(c(status, "warn"))
      }
    }
  }

  list(status = status, issues = issues)
}

#' Aggregate per-step results into the daily checkpoint gate
#'
#' Logs one `daily_checkpoint_log` row per named result and returns the overall
#' gate status. The orchestrator proceeds on "pass"/"warn" and halts on "fail".
#'
#' @param con A DBI connection.
#' @param simulation_id,branch,day Run key.
#' @param results A named list mapping step label -> `list(status, issues)`.
#' @return `list(status, log_ids)` with the overall gate status.
#' @export
run_daily_checkpoint_gate <- function(con, simulation_id, branch, day, results) {
  stopifnot(is.list(results), length(results) > 0)
  log_ids <- integer(0)
  statuses <- character(0)
  for (step in names(results)) {
    res <- results[[step]]
    st <- res$status %||% "fail"
    detail <- if (length(res$issues) > 0) {
      paste(res$issues, collapse = "; ")
    } else {
      NA_character_
    }
    id <- cdt_sim_log_checkpoint(con, simulation_id, branch, day, step, st, detail)
    log_ids <- c(log_ids, id)
    statuses <- c(statuses, st)
  }
  list(status = .cdt_worst_status(statuses), log_ids = log_ids)
}
