#' Project configuration and shared constants
#'
#' Central place for file paths, the canonical schema definition, and risk-tier
#' cutoffs. Keeping these in one module avoids magic strings scattered across
#' the codebase.
#'
#' @keywords internal
"_PACKAGE"

#' Locate the project root
#'
#' Resolves paths relative to this file so the code runs the same whether it is
#' sourced from the Shiny app, the plumber API, or a test.
#'
#' @return Absolute path to the project root directory.
#' @export
cdt_project_root <- function() {
  # When sourced normally, R offers no reliable __file__; fall back to the
  # CDT_PROJECT_ROOT env var, then the working directory.
  root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
  if (!is.na(root) && nzchar(root)) {
    return(normalizePath(root, mustWork = FALSE))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

#' Default SQLite database path
#'
#' @return Path to the SQLite database file.
#' @export
cdt_db_path <- function() {
  override <- Sys.getenv("CDT_DB_PATH", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    return(override)
  }
  file.path(cdt_project_root(), "data", "clinical_twin.sqlite")
}

#' Default trained-model path
#'
#' @return Path to the persisted model object (.rds).
#' @export
cdt_model_path <- function() {
  override <- Sys.getenv("CDT_MODEL_PATH", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    return(override)
  }
  file.path(cdt_project_root(), "data", "fall_risk_model.rds")
}

#' First date of the synthetic sensor timeline
#'
#' The demo cohort's daily read-outs begin on this date. Fixed so the timeline
#' is reproducible; the *end* of the timeline tracks "today" (see
#' [cdt_data_end_date()]) so that relative queries like "the previous two
#' months" land on real data.
#'
#' @return A `Date`.
#' @export
cdt_data_start_date <- function() {
  as.Date("2026-01-01")
}

#' Last date of the synthetic sensor timeline ("today")
#'
#' Defaults to the current system date so the ingested-daily timeline always
#' ends at "today", making real-clock-anchored relative windows resolve onto
#' real rows. Override with `CDT_DATA_END_DATE=YYYY-MM-DD` for a fully
#' reproducible build (e.g. in CI or a frozen release).
#'
#' @return A `Date`.
#' @export
cdt_data_end_date <- function() {
  override <- Sys.getenv("CDT_DATA_END_DATE", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    d <- tryCatch(as.Date(override), error = function(e) NA)
    if (!is.na(d)) {
      return(d)
    }
  }
  Sys.Date()
}

#' Number of daily read-outs spanning the synthetic timeline
#'
#' The inclusive day count from [cdt_data_start_date()] to [cdt_data_end_date()].
#' Guarded to a sensible minimum so a misconfigured end date cannot produce an
#' empty or degenerate series.
#'
#' @param min_days Lower bound on the span (default 90, the original sizing).
#' @return An integer number of days.
#' @export
cdt_data_days <- function(min_days = 90L) {
  span <- as.integer(cdt_data_end_date() - cdt_data_start_date()) + 1L
  max(as.integer(min_days), span)
}

#' Fall-risk tier cutoffs
#'
#' Probabilities at or above `high` are "High", at or above `moderate` are
#' "Moderate", otherwise "Low". Tuned for the synthetic cohort; documented as an
#' MVP heuristic, not a validated clinical threshold.
#'
#' @return Named numeric vector of cutoffs.
#' @export
cdt_risk_cutoffs <- function() {
  c(moderate = 0.15, high = 0.35)
}

#' Shift-triage alert configuration (P0-1)
#'
#' Change-detection thresholds for [cdt_compute_alerts()]. Modelled on the
#' human-digital-twin `AlarmDetector` shape: a threshold, a direction, and a
#' severity mapping, all data-driven rather than hardcoded in the detector.
#'
#' * `jump_pts` - minimum rise in 7-day risk (percentage points) to fire a
#'   "risk_jump" alert.
#' * `warning_pts` / `critical_pts` - severity cut-offs on the absolute jump.
#' * `tier_up_severity` - severity assigned when a patient crosses up a risk
#'   tier (Low->Moderate->High), independent of the point jump.
#'
#' @return A named list of alert-configuration values.
#' @export
cdt_alert_config <- function() {
  list(
    jump_pts         = 8,
    warning_pts      = 8,
    critical_pts     = 15,
    tier_up_severity = "warning"
  )
}

#' Map a probability to a risk tier label
#'
#' @param p Numeric vector of probabilities in `[0, 1]`.
#' @return Character vector of tier labels ("Low", "Moderate", "High").
#' @export
cdt_risk_tier <- function(p) {
  cuts <- cdt_risk_cutoffs()
  out <- ifelse(p >= cuts[["high"]], "High",
    ifelse(p >= cuts[["moderate"]], "Moderate", "Low")
  )
  factor(out, levels = c("Low", "Moderate", "High"))
}

#' Canonical clinical feature names used by the model
#'
#' @return Character vector of static (clinical) predictor names.
#' @export
cdt_static_features <- function() {
  c(
    "age", "sex_male", "parkinsons", "osteoporosis",
    "orthostatic_hypotension", "polypharmacy", "prior_falls",
    "n_medications"
  )
}

#' Canonical sensor-derived feature names used by the model
#'
#' Accelerometry is ingested and stored in full (counts + magnitude), but only
#' the near-orthogonal `accel_magnitude_mean_7d` enters the model. The
#' accelerometry *count* summaries (`accel_counts_mean_7d`,
#' `accel_counts_trend_7d`) are ~0.8 collinear with the step summaries and were
#' excluded from the design: feeding both near-duplicate activity measures into
#' the ridge splits the shared signal and flips weaker coefficient signs
#' (destabilizing interpretability). Steps are kept because they are the
#' clinically actionable what-if lever; magnitude adds an independent posture/
#' movement-intensity signal.
#'
#' @return Character vector of engineered sensor predictor names.
#' @export
cdt_sensor_features <- function() {
  c(
    "steps_mean_7d", "steps_trend_7d", "resting_hr_mean_7d",
    "resting_hr_trend_7d", "sbp_mean_7d", "sedentary_hours_mean_7d",
    "sedentary_hours_trend_7d", "hr_variability_7d",
    "accel_magnitude_mean_7d"
  )
}
