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
