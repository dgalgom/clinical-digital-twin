#' Service layer
#'
#' Convenience functions that combine the DB, feature engineering, and model to
#' answer the questions the UI/API/bot ask. Keeps those front-ends thin.

#' Compute current risk for one patient straight from the database
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param patient_id Patient identifier.
#' @param modified_inputs Optional counterfactual overrides.
#' @param include_baseline Passed to [predict_fall_risk()].
#' @return The prediction list, or `NULL` if the patient is unknown.
#' @export
cdt_patient_risk <- function(con, model, patient_id,
                             modified_inputs = NULL,
                             include_baseline = FALSE) {
  patient <- cdt_get_patient(con, patient_id)
  if (nrow(patient) == 0) {
    return(NULL)
  }
  readings <- cdt_get_patient_timeline(con, patient_id)
  fr <- cdt_assemble_features(patient, readings)
  predict_fall_risk(model, fr,
    modified_inputs = modified_inputs,
    include_baseline = include_baseline
  )
}

#' Cohort snapshot with current risk tiers
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @return A tibble: one row per patient with `p_7d`, `p_24h`, `tier_7d`.
#' @export
cdt_cohort_snapshot <- function(con, model) {
  cohort <- cdt_get_cohort(con)
  if (nrow(cohort) == 0) {
    return(cohort)
  }
  res <- lapply(cohort$patient_id, function(pid) {
    r <- cdt_patient_risk(con, model, pid)
    tibble::tibble(
      patient_id = pid,
      p_24h = r$p_24h, p_7d = r$p_7d,
      tier_24h = r$tier_24h, tier_7d = r$tier_7d
    )
  })
  risk <- dplyr::bind_rows(res)
  dplyr::arrange(dplyr::left_join(cohort, risk, by = "patient_id"), dplyr::desc(p_7d))
}

#' Assemble a grounded text summary of a patient for the LLM/bot
#'
#' Injects real (synthetic) data and model outputs so the LLM has facts to
#' ground on and is instructed not to invent clinical details.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param patient_id Patient identifier.
#' @param modified_inputs Optional counterfactual overrides for a what-if query.
#' @return A character scalar summary, or `NULL` if patient unknown.
#' @export
cdt_patient_context <- function(con, model, patient_id, modified_inputs = NULL) {
  patient <- cdt_get_patient(con, patient_id)
  if (nrow(patient) == 0) {
    return(NULL)
  }
  readings <- cdt_get_patient_timeline(con, patient_id)
  feats <- cdt_assemble_features(patient, readings)
  risk <- predict_fall_risk(model, feats,
    modified_inputs = modified_inputs, include_baseline = TRUE
  )
  imp <- utils::head(cdt_feature_importance(model, "7d"), 5)

  lines <- c(
    sprintf("Patient %s (%s), age %d, sex %s.",
      patient$patient_id, patient$name, patient$age, patient$sex),
    sprintf("Risk factors: parkinsons=%d, osteoporosis=%d, orthostatic_hypotension=%d, polypharmacy=%d, prior_falls=%d, n_medications=%d.",
      patient$parkinsons, patient$osteoporosis, patient$orthostatic_hypotension,
      patient$polypharmacy, patient$prior_falls, patient$n_medications),
    sprintf("Recent 7d sensor summary: steps_mean=%.0f (trend/day=%.1f), resting_hr_mean=%.1f (trend/day=%.2f), sbp_mean=%.1f, sedentary_hours_mean=%.1f.",
      feats$steps_mean_7d, feats$steps_trend_7d, feats$resting_hr_mean_7d,
      feats$resting_hr_trend_7d, feats$sbp_mean_7d, feats$sedentary_hours_mean_7d),
    sprintf("Baseline fall risk: 24h=%.1f%% (%s), 7d=%.1f%% (%s).",
      100 * risk$baseline$p_24h, risk$baseline$tier_24h,
      100 * risk$baseline$p_7d, risk$baseline$tier_7d)
  )
  if (!is.null(modified_inputs)) {
    lines <- c(lines, sprintf(
      "Simulated (what-if %s): 24h=%.1f%% (%s), 7d=%.1f%% (%s); 7d delta=%+.1f pts.",
      jsonlite::toJSON(modified_inputs, auto_unbox = TRUE),
      100 * risk$p_24h, risk$tier_24h, 100 * risk$p_7d, risk$tier_7d,
      100 * risk$delta$p_7d
    ))
  }
  lines <- c(lines, sprintf(
    "Top model drivers (7d, standardized coef): %s.",
    paste(sprintf("%s=%+.2f", imp$feature, imp$coefficient), collapse = ", ")
  ))
  paste(lines, collapse = "\n")
}

#' Source all package R files (helper for scripts that don't install the pkg)
#'
#' @param root Project root (default [cdt_project_root()]).
#' @return Invisibly `TRUE`.
#' @export
cdt_source_all <- function(root = cdt_project_root()) {
  files <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
  for (f in files) sys.source(f, envir = globalenv())
  invisible(TRUE)
}
