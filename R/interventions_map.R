#' Driver -> intervention mapping (P0-2)
#'
#' Maps each model driver (a feature name from [cdt_model_features()]) onto a
#' small menu of evidence-based, institution-appropriate fall-prevention
#' interventions. This turns a bare coefficient/probability into an actionable
#' care suggestion (see docs/p0_checkpoints.md, CP-2).
#'
#' Design notes:
#' * Pure R data (no YAML dependency, no `system.file` path resolution): the map
#'   is sourced like every other `R/` module and works offline in mock mode.
#' * Shape borrowed from the sibling human-digital-twin `RecommendedAction`
#'   contract: each entry carries `interventions`, an `urgency` band, and a short
#'   `evidence_note`. Values are curated here, never hardcoded at the call site.
#' * Content is ILLUSTRATIVE decision-support for a synthetic-data demo, not
#'   validated clinical guidance. The UI/bot surface this caveat.
#'
#' Every name in [cdt_model_features()] MUST have an entry so the patient view
#' never shows a driver without a suggested action (asserted in tests).

#' The curated driver -> intervention map
#'
#' @return A named list; each element is
#'   `list(label, interventions = character(), urgency, evidence_note)`.
#'   `urgency` is one of "routine", "prompt", "urgent".
#' @export
cdt_interventions_map <- function() {
  list(
    # --- Static clinical factors --------------------------------------------
    age = list(
      label = "Advanced age",
      interventions = c(
        "Reinforce universal fall precautions (footwear, lighting, clear paths)",
        "Ensure regular multidisciplinary review cadence"
      ),
      urgency = "routine",
      evidence_note = "Age is a non-modifiable risk marker; act on the modifiable drivers it accompanies."
    ),
    sex_male = list(
      label = "Sex",
      interventions = c(
        "No sex-specific action; address the concrete modifiable drivers below"
      ),
      urgency = "routine",
      evidence_note = "Sex is non-modifiable; retained only as a risk-adjustment covariate."
    ),
    parkinsons = list(
      label = "Parkinson's disease",
      interventions = c(
        "Physiotherapy referral for gait/balance training",
        "Review anti-parkinsonian dosing timing vs. mobility windows",
        "Assess for freezing-of-gait and orthostatic symptoms"
      ),
      urgency = "prompt",
      evidence_note = "Parkinsonian gait and postural instability are strong institutional fall drivers."
    ),
    osteoporosis = list(
      label = "Osteoporosis",
      interventions = c(
        "Hip-protector and injury-mitigation review (falls are higher-consequence)",
        "Verify calcium/vitamin-D and bone-protection therapy",
        "Environmental de-hazarding to reduce impact risk"
      ),
      urgency = "routine",
      evidence_note = "Raises injury severity of any fall; prioritise consequence reduction."
    ),
    orthostatic_hypotension = list(
      label = "Orthostatic hypotension",
      interventions = c(
        "Structured antihypertensive/diuretic medication review",
        "Sit-to-stand protocol: slow transfers, dangle before standing",
        "Lying/standing BP measurement and hydration review"
      ),
      urgency = "prompt",
      evidence_note = "Postural BP drop is a classic, highly modifiable fall mechanism."
    ),
    polypharmacy = list(
      label = "Polypharmacy",
      interventions = c(
        "Structured medication review / deprescribing (focus on FRIDs)",
        "Flag benzodiazepines, antipsychotics, opioids, diuretics for review",
        "Pharmacist-led reconciliation"
      ),
      urgency = "prompt",
      evidence_note = "Fall-Risk-Increasing Drugs are the most modifiable lever; deprescribing lowers risk."
    ),
    prior_falls = list(
      label = "Prior falls",
      interventions = c(
        "Complete a post-fall huddle for the most recent event",
        "Increase observation/rounding frequency",
        "Review bed-exit and toileting precautions"
      ),
      urgency = "urgent",
      evidence_note = "A recent fall is the single strongest predictor of the next fall."
    ),
    n_medications = list(
      label = "Number of medications",
      interventions = c(
        "Medication count reduction via review of overlapping/low-value agents",
        "Simplify regimen and administration schedule"
      ),
      urgency = "routine",
      evidence_note = "Higher medication counts track polypharmacy risk; overlaps with FRID review."
    ),

    # --- Sensor-derived drivers ---------------------------------------------
    steps_mean_7d = list(
      label = "Low daily activity (steps)",
      interventions = c(
        "Physiotherapy / mobility referral and progressive activity program",
        "Purposeful rounding to encourage safe ambulation",
        "Set an individualised daily mobility goal"
      ),
      urgency = "prompt",
      evidence_note = "Declining ambulation signals deconditioning and rising fall risk."
    ),
    steps_trend_7d = list(
      label = "Declining activity trend",
      interventions = c(
        "Investigate the cause of the recent decline (illness, pain, mood)",
        "Early physiotherapy review before further deconditioning"
      ),
      urgency = "prompt",
      evidence_note = "A downward step trend often precedes falls in the pre-fall signature."
    ),
    resting_hr_mean_7d = list(
      label = "Elevated resting heart rate",
      interventions = c(
        "Clinical review for infection, dehydration, or cardiac cause",
        "Check medications and recent vitals trend"
      ),
      urgency = "prompt",
      evidence_note = "Rising resting HR can indicate acute illness that elevates short-term fall risk."
    ),
    resting_hr_trend_7d = list(
      label = "Rising resting-HR trend",
      interventions = c(
        "Trend review with nursing for early deterioration",
        "Escalate to clinician if sustained rise"
      ),
      urgency = "prompt",
      evidence_note = "An upward resting-HR trend is part of the modelled pre-fall deterioration."
    ),
    sbp_mean_7d = list(
      label = "Systolic blood pressure",
      interventions = c(
        "BP medication review (over- or under-treatment)",
        "Correlate with orthostatic symptoms and transfers"
      ),
      urgency = "routine",
      evidence_note = "Both hypotension and lability contribute to falls; review in context."
    ),
    sedentary_hours_mean_7d = list(
      label = "High sedentary time",
      interventions = c(
        "Scheduled activity / anti-sedentary program",
        "Chair-based exercises and regular repositioning",
        "Toileting schedule to prompt safe movement"
      ),
      urgency = "routine",
      evidence_note = "Prolonged sitting/lying accelerates deconditioning and pressure risk."
    ),
    sedentary_hours_trend_7d = list(
      label = "Rising sedentary trend",
      interventions = c(
        "Investigate new immobility (pain, illness, low mood)",
        "Physiotherapy review to arrest the decline"
      ),
      urgency = "prompt",
      evidence_note = "Increasing sedentary time frequently precedes a fall."
    ),
    hr_variability_7d = list(
      label = "Heart-rate variability",
      interventions = c(
        "Clinical review if paired with other deterioration signals",
        "Monitor alongside resting HR and activity"
      ),
      urgency = "routine",
      evidence_note = "Autonomic variability is a soft signal; act when corroborated."
    ),
    accel_magnitude_mean_7d = list(
      label = "Low movement intensity",
      interventions = c(
        "Mobility/physiotherapy assessment of movement quality",
        "Encourage graded increases in purposeful activity"
      ),
      urgency = "routine",
      evidence_note = "Reduced accelerometry magnitude reflects less/weaker movement (posture, frailty)."
    )
  )
}

#' Suggested interventions for a patient's top model drivers
#'
#' Joins the patient's top drivers (by standardized-coefficient magnitude, from
#' [cdt_feature_importance()]) to the curated [cdt_interventions_map()]. The
#' importances are cohort-level model drivers (not patient-specific
#' attributions), consistent with how `/drivers` and the dashboard already frame
#' them.
#'
#' @param model A `cdt_model`.
#' @param top_n Number of top drivers to return suggestions for (default 3).
#' @return A tibble with `feature`, `label`, `coefficient`, `direction`,
#'   `urgency`, `evidence_note`, and `interventions` (a list-column of character
#'   vectors). Empty tibble if the model has no usable coefficients.
#' @export
cdt_driver_interventions <- function(model, top_n = 3L) {
  imp <- cdt_feature_importance(model)
  map <- cdt_interventions_map()
  if (nrow(imp) == 0) {
    return(tibble::tibble(
      feature = character(), label = character(), coefficient = numeric(),
      direction = character(), urgency = character(),
      evidence_note = character(), interventions = list()
    ))
  }
  top <- utils::head(imp, top_n)
  rows <- lapply(seq_len(nrow(top)), function(i) {
    f <- top$feature[i]
    entry <- map[[f]]
    if (is.null(entry)) {
      # Should never happen (coverage is asserted in tests); degrade gracefully.
      entry <- list(
        label = f, interventions = "Clinical review",
        urgency = "routine", evidence_note = ""
      )
    }
    tibble::tibble(
      feature = f,
      label = entry$label,
      coefficient = top$coefficient[i],
      direction = if (top$coefficient[i] >= 0) "increases risk" else "decreases risk",
      urgency = entry$urgency,
      evidence_note = entry$evidence_note,
      interventions = list(entry$interventions)
    )
  })
  dplyr::bind_rows(rows)
}
