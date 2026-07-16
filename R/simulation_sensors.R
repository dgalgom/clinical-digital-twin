#' Sensor modulation from agent decisions (Phase 3)
#'
#' Deterministic (no-LLM) bridge that turns one day's agent behavioural decision
#' into one day of sensor readings. It reuses the baseline SHAPE of
#' `cdt_simulate_patient_sensors` (frailty-driven baselines) but emits a single
#' day, scaled by the decision's `mobility_pct_of_baseline` and nudged by
#' institution/flu context. Crucially it PRESERVES the same guards the engine
#' uses inline (`pmax(0, .)`, `pmin(20, .)`, standing = residual to 24) so the
#' output always passes `validate_biological_plausibility`.

# Frailty score, identical formula to cdt_simulate_patient_sensors, so the two
# stay consistent. Expects a one-row patient with the canonical clinical cols.
.cdt_sim_frailty <- function(patient) {
  as.numeric(
    0.4 * patient$parkinsons +
      0.3 * patient$osteoporosis +
      0.3 * patient$orthostatic_hypotension +
      0.2 * patient$polypharmacy +
      0.4 * patient$prior_falls +
      (patient$age - 74) / 30
  )
}

#' Generate one day of sensor readings from an agent decision
#'
#' @param patient A one-row tibble in the canonical clinical schema.
#' @param decision A validated agent decision (named list) for this day.
#' @param institution_ctx The institution profile ([cdt_sim_institution()]).
#' @param day Integer simulation day (1-based).
#' @param start_date Date of day 1.
#' @param flu_ctx Optional list from [cdt_sim_flu_config()] describing an active
#'   outbreak, or NULL. Applied only if this patient is affected and the day is
#'   within the outbreak window (the caller decides; if supplied we honour it).
#' @param seed Optional integer for reproducible per-day noise.
#' @param simulation_id,branch Run key, stamped onto the row.
#' @return A one-row tibble matching the `sensor_readings` sim columns.
#' @export
cdt_sim_day_sensors <- function(patient, decision, institution_ctx, day,
                                start_date = cdt_data_start_date(),
                                flu_ctx = NULL, seed = NULL,
                                simulation_id = NA_character_,
                                branch = NA_character_) {
  stopifnot(nrow(patient) == 1)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  frailty <- .cdt_sim_frailty(patient)
  mob <- as.numeric(decision$mobility_pct_of_baseline %||% 1)
  mob <- max(0, min(2, mob)) # defensive clamp; validator already enforces

  # Baseline shape (mirrors the engine, minus the pre-baked pre-fall signature -
  # here deterioration comes from the agent's lowered mobility instead).
  base_steps <- max(600, 5200 - 1400 * frailty)
  base_resting_hr <- 62 + 6 * frailty
  base_sbp <- 128 + 8 * patient$orthostatic_hypotension
  base_dbp <- 78
  base_lying <- 8 + 1.5 * frailty
  base_sitting <- 7 + 1.0 * frailty
  base_accel_counts <- max(50, 320 - 90 * frailty)
  base_accel_mag <- 1.02 + 0.03 * (1 - frailty / 3)

  # Mild weekly rhythm at daily resolution (same term as the engine).
  circadian <- 0.05 * sin(2 * pi * day / 7)

  # Institution effects: on low-staffing days / no group activity, activity dips.
  staff_penalty <- 1
  if (isFALSE(decision$participated_group_activity %||% FALSE) ||
    identical(as.integer(decision$participated_group_activity %||% 0L), 0L)) {
    staff_penalty <- staff_penalty * 0.95
  }

  # Flu effects (only if the caller passes an active flu_ctx for this patient).
  hr_bump <- 0
  flu_mult <- 1
  if (!is.null(flu_ctx)) {
    hr_bump <- as.numeric(flu_ctx$resting_hr_bump %||% 0)
    flu_mult <- as.numeric(flu_ctx$mobility_multiplier %||% 1)
  }

  activity_scale <- mob * staff_penalty * flu_mult * (1 + circadian)

  steps <- pmax(0, round(base_steps * activity_scale +
    stats::rnorm(1, 0, 120)))
  accel_counts <- pmax(0, round(base_accel_counts * activity_scale +
    0.02 * steps + stats::rnorm(1, 0, 15)))
  accel_magnitude <- round(base_accel_mag - 0.01 * (1 - mob) +
    stats::rnorm(1, 0, 0.006), 3)

  resting_hr <- round(base_resting_hr + hr_bump +
    0.10 * base_resting_hr * (1 - mob) + stats::rnorm(1, 0, 1.5), 1)
  heart_rate <- round(resting_hr + 18 + 6 * (steps / (steps + 1)) +
    stats::rnorm(1, 0, 3), 1)
  sbp <- round(base_sbp + stats::rnorm(1, 0, 4) - 4 * circadian, 1)
  dbp <- round(base_dbp + stats::rnorm(1, 0, 3), 1)

  # Posture: less mobility -> more lying/sitting. Preserve the engine's guards
  # so the three hours always sum to 24 and lying is capped at 20.
  lying_scale <- 1 + 0.30 * (1 - mob)
  hours_lying <- pmin(20, base_lying * lying_scale + stats::rnorm(1, 0, 0.5))
  hours_sitting <- pmin(20 - hours_lying, base_sitting + stats::rnorm(1, 0, 0.4))
  hours_standing <- pmax(0, 24 - hours_lying - hours_sitting)

  tibble::tibble(
    patient_id = patient$patient_id,
    ts = cdt_daily_timestamps(start_date + day - 1, 1),
    heart_rate = heart_rate,
    resting_hr = resting_hr,
    sbp = sbp,
    dbp = dbp,
    step_count = as.integer(steps),
    accel_counts = as.integer(accel_counts),
    accel_magnitude = accel_magnitude,
    hours_sitting = round(hours_sitting, 2),
    hours_lying = round(hours_lying, 2),
    hours_standing = round(hours_standing, 2),
    simulation_id = simulation_id,
    branch = branch,
    day = as.integer(day),
    quality_flags = NA_character_
  )
}
