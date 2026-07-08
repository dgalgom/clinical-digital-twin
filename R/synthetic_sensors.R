#' Synthetic wearable sensor stream generation
#'
#' Produces daily-resolution (aggregated) streams per patient modelling the
#' clinical routine of a single wearable read-out each morning at 06:00 Europe
#' /Berlin (CET/CEST). Each daily record captures blood pressure, heart rate, and
#' accelerometry (plus derived activity/posture), with circadian structure,
#' noise, missingness (non-wear), and an elevated-risk "pre-fall signature" in
#' the days preceding a simulated fall. All data is synthetic.

#' Timezone used for the daily 06:00 routine read-out
#'
#' @return An Olson timezone string.
#' @export
cdt_sensor_timezone <- function() {
  "Europe/Berlin"
}

#' Build the 06:00-local daily timestamps for a run
#'
#' Timestamps are stored in ISO-8601 with an explicit offset so the CET/CEST
#' routine survives round-tripping through SQLite (which has no native tz type).
#'
#' @param start_date First date (Date).
#' @param days Number of consecutive daily read-outs.
#' @return Character vector of ISO-8601 timestamps at 06:00 local time.
#' @export
cdt_daily_timestamps <- function(start_date, days) {
  tz <- cdt_sensor_timezone()
  # 06:00 local on each date; as.POSIXct with tz applies the correct offset,
  # including DST transitions across the run.
  local_times <- as.POSIXct(
    paste0(format(start_date + seq_len(days) - 1), " 06:00:00"),
    tz = tz
  )
  format(local_times, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
}

#' Simulate one patient's daily sensor stream and fall events
#'
#' Baselines are perturbed by the patient's static risk profile. A latent daily
#' hazard drives simulated falls; in a configurable window before each fall the
#' activity/vitals show a deteriorating "pre-fall signature" (declining steps and
#' accelerometry, rising resting HR, more time lying down).
#'
#' @param patient A one-row data frame/tibble from [cdt_generate_cohort()].
#' @param days Number of daily read-outs to simulate.
#' @param start_date Date of the first reading (first 06:00 read-out).
#' @param missing_rate Fraction of daily readings dropped to mimic non-wear.
#' @return A list with `readings` (tibble) and `falls` (tibble).
#' @export
cdt_simulate_patient_sensors <- function(patient,
                                          days = 60,
                                          start_date = as.Date("2026-01-01"),
                                          missing_rate = 0.06) {
  stopifnot(nrow(patient) == 1)

  # Per-patient baseline shaped by static risk factors.
  frailty <- 0.4 * patient$parkinsons +
    0.3 * patient$osteoporosis +
    0.3 * patient$orthostatic_hypotension +
    0.2 * patient$polypharmacy +
    0.4 * patient$prior_falls +
    (patient$age - 74) / 30
  frailty <- as.numeric(frailty)

  base_steps <- max(600, 5200 - 1400 * frailty + stats::rnorm(1, 0, 400))
  base_resting_hr <- 62 + 6 * frailty + stats::rnorm(1, 0, 3)
  base_sbp <- 128 + 8 * patient$orthostatic_hypotension + stats::rnorm(1, 0, 6)
  base_dbp <- 78 + stats::rnorm(1, 0, 4)
  base_lying <- 8 + 1.5 * frailty            # hours/day lying down
  base_sitting <- 7 + 1.0 * frailty
  # Accelerometry baselines: activity counts scale with mobility; mean vector
  # magnitude (in g) sits near 1g at rest and rises with movement.
  base_accel_counts <- max(50, 320 - 90 * frailty + stats::rnorm(1, 0, 30))
  base_accel_mag <- 1.02 + 0.03 * (1 - frailty / 3) + stats::rnorm(1, 0, 0.01)

  dates <- start_date + seq_len(days) - 1

  # Latent daily fall hazard; higher for frail patients.
  daily_hazard <- plogis(-5.2 + 1.1 * frailty)
  fall_flags <- stats::rbinom(days, 1, daily_hazard)
  fall_idx <- which(fall_flags == 1)

  # Pre-fall deterioration multiplier (ramps up over the 5 days before a fall).
  signature <- rep(0, days)
  window <- 5
  for (fi in fall_idx) {
    lo <- max(1, fi - window)
    ramp <- seq(0, 1, length.out = fi - lo + 1)
    signature[lo:fi] <- pmax(signature[lo:fi], ramp)
  }

  # Circadian term expressed at daily resolution as mild day-to-day rhythm.
  circadian <- 0.05 * sin(2 * pi * seq_len(days) / 7)

  steps <- base_steps * (1 + circadian) * (1 - 0.45 * signature) +
    stats::rnorm(days, 0, 350)
  steps <- pmax(0, round(steps))

  resting_hr <- base_resting_hr * (1 + 0.12 * signature) +
    stats::rnorm(days, 0, 2.5)
  # Higher-frequency HR summary (mean over active periods).
  heart_rate <- resting_hr + 18 + 6 * (steps / max(steps + 1)) +
    stats::rnorm(days, 0, 4)

  sbp <- base_sbp + stats::rnorm(days, 0, 5) - 4 * circadian
  dbp <- base_dbp + stats::rnorm(days, 0, 3)

  hours_lying <- pmin(20, base_lying * (1 + 0.30 * signature) +
    stats::rnorm(days, 0, 0.8))
  hours_sitting <- pmin(20 - hours_lying, base_sitting +
    stats::rnorm(days, 0, 0.7))
  hours_standing <- pmax(0, 24 - hours_lying - hours_sitting)

  # Accelerometry: activity counts track steps and fall off with the pre-fall
  # signature; mean vector magnitude dips toward 1g as the patient moves less.
  accel_counts <- base_accel_counts * (1 + circadian) * (1 - 0.50 * signature) +
    0.02 * steps + stats::rnorm(days, 0, 20)
  accel_counts <- pmax(0, round(accel_counts))
  accel_magnitude <- base_accel_mag * (1 - 0.015 * signature) +
    stats::rnorm(days, 0, 0.008)

  readings <- tibble::tibble(
    patient_id = patient$patient_id,
    ts = cdt_daily_timestamps(start_date, days),
    heart_rate = round(heart_rate, 1),
    resting_hr = round(resting_hr, 1),
    sbp = round(sbp, 1),
    dbp = round(dbp, 1),
    step_count = as.integer(steps),
    accel_counts = as.integer(accel_counts),
    accel_magnitude = round(accel_magnitude, 3),
    hours_sitting = round(hours_sitting, 2),
    hours_lying = round(hours_lying, 2),
    hours_standing = round(hours_standing, 2)
  )

  # Inject missingness (device not worn): blank out sensor columns on some days.
  drop <- stats::runif(days) < missing_rate
  sensor_cols <- c(
    "heart_rate", "resting_hr", "sbp", "dbp", "step_count",
    "accel_counts", "accel_magnitude",
    "hours_sitting", "hours_lying", "hours_standing"
  )
  readings[drop, sensor_cols] <- NA

  falls <- tibble::tibble(
    patient_id = character(0), ts = character(0), severity = character(0)
  )
  if (length(fall_idx) > 0) {
    falls <- tibble::tibble(
      patient_id = patient$patient_id,
      ts = format(dates[fall_idx]),
      severity = sample(c("minor", "moderate", "severe"), length(fall_idx),
        replace = TRUE, prob = c(0.6, 0.3, 0.1)
      )
    )
  }

  list(readings = readings, falls = falls)
}

#' Simulate sensor streams for an entire cohort
#'
#' @param cohort A tibble from [cdt_generate_cohort()].
#' @param days Number of days to simulate per patient.
#' @param start_date First date.
#' @param seed RNG seed for reproducibility.
#' @return A list with combined `readings` and `falls` tibbles.
#' @export
cdt_simulate_cohort_sensors <- function(cohort,
                                        days = 60,
                                        start_date = as.Date("2026-01-01"),
                                        seed = 7) {
  set.seed(seed)
  reading_list <- vector("list", nrow(cohort))
  fall_list <- vector("list", nrow(cohort))
  for (i in seq_len(nrow(cohort))) {
    sim <- cdt_simulate_patient_sensors(
      cohort[i, , drop = FALSE],
      days = days, start_date = start_date
    )
    reading_list[[i]] <- sim$readings
    fall_list[[i]] <- sim$falls
  }
  list(
    readings = dplyr::bind_rows(reading_list),
    falls = dplyr::bind_rows(fall_list)
  )
}
