#' Feature engineering
#'
#' Turns raw sensor time series + static clinical data into the model's feature
#' vector. Used identically at training time and inference time so the "digital
#' twin" counterfactuals stay consistent with the fitted model.

#' Safe trend (slope per day) of a numeric series
#'
#' Fits a simple linear regression of value ~ day index; returns 0 when there is
#' insufficient non-missing data.
#'
#' @param x Numeric vector ordered by time.
#' @return Slope per step (numeric scalar).
#' @keywords internal
.cdt_trend <- function(x) {
  ok <- which(!is.na(x))
  if (length(ok) < 3) {
    return(0)
  }
  t <- seq_along(x)[ok]
  v <- x[ok]
  fit <- stats::lm(v ~ t)
  as.numeric(stats::coef(fit)[2])
}

#' Engineer sensor features from a window of readings
#'
#' Computes rolling summaries over the most recent `window_days` of readings:
#' means and trends of steps, resting HR, sedentary hours, plus HR variability
#' and mean SBP.
#'
#' @param readings A tibble of one patient's sensor readings (ordered by ts).
#' @param window_days Number of most-recent days to summarize (default 7).
#' @return A one-row tibble of engineered sensor features.
#' @export
cdt_engineer_sensor_features <- function(readings, window_days = 7) {
  if (nrow(readings) == 0) {
    # Neutral defaults when no data is available.
    return(tibble::tibble(
      steps_mean_7d = 3000, steps_trend_7d = 0,
      resting_hr_mean_7d = 65, resting_hr_trend_7d = 0,
      sbp_mean_7d = 128, sedentary_hours_mean_7d = 15,
      sedentary_hours_trend_7d = 0, hr_variability_7d = 5,
      accel_counts_mean_7d = 250, accel_counts_trend_7d = 0,
      accel_magnitude_mean_7d = 1.03
    ))
  }
  readings <- readings[order(readings$ts), ]
  w <- utils::tail(readings, window_days)

  sedentary <- w$hours_sitting + w$hours_lying

  tibble::tibble(
    steps_mean_7d = mean(w$step_count, na.rm = TRUE),
    steps_trend_7d = .cdt_trend(w$step_count),
    resting_hr_mean_7d = mean(w$resting_hr, na.rm = TRUE),
    resting_hr_trend_7d = .cdt_trend(w$resting_hr),
    sbp_mean_7d = mean(w$sbp, na.rm = TRUE),
    sedentary_hours_mean_7d = mean(sedentary, na.rm = TRUE),
    sedentary_hours_trend_7d = .cdt_trend(sedentary),
    hr_variability_7d = stats::sd(w$heart_rate, na.rm = TRUE),
    accel_counts_mean_7d = mean(w$accel_counts, na.rm = TRUE),
    accel_counts_trend_7d = .cdt_trend(w$accel_counts),
    accel_magnitude_mean_7d = mean(w$accel_magnitude, na.rm = TRUE)
  )
}

#' Assemble the full feature row (static + sensor) for a patient
#'
#' @param patient A one-row canonical patient tibble.
#' @param readings The patient's sensor readings tibble.
#' @param window_days Sensor window length.
#' @return A one-row tibble with all model features.
#' @export
cdt_assemble_features <- function(patient, readings, window_days = 7) {
  sens <- cdt_engineer_sensor_features(readings, window_days = window_days)
  stat <- tibble::tibble(
    age = as.numeric(patient$age),
    sex_male = as.numeric(patient$sex == "M"),
    parkinsons = as.numeric(patient$parkinsons),
    osteoporosis = as.numeric(patient$osteoporosis),
    orthostatic_hypotension = as.numeric(patient$orthostatic_hypotension),
    polypharmacy = as.numeric(patient$polypharmacy),
    prior_falls = as.numeric(patient$prior_falls),
    n_medications = as.numeric(patient$n_medications)
  )
  out <- dplyr::bind_cols(stat, sens)

  # Replace any NaN/NA (from all-missing windows) with neutral medians.
  for (nm in names(out)) {
    if (is.nan(out[[nm]]) || is.na(out[[nm]])) {
      out[[nm]] <- switch(nm,
        steps_mean_7d = 3000, resting_hr_mean_7d = 65,
        sbp_mean_7d = 128, sedentary_hours_mean_7d = 15,
        hr_variability_7d = 5, accel_counts_mean_7d = 250,
        accel_magnitude_mean_7d = 1.03, 0
      )
    }
  }
  out
}

#' Build a labeled training table from stored data
#'
#' For each patient and each "as-of" day (with enough history), engineer
#' features from the preceding window and attach binary labels: did a fall occur
#' within the next 1 day (24h horizon) and within the next 7 days.
#'
#' @param cohort Canonical patient tibble.
#' @param readings Combined sensor readings tibble.
#' @param falls Combined fall-events tibble.
#' @param window_days Sensor feature window.
#' @param stride Days between as-of snapshots (to control table size).
#' @return A tibble with features plus `label_24h` and `label_7d`.
#' @export
cdt_build_training_table <- function(cohort, readings, falls,
                                     window_days = 7, stride = 2) {
  rows <- list()
  ri <- 1L

  for (i in seq_len(nrow(cohort))) {
    pat <- cohort[i, , drop = FALSE]
    pid <- pat$patient_id
    pr <- readings[readings$patient_id == pid, ]
    pr <- pr[order(pr$ts), ]
    if (nrow(pr) < window_days + 1) next

    pf <- falls[falls$patient_id == pid, ]
    fall_dates <- as.Date(pf$ts)

    all_dates <- as.Date(pr$ts)
    # As-of indices where we have a full lookback window and at least 1 future day.
    idxs <- seq(window_days, nrow(pr) - 1, by = stride)
    for (k in idxs) {
      asof <- all_dates[k]
      window_rows <- pr[(k - window_days + 1):k, ]
      feats <- cdt_assemble_features(pat, window_rows, window_days = window_days)

      label_24h <- as.integer(any(fall_dates > asof & fall_dates <= asof + 1))
      label_7d <- as.integer(any(fall_dates > asof & fall_dates <= asof + 7))

      feats$patient_id <- pid
      feats$asof <- format(asof)
      feats$label_24h <- label_24h
      feats$label_7d <- label_7d
      rows[[ri]] <- feats
      ri <- ri + 1L
    }
  }
  dplyr::bind_rows(rows)
}
