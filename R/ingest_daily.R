#' Daily sensor ingestion -- PLACEHOLDER for a real institutional feed
#'
#' IMPORTANT -- read this before using [cdt_append_daily()]:
#'
#' The demo timeline is generated ALL AT ONCE at DB-build time by
#' [cdt_simulate_cohort_sensors()] (see data-raw/generate_synthetic_data.R). The
#' daily "one read-out per patient at 06:00 Europe/Berlin" cadence is faithfully
#' encoded in the stored timestamps, but there is NO live scheduler/cron that
#' appends a fresh reading each morning -- the whole span (start date .. "today")
#' is simulated in a single build step. In other words, the system *simulates*
#' daily ingestion; it does not *perform* it live.
#'
#' In a real deployment this is exactly where an institutional feed would land:
#' a job (cron / systemd timer / cloud scheduler) that, once per day, pulls each
#' patient's overnight wearable read-out from the device vendor or the EHR and
#' appends one new `sensor_readings` row per patient. Predictions
#' ([cdt_patient_risk()], [cdt_cohort_snapshot()]) then automatically reflect the
#' new day because they read the full stored timeline on every call.
#'
#' [cdt_append_daily()] is a PLACEHOLDER that stands in for that feed using the
#' SAME synthetic generator, so the append path can be demonstrated and tested
#' offline. It is intentionally NOT wired to any scheduler and is NOT called
#' during the normal build, `verify.R`, or the app/API/bot startup: appending
#' mutates the demo database and shifts the synthetic timeline, which would break
#' the reproducibility the statistical checkpoint relies on. Call it explicitly
#' (e.g. from your own scheduler script) if you want to exercise the append flow.
#'
#' No model / feature / cutoff / schema changes: this only writes schema-valid
#' rows into the existing `sensor_readings` (and `fall_events`) tables.

# The most recent stored reading date for a patient (a Date), or NA if none.
.cdt_last_reading_date <- function(con, patient_id) {
  row <- DBI::dbGetQuery(
    con,
    "SELECT MAX(ts) AS max_ts FROM sensor_readings WHERE patient_id = ?;",
    params = list(patient_id)
  )
  mt <- row$max_ts[1]
  if (is.null(mt) || is.na(mt) || !nzchar(mt)) {
    return(as.Date(NA))
  }
  # Stored ts is ISO-8601 with an offset (e.g. "2026-03-31T06:00:00+0200"); the
  # leading 10 characters are the calendar date, which is all we need here.
  as.Date(substr(mt, 1, 10))
}

#' Append one simulated day of sensor read-outs for the whole cohort (PLACEHOLDER)
#'
#' Stands in for a real daily institutional ingestion feed (see the file header).
#' For each patient, simulates a SINGLE new daily 06:00 read-out for `as_of`
#' (default: the day after that patient's last stored reading) using the same
#' synthetic generator as the build, and appends it to `sensor_readings`. Any
#' fall simulated on that day is appended to `fall_events`.
#'
#' This mutates the database and advances the synthetic timeline; it is NOT part
#' of the reproducible build and is never called automatically. See the file
#' header for why (statistical-checkpoint reproducibility).
#'
#' @param con A DBI connection (writable).
#' @param as_of Optional `Date` (or ISO "YYYY-MM-DD" string) to append for every
#'   patient. If `NULL` (default), each patient gets the day AFTER their own last
#'   stored reading, so a gap-free daily cadence is maintained per patient.
#' @param seed Optional RNG seed for reproducible appends (default `NULL` = leave
#'   the RNG stream untouched, i.e. non-deterministic like a real feed).
#' @param missing_rate Non-wear dropout probability for the appended day.
#' @return Invisibly a list with `readings_appended` and `falls_appended` counts.
#' @export
cdt_append_daily <- function(con, as_of = NULL, seed = NULL,
                             missing_rate = 0.06) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  cohort <- cdt_get_cohort(con)
  if (nrow(cohort) == 0) {
    return(invisible(list(readings_appended = 0L, falls_appended = 0L)))
  }

  # Normalize an explicit as_of to a Date if the caller passed a string.
  fixed_date <- if (is.null(as_of)) NULL else as.Date(as_of)

  reading_list <- vector("list", nrow(cohort))
  fall_list <- vector("list", nrow(cohort))
  for (i in seq_len(nrow(cohort))) {
    patient <- cohort[i, , drop = FALSE]
    day <- if (!is.null(fixed_date)) {
      fixed_date
    } else {
      last <- .cdt_last_reading_date(con, patient$patient_id)
      if (is.na(last)) cdt_data_end_date() else last + 1L
    }
    # Simulate exactly one daily read-out for this patient on `day`.
    sim <- cdt_simulate_patient_sensors(
      patient,
      days = 1L, start_date = day, missing_rate = missing_rate
    )
    reading_list[[i]] <- sim$readings
    fall_list[[i]] <- sim$falls
  }

  readings <- dplyr::bind_rows(reading_list)
  falls <- dplyr::bind_rows(fall_list)

  n_read <- 0L
  if (nrow(readings) > 0) {
    n_read <- cdt_db_write(con, "sensor_readings", readings, append = TRUE)
  }
  n_fall <- 0L
  if (nrow(falls) > 0) {
    n_fall <- cdt_db_write(con, "fall_events", falls, append = TRUE)
  }

  invisible(list(
    readings_appended = as.integer(n_read),
    falls_appended = as.integer(n_fall)
  ))
}
