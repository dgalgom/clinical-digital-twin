# Coverage for the daily-ingestion PLACEHOLDER (cdt_append_daily). All offline:
# a temp SQLite DB seeded from the shared fixture. These tests assert the append
# path writes schema-valid rows and advances the per-patient timeline by exactly
# one day; they do NOT exercise any live scheduler (there is none by design).

# A temp DB seeded with the fixture cohort + readings (+ falls), starting on a
# fixed date so "last reading date" is deterministic.
.ingest_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) {
    cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  }
  con
}

test_that("cdt_append_daily appends one read-out per patient for an explicit date", {
  fx <- make_test_fixtures()
  con <- .ingest_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n_patients <- nrow(cdt_get_cohort(con))
  before <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sensor_readings;")$n

  res <- cdt_append_daily(con, as_of = "2026-05-01", seed = 123)

  after <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sensor_readings;")$n
  # Exactly one new row per patient.
  expect_identical(res$readings_appended, as.integer(n_patients))
  expect_identical(after - before, n_patients)

  # The appended rows carry the requested calendar date at 06:00 local.
  appended <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT substr(ts,1,10) AS d FROM sensor_readings WHERE substr(ts,1,10)='2026-05-01';"
  )
  expect_identical(nrow(appended), 1L)
})

test_that("cdt_append_daily (default) advances each patient by exactly one day", {
  fx <- make_test_fixtures()
  con <- .ingest_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  pid <- cdt_get_cohort(con)$patient_id[[1]]
  last_before <- DBI::dbGetQuery(
    con, "SELECT MAX(ts) AS m FROM sensor_readings WHERE patient_id = ?;",
    params = list(pid)
  )$m
  last_before_date <- as.Date(substr(last_before, 1, 10))

  cdt_append_daily(con, seed = 7)

  last_after <- DBI::dbGetQuery(
    con, "SELECT MAX(ts) AS m FROM sensor_readings WHERE patient_id = ?;",
    params = list(pid)
  )$m
  last_after_date <- as.Date(substr(last_after, 1, 10))
  expect_identical(as.integer(last_after_date - last_before_date), 1L)
})

test_that("appended readings are schema-valid and flow into predictions", {
  fx <- make_test_fixtures()
  con <- .ingest_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  pid <- cdt_get_cohort(con)$patient_id[[1]]
  tl_before <- nrow(cdt_get_patient_timeline(con, pid))

  cdt_append_daily(con, as_of = "2026-06-01", seed = 42)

  tl_after <- cdt_get_patient_timeline(con, pid)
  expect_identical(nrow(tl_after) - tl_before, 1L)
  # The new row exposes the same accelerometry columns the model consumes.
  expect_true(all(c("accel_counts", "accel_magnitude") %in% names(tl_after)))

  # A prediction still computes cleanly against the extended timeline.
  risk <- cdt_patient_risk(con, fx$model, pid)
  expect_true(is.finite(risk$p_7d))
})

test_that("cdt_append_daily is a no-op on an empty cohort", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- cdt_append_daily(con, as_of = "2026-05-01")
  expect_identical(res$readings_appended, 0L)
  expect_identical(res$falls_appended, 0L)
})
