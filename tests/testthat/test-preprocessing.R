test_that("cohort generator is reproducible and well-formed", {
  a <- cdt_generate_cohort(n = 20, seed = 123)
  b <- cdt_generate_cohort(n = 20, seed = 123)
  expect_identical(a, b)
  expect_equal(nrow(a), 20)
  expect_true(all(cdt_canonical_patient_cols() %in% names(a)))
  expect_true(all(a$sex %in% c("F", "M")))
  expect_true(all(a$age >= 55 & a$age <= 98))
  # Flags are strictly 0/1.
  for (col in c("parkinsons", "osteoporosis", "polypharmacy", "prior_falls")) {
    expect_true(all(a[[col]] %in% c(0L, 1L)))
  }
})

test_that("ingestion normalizes column-name variants", {
  raw <- data.frame(
    id = c("A1", "A2"),
    gender = c("Male", "female"),
    age = c(70, NA),
    previous_falls = c("yes", "no"),
    medications = c("levodopa;amlodipine", ""),
    stringsAsFactors = FALSE
  )
  out <- cdt_ingest_patient_csv(raw)
  expect_equal(out$sex, c("M", "F"))
  expect_equal(out$patient_id, c("A1", "A2"))
  expect_equal(out$prior_falls, c(1L, 0L))
  # n_medications derived from the medication string.
  expect_equal(out$n_medications, c(2L, 0L))
  # Missing age imputed (no NA remains).
  expect_false(any(is.na(out$age)))
})

test_that("validation catches duplicates and bad ages", {
  good <- cdt_generate_cohort(n = 5, seed = 9)
  expect_invisible(cdt_validate_patients(good))

  dup <- good
  dup$patient_id[2] <- dup$patient_id[1]
  expect_error(cdt_validate_patients(dup), "Duplicate")

  bad_age <- good
  bad_age$age[1] <- 200
  expect_error(cdt_validate_patients(bad_age), "range")
})

test_that("flag coercion handles varied truthy/falsy inputs", {
  expect_equal(.cdt_as_flag(c("yes", "no", "TRUE", "0", "y")), c(1L, 0L, 1L, 0L, 1L))
  expect_equal(.cdt_as_flag(c(TRUE, FALSE)), c(1L, 0L))
})

test_that("sensor simulation produces expected shape and injects missingness", {
  cohort <- cdt_generate_cohort(n = 3, seed = 5)
  sim <- cdt_simulate_cohort_sensors(cohort, days = 40, seed = 5)
  expect_equal(nrow(sim$readings), 3 * 40)
  expect_true(all(c("step_count", "resting_hr", "sbp") %in% names(sim$readings)))
  # Blood pressure, heart rate, and accelerometry are all ingested per read-out.
  expect_true(all(c("sbp", "dbp", "heart_rate", "resting_hr",
    "accel_counts", "accel_magnitude") %in% names(sim$readings)))
  # Some missingness expected across the streams.
  expect_true(anyNA(sim$readings$step_count))
  # Standing + sitting + lying should be within a day's hours (allow rounding).
  tot <- sim$readings$hours_sitting + sim$readings$hours_lying +
    sim$readings$hours_standing
  expect_true(all(tot <= 24.5 | is.na(tot)))
})

test_that("daily read-outs are at 06:00 Europe/Berlin (CET/CEST)", {
  ts <- cdt_daily_timestamps(as.Date("2026-01-01"), 5)
  expect_length(ts, 5)
  # Local clock time is 06:00 on each day.
  expect_true(all(grepl("T06:00:00", ts, fixed = TRUE)))
  # Winter dates carry the CET offset (+0100).
  expect_true(all(grepl("\\+0100$", ts)))
  # A summer date carries CEST (+0200), proving DST is handled.
  summer <- cdt_daily_timestamps(as.Date("2026-07-01"), 1)
  expect_true(grepl("\\+0200$", summer))
})

test_that("feature engineering yields a full, finite feature row", {
  cohort <- cdt_generate_cohort(n = 1, seed = 3)
  sim <- cdt_simulate_cohort_sensors(cohort, days = 30, seed = 3)
  fr <- cdt_assemble_features(cohort[1, ], sim$readings)
  expect_equal(nrow(fr), 1)
  expect_true(all(cdt_model_features() %in% names(fr)))
  expect_true(all(is.finite(as.numeric(fr[1, cdt_model_features()]))))
})
