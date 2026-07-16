# Phase 0 - simulation configuration.
# These functions are pure config (no DB, no LLM); no fixtures required.

# The canonical `patients`-table columns produced by cdt_generate_cohort().
.canonical_patient_cols <- c(
  "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
  "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
  "medications", "comorbidities"
)

test_that("institution profile is a well-formed named list", {
  inst <- cdt_sim_institution()
  expect_true(is.list(inst))
  expect_identical(inst$module_size, 10L)
  expect_false(inst$night_nurse)
  expect_true(all(inst$physio_days %in% c("Mon", "Tue", "Wed", "Thu", "Fri")))
  expect_true(is.list(inst$staff_ratio))
  expect_true(all(c("morning", "afternoon", "night") %in% names(inst$staff_ratio)))
})

test_that("there are exactly 10 patients with ids P01..P10", {
  pts <- cdt_sim_patients()
  expect_identical(nrow(pts), 10L)
  expect_identical(pts$patient_id, sprintf("P%02d", 1:10))
})

test_that("patients carry every canonical clinical column", {
  pts <- cdt_sim_patients()
  missing <- setdiff(.canonical_patient_cols, names(pts))
  expect_identical(missing, character(0))
})

test_that("clinical flag columns are integer 0/1 (schema-compatible)", {
  pts <- cdt_sim_patients()
  flags <- c(
    "parkinsons", "osteoporosis", "orthostatic_hypotension",
    "polypharmacy", "prior_falls"
  )
  for (f in flags) {
    expect_true(is.integer(pts[[f]]), info = f)
    expect_true(all(pts[[f]] %in% c(0L, 1L)), info = f)
  }
  expect_true(is.integer(pts$age))
  expect_true(is.integer(pts$n_medications))
})

test_that("each patient has a non-empty behavioural system prompt", {
  pts <- cdt_sim_patients()
  expect_true("system_prompt" %in% names(pts))
  expect_true(all(nzchar(pts$system_prompt)))
  expect_true("baseline_notes" %in% names(pts))
})

test_that("names are flagged synthetic (no real PHI)", {
  pts <- cdt_sim_patients()
  expect_true(all(grepl("^\\[SYNTHETIC\\] ", pts$name)))
})

test_that("affinity matrix is 10x10 with P01..P10 dimnames and zero diagonal", {
  ids <- sprintf("P%02d", 1:10)
  m <- cdt_sim_affinity_matrix()
  expect_identical(dim(m), c(10L, 10L))
  expect_identical(rownames(m), ids)
  expect_identical(colnames(m), ids)
  expect_true(all(diag(m) == 0L))
})

test_that("P09 initiates no interaction (its whole row is zero)", {
  m <- cdt_sim_affinity_matrix()
  expect_true(all(m["P09", ] == 0L))
  # But P09 may be the *object* of interaction (its column is non-zero).
  expect_true(sum(m[, "P09"]) > 0L)
})

test_that("flu config targets valid resident ids", {
  flu <- cdt_sim_flu_config()
  ids <- cdt_sim_patients()$patient_id
  expect_true(all(flu$affected %in% ids))
  expect_true(flu$start_day >= 1L)
  expect_true(flu$duration_days >= 1L)
})

test_that("P08 experiment is keyed to patient P08 with a sane onset window", {
  exp <- cdt_sim_p08_experiment()
  expect_identical(exp$patient, "P08")
  expect_true(exp$patient %in% cdt_sim_patients()$patient_id)
  expect_identical(length(exp$latent_onset_window), 2L)
  expect_true(exp$latent_onset_window[1] <= exp$latent_onset_window[2])
  expect_true(exp$hazard_ceiling > 0 && exp$hazard_ceiling <= 1)
  expect_true(exp$hazard_reduction > 0 && exp$hazard_reduction <= 1)
})
