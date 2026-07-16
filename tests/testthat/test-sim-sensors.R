# Phase 3 - sensor modulation from agent decisions.
# The generator reuses the engine's baseline shape but emits ONE day scaled by
# the decision, preserving the sum-to-24 / pmax guards so the biological
# validator always passes. Deterministic under a seed.

.p08 <- function() {
  cdt_sim_patients()[cdt_sim_patients()$patient_id == "P08", , drop = FALSE]
}

.decision_at <- function(mob, activity = 1L) {
  list(
    patient_id = "P08", day = 1L,
    mobility_pct_of_baseline = mob,
    participated_group_activity = activity,
    medication_adherence = 1L,
    meaningful_social_interaction = 1L,
    mood_fatigue = "ok",
    notable_event = NA_character_,
    confidence = 0.9
  )
}

test_that("output always passes the biological validator across mobility range", {
  inst <- cdt_sim_institution()
  for (mob in c(0, 0.25, 0.5, 1.0, 1.5, 2.0)) {
    r <- cdt_sim_day_sensors(.p08(), .decision_at(mob), inst, day = 3,
      seed = 100 + round(mob * 10))
    res <- validate_biological_plausibility(r)
    expect_false(res$status == "fail",
      info = sprintf("mobility=%.2f -> %s", mob, res$status))
  }
})

test_that("posture hours sum to 24 for any decision", {
  inst <- cdt_sim_institution()
  r <- cdt_sim_day_sensors(.p08(), .decision_at(0.3), inst, day = 5, seed = 7)
  total <- r$hours_sitting + r$hours_lying + r$hours_standing
  expect_lt(abs(total - 24), 0.05)
})

test_that("steps increase monotonically with mobility (same seed)", {
  inst <- cdt_sim_institution()
  low <- cdt_sim_day_sensors(.p08(), .decision_at(0.4), inst, day = 2, seed = 42)
  high <- cdt_sim_day_sensors(.p08(), .decision_at(1.6), inst, day = 2, seed = 42)
  expect_gt(high$step_count, low$step_count)
  # Less mobility -> more time lying down.
  expect_gt(low$hours_lying, high$hours_lying)
})

test_that("generation is deterministic under a fixed seed", {
  inst <- cdt_sim_institution()
  a <- cdt_sim_day_sensors(.p08(), .decision_at(1.0), inst, day = 4, seed = 99)
  b <- cdt_sim_day_sensors(.p08(), .decision_at(1.0), inst, day = 4, seed = 99)
  expect_identical(a$step_count, b$step_count)
  expect_identical(a$resting_hr, b$resting_hr)
})

test_that("flu context raises resting HR but stays biologically plausible", {
  inst <- cdt_sim_institution()
  flu <- cdt_sim_flu_config()
  base <- cdt_sim_day_sensors(.p08(), .decision_at(1.0), inst, day = 12,
    seed = 5)
  febrile <- cdt_sim_day_sensors(.p08(), .decision_at(1.0), inst, day = 12,
    flu_ctx = flu, seed = 5)
  expect_gt(febrile$resting_hr, base$resting_hr)
  # Still passes (validator does not cap HR).
  expect_false(validate_biological_plausibility(febrile)$status == "fail")
})

test_that("row is stamped with the run key when supplied", {
  inst <- cdt_sim_institution()
  r <- cdt_sim_day_sensors(.p08(), .decision_at(1.0), inst, day = 1, seed = 1,
    simulation_id = "sim1_baseline", branch = "A")
  expect_identical(r$simulation_id, "sim1_baseline")
  expect_identical(r$branch, "A")
  expect_identical(r$day, 1L)
})
