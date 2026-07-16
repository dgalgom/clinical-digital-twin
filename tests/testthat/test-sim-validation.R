# Phase 2 - simulation validation gate.
# The biological validator codifies synthetic_sensors.R invariants and must NOT
# invent HR/BP caps, so flu-elevated vitals pass (WARN at most). Covers all five
# validators plus the checkpoint-gate aggregator.

.valid_decision <- function() {
  list(
    patient_id = "P01", day = 1L,
    mobility_pct_of_baseline = 1.0,
    participated_group_activity = 1L,
    medication_adherence = 1L,
    meaningful_social_interaction = 0L,
    mood_fatigue = "ok",
    notable_event = NA_character_,
    confidence = 0.9
  )
}

.good_reading <- function(...) {
  base <- data.frame(
    patient_id = "P01", ts = "2026-01-01T06:00:00+0100",
    heart_rate = 90, resting_hr = 70, sbp = 130, dbp = 80,
    step_count = 1200L, accel_counts = 5000L, accel_magnitude = 1.02,
    hours_sitting = 8, hours_lying = 9, hours_standing = 7,
    stringsAsFactors = FALSE
  )
  ov <- list(...)
  for (k in names(ov)) base[[k]] <- ov[[k]]
  base
}

# --- validate_agent_json ---------------------------------------------------

test_that("a well-formed decision passes", {
  expect_identical(validate_agent_json(.valid_decision())$status, "pass")
})

test_that("mobility out of [0,2] fails", {
  d <- .valid_decision(); d$mobility_pct_of_baseline <- 2.5
  expect_identical(validate_agent_json(d)$status, "fail")
})

test_that("a missing key fails", {
  d <- .valid_decision(); d$confidence <- NULL
  res <- validate_agent_json(d)
  expect_identical(res$status, "fail")
  expect_true(any(grepl("missing keys", res$issues)))
})

test_that("an out-of-set mood fails", {
  d <- .valid_decision(); d$mood_fatigue <- "euphoric"
  expect_identical(validate_agent_json(d)$status, "fail")
})

test_that("a non-binary participation flag fails", {
  d <- .valid_decision(); d$participated_group_activity <- 3L
  expect_identical(validate_agent_json(d)$status, "fail")
})

# --- validate_social_interactions ------------------------------------------

test_that("valid interactions pass", {
  df <- data.frame(
    initiated_by = "P04", stringsAsFactors = FALSE
  )
  df$participants <- I(list(c("P04", "P05")))
  res <- validate_social_interactions(df, sprintf("P%02d", 1:10))
  expect_identical(res$status, "pass")
})

test_that("an unknown participant id fails", {
  df <- data.frame(initiated_by = "P04", stringsAsFactors = FALSE)
  df$participants <- I(list(c("P04", "P99")))
  expect_identical(
    validate_social_interactions(df, sprintf("P%02d", 1:10))$status, "fail")
})

test_that("a self/duplicate participant fails", {
  df <- data.frame(initiated_by = "P04", stringsAsFactors = FALSE)
  df$participants <- I(list(c("P04", "P04")))
  expect_identical(
    validate_social_interactions(df, sprintf("P%02d", 1:10))$status, "fail")
})

test_that("exceeding max interactions per day fails", {
  df <- data.frame(initiated_by = rep("P04", 40), stringsAsFactors = FALSE)
  df$participants <- I(replicate(40, c("P04", "P05"), simplify = FALSE))
  expect_identical(
    validate_social_interactions(df, sprintf("P%02d", 1:10),
      max_per_day = 12L)$status, "fail")
})

# --- validate_biological_plausibility --------------------------------------

test_that("a clean reading passes", {
  expect_identical(validate_biological_plausibility(.good_reading())$status, "pass")
})

test_that("posture hours not summing to 24 fails", {
  r <- .good_reading(hours_sitting = 8, hours_lying = 9, hours_standing = 6) # =23
  expect_identical(validate_biological_plausibility(r)$status, "fail")
})

test_that("negative steps fails", {
  r <- .good_reading(step_count = -10L)
  expect_identical(validate_biological_plausibility(r)$status, "fail")
})

test_that("hours_lying above 20 fails", {
  r <- .good_reading(hours_lying = 21, hours_sitting = 2, hours_standing = 1)
  expect_identical(validate_biological_plausibility(r)$status, "fail")
})

test_that("a non-wear (all-NA) day is allowed as WARN, not FAIL", {
  r <- .good_reading(
    heart_rate = NA, resting_hr = NA, sbp = NA, dbp = NA,
    step_count = NA, accel_counts = NA, accel_magnitude = NA,
    hours_sitting = NA, hours_lying = NA, hours_standing = NA
  )
  expect_identical(validate_biological_plausibility(r)$status, "warn")
})

test_that("flu-elevated resting HR passes (large jump is WARN, never FAIL)", {
  prior <- .good_reading(resting_hr = 70)
  febrile <- .good_reading(resting_hr = 92)  # +22 bpm flu bump
  res <- validate_biological_plausibility(febrile, prior = prior)
  expect_true(res$status %in% c("pass", "warn"))
  expect_false(res$status == "fail")
})

# --- validate_model_output -------------------------------------------------

test_that("a valid prediction passes", {
  expect_identical(
    validate_model_output(list(p_24h = 0.1, p_7d = 0.2))$status, "pass")
})

test_that("a probability above 1 fails", {
  expect_identical(
    validate_model_output(list(p_24h = 0.1, p_7d = 1.2))$status, "fail")
})

test_that("NaN probability fails", {
  expect_identical(
    validate_model_output(list(p_24h = NaN, p_7d = 0.2))$status, "fail")
})

test_that("p_7d < p_24h warns (not fail)", {
  res <- validate_model_output(list(p_24h = 0.3, p_7d = 0.1))
  expect_identical(res$status, "warn")
})

# --- run_daily_checkpoint_gate ---------------------------------------------

test_that("the gate logs each step and returns the worst status", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  cdt_sim_init_schema(con)
  results <- list(
    agent_json = list(status = "pass", issues = character(0)),
    biological = list(status = "warn", issues = "non-wear"),
    model      = list(status = "pass", issues = character(0))
  )
  gate <- run_daily_checkpoint_gate(con, "sim1_baseline", "A", 1, results)
  expect_identical(gate$status, "warn")
  log <- cdt_sim_get_checkpoints(con, "sim1_baseline", "A")
  expect_identical(nrow(log), 3L)
})

test_that("the gate returns fail when any step fails", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  cdt_sim_init_schema(con)
  results <- list(
    agent_json = list(status = "pass", issues = character(0)),
    biological = list(status = "fail", issues = "negative steps")
  )
  gate <- run_daily_checkpoint_gate(con, "sim1_baseline", "A", 2, results)
  expect_identical(gate$status, "fail")
})
