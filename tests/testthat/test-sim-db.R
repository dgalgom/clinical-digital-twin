# Phase 1 - simulation DB schema.
# Verifies the additive, isolated simulation schema: idempotent init, round-trip
# on each new table, the nullable sensor columns, and that the existing sensor
# write path is unaffected. Throwaway SQLite file per test.

.sim_con <- function() {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)   # production schema (creates sensor_readings)
  cdt_sim_init_schema(con)  # additive simulation schema
  # Seed the 10 sim patients (canonical cols only) so sensor FK is satisfiable.
  pts <- cdt_sim_patients()
  canonical <- c(
    "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
    "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
    "medications", "comorbidities"
  )
  cdt_db_write(con, "patients", pts[, canonical], append = TRUE)
  con
}

test_that("sim schema creates all five run-keyed tables", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  tbls <- DBI::dbListTables(con)
  expect_true(all(c(
    "agent_decisions", "social_interactions", "model_predictions",
    "daily_checkpoint_log", "ground_truth_evaluation"
  ) %in% tbls))
})

test_that("cdt_sim_init_schema is idempotent (safe to call twice)", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  expect_invisible(cdt_sim_init_schema(con))
  expect_silent(cdt_sim_init_schema(con))
  # Sensor columns added exactly once, not duplicated.
  cols <- DBI::dbListFields(con, "sensor_readings")
  expect_identical(sum(cols == "simulation_id"), 1L)
})

test_that("sensor_readings gains nullable simulation columns", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  cols <- DBI::dbListFields(con, "sensor_readings")
  expect_true(all(c("simulation_id", "branch", "day", "quality_flags") %in% cols))
})

test_that("existing sensor write path still works after widening", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  # A production-style row that names none of the new columns.
  row <- data.frame(
    patient_id = "P01", ts = "2026-01-01T06:00:00+0100",
    heart_rate = 72, resting_hr = 70, sbp = 130, dbp = 80,
    step_count = 1200L, accel_counts = 5000L, accel_magnitude = 1.02,
    hours_sitting = 8, hours_lying = 9, hours_standing = 7,
    stringsAsFactors = FALSE
  )
  n <- cdt_db_write(con, "sensor_readings", row, append = TRUE)
  expect_identical(n, 1L)
  back <- DBI::dbGetQuery(con, "SELECT * FROM sensor_readings WHERE patient_id='P01';")
  expect_identical(nrow(back), 1L)
  # New sim columns are NULL for a production row.
  expect_true(is.na(back$simulation_id))
})

test_that("agent_decisions round-trips via write + get helpers", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  df <- data.frame(
    simulation_id = "sim1_baseline", branch = "A", day = 1L,
    patient_id = c("P01", "P02"),
    mobility_pct_of_baseline = c(1.0, 0.8),
    participated_group_activity = c(1L, 0L),
    medication_adherence = c(1L, 1L),
    meaningful_social_interaction = c(1L, 0L),
    mood_fatigue = c("ok", "tired"),
    notable_event = c(NA_character_, "declined physio"),
    confidence = c(0.9, 0.7),
    agent_output_invalid = c(0L, 0L),
    temperature = c(0.4, 0.4),
    prompt_text = c("p1", "p2"),
    raw_reply = c("{...}", "{...}"),
    stringsAsFactors = FALSE
  )
  expect_identical(cdt_sim_write_agent_decisions(con, df), 2L)
  got <- cdt_sim_get_agent_decisions(con, "sim1_baseline", "A")
  expect_identical(nrow(got), 2L)
  got1 <- cdt_sim_get_agent_decisions(con, "sim1_baseline", "A",
    day = 1, patient_id = "P02")
  expect_identical(nrow(got1), 1L)
  expect_identical(got1$mood_fatigue, "tired")
})

test_that("model_predictions round-trips and scopes to the run", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  df <- data.frame(
    simulation_id = "sim1_baseline", branch = "B", day = 2L,
    patient_id = c("P08", "P09"),
    p_24h = c(0.12, 0.22), p_7d = c(0.27, 0.31),
    tier_24h = c("Low", "Moderate"), tier_7d = c("Moderate", "High"),
    quality_flag = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  cdt_sim_write_predictions(con, df)
  got <- cdt_sim_get_predictions(con, "sim1_baseline", "B", patient_id = "P08")
  expect_identical(nrow(got), 1L)
  expect_equal(got$p_7d, 0.27)
  # A different branch returns nothing.
  expect_identical(nrow(cdt_sim_get_predictions(con, "sim1_baseline", "A")), 0L)
})

test_that("social_interactions round-trips with JSON participants", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  df <- data.frame(
    simulation_id = "sim1_baseline", branch = "A", day = 1L,
    participants = '["P04","P05"]',
    interaction_type = "shared_activity",
    initiated_by = "P04",
    summary_text = "sat together at lunch",
    stringsAsFactors = FALSE
  )
  expect_identical(cdt_sim_write_social(con, df), 1L)
  back <- DBI::dbGetQuery(con,
    "SELECT * FROM social_interactions WHERE simulation_id='sim1_baseline';")
  expect_identical(nrow(back), 1L)
  expect_identical(back$initiated_by, "P04")
})

test_that("checkpoint log records pass/warn/fail and reads back in order", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_sim_log_checkpoint(con, "sim1_baseline", "A", 1, "agent_json", "pass")
  cdt_sim_log_checkpoint(con, "sim1_baseline", "A", 1, "biological", "warn",
    "flu HR elevated")
  cdt_sim_log_checkpoint(con, "sim1_baseline", "A", 2, "gate", "fail", "bad p")
  log <- cdt_sim_get_checkpoints(con, "sim1_baseline", "A")
  expect_identical(nrow(log), 3L)
  expect_identical(log$status, c("pass", "warn", "fail"))
  expect_error(
    cdt_sim_log_checkpoint(con, "sim1_baseline", "A", 1, "x", "bogus"),
    NULL
  )
})

test_that("ground_truth is writable but has no exported getter", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_sim_write_ground_truth(con, "sim1_baseline", "B", 5, "P08",
    latent_risk = 0.3, hazard = 0.05, fall_sampled = 0L,
    intervention_fired = 1L)
  # Present in the table (raw query, as the orchestrator would).
  raw <- DBI::dbGetQuery(con,
    "SELECT * FROM ground_truth_evaluation WHERE patient_id='P08';")
  expect_identical(nrow(raw), 1L)
  expect_identical(raw$intervention_fired, 1L)
  # No exported getter exists for the restricted table.
  expect_false(exists("cdt_sim_get_ground_truth"))
})

test_that("sim timeline getter scopes to a single run", {
  con <- .sim_con()
  on.exit(DBI::dbDisconnect(con))
  base <- list(
    heart_rate = 72, resting_hr = 70, sbp = 130, dbp = 80,
    step_count = 1200L, accel_counts = 5000L, accel_magnitude = 1.02,
    hours_sitting = 8, hours_lying = 9, hours_standing = 7
  )
  mk <- function(sim, br) {
    data.frame(
      patient_id = "P01", ts = "2026-01-01T06:00:00+0100",
      heart_rate = base$heart_rate, resting_hr = base$resting_hr,
      sbp = base$sbp, dbp = base$dbp, step_count = base$step_count,
      accel_counts = base$accel_counts, accel_magnitude = base$accel_magnitude,
      hours_sitting = base$hours_sitting, hours_lying = base$hours_lying,
      hours_standing = base$hours_standing,
      simulation_id = sim, branch = br, day = 1L, quality_flags = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  cdt_db_write(con, "sensor_readings", mk("sim1_baseline", "A"), append = TRUE)
  cdt_db_write(con, "sensor_readings", mk("sim2_flu", "A"), append = TRUE)
  tl <- cdt_sim_get_patient_timeline(con, "sim1_baseline", "A", "P01")
  expect_identical(nrow(tl), 1L)
  expect_identical(tl$simulation_id, "sim1_baseline")
})
