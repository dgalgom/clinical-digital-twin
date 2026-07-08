# Step 5d: what-if levers spanning ALL modeled modifiable inputs, the named-drug
# lever, and honest declines for factors the model does not contain.

# Seed a DB with the shared fixture cohort + a demo user; return the connection.
.wi_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) {
    cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  }
  cdt_create_user(con, "dr_wi", "pw12345")
  con
}

.wi_login <- function(con, model, chat_id) {
  cdt_bot_handle_message(con, model, chat_id, "login as dr_wi", llm_mock = TRUE)
}

# --- parser: each lever maps to the correct model override -----------------

test_that("parse_whatif keeps the original steps%/SBP/sedentary levers", {
  expect_equal(cdt_bot_parse_whatif("increase mobility by 25%")$steps_pct, 25)
  expect_equal(cdt_bot_parse_whatif("lower systolic BP by 10 mmHg")$sbp_delta, -10)
  # A bare "reduce sedentary time" still yields the absolute default.
  expect_equal(
    cdt_bot_parse_whatif("reduce sedentary time")$sedentary_hours_mean_7d, 12
  )
  expect_null(cdt_bot_parse_whatif("how are they doing?"))
})

test_that("parse_whatif emits a minutes-of-activity hint (caller-resolved)", {
  ov <- cdt_bot_parse_whatif("what if the patient does 30 min more activity?")
  expect_equal(ov$steps_minutes_delta, 30)
  expect_null(ov$steps_pct) # not resolved until the caller has a baseline
})

test_that("parse_whatif handles resting HR, HR variability, prior falls, OH", {
  expect_equal(
    cdt_bot_parse_whatif("bring resting HR to 60")$resting_hr_mean_7d, 60
  )
  expect_equal(
    cdt_bot_parse_whatif("raise HR variability to 8")$hr_variability_7d, 8
  )
  expect_equal(cdt_bot_parse_whatif("assume no prior falls")$prior_falls, 0)
  expect_equal(
    cdt_bot_parse_whatif("treat orthostatic hypotension")$orthostatic_hypotension,
    0
  )
})

test_that("parse_whatif returns NULL when no modeled lever is named", {
  expect_null(cdt_bot_parse_whatif("what if the patient sleeps one hour more?"))
  expect_null(cdt_bot_parse_whatif("tell me a joke"))
})

# --- unmodeled-factor detection --------------------------------------------

test_that("unmodeled factors are detected for honest decline", {
  expect_equal(cdt_bot_unmodeled_factor("what if they sleep 1 hour more"), "sleep")
  expect_match(cdt_bot_unmodeled_factor("improve their diet"), "diet")
  expect_null(cdt_bot_unmodeled_factor("increase activity by 20%"))
})

# --- named-drug resolution (needs the patient row) --------------------------

test_that("named-drug lever decrements meds and recomputes polypharmacy", {
  # A patient on >= 5 meds so the decrement is observable and polypharmacy math
  # is exercised on both sides of the threshold.
  patient <- tibble::tibble(
    patient_id = "P001",
    n_medications = 5L,
    polypharmacy = 1L,
    medications = "levodopa;amlodipine;furosemide;warfarin;sertraline"
  )
  res <- cdt_bot_resolve_drug_override(patient, "what if we remove furosemide?")
  expect_false(isTRUE(res$not_found))
  expect_equal(res$overrides$n_medications, 4)
  expect_equal(res$overrides$polypharmacy, 0L) # dropped below 5
  expect_equal(res$drug, "furosemide")
})

test_that("named-drug lever flags a drug the patient is not on", {
  patient <- tibble::tibble(
    patient_id = "P002",
    n_medications = 2L,
    polypharmacy = 0L,
    medications = "metformin;atorvastatin"
  )
  res <- cdt_bot_resolve_drug_override(patient, "stop warfarin")
  expect_true(isTRUE(res$not_found))
  expect_equal(res$drug, "warfarin")
})

test_that("non-drug what-if is not treated as a drug override", {
  patient <- tibble::tibble(
    patient_id = "P003", n_medications = 3L, polypharmacy = 0L,
    medications = "metformin;atorvastatin;amlodipine"
  )
  expect_null(cdt_bot_resolve_drug_override(patient, "increase activity by 20%"))
})

# --- end-to-end via the bot (mock LLM) --------------------------------------

test_that("bot resolves a minutes-of-activity what-if into a simulation", {
  fx <- make_test_fixtures()
  con <- .wi_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .wi_login(con, fx$model, "chatWI")

  r <- cdt_bot_reply(con, fx$model, "chatWI",
    sprintf("How about patient %s? What if they do 30 min more activity daily?", pid),
    llm_mock = TRUE)
  expect_true(nzchar(r$text))
})

test_that("bot declines an unmodeled-factor what-if honestly", {
  fx <- make_test_fixtures()
  con <- .wi_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .wi_login(con, fx$model, "chatWI2")

  # Focus a patient first, then ask an unmodeled what-if.
  cdt_bot_reply(con, fx$model, "chatWI2",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  r <- cdt_bot_reply(con, fx$model, "chatWI2",
    "what if this patient slept one hour more each night?", llm_mock = TRUE)
  expect_match(r$text, "doesn't model sleep")
  expect_null(r$photo)
})

test_that("bot declines a drug the patient is not on", {
  fx <- make_test_fixtures()
  con <- .wi_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()

  # Find a patient and a drug that is NOT in their list.
  cohort <- fx$cohort
  pid <- cohort$patient_id[1]
  meds <- tolower(cohort$medications[cohort$patient_id == pid])
  candidate <- setdiff(
    c("levodopa", "warfarin", "furosemide", "metformin", "gabapentin"),
    tolower(unlist(strsplit(meds, "[;,|]")))
  )[1]
  .wi_login(con, fx$model, "chatWI3")
  cdt_bot_reply(con, fx$model, "chatWI3",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)

  r <- cdt_bot_reply(con, fx$model, "chatWI3",
    sprintf("what if we remove %s?", candidate), llm_mock = TRUE)
  expect_match(r$text, "not in")
  expect_null(r$photo)
})
