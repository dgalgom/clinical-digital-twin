# Phase 5 - simulation orchestrator + hidden P08 blind experiment.
# A 3-day mock A/B run in the isolated sim schema: verifies the four run-keyed
# tables are populated and scoped, predictions stay in [0,1], the P08 onset is
# shared across branches (valid counterfactual), and - critically - that no
# clinical surface can read the restricted ground_truth_evaluation fields.

.orch_con <- function() {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_sim_init_schema(con)
  canonical <- c(
    "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
    "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
    "medications", "comorbidities"
  )
  cdt_db_write(con, "patients", cdt_sim_patients()[, canonical], append = TRUE)
  con
}

# A small, cheap model fixture trained on generated (non-sim) data. Cached so
# the several tests below share one fit.
.orch_model <- local({
  m <- NULL
  function() {
    if (is.null(m)) {
      cohort <- cdt_generate_cohort(n = 60, seed = 1)
      sim <- cdt_simulate_cohort_sensors(cohort, days = 90, seed = 2)
      training <- cdt_build_training_table(cohort, sim$readings, sim$falls,
        window_days = 7, stride = 2)
      m <<- cdt_fit_model(training)
    }
    m
  }
})

test_that("a 3-day mock run populates the four run-keyed sim tables", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  res <- cdt_run_simulation(con, .orch_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  expect_identical(res$days_completed, 3L)
  expect_true(all(res$gate_statuses %in% c("pass", "warn")))

  dec <- cdt_sim_get_agent_decisions(con, "sim1_baseline", "A")
  expect_identical(nrow(dec), 30L) # 10 patients x 3 days
  preds <- cdt_sim_get_predictions(con, "sim1_baseline", "A")
  expect_identical(nrow(preds), 30L)
  ck <- cdt_sim_get_checkpoints(con, "sim1_baseline", "A")
  expect_true(nrow(ck) > 0)
  soc <- DBI::dbGetQuery(con,
    "SELECT * FROM social_interactions WHERE simulation_id='sim1_baseline';")
  expect_true(nrow(soc) > 0)
})

test_that("predictions stay within [0,1] and carry both horizons", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .orch_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  preds <- cdt_sim_get_predictions(con, "sim1_baseline", "A")
  expect_true(all(preds$p_24h >= 0 & preds$p_24h <= 1))
  expect_true(all(preds$p_7d >= 0 & preds$p_7d <= 1))
  expect_true(all(!is.na(preds$tier_24h)))
  expect_true(all(!is.na(preds$tier_7d)))
})

test_that("run keys scope strictly to their own (simulation_id, branch)", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .orch_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  # A different branch (not run) returns nothing.
  expect_identical(nrow(cdt_sim_get_predictions(con, "sim1_baseline", "B")), 0L)
  # A different simulation returns nothing.
  expect_identical(nrow(cdt_sim_get_predictions(con, "sim2_flu", "A")), 0L)
})

test_that("A and B share the P08 onset (valid counterfactual)", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  a <- cdt_run_simulation(con, .orch_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  b <- cdt_run_simulation(con, .orch_model(), "sim1_baseline", "B",
    days = 3, seed = 111, mock = TRUE)
  expect_identical(a$p08_onset, b$p08_onset)
})

test_that("ground_truth_evaluation is populated for P08 but write-only", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .orch_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  gt <- DBI::dbGetQuery(con,
    "SELECT * FROM ground_truth_evaluation WHERE patient_id='P08';")
  expect_identical(nrow(gt), 3L) # one hidden row per day
  expect_true(all(c("latent_risk", "hazard", "fall_sampled",
    "intervention_fired") %in% names(gt)))
  # No exported getter exists for the restricted table.
  expect_false(exists("cdt_sim_get_ground_truth"))
})

test_that("no clinical surface leaks latent/hazard/fall fields (#7)", {
  con <- .orch_con()
  on.exit(DBI::dbDisconnect(con))
  model <- .orch_model()
  cdt_run_simulation(con, model, "sim1_baseline", "B",
    days = 3, seed = 111, mock = TRUE)
  leak_pat <- "latent|hazard|fall_sampled|intervention_fired"

  # cdt_patient_context (LLM-facing text): no restricted terms.
  ctx <- cdt_patient_context(con, model, "P08")
  expect_false(any(grepl(leak_pat, ctx, ignore.case = TRUE)))

  # cdt_cohort_snapshot: only public risk columns, no restricted fields.
  snap <- cdt_cohort_snapshot(con, model)
  expect_false(any(grepl(leak_pat, names(snap), ignore.case = TRUE)))

  # cdt_patient_risk: prediction object exposes no restricted fields.
  risk <- cdt_patient_risk(con, model, "P08")
  expect_false(any(grepl(leak_pat, names(risk), ignore.case = TRUE)))
})
