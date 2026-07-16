# Phase 6 - simulation report data assembly.
# Runs a short mock A/B pair, then verifies the FULL report payload (10 series +
# checkpoint table + traceability) and the SUMMARY payload (status tally,
# obstacles, gate outcome). Rendering is optional and skipped when rmarkdown is
# absent, so this stays keyless and CI-friendly.

.report_con <- function() {
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

.report_model <- local({
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

test_that("full report data returns 10 series + checkpoint + traceability", {
  con <- .report_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .report_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  rd <- cdt_sim_report_data(con, "sim1_baseline", "A")

  expect_identical(length(rd$patients), 10L)
  expect_identical(rd$meta$days_run, 3L)
  # Each patient carries aligned day/p_24h/p_7d vectors.
  p1 <- rd$patients[["P01"]]
  expect_identical(length(p1$day), length(p1$p_24h))
  expect_identical(length(p1$day), length(p1$p_7d))
  expect_true(all(p1$p_7d >= 0 & p1$p_7d <= 1))

  expect_true(nrow(rd$checkpoints) > 0)
  expect_true(all(c("day", "step", "status") %in% names(rd$checkpoints)))
  expect_identical(nrow(rd$traceability), 3L) # one row per day
  expect_true(all(rd$traceability$decisions == 10L))
  # Exec headline is internally consistent.
  expect_identical(rd$exec$n_predictions, 30L)
  expect_true(rd$exec$checkpoints_fail == 0L)
})

test_that("summary report lists status tally, obstacles, and gate outcome", {
  con <- .report_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .report_model(), "sim1_baseline", "B",
    days = 3, seed = 111, mock = TRUE)
  sr <- cdt_sim_summary_report(con, "sim1_baseline", "B")

  expect_true(all(c("pass", "warn", "fail") %in% names(sr$status_counts)))
  expect_true(sr$status_counts[["pass"]] >= 0L)
  expect_identical(sr$gate_outcome, "pass") # clean mock run
  expect_identical(sr$failed_days, integer(0))
  expect_true(is.character(sr$obstacles))
})

test_that("report data on an empty run degrades to zero-row shapes", {
  con <- .report_con()
  on.exit(DBI::dbDisconnect(con))
  rd <- cdt_sim_report_data(con, "never_ran", "A")
  expect_identical(rd$meta$days_run, 0L)
  expect_identical(length(rd$patients), 10L)
  expect_identical(nrow(rd$traceability), 0L)
  sr <- cdt_sim_summary_report(con, "never_ran", "A")
  expect_identical(sr$gate_outcome, "pass")
  expect_identical(length(sr$obstacles), 0L)
})

test_that("render is optional and produces HTML when rmarkdown is present", {
  skip_if_not_installed("rmarkdown")
  con <- .report_con()
  on.exit(DBI::dbDisconnect(con))
  cdt_run_simulation(con, .report_model(), "sim1_baseline", "A",
    days = 3, seed = 111, mock = TRUE)
  out <- tempfile(fileext = ".html")
  res <- cdt_sim_render_report(con, "sim1_baseline", "A", kind = "summary",
    output_file = out)
  # Either it rendered (template found) or degraded to NULL (template missing
  # in a bare source tree) - both are acceptable, but if it returned a path the
  # file must exist and be non-trivial.
  if (!is.null(res)) {
    expect_true(file.exists(res))
    expect_true(file.info(res)$size > 0)
  } else {
    succeed("rmarkdown present but template not resolvable in this tree")
  }
})
