# Shared prelude for the stress-scenario scripts.
#
# Each scenario is a short, self-contained, INDEPENDENT-seed run against its own
# throwaway SQLite file and its own simulation_id, followed by a SUMMARY report
# (obstacles + which checkpoints passed/failed). Source this from each scenario
# to get: forced mock mode, project root + package sourced, a fresh sim DB with
# both schemas + the 10 residents, and a fixture model.
#
# This prelude adds NO new clinical logic; it only wires up the existing
# orchestrator + report helpers for a scenario driver.

suppressWarnings(suppressMessages({
  library(DBI)
  library(RSQLite)
}))

# Stress scenarios always run in mock mode (deterministic, keyless, fast).
Sys.setenv(CDT_MOCK_LLM = "1")

.stress_root <- local({
  env_root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
  if (!is.na(env_root) && nzchar(env_root) &&
    dir.exists(file.path(env_root, "R"))) {
    normalizePath(env_root)
  } else {
    a <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", a[grep("^--file=", a)])
    if (length(file_arg) == 1) {
      normalizePath(file.path(dirname(file_arg), "..", ".."))
    } else {
      normalizePath(getwd())
    }
  }
})
Sys.setenv(CDT_PROJECT_ROOT = .stress_root)
for (f in list.files(file.path(.stress_root, "R"), pattern = "\\.R$",
  full.names = TRUE)) {
  source(f)
}

# Build a fresh, isolated simulation DB seeded with the 10 residents.
stress_new_con <- function() {
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

# A reproducible fixture model shared by the scenarios.
stress_model <- function() {
  m <- tryCatch(cdt_load_model(), error = function(e) NULL)
  if (!is.null(m)) {
    return(m)
  }
  cohort <- cdt_generate_cohort(n = 80, seed = 1)
  s <- cdt_simulate_cohort_sensors(cohort, days = 100, seed = 2)
  training <- cdt_build_training_table(cohort, s$readings, s$falls,
    window_days = 7, stride = 2)
  cdt_fit_model(training)
}

# Print an honest summary of a scenario run: tally, failed days, obstacles.
stress_report <- function(con, sim, branch, title) {
  sr <- cdt_sim_summary_report(con, sim, branch)
  message(sprintf("\n--- %s ---", title))
  message(sprintf("  run: %s/%s  days: %d  gate: %s", sim, branch,
    sr$meta$days_run, toupper(sr$gate_outcome)))
  message(sprintf("  checkpoints  pass=%d  warn=%d  fail=%d",
    sr$status_counts[["pass"]], sr$status_counts[["warn"]],
    sr$status_counts[["fail"]]))
  if (length(sr$failed_days) > 0) {
    message(sprintf("  failed gate day(s): %s",
      paste(sr$failed_days, collapse = ", ")))
  }
  if (length(sr$obstacles) == 0) {
    message("  obstacles: none (clean run)")
  } else {
    message("  obstacles:")
    for (o in sr$obstacles) message(sprintf("    - %s", o))
  }
  invisible(sr)
}
