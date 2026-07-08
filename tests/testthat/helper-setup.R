# Shared fixtures: a small in-memory cohort + sensors + trained model.
# Sourced automatically by testthat before tests run.

if (!exists("cdt_generate_cohort")) {
  root <- Sys.getenv("CDT_PROJECT_ROOT", unset = normalizePath(file.path(getwd(), "..", "..")))
  for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    source(f)
  }
}

.test_fixture_cache <- new.env(parent = emptyenv())

make_test_fixtures <- function() {
  # Mirror the production sizing (100 patients x 90 daily read-outs) so the
  # fixture model is well-identified and its learned effect directions are
  # stable. Smaller cohorts have too few 7-day fall events to reliably recover
  # coefficient signs. Cached across tests in the same process for speed.
  if (!is.null(.test_fixture_cache$fx)) {
    return(.test_fixture_cache$fx)
  }
  cohort <- cdt_generate_cohort(n = 100, seed = 1)
  sim <- cdt_simulate_cohort_sensors(cohort, days = 90, seed = 2)
  training <- cdt_build_training_table(cohort, sim$readings, sim$falls,
    window_days = 7, stride = 2)
  model <- cdt_fit_model(training)
  fx <- list(cohort = cohort, sim = sim, training = training, model = model)
  .test_fixture_cache$fx <- fx
  fx
}
