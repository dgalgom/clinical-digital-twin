#!/usr/bin/env Rscript
# End-to-end simulation smoke check (no network; mock LLM).
# Runs a 3-day mock A/B pair on a throwaway SQLite, asserting the run completes,
# predictions are valid, the P08 ground truth is populated, and nothing on the
# clinical surface leaks the hidden latent/hazard/fall fields.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) normalizePath(file.path(dirname(file_arg), "..")) else normalizePath(getwd())
Sys.setenv(CDT_PROJECT_ROOT = root)
Sys.setenv(CDT_MOCK_LLM = "1")

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

# Fresh isolated DB + both schemas + the 10 residents.
con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
cdt_db_init_schema(con)
cdt_sim_init_schema(con)
canonical <- c(
  "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
  "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
  "medications", "comorbidities"
)
cdt_db_write(con, "patients", cdt_sim_patients()[, canonical], append = TRUE)

# A reproducible fixture model.
cohort <- cdt_generate_cohort(n = 60, seed = 1)
sim <- cdt_simulate_cohort_sensors(cohort, days = 90, seed = 2)
training <- cdt_build_training_table(cohort, sim$readings, sim$falls,
  window_days = 7, stride = 2)
model <- cdt_fit_model(training)

# 1. A/B run completes 3 days.
a <- cdt_run_simulation(con, model, "sim1_baseline", "A", days = 3,
  seed = 111, mock = TRUE)
b <- cdt_run_simulation(con, model, "sim1_baseline", "B", days = 3,
  seed = 111, mock = TRUE)
stopifnot(a$days_completed == 3L, b$days_completed == 3L)
stopifnot(identical(a$p08_onset, b$p08_onset))
cat("[ok] 3-day A/B run completed with shared P08 onset\n")

# 2. Predictions valid.
preds <- cdt_sim_get_predictions(con, "sim1_baseline", "A")
stopifnot(nrow(preds) == 30L,
  all(preds$p_7d >= 0 & preds$p_7d <= 1),
  all(preds$p_24h >= 0 & preds$p_24h <= 1))
cat("[ok] predictions in [0,1] (", nrow(preds), "rows )\n")

# 3. Ground truth populated for P08.
gt <- DBI::dbGetQuery(con,
  "SELECT COUNT(*) n FROM ground_truth_evaluation WHERE patient_id='P08';")$n
stopifnot(gt > 0)
cat("[ok] ground truth populated for P08 (", gt, "rows )\n")

# 4. Non-leak: clinical surfaces expose no restricted fields.
leak_pat <- "latent|hazard|fall_sampled|intervention_fired"
ctx <- cdt_patient_context(con, model, "P08")
snap <- cdt_cohort_snapshot(con, model)
risk <- cdt_patient_risk(con, model, "P08")
stopifnot(
  !any(grepl(leak_pat, ctx, ignore.case = TRUE)),
  !any(grepl(leak_pat, names(snap), ignore.case = TRUE)),
  !any(grepl(leak_pat, names(risk), ignore.case = TRUE)),
  !exists("cdt_sim_get_ground_truth")
)
cat("[ok] no latent/hazard/fall leak on clinical surfaces\n")

DBI::dbDisconnect(con)
cat("\nSimulation integration check passed.\n")
