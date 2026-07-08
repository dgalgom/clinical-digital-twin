#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Reproducibly build the entire demo dataset and train the fall-risk model.
#
# Run from the project root:
#   Rscript data-raw/generate_synthetic_data.R
#
# This script:
#   1. Generates a synthetic patient cohort (NO real PHI).
#   2. Simulates daily wearable sensor streams + simulated fall events.
#   3. Writes everything into a fresh SQLite database.
#   4. Also exports a raw CSV so the ingestion pipeline can be demoed.
#   5. Builds the labeled training table, fits the models, and persists them.
#   6. Seeds a demo clinician login.
#
# Everything is seeded for reproducibility.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tibble)
}))

# Resolve project root: this script lives in <root>/data-raw/.
# Prefer an already-set CDT_PROJECT_ROOT (e.g. when this script is source()d
# from setup.R). Otherwise derive it from this file's own path via commandArgs().
# Note: commandArgs() reflects the *outermost* Rscript --file, so relying on it
# alone breaks when the script is sourced from another script.
env_root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
if (!is.na(env_root) && nzchar(env_root) && dir.exists(file.path(env_root, "R"))) {
  root <- normalizePath(env_root)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(file_arg) == 1) {
    root <- normalizePath(file.path(dirname(file_arg), ".."))
  } else {
    root <- normalizePath(getwd())
  }
}
Sys.setenv(CDT_PROJECT_ROOT = root)
message("Project root: ", root)

# Source package code directly (no install needed for the hackathon).
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

set.seed(42)

# --- 1. Cohort ------------------------------------------------------------
message("Generating synthetic cohort...")
# 100 patients x 90 daily read-outs: sized so the 7-day horizon has enough
# events (~50) for stable coefficient signs and a held-out 7d AUC well above
# chance (see checkpoints/evaluate_model.R). Smaller cohorts leave the 7d model
# under-identified and flip weaker coefficient signs.
cohort <- cdt_generate_cohort(n = 100, seed = 42)
message(sprintf("  %d patients generated.", nrow(cohort)))

# Export a raw institution-style CSV to demonstrate ingestion.
dir.create(file.path(root, "data-raw"), showWarnings = FALSE)
raw_csv <- file.path(root, "data-raw", "example_institution_patients.csv")
write.csv(
  cohort %>%
    rename(id = patient_id, gender = sex, previous_falls = prior_falls),
  raw_csv,
  row.names = FALSE
)
message("  Raw CSV written: ", raw_csv)

# Round-trip through the ingestion pipeline to prove it works.
cohort <- cdt_ingest_patient_csv(raw_csv)
cdt_validate_patients(cohort)
message("  Ingestion + validation OK.")

# --- 2. Sensors + falls ---------------------------------------------------
message("Simulating wearable sensor streams...")
sim <- cdt_simulate_cohort_sensors(cohort, days = 90,
  start_date = as.Date("2026-01-01"), seed = 7)
message(sprintf("  %d sensor readings, %d fall events.",
  nrow(sim$readings), nrow(sim$falls)))

# --- 3. Database ----------------------------------------------------------
db_path <- cdt_db_path()
if (file.exists(db_path)) file.remove(db_path)
message("Writing SQLite database: ", db_path)
con <- cdt_db_connect(db_path)
# Explicit disconnect at the end (not on.exit): this script is also source()d
# from setup.R, where on.exit would register on the caller's frame and could
# close the connection prematurely.
cdt_db_init_schema(con)

cdt_db_write(con, "patients", cohort, append = TRUE)
cdt_db_write(con, "sensor_readings", sim$readings, append = TRUE)
if (nrow(sim$falls) > 0) {
  cdt_db_write(con, "fall_events", sim$falls, append = TRUE)
}

# Seed a demo clinician. Password is intentionally simple for the demo ONLY.
if (nrow(dbGetQuery(con, "SELECT 1 FROM users WHERE username='clinician';")) == 0) {
  cdt_create_user(con, "clinician", "demo1234", role = "clinician")
  message("  Demo login seeded: clinician / demo1234")
}

# --- 4. Train models ------------------------------------------------------
message("Building training table + fitting models...")
training <- cdt_build_training_table(cohort, sim$readings, sim$falls,
  window_days = 7, stride = 2)
message(sprintf("  Training rows: %d  (24h prevalence=%.3f, 7d prevalence=%.3f)",
  nrow(training), mean(training$label_24h), mean(training$label_7d)))

model <- cdt_fit_model(training)
cdt_save_model(model, cdt_model_path())
print(model)
message("  Model saved: ", cdt_model_path())

dbDisconnect(con)
message("\nDone. Demo dataset + model are ready.")
