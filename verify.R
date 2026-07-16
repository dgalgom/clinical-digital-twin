#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# One-command end-to-end verification for external users.
#
#   Rscript verify.R
#
# Confirms a fresh checkout is functional and usable WITHOUT any API keys:
#   1. Required packages are installed.
#   2. The demo dataset + model exist (builds them if missing).
#   3. The SQLite database is queryable (cohort, timeline).
#   4. The digital twin predicts and responds to a counterfactual.
#   5. The REST API router builds (plumber).
#   6. The Telegram bot replies in deterministic mock mode.
#   7. Shift-triage change detection surfaces movement alerts.
#   8. The statistical-adequacy checkpoint passes.
#   9. The multi-agent simulation runs a 3-day mock A/B pair with a valid,
#      non-leaking hidden P08 experiment.
#
# Prints a numbered PASS/FAIL log and exits non-zero on the first failure, so it
# doubles as a CI gate. No network access is required.
# ---------------------------------------------------------------------------

# --- Resolve root + load code ---------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) {
  normalizePath(dirname(file_arg))
} else {
  normalizePath(getwd())
}
Sys.setenv(CDT_PROJECT_ROOT = root)
# Force offline mock mode so verification never needs keys or the network.
Sys.setenv(CDT_MOCK_LLM = "1", CDT_MOCK_TELEGRAM = "1")

ok <- function(msg) cat(sprintf("[ok]   %s\n", msg))
fail <- function(msg) {
  cat(sprintf("[FAIL] %s\n", msg))
  quit(status = 1, save = "no")
}
step <- function(n, msg) cat(sprintf("\n(%d) %s\n", n, msg))

# --- 1. Packages ----------------------------------------------------------
step(1, "Checking required packages")
required <- c(
  "DBI", "RSQLite", "plogr", "memoise", "bit64", "blob", "rlang", "dplyr", "tidyr", "tibble", "jsonlite", "sodium",
  "plumber", "httr2", "testthat"
)
missing <- required[!vapply(required, requireNamespace, logical(1),
  quietly = TRUE)]
if (length(missing) > 0) {
  fail(sprintf("missing packages: %s (run: Rscript setup.R)",
    paste(missing, collapse = ", ")))
}
ok(sprintf("all %d core packages installed", length(required)))
# Shiny/plotly/DT are dashboard-only; note if absent but don't fail.
for (p in c("shiny", "plotly", "DT")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("       (note: '%s' not installed; dashboard won't run)\n", p))
  }
}

# --- Load package source --------------------------------------------------
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# --- 2. Dataset + model ---------------------------------------------------
step(2, "Ensuring demo dataset + model exist")
db_path <- cdt_db_path()
model_path <- cdt_model_path()
if (!file.exists(db_path) || !file.exists(model_path)) {
  cat("       building (this runs data-raw/generate_synthetic_data.R)...\n")
  gen <- file.path(root, "data-raw", "generate_synthetic_data.R")
  source(gen, local = new.env())
}
if (!file.exists(db_path)) fail("SQLite database was not created")
if (!file.exists(model_path)) fail("model .rds was not created")
ok("database and model present")

# --- 3. Database queryable ------------------------------------------------
step(3, "Querying the database")
con <- cdt_db_connect()
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
cohort <- cdt_get_cohort(con)
if (nrow(cohort) == 0) fail("cohort is empty")
pid <- cohort$patient_id[[1]]
tl <- cdt_get_patient_timeline(con, pid)
if (nrow(tl) == 0) fail(sprintf("no sensor timeline for %s", pid))
# Confirm accelerometry channels are ingested and stored.
if (!all(c("accel_counts", "accel_magnitude") %in% names(tl))) {
  fail("accelerometry columns missing from sensor_readings")
}
ok(sprintf("cohort=%d patients; %s has %d daily readings incl. accelerometry",
  nrow(cohort), pid, nrow(tl)))

# --- 4. Digital twin prediction + counterfactual --------------------------
step(4, "Running the digital twin")
model <- cdt_load_model()
base <- cdt_patient_risk(con, model, pid)
if (is.null(base) || !is.finite(base$p_7d)) fail("baseline prediction failed")
cf <- cdt_patient_risk(con, model, pid,
  modified_inputs = list(steps_pct = 30, sedentary_hours_mean_7d = 4),
  include_baseline = TRUE)
if (is.null(cf$delta)) fail("counterfactual prediction failed")
ok(sprintf("%s baseline 7d=%.1f%% (%s); what-if (more steps) -> %.1f%% (delta %+.1f pts)",
  pid, 100 * base$p_7d, base$tier_7d, 100 * cf$p_7d, 100 * cf$delta$p_7d))

# --- 5. REST API router builds --------------------------------------------
step(5, "Building the REST API router")
pr <- plumber::plumb(file.path(root, "api", "plumber.R"))
if (!inherits(pr, "Plumber")) fail("plumber router did not build")
ok("plumber router builds")

# --- 6. Telegram bot (mock) -----------------------------------------------
step(6, "Exercising the Telegram bot in mock mode")
cdt_bot_reset()
invisible(cdt_telegram_sent(clear = TRUE))
# Username gate: identify with the seeded demo clinician before querying.
gate <- cdt_bot_handle_message(con, model, chat_id = 999, text = "login as clinician")
if (!grepl("signed in", gate, ignore.case = TRUE)) {
  fail("bot username gate did not accept 'clinician'")
}
reply <- cdt_bot_handle_message(con, model, chat_id = 999,
  text = sprintf("How is patient %s trending?", pid))
if (!nzchar(reply)) fail("bot returned an empty reply")
ok(sprintf("bot replied (%d chars) in deterministic mock mode", nchar(reply)))

# --- 7. Shift-triage change detection (P0-1) ------------------------------
step(7, "Detecting shift-triage change alerts")
# Read-only pass (write_snapshot = FALSE) so verification never mutates the
# demo DB's snapshot history. The data-raw seeder plants a "previous shift"
# snapshot, so a fresh build should surface at least one movement alert.
fired <- tryCatch(
  cdt_compute_alerts(con, model, as_of = "verify", write_snapshot = FALSE),
  error = function(e) {
    fail(sprintf("cdt_compute_alerts errored: %s", conditionMessage(e)))
  })
n_prev <- nrow(cdt_get_last_snapshot(con))
ok(sprintf("compared against last snapshot (%d rows); %d change alert(s) detected",
  n_prev, nrow(fired)))

try(DBI::dbDisconnect(con), silent = TRUE)

# --- 8. Statistical-adequacy checkpoint -----------------------------------
step(8, "Running the statistical-adequacy checkpoint")
eval_script <- file.path(root, "checkpoints", "evaluate_model.R")
rc <- system2("Rscript", eval_script, stdout = FALSE, stderr = FALSE,
  env = sprintf("CDT_PROJECT_ROOT=%s", root))
if (!identical(rc, 0L)) {
  fail("checkpoints/evaluate_model.R reported statistical inadequacy")
}
ok("model passed AUC/Brier/directionality/latency checks")

# --- 9. Multi-agent simulation smoke (mock) -------------------------------
step(9, "Running a 3-day multi-agent simulation (mock)")
# Isolated throwaway DB so the demo database is never touched. Runs a short
# mock A/B pair, then asserts predictions are valid, the day-gate is logged,
# the hidden P08 ground truth is populated, and no latent/hazard/fall field
# leaks onto any clinical surface.
sim_con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
cdt_db_init_schema(sim_con)
cdt_sim_init_schema(sim_con)
sim_cols <- c(
  "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
  "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
  "medications", "comorbidities"
)
cdt_db_write(sim_con, "patients", cdt_sim_patients()[, sim_cols], append = TRUE)
sa <- cdt_run_simulation(sim_con, model, "verify_sim", "A", days = 3,
  seed = 424242L, mock = TRUE)
sb <- cdt_run_simulation(sim_con, model, "verify_sim", "B", days = 3,
  seed = 424242L, mock = TRUE)
if (sa$days_completed != 3L || sb$days_completed != 3L) {
  fail("3-day mock A/B simulation did not complete")
}
if (!identical(sa$p08_onset, sb$p08_onset)) {
  fail("A/B branches drew different P08 onset (RNG stream not shared)")
}
sim_preds <- cdt_sim_get_predictions(sim_con, "verify_sim", "A")
if (nrow(sim_preds) == 0 ||
    !all(sim_preds$p_7d >= 0 & sim_preds$p_7d <= 1) ||
    !all(sim_preds$p_24h >= 0 & sim_preds$p_24h <= 1)) {
  fail("simulation predictions out of [0,1]")
}
sim_ck <- cdt_sim_get_checkpoints(sim_con, "verify_sim", "A")
if (nrow(sim_ck) == 0) fail("no daily checkpoints were logged")
sim_gt <- DBI::dbGetQuery(sim_con,
  "SELECT COUNT(*) n FROM ground_truth_evaluation WHERE patient_id='P08';")$n
if (sim_gt == 0) fail("hidden P08 ground truth not populated")
leak_pat <- "latent|hazard|fall_sampled|intervention_fired"
sim_ctx <- cdt_patient_context(sim_con, model, "P08")
sim_snap <- cdt_cohort_snapshot(sim_con, model)
sim_risk <- cdt_patient_risk(sim_con, model, "P08")
if (any(grepl(leak_pat, sim_ctx, ignore.case = TRUE)) ||
    any(grepl(leak_pat, names(sim_snap), ignore.case = TRUE)) ||
    any(grepl(leak_pat, names(sim_risk), ignore.case = TRUE)) ||
    exists("cdt_sim_get_ground_truth")) {
  fail("latent/hazard/fall field leaked onto a clinical surface")
}
try(DBI::dbDisconnect(sim_con), silent = TRUE)
ok(sprintf(paste0("3-day A/B sim complete; P08 onset day %d shared; ",
  "%d predictions in [0,1]; %d checkpoints; no ground-truth leak"),
  sa$p08_onset, nrow(sim_preds), nrow(sim_ck)))

cat("\n============================================================\n")
cat("VERIFICATION PASSED - the system is functional end-to-end.\n")
cat("Next: launch the dashboard with\n")
cat("  Rscript -e \"shiny::runApp('app.R', port=3838, launch.browser=TRUE)\"\n")
cat("  (demo login: clinician / demo1234)\n")
cat("============================================================\n")
