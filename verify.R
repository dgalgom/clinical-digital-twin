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
#   7. The statistical-adequacy checkpoint passes.
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
  "DBI", "RSQLite", "dplyr", "tidyr", "tibble", "jsonlite", "sodium",
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
reply <- cdt_bot_handle_message(con, model, chat_id = 999,
  text = sprintf("How is patient %s trending?", pid))
if (!nzchar(reply)) fail("bot returned an empty reply")
ok(sprintf("bot replied (%d chars) in deterministic mock mode", nchar(reply)))

try(DBI::dbDisconnect(con), silent = TRUE)

# --- 7. Statistical-adequacy checkpoint -----------------------------------
step(7, "Running the statistical-adequacy checkpoint")
eval_script <- file.path(root, "checkpoints", "evaluate_model.R")
rc <- system2("Rscript", eval_script, stdout = FALSE, stderr = FALSE,
  env = sprintf("CDT_PROJECT_ROOT=%s", root))
if (!identical(rc, 0L)) {
  fail("checkpoints/evaluate_model.R reported statistical inadequacy")
}
ok("model passed AUC/Brier/directionality/latency checks")

cat("\n============================================================\n")
cat("VERIFICATION PASSED - the system is functional end-to-end.\n")
cat("Next: launch the dashboard with\n")
cat("  Rscript -e \"shiny::runApp('app.R', port=3838, launch.browser=TRUE)\"\n")
cat("  (demo login: clinician / demo1234)\n")
cat("============================================================\n")
