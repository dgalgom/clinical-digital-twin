#!/usr/bin/env Rscript
# Stress 8.3 - HOSPITALIZATION GAP + READMISSION.
# A resident (P03) is hospitalised mid-run: several days of sensor rows are
# removed (a wear-time / data gap), then readmitted. We confirm the model still
# produces a prediction after the gap (no crash on a discontinuous timeline) and
# log the gap as a WARN obstacle.
#
#   Rscript data-raw/stress_scenarios/8_3_hospitalization_gap.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_hospitalization"
GAP_PATIENT <- "P03"
con <- stress_new_con()
model <- stress_model()

res <- cdt_run_simulation(con, model, SIM, "A", days = 9, seed = 80301L,
  mock = TRUE)
message(sprintf("ran %d day(s)", res$days_completed))

# Simulate a hospital stay: delete this patient's rows for days 4-6 (gap), which
# leaves a discontinuous timeline on readmission.
before <- DBI::dbGetQuery(con, sprintf(
  "SELECT COUNT(*) n FROM sensor_readings WHERE simulation_id='%s' AND patient_id='%s';",
  SIM, GAP_PATIENT))$n
DBI::dbExecute(con, sprintf(
  "DELETE FROM sensor_readings
     WHERE simulation_id='%s' AND branch='A' AND patient_id='%s'
       AND day IN (4,5,6);", SIM, GAP_PATIENT))
after <- DBI::dbGetQuery(con, sprintf(
  "SELECT COUNT(*) n FROM sensor_readings WHERE simulation_id='%s' AND patient_id='%s';",
  SIM, GAP_PATIENT))$n
message(sprintf("hospitalization gap: %d -> %d rows for %s",
  before, after, GAP_PATIENT))

# The model must still predict on the post-gap (readmission) timeline.
patient <- cdt_sim_patients()[cdt_sim_patients()$patient_id == GAP_PATIENT, ]
pred <- tryCatch(
  .cdt_sim_patient_risk(con, model, SIM, "A", patient),
  error = function(e) NULL)
ok <- !is.null(pred) && is.finite(pred$p_7d) && pred$p_7d >= 0 && pred$p_7d <= 1
cdt_sim_log_checkpoint(con, SIM, "A", 9L, "readmission_predict",
  if (ok) "warn" else "fail",
  sprintf("%s hospitalised days 4-6, readmitted; post-gap prediction %s",
    GAP_PATIENT, if (ok) "produced" else "FAILED"))

stress_report(con, SIM, "A", "8.3 Hospitalization gap + readmission")
if (!ok) message("UNEXPECTED: model failed on the post-gap timeline.")
DBI::dbDisconnect(con)
