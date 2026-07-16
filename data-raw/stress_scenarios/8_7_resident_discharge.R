#!/usr/bin/env Rscript
# Stress 8.7 - MID-SIM RESIDENT DISCHARGE.
# A resident (P10) is discharged part-way through: their rows stop after day 5.
# We confirm the run continues for the remaining residents and that a run-scoped
# prediction for the discharged resident still works on their truncated (pre-
# discharge) timeline, logging the discharge as an obstacle.
#
#   Rscript data-raw/stress_scenarios/8_7_resident_discharge.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_discharge"
DISCHARGED <- "P10"
con <- stress_new_con()
model <- stress_model()

res <- cdt_run_simulation(con, model, SIM, "A", days = 8, seed = 80701L,
  mock = TRUE)
message(sprintf("ran %d day(s)", res$days_completed))

# Discharge: remove the resident's rows for days > 5 (they left the module).
DBI::dbExecute(con, sprintf(
  "DELETE FROM sensor_readings
     WHERE simulation_id='%s' AND branch='A' AND patient_id='%s' AND day > 5;",
  SIM, DISCHARGED))
remaining <- DBI::dbGetQuery(con, sprintf(
  "SELECT COUNT(*) n FROM sensor_readings
     WHERE simulation_id='%s' AND patient_id='%s';", SIM, DISCHARGED))$n
message(sprintf("%s discharged after day 5; %d pre-discharge row(s) remain",
  DISCHARGED, remaining))

# The other residents' timelines are unaffected: spot-check P01 still predicts.
p01 <- cdt_sim_patients()[cdt_sim_patients()$patient_id == "P01", ]
pred01 <- tryCatch(.cdt_sim_patient_risk(con, model, SIM, "A", p01),
  error = function(e) NULL)
others_ok <- !is.null(pred01) && is.finite(pred01$p_7d)

# The discharged resident still resolves on their truncated timeline.
p10 <- cdt_sim_patients()[cdt_sim_patients()$patient_id == DISCHARGED, ]
pred10 <- tryCatch(.cdt_sim_patient_risk(con, model, SIM, "A", p10),
  error = function(e) NULL)
discharged_ok <- !is.null(pred10) && is.finite(pred10$p_7d)

cdt_sim_log_checkpoint(con, SIM, "A", 8L, "discharge",
  if (others_ok && discharged_ok) "warn" else "fail",
  sprintf("%s discharged after day 5; remaining cohort predicts=%s, truncated predicts=%s",
    DISCHARGED, others_ok, discharged_ok))

stress_report(con, SIM, "A", "8.7 Mid-sim resident discharge")
if (!(others_ok && discharged_ok)) {
  message("UNEXPECTED: a prediction failed after discharge.")
}
DBI::dbDisconnect(con)
