#!/usr/bin/env Rscript
# Stress 8.1 - SENSOR CORRUPTION.
# A short baseline run, then a day of deliberately corrupted sensor rows
# (negative steps, posture hours that do not sum to 24) is fed to the biological
# validator to confirm the checkpoint CATCHES it (an honest FAIL, logged).
#
#   Rscript data-raw/stress_scenarios/8_1_sensor_corruption.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_sensor_corruption"
con <- stress_new_con()
model <- stress_model()

# Clean baseline (7 days).
res <- cdt_run_simulation(con, model, SIM, "A", days = 7, seed = 80101L,
  mock = TRUE)
message(sprintf("baseline: %d clean day(s)", res$days_completed))

# Now corrupt a reading and run it through the validator directly, logging the
# outcome as a scenario checkpoint. This proves the biological gate rejects
# physically impossible sensor data.
bad <- data.frame(
  patient_id = "P01", ts = "2026-01-08T06:00:00+0100",
  heart_rate = 70, resting_hr = 68, sbp = 130, dbp = 80,
  step_count = -500L,          # impossible: negative steps
  accel_counts = 4000L, accel_magnitude = 1.02,
  hours_sitting = 10, hours_lying = 10, hours_standing = 10, # sums to 30
  stringsAsFactors = FALSE
)
v <- validate_biological_plausibility(bad)
cdt_sim_log_checkpoint(con, SIM, "A", 8L, "biological", v$status,
  paste(v$issues, collapse = "; "))
cdt_sim_log_checkpoint(con, SIM, "A", 8L, "gate", v$status,
  "injected corrupted sensor row")
message(sprintf("corrupted-day biological status: %s", v$status))

stress_report(con, SIM, "A", "8.1 Sensor corruption")
if (v$status != "fail") {
  message("UNEXPECTED: validator did not FAIL on corrupted data.")
}
DBI::dbDisconnect(con)
