#!/usr/bin/env Rscript
# Stress 8.5 - STAFFING CRISIS.
# A severe staffing shortfall is embedded as an extended flu-style outbreak
# (reduced staffing + lowered mobility across the whole module). We run it and
# confirm the biological gate still ACCEPTS the depressed-activity days (a
# staffing crisis lowers activity but does not make readings physically
# impossible) - i.e. no false FAILs on a legitimately low-activity module.
#
#   Rscript data-raw/stress_scenarios/8_5_staffing_crisis.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_staffing_crisis"
con <- stress_new_con()
model <- stress_model()

# Run WITH flu enabled (the config's staff_reduction + mobility_multiplier is
# the closest existing lever for a staffing crisis) over the outbreak window.
res <- cdt_run_simulation(con, model, SIM, "A", days = 14, seed = 80501L,
  mock = TRUE, flu = TRUE)
message(sprintf("ran %d day(s) through the crisis window", res$days_completed))

ck <- cdt_sim_get_checkpoints(con, SIM, "A")
bio_fail <- ck[ck$step == "biological" & ck$status == "fail", , drop = FALSE]
crisis_ok <- nrow(bio_fail) == 0
cdt_sim_log_checkpoint(con, SIM, "A", res$days_completed, "crisis_tolerance",
  if (crisis_ok) "pass" else "fail",
  sprintf("%d biological FAILs during depressed-activity window",
    nrow(bio_fail)))

stress_report(con, SIM, "A", "8.5 Staffing crisis")
if (!crisis_ok) {
  message("UNEXPECTED: biological gate FAILED on legitimately low-activity days.")
}
DBI::dbDisconnect(con)
