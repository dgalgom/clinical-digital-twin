#!/usr/bin/env Rscript
# Stress 8.6 - ALERT BURST.
# A model-output validator is exercised against a burst of predictions with
# large day-over-day jumps to confirm the model gate WARNs (not FAILs) on rapid
# swings, so a genuine deterioration burst is surfaced but never silently halts
# the run. Honest accounting of how many predictions warned.
#
#   Rscript data-raw/stress_scenarios/8_6_alert_burst.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_alert_burst"
con <- stress_new_con()
model <- stress_model()

res <- cdt_run_simulation(con, model, SIM, "A", days = 7, seed = 80601L,
  mock = TRUE)
message(sprintf("ran %d day(s)", res$days_completed))

# Feed the model-output validator a burst of predictions whose day-over-day 7d
# jumps EXCEED the validator's 0.5 swing threshold, so each is surfaced as a
# WARN (a deterioration burst the clinician should see) but never a FAIL.
burst_p <- c(0.05, 0.60, 0.05, 0.70)  # jumps of 0.55, 0.55, 0.65 (> 0.5)
warns <- 0L
fails <- 0L
prior <- NULL
for (p in burst_p) {
  pred <- list(p_24h = p * 0.6, p_7d = p, tier_24h = "Moderate", tier_7d = "High")
  v <- validate_model_output(pred, prior_p7d = prior)
  if (v$status == "warn") warns <- warns + 1L
  if (v$status == "fail") fails <- fails + 1L
  prior <- p
}
message(sprintf("alert burst: %d WARN, %d FAIL over %d steps",
  warns, fails, length(burst_p)))
cdt_sim_log_checkpoint(con, SIM, "A", res$days_completed, "alert_burst",
  if (fails == 0) "warn" else "fail",
  sprintf("%d/%d steep prediction jumps flagged as WARN, %d FAIL",
    warns, length(burst_p), fails))

stress_report(con, SIM, "A", "8.6 Alert burst")
if (fails > 0) message("UNEXPECTED: a steep-but-valid jump FAILED the gate.")
DBI::dbDisconnect(con)
