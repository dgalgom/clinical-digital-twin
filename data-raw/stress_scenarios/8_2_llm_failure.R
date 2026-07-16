#!/usr/bin/env Rscript
# Stress 8.2 - SUSTAINED LLM FAILURE.
# Simulates an agent backend that returns unparseable output every day. The
# orchestrator's degrade path (reuse prior decision, flag agent_output_invalid)
# should keep the run alive; we then confirm the degraded days were recorded so
# the failure is auditable rather than silent.
#
#   Rscript data-raw/stress_scenarios/8_2_llm_failure.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_llm_failure"
con <- stress_new_con()
model <- stress_model()

# Force the agent path to always fail parsing by overriding the mock JSON
# generator to emit prose (no JSON object). This exercises the retry -> reuse
# -> invalid-flag degrade path in cdt_call_agent for every patient, every day.
orig <- get(".cdt_mock_agent_json", envir = globalenv())
assign(".cdt_mock_agent_json",
  function(patient, day, ctx = list()) "sorry, the model is unavailable today",
  envir = globalenv())
on.exit(assign(".cdt_mock_agent_json", orig, envir = globalenv()), add = TRUE)

res <- cdt_run_simulation(con, model, SIM, "A", days = 7, seed = 80201L,
  mock = TRUE)
message(sprintf("ran %d day(s) under sustained LLM failure",
  res$days_completed))

dec <- cdt_sim_get_agent_decisions(con, SIM, "A")
degraded <- sum(as.integer(dec$agent_output_invalid), na.rm = TRUE)
message(sprintf("degraded decisions (invalid -> reused): %d / %d",
  degraded, nrow(dec)))
cdt_sim_log_checkpoint(con, SIM, "A", 7L, "agent_degrade",
  if (degraded > 0) "warn" else "pass",
  sprintf("%d degraded decisions recorded", degraded))

stress_report(con, SIM, "A", "8.2 Sustained LLM failure")
if (degraded == 0) {
  message("UNEXPECTED: no degraded decisions were flagged.")
}
DBI::dbDisconnect(con)
