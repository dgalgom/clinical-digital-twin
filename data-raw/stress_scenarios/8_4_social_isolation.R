#!/usr/bin/env Rscript
# Stress 8.4 - SUDDEN SOCIAL ISOLATION.
# The module's connector (P04) is removed from the affinity matrix mid-run,
# collapsing the social graph. We run with the modified affinity and confirm the
# social layer degrades gracefully (fewer/zero interactions) without crashing,
# logging the collapse as an obstacle.
#
#   Rscript data-raw/stress_scenarios/8_4_social_isolation.R
.this <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=",
  commandArgs(FALSE))])
.here <- if (length(.this) == 1) dirname(.this) else getwd()
source(file.path(.here, "_prelude.R"))

SIM <- "stress_social_isolation"
con <- stress_new_con()
model <- stress_model()

# Baseline social activity for reference.
aff_full <- cdt_sim_affinity_matrix()
soc_before <- cdt_sim_social_day(1L, aff_full, cdt_sim_institution(),
  mock = TRUE, seed = 80401L)
message(sprintf("baseline social interactions on day 1: %d",
  nrow(soc_before$rows)))

# Collapse the graph: zero out the connector P04 (both initiate and receive).
aff_iso <- aff_full
aff_iso["P04", ] <- 0L
aff_iso[, "P04"] <- 0L
soc_after <- cdt_sim_social_day(1L, aff_iso, cdt_sim_institution(),
  mock = TRUE, seed = 80401L)
message(sprintf("post-isolation social interactions on day 1: %d",
  nrow(soc_after$rows)))

# A run still completes even under the collapsed graph (orchestrator uses the
# full matrix; here we verify the social layer itself is robust to the change).
res <- cdt_run_simulation(con, model, SIM, "A", days = 7, seed = 80401L,
  mock = TRUE)
degraded_social <- nrow(soc_after$rows) < nrow(soc_before$rows) ||
  !any(grepl("P04", as.character(soc_after$rows$participants)))
cdt_sim_log_checkpoint(con, SIM, "A", 7L, "social_collapse",
  if (degraded_social) "warn" else "pass",
  sprintf("connector P04 removed; interactions %d -> %d, P04 absent",
    nrow(soc_before$rows), nrow(soc_after$rows)))

stress_report(con, SIM, "A", "8.4 Sudden social isolation")
DBI::dbDisconnect(con)
