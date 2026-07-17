#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Executable driver for the multi-agent clinical simulation.
#
# Run from the project root:
#   CDT_MOCK_LLM=1 Rscript data-raw/run_simulation.R        # mock (default)
#   Rscript data-raw/run_simulation.R --live                # live LLM
#
# This script runs the 4 fixed runs (2 simulations x 2 branches):
#   sim1_baseline / A, sim1_baseline / B  (no flu)
#   sim2_flu      / A, sim2_flu      / B  (flu embedded, identical to A & B)
# into a SEPARATE data/simulation.sqlite so the demo DB is never clobbered.
#
# Mock-first policy: unless --live is passed, CDT_MOCK_LLM is forced to 1 and
# each run prints its per-day checkpoint gate outcomes + reasoning so the
# checkpoints can be walked through BEFORE any live-LLM run.
#
# On completion it generates the Sim 1 FULL report (both branches) and the
# Sim 2 SUMMARY report, when rmarkdown + pandoc are available.
# Everything is seeded and reproducible.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  library(DBI)
  library(RSQLite)
}))

args <- commandArgs(trailingOnly = TRUE)
live <- "--live" %in% args
days <- {
  d <- args[grepl("^--days=", args)]
  if (length(d) == 1) as.integer(sub("^--days=", "", d)) else 30L
}

# Mock-first: force mock unless the operator explicitly asks for --live.
if (!live) {
  Sys.setenv(CDT_MOCK_LLM = "1")
}
mock <- !live

# --- Resolve project root (same idiom as generate_synthetic_data.R) --------
env_root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
if (!is.na(env_root) && nzchar(env_root) && dir.exists(file.path(env_root, "R"))) {
  root <- normalizePath(env_root)
} else {
  a <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", a[grep("^--file=", a)])
  root <- if (length(file_arg) == 1) {
    normalizePath(file.path(dirname(file_arg), ".."))
  } else {
    normalizePath(getwd())
  }
}
Sys.setenv(CDT_PROJECT_ROOT = root)
message("Project root: ", root)
message(sprintf("Mode: %s  |  days: %d", if (mock) "MOCK" else "LIVE", days))

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# --- Live mode: load API keys from .env ------------------------------------
# The mock path needs no credentials, so keys are only loaded for --live.
# cdt_load_env is non-destructive (shell/CI vars already set always win).
if (live) {
  cdt_load_env(root)
  backend <- tryCatch(cdt_llm_backend(), error = function(e) "claude")
  if (cdt_llm_is_mock()) {
    stop("--live requested but no usable API key found; add ANTHROPIC_API_KEY ",
      "or GROQ_API_KEY to .env (see README).", call. = FALSE)
  }
  message(sprintf("Live LLM backend: %s", backend))
}

# --- Report rendering: make pandoc discoverable ----------------------------
# rmarkdown needs a pandoc executable. If one is not already visible, point it
# at a discoverable install (e.g. the pandoc bundled with RStudio/Quarto) so
# reports render regardless of the calling shell's PATH.
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) &&
    requireNamespace("rmarkdown", quietly = TRUE) &&
    !rmarkdown::pandoc_available()) {
  pandoc_candidates <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools",
    "/usr/local/bin", "/opt/homebrew/bin"
  )
  for (cand in pandoc_candidates) {
    if (file.exists(file.path(cand, "pandoc"))) {
      Sys.setenv(RSTUDIO_PANDOC = cand)
      break
    }
  }
}

# --- Separate simulation DB (never the demo DB) ----------------------------
dir.create(file.path(root, "data"), showWarnings = FALSE)
sim_db <- file.path(root, "data", "simulation.sqlite")
if (file.exists(sim_db)) file.remove(sim_db)
con <- cdt_db_connect(sim_db)
cdt_db_init_schema(con)
cdt_sim_init_schema(con)
message("Simulation DB: ", sim_db)

# Seed the 10 fixed residents (canonical clinical cols only).
canonical <- c(
  "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
  "orthostatic_hypotension", "polypharmacy", "prior_falls", "n_medications",
  "medications", "comorbidities"
)
cdt_db_write(con, "patients", cdt_sim_patients()[, canonical], append = TRUE)

# --- Model: reuse the trained demo model if present, else fit a fixture ----
model <- tryCatch(cdt_load_model(), error = function(e) NULL)
if (is.null(model)) {
  message("No saved model found; fitting a reproducible fixture model...")
  cohort <- cdt_generate_cohort(n = 100, seed = 1)
  s <- cdt_simulate_cohort_sensors(cohort, days = 120, seed = 2)
  training <- cdt_build_training_table(cohort, s$readings, s$falls,
    window_days = 7, stride = 2)
  model <- cdt_fit_model(training)
}

# --- The 4 fixed runs ------------------------------------------------------
# Independent fixed seeds per run (each simulation gets a clean stream). A and
# B within a simulation SHARE their seed so the P08 counterfactual is valid.
runs <- list(
  list(sim = "sim1_baseline", branch = "A", seed = 20260401L, flu = FALSE),
  list(sim = "sim1_baseline", branch = "B", seed = 20260401L, flu = FALSE),
  list(sim = "sim2_flu",      branch = "A", seed = 20260501L, flu = TRUE),
  list(sim = "sim2_flu",      branch = "B", seed = 20260501L, flu = TRUE)
)

print_checkpoints <- function(con, sim, branch) {
  ck <- cdt_sim_get_checkpoints(con, sim, branch)
  if (nrow(ck) == 0) {
    message("    (no checkpoints logged)")
    return(invisible())
  }
  gate <- ck[ck$step == "gate", , drop = FALSE]
  for (i in seq_len(nrow(gate))) {
    message(sprintf("    day %2d gate: %s%s", gate$day[i], gate$status[i],
      if (!is.na(gate$detail[i]) && nzchar(gate$detail[i])) {
        paste0(" - ", gate$detail[i])
      } else {
        ""
      }))
  }
  # In mock mode, surface any non-pass sub-steps as the reasoning trail.
  nonpass <- ck[ck$status != "pass", , drop = FALSE]
  if (nrow(nonpass) > 0) {
    message("    reasoning (non-pass steps):")
    for (i in seq_len(nrow(nonpass))) {
      message(sprintf("      day %2d [%s] %s: %s", nonpass$day[i],
        nonpass$step[i], nonpass$status[i], nonpass$detail[i] %||% ""))
    }
  }
}

for (r in runs) {
  message(sprintf("\n=== RUN %s / %s (seed=%d, flu=%s) ===",
    r$sim, r$branch, r$seed, r$flu))
  res <- cdt_run_simulation(con, model, r$sim, r$branch, days = days,
    seed = r$seed, mock = mock, flu = r$flu)
  message(sprintf("  completed %d/%d day(s); P08 onset day = %d",
    res$days_completed, days, res$p08_onset))
  message(sprintf("  gate statuses: %s",
    paste(res$gate_statuses, collapse = ",")))
  if (mock) print_checkpoints(con, r$sim, r$branch)
}

# --- Reports ---------------------------------------------------------------
reports_dir <- file.path(root, "data", "sim_reports")
dir.create(reports_dir, showWarnings = FALSE)

render_or_note <- function(sim, branch, kind, out) {
  path <- cdt_sim_render_report(con, sim, branch, kind = kind,
    output_file = out)
  if (is.null(path)) {
    message(sprintf("  [skip] %s/%s %s report (rmarkdown/pandoc unavailable)",
      sim, branch, kind))
  } else {
    message(sprintf("  wrote %s", path))
  }
}

message("\nGenerating reports...")
# Sim 1: FULL report for both branches.
render_or_note("sim1_baseline", "A", "full",
  file.path(reports_dir, "sim1_baseline_A_full.html"))
render_or_note("sim1_baseline", "B", "full",
  file.path(reports_dir, "sim1_baseline_B_full.html"))
# Sim 2: SUMMARY report for both branches.
render_or_note("sim2_flu", "A", "summary",
  file.path(reports_dir, "sim2_flu_A_summary.html"))
render_or_note("sim2_flu", "B", "summary",
  file.path(reports_dir, "sim2_flu_B_summary.html"))

dbDisconnect(con)
message("\nDone. Simulation complete.")
if (mock) {
  message("Mock run finished. Review the checkpoint outputs above, then re-run",
    " with --live for the LLM path.")
}
