#' Simulation report data assembly (Phase 6)
#'
#' Pure-R helpers that turn a completed run's persisted tables into the data a
#' report needs. NO rendering here (that lives in the Rmd templates under
#' `inst/simulation/`), and NO model math — these only read the run-keyed sim
#' tables via the exported getters, so they stay a thin display layer and never
#' touch the restricted `ground_truth_evaluation`.
#'
#' Two shapes:
#'   * `cdt_sim_report_data()`  — the FULL Sim 1 payload: exec stats, one 24h/7d
#'     risk series per patient, the checkpoint table, and a traceability appendix.
#'   * `cdt_sim_summary_report()` — the SUMMARY payload for Sim 2 and the stress
#'     scenarios: obstacles encountered + which checkpoints passed/warned/failed.

# Count checkpoint rows by status, returned as a named integer vector so both
# report shapes can headline pass/warn/fail totals identically.
.cdt_ck_status_counts <- function(ck) {
  out <- c(pass = 0L, warn = 0L, fail = 0L)
  if (is.null(ck) || nrow(ck) == 0) {
    return(out)
  }
  tab <- table(factor(ck$status, levels = c("pass", "warn", "fail")))
  out[names(tab)] <- as.integer(tab)
  out
}

#' Assemble the FULL report payload for one run (Sim 1)
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @return A list: `meta`, `exec` (headline stats), `patients` (one entry per
#'   patient with `p_24h`/`p_7d`/`day` series + final tiers), `checkpoints`
#'   (the full log tibble), and `traceability` (per-day decision/prediction
#'   counts for the audit appendix).
#' @export
cdt_sim_report_data <- function(con, simulation_id, branch) {
  patients <- cdt_sim_patients()
  preds <- cdt_sim_get_predictions(con, simulation_id, branch)
  ck <- cdt_sim_get_checkpoints(con, simulation_id, branch)
  dec <- cdt_sim_get_agent_decisions(con, simulation_id, branch)

  # One risk series per patient (ordered by day). Empty series stay as a
  # zero-row frame so the template can render a "no data" note per patient.
  per_patient <- lapply(patients$patient_id, function(pid) {
    p <- preds[preds$patient_id == pid, , drop = FALSE]
    p <- p[order(p$day), , drop = FALSE]
    list(
      patient_id = pid,
      name = patients$name[patients$patient_id == pid],
      day = p$day,
      p_24h = p$p_24h,
      p_7d = p$p_7d,
      final_tier_24h = if (nrow(p)) utils::tail(p$tier_24h, 1) else NA_character_,
      final_tier_7d = if (nrow(p)) utils::tail(p$tier_7d, 1) else NA_character_,
      peak_p_7d = if (nrow(p)) max(p$p_7d, na.rm = TRUE) else NA_real_
    )
  })
  names(per_patient) <- patients$patient_id

  days_run <- if (nrow(preds)) max(preds$day) else 0L
  status_counts <- .cdt_ck_status_counts(ck)

  # Per-day traceability: how many decisions/predictions landed, and how many
  # decisions were degraded (invalid agent output reused).
  trace <- if (nrow(dec)) {
    days <- sort(unique(dec$day))
    do.call(rbind, lapply(days, function(d) {
      dd <- dec[dec$day == d, , drop = FALSE]
      pp <- preds[preds$day == d, , drop = FALSE]
      data.frame(
        day = d,
        decisions = nrow(dd),
        invalid_decisions = sum(as.integer(dd$agent_output_invalid), na.rm = TRUE),
        predictions = nrow(pp),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(day = integer(0), decisions = integer(0),
      invalid_decisions = integer(0), predictions = integer(0))
  }

  list(
    meta = list(simulation_id = simulation_id, branch = branch,
      days_run = days_run, n_patients = nrow(patients),
      generated_at = as.character(Sys.time())),
    exec = list(
      days_run = days_run,
      n_predictions = nrow(preds),
      checkpoints_pass = status_counts[["pass"]],
      checkpoints_warn = status_counts[["warn"]],
      checkpoints_fail = status_counts[["fail"]],
      degraded_decisions = if (nrow(dec)) {
        sum(as.integer(dec$agent_output_invalid), na.rm = TRUE)
      } else {
        0L
      },
      highest_risk_patient = if (nrow(preds)) {
        agg <- stats::aggregate(p_7d ~ patient_id, data = preds, FUN = max)
        agg$patient_id[which.max(agg$p_7d)]
      } else {
        NA_character_
      }
    ),
    patients = per_patient,
    checkpoints = ck,
    traceability = trace,
    validation = .cdt_sim_validation_block(con, simulation_id, branch,
      ck, preds, dec, per_patient)
  )
}

# Persona -> expected observable signature, used for the clinical face-validity
# axis of the validation block. Descriptions mirror the fichas in
# cdt_sim_patients()$system_prompt (kept here as short display strings).
.cdt_sim_expected_signatures <- function() {
  c(
    P01 = "fear-of-falling: activity below physical capacity, worse without physio",
    P02 = "Parkinson's wearing-off: mobility fluctuates around med timing",
    P03 = "night-concentrated risk; daytime activity non-predictive",
    P04 = "stable low-risk control (social connector)",
    P05 = "afternoon knee pain: PM mobility below AM",
    P06 = "progressive decline over 5-7 days after new diuretic",
    P07 = "morning instability/drowsiness (night benzodiazepine)",
    P08 = "subtle risk not obvious from step volume (blind experiment)",
    P09 = "high stable baseline; occasional night agitation",
    P10 = "non-linear post-hospital recovery (upward trend)"
  )
}

# Build the four-axis validation block for a report payload. It reads ONLY the
# run-keyed sim tables already loaded by the caller (checkpoints, predictions,
# decisions, per-patient series). It NEVER queries ground_truth_evaluation: the
# hidden P08 A/B outcome is a separate privileged analysis, deliberately kept
# out of any clinical/report surface. The safety axis actively scans the very
# payload the report will render and asserts no restricted field leaked in.
.cdt_sim_validation_block <- function(con, simulation_id, branch,
                                      ck, preds, dec, per_patient) {
  leak_pat <- "latent|hazard|fall_sampled|intervention_fired"

  # (i) Technical integrity: split checkpoint outcomes by gate family. The
  # orchestrator logs steps: social, agent_json, biological, model.
  status_of <- function(steps) {
    rows <- ck[ck$step %in% steps, , drop = FALSE]
    c(pass = sum(rows$status == "pass"),
      warn = sum(rows$status == "warn"),
      fail = sum(rows$status == "fail"))
  }
  technical <- list(
    total = .cdt_ck_status_counts(ck),
    biological = status_of("biological"),
    model_output = status_of("model"),
    all_predictions_in_range = if (nrow(preds)) {
      all(preds$p_24h >= 0 & preds$p_24h <= 1 &
          preds$p_7d >= 0 & preds$p_7d <= 1)
    } else {
      NA
    }
  )

  # (ii) Experimental validity (observable side only). For P08 we surface the
  # model-risk trajectory and, on Branch B, whether the scripted preventive
  # intervention was logged (from the interventions table — a legitimate
  # clinical record, NOT the hidden hazard state). The counterfactual fall
  # comparison lives in the privileged offline analysis, NOT here.
  p08 <- preds[preds$patient_id == "P08", , drop = FALSE]
  p08 <- p08[order(p08$day), , drop = FALSE]
  interv_n <- .cdt_sim_intervention_count(con)
  experimental <- list(
    p08_days = p08$day,
    p08_p_24h = p08$p_24h,
    p08_p_7d = p08$p_7d,
    p08_peak_7d = if (nrow(p08)) max(p08$p_7d, na.rm = TRUE) else NA_real_,
    branch = branch,
    intervention_fired = branch == "B" && interv_n > 0,
    intervention_count = if (branch == "B") interv_n else 0L,
    note = paste("A/B fall-outcome comparison is a separate privileged",
      "analysis; this report shows only observable model risk.")
  )

  # (iii) Clinical face-validity: pair each persona's expected signature with a
  # coarse observed summary (final tier + peak 7d) for eyeball validation.
  expected <- .cdt_sim_expected_signatures()
  face <- do.call(rbind, lapply(names(per_patient), function(pid) {
    pp <- per_patient[[pid]]
    data.frame(
      patient_id = pid,
      expected = unname(expected[pid]),
      observed_final_tier_7d = pp$final_tier_7d %||% NA_character_,
      observed_peak_7d = pp$peak_p_7d %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  # (iv) Safety / leakage: scan the assembled payload's text + names for any
  # restricted field. The report must carry a positive attestation.
  scan_targets <- c(
    unlist(lapply(per_patient, names)),
    names(preds), names(dec), names(ck),
    if (nrow(ck)) as.character(ck$detail) else character(0)
  )
  leak_hits <- scan_targets[grepl(leak_pat, scan_targets, ignore.case = TRUE)]
  # Also confirm the clinical surfaces stay clean for P08 in this DB.
  surface_clean <- tryCatch({
    ctx <- cdt_patient_context(con, cdt_load_model_safe(), "P08")
    !any(grepl(leak_pat, ctx, ignore.case = TRUE))
  }, error = function(e) NA)
  safety <- list(
    restricted_pattern = leak_pat,
    payload_leak_hits = leak_hits,
    ground_truth_getter_exists = exists("cdt_sim_get_ground_truth"),
    clinical_surface_clean = surface_clean,
    attestation = length(leak_hits) == 0 &&
      !exists("cdt_sim_get_ground_truth")
  )

  list(technical = technical, experimental = experimental,
    face_validity = face, safety = safety)
}

# Best-effort model loader for the safety-surface scan; returns NULL if no model
# is available so the scan degrades to NA rather than erroring.
cdt_load_model_safe <- function() {
  tryCatch(cdt_load_model(), error = function(e) NULL)
}

# Count simulation-authored preventive interventions for P08 (the Branch B
# intervention record). Reads the interventions table only — a legitimate
# clinical record — never the hidden ground_truth_evaluation table. Returns 0
# on any error so report assembly never fails.
.cdt_sim_intervention_count <- function(con) {
  tryCatch({
    q <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM interventions
         WHERE patient_id = 'P08' AND created_by = 'simulation';")
    as.integer(q$n[[1]])
  }, error = function(e) 0L)
}

#' Assemble the SUMMARY report payload for one run (Sim 2 / stress)
#'
#' A compact honest account: which checkpoints passed/warned/failed, the days
#' (if any) that failed the gate, and the concrete obstacles surfaced in the
#' checkpoint `detail` column (warn/fail messages). No per-patient charts.
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @return A list: `meta`, `status_counts`, `failed_days`, `obstacles`
#'   (distinct warn/fail detail lines), and `gate_outcome` (overall pass/fail).
#' @export
cdt_sim_summary_report <- function(con, simulation_id, branch) {
  ck <- cdt_sim_get_checkpoints(con, simulation_id, branch)
  preds <- cdt_sim_get_predictions(con, simulation_id, branch)
  status_counts <- .cdt_ck_status_counts(ck)

  # Days where the aggregate gate step recorded a failure.
  failed_days <- integer(0)
  if (nrow(ck) > 0) {
    gate_rows <- ck[ck$step == "gate", , drop = FALSE]
    failed_days <- sort(unique(gate_rows$day[gate_rows$status == "fail"]))
  }

  # Obstacles = the distinct non-pass detail lines (the honest "what went wrong"
  # / "what we tolerated" list), tagged with their status and day.
  obstacles <- character(0)
  if (nrow(ck) > 0) {
    nonpass <- ck[ck$status %in% c("warn", "fail") &
      !is.na(ck$detail) & nzchar(ck$detail), , drop = FALSE]
    if (nrow(nonpass) > 0) {
      obstacles <- unique(sprintf("day %d [%s/%s]: %s",
        nonpass$day, nonpass$step, nonpass$status, nonpass$detail))
    }
  }

  gate_outcome <- if (status_counts[["fail"]] > 0) "fail" else "pass"

  # Compact four-axis validation for the summary shape: technical gate split,
  # observable P08/intervention note, and the safety attestation. No per-patient
  # face-validity table (that is a full-report feature).
  leak_pat <- "latent|hazard|fall_sampled|intervention_fired"
  status_of <- function(steps) {
    rows <- ck[ck$step %in% steps, , drop = FALSE]
    c(pass = sum(rows$status == "pass"),
      warn = sum(rows$status == "warn"),
      fail = sum(rows$status == "fail"))
  }
  interv_n <- .cdt_sim_intervention_count(con)
  scan_targets <- c(names(preds), names(ck),
    if (nrow(ck)) as.character(ck$detail) else character(0))
  leak_hits <- scan_targets[grepl(leak_pat, scan_targets, ignore.case = TRUE)]
  validation <- list(
    technical = list(
      total = status_counts,
      biological = status_of("biological"),
      model_output = status_of("model"),
      all_predictions_in_range = if (nrow(preds)) {
        all(preds$p_24h >= 0 & preds$p_24h <= 1 &
            preds$p_7d >= 0 & preds$p_7d <= 1)
      } else {
        NA
      }
    ),
    experimental = list(
      branch = branch,
      intervention_fired = branch == "B" && interv_n > 0,
      intervention_count = if (branch == "B") interv_n else 0L,
      note = paste("A/B fall-outcome comparison is a separate privileged",
        "analysis; not shown in this run report.")
    ),
    safety = list(
      restricted_pattern = leak_pat,
      payload_leak_hits = leak_hits,
      ground_truth_getter_exists = exists("cdt_sim_get_ground_truth"),
      attestation = length(leak_hits) == 0 &&
        !exists("cdt_sim_get_ground_truth")
    )
  )

  list(
    meta = list(simulation_id = simulation_id, branch = branch,
      days_run = if (nrow(preds)) max(preds$day) else 0L,
      generated_at = as.character(Sys.time())),
    status_counts = status_counts,
    failed_days = failed_days,
    obstacles = obstacles,
    gate_outcome = gate_outcome,
    validation = validation
  )
}

#' Render a simulation report to HTML if rmarkdown is available
#'
#' Thin wrapper that degrades gracefully: if `rmarkdown` is not installed it
#' returns `NULL` (callers/tests must treat rendering as optional). The `kind`
#' selects the FULL Sim 1 template or the SUMMARY template.
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @param kind One of "full" or "summary".
#' @param output_file Destination HTML path (default: a tempfile).
#' @return The output path on success, or `NULL` if rmarkdown/pandoc is
#'   unavailable.
#' @export
cdt_sim_render_report <- function(con, simulation_id, branch, kind = "full",
                                  output_file = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available()) {
    return(NULL)
  }
  stopifnot(kind %in% c("full", "summary"))
  tmpl <- system.file("simulation",
    if (kind == "full") "report_full.Rmd" else "report_summary.Rmd",
    package = "clinicaldigitaltwin")
  # In source-tree runs (no installed package) fall back to the repo path.
  if (!nzchar(tmpl) || !file.exists(tmpl)) {
    root <- Sys.getenv("CDT_PROJECT_ROOT", ".")
    tmpl <- file.path(root, "inst", "simulation",
      if (kind == "full") "report_full.Rmd" else "report_summary.Rmd")
  }
  if (!file.exists(tmpl)) {
    return(NULL)
  }
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".html")
  }
  data <- if (kind == "full") {
    cdt_sim_report_data(con, simulation_id, branch)
  } else {
    cdt_sim_summary_report(con, simulation_id, branch)
  }
  rmarkdown::render(tmpl, output_file = output_file, quiet = TRUE,
    params = list(data = data), envir = new.env(parent = globalenv()))
  output_file
}
