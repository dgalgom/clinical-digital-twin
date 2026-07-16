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
    traceability = trace
  )
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

  list(
    meta = list(simulation_id = simulation_id, branch = branch,
      days_run = if (nrow(preds)) max(preds$day) else 0L,
      generated_at = as.character(Sys.time())),
    status_counts = status_counts,
    failed_days = failed_days,
    obstacles = obstacles,
    gate_outcome = gate_outcome
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
