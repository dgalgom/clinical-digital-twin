#' Simulation database layer (Phase 1)
#'
#' Additive, isolated persistence for the multi-agent simulation. Deliberately
#' SEPARATE from [cdt_db_init_schema()] so the production schema (and `verify.R`
#' step 2) is untouched: this creates only new, run-keyed tables and idempotently
#' widens `sensor_readings` with nullable simulation columns (production rows keep
#' them NULL, so `SELECT *` timeline reads are unaffected).
#'
#' All simulation rows are keyed by `(simulation_id, branch, day)`:
#'   * `simulation_id in {sim1_baseline, sim2_flu}` (plus stress-scenario ids)
#'   * `branch in {A, B}`
#'   * `day in 1..30` (fast mode 1..3)
#'
#' The `ground_truth_evaluation` table is RESTRICTED: it holds the hidden P08
#' latent state and stochastic fall outcome. There is intentionally NO exported
#' getter for it — the orchestrator reads it via a raw query, and the leak test
#' asserts the clinical surfaces never expose it.

#' Create the simulation schema (idempotent)
#'
#' @param con A DBI connection.
#' @return Invisibly `TRUE`.
#' @export
cdt_sim_init_schema <- function(con) {
  # Per-patient, per-day behavioural decision emitted by an agent. `agent_output
  # _invalid` records that the LLM produced unparseable/invalid JSON and the
  # prior day's decision was reused (audit of degraded days).
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS agent_decisions (
      decision_id             INTEGER PRIMARY KEY AUTOINCREMENT,
      simulation_id           TEXT NOT NULL,
      branch                  TEXT NOT NULL,
      day                     INTEGER NOT NULL,
      patient_id              TEXT NOT NULL,
      mobility_pct_of_baseline REAL,
      participated_group_activity INTEGER,
      medication_adherence    INTEGER,
      meaningful_social_interaction INTEGER,
      mood_fatigue            TEXT,
      notable_event           TEXT,
      confidence              REAL,
      agent_output_invalid    INTEGER NOT NULL DEFAULT 0,
      temperature             REAL,
      prompt_text             TEXT,
      raw_reply               TEXT,
      created_at              TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # Deterministic social layer: who interacted with whom on a given day.
  # `participants` is a JSON array of patient ids.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS social_interactions (
      interaction_id  INTEGER PRIMARY KEY AUTOINCREMENT,
      simulation_id   TEXT NOT NULL,
      branch          TEXT NOT NULL,
      day             INTEGER NOT NULL,
      participants    TEXT NOT NULL,   -- JSON array of patient ids
      interaction_type TEXT,
      initiated_by    TEXT,
      summary_text    TEXT,
      created_at      TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # Per-run model inference. Distinct from `risk_snapshots` (which is as_of-keyed
  # for production shift-triage) — this is run-keyed and stores both horizons.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_predictions (
      prediction_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      simulation_id   TEXT NOT NULL,
      branch          TEXT NOT NULL,
      day             INTEGER NOT NULL,
      patient_id      TEXT NOT NULL,
      p_24h           REAL,
      p_7d            REAL,
      tier_24h        TEXT,
      tier_7d         TEXT,
      quality_flag    TEXT,
      created_at      TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # Daily checkpoint gate outcomes (one row per checkpoint step per day).
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS daily_checkpoint_log (
      log_id          INTEGER PRIMARY KEY AUTOINCREMENT,
      simulation_id   TEXT NOT NULL,
      branch          TEXT NOT NULL,
      day             INTEGER NOT NULL,
      step            TEXT NOT NULL,
      status          TEXT NOT NULL,   -- pass | warn | fail
      detail          TEXT,
      created_at      TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # RESTRICTED hidden ground truth for the blind P08 experiment. No exported
  # getter; read only via raw query inside the orchestrator.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ground_truth_evaluation (
      gt_id            INTEGER PRIMARY KEY AUTOINCREMENT,
      simulation_id    TEXT NOT NULL,
      branch           TEXT NOT NULL,
      day              INTEGER NOT NULL,
      patient_id       TEXT NOT NULL,
      latent_risk      REAL,
      hazard           REAL,
      fall_sampled     INTEGER,
      intervention_fired INTEGER,
      created_at       TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  # Composite indexes for the common run-scoped access patterns.
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_agent_run
      ON agent_decisions(simulation_id, branch, day, patient_id);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_social_run
      ON social_interactions(simulation_id, branch, day);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_pred_run
      ON model_predictions(simulation_id, branch, day, patient_id);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_checkpoint_run
      ON daily_checkpoint_log(simulation_id, branch, day);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_gt_run
      ON ground_truth_evaluation(simulation_id, branch, day, patient_id);")

  # Idempotently widen sensor_readings with nullable simulation columns.
  .cdt_add_sim_sensor_columns(con)

  invisible(TRUE)
}

# Idempotently add the simulation run-keying columns to sensor_readings. SQLite
# has no "ADD COLUMN IF NOT EXISTS"; introspect first. Production rows leave
# these NULL, so cdt_get_patient_timeline (SELECT *) is unaffected.
.cdt_add_sim_sensor_columns <- function(con) {
  existing <- DBI::dbGetQuery(con, "PRAGMA table_info(sensor_readings);")$name
  sim_cols <- c(
    simulation_id = "TEXT",
    branch        = "TEXT",
    day           = "INTEGER",
    quality_flags = "TEXT"
  )
  for (col in names(sim_cols)) {
    if (!col %in% existing) {
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE sensor_readings ADD COLUMN %s %s;", col, sim_cols[[col]]
      ))
    }
  }
  invisible(TRUE)
}

# --- Write helpers ---------------------------------------------------------

#' Persist agent decisions for one run-day
#'
#' @param con A DBI connection.
#' @param df A data frame of decision rows. Must carry `simulation_id`, `branch`,
#'   `day`, `patient_id` plus the behavioural fields.
#' @return Invisibly the number of rows written.
#' @export
cdt_sim_write_agent_decisions <- function(con, df) {
  stopifnot(nrow(df) > 0,
    all(c("simulation_id", "branch", "day", "patient_id") %in% names(df)))
  DBI::dbWriteTable(con, "agent_decisions", as.data.frame(df), append = TRUE)
  invisible(nrow(df))
}

#' Persist social interactions for one run-day
#'
#' @param con A DBI connection.
#' @param df A data frame of interaction rows.
#' @return Invisibly the number of rows written.
#' @export
cdt_sim_write_social <- function(con, df) {
  stopifnot(nrow(df) > 0,
    all(c("simulation_id", "branch", "day", "participants") %in% names(df)))
  DBI::dbWriteTable(con, "social_interactions", as.data.frame(df), append = TRUE)
  invisible(nrow(df))
}

#' Persist model predictions for one run-day
#'
#' @param con A DBI connection.
#' @param df A data frame of prediction rows.
#' @return Invisibly the number of rows written.
#' @export
cdt_sim_write_predictions <- function(con, df) {
  stopifnot(nrow(df) > 0,
    all(c("simulation_id", "branch", "day", "patient_id") %in% names(df)))
  DBI::dbWriteTable(con, "model_predictions", as.data.frame(df), append = TRUE)
  invisible(nrow(df))
}

#' Log a single checkpoint outcome
#'
#' @param con A DBI connection.
#' @param simulation_id,branch,day Run key.
#' @param step Checkpoint step label.
#' @param status One of "pass", "warn", "fail".
#' @param detail Optional free-text detail.
#' @return Invisibly the new `log_id`.
#' @export
cdt_sim_log_checkpoint <- function(con, simulation_id, branch, day, step,
                                   status, detail = NULL) {
  stopifnot(nzchar(simulation_id), nzchar(branch), nzchar(step),
    status %in% c("pass", "warn", "fail"))
  DBI::dbExecute(con,
    "INSERT INTO daily_checkpoint_log
       (simulation_id, branch, day, step, status, detail)
       VALUES (?, ?, ?, ?, ?, ?);",
    params = list(simulation_id, branch, as.integer(day), step, status,
      detail %||% NA_character_))
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[1]
  invisible(as.integer(id))
}

#' Persist a hidden ground-truth evaluation row (RESTRICTED)
#'
#' Intentionally paired with NO exported getter. The orchestrator writes here and
#' reads back via a raw query; the clinical surfaces must never see this table.
#'
#' @param con A DBI connection.
#' @param simulation_id,branch,day,patient_id Run + patient key.
#' @param latent_risk,hazard Hidden state.
#' @param fall_sampled,intervention_fired Integer 0/1 outcome flags.
#' @return Invisibly the new `gt_id`.
#' @export
cdt_sim_write_ground_truth <- function(con, simulation_id, branch, day,
                                       patient_id, latent_risk, hazard,
                                       fall_sampled, intervention_fired) {
  stopifnot(nzchar(simulation_id), nzchar(branch), nzchar(patient_id))
  DBI::dbExecute(con,
    "INSERT INTO ground_truth_evaluation
       (simulation_id, branch, day, patient_id, latent_risk, hazard,
        fall_sampled, intervention_fired)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(simulation_id, branch, as.integer(day), patient_id,
      as.numeric(latent_risk), as.numeric(hazard),
      as.integer(fall_sampled), as.integer(intervention_fired)))
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[1]
  invisible(as.integer(id))
}

# --- Read helpers (non-restricted tables only) -----------------------------

#' Fetch agent decisions for a run (optionally a specific day/patient)
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @param day Optional day filter.
#' @param patient_id Optional patient filter.
#' @return A tibble.
#' @export
cdt_sim_get_agent_decisions <- function(con, simulation_id, branch,
                                        day = NULL, patient_id = NULL) {
  q <- "SELECT * FROM agent_decisions WHERE simulation_id = ? AND branch = ?"
  params <- list(simulation_id, branch)
  if (!is.null(day)) {
    q <- paste(q, "AND day = ?")
    params <- c(params, list(as.integer(day)))
  }
  if (!is.null(patient_id)) {
    q <- paste(q, "AND patient_id = ?")
    params <- c(params, list(patient_id))
  }
  q <- paste(q, "ORDER BY day, patient_id;")
  tibble::as_tibble(DBI::dbGetQuery(con, q, params = params))
}

#' Fetch model predictions for a run (optionally a specific patient)
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @param patient_id Optional patient filter.
#' @return A tibble ordered by day.
#' @export
cdt_sim_get_predictions <- function(con, simulation_id, branch,
                                    patient_id = NULL) {
  q <- "SELECT * FROM model_predictions WHERE simulation_id = ? AND branch = ?"
  params <- list(simulation_id, branch)
  if (!is.null(patient_id)) {
    q <- paste(q, "AND patient_id = ?")
    params <- c(params, list(patient_id))
  }
  q <- paste(q, "ORDER BY day, patient_id;")
  tibble::as_tibble(DBI::dbGetQuery(con, q, params = params))
}

#' Fetch the checkpoint log for a run
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @return A tibble ordered by day then step.
#' @export
cdt_sim_get_checkpoints <- function(con, simulation_id, branch) {
  tibble::as_tibble(DBI::dbGetQuery(con,
    "SELECT * FROM daily_checkpoint_log
       WHERE simulation_id = ? AND branch = ?
       ORDER BY day, log_id;",
    params = list(simulation_id, branch)))
}

#' Fetch the simulation sensor timeline for one patient on one run
#'
#' Scopes `sensor_readings` to a single run so predictions never mix production
#' rows (NULL `simulation_id`) with simulated ones.
#'
#' @param con A DBI connection.
#' @param simulation_id,branch Run key.
#' @param patient_id Patient identifier.
#' @return A tibble of sensor readings ordered by time.
#' @export
cdt_sim_get_patient_timeline <- function(con, simulation_id, branch, patient_id) {
  tibble::as_tibble(DBI::dbGetQuery(con,
    "SELECT * FROM sensor_readings
       WHERE simulation_id = ? AND branch = ? AND patient_id = ?
       ORDER BY ts;",
    params = list(simulation_id, branch, patient_id)))
}
