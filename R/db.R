#' Database layer (SQLite via DBI/RSQLite)
#'
#' Owns the schema and provides indexed query helpers. SQLite is used for the
#' hackathon; the migration path to Postgres is documented in README.md (swap
#' the driver in `cdt_db_connect()` and replace `AUTOINCREMENT` semantics).

#' Open a connection to the project SQLite database
#'
#' @param path Path to the SQLite file. Defaults to [cdt_db_path()].
#' @return A DBI connection. Caller is responsible for `DBI::dbDisconnect()`.
#' @export
cdt_db_connect <- function(path = cdt_db_path()) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

#' Create the full schema (idempotent)
#'
#' Tables:
#' * `users` - clinician accounts (hashed passwords, roles)
#' * `sessions` - session tokens for the MVP auth
#' * `patients` - static clinical/demographic data (canonical schema)
#' * `sensor_readings` - daily-resolution vitals/activity time series
#' * `fall_events` - simulated ground-truth fall labels
#' * `interventions` - clinician-logged actions (P0-3 closed loop)
#' * `risk_snapshots` - per-patient risk history for delta computation (P0-1)
#' * `alerts` - change-detection events for the shift-triage view (P0-1)
#'
#' @param con A DBI connection.
#' @return Invisibly `TRUE`.
#' @export
cdt_db_init_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id       INTEGER PRIMARY KEY AUTOINCREMENT,
      username      TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      role          TEXT NOT NULL DEFAULT 'clinician',
      created_at    TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      token      TEXT PRIMARY KEY,
      user_id    INTEGER NOT NULL,
      created_at TEXT NOT NULL DEFAULT (datetime('now')),
      expires_at TEXT NOT NULL,
      FOREIGN KEY (user_id) REFERENCES users(user_id)
    );")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS patients (
      patient_id              TEXT PRIMARY KEY,
      name                    TEXT,
      age                     INTEGER,
      sex                     TEXT,
      parkinsons              INTEGER DEFAULT 0,
      osteoporosis            INTEGER DEFAULT 0,
      orthostatic_hypotension INTEGER DEFAULT 0,
      polypharmacy            INTEGER DEFAULT 0,
      prior_falls             INTEGER DEFAULT 0,
      n_medications           INTEGER DEFAULT 0,
      medications             TEXT,
      comorbidities           TEXT,
      created_at              TEXT NOT NULL DEFAULT (datetime('now'))
    );")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sensor_readings (
      reading_id       INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id       TEXT NOT NULL,
      ts               TEXT NOT NULL,   -- ISO-8601, daily 06:00 Europe/Berlin
      heart_rate       REAL,
      resting_hr       REAL,
      sbp              REAL,
      dbp              REAL,
      step_count       INTEGER,
      accel_counts     INTEGER,         -- accelerometry activity counts
      accel_magnitude  REAL,            -- mean vector magnitude (g)
      hours_sitting    REAL,
      hours_lying      REAL,
      hours_standing   REAL,
      FOREIGN KEY (patient_id) REFERENCES patients(patient_id)
    );")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fall_events (
      event_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id TEXT NOT NULL,
      ts         TEXT NOT NULL,
      severity   TEXT,
      FOREIGN KEY (patient_id) REFERENCES patients(patient_id)
    );")

  # Interventions logged by clinicians (P0-3 closed loop): the record of what
  # was actually done, so the what-if panel leads somewhere and the trend plot
  # can overlay 'we acted here' markers. `created_at` is when it was logged;
  # `detail` optionally carries the counterfactual JSON from a logged what-if.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS interventions (
      intervention_id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id      TEXT NOT NULL,
      type            TEXT NOT NULL,
      detail          TEXT,
      created_by      TEXT,
      created_at      TEXT NOT NULL DEFAULT (datetime('now')),
      FOREIGN KEY (patient_id) REFERENCES patients(patient_id)
    );")

  # Risk snapshots (P0-1): a persisted history of per-patient risk so the
  # shift-triage view can compute "what changed since last shift". `as_of` is a
  # logical label (e.g. an ISO date or "previous shift"); the delta compares the
  # newest snapshot against the previous one for each patient.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS risk_snapshots (
      snapshot_id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id  TEXT NOT NULL,
      as_of       TEXT NOT NULL,
      p_24h       REAL,
      p_7d        REAL,
      tier_7d     TEXT,
      created_at  TEXT NOT NULL DEFAULT (datetime('now')),
      FOREIGN KEY (patient_id) REFERENCES patients(patient_id)
    );")

  # Alerts (P0-1): change-detection events emitted by cdt_compute_alerts(). Each
  # row is a movement worth a clinician's attention (risk jump or tier crossing),
  # carrying a one-line human-readable reason and an acknowledgement audit trail.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS alerts (
      alert_id        INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id      TEXT NOT NULL,
      created_at      TEXT NOT NULL DEFAULT (datetime('now')),
      kind            TEXT NOT NULL,
      severity        TEXT NOT NULL,
      delta_pts       REAL,
      reason_text     TEXT,
      acknowledged_by TEXT,
      acknowledged_at TEXT,
      FOREIGN KEY (patient_id) REFERENCES patients(patient_id)
    );")

  # Indexes for the common access patterns (patient timeline, cohort snapshot).
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_sensor_patient_ts
      ON sensor_readings(patient_id, ts);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_fall_patient_ts
      ON fall_events(patient_id, ts);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_sessions_expires
      ON sessions(expires_at);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_interventions_patient
      ON interventions(patient_id, created_at);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_snapshots_patient
      ON risk_snapshots(patient_id, created_at);")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_alerts_patient
      ON alerts(patient_id, created_at);")

  # Post-fall huddle fields (P0-4). `fall_events` was historically labels-only
  # (patient_id, ts, severity). The structured post-fall huddle stores its
  # findings on the same row. These are ADDITIVE columns: the training-table
  # builder reads only patient_id + ts, so the model/labels are unaffected. We
  # add them with idempotent ALTER guards so an existing DB is upgraded in place
  # (CREATE TABLE IF NOT EXISTS cannot add columns to an already-created table).
  .cdt_add_fall_huddle_columns(con)

  invisible(TRUE)
}

# Idempotently add the post-fall huddle columns to fall_events. SQLite has no
# "ADD COLUMN IF NOT EXISTS", so we introspect the existing columns first and
# only add the ones that are missing. Safe to call on every init.
.cdt_add_fall_huddle_columns <- function(con) {
  existing <- DBI::dbGetQuery(con, "PRAGMA table_info(fall_events);")$name
  huddle_cols <- c(
    location             = "TEXT",
    activity_at_fall     = "TEXT",
    injury_level         = "TEXT",
    contributing_factors = "TEXT",
    plan                 = "TEXT",
    huddle_summary       = "TEXT",
    huddle_completed_by  = "TEXT",
    huddle_completed_at  = "TEXT"
  )
  for (col in names(huddle_cols)) {
    if (!col %in% existing) {
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE fall_events ADD COLUMN %s %s;", col, huddle_cols[[col]]
      ))
    }
  }
  invisible(TRUE)
}

#' Write a data frame to a table, replacing existing rows
#'
#' @param con A DBI connection.
#' @param table Table name.
#' @param df Data frame to write.
#' @param append If `TRUE`, append; otherwise overwrite table contents.
#' @return Invisibly the number of rows written.
#' @export
cdt_db_write <- function(con, table, df, append = TRUE) {
  DBI::dbWriteTable(con, table, as.data.frame(df),
    append = append, overwrite = !append
  )
  invisible(nrow(df))
}

#' Fetch a patient's full timeline (sensor readings ordered by time)
#'
#' @param con A DBI connection.
#' @param patient_id Patient identifier.
#' @return A tibble of sensor readings.
#' @export
cdt_get_patient_timeline <- function(con, patient_id) {
  q <- "SELECT * FROM sensor_readings WHERE patient_id = ? ORDER BY ts;"
  res <- DBI::dbGetQuery(con, q, params = list(patient_id))
  tibble::as_tibble(res)
}

#' Fetch static clinical data for one patient
#'
#' @param con A DBI connection.
#' @param patient_id Patient identifier.
#' @return A one-row tibble, or a zero-row tibble if not found.
#' @export
cdt_get_patient <- function(con, patient_id) {
  q <- "SELECT * FROM patients WHERE patient_id = ?;"
  tibble::as_tibble(DBI::dbGetQuery(con, q, params = list(patient_id)))
}

#' Fetch all patients (cohort static data)
#'
#' @param con A DBI connection.
#' @return A tibble with one row per patient.
#' @export
cdt_get_cohort <- function(con) {
  tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM patients ORDER BY patient_id;"))
}

#' Fetch fall events for one patient (or all if `patient_id` is NULL)
#'
#' @param con A DBI connection.
#' @param patient_id Optional patient identifier.
#' @return A tibble of fall events.
#' @export
cdt_get_fall_events <- function(con, patient_id = NULL) {
  if (is.null(patient_id)) {
    res <- DBI::dbGetQuery(con, "SELECT * FROM fall_events ORDER BY patient_id, ts;")
  } else {
    res <- DBI::dbGetQuery(con,
      "SELECT * FROM fall_events WHERE patient_id = ? ORDER BY ts;",
      params = list(patient_id)
    )
  }
  tibble::as_tibble(res)
}

#' Fetch a single fall event by id (P0-4)
#'
#' @param con A DBI connection.
#' @param event_id Fall event identifier.
#' @return A one-row tibble, or a zero-row tibble if not found.
#' @export
cdt_get_fall_event <- function(con, event_id) {
  tibble::as_tibble(DBI::dbGetQuery(con,
    "SELECT * FROM fall_events WHERE event_id = ?;",
    params = list(event_id)
  ))
}

#' List falls that still need a post-fall huddle (P0-4)
#'
#' A fall is "open" until its huddle is completed (`huddle_completed_at` is
#' NULL). Most-recent first, so the shift can action the freshest falls.
#'
#' @param con A DBI connection.
#' @return A tibble of un-huddled fall events (zero-row if none).
#' @export
cdt_get_open_huddles <- function(con) {
  tibble::as_tibble(DBI::dbGetQuery(con,
    "SELECT * FROM fall_events
       WHERE huddle_completed_at IS NULL
       ORDER BY ts DESC, event_id DESC;"
  ))
}

#' Save a completed post-fall huddle onto its fall event (P0-4)
#'
#' Persists the structured huddle findings the clinician reviewed/edited. The
#' LLM draft NEVER writes here directly: this is only called from an explicit
#' clinician save action, so the stored record reflects a human decision.
#'
#' @param con A DBI connection.
#' @param event_id Fall event identifier.
#' @param fields Named list of huddle fields to store. Recognised keys:
#'   `location`, `activity_at_fall`, `injury_level`, `contributing_factors`,
#'   `plan`, `huddle_summary`. Unknown keys are ignored.
#' @param completed_by Optional user identifier (username/role).
#' @param completed_at Optional ISO timestamp; defaults to `datetime('now')`.
#' @return Invisibly the number of rows updated (1 on success, 0 if not found).
#' @export
cdt_complete_huddle <- function(con, event_id, fields = list(),
                                completed_by = NULL, completed_at = NULL) {
  stopifnot(!is.null(event_id))
  allowed <- c("location", "activity_at_fall", "injury_level",
    "contributing_factors", "plan", "huddle_summary")
  fields <- fields[intersect(names(fields), allowed)]

  set_cols <- character(0)
  params <- list()
  for (k in names(fields)) {
    set_cols <- c(set_cols, sprintf("%s = ?", k))
    v <- fields[[k]]
    params <- c(params, list(if (is.null(v)) NA_character_ else as.character(v)))
  }
  # Always stamp completion.
  set_cols <- c(set_cols, "huddle_completed_by = ?")
  params <- c(params, list(if (is.null(completed_by)) NA_character_ else
    as.character(completed_by)))
  if (is.null(completed_at)) {
    set_cols <- c(set_cols, "huddle_completed_at = datetime('now')")
  } else {
    set_cols <- c(set_cols, "huddle_completed_at = ?")
    params <- c(params, list(as.character(completed_at)))
  }

  sql <- sprintf("UPDATE fall_events SET %s WHERE event_id = ?;",
    paste(set_cols, collapse = ", "))
  params <- c(params, list(event_id))
  n <- DBI::dbExecute(con, sql, params = params)
  invisible(n)
}

#' Log a clinician-initiated intervention (P0-3 closed loop)
#'
#' Records what was actually done for a patient. Never called automatically: the
#' UI wires this to an explicit clinician action (a button), so the record
#' reflects a human decision. `detail` may carry a free-text note or the
#' counterfactual JSON captured from a logged what-if scenario.
#'
#' @param con A DBI connection.
#' @param patient_id Patient identifier.
#' @param type Short intervention type/category (e.g. "Medication review").
#' @param detail Optional free-text or JSON detail.
#' @param created_by Optional user identifier (username/role); unrestricted for
#'   the MVP demo.
#' @param created_at Optional ISO timestamp; defaults to the DB `datetime('now')`.
#' @return Invisibly the new `intervention_id`.
#' @export
cdt_log_intervention <- function(con, patient_id, type, detail = NULL,
                                 created_by = NULL, created_at = NULL) {
  stopifnot(nzchar(patient_id), nzchar(type))
  if (is.null(created_at)) {
    DBI::dbExecute(con,
      "INSERT INTO interventions (patient_id, type, detail, created_by)
         VALUES (?, ?, ?, ?);",
      params = list(patient_id, type,
        detail %||% NA_character_, created_by %||% NA_character_)
    )
  } else {
    DBI::dbExecute(con,
      "INSERT INTO interventions (patient_id, type, detail, created_by, created_at)
         VALUES (?, ?, ?, ?, ?);",
      params = list(patient_id, type,
        detail %||% NA_character_, created_by %||% NA_character_, created_at)
    )
  }
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[1]
  invisible(as.integer(id))
}

#' Fetch logged interventions for one patient (or all if `patient_id` is NULL)
#'
#' @param con A DBI connection.
#' @param patient_id Optional patient identifier.
#' @return A tibble of interventions ordered by `created_at`.
#' @export
cdt_get_interventions <- function(con, patient_id = NULL) {
  if (is.null(patient_id)) {
    res <- DBI::dbGetQuery(con,
      "SELECT * FROM interventions ORDER BY patient_id, created_at;")
  } else {
    res <- DBI::dbGetQuery(con,
      "SELECT * FROM interventions WHERE patient_id = ? ORDER BY created_at;",
      params = list(patient_id)
    )
  }
  tibble::as_tibble(res)
}

# --- Risk snapshots + alerts (P0-1 shift triage) ---------------------------

#' Persist a cohort risk snapshot (P0-1)
#'
#' Writes one row per patient capturing their current risk under an `as_of`
#' label, so a later snapshot can be diffed against it.
#'
#' @param con A DBI connection.
#' @param snapshot A data frame with `patient_id`, `p_24h`, `p_7d`, `tier_7d`.
#' @param as_of A short label for this snapshot (e.g. an ISO date).
#' @return Invisibly the number of rows written.
#' @export
cdt_write_risk_snapshot <- function(con, snapshot, as_of) {
  stopifnot(nzchar(as_of), nrow(snapshot) > 0,
    all(c("patient_id", "p_24h", "p_7d", "tier_7d") %in% names(snapshot)))
  df <- data.frame(
    patient_id = as.character(snapshot$patient_id),
    as_of      = as_of,
    p_24h      = as.numeric(snapshot$p_24h),
    p_7d       = as.numeric(snapshot$p_7d),
    tier_7d    = as.character(snapshot$tier_7d),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "risk_snapshots", df, append = TRUE)
  invisible(nrow(df))
}

#' Fetch the most recent risk snapshot (P0-1)
#'
#' Returns the rows belonging to the newest `as_of` group. "Newest" is decided
#' by the maximum `snapshot_id` seen, so it is robust to snapshots sharing a
#' coarse timestamp.
#'
#' @param con A DBI connection.
#' @return A tibble (possibly zero-row) of the latest snapshot's rows.
#' @export
cdt_get_last_snapshot <- function(con) {
  as_of <- DBI::dbGetQuery(con,
    "SELECT as_of FROM risk_snapshots ORDER BY snapshot_id DESC LIMIT 1;")
  if (nrow(as_of) == 0) {
    return(tibble::tibble(
      patient_id = character(0), as_of = character(0),
      p_24h = numeric(0), p_7d = numeric(0), tier_7d = character(0)))
  }
  res <- DBI::dbGetQuery(con,
    "SELECT patient_id, as_of, p_24h, p_7d, tier_7d
       FROM risk_snapshots WHERE as_of = ?;",
    params = list(as_of$as_of[1]))
  tibble::as_tibble(res)
}

#' Insert an alert row (P0-1)
#'
#' @param con A DBI connection.
#' @param patient_id Patient identifier.
#' @param kind Alert category (e.g. "risk_jump", "tier_up").
#' @param severity One of "info", "warning", "critical".
#' @param delta_pts Signed change in 7-day risk (percentage points).
#' @param reason_text One-line human-readable reason.
#' @return Invisibly the new `alert_id`.
#' @export
cdt_insert_alert <- function(con, patient_id, kind, severity,
                             delta_pts = NA_real_, reason_text = NULL) {
  stopifnot(nzchar(patient_id), nzchar(kind), nzchar(severity))
  DBI::dbExecute(con,
    "INSERT INTO alerts (patient_id, kind, severity, delta_pts, reason_text)
       VALUES (?, ?, ?, ?, ?);",
    params = list(patient_id, kind, severity,
      as.numeric(delta_pts), reason_text %||% NA_character_))
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[1]
  invisible(as.integer(id))
}

#' Fetch alerts (P0-1)
#'
#' @param con A DBI connection.
#' @param only_open If `TRUE`, return only un-acknowledged alerts.
#' @return A tibble of alerts, newest first.
#' @export
cdt_get_alerts <- function(con, only_open = FALSE) {
  q <- "SELECT * FROM alerts"
  if (isTRUE(only_open)) q <- paste(q, "WHERE acknowledged_at IS NULL")
  q <- paste(q, "ORDER BY created_at DESC, alert_id DESC;")
  tibble::as_tibble(DBI::dbGetQuery(con, q))
}

#' Acknowledge an alert (P0-1)
#'
#' Stamps the acknowledgement audit fields. Re-acknowledging overwrites the
#' stamp with the latest actor/time.
#'
#' @param con A DBI connection.
#' @param alert_id Alert identifier.
#' @param acknowledged_by Optional user identifier.
#' @return Invisibly `TRUE`.
#' @export
cdt_ack_alert <- function(con, alert_id, acknowledged_by = NULL) {
  DBI::dbExecute(con,
    "UPDATE alerts
        SET acknowledged_by = ?, acknowledged_at = datetime('now')
      WHERE alert_id = ?;",
    params = list(acknowledged_by %||% NA_character_, as.integer(alert_id)))
  invisible(TRUE)
}
