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
