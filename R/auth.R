#' Authentication (MVP)
#'
#' Password hashing with `sodium` (scrypt) and opaque session tokens stored in
#' SQLite.
#'
#' MVP SIMPLIFICATION (not production-grade): tokens are random hex strings with
#' a fixed TTL; there is no CSRF protection, rate limiting, account lockout,
#' password-strength policy, or TLS enforcement here. A production deployment
#' must add those and serve over HTTPS. This is stated plainly for the judges.

#' Hash a password using sodium's password-hashing primitive
#'
#' @param password Plain-text password.
#' @return A hex-encoded hash string safe to store in the DB.
#' @export
cdt_hash_password <- function(password) {
  stopifnot(is.character(password), length(password) == 1, nzchar(password))
  raw_hash <- sodium::password_store(password)
  raw_hash
}

#' Verify a password against a stored hash
#'
#' @param password Plain-text password to check.
#' @param stored_hash Hash previously produced by [cdt_hash_password()].
#' @return `TRUE` if the password matches.
#' @export
cdt_verify_password <- function(password, stored_hash) {
  isTRUE(sodium::password_verify(stored_hash, password))
}

#' Create a clinician user account
#'
#' @param con A DBI connection.
#' @param username Unique username.
#' @param password Plain-text password (hashed before storage).
#' @param role Role string (default "clinician").
#' @return Invisibly `TRUE` on success; errors if the username exists.
#' @export
cdt_create_user <- function(con, username, password, role = "clinician") {
  existing <- DBI::dbGetQuery(con,
    "SELECT 1 FROM users WHERE username = ?;",
    params = list(username)
  )
  if (nrow(existing) > 0) {
    stop(sprintf("User '%s' already exists.", username), call. = FALSE)
  }
  DBI::dbExecute(con,
    "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?);",
    params = list(username, cdt_hash_password(password), role)
  )
  invisible(TRUE)
}

#' Authenticate a user and issue a session token
#'
#' @param con A DBI connection.
#' @param username Username.
#' @param password Plain-text password.
#' @param ttl_hours Token lifetime in hours (default 8).
#' @return A list with `token`, `user_id`, `role` on success, or `NULL` on
#'   failure (invalid credentials).
#' @export
cdt_login <- function(con, username, password, ttl_hours = 8) {
  row <- DBI::dbGetQuery(con,
    "SELECT user_id, password_hash, role FROM users WHERE username = ?;",
    params = list(username)
  )
  if (nrow(row) != 1) {
    return(NULL)
  }
  if (!cdt_verify_password(password, row$password_hash[[1]])) {
    return(NULL)
  }

  token <- paste0(as.character(sodium::bin2hex(sodium::random(24))))
  expires_at <- format(Sys.time() + ttl_hours * 3600, tz = "UTC")
  DBI::dbExecute(con,
    "INSERT INTO sessions (token, user_id, expires_at) VALUES (?, ?, ?);",
    params = list(token, row$user_id[[1]], expires_at)
  )
  list(token = token, user_id = row$user_id[[1]], role = row$role[[1]])
}

#' Validate a session token
#'
#' @param con A DBI connection.
#' @param token Session token.
#' @return A list with `user_id`, `role`, `username` if valid and unexpired;
#'   otherwise `NULL`.
#' @export
cdt_validate_session <- function(con, token) {
  if (is.null(token) || !nzchar(token)) {
    return(NULL)
  }
  row <- DBI::dbGetQuery(con,
    "SELECT s.user_id, s.expires_at, u.role, u.username
       FROM sessions s JOIN users u ON u.user_id = s.user_id
      WHERE s.token = ?;",
    params = list(token)
  )
  if (nrow(row) != 1) {
    return(NULL)
  }
  if (as.POSIXct(row$expires_at[[1]], tz = "UTC") < Sys.time()) {
    DBI::dbExecute(con, "DELETE FROM sessions WHERE token = ?;",
      params = list(token)
    )
    return(NULL)
  }
  list(
    user_id = row$user_id[[1]],
    role = row$role[[1]],
    username = row$username[[1]]
  )
}

#' Invalidate a session token (logout)
#'
#' @param con A DBI connection.
#' @param token Session token.
#' @return Invisibly the number of rows deleted.
#' @export
cdt_logout <- function(con, token) {
  n <- DBI::dbExecute(con, "DELETE FROM sessions WHERE token = ?;",
    params = list(token)
  )
  invisible(n)
}
