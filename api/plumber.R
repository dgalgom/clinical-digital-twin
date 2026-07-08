# ---------------------------------------------------------------------------
# plumber REST API for the Clinical Digital Twin Monitoring System.
#
# Endpoints:
#   POST /login                      -> issue a session token
#   POST /logout                     -> invalidate a token
#   GET  /cohort        (auth)       -> cohort snapshot with risk tiers
#   GET  /patient/<id>  (auth)       -> patient static + timeline + risk
#   POST /predict       (auth)       -> risk with optional counterfactuals
#   POST /telegram/webhook           -> Telegram update handler (bot)
#   GET  /health                     -> liveness check
#
# Auth model (MVP): send the session token in the `Authorization: Bearer <t>`
# header (or `?token=`). See R/auth.R for the documented simplifications.
#
# Run:  Rscript api/run_api.R      (see that launcher for host/port)
# ---------------------------------------------------------------------------

# Resolve root and source package code.
.root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
if (is.na(.root) || !nzchar(.root)) {
  .root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."))
}
if (is.na(.root) || !nzchar(.root)) .root <- normalizePath(getwd())
Sys.setenv(CDT_PROJECT_ROOT = .root)
for (f in list.files(file.path(.root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Shared, long-lived resources for the API process.
.api_con <- cdt_db_connect()
.api_model <- cdt_load_model()

#* @apiTitle Clinical Digital Twin API (Hackathon MVP)
#* @apiDescription Synthetic-data fall-risk digital twin. Not for clinical use.

# --- helpers ---------------------------------------------------------------

.extract_token <- function(req) {
  auth <- req$HTTP_AUTHORIZATION
  if (!is.null(auth) && grepl("^Bearer ", auth)) {
    return(sub("^Bearer ", "", auth))
  }
  q <- req$args$token
  if (!is.null(q)) {
    return(q)
  }
  NULL
}

.require_auth <- function(req, res) {
  token <- .extract_token(req)
  sess <- cdt_validate_session(.api_con, token)
  if (is.null(sess)) {
    res$status <- 401
    return(NULL)
  }
  sess
}

# --- endpoints -------------------------------------------------------------

#* Liveness check
#* @get /health
function() {
  list(status = "ok", service = "clinical-digital-twin", synthetic_data = TRUE)
}

#* Log in and receive a session token
#* @post /login
#* @param username:str
#* @param password:str
function(req, res, username = "", password = "") {
  sess <- cdt_login(.api_con, username, password)
  if (is.null(sess)) {
    res$status <- 401
    return(list(error = "invalid_credentials"))
  }
  list(token = sess$token, role = sess$role)
}

#* Log out (invalidate token)
#* @post /logout
function(req, res) {
  token <- .extract_token(req)
  cdt_logout(.api_con, token)
  list(status = "logged_out")
}

#* Cohort snapshot with current risk tiers
#* @get /cohort
function(req, res) {
  sess <- .require_auth(req, res)
  if (is.null(sess)) {
    return(list(error = "unauthorized"))
  }
  snap <- cdt_cohort_snapshot(.api_con, .api_model)
  snap[, c(
    "patient_id", "name", "age", "sex", "prior_falls",
    "p_24h", "p_7d", "tier_24h", "tier_7d"
  )]
}

#* Patient detail: static data, recent timeline, and current risk
#* @get /patient/<id>
function(req, res, id) {
  sess <- .require_auth(req, res)
  if (is.null(sess)) {
    return(list(error = "unauthorized"))
  }
  patient <- cdt_get_patient(.api_con, id)
  if (nrow(patient) == 0) {
    res$status <- 404
    return(list(error = "not_found"))
  }
  timeline <- cdt_get_patient_timeline(.api_con, id)
  risk <- cdt_patient_risk(.api_con, .api_model, id, include_baseline = FALSE)
  list(
    patient = patient,
    timeline = timeline,
    risk = risk,
    falls = cdt_get_fall_events(.api_con, id)
  )
}

#* Predict fall risk with optional counterfactual overrides (the digital twin)
#* @post /predict
#* @param patient_id:str
function(req, res, patient_id = "") {
  sess <- .require_auth(req, res)
  if (is.null(sess)) {
    return(list(error = "unauthorized"))
  }

  # Counterfactual overrides come in the JSON body under `modified_inputs`.
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  modified <- body$modified_inputs
  pid <- if (nzchar(patient_id)) patient_id else body$patient_id

  risk <- cdt_patient_risk(.api_con, .api_model, pid,
    modified_inputs = modified, include_baseline = TRUE
  )
  if (is.null(risk)) {
    res$status <- 404
    return(list(error = "not_found"))
  }
  risk
}

#* Telegram webhook: receive an update, run the bot, reply via Telegram
#* @post /telegram/webhook
function(req, res) {
  update <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(update) || is.null(update$message)) {
    return(list(ok = TRUE))
  }
  chat_id <- update$message$chat$id
  text <- update$message$text %||% ""

  reply <- cdt_bot_handle_message(.api_con, .api_model, chat_id, text)
  cdt_telegram_send(chat_id, reply)

  list(ok = TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
