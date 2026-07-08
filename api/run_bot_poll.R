#!/usr/bin/env Rscript
# Run the Telegram bot LOCALLY via long-polling (getUpdates) -- no public URL,
# no webhook, no Render. One outbound HTTPS connection pulls new messages and
# feeds them to the same bot dispatcher the webhook uses (cdt_bot_reply).
#
# Live Claude replies turn on automatically when ANTHROPIC_API_KEY is set;
# otherwise the bot answers in deterministic MOCK mode.
#
# Usage:
#   Rscript setup.R                 # once, builds the synthetic DB + model
#   Rscript api/run_bot_poll.R      # start the bot; Ctrl-C to stop
#
# Secrets are read ONLY from the environment (or a git-ignored .env/.Renviron):
#   TELEGRAM_BOT_TOKEN   - required; from BotFather
#   ANTHROPIC_API_KEY    - optional; enables live Claude replies (else mock)
#   CDT_APP_URL          - optional; base URL used in /dashboard deep links
#
# NOTE: long-polling and webhooks are mutually exclusive. If a webhook was ever
# registered for this bot, Telegram returns HTTP 409 to getUpdates. Clear it:
#   curl "https://api.telegram.org/bot<TOKEN>/deleteWebhook"

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) {
  normalizePath(file.path(dirname(file_arg), ".."))
} else {
  normalizePath(getwd())
}
Sys.setenv(CDT_PROJECT_ROOT = root)

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Load secrets/overrides from a git-ignored .env (or .Renviron) if present.
# Existing shell vars win; empty placeholders are skipped; values are never
# printed.
cdt_load_env()

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
if (!nzchar(token)) {
  stop(
    "TELEGRAM_BOT_TOKEN is not set. Put it in a git-ignored .env (or .Renviron) ",
    "and re-run. See .Renviron.example.",
    call. = FALSE
  )
}

con <- cdt_db_connect()
model <- cdt_load_model()

message(sprintf(
  "Bot online (long-polling). LLM mode: %s. Press Ctrl-C to stop.",
  if (cdt_llm_is_mock()) "MOCK" else "LIVE Claude"
))

base <- sprintf("https://api.telegram.org/bot%s", token)
offset <- NULL

repeat {
  # Long-poll: one outbound HTTPS GET that blocks up to 50s for new updates.
  req <- httr2::request(paste0(base, "/getUpdates")) |>
    httr2::req_url_query(timeout = 50, offset = offset) |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = function(r) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) {
    Sys.sleep(3)
    next
  }
  if (httr2::resp_status(resp) == 409) {
    message(
      "Telegram returned 409 (a webhook is registered). Clear it with ",
      "deleteWebhook, then restart. Retrying in 10s..."
    )
    Sys.sleep(10)
    next
  }
  if (httr2::resp_status(resp) >= 400) {
    Sys.sleep(3)
    next
  }

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  updates <- body$result %||% list()

  for (u in updates) {
    offset <- u$update_id + 1 # ack so each update is processed once
    msg <- u$message
    if (is.null(msg)) next
    chat_id <- msg$chat$id
    text <- msg$text %||% ""

    reply <- tryCatch(
      cdt_bot_reply(con, model, chat_id, text),
      error = function(e) list(
        text = "Sorry \u2014 an internal error occurred handling that.",
        photo = NULL
      )
    )
    cdt_telegram_send(chat_id, reply$text)
    if (!is.null(reply$photo)) {
      cdt_telegram_send_photo(chat_id, reply$photo)
      unlink(reply$photo)
    }
  }
}
