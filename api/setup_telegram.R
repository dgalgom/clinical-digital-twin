#!/usr/bin/env Rscript
# One-off Telegram bot SETUP (idempotent, safe to re-run) -- POLL MODE.
#
# This registers the professional "chrome" around the bot so the chat looks
# polished before anyone types, and clears the way for long-polling:
#   1. setMyCommands       -> the folded "menu" button + command autocomplete
#                             (sourced from cdt_bot_commands(), same as /help).
#   2. setMyDescription    -> the empty-chat splash text (long) and the profile
#      setMyShortDescription  blurb (short).
#   3. deleteWebhook       -> remove any registered webhook AND drop the queue of
#                             pending updates, so getUpdates (long-polling) can
#                             consume messages without a 409 conflict and without
#                             replaying stale /start messages.
#
# It does NOT start the bot. After running this once, start the consumer with:
#   Rscript api/run_bot_poll.R      # long-polling; Ctrl-C to stop
#
# Secrets are read ONLY from the environment (or a git-ignored .env/.Renviron):
#   TELEGRAM_BOT_TOKEN   - required; from BotFather
#
# Usage:
#   Rscript setup.R                 # once, builds the synthetic DB + model
#   Rscript api/setup_telegram.R    # register menu + description, clear webhook
#   Rscript api/run_bot_poll.R      # start the bot (long-polling)
#
# -----------------------------------------------------------------------------
# WEBHOOK ROUTE -- PLACEHOLDER FOR FUTURE IMPLEMENTATION (NOT WIRED YET)
# -----------------------------------------------------------------------------
# For an always-on hosted deployment you would switch from polling to a webhook:
# publish api/plumber.R behind a public HTTPS URL and register that URL with
# Telegram (setWebhook), instead of calling deleteWebhook. When that route is
# built, flip `setup_webhook` to TRUE below and provide the two values it needs
# (a public https URL ending in /telegram/webhook, and the secret already used
# by the webhook handler's .webhook_secret_ok check, TELEGRAM_WEBHOOK_SECRET).
#
# The scaffold is intentionally INERT today: `setup_webhook` is FALSE, and the
# setWebhook call is written but never executed. Wiring it is deferred on
# purpose so this script stays a pure poll-mode setup for now.
# -----------------------------------------------------------------------------

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

# --- Toggle: poll-mode setup (default) vs. webhook route (future) ------------
# Leave FALSE for now. The webhook branch is a documented placeholder and is not
# exercised; flip to TRUE only once the hosted webhook deployment exists.
setup_webhook <- FALSE

# 1 + 2: publish the command menu and the descriptions (professional chrome).
ok_cmds <- cdt_telegram_set_commands()
message(if (isTRUE(ok_cmds)) "[ok]  registered command menu (setMyCommands)"
  else "[warn] setMyCommands did not confirm (check token/network)")

ok_desc <- cdt_telegram_set_description()
message(if (isTRUE(ok_desc)) "[ok]  set bot description + short description"
  else "[warn] setMyDescription did not confirm (check token/network)")

if (!isTRUE(setup_webhook)) {
  # 3: POLL MODE -- clear any webhook and drop stale pending updates so the
  # long-poller starts clean.
  ok_del <- cdt_telegram_delete_webhook(drop_pending = TRUE)
  message(if (isTRUE(ok_del))
    "[ok]  cleared webhook + dropped pending updates (ready for long-polling)"
  else "[warn] deleteWebhook did not confirm (check token/network)")

  message("")
  message("Setup complete (POLL MODE). Start the bot with:")
  message("  Rscript api/run_bot_poll.R")
} else {
  # -------------------------------------------------------------------------
  # FUTURE: WEBHOOK ROUTE -- placeholder, intentionally not executed today.
  # -------------------------------------------------------------------------
  # Requires a public HTTPS endpoint serving api/plumber.R and the shared
  # secret the webhook handler validates (TELEGRAM_WEBHOOK_SECRET).
  webhook_url <- Sys.getenv("CDT_WEBHOOK_URL")           # e.g. https://host/telegram/webhook
  webhook_secret <- Sys.getenv("TELEGRAM_WEBHOOK_SECRET")
  message("[skip] webhook route is a placeholder and is not wired yet.")
  message("       To enable later: deploy api/plumber.R publicly, set")
  message("       CDT_WEBHOOK_URL + TELEGRAM_WEBHOOK_SECRET, and register via")
  message("       setWebhook(url=CDT_WEBHOOK_URL, secret_token=TELEGRAM_WEBHOOK_SECRET).")
  # Deliberately NOT called:
  #   cdt_telegram_set_webhook(webhook_url, secret = webhook_secret)
  invisible(list(webhook_url = webhook_url, webhook_secret_set = nzchar(webhook_secret)))
}
