#!/usr/bin/env Rscript
# End-to-end integration smoke check (no network; mock LLM/Telegram).
# Verifies the plumber router builds and the webhook handler replies.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) normalizePath(file.path(dirname(file_arg), "..")) else normalizePath(getwd())
Sys.setenv(CDT_PROJECT_ROOT = root)
Sys.setenv(CDT_MOCK_LLM = "1", CDT_MOCK_TELEGRAM = "1")

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

# 1. Router builds without error.
pr <- plumber::plumb(file.path(root, "api", "plumber.R"))
stopifnot(inherits(pr, "Plumber"))
cat("[ok] plumber router builds\n")

# 2. Simulate the Telegram webhook handler directly.
con <- cdt_db_connect()
model <- cdt_load_model()
cdt_bot_reset()
invisible(cdt_telegram_sent(clear = TRUE))

# Username gate: identify with the seeded demo clinician before querying.
gate <- cdt_bot_handle_message(con, model, chat_id = 12345, text = "login as clinician")
stopifnot(grepl("Signed in", gate))

reply <- cdt_bot_handle_message(con, model, chat_id = 12345,
  text = "How is patient P048 trending?")
cdt_telegram_send(12345, reply)
sent <- cdt_telegram_sent()
stopifnot(length(sent) == 1)
cat("[ok] webhook produced a reply:\n    ",
  gsub("\n", "\n     ", substr(sent[[1]]$text, 1, 200)), "\n")

# 3. What-if over the bot.
reply2 <- cdt_bot_handle_message(con, model, chat_id = 12345,
  text = "what if we increase their mobility by 30%?")
cat("[ok] what-if reply generated (", nchar(reply2), "chars )\n")

DBI::dbDisconnect(con)
cat("\nIntegration check passed.\n")
