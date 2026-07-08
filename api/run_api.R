#!/usr/bin/env Rscript
# Launch the plumber API.
#
# Usage:
#   Rscript api/run_api.R            # defaults to 127.0.0.1:8000
#   CDT_API_PORT=9000 Rscript api/run_api.R
#
# Environment variables:
#   ANTHROPIC_API_KEY    - enables live Claude replies (else mock mode)
#   TELEGRAM_BOT_TOKEN   - enables live Telegram sends (else mock mode)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1) {
  normalizePath(file.path(dirname(file_arg), ".."))
} else {
  normalizePath(getwd())
}
Sys.setenv(CDT_PROJECT_ROOT = root)

host <- Sys.getenv("CDT_API_HOST", unset = "127.0.0.1")
port <- as.integer(Sys.getenv("CDT_API_PORT", unset = "8000"))

message(sprintf("Starting Clinical Digital Twin API on http://%s:%d", host, port))
pr <- plumber::plumb(file.path(root, "api", "plumber.R"))
pr$run(host = host, port = port)
