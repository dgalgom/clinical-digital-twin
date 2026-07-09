#' Telegram Bot API client (httr2)
#'
#' Sends messages via the Telegram Bot API. The token is read ONLY from the
#' `TELEGRAM_BOT_TOKEN` environment variable.
#'
#' Mock mode: if the token is absent, or `CDT_MOCK_TELEGRAM=1`, or `mock = TRUE`,
#' outgoing messages are captured in-memory (see [cdt_telegram_sent()]) instead
#' of being sent, so the webhook logic can be tested offline.

# In-memory sink for mock mode.
.cdt_tg_env <- new.env(parent = emptyenv())
.cdt_tg_env$sent <- list()

#' Is the Telegram client in mock mode?
#' @param mock Explicit override.
#' @return Logical.
#' @export
cdt_telegram_is_mock <- function(mock = NULL) {
  if (!is.null(mock)) {
    return(isTRUE(mock))
  }
  if (identical(Sys.getenv("CDT_MOCK_TELEGRAM"), "1")) {
    return(TRUE)
  }
  !nzchar(Sys.getenv("TELEGRAM_BOT_TOKEN"))
}

#' Retrieve (and optionally clear) messages captured in mock mode
#'
#' Each entry is a list tagged with `type`: text entries are
#' `list(type = "text", chat_id, text)` and photo entries are
#' `list(type = "photo", chat_id, photo, caption)`. The `chat_id`/`text` keys on
#' text entries are unchanged for back-compat.
#'
#' @param clear If `TRUE`, empty the sink after returning.
#' @return A list of captured send entries, most recent last.
#' @export
cdt_telegram_sent <- function(clear = FALSE) {
  out <- .cdt_tg_env$sent
  if (clear) {
    .cdt_tg_env$sent <- list()
  }
  out
}

#' Send a text message to a Telegram chat
#'
#' @param chat_id Target chat id.
#' @param text Message text.
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` on success (or capture in mock mode).
#' @export
cdt_telegram_send <- function(chat_id, text, mock = NULL) {
  if (cdt_telegram_is_mock(mock)) {
    .cdt_tg_env$sent <- c(
      .cdt_tg_env$sent,
      list(list(type = "text", chat_id = chat_id, text = text))
    )
    return(invisible(TRUE))
  }

  token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  url <- sprintf("https://api.telegram.org/bot%s/sendMessage", token)
  resp <- httr2::request(url) |>
    httr2::req_body_json(list(chat_id = chat_id, text = text)) |>
    httr2::req_timeout(20) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  invisible(httr2::resp_status(resp) < 400)
}

#' Show a "typing..." chat action in a Telegram chat
#'
#' Sends `sendChatAction` with action `typing`, which makes Telegram display the
#' bot's "typing..." status for a few seconds. This is a pure UX cue used to
#' mask model latency while a reply is being generated; it never blocks and its
#' failure is non-fatal (the reply is sent regardless). In mock mode the call is
#' captured in the same in-memory sink as `list(type = "action", chat_id,
#' action)` so the webhook path can be exercised offline.
#'
#' @param chat_id Target chat id.
#' @param action Chat action string (default `"typing"`).
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` on success (or capture in mock mode), `FALSE` on a
#'   transport error.
#' @export
cdt_telegram_typing <- function(chat_id, action = "typing", mock = NULL) {
  if (cdt_telegram_is_mock(mock)) {
    .cdt_tg_env$sent <- c(
      .cdt_tg_env$sent,
      list(list(type = "action", chat_id = chat_id, action = action))
    )
    return(invisible(TRUE))
  }

  token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  url <- sprintf("https://api.telegram.org/bot%s/sendChatAction", token)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_body_json(list(chat_id = chat_id, action = action)) |>
      httr2::req_timeout(10) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  invisible(!is.null(resp) && httr2::resp_status(resp) < 400)
}

# Internal: POST a JSON body to a Telegram Bot API method, returning TRUE when
# the API answers < 400. In mock mode the call is captured in the shared sink as
# `list(type = "api", method, body)` so setup flows can be exercised offline.
# Failures are non-fatal (return FALSE) so one-off setup never crashes a deploy.
.cdt_tg_api_post <- function(method, body, mock = NULL) {
  if (cdt_telegram_is_mock(mock)) {
    .cdt_tg_env$sent <- c(
      .cdt_tg_env$sent,
      list(list(type = "api", method = method, body = body))
    )
    return(invisible(TRUE))
  }
  token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  url <- sprintf("https://api.telegram.org/bot%s/%s", token, method)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(20) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  invisible(!is.null(resp) && httr2::resp_status(resp) < 400)
}

#' Register the bot's slash-command menu with Telegram (`setMyCommands`)
#'
#' Publishes the command list to Telegram so the chat shows the folded "menu"
#' button and command autocomplete. The list defaults to [cdt_bot_commands()]
#' (the same source `/help` renders), so the menu and the in-chat help never
#' drift apart. This is an idempotent, one-off SETUP call (run it from
#' `api/setup_telegram.R`), not part of the per-message path.
#'
#' @param commands A data frame with `command`/`description` columns; defaults
#'   to [cdt_bot_commands()].
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` on success (or capture in mock mode).
#' @export
cdt_telegram_set_commands <- function(commands = NULL, mock = NULL) {
  if (is.null(commands)) {
    commands <- cdt_bot_commands()
  }
  # Telegram requires command names 1-32 chars, lowercase, no leading slash.
  cmd_list <- lapply(seq_len(nrow(commands)), function(i) {
    list(
      command = tolower(sub("^/", "", commands$command[i])),
      description = commands$description[i]
    )
  })
  .cdt_tg_api_post("setMyCommands", list(commands = cmd_list), mock = mock)
}

#' Set the bot's long and short descriptions (`setMyDescription`)
#'
#' The long description is shown on the bot's empty-chat splash screen (before
#' the user sends anything); the short description appears on the bot's profile
#' and in shared links. A polished first impression, matching the sibling
#' project's presentation. Idempotent SETUP call; defaults reuse the bot's own
#' [cdt_bot_description()]/[cdt_bot_tagline()] copy.
#'
#' Telegram caps the long description at 512 chars and the short one at 120, so
#' both are truncated defensively before sending.
#'
#' @param description Long description (empty-chat splash). Defaults to
#'   [cdt_bot_description()].
#' @param short_description Short profile blurb. Defaults to [cdt_bot_tagline()].
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` if both calls succeed (or capture in mock mode).
#' @export
cdt_telegram_set_description <- function(description = NULL,
                                         short_description = NULL,
                                         mock = NULL) {
  if (is.null(description)) {
    description <- cdt_bot_description()
  }
  if (is.null(short_description)) {
    short_description <- cdt_bot_tagline()
  }
  clip <- function(s, n) if (nchar(s) > n) substr(s, 1, n) else s
  ok1 <- .cdt_tg_api_post(
    "setMyDescription",
    list(description = clip(description, 512L)), mock = mock
  )
  ok2 <- .cdt_tg_api_post(
    "setMyShortDescription",
    list(short_description = clip(short_description, 120L)), mock = mock
  )
  invisible(isTRUE(ok1) && isTRUE(ok2))
}

#' Remove any registered webhook so long-polling can consume updates
#'
#' Telegram allows EITHER a webhook OR `getUpdates` long-polling, never both:
#' if a webhook is set, `getUpdates` returns HTTP 409. This clears any webhook
#' (and, by default, drops the queue of pending updates so a fresh poller does
#' not replay stale `/start`s). Call it before starting `api/run_bot_poll.R`.
#'
#' NOTE: this is the POLL-mode setup path. The webhook route is intentionally
#' NOT wired yet -- see the `setup_webhook = TRUE` placeholder in
#' `api/setup_telegram.R` for the future switch.
#'
#' @param drop_pending If `TRUE` (default), also discard queued updates.
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` on success (or capture in mock mode).
#' @export
cdt_telegram_delete_webhook <- function(drop_pending = TRUE, mock = NULL) {
  .cdt_tg_api_post(
    "deleteWebhook",
    list(drop_pending_updates = isTRUE(drop_pending)), mock = mock
  )
}

#' Send a photo (PNG) to a Telegram chat via `sendPhoto`
#'
#' Uploads a local image file as multipart/form-data. In mock mode the send is
#' captured in the same in-memory sink as `list(type = "photo", chat_id, photo,
#' caption)` so the webhook's image path can be tested offline.
#'
#' @param chat_id Target chat id.
#' @param path Path to a local image file (e.g. a rendered PNG).
#' @param caption Optional caption text (Telegram caps captions at ~1024 chars).
#' @param mock Optional explicit mock override.
#' @return Invisibly `TRUE` on success (or capture in mock mode), `FALSE` if the
#'   file is missing or the API returns an error.
#' @export
cdt_telegram_send_photo <- function(chat_id, path, caption = NULL, mock = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(invisible(FALSE))
  }

  if (cdt_telegram_is_mock(mock)) {
    .cdt_tg_env$sent <- c(
      .cdt_tg_env$sent,
      list(list(
        type = "photo", chat_id = chat_id, photo = path, caption = caption
      ))
    )
    return(invisible(TRUE))
  }

  token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  url <- sprintf("https://api.telegram.org/bot%s/sendPhoto", token)
  fields <- list(
    chat_id = as.character(chat_id),
    photo = curl::form_file(path)
  )
  if (!is.null(caption) && nzchar(caption)) {
    fields$caption <- caption
  }
  resp <- do.call(httr2::req_body_multipart, c(list(httr2::request(url)), fields)) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  invisible(httr2::resp_status(resp) < 400)
}
