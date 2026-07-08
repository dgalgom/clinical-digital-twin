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
