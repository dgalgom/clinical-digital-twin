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
#' @param clear If `TRUE`, empty the sink after returning.
#' @return A list of `list(chat_id, text)` entries, most recent last.
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
      list(list(chat_id = chat_id, text = text))
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
