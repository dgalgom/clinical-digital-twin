# Coverage for the one-off Telegram SETUP helpers (setMyCommands /
# setMyDescription / deleteWebhook). All offline: mock mode captures each call
# in the in-memory sink as list(type = "api", method, body) so we can assert the
# request shape without touching the network.

# Force mock capture regardless of any ambient TELEGRAM_BOT_TOKEN.
.with_tg_mock <- function(code) {
  old <- Sys.getenv("CDT_MOCK_TELEGRAM", unset = NA)
  Sys.setenv(CDT_MOCK_TELEGRAM = "1")
  on.exit({
    if (is.na(old)) Sys.unsetenv("CDT_MOCK_TELEGRAM") else
      Sys.setenv(CDT_MOCK_TELEGRAM = old)
  }, add = TRUE)
  force(code)
}

# Pull the most recent captured API call of a given method.
.last_api <- function(method) {
  sent <- cdt_telegram_sent()
  api <- Filter(function(e) identical(e$type, "api") &&
    identical(e$method, method), sent)
  if (length(api) == 0) NULL else api[[length(api)]]
}

test_that("cdt_telegram_set_commands publishes the full command menu", {
  .with_tg_mock({
    cdt_telegram_sent(clear = TRUE)
    ok <- cdt_telegram_set_commands()
    expect_true(isTRUE(ok))

    call <- .last_api("setMyCommands")
    expect_false(is.null(call))
    cmds <- call$body$commands
    expect_true(length(cmds) >= 1)

    names_sent <- vapply(cmds, function(c) c$command, character(1))
    # Menu mirrors cdt_bot_commands(); names are lowercase and slash-free.
    expect_setequal(names_sent, cdt_bot_commands()$command)
    expect_false(any(grepl("^/", names_sent)))
    expect_identical(names_sent, tolower(names_sent))
    # Every command carries a non-empty description.
    descs <- vapply(cmds, function(c) c$description, character(1))
    expect_true(all(nzchar(descs)))
  })
})

test_that("cdt_telegram_set_commands accepts a custom command frame", {
  .with_tg_mock({
    cdt_telegram_sent(clear = TRUE)
    custom <- data.frame(
      command = c("/Foo", "bar"),
      description = c("Foo cmd", "Bar cmd"),
      stringsAsFactors = FALSE
    )
    cdt_telegram_set_commands(custom)
    call <- .last_api("setMyCommands")
    names_sent <- vapply(call$body$commands, function(c) c$command, character(1))
    # Leading slash stripped and lowercased.
    expect_identical(names_sent, c("foo", "bar"))
  })
})

test_that("cdt_telegram_set_description sends both long and short blurbs", {
  .with_tg_mock({
    cdt_telegram_sent(clear = TRUE)
    ok <- cdt_telegram_set_description()
    expect_true(isTRUE(ok))

    long <- .last_api("setMyDescription")
    short <- .last_api("setMyShortDescription")
    expect_false(is.null(long))
    expect_false(is.null(short))
    # Defaults come from the bot's own copy; caps respected.
    expect_true(nzchar(long$body$description))
    expect_lte(nchar(long$body$description), 512L)
    expect_true(nzchar(short$body$short_description))
    expect_lte(nchar(short$body$short_description), 120L)
  })
})

test_that("cdt_telegram_set_description truncates over-long input", {
  .with_tg_mock({
    cdt_telegram_sent(clear = TRUE)
    big <- paste(rep("x", 900), collapse = "")
    cdt_telegram_set_description(description = big, short_description = big)
    long <- .last_api("setMyDescription")
    short <- .last_api("setMyShortDescription")
    expect_identical(nchar(long$body$description), 512L)
    expect_identical(nchar(short$body$short_description), 120L)
  })
})

test_that("cdt_telegram_delete_webhook drops pending updates by default", {
  .with_tg_mock({
    cdt_telegram_sent(clear = TRUE)
    ok <- cdt_telegram_delete_webhook()
    expect_true(isTRUE(ok))
    call <- .last_api("deleteWebhook")
    expect_false(is.null(call))
    expect_true(isTRUE(call$body$drop_pending_updates))

    cdt_telegram_delete_webhook(drop_pending = FALSE)
    call2 <- .last_api("deleteWebhook")
    expect_false(isTRUE(call2$body$drop_pending_updates))
  })
})

test_that("setup helpers accept an explicit mock override", {
  # Even with no ambient mock env, mock=TRUE captures instead of sending.
  cdt_telegram_sent(clear = TRUE)
  cdt_telegram_set_commands(mock = TRUE)
  expect_false(is.null(.last_api("setMyCommands")))
})
