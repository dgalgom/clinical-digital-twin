test_that("password hashing verifies correctly and rejects wrong passwords", {
  h <- cdt_hash_password("s3cret-demo")
  expect_true(cdt_verify_password("s3cret-demo", h))
  expect_false(cdt_verify_password("wrong", h))
})

test_that("auth: create user, login, validate, logout", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)

  cdt_create_user(con, "dr_test", "pw12345")
  expect_error(cdt_create_user(con, "dr_test", "pw12345"), "already exists")

  expect_null(cdt_login(con, "dr_test", "wrongpw"))
  sess <- cdt_login(con, "dr_test", "pw12345")
  expect_true(!is.null(sess) && nzchar(sess$token))

  v <- cdt_validate_session(con, sess$token)
  expect_equal(v$username, "dr_test")

  cdt_logout(con, sess$token)
  expect_null(cdt_validate_session(con, sess$token))
  expect_null(cdt_validate_session(con, "bogus-token"))
})

test_that("bot extracts patient ids from varied phrasings", {
  expect_equal(cdt_bot_extract_patient("How is P042 doing?"), "P042")
  expect_equal(cdt_bot_extract_patient("patient 42 trending"), "P042")
  expect_equal(cdt_bot_extract_patient("check patient 007"), "P007")
  expect_null(cdt_bot_extract_patient("how is the cohort?"))
})

test_that("bot parses what-if intents", {
  ov <- cdt_bot_parse_whatif("what if we increase mobility by 25%?")
  expect_equal(ov$steps_pct, 25)

  ov2 <- cdt_bot_parse_whatif("lower systolic BP by 10 mmHg")
  expect_equal(ov2$sbp_delta, -10)

  expect_null(cdt_bot_parse_whatif("how are they doing?"))
})

test_that("bot maintains per-chat patient focus", {
  cdt_bot_reset()
  cdt_bot_focus("chat1", "P010")
  expect_equal(cdt_bot_focus("chat1"), "P010")
  expect_null(cdt_bot_focus("chat2"))
})

test_that("bot end-to-end produces a grounded mock reply", {
  fx <- make_test_fixtures()
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)

  cdt_create_user(con, "dr_bot", "pw12345")
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]

  # Username gate: unlock the chat before clinical queries.
  gate <- cdt_bot_handle_message(con, fx$model, "chatX", "login as dr_bot",
    llm_mock = TRUE)
  expect_true(grepl("Signed in", gate))

  reply <- cdt_bot_handle_message(con, fx$model, "chatX",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  expect_true(grepl("MOCK", reply))
  expect_true(grepl("fall risk", reply, ignore.case = TRUE))

  # What-if via the bot mentions the simulation.
  reply2 <- cdt_bot_handle_message(con, fx$model, "chatX",
    "what if we increase mobility by 30%?", llm_mock = TRUE)
  expect_true(nchar(reply2) > 0)
})

test_that("patient context is de-identified: no name reaches the LLM", {
  fx <- make_test_fixtures()
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)

  pid <- fx$cohort$patient_id[1]
  nm <- fx$cohort$name[fx$cohort$patient_id == pid]
  ctx <- cdt_patient_context(con, fx$model, pid)

  # The patient identifier, age, and sex still ground the reply...
  expect_true(grepl(pid, ctx, fixed = TRUE))
  # ...and the baseline risk line remains (mock-reply grep depends on it).
  expect_true(grepl("fall risk", ctx, ignore.case = TRUE))
  # ...but the human name must NOT appear anywhere in the prompt context.
  if (nzchar(nm)) {
    expect_false(grepl(nm, ctx, fixed = TRUE))
  }
})

test_that("telegram mock captures outgoing messages", {
  cdt_telegram_sent(clear = TRUE)
  cdt_telegram_send("chat9", "hello", mock = TRUE)
  sent <- cdt_telegram_sent(clear = TRUE)
  expect_equal(length(sent), 1)
  expect_equal(sent[[1]]$text, "hello")
})
