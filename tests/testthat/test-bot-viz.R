# Coverage for Step 5b: the visualization classifier/router, the PNG renderers,
# the sendPhoto client, and the structured {text, photo} bot reply. All offline
# (mock LLM + mock Telegram); no network.

# --- helpers ---------------------------------------------------------------

# TRUE if `p` is an existing file whose first 4 bytes are the PNG signature.
.is_png_file <- function(p) {
  if (is.null(p) || !is.character(p) || !nzchar(p) || !file.exists(p)) {
    return(FALSE)
  }
  sig <- readBin(p, "raw", 8L)
  identical(as.integer(sig[1:4]), c(137L, 80L, 78L, 71L))
}

# A DB seeded with the shared fixture cohort/readings/falls + a demo user.
.seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) {
    cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  }
  cdt_create_user(con, "dr_viz", "pw12345")
  con
}

# Unlock a chat's username gate for the seeded demo user.
.bot_login <- function(con, model, chat_id) {
  cdt_bot_handle_message(con, model, chat_id, "login as dr_viz", llm_mock = TRUE)
}

# --- classifier taxonomy ---------------------------------------------------

test_that("intent taxonomy is the fixed set of seven renderable intents", {
  expect_setequal(
    cdt_bot_intents(),
    c("fall_history", "functional_history", "steps_over_time",
      "resting_hr_over_time", "sbp_over_time", "sedentary_over_time", "whatif")
  )
})

test_that("deterministic classifier routes queries to the right intent", {
  expect_equal(
    cdt_bot_classify_query("daily steps for patient P004")$intent,
    "steps_over_time"
  )
  expect_equal(
    cdt_bot_classify_query("resting heart rate trend for P004")$intent,
    "resting_hr_over_time"
  )
  expect_equal(
    cdt_bot_classify_query("systolic blood pressure for P004")$intent,
    "sbp_over_time"
  )
  expect_equal(
    cdt_bot_classify_query("sedentary hours for P004")$intent,
    "sedentary_over_time"
  )
  expect_equal(
    cdt_bot_classify_query("fall history of patient P006")$intent,
    "fall_history"
  )
  expect_equal(
    cdt_bot_classify_query("how is patient P004 trending?")$intent,
    "functional_history"
  )
  expect_equal(
    cdt_bot_classify_query("what if we increase P004 mobility by 20%?")$intent,
    "whatif"
  )
})

test_that("classifier resolves the patient id and carries a metric for series", {
  spec <- cdt_bot_classify_query("daily steps for patient 4")
  expect_equal(spec$patient_id, "P004")
  expect_equal(spec$metric, "steps")

  # Unrecognized subject -> NA intent, but patient still resolved.
  spec2 <- cdt_bot_classify_query("tell me a joke about P004")
  expect_true(is.na(spec2$intent))
  expect_equal(spec2$patient_id, "P004")
})

test_that("classifier fills the patient from chat focus when omitted", {
  cdt_bot_reset()
  cdt_bot_focus("chatViz", "P011")
  spec <- cdt_bot_classify_query("show me the steps", chat_id = "chatViz")
  expect_equal(spec$patient_id, "P011")
  cdt_bot_reset()
})

# --- router (offline path) -------------------------------------------------

test_that("router falls back to the deterministic classifier offline", {
  spec <- cdt_bot_route_query("daily steps for patient P004", mock = TRUE)
  expect_equal(spec$intent, "steps_over_time")
  expect_equal(spec$source, "fallback")
  expect_true(isTRUE(spec$graded$pass))
})

# --- deterministic grader --------------------------------------------------

test_that("grader passes a valid spec and fails malformed ones", {
  good <- list(intent = "steps_over_time", patient_id = "P004", metric = "steps")
  expect_true(cdt_bot_grade_classification("steps for P004", good)$pass)

  no_metric <- list(intent = "steps_over_time", patient_id = "P004", metric = NULL)
  expect_false(cdt_bot_grade_classification("steps for P004", no_metric)$pass)

  no_pid <- list(intent = "functional_history", patient_id = NULL)
  expect_false(cdt_bot_grade_classification("how are they?", no_pid)$pass)

  bad_intent <- list(intent = "made_up_intent", patient_id = "P004")
  expect_false(cdt_bot_grade_classification("x", bad_intent)$pass)
})

# --- PNG renderers ---------------------------------------------------------

test_that("series renderer produces a PNG for a real metric, NULL when empty", {
  fx <- make_test_fixtures()
  pid <- fx$cohort$patient_id[1]
  r <- fx$sim$readings[fx$sim$readings$patient_id == pid, ]

  for (m in c("steps", "resting_hr", "sbp", "sedentary")) {
    p <- cdt_bot_plot_series(r, pid, m)
    expect_true(.is_png_file(p), info = m)
    unlink(p)
  }

  # Empty readings -> NULL (caller falls back to text).
  expect_null(cdt_bot_plot_series(r[0, ], pid, "steps"))
})

test_that("history renderer produces a PNG with and without fall markers", {
  fx <- make_test_fixtures()
  pid <- fx$cohort$patient_id[1]
  r <- fx$sim$readings[fx$sim$readings$patient_id == pid, ]
  falls <- fx$sim$falls[fx$sim$falls$patient_id == pid, ]

  p1 <- cdt_bot_plot_history(r, pid, fall_dates = falls$ts)
  expect_true(.is_png_file(p1))
  unlink(p1)

  p2 <- cdt_bot_plot_history(r, pid, fall_dates = NULL)
  expect_true(.is_png_file(p2))
  unlink(p2)

  expect_null(cdt_bot_plot_history(r[0, ], pid))
})

test_that("what-if renderer needs a baseline; NULL otherwise", {
  fx <- make_test_fixtures()
  pid <- fx$cohort$patient_id[1]
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))

  risk <- cdt_patient_risk(con, fx$model, pid,
    modified_inputs = list(steps_pct = 20), include_baseline = TRUE)
  p <- cdt_bot_plot_whatif(risk, pid, caption = "steps +20%")
  expect_true(.is_png_file(p))
  unlink(p)

  # A prediction without a baseline block -> NULL.
  expect_null(cdt_bot_plot_whatif(list(p_24h = 0.1, p_7d = 0.2), pid))
})

# --- sendPhoto client (mock sink) ------------------------------------------

test_that("send_photo captures a photo entry in mock mode", {
  cdt_telegram_sent(clear = TRUE)
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 300, height = 200)
  graphics::par(mar = c(2, 2, 1, 1)); plot(1:3)
  grDevices::dev.off()

  ok <- cdt_telegram_send_photo("chatP", png, caption = "cap", mock = TRUE)
  expect_true(ok)
  sent <- cdt_telegram_sent(clear = TRUE)
  expect_equal(length(sent), 1L)
  expect_equal(sent[[1]]$type, "photo")
  expect_equal(sent[[1]]$chat_id, "chatP")
  expect_equal(sent[[1]]$photo, png)
  expect_equal(sent[[1]]$caption, "cap")
  unlink(png)
})

test_that("send_photo returns FALSE and captures nothing for a missing file", {
  cdt_telegram_sent(clear = TRUE)
  expect_false(cdt_telegram_send_photo("chatP", NULL, mock = TRUE))
  expect_false(cdt_telegram_send_photo("chatP", "", mock = TRUE))
  expect_false(cdt_telegram_send_photo("chatP",
    file.path(tempdir(), "nope.png"), mock = TRUE))
  expect_equal(length(cdt_telegram_sent(clear = TRUE)), 0L)
})

# --- structured reply ------------------------------------------------------

test_that("cdt_bot_reply returns {text, photo} and renders a chart", {
  fx <- make_test_fixtures()
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .bot_login(con, fx$model, "chatR")

  r <- cdt_bot_reply(con, fx$model, "chatR",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  expect_true(is.list(r) && all(c("text", "photo") %in% names(r)))
  expect_true(is.character(r$text) && nzchar(r$text))
  expect_true(.is_png_file(r$photo))
  unlink(r$photo)
})

test_that("cdt_bot_reply gives text-only for /start and unknown patients", {
  fx <- make_test_fixtures()
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()

  # /start is available before authentication.
  r1 <- cdt_bot_reply(con, fx$model, "chatR2", "/start", llm_mock = TRUE)
  expect_true(nzchar(r1$text))
  expect_null(r1$photo)

  # After login, an unknown patient still yields a text-only reply.
  .bot_login(con, fx$model, "chatR3")
  r2 <- cdt_bot_reply(con, fx$model, "chatR3",
    "How is patient P999 doing?", llm_mock = TRUE)
  expect_true(nzchar(r2$text))
  expect_null(r2$photo)
})

test_that("cdt_bot_handle_message stays a bare-string wrapper (back-compat)", {
  fx <- make_test_fixtures()
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .bot_login(con, fx$model, "chatR4")

  s <- cdt_bot_handle_message(con, fx$model, "chatR4",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  expect_true(is.character(s) && length(s) == 1L && nzchar(s))
})

# --- username gate ---------------------------------------------------------

test_that("bot username gate blocks until a known username is provided", {
  fx <- make_test_fixtures()
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]

  # Before login, a clinical query is refused (asked to identify), no photo.
  r0 <- cdt_bot_reply(con, fx$model, "chatGate",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  expect_true(grepl("identify yourself", r0$text, ignore.case = TRUE))
  expect_null(r0$photo)

  # An unknown username is rejected.
  r1 <- cdt_bot_reply(con, fx$model, "chatGate", "login as nobody",
    llm_mock = TRUE)
  expect_true(grepl("couldn't find the username", r1$text))
  expect_null(cdt_bot_authed("chatGate"))

  # A known username unlocks the chat.
  r2 <- cdt_bot_reply(con, fx$model, "chatGate", "login as dr_viz",
    llm_mock = TRUE)
  expect_true(grepl("signed in", r2$text, ignore.case = TRUE))
  expect_equal(cdt_bot_authed("chatGate"), "dr_viz")

  # Now the clinical query goes through and renders a chart.
  r3 <- cdt_bot_reply(con, fx$model, "chatGate",
    sprintf("How is patient %s trending?", pid), llm_mock = TRUE)
  expect_true(.is_png_file(r3$photo))
  unlink(r3$photo)
})

test_that("cdt_user_exists is a read-only known/unknown check", {
  fx <- make_test_fixtures()
  con <- .seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  expect_true(cdt_user_exists(con, "dr_viz"))
  expect_true(cdt_user_exists(con, "  dr_viz  "))  # trimmed
  expect_false(cdt_user_exists(con, "ghost"))
  expect_false(cdt_user_exists(con, ""))
  expect_false(cdt_user_exists(con, NULL))
})

test_that("cdt_bot_parse_login extracts usernames from login phrasings", {
  expect_equal(cdt_bot_parse_login("login as dr_viz"), "dr_viz")
  expect_equal(cdt_bot_parse_login("/login dr_viz"), "dr_viz")
  expect_equal(cdt_bot_parse_login("i am dr_viz"), "dr_viz")
  expect_equal(cdt_bot_parse_login("sign in as dr_viz"), "dr_viz")
  expect_null(cdt_bot_parse_login("how is patient P004?"))
})
