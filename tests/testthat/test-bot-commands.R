# Step 5e: Telegram command surface (description + menu commands).

.cmd_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) {
    cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  }
  cdt_create_user(con, "dr_cmd", "pw12345")
  con
}

.cmd_login <- function(con, model, chat_id) {
  cdt_bot_handle_message(con, model, chat_id, "login as dr_cmd", llm_mock = TRUE)
}

.is_png_cmd <- function(p) {
  if (is.null(p) || !is.character(p) || !nzchar(p) || !file.exists(p)) {
    return(FALSE)
  }
  identical(as.integer(readBin(p, "raw", 8L)[1:4]), c(137L, 80L, 78L, 71L))
}

# --- description + menu -----------------------------------------------------

test_that("cdt_bot_commands lists the expected commands", {
  cmds <- cdt_bot_commands()
  expect_true(is.data.frame(cmds))
  expect_true(all(c("command", "description") %in% names(cmds)))
  expect_true(all(c(
    "start", "help", "risk", "history", "whatif",
    "triage", "drivers", "explain", "dashboard"
  ) %in% cmds$command))
  # Descriptions are non-empty and carry no leading slash in `command`.
  expect_true(all(nzchar(cmds$description)))
  expect_false(any(grepl("^/", cmds$command)))
})

test_that("description functions are warm/professional and non-empty", {
  expect_true(nzchar(cdt_bot_tagline()))
  d <- cdt_bot_description()
  expect_match(d, "decision-support", ignore.case = TRUE)
  expect_match(d, "synthetic", ignore.case = TRUE)
})

# --- open commands (no auth) ------------------------------------------------

test_that("/start, /help, /explain work without authentication", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()

  s <- cdt_bot_reply(con, fx$model, "cS", "/start", llm_mock = TRUE)
  expect_match(s$text, "login as")
  expect_null(s$photo)

  h <- cdt_bot_reply(con, fx$model, "cH", "/help", llm_mock = TRUE)
  expect_match(h$text, "/triage")
  expect_match(h$text, "/whatif")

  e <- cdt_bot_reply(con, fx$model, "cE", "/explain", llm_mock = TRUE)
  expect_match(e$text, "does NOT model")
})

# --- gated commands ---------------------------------------------------------

test_that("gated commands require login", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()

  r <- cdt_bot_reply(con, fx$model, "cG", "/triage", llm_mock = TRUE)
  expect_match(r$text, "identify yourself", ignore.case = TRUE)
})

test_that("/triage all ranks patients by 7-day risk", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  .cmd_login(con, fx$model, "cT")

  # "all" forces the classic absolute worklist (delta view is the new default).
  r <- cdt_bot_reply(con, fx$model, "cT", "/triage all 3", llm_mock = TRUE)
  expect_match(r$text, "Top 3 patients")
  # Three ranked lines.
  expect_equal(length(gregexpr("7d=", r$text)[[1]]), 3)
  expect_null(r$photo)

  # Ranking is monotonically non-increasing in 7-day risk.
  snap <- cdt_cohort_snapshot(con, fx$model)
  expect_true(all(diff(head(snap$p_7d, 5)) <= 1e-9))
})

test_that("/triage (default) shows the change view; no movement -> falls back", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  .cmd_login(con, fx$model, "cT")

  # No snapshot/alerts yet -> graceful fallback to the absolute worklist.
  r0 <- cdt_bot_reply(con, fx$model, "cT", "/triage", llm_mock = TRUE)
  expect_match(r0$text, "No new movement", ignore.case = TRUE)

  # Seed a lower "previous" snapshot so deltas fire, then re-run.
  snap <- cdt_cohort_snapshot(con, fx$model)
  snap$p_7d <- pmax(snap$p_7d - 0.25, 0)
  snap$tier_7d <- as.character(cdt_risk_tier(snap$p_7d))
  cdt_write_risk_snapshot(con, snap, as_of = "prev")
  cdt_compute_alerts(con, fx$model, as_of = "now")

  r1 <- cdt_bot_reply(con, fx$model, "cT", "/triage", llm_mock = TRUE)
  expect_match(r1$text, "changed since last snapshot", ignore.case = TRUE)
})

test_that("/risk returns numbers only for the focus/explicit patient", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cR")

  r <- cdt_bot_reply(con, fx$model, "cR", sprintf("/risk %s", pid), llm_mock = TRUE)
  expect_match(r$text, sprintf("%s fall risk", pid))
  expect_match(r$text, "24h=")
  expect_match(r$text, "7d=")
  expect_null(r$photo)
})

test_that("/history renders the functional-history chart", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cHi")

  r <- cdt_bot_reply(con, fx$model, "cHi", sprintf("/history %s", pid),
    llm_mock = TRUE)
  expect_true(nzchar(r$text))
  expect_true(.is_png_cmd(r$photo))
  unlink(r$photo)
})

test_that("/whatif routes through the what-if pipeline", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cW")

  r <- cdt_bot_reply(con, fx$model, "cW",
    sprintf("/whatif %s more activity 20%%", pid), llm_mock = TRUE)
  expect_true(nzchar(r$text))
})

test_that("/drivers lists model drivers for the patient", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cD")

  r <- cdt_bot_reply(con, fx$model, "cD", sprintf("/drivers %s", pid),
    llm_mock = TRUE)
  expect_match(r$text, "Top model drivers")
  expect_null(r$photo)
})

test_that("/dashboard returns a deep link with the patient", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cDash")

  r <- cdt_bot_reply(con, fx$model, "cDash", sprintf("/dashboard %s", pid),
    llm_mock = TRUE)
  expect_match(r$text, "http")
  expect_match(r$text, pid)
  expect_null(r$photo)
})

test_that("/dashboard needs no patient: returns the cohort dashboard link", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  .cmd_login(con, fx$model, "cDashCohort")

  # No patient in focus and none named -> cohort-level URL (no ?patient=).
  r <- cdt_bot_reply(con, fx$model, "cDashCohort", "/dashboard", llm_mock = TRUE)
  expect_match(r$text, "http")
  expect_false(grepl("[?]patient=", r$text))
  expect_null(r$photo)
})

test_that("/dashboard ignores sticky focus: needs the patient in-message", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cDashSticky")

  # Pin a patient in focus via a patient-scoped query...
  cdt_bot_reply(con, fx$model, "cDashSticky",
    sprintf("how is %s trending?", pid), llm_mock = TRUE)
  expect_equal(cdt_bot_focus("cDashSticky"), pid)

  # ...a bare /dashboard must still return the OVERALL (cohort) link, not P###'s.
  r <- cdt_bot_reply(con, fx$model, "cDashSticky", "/dashboard", llm_mock = TRUE)
  expect_match(r$text, "http")
  expect_false(grepl("[?]patient=", r$text))
  expect_false(grepl(pid, r$text, fixed = TRUE))
})

# --- /panel: cohort fall-risk chart ----------------------------------------

test_that("/panel returns a cohort risk chart (PNG) after login", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  .cmd_login(con, fx$model, "cPanel")

  r <- cdt_bot_reply(con, fx$model, "cPanel", "/panel", llm_mock = TRUE)
  expect_true(.is_png_cmd(r$photo))
  # Caption summarizes the cohort and stays honest about synthetic data.
  expect_match(r$text, "Cohort fall-risk panel")
  expect_match(r$text, "Synthetic data")
  unlink(r$photo)
})

test_that("/panel is gated behind login like other cohort commands", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()

  r <- cdt_bot_reply(con, fx$model, "cPanelGate", "/panel", llm_mock = TRUE)
  expect_match(r$text, "identify yourself", ignore.case = TRUE)
  expect_null(r$photo)
})

test_that("free-text 'list all patients' returns the coded patient IDs", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  .cmd_login(con, fx$model, "cList")

  r <- cdt_bot_reply(con, fx$model, "cList", "list all patients",
    llm_mock = TRUE)
  # Every cohort id appears; the human names never do (coded IDs only).
  for (pid in fx$cohort$patient_id) expect_match(r$text, pid, fixed = TRUE)
  for (nm in fx$cohort$name) {
    if (nzchar(nm)) expect_false(grepl(nm, r$text, fixed = TRUE))
  }
  expect_null(r$photo)
})

# --- deterministic cohort-intent recognizers -------------------------------

test_that("cdt_bot_wants_panel matches cohort phrasing, not patient queries", {
  expect_true(cdt_bot_wants_panel("/panel"))
  expect_true(cdt_bot_wants_panel("show me the panel"))
  expect_true(cdt_bot_wants_panel("cohort risk overview chart"))
  # A patient-scoped query must fall through to the single-patient pipeline.
  expect_false(cdt_bot_wants_panel("how is patient P042 trending?"))
  expect_false(cdt_bot_wants_panel("steps for P004"))
})

test_that("cdt_bot_wants_patient_list matches roster phrasing, not patient queries", {
  expect_true(cdt_bot_wants_patient_list("list all patients"))
  expect_true(cdt_bot_wants_patient_list("which patients do we have?"))
  expect_true(cdt_bot_wants_patient_list("show the patient roster"))
  expect_false(cdt_bot_wants_patient_list("how is patient P042 doing?"))
})

# --- deterministic patient-data lookup -------------------------------------

test_that(".cdt_patient_data_field recognizes datum lookups, not chart/what-if", {
  expect_equal(.cdt_patient_data_field("which medication takes P041?"),
    "medications")
  expect_equal(.cdt_patient_data_field("list P041 meds"), "medications")
  expect_equal(.cdt_patient_data_field("what comorbidities does P041 have?"),
    "comorbidities")
  expect_equal(.cdt_patient_data_field("how old is P041?"), "age")
  expect_equal(.cdt_patient_data_field("does P041 have parkinson's?"),
    "parkinsons")
  expect_equal(.cdt_patient_data_field("show patient details"), "profile")
  # Not a datum lookup: plots, trends, generic questions -> NULL.
  expect_null(.cdt_patient_data_field("how is P041 trending?"))
  expect_null(.cdt_patient_data_field("plot P041 steps over time"))
})

test_that("patient-data lookup answers deterministically with the coded id only", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  nm <- fx$cohort$name[fx$cohort$patient_id == pid]
  .cmd_login(con, fx$model, "cData")

  r <- cdt_bot_reply(con, fx$model, "cData",
    sprintf("which medication takes %s?", pid), llm_mock = TRUE)
  # Deterministic template -> no chart, coded id present, synthetic name absent.
  expect_null(r$photo)
  expect_match(r$text, pid, fixed = TRUE)
  expect_match(r$text, "taking", ignore.case = TRUE)
  if (nzchar(nm)) expect_false(grepl(nm, r$text, fixed = TRUE))
})

test_that("a what-if for the same patient still routes to the model, not a datum", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  cdt_bot_reset()
  pid <- fx$cohort$patient_id[1]
  .cmd_login(con, fx$model, "cDataWi")

  # "increase" is what-if language: the datum lookup must NOT intercept it.
  r <- cdt_bot_reply(con, fx$model, "cDataWi",
    sprintf("what if we increase %s mobility by 20%%?", pid), llm_mock = TRUE)
  expect_true(nchar(r$text) > 0)
})

# --- panel renderer (direct) -----------------------------------------------

test_that("cdt_bot_plot_panel renders a PNG and NULLs an empty cohort", {
  fx <- make_test_fixtures()
  con <- .cmd_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  snap <- cdt_cohort_snapshot(con, fx$model)

  p <- cdt_bot_plot_panel(snap)
  expect_true(.is_png_cmd(p))
  unlink(p)

  # Empty snapshot -> NULL (caller falls back to text).
  expect_null(cdt_bot_plot_panel(snap[0, ]))
})
