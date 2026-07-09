# Coverage for the post-fall huddle module (P0-4). All offline: a temp SQLite DB
# seeded from the shared fixture. These tests assert (a) the DB open->closed
# transition + field round-trip, (b) the grounded context/mock draft are built
# from the windowed sensor facts, and (c) no patient NAME leaks into the draft.

# A temp DB seeded with the fixture cohort + readings + falls. Returns the con
# and the id of a fall event that has enough pre-fall history to summarise.
.huddle_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  con
}

# Pick a fall event id that sits far enough into the timeline to have a real
# pre-fall window (so the grounded facts are non-empty).
.huddle_pick_event <- function(con) {
  falls <- cdt_get_fall_events(con)
  # Falls at least ~7 days after each patient's first reading have a full window.
  falls$fdate <- as.Date(substr(falls$ts, 1, 10))
  falls <- falls[order(falls$fdate, decreasing = TRUE), , drop = FALSE]
  falls$event_id[[1]]
}

test_that("fall_events carries the huddle columns after schema init", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(fall_events);")$name
  expect_true(all(c(
    "location", "activity_at_fall", "injury_level", "contributing_factors",
    "plan", "huddle_summary", "huddle_completed_by", "huddle_completed_at"
  ) %in% cols))
})

test_that(".cdt_add_fall_huddle_columns is idempotent", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  before <- DBI::dbGetQuery(con, "PRAGMA table_info(fall_events);")$name
  # Re-running must not error or duplicate columns.
  expect_silent(.cdt_add_fall_huddle_columns(con))
  after <- DBI::dbGetQuery(con, "PRAGMA table_info(fall_events);")$name
  expect_identical(after, before)
})

test_that("open huddles list excludes completed ones (open->closed transition)", {
  fx <- make_test_fixtures()
  con <- .huddle_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  open_before <- cdt_get_open_huddles(con)
  expect_gt(nrow(open_before), 0)
  eid <- open_before$event_id[[1]]
  expect_true(eid %in% open_before$event_id)

  n <- cdt_complete_huddle(con, eid,
    fields = list(
      location = "Bathroom",
      injury_level = "None",
      huddle_summary = "Reviewed; low activity trend noted.",
      plan = "Toileting schedule + PT referral."
    ),
    completed_by = "clinician")
  expect_identical(as.integer(n), 1L)

  open_after <- cdt_get_open_huddles(con)
  expect_false(eid %in% open_after$event_id)
  expect_identical(nrow(open_before) - nrow(open_after), 1L)
})

test_that("cdt_complete_huddle round-trips fields and stamps completion", {
  fx <- make_test_fixtures()
  con <- .huddle_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  eid <- cdt_get_open_huddles(con)$event_id[[1]]
  cdt_complete_huddle(con, eid,
    fields = list(
      location = "Hallway",
      activity_at_fall = "Walking to dining room",
      injury_level = "Minor bruise",
      contributing_factors = "Orthostatic drop; new sedative",
      plan = "Med review",
      huddle_summary = "Fall while ambulating; BP review advised."
    ),
    completed_by = "nurse_a")

  row <- cdt_get_fall_event(con, eid)
  expect_identical(row$location[[1]], "Hallway")
  expect_identical(row$activity_at_fall[[1]], "Walking to dining room")
  expect_identical(row$injury_level[[1]], "Minor bruise")
  expect_identical(row$contributing_factors[[1]], "Orthostatic drop; new sedative")
  expect_identical(row$plan[[1]], "Med review")
  expect_identical(row$huddle_summary[[1]], "Fall while ambulating; BP review advised.")
  expect_identical(row$huddle_completed_by[[1]], "nurse_a")
  expect_true(nzchar(row$huddle_completed_at[[1]]))
})

test_that("cdt_huddle_context grounds on the windowed sensor facts (coded id only)", {
  fx <- make_test_fixtures()
  con <- .huddle_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  eid <- .huddle_pick_event(con)
  ctx <- cdt_huddle_context(con, fx$model, eid)
  expect_type(ctx, "list")
  expect_true(all(c("text", "facts") %in% names(ctx)))
  # Facts carry the coded patient id, the fall date and the pre-fall window.
  expect_true(nzchar(ctx$facts$patient_id))
  expect_true(nzchar(ctx$facts$fall_date))
  expect_gte(ctx$facts$n_pre_days, 0L)
  # The context text refers to the coded id, and never to a human name.
  expect_true(grepl(ctx$facts$patient_id, ctx$text, fixed = TRUE))
  nm <- cdt_get_patient(con, ctx$facts$patient_id)$name[[1]]
  expect_false(grepl(nm, ctx$text, fixed = TRUE))
})

test_that("mock huddle draft is deterministic, grounded and leaks no name", {
  fx <- make_test_fixtures()
  con <- .huddle_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  eid <- .huddle_pick_event(con)
  d1 <- cdt_draft_huddle_summary(con, fx$model, eid, mock = TRUE)
  d2 <- cdt_draft_huddle_summary(con, fx$model, eid, mock = TRUE)
  expect_type(d1, "character")
  expect_identical(d1, d2)                 # deterministic
  expect_true(grepl("MOCK HUDDLE DRAFT", d1))
  # Grounded: the coded id appears; the human name does not.
  pid <- cdt_get_fall_event(con, eid)$patient_id[[1]]
  expect_true(grepl(pid, d1, fixed = TRUE))
  nm <- cdt_get_patient(con, pid)$name[[1]]
  expect_false(grepl(nm, d1, fixed = TRUE))
})

test_that("cdt_draft_huddle_summary returns NULL for an unknown event", {
  fx <- make_test_fixtures()
  con <- .huddle_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_null(cdt_draft_huddle_summary(con, fx$model, 999999L, mock = TRUE))
})
