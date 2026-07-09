# P0-3: interventions log (closed loop) DB layer.
# Verifies the schema, the write/read round-trip, created_by capture, ordering,
# and the data shape the dashboard overlay relies on. Uses a throwaway SQLite
# file per test so nothing touches the project DB.

fx <- make_test_fixtures()

.iv_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  con
}

test_that("schema creates the interventions table with expected columns", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  expect_true("interventions" %in% DBI::dbListTables(con))
  cols <- DBI::dbListFields(con, "interventions")
  expect_true(all(c("intervention_id", "patient_id", "type", "detail",
    "created_by", "created_at") %in% cols))
})

test_that("cdt_log_intervention inserts a row and returns its id", {
  con <- .iv_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  pid <- fx$cohort$patient_id[1]

  id1 <- cdt_log_intervention(con, pid, "Medication review",
    detail = "review benzodiazepine", created_by = "clinician")
  expect_true(is.integer(id1) && id1 >= 1L)

  iv <- cdt_get_interventions(con, pid)
  expect_equal(nrow(iv), 1L)
  expect_identical(iv$type[1], "Medication review")
  expect_identical(iv$detail[1], "review benzodiazepine")
  expect_identical(iv$created_by[1], "clinician")
  # created_at is auto-populated by the DB default.
  expect_true(is.character(iv$created_at) && nzchar(iv$created_at[1]))
})

test_that("NULL detail/created_by are stored as NA and read back as NA", {
  con <- .iv_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  pid <- fx$cohort$patient_id[1]
  cdt_log_intervention(con, pid, "Physiotherapy referral")
  iv <- cdt_get_interventions(con, pid)
  expect_equal(nrow(iv), 1L)
  expect_true(is.na(iv$detail[1]))
  expect_true(is.na(iv$created_by[1]))
})

test_that("cdt_get_interventions filters by patient and orders by created_at", {
  con <- .iv_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  p1 <- fx$cohort$patient_id[1]
  p2 <- fx$cohort$patient_id[2]

  # Explicit timestamps so ordering is deterministic.
  cdt_log_intervention(con, p1, "A", created_at = "2024-01-01 08:00:00")
  cdt_log_intervention(con, p1, "B", created_at = "2024-01-03 08:00:00")
  cdt_log_intervention(con, p1, "C", created_at = "2024-01-02 08:00:00")
  cdt_log_intervention(con, p2, "Other patient", created_at = "2024-01-01 08:00:00")

  iv1 <- cdt_get_interventions(con, p1)
  expect_equal(nrow(iv1), 3L)
  expect_true(all(iv1$patient_id == p1))
  # Ascending created_at order.
  expect_identical(iv1$type, c("A", "C", "B"))

  # NULL patient_id returns the whole log.
  all_iv <- cdt_get_interventions(con)
  expect_equal(nrow(all_iv), 4L)
})

test_that("logged rows carry the date substring used by the plot overlay", {
  con <- .iv_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  pid <- fx$cohort$patient_id[1]
  cdt_log_intervention(con, pid, "Toileting schedule",
    created_at = "2024-02-15 09:30:00")
  iv <- cdt_get_interventions(con, pid)
  # The dashboard derives a Date from the first 10 chars of created_at.
  d <- as.Date(substr(iv$created_at, 1, 10))
  expect_identical(d, as.Date("2024-02-15"))
})

test_that("cdt_log_intervention rejects empty patient_id or type", {
  con <- .iv_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  pid <- fx$cohort$patient_id[1]
  expect_error(cdt_log_intervention(con, "", "Something"))
  expect_error(cdt_log_intervention(con, pid, ""))
})
