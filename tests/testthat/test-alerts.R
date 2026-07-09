# P0-1: shift-triage change detection.
# Covers schema, snapshot round-trip, alert firing on risk jumps and tier
# crossings, the acknowledge path, and the "no movement -> no alert" case.
# Uses a throwaway SQLite file seeded from the shared fixtures.

fx <- make_test_fixtures()

.alerts_seeded_con <- function(fx) {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  cdt_db_init_schema(con)
  cdt_db_write(con, "patients", fx$cohort, append = TRUE)
  cdt_db_write(con, "sensor_readings", fx$sim$readings, append = TRUE)
  if (nrow(fx$sim$falls) > 0) {
    cdt_db_write(con, "fall_events", fx$sim$falls, append = TRUE)
  }
  con
}

test_that("schema creates risk_snapshots and alerts tables", {
  con <- cdt_db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  cdt_db_init_schema(con)
  tbls <- DBI::dbListTables(con)
  expect_true(all(c("risk_snapshots", "alerts") %in% tbls))
  expect_true(all(c("patient_id", "as_of", "p_24h", "p_7d", "tier_7d") %in%
    DBI::dbListFields(con, "risk_snapshots")))
  expect_true(all(c("patient_id", "kind", "severity", "delta_pts",
    "reason_text", "acknowledged_by", "acknowledged_at") %in%
    DBI::dbListFields(con, "alerts")))
})

test_that("cdt_alert_config exposes sane thresholds", {
  cfg <- cdt_alert_config()
  expect_true(is.numeric(cfg$jump_pts) && cfg$jump_pts > 0)
  expect_true(cfg$critical_pts >= cfg$warning_pts)
  expect_true(cfg$tier_up_severity %in% c("info", "warning", "critical"))
})

test_that("snapshot round-trips and cdt_get_last_snapshot returns the newest", {
  con <- .alerts_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  snap <- cdt_cohort_snapshot(con, fx$model)

  cdt_write_risk_snapshot(con, snap, as_of = "older")
  # A later snapshot with a distinct marker value.
  snap2 <- snap
  snap2$p_7d <- pmin(snap2$p_7d + 0.01, 1)
  cdt_write_risk_snapshot(con, snap2, as_of = "newest")

  last <- cdt_get_last_snapshot(con)
  expect_true(all(last$as_of == "newest"))
  expect_equal(nrow(last), nrow(snap))
})

test_that("no prior snapshot -> no alerts, but snapshot is written", {
  con <- .alerts_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  fired <- cdt_compute_alerts(con, fx$model, as_of = "first")
  expect_equal(nrow(fired), 0L)
  # The current risk was persisted for next time.
  expect_gt(nrow(cdt_get_last_snapshot(con)), 0L)
})

test_that("a downward-perturbed prior snapshot fires risk-jump/tier-up alerts", {
  con <- .alerts_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  snap <- cdt_cohort_snapshot(con, fx$model)
  # Pretend yesterday everyone was 25 pts lower -> today shows big jumps.
  prev <- snap
  prev$p_7d <- pmax(prev$p_7d - 0.25, 0)
  prev$tier_7d <- as.character(cdt_risk_tier(prev$p_7d))
  cdt_write_risk_snapshot(con, prev, as_of = "yesterday")

  fired <- cdt_compute_alerts(con, fx$model, as_of = "today")
  expect_gt(nrow(fired), 0L)
  expect_true(all(fired$kind %in% c("risk_jump", "tier_up")))
  expect_true(all(fired$severity %in% c("info", "warning", "critical")))
  expect_true(all(fired$delta_pts >= 0))
  # Reason text is a non-empty one-liner.
  expect_true(all(nzchar(fired$reason_text)))
})

test_that("stable risk between snapshots fires no alerts", {
  con <- .alerts_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  snap <- cdt_cohort_snapshot(con, fx$model)
  cdt_write_risk_snapshot(con, snap, as_of = "t0")  # identical to current
  fired <- cdt_compute_alerts(con, fx$model, as_of = "t1")
  expect_equal(nrow(fired), 0L)
})

test_that("acknowledge removes an alert from the open list", {
  con <- .alerts_seeded_con(fx)
  on.exit(DBI::dbDisconnect(con))
  snap <- cdt_cohort_snapshot(con, fx$model)
  prev <- snap
  prev$p_7d <- pmax(prev$p_7d - 0.25, 0)
  prev$tier_7d <- as.character(cdt_risk_tier(prev$p_7d))
  cdt_write_risk_snapshot(con, prev, as_of = "yesterday")
  fired <- cdt_compute_alerts(con, fx$model, as_of = "today")
  expect_gt(nrow(fired), 0L)

  open_before <- cdt_get_alerts(con, only_open = TRUE)
  n_open <- nrow(open_before)
  expect_gt(n_open, 0L)

  cdt_ack_alert(con, open_before$alert_id[1], acknowledged_by = "nurse1")
  open_after <- cdt_get_alerts(con, only_open = TRUE)
  expect_equal(nrow(open_after), n_open - 1L)

  # The acknowledged row records the actor + timestamp.
  all_alerts <- cdt_get_alerts(con)
  acked <- all_alerts[all_alerts$alert_id == open_before$alert_id[1], ]
  expect_identical(acked$acknowledged_by[1], "nurse1")
  expect_true(!is.na(acked$acknowledged_at[1]))
})
