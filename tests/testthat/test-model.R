fx <- make_test_fixtures()

test_that("model fits and returns the expected object", {
  expect_s3_class(fx$model, "cdt_model")
  expect_equal(length(fx$model$features), length(cdt_model_features()))
  expect_true(fx$model$n_train > 0)
})

test_that("there is a SINGLE pooled model with a horizon indicator", {
  # One fitted part (not model_24h / model_7d), and the design carries the
  # pooled horizon feature so one object serves both horizons.
  expect_null(fx$model$model_24h)
  expect_null(fx$model$model_7d)
  expect_false(is.null(fx$model$model$coef))
  expect_true("horizon_7d" %in% fx$model$design)
  expect_true("horizon_7d" %in% names(fx$model$model$coef))
  # Pooled rows = two per training snapshot (24h + 7d stacked).
  expect_equal(fx$model$n_pooled, 2L * fx$model$n_train)
})

test_that("the horizon feature drives 7d risk >= 24h risk on average", {
  # The 7-day window subsumes the 24h window, so pooled-logistic risk at the
  # 7d horizon should be at least the 24h risk for essentially every patient.
  pids <- fx$cohort$patient_id
  gaps <- vapply(pids, function(p) {
    fr <- cdt_assemble_features(fx$cohort[fx$cohort$patient_id == p, ],
      fx$sim$readings[fx$sim$readings$patient_id == p, ])
    r <- predict_fall_risk(fx$model, fr)
    r$p_7d - r$p_24h
  }, numeric(1))
  expect_gt(mean(gaps >= -1e-9), 0.95)
})

test_that("predict_fall_risk returns valid probabilities and tiers", {
  fr <- cdt_assemble_features(fx$cohort[1, ],
    fx$sim$readings[fx$sim$readings$patient_id == fx$cohort$patient_id[1], ])
  r <- predict_fall_risk(fx$model, fr)
  expect_true(r$p_24h >= 0 && r$p_24h <= 1)
  expect_true(r$p_7d >= 0 && r$p_7d <= 1)
  expect_true(r$tier_7d %in% c("Low", "Moderate", "High"))
})

test_that("counterfactual: more steps + less sedentary lowers risk", {
  # Pick the highest-risk patient so there is room to move.
  pids <- fx$cohort$patient_id
  feats <- lapply(pids, function(p) {
    cdt_assemble_features(fx$cohort[fx$cohort$patient_id == p, ],
      fx$sim$readings[fx$sim$readings$patient_id == p, ])
  })
  risks <- vapply(feats, function(f) predict_fall_risk(fx$model, f)$p_7d, numeric(1))
  fr <- feats[[which.max(risks)]]

  r <- predict_fall_risk(fx$model, fr,
    modified_inputs = list(steps_pct = 40, sedentary_hours_mean_7d = 10),
    include_baseline = TRUE)
  expect_lt(r$p_7d, r$baseline$p_7d)
  expect_lt(r$delta$p_7d, 0)
})

test_that("counterfactual: worsening trends raises risk", {
  fr <- cdt_assemble_features(fx$cohort[1, ],
    fx$sim$readings[fx$sim$readings$patient_id == fx$cohort$patient_id[1], ])
  r <- predict_fall_risk(fx$model, fr,
    modified_inputs = list(steps_trend_7d = -200, resting_hr_trend_7d = 1.5),
    include_baseline = TRUE)
  expect_gt(r$p_7d, r$baseline$p_7d)
})

test_that("overrides with NULL are a no-op", {
  fr <- cdt_assemble_features(fx$cohort[2, ],
    fx$sim$readings[fx$sim$readings$patient_id == fx$cohort$patient_id[2], ])
  expect_identical(cdt_apply_overrides(fr, NULL), fr)
})

test_that("model round-trips through save/load", {
  tmp <- tempfile(fileext = ".rds")
  cdt_save_model(fx$model, tmp)
  loaded <- cdt_load_model(tmp)
  fr <- cdt_assemble_features(fx$cohort[1, ],
    fx$sim$readings[fx$sim$readings$patient_id == fx$cohort$patient_id[1], ])
  expect_equal(
    predict_fall_risk(fx$model, fr)$p_7d,
    predict_fall_risk(loaded, fr)$p_7d
  )
  unlink(tmp)
})

test_that("feature importance is sorted by absolute influence", {
  imp <- cdt_feature_importance(fx$model, "7d")
  expect_true(all(diff(imp$abs_coefficient) <= 1e-9))
})

test_that("risk tiers map probabilities correctly", {
  expect_equal(as.character(cdt_risk_tier(c(0.05, 0.20, 0.50))),
    c("Low", "Moderate", "High"))
})
