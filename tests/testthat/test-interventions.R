fx <- make_test_fixtures()

test_that("every model feature has an intervention-map entry", {
  map <- cdt_interventions_map()
  feats <- cdt_model_features()
  missing <- setdiff(feats, names(map))
  expect_identical(missing, character(0))
})

test_that("each map entry is well-formed (label, interventions, urgency, note)", {
  map <- cdt_interventions_map()
  for (nm in names(map)) {
    entry <- map[[nm]]
    expect_true(is.character(entry$label) && nzchar(entry$label), info = nm)
    expect_true(length(entry$interventions) >= 1, info = nm)
    expect_true(all(nzchar(entry$interventions)), info = nm)
    expect_true(entry$urgency %in% c("routine", "prompt", "urgent"), info = nm)
    expect_true(is.character(entry$evidence_note), info = nm)
  }
})

test_that("cdt_driver_interventions returns suggestions for the top drivers", {
  di <- cdt_driver_interventions(fx$model, top_n = 3L)
  expect_equal(nrow(di), 3L)
  expect_true(all(c("feature", "label", "coefficient", "direction",
    "urgency", "evidence_note", "interventions") %in% names(di)))
  # interventions is a list-column of non-empty character vectors.
  expect_true(is.list(di$interventions))
  expect_true(all(vapply(di$interventions, function(x) length(x) >= 1, logical(1))))
  # direction is derived from the coefficient sign.
  expect_true(all(di$direction %in% c("increases risk", "decreases risk")))
})

test_that("top drivers align with cdt_feature_importance ordering", {
  di <- cdt_driver_interventions(fx$model, top_n = 5L)
  imp <- utils::head(cdt_feature_importance(fx$model), 5)
  expect_identical(di$feature, imp$feature)
})
