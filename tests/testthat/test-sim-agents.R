# Phase 4 - agent LLM path + mock mode. Runs entirely in mock mode: mock emits
# real JSON so it exercises the same extract/validate path as a live reply.

.old_mock_env <- Sys.getenv("CDT_MOCK_LLM", unset = NA)
Sys.setenv(CDT_MOCK_LLM = "1")

.pt <- function(id) {
  p <- cdt_sim_patients()
  p[p$patient_id == id, , drop = FALSE]
}

# Temporarily override a global-env function for a block, restoring it after.
# The test harness sources R/ into globalenv(), so we patch there.
.with_global_override <- function(name, fn, expr) {
  had <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(name, envir = globalenv()) else NULL
  assign(name, fn, envir = globalenv())
  on.exit({
    if (had) {
      assign(name, old, envir = globalenv())
    } else {
      rm(list = name, envir = globalenv())
    }
  }, add = TRUE)
  force(expr)
}

test_that("the agent prompt embeds the persona and the JSON schema", {
  prompt <- cdt_agent_prompt(.pt("P08"), day = 3, ctx = list(weekend = FALSE))
  expect_true(grepl("Joaquin", prompt))
  expect_true(grepl("mobility_pct_of_baseline", prompt))
  expect_true(grepl("simulation day 3", prompt))
})

test_that("a mock agent call returns a valid 9-key decision", {
  res <- cdt_call_agent(.pt("P08"), day = 3, ctx = list(), mock = TRUE)
  expect_false(res$invalid)
  expect_identical(validate_agent_json(res$decision)$status, "pass")
  expect_true(all(.cdt_agent_required_keys() %in% names(res$decision)))
  expect_identical(res$decision$patient_id, "P08")
})

test_that("the mock decision is reproducible for the same (patient, day)", {
  a <- cdt_call_agent(.pt("P08"), day = 5, mock = TRUE)
  b <- cdt_call_agent(.pt("P08"), day = 5, mock = TRUE)
  expect_equal(a$decision$mobility_pct_of_baseline,
    b$decision$mobility_pct_of_baseline)
})

test_that("the returned prompt is stored for the audit trail", {
  res <- cdt_call_agent(.pt("P02"), day = 1, mock = TRUE)
  expect_true(is.character(res$prompt) && nzchar(res$prompt))
  expect_true(grepl("Antonio", res$prompt))
})

test_that("an unparseable reply reuses the prior decision and flags invalid", {
  prior <- list(
    patient_id = "P03", day = 4L, mobility_pct_of_baseline = 0.8,
    participated_group_activity = 0L, medication_adherence = 1L,
    meaningful_social_interaction = 1L, mood_fatigue = "tired",
    notable_event = NA_character_, confidence = 0.6
  )
  # Force the live path with a stub that always returns junk (no JSON).
  .with_global_override("cdt_llm_is_mock", function(mock = NULL) FALSE,
    .with_global_override("cdt_claude_reply",
      function(...) "sorry, I cannot comply", {
        res <- cdt_call_agent(.pt("P03"), day = 5, prior_decision = prior,
          mock = FALSE, temperature = 0.7)
        expect_true(res$invalid)
        # Reused prior decision, stamped with the new day.
        expect_identical(res$decision$mobility_pct_of_baseline, 0.8)
        expect_identical(res$decision$day, 5L)
      }))
})

test_that("invalid output with no prior falls back to a neutral decision", {
  .with_global_override("cdt_llm_is_mock", function(mock = NULL) FALSE,
    .with_global_override("cdt_claude_reply", function(...) "no json here", {
      res <- cdt_call_agent(.pt("P03"), day = 1, prior_decision = NULL,
        mock = FALSE)
      expect_true(res$invalid)
      expect_identical(validate_agent_json(res$decision)$status, "pass")
    }))
})

test_that("social day yields 2-4 valid interactions and P09 never initiates", {
  aff <- cdt_sim_affinity_matrix()
  for (d in 1:5) {
    soc <- cdt_sim_social_day(d, aff, cdt_sim_institution(), mock = TRUE,
      seed = 10 + d)
    expect_true(nrow(soc$rows) >= 2 && nrow(soc$rows) <= 4, info = paste("day", d))
    expect_identical(sum(soc$rows$initiated_by == "P09"), 0L, info = paste("day", d))
    res <- validate_social_interactions(soc$rows, rownames(aff))
    expect_false(res$status == "fail", info = paste("day", d))
  }
})

test_that("cdt_claude_reply accepts a temperature arg in mock mode (no error)", {
  out <- cdt_claude_reply("hi", context = "fall risk: 24h=5% (Low)",
    mock = TRUE, temperature = 0.2)
  expect_true(is.character(out) && nzchar(out))
})

# Restore the environment variable to its pre-test value.
if (is.na(.old_mock_env)) {
  Sys.unsetenv("CDT_MOCK_LLM")
} else {
  Sys.setenv(CDT_MOCK_LLM = .old_mock_env)
}
