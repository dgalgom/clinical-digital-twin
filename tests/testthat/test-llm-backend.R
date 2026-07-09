# Coverage for the pluggable LLM backend selection, the mock-mode gate, and the
# Telegram "typing..." chat action. All offline: no network, no real API keys.
# These tests exercise the routing/config logic only -- the live Groq/Claude
# HTTP calls are never made here (mock mode short-circuits before any request).

# Run each test with a clean, restored set of the relevant env vars so one
# test's Sys.setenv() cannot leak into another.
.with_env <- function(vars, code) {
  keys <- names(vars)
  old <- Sys.getenv(keys, unset = NA, names = TRUE)
  # Apply: set non-NA, unset NA.
  for (k in keys) {
    v <- vars[[k]]
    if (is.na(v)) Sys.unsetenv(k) else do.call(Sys.setenv, stats::setNames(list(v), k))
  }
  on.exit({
    for (k in keys) {
      ov <- old[[k]]
      if (is.na(ov)) Sys.unsetenv(k) else do.call(Sys.setenv, stats::setNames(list(ov), k))
    }
  }, add = TRUE)
  force(code)
}

test_that("cdt_llm_backend honors CDT_LLM_BACKEND explicitly", {
  .with_env(list(CDT_LLM_BACKEND = "groq", GROQ_API_KEY = NA, ANTHROPIC_API_KEY = NA), {
    expect_identical(cdt_llm_backend(), "groq")
  })
  .with_env(list(CDT_LLM_BACKEND = "claude", GROQ_API_KEY = "gsk_x", ANTHROPIC_API_KEY = NA), {
    # Explicit choice wins even when a Groq key is present.
    expect_identical(cdt_llm_backend(), "claude")
  })
})

test_that("cdt_llm_backend opts into groq when only GROQ_API_KEY is present", {
  .with_env(list(CDT_LLM_BACKEND = "", GROQ_API_KEY = "gsk_x", ANTHROPIC_API_KEY = NA), {
    expect_identical(cdt_llm_backend(), "groq")
  })
  .with_env(list(CDT_LLM_BACKEND = "", GROQ_API_KEY = NA, ANTHROPIC_API_KEY = NA), {
    expect_identical(cdt_llm_backend(), "claude")
  })
})

test_that("model id helpers respect their env overrides", {
  .with_env(list(CDT_GROQ_MODEL = NA), {
    expect_identical(cdt_groq_model(), "llama-3.3-70b-versatile")
  })
  .with_env(list(CDT_GROQ_MODEL = "llama-3.1-8b-instant"), {
    expect_identical(cdt_groq_model(), "llama-3.1-8b-instant")
  })
  .with_env(list(CDT_CLAUDE_MODEL = NA), {
    expect_match(cdt_claude_model(), "^claude-")
  })
})

test_that("mock gate: explicit mock and CDT_MOCK_LLM=1 force mock regardless of keys", {
  .with_env(list(CDT_MOCK_LLM = "1", GROQ_API_KEY = "gsk_x", ANTHROPIC_API_KEY = "sk-x"), {
    expect_true(cdt_llm_is_mock())
  })
  expect_true(cdt_llm_is_mock(TRUE))
  # Explicit FALSE overrides even a mock env flag.
  .with_env(list(CDT_MOCK_LLM = "1"), {
    expect_false(cdt_llm_is_mock(FALSE))
  })
})

test_that("mock gate checks the SELECTED backend's key", {
  # Groq selected but no Groq key -> mock, even if an Anthropic key exists.
  .with_env(list(CDT_MOCK_LLM = NA, CDT_LLM_BACKEND = "groq",
                 GROQ_API_KEY = NA, ANTHROPIC_API_KEY = "sk-x"), {
    expect_true(cdt_llm_is_mock())
  })
  # Groq selected with a Groq key -> live (not mock).
  .with_env(list(CDT_MOCK_LLM = NA, CDT_LLM_BACKEND = "groq",
                 GROQ_API_KEY = "gsk_x", ANTHROPIC_API_KEY = NA), {
    expect_false(cdt_llm_is_mock())
  })
  # Claude selected but no Anthropic key -> mock, even with a Groq key present.
  .with_env(list(CDT_MOCK_LLM = NA, CDT_LLM_BACKEND = "claude",
                 GROQ_API_KEY = "gsk_x", ANTHROPIC_API_KEY = NA), {
    expect_true(cdt_llm_is_mock())
  })
})

test_that("cdt_claude_reply stays offline+deterministic in mock mode (any backend)", {
  ctx <- paste(
    "Patient P042, age 81, sex F.",
    "Baseline fall risk: 24h=6.0% (Low), 7d=22.0% (Moderate).",
    sep = "\n"
  )
  for (backend in c("claude", "groq")) {
    r <- cdt_claude_reply("How is patient P042 trending?", context = ctx,
      mock = TRUE)
    expect_type(r, "character")
    expect_true(nzchar(r))
    # Mock echoes the grounded risk line and never invents a name.
    expect_match(r, "22.0%")
    expect_false(grepl("[A-Z][a-z]+ [A-Z][a-z]+", r)) # no "Firstname Lastname"
  }
})

test_that("cdt_telegram_typing captures a typing action in mock mode", {
  invisible(cdt_telegram_sent(clear = TRUE))
  expect_true(cdt_telegram_typing(4242, mock = TRUE))
  sent <- cdt_telegram_sent(clear = TRUE)
  expect_length(sent, 1L)
  expect_identical(sent[[1]]$type, "action")
  expect_identical(sent[[1]]$action, "typing")
  expect_identical(sent[[1]]$chat_id, 4242)
})
