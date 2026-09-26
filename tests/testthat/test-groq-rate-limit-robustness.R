test_that("Groq GPT-OSS structured generation uses the strict item schema", {
  session <- structure(
    list(
      backend = "groq",
      chat_model = "openai/gpt-oss-120b",
      supports_structured_output = TRUE
    ),
    class = c("semantica_session", "list")
  )
  fmt <- SEMANTICA:::.semantica_chat_response_format(session, "json")
  expect_identical(fmt$type, "json_schema")
  expect_true(isTRUE(fmt$json_schema$strict))
  expect_identical(fmt$json_schema$schema$type, "object")
  expect_identical(fmt$json_schema$schema$required, list("items"))
  expect_false(isTRUE(fmt$json_schema$schema$additionalProperties))
  expect_identical(fmt$json_schema$schema$properties$items$type, "array")
  expect_identical(fmt$json_schema$schema$properties$items$items$type, "string")
})

test_that("reasoning controls are capability-driven and unknown transports remain unchanged", {
  groq <- structure(list(backend = "groq", chat_model = "openai/gpt-oss-120b"), class = "list")
  p1 <- SEMANTICA:::.semantica_model_generation_profile(groq)
  expect_true(p1$reasoning_model)
  expect_identical(p1$reasoning_effort, "low")
  expect_false(p1$include_reasoning)
  expect_identical(p1$completion_field, "max_completion_tokens")

  generic <- structure(list(backend = "generic_openai", chat_model = "future-reasoning-model"), class = "list")
  p2 <- SEMANTICA:::.semantica_model_generation_profile(generic)
  expect_identical(p2$completion_field, "max_tokens")
  expect_null(p2$reasoning_effort)
  expect_null(p2$include_reasoning)
})

test_that("Groq rate budget planning uses provider state without inventing a token cutoff", {
  state <- SEMANTICA:::.semantica_new_rate_limit_state()
  now <- as.numeric(Sys.time())
  state$remaining_tokens <- 300
  state$reset_tokens_s <- 20
  state$token_state_observed_at <- now
  state$last_request_at <- now - 10
  session <- structure(
    list(
      backend = "groq", chat_model = "openai/gpt-oss-120b",
      chat_url = "https://api.groq.com/openai/v1/chat/completions",
      rate_limit_state = state
    ),
    class = c("semantica_session", "list")
  )

  plan <- SEMANTICA:::.semantica_rate_limit_plan(
    session, estimated_tokens = 500, now = as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  )
  expect_equal(plan$token_wait_s, 20, tolerance = 0.2)
  expect_true(plan$wait_s >= plan$token_wait_s)

  state$remaining_tokens <- 10000
  plan2 <- SEMANTICA:::.semantica_rate_limit_plan(
    session, estimated_tokens = 500, now = as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  )
  expect_equal(plan2$token_wait_s, 0)
})

test_that("successful zero-yield generation is recorded as zero rather than unknown", {
  plan <- SEMANTICA:::.semantica_generation_replenishment_plan(
    deficit = 8L,
    successful_requested = 14L,
    successful_new_retained = 0L,
    initial_request = 14L
  )
  expect_equal(plan$observed_yield, 0)
  expect_identical(plan$request_n, 8L)
})

test_that("failed generation diagnostics distinguish parser and backend failures", {
  txt <- SEMANTICA:::.semantica_generation_failure_summary(list(
    list(
      attempt = 1L, requested = 14L, parsed = 0L, retained = 0L,
      rejection_reasons = c("empty_response", "below_minimum_usable")
    ),
    list(
      attempt = 2L, requested = 8L, parsed = 0L, retained = 0L,
      rejection_reasons = "backend_error", backend_error = "HTTP 429"
    )
  ))
  expect_match(txt, "empty_response", fixed = TRUE)
  expect_match(txt, "backend_error", fixed = TRUE)
  expect_match(txt, "HTTP 429", fixed = TRUE)
})

test_that("session rate state is mutable internally but excluded from sanitized results", {
  session <- semantica_connect(
    backend = "groq",
    api_key = "test-key-not-real",
    preflight = FALSE,
    verbose = FALSE
  )
  expect_true(is.environment(session$rate_limit_state))
  session$rate_limit_state$remaining_tokens <- 123
  expect_equal(session$rate_limit_state$remaining_tokens, 123)

  sanitized <- SEMANTICA:::sanitize_session_for_result(session)
  expect_null(sanitized$rate_limit_state)
  expect_null(sanitized$api_key)
})
