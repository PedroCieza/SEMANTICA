test_that("reasoning-model profiles separate generic detection from provider-specific fields", {
  groq <- list(backend = "groq", chat_model = "openai/gpt-oss-120b")
  generic <- list(backend = "generic_openai", chat_model = "openai/gpt-oss-120b")

  pg <- SEMANTICA:::.semantica_reasoning_model_profile(groq)
  pu <- SEMANTICA:::.semantica_reasoning_model_profile(generic)

  expect_true(pg$reasoning_model)
  expect_true(pu$reasoning_model)
  expect_true(pg$provider_reasoning_fields)
  expect_false(pu$provider_reasoning_fields)
  expect_identical(pg$completion_field, "max_completion_tokens")
  expect_identical(pu$completion_field, "max_tokens")
  expect_identical(pg$reasoning_effort, "low")
  expect_null(pu$reasoning_effort)
})

test_that("Groq GPT-OSS item requests use low reasoning and strict structured output when requested", {
  session <- structure(list(
    backend = "groq", protocol = "openai_compat",
    chat_model = "openai/gpt-oss-120b", supports_structured_output = TRUE
  ), class = c("semantica_session", "list"))

  schema <- SEMANTICA:::.semantica_items_response_schema()
  body <- SEMANTICA:::.semantica_openai_chat_body(
    session,
    messages = list(list(role = "user", content = "Generate one item.")),
    max_tokens = 256L,
    response_format = "json"
  )

  expect_identical(body$reasoning_effort, "low")
  expect_false(body$include_reasoning)
  expect_identical(body$max_completion_tokens, 256L)
  expect_identical(body$response_format, schema)
  expect_null(body$max_tokens)
})

test_that("unknown reasoning servers receive generic recovery without unsupported request fields", {
  session <- structure(list(
    backend = "generic_openai", protocol = "openai_compat",
    chat_model = "openai/gpt-oss-120b", supports_structured_output = TRUE
  ), class = c("semantica_session", "list"))

  body <- SEMANTICA:::.semantica_openai_chat_body(
    session,
    messages = list(list(role = "user", content = "Generate one item.")),
    max_tokens = 256L,
    response_format = "json"
  )

  expect_identical(body$max_tokens, 256L)
  expect_null(body$max_completion_tokens)
  expect_null(body$reasoning_effort)
  expect_null(body$include_reasoning)
  expect_identical(body$response_format$type, "json_object")
})

test_that("reasoning budget expands only by observed or evidenced reasoning cost", {
  expect_identical(
    SEMANTICA:::.semantica_generation_visible_token_budget(256L),
    256L
  )
  expect_identical(
    SEMANTICA:::.semantica_generation_visible_token_budget(
      visible_tokens = 256L, reasoning_tokens = 300L, reasoning_model = TRUE
    ),
    556L
  )
  expect_identical(
    SEMANTICA:::.semantica_generation_visible_token_budget(
      visible_tokens = 256L, reasoning_model = TRUE
    ),
    256L
  )
})

test_that("runtime reasoning evidence upgrades unknown model detection without adding transport fields", {
  state <- SEMANTICA:::.semantica_new_rate_limit_state()
  state$observed_reasoning_tokens <- 300
  session <- structure(list(
    backend = "generic_openai", protocol = "openai_compat",
    chat_model = "future-model", supports_structured_output = FALSE,
    rate_limit_state = state
  ), class = c("semantica_session", "list"))

  profile <- SEMANTICA:::.semantica_reasoning_model_profile(session)
  expect_true(profile$reasoning_model)
  expect_identical(profile$completion_field, "max_tokens")
  expect_null(profile$reasoning_effort)
  expect_null(profile$include_reasoning)
  expect_identical(
    SEMANTICA:::.semantica_generation_visible_token_budget(
      visible_tokens = 256L, session = session
    ),
    556L
  )
})

test_that("rate-limit delay uses observed token headers rather than a fixed TPM guess", {
  state <- SEMANTICA:::.semantica_new_rate_limit_state()
  now <- as.numeric(Sys.time())
  state$remaining_tokens <- 100
  state$reset_tokens_s <- 12
  state$token_state_observed_at <- now
  session <- structure(list(
    backend = "generic_openai", chat_url = "https://models.example/v1/chat/completions",
    rate_limit_state = state
  ), class = c("semantica_session", "list"))

  expect_equal(
    SEMANTICA:::.semantica_rate_limit_delay_s(
      session, estimated_tokens = 500,
      now = as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
    ),
    12,
    tolerance = 0.2
  )
})

test_that("Ollama structured-output transport keeps its native format field", {
  session <- structure(list(
    backend = "ollama", protocol = "ollama", chat_model = "test-model",
    supports_structured_output = TRUE
  ), class = c("semantica_session", "list"))

  body <- SEMANTICA:::.semantica_openai_chat_body(
    session,
    messages = list(list(role = "user", content = "Generate one item.")),
    max_tokens = 256L,
    response_format = "json"
  )

  expect_identical(body$format, "json")
  expect_null(body$response_format)
  expect_identical(body$options$num_predict, 256L)
  expect_null(body$max_tokens)
})

test_that("generation retries reasoning-empty responses with observed reasoning reserve", {
  budgets <- integer(0L)

  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, ...) {
      budgets <<- c(budgets, as.integer(max_tokens))
      if (length(budgets) == 1L) {
        stop(SEMANTICA:::.semantica_condition(
          "reasoning budget exhausted",
          subclass = "semantica_error_reasoning_budget",
          fields = list(
            finish_reason = "length",
            reasoning_tokens = 300,
            max_completion_tokens = as.integer(max_tokens),
            completion_budget = as.integer(max_tokens)
          )
        ))
      }
      "1. I can stay focused on a demanding task."
    },
    .package = "SEMANTICA"
  )

  session <- structure(list(
    backend = "generic_openai", protocol = "openai_compat",
    chat_model = "future-model", supports_structured_output = FALSE
  ), class = c("semantica_session", "list"))

  items <- suppressWarnings(semantica_generate_items(
    session = session,
    scale_name = "Focus",
    scale_description = "Sustained attention.",
    factors = list(Focus = list(description = "Sustained attention.")),
    n_per_factor = 1L,
    overgenerate = 1,
    max_retries = 2L,
    structured_output = "numbered",
    verbose = FALSE
  ))

  expect_identical(budgets, c(256L, 556L))
  expect_equal(nrow(items), 1L)
  meta <- attr(items, "semantica_generation_metadata")
  expect_identical(meta$attempts[[1L]]$rejection_reasons, "reasoning_budget_exhausted")
  expect_identical(meta$attempts[[1L]]$completion_budget, 256L)
  expect_identical(meta$attempts[[2L]]$completion_budget, 556L)
})
