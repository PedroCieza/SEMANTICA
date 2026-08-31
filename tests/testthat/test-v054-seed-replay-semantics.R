test_that("seed provenance separates replay plan from realized backend output", {
  response_counter <- 0L
  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, seed = NULL, ...) {
      response_counter <<- response_counter + 1L
      variant <- if (response_counter %% 2L) "alpha" else "beta"
      paste(c(
        sprintf("1. I choose actions that reflect my values %s.", variant),
        sprintf("2. My decisions feel personally endorsed %s.", variant),
        sprintf("3. I act in ways that feel authentic to me %s.", variant),
        sprintf("4. My actions feel willingly chosen %s.", variant)
      ), collapse = "\n")
    },
    .package = "SEMANTICA"
  )

  session <- structure(
    list(
      protocol = "ollama", backend = "ollama", chat_model = "test-model",
      supports_structured_output = FALSE
    ),
    class = c("semantica_session", "list")
  )
  factors <- list(Autonomy = list(description = "Volitional, self-endorsed action."))

  run_once <- function(seed) suppressWarnings(semantica_generate_items(
    session = session,
    scale_name = "Autonomy",
    scale_description = "General autonomy satisfaction.",
    factors = factors,
    n_per_factor = 4L,
    overgenerate = 1,
    max_retries = 1L,
    structured_output = "numbered",
    verbose = FALSE,
    seed = seed
  ))

  x <- run_once(42L)
  y <- run_once(42L)
  z <- run_once(43L)

  mx <- attr(x, "semantica_generation_metadata")
  my <- attr(y, "semantica_generation_metadata")
  mz <- attr(z, "semantica_generation_metadata")

  # The backend is deliberately nondeterministic here: exact text is allowed to
  # differ without turning the SEMANTICA seed-control plan into a failure.
  expect_false(identical(x$item_text, y$item_text))
  expect_false(identical(mx$item_pool_fingerprint, my$item_pool_fingerprint))

  expect_identical(mx$generation_seed_schedule, my$generation_seed_schedule)
  expect_identical(mx$generation_spec_fingerprint, my$generation_spec_fingerprint)
  expect_identical(mx$generation_replay_plan_fingerprint,
                   my$generation_replay_plan_fingerprint)

  # Changing the master seed leaves the generation specification unchanged but
  # must change the controlled replay plan.
  expect_identical(mx$generation_spec_fingerprint, mz$generation_spec_fingerprint)
  expect_false(identical(mx$generation_replay_plan_fingerprint,
                         mz$generation_replay_plan_fingerprint))

  expect_true(nzchar(mx$generation_contract_fingerprint))
  expect_true(nzchar(mx$generation_spec_fingerprint))
  expect_false(mx$exact_text_replay_guaranteed)
  expect_match(mx$generation_replay_note, "Exact downstream replay")

  expect_true("prompt_fingerprint" %in% names(mx$task_seed_ledger))
  expect_false("prompt_fingerprint" %in% names(mx$generation_seed_schedule))
  expect_identical(
    mx$task_seed_ledger$generation_task_seed,
    mx$generation_seed_schedule$generation_task_seed
  )
})

test_that("unsupported generation seeds do not receive a replay-plan fingerprint", {
  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, seed = NULL, ...) {
      expect_null(seed)
      paste(c(
        "1. I choose actions that reflect my values.",
        "2. My decisions feel personally meaningful.",
        "3. I act in ways that feel authentic to me.",
        "4. I willingly endorse the actions I take."
      ), collapse = "\n")
    },
    .package = "SEMANTICA"
  )

  session <- structure(
    list(
      protocol = "openai_compat", backend = "custom", chat_model = "test-model",
      supports_structured_output = FALSE
    ),
    class = c("semantica_session", "list")
  )

  out <- semantica_generate_items(
    session = session,
    scale_name = "Autonomy",
    scale_description = "General autonomy satisfaction.",
    factors = list(Autonomy = list(description = "Volitional action.")),
    n_per_factor = 4L,
    overgenerate = 1,
    max_retries = 1L,
    structured_output = "numbered",
    verbose = FALSE,
    seed = 42L
  )

  meta <- attr(out, "semantica_generation_metadata")
  expect_false(meta$generation_seed_controlled)
  expect_true(is.na(meta$generation_replay_plan_fingerprint))
  expect_false(meta$exact_text_replay_guaranteed)
})
