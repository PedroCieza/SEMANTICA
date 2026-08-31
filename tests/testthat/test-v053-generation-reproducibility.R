test_that("generation task seeds are deterministic without consuming caller RNG", {
  set.seed(8675309L)
  before <- .Random.seed

  a <- SEMANTICA:::.semantica_derive_generation_task_seed(
    20260826L, "Autonomy", "Autonomy", 1L, 12L
  )
  b <- SEMANTICA:::.semantica_derive_generation_task_seed(
    20260826L, "Autonomy", "Autonomy", 1L, 12L
  )
  c <- SEMANTICA:::.semantica_derive_generation_task_seed(
    20260826L, "Competence", "Competence", 1L, 12L
  )

  expect_identical(a, b)
  expect_true(is.integer(a) && a > 0L)
  expect_false(identical(a, c))
  expect_identical(.Random.seed, before)
})

test_that("seeded Ollama generation propagates deterministic seeds and provenance", {
  make_response <- function(seed) {
    suffix <- sprintf("seed%s", seed)
    paste(c(
      sprintf("1. I choose activities that reflect my values %s.", suffix),
      sprintf("2. My actions feel willingly chosen and self endorsed %s.", suffix),
      sprintf("3. I make decisions that feel authentic to me %s.", suffix),
      sprintf("4. I act from motives that I personally endorse %s.", suffix)
    ), collapse = "\n")
  }

  seen <- integer(0L)
  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, seed = NULL, ...) {
      seen <<- c(seen, as.integer(seed))
      make_response(seed)
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

  x <- run_once(20260826L)
  y <- run_once(20260826L)
  z <- run_once(20260827L)

  expect_identical(x$item_text, y$item_text)
  expect_false(identical(x$item_text, z$item_text))

  mx <- attr(x, "semantica_generation_metadata")
  my <- attr(y, "semantica_generation_metadata")
  mz <- attr(z, "semantica_generation_metadata")

  expect_identical(mx$schema, "semantica-generation-provenance-v1")
  expect_true(mx$generation_seed_controlled)
  expect_identical(mx$generation_seed_mechanism, "ollama_options_seed")
  expect_identical(mx$item_pool_fingerprint, my$item_pool_fingerprint)
  expect_false(identical(mx$item_pool_fingerprint, mz$item_pool_fingerprint))
  expect_identical(mx$task_seed_ledger$generation_task_seed,
                   my$task_seed_ledger$generation_task_seed)
  expect_true(nzchar(mx$generation_contract_fingerprint))
  expect_identical(mx$content_screening_status, "not_yet_performed")
  expect_true(length(seen) >= 3L)
})

test_that("unsupported generation protocols are never silently treated as seeded", {
  seen <- list()
  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, seed = NULL, ...) {
      seen[[length(seen) + 1L]] <<- seed
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
  expect_false(meta$generation_seed_supported)
  expect_false(meta$generation_seed_controlled)
  expect_identical(meta$generation_seed_guarantee, "not_controlled")
  expect_true(all(vapply(seen, is.null, logical(1L))))
  expect_true(all(is.na(meta$task_seed_ledger$generation_task_seed)))
})

test_that("semantic reduction summary makes relative separation explicit", {
  ids <- c("a1", "a2", "b1", "b2")
  m <- matrix(c(
    1, .80, .45, .40,
    .80, 1, .42, .38,
    .45, .42, 1, .82,
    .40, .38, .82, 1
  ), 4, 4, byrow = TRUE, dimnames = list(ids, ids))
  fa <- c(a1 = "A", a2 = "A", b1 = "B", b2 = "B")

  out <- SEMANTICA:::compute_semantic_similarity_reduction_summary(
    m,
    pool_items = ids,
    pool_factor_assignment = fa,
    selected_items = ids,
    selected_factor_assignment = fa,
    factors = c("A", "B"),
    within_similarity_target = c(A = .75, B = .75),
    within_similarity_band = .08
  )

  expect_true(is.finite(out$separation_gap_before))
  expect_true(is.finite(out$separation_gap_after))
  expect_identical(out$separation_gap_change, 0)
  expect_match(out$interpretation, "Relative within-versus-between semantic separation")
})

test_that("new generation controls preserve legacy positional API order", {
  gen_formals <- names(formals(SEMANTICA::semantica_generate_items))
  pipe_formals <- names(formals(SEMANTICA::semantica_pipeline))
  full_formals <- names(formals(SEMANTICA::semantica_full_pipeline_custom))

  expect_identical(tail(gen_formals, 2L), c("verbose", "seed"))
  expect_gt(match("generation_seed", pipe_formals), match("...", pipe_formals))
  expect_gt(match("generation_seed", full_formals), match("...", full_formals))
})

test_that("generation seed validation rejects ambiguous vectors and non-integers", {
  expect_error(
    SEMANTICA:::.semantica_normalize_generation_seed(c(1L, 2L)),
    "one nonnegative integer"
  )
  expect_error(
    SEMANTICA:::.semantica_normalize_generation_seed(1.5),
    "one nonnegative integer"
  )
  expect_identical(SEMANTICA:::.semantica_normalize_generation_seed("42"), 42L)
})
