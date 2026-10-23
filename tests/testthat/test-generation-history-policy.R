history_policy_session <- function() structure(list(protocol = "openai_compat", backend = "unsloth",
  chat_model = "offline", supports_structured_output = FALSE), class = "semantica_session")

history_policy_factors <- function() list(A = list(description = "Offline",
  facets = list(First = list(description = "First"), Second = list(description = "Second"))))

test_that("history-free initial and retry requests still reject local exact and near copies", {
  a <- "Cuando veo a un amigo llorar por algo que le paso, me conmueve tanto que no puedo dejar de reaccionar."
  near_a <- sub("por algo que", "por lo que", a, fixed = TRUE)
  b <- "I explore unfamiliar scientific questions with curiosity."
  c <- "I carefully organize my workspace before starting each task so I can find every tool when I need it."
  near_c <- sub("every tool", "each tool", c, fixed = TRUE)
  d <- "I enjoy painting quiet landscapes in the afternoon."
  expect_gte(SEMANTICA:::.semantica_lexical_jaccard(a, near_a), .92)
  expect_gte(SEMANTICA:::.semantica_lexical_jaccard(c, near_c), .92)
  responses <- list(a, c(a, b), c(near_a, c), c(near_c, d))
  prompts <- character()
  local_mocked_bindings(.call_chat = function(session, messages, ...) {
    prompts <<- c(prompts, messages[[1L]]$content)
    response <- responses[[length(prompts)]]
    paste(seq_along(response), response, sep = ". ", collapse = "\n")
  }, .package = "SEMANTICA")
  x <- semantica_generate_items(history_policy_session(), "Offline", "Offline", history_policy_factors(),
    n_per_factor = 4L, overgenerate = 1, max_retries = 2L, verbose = FALSE,
    prompt_history = "omit", history_dedup = "lexical")
  expect_length(prompts, 4L)
  expect_false(any(grepl("<previous_items>", prompts, fixed = TRUE)))
  for (item in c(a, b, c, d)) expect_false(any(grepl(item, prompts, fixed = TRUE)))
  expect_equal(x$item_text, c(a, b, c, d))
  meta <- attr(x, "semantica_generation_metadata")
  expect_equal(vapply(meta$attempts, `[[`, integer(1L), "duplicate_history_exact"), c(0L, 1L, 0L, 0L))
  expect_equal(vapply(meta$attempts, `[[`, integer(1L), "duplicate_history_near"), c(0L, 0L, 1L, 1L))
  expect_equal(vapply(meta$attempts, `[[`, integer(1L), "prompt_history_items"), rep(0L, 4L))
  expect_equal(vapply(meta$attempts, `[[`, integer(1L), "local_previous_items"), c(0L, 0L, 2L, 2L))
  expect_identical(meta$prompt_policy, "history_free_v1")
  expect_identical(meta$history_dedup, "lexical")
})

test_that("policy changes alter the specification fingerprint with identical generated text", {
  local_mocked_bindings(.call_chat = function(...) "1. I explore unfamiliar scientific questions with curiosity.", .package = "SEMANTICA")
  run <- function(...) semantica_generate_items(history_policy_session(), "Offline", "Offline",
    list(A = list(description = "Offline")), n_per_factor = 1L, overgenerate = 1,
    max_retries = 1L, verbose = FALSE, ...)
  a <- attr(run(), "semantica_generation_metadata")
  b <- attr(run(prompt_history = "omit"), "semantica_generation_metadata")
  c <- attr(run(prompt_history = "omit", history_dedup = "lexical"), "semantica_generation_metadata")
  expect_identical(a$item_pool_fingerprint, c$item_pool_fingerprint)
  expect_length(unique(c(a$generation_spec_fingerprint, b$generation_spec_fingerprint, c$generation_spec_fingerprint)), 3L)
  expect_identical(a$generation_contract_fingerprint, c$generation_contract_fingerprint)
  expect_identical(c$generation_spec_fingerprint_schema, "semantica-generation-spec-md5-v3")
  expect_error(run(prompt_history = "typo"), "arg")
  expect_error(semantica_generation_config(history_dedup = "typo"), "arg")
})

test_that("native shortages retain the preceding facet and the full attempt ledger", {
  calls <- 0L
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    "1. I explore unfamiliar scientific questions with curiosity."
  }, .package = "SEMANTICA")
  error <- tryCatch(semantica_generate_items(history_policy_session(), "Offline", "Offline",
    history_policy_factors(), n_per_factor = 2L, overgenerate = 1, max_retries = 2L,
    prompt_history = "omit", history_dedup = "lexical", verbose = FALSE), error = identity)
  expect_s3_class(error, "semantica_error_generation_incomplete")
  expect_equal(calls, 3L)
  f <- error$generation_failure
  expect_length(f$all_attempts, 3L)
  expect_length(f$attempts, 2L)
  expect_equal(f$completed_items$Facet, "First")
  expect_equal(f$all_attempts[[2L]]$local_previous_items, 1L)
  expect_identical(f$prompt_history, "omit")
})

test_that("the public generation configuration reaches the execution boundary", {
  seen <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    seen <<- list(...)
    stop("offline execution boundary reached")
  }, .package = "SEMANTICA")
  expect_error(semantica_run_custom("Offline", "Offline", list(A = list(description = "Offline")),
    generation = semantica_generation_config(prompt_history = "omit", history_dedup = "lexical"),
    plots = "none", verbose = FALSE), "offline execution boundary reached")
  expect_identical(seen$prompt_history, "omit")
  expect_identical(seen$history_dedup, "lexical")
})

test_that("the real facade and pipeline forward history policy to native generation", {
  seen <- NULL
  local_mocked_bindings(semantica_connect = function(...) history_policy_session(),
    semantica_generate_items = function(...) {
      seen <<- list(...)
      stop("offline native generation boundary reached")
    }, .package = "SEMANTICA")
  expect_error(semantica_run_custom("Offline", "Offline", history_policy_factors(),
    llm = semantica_llm_config(backend = "ollama", embed_backend = "ollama", preflight = FALSE),
    chat_model = "offline-chat", embed_model = "offline-embed",
    generation = semantica_generation_config(prompt_history = "omit", history_dedup = "lexical", pool_topup = FALSE),
    checkpoint_dir = tempfile("history-pipeline-"), plots = "none", verbose = FALSE),
    "offline native generation boundary reached")
  expect_identical(seen$prompt_history, "omit")
  expect_identical(seen$history_dedup, "lexical")
})
