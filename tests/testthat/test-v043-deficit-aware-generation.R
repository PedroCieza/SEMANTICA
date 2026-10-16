test_that("cross-facet exact repeats are replenished within the existing attempt budget", {
  calls <- 0L
  responses <- c("1. I enjoy painting quiet landscapes.",
    "1. I ENJOY painting quiet landscapes!",
    "1. I explore unfamiliar scientific questions.")
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    responses[[calls]]
  }, .package = "SEMANTICA")
  items <- semantica_generate_items(
    structure(list(supports_structured_output = FALSE), class = "semantica_session"),
    "Offline", "Offline pool", factors = list(Openness = list(description = "Openness",
      facets = list(Art = list(description = "Art"), Curiosity = list(description = "Curiosity")))),
    n_per_factor = 2L, overgenerate = 1, max_retries = 2L,
    global_forbidden_max = 0L, structured_output = "numbered", verbose = FALSE)
  expect_equal(calls, 3L)
  expect_equal(nrow(items), 2L)
  expect_false(anyDuplicated(SEMANTICA:::.semantica_normalize_item_text(items$item)) > 0L)
  expect_equal(items$item, c("I enjoy painting quiet landscapes.", "I explore unfamiliar scientific questions."))
  attempts <- attr(items, "semantica_generation_metadata")$attempts
  expect_equal(attempts[[2L]]$duplicate_previous_units, 1L)
  expect_equal(attempts[[2L]]$newly_retained, 0L)
  expect_equal(attempts[[3L]]$needed_before, 1L)
})

test_that("persistent cross-facet repetition fails without extra whole-pool retries", {
  calls <- 0L
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    "1. I enjoy painting quiet landscapes."
  }, .package = "SEMANTICA")
  expect_error(semantica_generate_items(
    structure(list(supports_structured_output = FALSE), class = "semantica_session"),
    "Offline", "Offline pool", factors = list(Openness = list(description = "Openness",
      facets = list(Art = list(description = "Art"), Curiosity = list(description = "Curiosity")))),
    n_per_factor = 2L, overgenerate = 1, max_retries = 2L,
    structured_output = "numbered", verbose = FALSE), "generated only 0/1 usable items after 2 attempt")
  expect_equal(calls, 3L)
})

test_that("generation shortages preserve response evidence without extra attempts", {
  response <- paste(paste0(1:5, ". ", c(
    "I discuss personal troubles with a trusted friend.",
    "Comfort from my family helps me face difficult days.",
    "When overwhelmed I seek emotional support from others.",
    "Sharing my worries brings me relief.",
    "I need someone close to listen when I feel upset.")), collapse = "\n")
  calls <- 0L
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    response
  }, .package = "SEMANTICA")
  error <- tryCatch(semantica_generate_items(
    structure(list(supports_structured_output = FALSE), class = "semantica_session"),
    "Offline", "Offline pool", factors = list(Dependence = list(description = "Emotional support")),
    n_per_factor = 10L, overgenerate = 1.5, max_retries = 3L,
    structured_output = "numbered", verbose = FALSE), error = identity)
  expect_s3_class(error, "semantica_error_generation_incomplete")
  expect_equal(calls, 3L)
  evidence <- error$generation_failure
  expect_length(evidence$retained_items, 5L)
  expect_equal(vapply(evidence$attempts, `[[`, integer(1), "newly_retained"), c(5L, 0L, 0L))
  expect_equal(evidence$attempts[[2L]]$duplicate_current_unit, 5L)
  expect_equal(evidence$attempts[[2L]]$duplicate_previous_units, 0L)
  expect_identical(evidence$responses[[3L]]$raw_response, response)
  expect_match(evidence$responses[[2L]]$prompt, "Generate NEW items only", fixed = TRUE)
  expect_match(conditionMessage(error), "duplicate_current_unit=5", fixed = TRUE)
  expect_identical(SEMANTICA:::.semantica_failure_details(error)$generation_failure, evidence)
})

test_that("generation failure evidence reaches the native incomplete checkpoint", {
  evidence <- list(facet = "Dependence", responses = list(list(raw_response = "offline response")))
  local_mocked_bindings(semantica_pipeline = function(...) {
    SEMANTICA:::.semantica_abort("offline generation shortage",
      subclass = "semantica_error_generation_incomplete", generation_failure = evidence)
  }, .package = "SEMANTICA")
  path <- tempfile("generation-checkpoint-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  expect_error(semantica_run_custom(scale_name = "Offline", scale_description = "Offline",
    factors = list(A = list(description = "A"), B = list(description = "B")),
    llm = semantica_llm_config(backend = "ollama", embed_backend = "ollama"),
    chat_model = "offline", embed_model = "offline", checkpoint_dir = path,
    plots = "none", verbose = FALSE), class = "semantica_error_generation_incomplete")
  saved <- readRDS(file.path(path, "99_incomplete_run.rds"))
  expect_identical(saved$failure_details$generation_failure, evidence)
})

test_that("yield-adaptive replenishment scales to the remaining generation deficit", {
  plan <- SEMANTICA:::.semantica_generation_replenishment_plan(
    deficit = 4L,
    successful_requested = 40L,
    successful_new_retained = 36L,
    initial_request = 40L
  )

  expect_identical(plan$request_n, 5L)
  expect_equal(plan$observed_yield, 0.9)
  expect_identical(plan$policy, "yield_adaptive_v1")

  # No successful response yet: retry the unresolved deficit, not an arbitrary
  # larger batch. For a completely failed initial 40-item call this is 40.
  no_yield <- SEMANTICA:::.semantica_generation_replenishment_plan(
    deficit = 40L,
    successful_requested = 0L,
    successful_new_retained = 0L,
    initial_request = 40L
  )
  expect_identical(no_yield$request_n, 40L)
  expect_true(is.na(no_yield$observed_yield))

  # A completed backend response that retained nothing is an observed zero yield,
  # not missing information. This distinction drives clearer replenishment diagnostics.
  zero_yield <- SEMANTICA:::.semantica_generation_replenishment_plan(
    deficit = 8L,
    successful_requested = 14L,
    successful_new_retained = 0L,
    initial_request = 14L
  )
  expect_identical(zero_yield$request_n, 8L)
  expect_equal(zero_yield$observed_yield, 0)
})

test_that("partial generation is preserved and retries do not regenerate the full target", {
  requested <- integer(0L)
  call_number <- 0L

  extract_requested <- function(prompt) {
    hit <- regmatches(prompt, regexpr("Generate exactly [0-9]+ psychometric items", prompt))
    if (!length(hit) || !nzchar(hit)) {
      hit <- regmatches(prompt, regexpr("Output exactly [0-9]+ items", prompt))
    }
    as.integer(gsub("[^0-9]", "", hit))
  }

  local_mocked_bindings(
    .call_chat = function(session, messages, max_tokens, temperature,
                          system_prompt, response_format = NULL, ...) {
      call_number <<- call_number + 1L
      n <- extract_requested(messages[[1L]]$content)
      requested <<- c(requested, n)

      make_items <- function(indices) {
        tokens <- vapply(indices, function(i) {
          i0 <- i - 1L
          a <- letters[(i0 %/% 26L) + 1L]
          b <- letters[(i0 %% 26L) + 1L]
          paste(rep(c(a, b), 8L), collapse = "")
        }, character(1L))
        paste(sprintf("%d. I can handle challenge %s effectively.", seq_along(indices), tokens), collapse = "\n")
      }

      if (call_number == 1L) {
        return(make_items(1:36))
      }
      if (call_number == 2L) {
        stop("simulated backend timeout")
      }
      make_items(37:41)
    },
    .package = "SEMANTICA"
  )

  session <- structure(
    list(supports_structured_output = FALSE),
    class = c("semantica_session", "list")
  )

  items <- suppressWarnings(semantica_generate_items(
    session = session,
    scale_name = "Self-Efficacy",
    scale_description = "General capability beliefs.",
    factors = list(Self_Efficacy = list(description = "General perceived capability.")),
    n_per_factor = 40L,
    overgenerate = 1,
    max_retries = 3L,
    structured_output = "numbered",
    verbose = FALSE
  ))

  expect_identical(requested, c(40L, 5L, 5L))
  expect_equal(nrow(items), 40L)

  meta <- attr(items, "semantica_generation_metadata")
  expect_identical(meta$replenishment_policy, "yield_adaptive_v1")
  expect_identical(vapply(meta$attempts, `[[`, integer(1L), "requested"), c(40L, 5L, 5L))
  expect_identical(meta$attempts[[1L]]$newly_retained, 36L)
  expect_identical(meta$attempts[[2L]]$rejection_reasons, "backend_error")
  expect_true(meta$attempts[[3L]]$newly_retained >= 4L)
})
