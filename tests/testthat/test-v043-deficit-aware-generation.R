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
