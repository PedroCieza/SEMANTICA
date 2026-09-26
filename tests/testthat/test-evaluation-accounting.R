test_that("evaluation broker coalesces duplicates and does not charge cache hits", {
  cache <- new.env(parent = emptyenv())
  vectors <- list(c(1L, 0L, 1L), c(1L, 0L, 1L), c(0L, 1L, 1L))
  cache_set(cache, make_solution_key(vectors[[3L]]), list(
    esem_evaluated = TRUE,
    esem_score = 0.8,
    fit_result = list(converged = TRUE, admissible = TRUE)
  ))
  broker <- .semantica_new_evaluation_broker(max_esem_fits = 1L)

  plan <- .semantica_plan_esem_batch(
    broker, seq_along(vectors), vectors, cache
  )

  expect_equal(plan$indices, c(1L, 3L))
  expect_equal(plan$jobs_started, c(TRUE, FALSE))
  expect_equal(plan$request_indices, c(1L, 2L, 3L))
  expect_equal(plan$request_to_evaluation, c(1L, 1L, 2L))
  representative_scores <- c(0.6, 0.8)
  expect_equal(
    representative_scores[plan$request_to_evaluation],
    c(0.6, 0.6, 0.8)
  )
  snap <- .semantica_evaluation_snapshot(broker)
  expect_equal(snap$esem_requests, 3L)
  expect_equal(snap$esem_coalesced_requests, 1L)
  expect_equal(snap$esem_cache_hits, 1L)
  expect_equal(snap$esem_unique_candidates, 2L)
  expect_equal(snap$esem_fits_started, 1L)
})

test_that("evaluation broker records converged and admissible jobs separately", {
  broker <- .semantica_new_evaluation_broker(2L)
  payloads <- list(
    list(cache_entry = list(fit_attempt = 2L,
                            fit_result = list(converged = TRUE, admissible = TRUE))),
    list(cache_entry = list(fit_attempt = 1L,
                            fit_result = list(converged = TRUE, admissible = FALSE)))
  )
  .semantica_record_esem_payloads(broker, payloads, c(TRUE, TRUE))
  snap <- .semantica_evaluation_snapshot(broker)

  expect_equal(snap$esem_fits_converged, 2L)
  expect_equal(snap$esem_fits_admissible, 1L)
  expect_equal(snap$esem_fits_failed, 1L)
  expect_equal(snap$esem_solver_attempts_observed, 3L)
})

test_that("evaluation broker records full-solver retry telemetry separately", {
  broker <- .semantica_new_evaluation_broker(2L)
  payloads <- list(
    list(
      score = NA_real_,
      key = "failed",
      cache_entry = list(
        fit_result = list(
          converged = FALSE,
          admissible = FALSE,
          admissibility = list(reasons = "not_converged")
        )
      ),
      error = "not_converged"
    ),
    list(
      score = 0.5,
      key = "recovered",
      cache_entry = list(
        fit_result = list(
          converged = TRUE,
          admissible = TRUE,
          admissibility = list(reasons = character(0L))
        )
      ),
      error = NA_character_
    )
  )

  .semantica_record_esem_retry_payloads(
    broker, payloads, keys = c("failed", "recovered")
  )
  snap <- .semantica_evaluation_snapshot(broker)

  expect_equal(snap$esem_full_retries_started, 2L)
  expect_equal(snap$esem_full_retries_admissible, 1L)
  expect_equal(snap$esem_full_retries_failed, 1L)
  expect_true(any(snap$esem_events$stage == "search_full_retry"))
})
