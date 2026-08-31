.proper_esem_matrices <- function() {
  list(
    lambda = matrix(c(0.80, 0.15, 0.10, 0.75), 2L, 2L, byrow = TRUE),
    theta = diag(c(0.36, 0.44)),
    psi = matrix(c(1.0, 0.35, 0.35, 1.0), 2L, 2L)
  )
}

test_that("a converged, proper matrix solution is admissible", {
  matrices <- .proper_esem_matrices()
  assessment <- SEMANTICA:::assess_esem_admissibility(
    converged = TRUE,
    post_check = TRUE,
    lambda = matrices$lambda,
    theta = matrices$theta,
    psi = matrices$psi,
    estimates = c(0.8, 0.35, 0.44)
  )

  expect_true(assessment$admissible)
  expect_length(assessment$reasons, 0L)
  expect_identical(assessment$details$post_check_status, "passed")
  expect_equal(assessment$details$max_abs_latent_correlation, 0.35)
})

test_that("an unavailable post-check is recorded without false rejection", {
  matrices <- .proper_esem_matrices()
  assessment <- SEMANTICA:::assess_esem_admissibility(
    converged = TRUE,
    post_check = NA,
    lambda = matrices$lambda,
    theta = matrices$theta,
    psi = matrices$psi
  )

  expect_true(assessment$admissible)
  expect_identical(assessment$details$post_check_status, "unavailable")
  expect_contains(assessment$warnings, "post_check_unavailable")
})

test_that("admissibility rejects convergence and post-check failures", {
  matrices <- .proper_esem_matrices()
  nonconverged <- SEMANTICA:::assess_esem_admissibility(
    FALSE, TRUE, matrices$lambda, matrices$theta, matrices$psi
  )
  failed_post_check <- SEMANTICA:::assess_esem_admissibility(
    TRUE, FALSE, matrices$lambda, matrices$theta, matrices$psi
  )

  expect_false(nonconverged$admissible)
  expect_contains(nonconverged$reasons, "not_converged")
  expect_false(failed_post_check$admissible)
  expect_contains(failed_post_check$reasons, "post_check_failed")
})

test_that("nonfinite standardized matrices or estimates are rejected", {
  matrices <- .proper_esem_matrices()
  matrices$lambda[1L, 1L] <- Inf
  bad_matrix <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, matrices$psi
  )
  matrices <- .proper_esem_matrices()
  bad_estimate <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, matrices$psi,
    estimates = c(0.4, NA_real_)
  )

  expect_contains(bad_matrix$reasons, "nonfinite_lambda")
  expect_contains(bad_estimate$reasons, "nonfinite_estimates")
})

test_that("negative residual variance is rejected with numerical tolerance", {
  matrices <- .proper_esem_matrices()
  matrices$theta[1L, 1L] <- -1e-3
  improper <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, matrices$psi
  )
  matrices$theta[1L, 1L] <- -1e-10
  numerical_noise <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, matrices$psi,
    residual_tolerance = 1e-8
  )

  expect_false(improper$admissible)
  expect_contains(improper$reasons, "negative_residual_variance")
  expect_true(numerical_noise$admissible)
})

test_that("invalid latent covariance and correlations are rejected", {
  matrices <- .proper_esem_matrices()
  zero_variance <- matrices$psi
  zero_variance[2L, 2L] <- 0
  zero_assessment <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, zero_variance
  )

  invalid_correlation <- matrix(c(1, 1.2, 1.2, 1), 2L, 2L)
  correlation_assessment <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, invalid_correlation
  )

  singular <- matrix(1, 2L, 2L)
  singular_assessment <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, singular
  )

  expect_contains(zero_assessment$reasons, "nonpositive_latent_variance")
  expect_contains(correlation_assessment$reasons, "invalid_latent_correlation")
  expect_contains(correlation_assessment$reasons, "latent_covariance_not_positive_definite")
  expect_contains(singular_assessment$reasons, "latent_covariance_not_positive_definite")
})

test_that("admissibility does not impose a substantive loading cutoff", {
  matrices <- .proper_esem_matrices()
  matrices$lambda[1L, 1L] <- 1.20
  assessment <- SEMANTICA:::assess_esem_admissibility(
    TRUE, TRUE, matrices$lambda, matrices$theta, matrices$psi
  )

  expect_true(assessment$admissible)
})

test_that("the lavaan fit predicate returns a boolean and optional assessment", {
  skip_if_not_installed("lavaan")
  data("HolzingerSwineford1939", package = "lavaan")
  model <- paste(
    "visual =~ x1 + x2 + x3",
    "textual =~ x4 + x5 + x6",
    "speed =~ x7 + x8 + x9",
    sep = "\n"
  )
  fit <- lavaan::cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)

  predicate <- SEMANTICA:::is_admissible_esem_fit(fit)
  assessment <- SEMANTICA:::is_admissible_esem_fit(
    fit, return_assessment = TRUE
  )

  expect_type(predicate, "logical")
  expect_length(predicate, 1L)
  expect_s3_class(assessment, "semantica_esem_admissibility")
  expect_identical(predicate, assessment$admissible)
})

test_that("admissible ESEM search jobs pass the integrated gate and ledger", {
  item_ids <- paste0("item_", seq_len(10L))
  intended <- rep(c("F1", "F2"), each = 5L)
  lambda <- matrix(0.08, nrow = 10L, ncol = 2L)
  lambda[cbind(seq_len(10L), rep(seq_len(2L), each = 5L))] <- 0.68
  phi <- matrix(c(1, 0.25, 0.25, 1), nrow = 2L)
  similarity <- lambda %*% phi %*% t(lambda)
  diag(similarity) <- 1
  dimnames(similarity) <- list(item_ids, item_ids)

  result <- ACO_with_ESEM(
    cosine_sim_matrix = similarity,
    df = data.frame(item = item_ids, factor = intended),
    i.per.f = c(F1 = 4L, F2 = 4L),
    ants = 4L,
    max.iter = 1L,
    max_total_iter = 1L,
    esem_every = 1L,
    run_esem_during_search = TRUE,
    max_esem_fits = 2L,
    esem_failure_policy = "stop",
    dfi_mode = "heuristic_semantic",
    pfa_mode = "off",
    run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE,
    validation_n_diagnostic = FALSE,
    final_dddfi = FALSE,
    final_equivtest = FALSE,
    use_parallel = FALSE,
    seed = 20260820L,
    verbose = FALSE
  )

  expect_equal(result$evaluation_telemetry$esem_fits_started, 2L)
  expect_equal(result$evaluation_telemetry$esem_fits_admissible, 2L)
  expect_equal(result$evaluation_telemetry$esem_fits_failed, 0L)
  expect_true(result$esem_admissible)
  expect_true(result$esem_alignment$globally_optimal_assignment)
})

test_that("an inadmissible full archive refit cannot win or finalize", {
  bootstrap_fit_calls <- 0L
  search_fit_calls <- 0L
  archive_fit_calls <- 0L
  archive_candidate_signatures <- character(0L)

  local_mocked_bindings(
    run_esem_on_matrix = function(..., iter_max = NULL, fallback = TRUE,
                                  return_diagnostics = FALSE) {
      if (is.null(iter_max)) {
        bootstrap_fit_calls <<- bootstrap_fit_calls + 1L
        return(NULL)
      }
      # Search fits disable solver fallback in fast mode; archive/final fits
      # use full fallback. Both now request solver diagnostics.
      if (isTRUE(return_diagnostics) && isTRUE(fallback)) {
        archive_fit_calls <<- archive_fit_calls + 1L
        dots <- list(...)
        # Full-ESEM finalization may now receive multiple evidence-stratified
        # finalists. Record a stable signature so the regression guards against
        # duplicate refits rather than incorrectly requiring a single finalist.
        candidate_syntax <- if (length(dots) >= 1L) dots[[1L]] else NULL
        candidate_signature <- if (!is.null(candidate_syntax)) {
          paste(as.character(candidate_syntax), collapse = "\n")
        } else {
          paste0("archive_call_", archive_fit_calls)
        }
        archive_candidate_signatures <<- c(archive_candidate_signatures, candidate_signature)
        return(list(
          fit = list(stage = "archive"),
          accepted_attempt = 1L,
          solver_attempts_started = 1L
        ))
      }
      if (isTRUE(return_diagnostics)) {
        search_fit_calls <<- search_fit_calls + 1L
        return(list(
          fit = list(stage = "search"),
          accepted_attempt = 1L,
          solver_attempts_started = 1L
        ))
      }
      archive_fit_calls <<- archive_fit_calls + 1L
      list(stage = "archive")
    },
    extract_and_score_esem = function(esem_fit, ...) {
      if (identical(esem_fit$stage, "archive")) {
        return(list(
          converged = TRUE,
          admissible = FALSE,
          admissibility = list(
            admissible = FALSE,
            reasons = "mock_improper_archive_solution"
          ),
          score = 1e9
        ))
      }
      list(
        converged = TRUE,
        admissible = TRUE,
        admissibility = list(admissible = TRUE, reasons = character(0L)),
        score = 0.80,
        loading_quality = 0.80,
        ave = 0.60,
        htmt_max = 0.30,
        structure_diagnostics = NULL
      )
    },
    .package = "SEMANTICA"
  )

  item_ids <- paste0("item_", seq_len(10L))
  intended <- rep(c("F1", "F2"), each = 5L)
  lambda <- matrix(0.08, nrow = 10L, ncol = 2L)
  lambda[cbind(seq_len(10L), rep(seq_len(2L), each = 5L))] <- 0.68
  phi <- matrix(c(1, 0.25, 0.25, 1), nrow = 2L)
  similarity <- lambda %*% phi %*% t(lambda)
  diag(similarity) <- 1
  dimnames(similarity) <- list(item_ids, item_ids)

  expect_error(
    ACO_with_ESEM(
      cosine_sim_matrix = similarity,
      df = data.frame(item = item_ids, factor = intended),
      i.per.f = c(F1 = 4L, F2 = 4L),
      ants = 2L,
      max.iter = 1L,
      max_total_iter = 1L,
      esem_every = 1L,
      esem_eval_top_k = 1L,
      run_esem_during_search = TRUE,
      max_esem_fits = 1L,
      esem_failure_policy = "stop",
      dfi_mode = "heuristic_semantic",
      pfa_mode = "off",
      run_pfa_during_search = FALSE,
      semantic_n_sensitivity = FALSE,
      validation_n_diagnostic = FALSE,
      final_dddfi = FALSE,
      final_equivtest = FALSE,
      use_parallel = FALSE,
      seed = 20260820L,
      verbose = FALSE
    ),
    "No archived solution produced an admissible full-ESEM refit"
  )

  expect_equal(bootstrap_fit_calls, 0L)
  expect_equal(search_fit_calls, 1L)
  # v0.3 uses an evidence-stratified finalist reservoir. More than one unique
  # finalist may therefore require a canonical full-ESEM refit. The invariant
  # is that candidates are not refit redundantly and none of the inadmissible
  # finalists may win/finalize.
  expect_gte(archive_fit_calls, 1L)
  expect_equal(length(unique(archive_candidate_signatures)), archive_fit_calls)
})
