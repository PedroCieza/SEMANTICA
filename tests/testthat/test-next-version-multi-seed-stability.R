mock_multi_seed_aco <- function(cosine_sim_matrix, df, i.per.f, seed = NULL, ...) {
  odd <- (as.integer(seed) %% 2L) == 1L
  selected <- if (odd) c("i1", "i2", "i5", "i6", "i9", "i10") else c("i1", "i3", "i5", "i7", "i9", "i11")
  factor <- c("F1", "F1", "F2", "F2", "F3", "F3")
  list(
    best_items = selected,
    factor_assignment = stats::setNames(factor, selected),
    best_objective = if (odd) 0.81 else 0.80,
    proposal_objective_score = if (odd) 0.79 else 0.78,
    search_guidance_status = "semantic_only",
    esem_attempts = 0L,
    esem_successes = 0L,
    esem_failures = 0L
  )
}

test_that("multi-seed result exposes equifinality summaries without declaring an optimum", {
  local_mocked_bindings(ACO_with_ESEM = mock_multi_seed_aco, .package = "SEMANTICA")
  fx <- semantica_test_three_factor_fixture("separable")
  res <- run_multi_seed_semantica(
    seeds = c(101L, 202L), cosine_sim_matrix = fx$matrix, df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L), verbose_seeds = FALSE
  )
  expect_true(is.list(res))
  expect_identical(res$n_unique_solutions, 2L)
  expect_length(res$unique_solutions, res$n_unique_solutions)
  expect_named(res$objective_dispersion, c("sd", "iqr", "range", "min", "median", "max"))
  expect_true(is.list(res$objective_comparability))
  expect_true(isTRUE(res$objective_comparability$comparable_across_seeds))
  expect_true(is.data.frame(res$factor_inclusion_frequency))
  expect_true(all(c("item_id", "factor", "count", "frequency") %in% names(res$factor_inclusion_frequency)))
  expect_true(all(res$factor_inclusion_frequency$frequency >= 0 & res$factor_inclusion_frequency$frequency <= 1))
  expect_false("global_optimum" %in% names(res))
})

test_that("multi-seed selection frequency and Jaccard remain descriptive", {
  local_mocked_bindings(ACO_with_ESEM = mock_multi_seed_aco, .package = "SEMANTICA")
  fx <- semantica_test_three_factor_fixture("separable")
  res <- run_multi_seed_semantica(
    seeds = c(1L, 2L), cosine_sim_matrix = fx$matrix, df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L), verbose_seeds = FALSE
  )
  expect_true(all(as.numeric(res$item_frequencies) <= res$n_successful))
  expect_true(all(res$pairwise_jaccard >= 0 & res$pairwise_jaccard <= 1))
  expect_equal(unname(res$pairwise_jaccard[[1L]]), 1/3, tolerance = 1e-12)
  expect_false("global_optimum" %in% names(res))
})

test_that("real multi-seed ACO/ESEM integration is opt-in", {
  skip_if(Sys.getenv("SEMANTICA_RUN_SLOW_TESTS") != "true",
          "real multi-seed ESEM integration is intentionally excluded from ordinary CI")
  fx <- semantica_test_three_factor_fixture("separable")
  res <- run_multi_seed_semantica(
    seeds = c(1L, 2L), cosine_sim_matrix = fx$matrix, df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L), verbose_seeds = FALSE,
    ants = 4L, max.iter = 2L, max_total_iter = 2L, max_esem_fits = 1L,
    use_parallel = FALSE, run_esem_during_search = FALSE,
    pfa_mode = "off", dfi_mode = "heuristic_semantic",
    semantic_n_sensitivity = FALSE, final_dddfi = FALSE,
    final_equivtest = FALSE, validation_n_diagnostic = FALSE,
    full_esem_iter_max = 250L, verbose = FALSE
  )
  expect_true(is.list(res))
  expect_gte(res$n_successful, 1L)
})


test_that("multi-seed objective comparability flags mixed evidence regimes", {
  mixed_regime_aco <- function(cosine_sim_matrix, df, i.per.f, seed = NULL, ...) {
    odd <- (as.integer(seed) %% 2L) == 1L
    selected <- if (odd) c("i1", "i2", "i5", "i6", "i9", "i10") else c("i1", "i3", "i5", "i7", "i9", "i11")
    factor <- c("F1", "F1", "F2", "F2", "F3", "F3")
    list(
      best_items = selected,
      factor_assignment = stats::setNames(factor, selected),
      best_objective = if (odd) .82 else .79,
      proposal_objective_score = .78,
      search_guidance_status = if (odd) "semantic_fallback_no_admissible_archive_esem" else "esem_guided",
      objective_context = list(
        evidence_regime = if (odd) "semantic_fallback_no_admissible_archive_esem" else "esem_guided"
      ),
      esem_attempts = 1L, esem_successes = if (odd) 0L else 1L, esem_failures = if (odd) 1L else 0L
    )
  }

  local_mocked_bindings(ACO_with_ESEM = mixed_regime_aco, .package = "SEMANTICA")
  fx <- semantica_test_three_factor_fixture("separable")
  res <- run_multi_seed_semantica(
    seeds = c(1L, 2L), cosine_sim_matrix = fx$matrix, df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L), verbose_seeds = FALSE
  )

  expect_false(isTRUE(res$objective_comparability$comparable_across_seeds))
  expect_setequal(
    unname(res$objective_comparability$evidence_regimes),
    c("semantic_fallback_no_admissible_archive_esem", "esem_guided")
  )
  expect_match(res$objective_comparability$note, "should not", fixed = TRUE)
})
