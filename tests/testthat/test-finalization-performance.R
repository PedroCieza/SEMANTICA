test_that("semantic N sensitivity reuses the precomputed reference anchor", {
  fit_calls <- 0L
  fitted_n <- integer()
  scored_for <- function(n_obs, cfi) {
    list(
      converged = TRUE,
      cfi = cfi, tli = 0.91, rmsea = 0.05, srmr = 0.06,
      ave = 0.40, htmt_max = 0.20, loading_quality = 0.70, score = 0.60,
      structure_diagnostics = list(
        mean_primary_loading = 0.60,
        median_primary_loading = 0.60,
        primary_ge_50 = 1.00,
        q90_max_cross_loading = 0.10,
        correct_dominance = 1.00,
        simple_structure = 1.00,
        mean_abs_residual = 0.02,
        item_diagnostics = data.frame(
          ID = c("i1", "i2"),
          assigned_factor = c("F1", "F2"),
          dominant_factor = c("F1", "F2"),
          primary_loading = c(0.60, 0.60),
          max_cross_loading = c(0.10, 0.10),
          stringsAsFactors = FALSE
        )
      ),
      n_obs = n_obs
    )
  }

  local_mocked_bindings(
    run_esem_on_matrix = function(...) {
      args <- list(...)
      fit_calls <<- fit_calls + 1L
      fitted_n <<- c(fitted_n, as.integer(args$n_obs))
      list(n_obs = as.integer(args$n_obs))
    },
    extract_and_score_esem = function(fit, ...) {
      scored_for(fit$n_obs, cfi = 0.80)
    },
    .package = "SEMANTICA"
  )

  items <- c("i1", "i2")
  cor_matrix <- matrix(c(1, 0.2, 0.2, 1), 2L, 2L, dimnames = list(items, items))
  factor_assignment <- stats::setNames(c("F1", "F2"), items)
  out <- SEMANTICA:::evaluate_semantic_n_sensitivity(
    syntax = "efa(\"E\")*F1 =~ i1 + i2",
    cor_matrix = cor_matrix,
    factor_assignment = factor_assignment,
    factors = c("F1", "F2"),
    n_grid = c(10L, 20L),
    reference_n = 10L,
    precomputed_reference = list(n_obs = 10L, scored = scored_for(10L, cfi = 0.99)),
    progress = FALSE
  )

  expect_true(out$available)
  expect_equal(fit_calls, 1L)
  expect_equal(fitted_n, 20L)
  expect_equal(out$summary$refitted_anchors, 1L)
  expect_true(out$summary$reused_reference_fit)
  expect_equal(out$grid_results$cfi[out$grid_results$n_obs == 10L], 0.99)
  expect_equal(out$grid_results$cfi[out$grid_results$n_obs == 20L], 0.80)
})
