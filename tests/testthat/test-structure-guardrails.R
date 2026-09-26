test_that("structure guardrails add transparent penalties", {
  fit <- list(
    converged = TRUE,
    loading_quality = 0.90,
    ave = 0.50,
    htmt_max = 0.90,
    ave_warnings = character(),
    structure_diagnostics = list(
      min_primary_loading = 0.35,
      primary_ge_50 = 0.55,
      simple_structure = 0.60,
      correct_dominance = 0.75,
      max_cross_loading = 0.45
    )
  )

  baseline <- SEMANTICA:::compute_psychometric_guard_penalty(
    fit,
    min_ave = 0.30,
    min_primary_loading = 0.30,
    min_primary_prop_ge_50 = 0.50,
    htmt_guard_threshold = Inf
  )
  guarded <- SEMANTICA:::compute_psychometric_guard_penalty(
    fit,
    min_ave = 0.30,
    min_primary_loading = 0.30,
    min_primary_prop_ge_50 = 0.50,
    min_simple_structure = 0.80,
    min_correct_dominance = 0.80,
    max_cross_loading = 0.30,
    htmt_guard_threshold = 0.85
  )

  expect_lt(as.numeric(guarded), as.numeric(baseline))
  diagnostics <- attr(guarded, "diagnostics", exact = TRUE)
  expect_false(diagnostics$passed)
  expect_true(all(
    c("simple_structure", "correct_dominance", "max_cross_loading", "htmt_max") %in%
      diagnostics$criteria$criterion
  ))
})

test_that("PFA boundary-loading ceiling penalizes improper proxy geometry", {
  fit <- list(
    converged = TRUE,
    loading_quality = 0.95,
    ave = 0.60,
    htmt_max = 0.40,
    ave_warnings = character(),
    structure_diagnostics = list(
      min_primary_loading = 0.60,
      primary_ge_50 = 1.00
    )
  )
  pfa <- list(
    available = TRUE,
    max_abs_loading_observed = 1.02,
    loadings = matrix(c(1.02, 0.10, 0.15, 0.70), nrow = 2L)
  )

  guarded <- SEMANTICA:::compute_psychometric_guard_penalty(
    fit,
    min_ave = 0.30,
    min_primary_loading = 0.40,
    min_primary_prop_ge_50 = 0.70,
    pfa_result = pfa,
    pfa_max_abs_loading = 0.95
  )

  diagnostics <- attr(guarded, "diagnostics", exact = TRUE)
  expect_lt(as.numeric(guarded), 1)
  expect_true("pfa_max_abs_loading" %in% diagnostics$criteria$criterion)
})

test_that("PFA diagnostics report and score boundary-loading penalties", {
  m <- matrix(0.10, nrow = 6L, ncol = 6L)
  diag(m) <- 1
  rownames(m) <- colnames(m) <- c("A1", "A2", "A3", "B1", "B2", "B3")
  m[1:3, 1:3] <- 0.70
  m[4:6, 4:6] <- 0.70
  diag(m) <- 1
  fa <- c(A1 = "A", A2 = "A", A3 = "A", B1 = "B", B2 = "B", B3 = "B")

  unbounded <- SEMANTICA:::compute_pfa_diagnostics(
    m, fa, c("A", "B"),
    extraction = "principal",
    rotation = "promax",
    max_abs_loading = Inf
  )
  bounded <- SEMANTICA:::compute_pfa_diagnostics(
    m, fa, c("A", "B"),
    extraction = "principal",
    rotation = "promax",
    max_abs_loading = 0.20
  )

  expect_true(isTRUE(unbounded$available))
  expect_true(isTRUE(bounded$available))
  expect_gt(bounded$boundary_loading_count, 0)
  expect_lt(bounded$boundary_loading_penalty, 1)
  expect_lt(bounded$score, unbounded$score)
})

test_that("ML promax PFA preserves oblique factor correlations", {
  # Unequal primary loadings and small cross-loadings avoid the exact rotational
  # symmetry of an equal-loading simple-structure population. That symmetry can
  # yield numerically equivalent promax axes with different factor correlations
  # across supported BLAS implementations.
  lambda <- matrix(.05, nrow = 8L, ncol = 2L)
  lambda[1:4, 1L] <- c(.95, .85, .75, .65)
  lambda[5:8, 2L] <- c(.92, .82, .72, .62)
  phi_pop <- matrix(c(1, .60, .60, 1), 2L, 2L)
  common <- lambda %*% phi_pop %*% t(lambda)
  cor_mat <- common
  diag(cor_mat) <- 1
  rownames(cor_mat) <- colnames(cor_mat) <- paste0("i", seq_len(nrow(cor_mat)))

  pfa <- SEMANTICA:::extract_pfa_loadings(
    cor_mat,
    n_factors = 2L,
    extraction = "ml",
    rotation = "promax",
    already_transformed = TRUE
  )

  expect_true(is.list(pfa))
  expect_true(is.matrix(pfa$phi))
  expect_equal(dim(pfa$phi), c(2L, 2L))
  expect_gt(max(abs(pfa$phi[upper.tri(pfa$phi)])), .50)
})

test_that("quality/PFA configs and ACO formals expose structure guardrails", {
  q <- semantica_quality_config(
    psychometric_guard_min_simple_structure = 0.85,
    psychometric_guard_min_dominance = 0.90,
    psychometric_guard_max_cross_loading = 0.25,
    psychometric_guard_include_htmt = TRUE,
    final_structure_repair = TRUE,
    final_structure_repair_max_swaps = 3L,
    final_structure_repair_max_seconds = 30,
    final_structure_repair_max_evals = 12,
    final_structure_repair_prefilter_top_k = 5,
    final_structure_repair_auto_skip_infeasible = TRUE
  )
  p <- semantica_pfa_config(max_abs_loading = 0.95)

  expect_equal(q$psychometric_guard_min_simple_structure, 0.85)
  expect_equal(q$psychometric_guard_min_dominance, 0.90)
  expect_equal(q$psychometric_guard_max_cross_loading, 0.25)
  expect_true(q$psychometric_guard_include_htmt)
  expect_true(q$final_structure_repair)
  expect_equal(q$final_structure_repair_max_swaps, 3L)
  expect_equal(q$final_structure_repair_max_seconds, 30)
  expect_equal(q$final_structure_repair_max_evals, 12)
  expect_equal(q$final_structure_repair_prefilter_top_k, 5)
  expect_true(q$final_structure_repair_auto_skip_infeasible)
  expect_equal(p$max_abs_loading, 0.95)
  expect_true(all(
    c(
      "facet_constraint_mode",
      "facet_min_per_facet",
      "facet_max_imbalance",
      "psychometric_guard_action",
      "psychometric_guard_min_simple_structure",
      "psychometric_guard_min_dominance",
      "psychometric_guard_max_cross_loading",
      "psychometric_guard_include_htmt",
      "pfa_max_abs_loading",
      "final_structure_repair",
      "final_structure_repair_max_seconds",
      "final_structure_repair_max_evals",
      "final_structure_repair_prefilter_top_k",
      "final_structure_repair_auto_skip_infeasible"
    ) %in% names(formals(ACO_with_ESEM))
  ))
})

test_that("auto DFI ROC skip covers bounded high-dimensional auto runs", {
  should_skip <- SEMANTICA:::.semantica_should_skip_auto_semantic_roc

  expect_true(should_skip(
    dfi_mode = "auto",
    dfi_esem_reps = 150L,
    dfi_esem_reps_supplied = FALSE,
    dfi_search_reps_supplied = FALSE,
    use_parallel = FALSE,
    n_items = 48L,
    n_factors = 6L
  ))
  expect_false(should_skip("semantic_roc_dfi", 150L, FALSE, FALSE, FALSE, 48L, 6L))
  expect_false(should_skip("auto", 150L, TRUE, FALSE, TRUE, 48L, 6L))
  expect_true(should_skip("auto", 50L, FALSE, TRUE, TRUE, 48L, 6L))
  expect_true(should_skip("auto", 150L, FALSE, FALSE, TRUE, 48L, 6L))
  expect_false(should_skip("auto", 300L, FALSE, FALSE, TRUE, 48L, 6L))
  expect_false(should_skip("auto", 150L, FALSE, FALSE, FALSE, 24L, 3L))
})

test_that("auto DFI semantic-approx skip is limited to serial high-dimensional auto runs", {
  should_skip <- SEMANTICA:::.semantica_should_skip_auto_semantic_approx

  expect_true(should_skip(
    dfi_mode = "auto",
    auto_skipped_semantic_roc = TRUE,
    use_parallel = FALSE,
    n_items = 48L,
    n_factors = 6L
  ))
  expect_false(should_skip("semantic_approx_dfi", TRUE, FALSE, 48L, 6L))
  expect_false(should_skip("auto", FALSE, FALSE, 48L, 6L))
  expect_false(should_skip("auto", TRUE, TRUE, 48L, 6L))
  expect_false(should_skip("auto", TRUE, FALSE, 24L, 3L))
})

test_that("final repair is explicit when ESEM-guided selection is inactive", {
  semantica_test_mock_esem_unavailable()
  fx <- semantica_test_three_factor_fixture("separable")
  args <- semantica_test_aco_args(fx, seed = 909L)
  args$final_structure_repair <- TRUE

  out <- do.call(ACO_with_ESEM, args)

  expect_true(out$final_structure_repair$enabled)
  expect_false(out$final_structure_repair$attempted)
  expect_equal(out$final_structure_repair$reason, "requires_esem_guided_selection")
  expect_equal(out$summary$final_structure_repair$reason, "requires_esem_guided_selection")
})

test_that("final repair auto-skips no-slack pools instead of evaluating impossible swaps", {
  local_mocked_bindings(
    run_esem_on_matrix = function(...) list(fit = list()),
    extract_and_score_esem = function(fit, cor_matrix, factor_assignment, factors, ...) {
      ids <- rownames(cor_matrix)
      assigned <- unname(factor_assignment[ids])
      list(
        converged = TRUE,
        admissible = TRUE,
        admissibility = list(admissible = TRUE, reasons = character(0L)),
        cfi = 0.96,
        tli = 0.95,
        rmsea = 0.03,
        srmr = 0.04,
        ave = 0.50,
        htmt_max = 0.20,
        htmt_violations = 0L,
        loading_quality = 0.80,
        score = 0.70,
        structure_diagnostics = list(
          mean_primary_loading = 0.60,
          min_primary_loading = 0.60,
          primary_ge_50 = 1.00,
          correct_dominance = 1.00,
          simple_structure = 1.00,
          max_cross_loading = 0.10,
          no_large_cross_loading = 1.00,
          item_diagnostics = data.frame(
            ID = ids,
            assigned_factor = assigned,
            dominant_factor = assigned,
            primary_loading = 0.60,
            max_cross_loading = 0.10,
            simple_structure = TRUE,
            stringsAsFactors = FALSE
          )
        )
      )
    },
    .package = "SEMANTICA"
  )

  fx <- semantica_test_three_factor_fixture("separable", per_factor = 2L)
  args <- semantica_test_aco_args(fx, seed = 910L, history_mode = "none")
  args$ants <- 2L
  args$max.iter <- 1L
  args$max_total_iter <- 1L
  args$run_esem_during_search <- TRUE
  args$max_esem_fits <- 1L
  # This regression intentionally reaches the repair layer with an infeasible
  # pool; production defaults now stop at the hard pre-ACO gate.
  args$preaco_feasibility_action <- "warn"
  args$preaco_min_slack <- 0L
  args$final_structure_repair <- TRUE

  out <- do.call(ACO_with_ESEM, args)

  expect_true(out$final_structure_repair$enabled)
  expect_false(out$final_structure_repair$attempted)
  expect_equal(out$final_structure_repair$reason, "pool_infeasible_no_repair_capacity")
  expect_equal(sort(out$final_structure_repair$infeasible_factors), c("F1", "F2", "F3"))
  expect_equal(out$final_structure_repair$evaluated_swaps, 0L)
  expect_equal(out$proxy_quality$status, "failed_pool_feasibility")
  expect_equal(out$summary$quality_status, "failed_pool_feasibility")
  expect_false(out$eligible_for_participant_validation)
  expect_identical(out$validation_status, "not_eligible_for_participant_validation")
  expect_true(is.list(out$structural_guard))
  expect_identical(out$structural_guard$action, "penalty")
})

test_that("final repair targets global AVE and primary-loading failures", {
  local_mocked_bindings(
    run_esem_on_matrix = function(...) list(fit = list()),
    extract_and_score_esem = function(fit, cor_matrix, factor_assignment, factors, ...) {
      ids <- rownames(cor_matrix)
      assigned <- unname(factor_assignment[ids])
      list(
        converged = TRUE,
        admissible = TRUE,
        admissibility = list(admissible = TRUE, reasons = character(0L)),
        cfi = .96, tli = .95, rmsea = .03, srmr = .04,
        ave = .20, htmt_max = .20, htmt_violations = 0L,
        loading_quality = .46, score = .70,
        structure_diagnostics = list(
          mean_primary_loading = .46,
          min_primary_loading = .46,
          primary_ge_50 = 0,
          correct_dominance = 1,
          simple_structure = 1,
          max_cross_loading = .10,
          no_large_cross_loading = 1,
          item_diagnostics = data.frame(
            ID = ids,
            assigned_factor = assigned,
            dominant_factor = assigned,
            primary_loading = .46,
            max_cross_loading = .10,
            simple_structure = TRUE,
            stringsAsFactors = FALSE
          )
        )
      )
    },
    .package = "SEMANTICA"
  )

  fx <- semantica_test_three_factor_fixture("separable", per_factor = 4L)
  args <- semantica_test_aco_args(fx, seed = 911L, history_mode = "none")
  args$ants <- 2L
  args$max.iter <- 1L
  args$max_total_iter <- 1L
  args$run_esem_during_search <- TRUE
  args$max_esem_fits <- 4L
  args$psychometric_guard_action <- "final_constraint"
  args$psychometric_guard_min_ave <- .35
  args$psychometric_guard_min_loading <- .45
  args$psychometric_guard_min_primary_ge_50 <- .75
  args$final_structure_repair <- TRUE
  args$final_structure_repair_max_swaps <- 2L
  args$final_structure_repair_candidates_per_factor <- 2L
  args$final_structure_repair_max_evals <- 6L
  args$final_structure_repair_prefilter_top_k <- 6L

  err <- tryCatch(
    do.call(ACO_with_ESEM, args),
    semantica_error_no_admissible_solution = function(e) e
  )
  expect_s3_class(err, "semantica_error_no_admissible_solution")
  expect_gt(err$final_structure_repair$evaluated_swaps, 0L)
  expect_equal(
    err$final_structure_repair$reason,
    "no_guard_violation_reducing_same_factor_swap"
  )
})
