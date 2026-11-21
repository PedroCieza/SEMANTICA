fixed_comparison_fixture <- function() {
  ids <- letters[1:12]; assignment <- setNames(rep(c("A", "B", "C"), each = 4), ids)
  L <- matrix(0, 12, 3); L[cbind(seq_len(12), rep(1:3, each = 4))] <- rep(c(.35, .5, .7, .85), 3)
  Phi <- matrix(.35, 3, 3); diag(Phi) <- 1
  m <- L %*% Phi %*% t(L); diag(m) <- 1; dimnames(m) <- list(ids, ids)
  list(matrix = m, assignment = assignment, factors = c("A", "B", "C"), ids = ids)
}

test_that("fixed comparisons preserve full-pool scores and have distinct provenance", {
  x <- fixed_comparison_fixture()
  old <- do.call(.semantica_semantic_reference, x)
  ref <- do.call(.semantica_semantic_reference, c(x, list(comparison = "fixed")))
  raw <- .semantica_relative_semantic_components(x$matrix, x$assignment, x$factors)
  actual <- .semantica_apply_semantic_reference(raw, ref, x$matrix, x$assignment)
  expect_equal(actual$aggregate_score, raw$aggregate_score, tolerance = 1e-14)
  expect_equal(actual$factor_table, raw$factor_table, tolerance = 1e-14)
  expect_identical(actual$candidate_comparison, raw)
  expect_false(identical(ref$id, old$id))
  expect_false(identical(ref$schema, old$schema))
  expect_identical(ref$id, do.call(.semantica_semantic_reference, c(x, list(comparison = "fixed")))$id)
  expect_identical(ref$source, "native_admitted_full_pool_fixed_before_subset_evaluation")
  expect_error(.semantica_apply_semantic_reference(raw, ref), "frozen admitted pool")
  wrong <- x$assignment; wrong[1L] <- "B"
  expect_error(.semantica_apply_semantic_reference(raw, ref, x$matrix, wrong), "frozen admitted pool")
})

test_that("a domain's fixed base score is invariant to every other domain's replacements", {
  x <- fixed_comparison_fixture()
  ref <- do.call(.semantica_semantic_reference, c(x, list(comparison = "fixed")))
  forms <- expand.grid(A = 1:4, B = 1:4, C = 1:4)
  rows <- lapply(seq_len(nrow(forms)), function(i) {
    ids <- unlist(lapply(seq_along(x$factors), function(j)
      names(x$assignment)[x$assignment == x$factors[j]][-forms[i, j]]))
    score <- compute_semantic_sim_index_v2(x$matrix[ids, ids], ids, x$assignment[ids], x$factors,
      semantic_reference = ref)
    data.frame(forms[rep(i, length(x$factors)), ], score$relative_discrimination$factor_table, row.names = NULL)
  })
  rows <- do.call(rbind, rows)
  for (f in x$factors) for (own in 1:4) {
    z <- rows[rows$factor == f & rows[[f]] == own, ]
    for (metric in c("stochastic_superiority", "median_between", "median_gap", "robust_scale",
        "standardized_gap", "rank_component", "gap_component", "relative_score"))
      expect_equal(length(unique(z[[metric]])), 1L, info = paste(f, own, metric))
  }
})

test_that("global and domain comparison distributions stay fixed together", {
  ids <- letters[1:10]; assignment <- setNames(rep(c("A", "B"), c(4, 6)), ids)
  m <- matrix(.55, 10, 10, dimnames = list(ids, ids)); diag(m) <- 1
  m[1:4, 5:10] <- rep(c(.05, .1, .15, .25, .3, .35), each = 4)
  m[5:10, 1:4] <- t(m[1:4, 5:10])
  ref <- .semantica_semantic_reference(m, assignment, c("A", "B"), ids, "fixed")
  evaluate <- function(selected) compute_semantic_sim_index_v2(m[selected, selected], selected,
    assignment[selected], c("A", "B"), semantic_reference = ref)$relative_discrimination
  a <- evaluate(ids[c(1:3, 5:7)]); b <- evaluate(ids[c(1:3, 8:10)])
  expect_identical(a$global, b$global)
  expect_identical(a$factor_table, b$factor_table)
  expect_identical(a$aggregate_score, b$aggregate_score)
  expect_false(identical(a$candidate_comparison$global$median_gap, b$candidate_comparison$global$median_gap))
})

test_that("sorted fixed comparisons agree with a direct pairwise rank calculation including ties", {
  between <- sort(c(-.5, 0, 0, .2, .2, .8))
  ref <- .semantica_robust_relative_gap(c(.1, .4), between)
  for (within in list(c(-1, -.5, 0, .2, .9), c(0, 0, .2), c(NA, Inf, -.5, .8))) {
    actual <- .semantica_fixed_comparison_gap(within, between, ref)
    w <- within[is.finite(within)]
    expected <- mean(outer(w, between, ">") + .5 * outer(w, between, "=="))
    expect_equal(actual$stochastic_superiority, expected, tolerance = 1e-15)
    expect_equal(actual$median_gap, median(w) - median(between))
    expect_equal(actual$robust_scale, ref$robust_scale)
  }
})

test_that("fixed comparisons retain missing evidence, affine invariance and item-order invariance", {
  x <- fixed_comparison_fixture(); ids <- x$ids[c(1:3, 5:7, 9:11)]
  evaluate <- function(m, ids) {
    ref <- .semantica_semantic_reference(m, x$assignment, x$factors, x$ids, "fixed")
    compute_semantic_sim_index_v2(m[ids, ids], ids, x$assignment[ids], x$factors, semantic_reference = ref)
  }
  a <- evaluate(x$matrix, ids)
  expect_equal(a$sem_score, evaluate(.5*x$matrix + .1, ids)$sem_score, tolerance = 1e-14)
  expect_equal(a$sem_score, evaluate(x$matrix, rev(ids))$sem_score, tolerance = 1e-14)
  flat <- x$matrix; flat[,] <- .4; diag(flat) <- 1
  missing <- evaluate(flat, ids)
  expect_false(missing$semantic_evidence_available)
  expect_identical(missing$sem_score, 0)
  expect_true(is.na(missing$relative_discrimination$aggregate_score))
  sparse <- evaluate(x$matrix, ids[c(1, 4:9)])
  expect_false(sparse$semantic_evidence_available)
  expect_identical(sparse$sem_score, 0)
})

test_that("the new policy is opt-in and its guard can be overridden independently", {
  expect_identical(semantica_quality_config()$semantic_reference_policy, "candidate")
  q <- semantica_quality_config(semantic_reference_policy = "admitted_pool_fixed_comparison")
  p <- .semantica_resolve_semantic_policy(q$semantic_reference_policy)
  expect_identical(p$gap_reference, "admitted_pool_fixed_comparison")
  expect_identical(p$cohesion_guard, "lower_only")
  expect_identical(p$schema, "semantica-semantic-policy-v3")
  expect_identical(p$calibration_status, "not_externally_calibrated")
  p <- .semantica_resolve_semantic_policy("candidate", "admitted_pool_fixed_comparison", "two_sided")
  expect_identical(p$id, "admitted_pool_fixed_comparison_two_sided")
  expect_identical(semantica_quality_config(semantic_gap_reference = p$gap_reference)$semantic_gap_reference,
    "admitted_pool_fixed_comparison")
})

test_that("native search and audits share the fixed policy and retain scalar-first rescue safeguards", {
  ids <- letters[1:18]; assignment <- setNames(rep(c("A", "B", "C"), each = 6), ids)
  L <- matrix(0, 18, 3); L[cbind(seq_len(18), rep(1:3, each = 6))] <- rep(seq(.4, .8, length.out = 6), 3)
  m <- tcrossprod(L); diag(m) <- 1; dimnames(m) <- list(ids, ids)
  x <- list(matrix = m, ids = ids, assignment = assignment)
  args <- list(cosine_sim_matrix = x$matrix, df = data.frame(ID = x$ids, Dimension = x$assignment),
    i.per.f = c(A = 3L, B = 3L, C = 3L), ants = 2L, max.iter = 1L, max_total_iter = 1L,
    run_esem_during_search = FALSE, esem_weight = 0, pfa_mode = "off", dfi_warmup_iters = 0L,
    semantic_esem_score_mode = "continuous_balanced", psychometric_guard_action = "report_only",
    elite_multicriteria_rerank = FALSE, final_selection_mode = "scalar", final_structure_repair = FALSE,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE, final_dfi_recalibrate = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE, use_parallel = FALSE, verbose = FALSE, seed = 606L,
    semantic_reference_policy = "admitted_pool_fixed_comparison")
  local_mocked_bindings(run_esem_on_matrix = function(...) {
    expect_identical(list(...)$progress$stage, "final")
    list(fit = NULL)
  }, compute_pfa_diagnostics = function(...) stop("No PFA authorized"), .package = "SEMANTICA")
  withr::local_preserve_seed(); set.seed(902L)
  baseline <- do.call(ACO_with_ESEM, args)
  after <- .Random.seed; set.seed(902L)
  rescue <- do.call(ACO_with_ESEM, c(args, list(proposal_rescue = TRUE, proposal_rescue_max_evals = 8L)))
  expect_identical(.Random.seed, after)
  expect_identical(rescue$pheromone, baseline$pheromone)
  expect_gte(rescue$objective_context$value, baseline$objective_context$value)
  expect_true(rescue$proposal_rescue$scalar_only_score_preserved)
  expect_equal(rescue$proposal_rescue$esem_fits, 0L)
  expect_identical(rescue$model_info$semantic_policy$id, "admitted_pool_fixed_comparison")
  expect_identical(rescue$model_info$semantic_reference$schema, "semantica-admitted-pool-fixed-comparison-v1")
  audited <- do.call(semantica_audit_solutions, c(args, list(solutions = list(selected = rescue$best_items),
    stage = "proposal", max_new_esem_fits = 0L, execute = TRUE)))
  expect_equal(audited$results$proposal_score, as.numeric(rescue$best_objective), tolerance = 1e-12)
  expect_identical(audited$semantic_policy$id, "admitted_pool_fixed_comparison")
})

test_that("custom pipelines forward the new opt-in policy", {
  captured <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    captured <<- list(...); stop("captured configuration")
  }, .package = "SEMANTICA")
  expect_error(semantica_run_custom("fixture", "fixture", list(A = "a", B = "b"),
    quality = semantica_quality_config(semantic_reference_policy = "admitted_pool_fixed_comparison"),
    checkpoint_dir = NULL, verbose = FALSE), "captured configuration")
  expect_identical(captured$semantic_reference_policy, "admitted_pool_fixed_comparison")
})
