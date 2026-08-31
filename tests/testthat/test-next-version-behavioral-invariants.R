esem_control_score <- function(fx) {
  cor_mat <- SEMANTICA:::transform_cosine_for_esem(fx$cosine)
  syntax <- SEMANTICA:::build_esem_syntax_safe(fx$ids, fx$assignment, fx$factors)
  fit <- SEMANTICA:::run_esem_on_matrix(
    syntax, cor_mat, n_obs = 500L, estimator = "ML", rotation = "geomin",
    iter_max = 250L, fallback = FALSE
  )
  SEMANTICA:::extract_and_score_esem(
    fit, observed_cor = cor_mat, factor_assignment = fx$assignment, factors = fx$factors
  )
}

test_that("synthetic controls preserve directional semantic discrimination", {
  sep <- make_semantica_control_fixture("separable")
  ov <- make_semantica_control_fixture("overlapping")
  sh <- make_semantica_control_fixture("shuffled")
  d_sep <- SEMANTICA:::.cosine_diagnostics(sep$cosine, sep$assignment)
  d_ov <- SEMANTICA:::.cosine_diagnostics(ov$cosine, ov$assignment)
  d_sh <- SEMANTICA:::.cosine_diagnostics(sh$cosine, sh$assignment)
  expect_gt(d_sep$within_between_gap, d_ov$within_between_gap)
  expect_gt(d_sep$within_between_gap, d_sh$within_between_gap)
})

test_that("positive-control PFA clarity exceeds negative controls", {
  sep <- make_semantica_control_fixture("separable")
  ov <- make_semantica_control_fixture("overlapping")
  sh <- make_semantica_control_fixture("shuffled")
  p_sep <- SEMANTICA:::compute_pfa_diagnostics(sep$cosine, sep$assignment, sep$factors, extraction = "principal", rotation = "promax")
  p_ov <- SEMANTICA:::compute_pfa_diagnostics(ov$cosine, ov$assignment, ov$factors, extraction = "principal", rotation = "promax")
  p_sh <- SEMANTICA:::compute_pfa_diagnostics(sh$cosine, sh$assignment, sh$factors, extraction = "principal", rotation = "promax")
  expect_true(p_sep$available)
  expect_gt(p_sep$score, p_ov$score)
  expect_gt(p_sep$clarity_score, p_sh$clarity_score)
})

test_that("positive-control ESEM structural score exceeds negative controls when comparable", {
  sep <- esem_control_score(make_semantica_control_fixture("separable"))
  ov <- esem_control_score(make_semantica_control_fixture("overlapping"))
  sh <- esem_control_score(make_semantica_control_fixture("shuffled"))
  skip_if_not(isTRUE(sep$admissible), "Positive-control ESEM was not admissible on this lavaan build.")
  comparable <- Filter(function(x) isTRUE(x$admissible), list(overlap = ov, shuffled = sh))
  skip_if(length(comparable) == 0L, "Negative-control ESEM fits were not admissible/comparable.")
  for (x in comparable) expect_gt(sep$score, x$score)
})

test_that("failed or inadmissible ESEM does not fabricate downstream evidence", {
  scored <- SEMANTICA:::extract_and_score_esem(NULL)
  expect_false(isTRUE(scored$admissible))
  expect_true(is.na(scored$htmt_max))
  expect_true(is.na(scored$ave))
  expect_null(scored$structure_diagnostics)
})

test_that("ACO obeys exact selection and budget contracts on deterministic controls", {
  semantica_test_mock_esem_unavailable()
  fx <- make_semantica_control_fixture("separable")
  out <- ACO_with_ESEM(
    cosine_sim_matrix = fx$cosine, df = fx$df, i.per.f = fx$i.per.f,
    ants = 12L, max.iter = 4L, max_total_iter = 3L, max_esem_fits = 1L,
    run_esem_during_search = FALSE, esem_failure_policy = "semantic_fallback", pfa_mode = "off", use_parallel = FALSE,
    dfi_mode = "heuristic_semantic", semantic_n_sensitivity = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE, validation_n_diagnostic = FALSE,
    esem_sample_size = 200L, full_esem_iter_max = 200L, elite_multicriteria_rerank = FALSE,
    seed = 11L, verbose = FALSE
  )
  selected <- out$best_items
  expect_length(selected, sum(fx$i.per.f))
  expect_length(unique(selected), length(selected))
  expect_true(all(selected %in% fx$ids))
  selected_factor <- fx$assignment[selected]
  expect_identical(as.integer(table(factor(selected_factor, levels = names(fx$i.per.f)))), unname(as.integer(fx$i.per.f)))
  expect_lte(out$total_iterations, 3L)
  expect_lte(out$esem_attempts %||% 0L, 1L)
  expect_true(is.character(out$termination_reason) && nzchar(out$termination_reason))
})

test_that("ACO final objective is recomputable from the selected solution", {
  semantica_test_mock_esem_unavailable()
  fx <- make_semantica_control_fixture("separable")
  out <- ACO_with_ESEM(
    cosine_sim_matrix = fx$cosine, df = fx$df, i.per.f = fx$i.per.f,
    ants = 10L, max.iter = 3L, max_total_iter = 2L,
    run_esem_during_search = FALSE, esem_failure_policy = "semantic_fallback", pfa_mode = "off", use_parallel = FALSE,
    dfi_mode = "heuristic_semantic", semantic_n_sensitivity = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE, validation_n_diagnostic = FALSE,
    esem_sample_size = 200L, full_esem_iter_max = 200L, elite_multicriteria_rerank = FALSE,
    seed = 7L, verbose = FALSE
  )
  recomputed <- out$semantic_score * out$duplicate_penalty * out$facet_coverage_multiplier
  expect_equal(out$semantic_objective_score, recomputed, tolerance = 1e-10)
})

test_that("optimization does not make a deliberately bad control clean by definition", {
  semantica_test_mock_esem_unavailable()
  sh <- make_semantica_control_fixture("shuffled")
  before <- SEMANTICA:::.cosine_diagnostics(sh$cosine, sh$assignment)$within_between_gap
  out <- ACO_with_ESEM(
    cosine_sim_matrix = sh$cosine, df = sh$df, i.per.f = sh$i.per.f,
    ants = 10L, max.iter = 3L, max_total_iter = 2L,
    run_esem_during_search = FALSE, esem_failure_policy = "semantic_fallback", pfa_mode = "off", use_parallel = FALSE,
    dfi_mode = "heuristic_semantic", semantic_n_sensitivity = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE, validation_n_diagnostic = FALSE,
    esem_sample_size = 200L, full_esem_iter_max = 200L, elite_multicriteria_rerank = FALSE,
    seed = 17L, verbose = FALSE
  )
  selected <- out$best_items
  after <- SEMANTICA:::.cosine_diagnostics(sh$cosine[selected, selected, drop = FALSE], sh$assignment[selected])$within_between_gap
  expect_true(is.finite(before) && is.finite(after))
  expect_lt(after, 0.75)
})
