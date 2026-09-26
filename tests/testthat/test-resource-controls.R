performance_baseline <- dget(testthat::test_path(
  "fixtures", "performance-baseline.R"
))

.resource_control_fixture <- function() {
  per_factor <- 5L
  factor <- rep(c("F1", "F2"), each = per_factor)
  items <- paste0("item_", seq_along(factor))
  n_items <- length(items)
  embedding <- matrix(0, nrow = n_items, ncol = 2L + n_items)
  primary <- 0.78
  unique_weight <- sqrt(1 - primary^2)
  for (i in seq_len(n_items)) {
    embedding[i, match(factor[[i]], c("F1", "F2"))] <- primary
    embedding[i, 2L + i] <- unique_weight
  }
  embedding <- embedding / sqrt(rowSums(embedding^2))
  cos_mat <- tcrossprod(embedding)
  dimnames(cos_mat) <- list(items, items)
  item_df <- data.frame(item = items, type = factor, factor = factor)
  list(items = items, factor = factor, embedding = embedding,
       cosine = cos_mat, df = item_df)
}

test_that("legacy near-duplicate performance fixture is rejected by the new pre-ACO contract", {
  cos_mat <- performance_baseline$inputs$cosine
  factor <- performance_baseline$inputs$factor
  items <- performance_baseline$inputs$item_ids
  gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    cosine_sim_matrix = cos_mat,
    df = data.frame(item = items, type = factor, factor = factor),
    i.per.f = c(F1 = 3L, F2 = 3L),
    min_slack = 0L,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_false(gate$feasible)
  expect_true(all(gate$table$independent_duplicate_units < gate$table$selected_n))
})

test_that("ACO resource controls preserve structural invariants on a feasible pool", {
  skip_if_not_installed("lavaan")
  fx <- .resource_control_fixture()

  gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    fx$cosine, fx$df, c(F1 = 3L, F2 = 3L),
    min_slack = 2L,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_true(gate$feasible)

  set.seed(101)
  out <- ACO_with_ESEM(
    cosine_sim_matrix = fx$cosine,
    df = fx$df,
    i.per.f = c(F1 = 3L, F2 = 3L),
    ants = 2L,
    max.iter = 50L,
    max_total_iter = 2L,
    run_esem_during_search = FALSE,
    dfi_mode = "heuristic_semantic",
    pfa_mode = "off",
    final_dddfi = FALSE,
    final_equivtest = FALSE,
    semantic_n_sensitivity = FALSE,
    validation_n_diagnostic = FALSE,
    within_target_method = "legacy_q40",
    archive_stable_window = 100L,
    keep_solution_history = TRUE,
    history_mode = "summary",
    use_parallel = FALSE,
    verbose = FALSE
  )

  expect_equal(out$total_iterations, 2L)
  expect_equal(out$termination_reason, "max_total_iter_reached")
  expect_lte(length(out$solution_history), 2L)
  expect_equal(length(out$best_items), 6L)
  expect_equal(as.integer(table(out$factor_assignment)[c("F1", "F2")]), c(3L, 3L))
  expect_true(is.finite(out$semantic_objective_score))
  expect_lte(length(out$elite_archive), 10L)
  expect_identical(out$objective_schema$version, "SEMANTICA-objective-v4")
  expect_false(isTRUE(out$objective_schema$cross_schema_raw_score_comparison))
  expect_true(all(is.na(out$duplicate_cluster_id)))
  expect_equal(out$heuristic_cutoffs, list(cfi = 0.96, tli = 0.94, rmsea = 0.09, srmr = 0.08))
  expect_equal(out$evaluation_telemetry$esem_fits_started, 0L)
  expect_true(all(c(
    "best_items", "best_objective", "duplicate_clusters",
    "elite_archive", "termination_reason", "total_iterations",
    "evaluation_telemetry", "performance", "resource_plan",
    "reproducibility", "esem_alignment", "esem_admissibility"
  ) %in% names(out)))
})

test_that("parallel worker requests respect the visible allocation without a fixed cap", {
  expect_equal(SEMANTICA:::.semantica_max_workers(1L, available.cores = 16L), 1L)
  expect_equal(SEMANTICA:::.semantica_max_workers(2L, available.cores = 16L), 2L)
  expect_equal(SEMANTICA:::.semantica_max_workers(16L, available.cores = 16L), 16L)
})

test_that("PFA objective can run on a tunable search interval", {
  skip_if_not_installed("lavaan")
  fx <- .resource_control_fixture()

  set.seed(202)
  out <- ACO_with_ESEM(
    cosine_sim_matrix = fx$cosine,
    df = fx$df,
    i.per.f = c(F1 = 3L, F2 = 3L),
    ants = 2L,
    max.iter = 50L,
    max_total_iter = 3L,
    run_esem_during_search = FALSE,
    dfi_mode = "heuristic_semantic",
    pfa_mode = "objective",
    pfa_weight = 0.30,
    pfa_every = 2L,
    pfa_final_extraction = "principal",
    final_dddfi = FALSE,
    final_equivtest = FALSE,
    semantic_n_sensitivity = FALSE,
    validation_n_diagnostic = FALSE,
    within_target_method = "legacy_q40",
    archive_stable_window = 100L,
    history_mode = "summary",
    use_parallel = FALSE,
    verbose = FALSE
  )

  expect_true(out$run_pfa_during_search)
  expect_equal(out$pfa_every, 2L)
  expect_equal(out$pfa_search_iterations, 1L)
  expect_equal(out$pfa_search_attempts, 2L)
  expect_gt(out$pfa_search_successes, 0L)
  expect_true(isTRUE(out$pfa_diagnostics$available))
  expect_true(is.matrix(out$pfa_diagnostics$loadings))
  expect_true(is.finite(out$best_objective))
  expect_true(isTRUE(out$objective_schema$pfa$active))
  expect_identical(out$objective_schema$pfa$score_schema, "pfa-proposal-v2")
  expect_gt(length(out$evidence_archives$pfa), 0L)
})
