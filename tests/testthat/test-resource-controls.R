performance_baseline <- dget(testthat::test_path(
  "fixtures", "performance-baseline.R"
))

test_that("ACO resource controls preserve structural invariants under the 0.3 optimizer", {
  skip_if_not_installed("lavaan")

  items <- paste0("item_", seq_len(8L))
  factor <- rep(c("F1", "F2"), each = 4L)
  embedding <- rbind(
    c(1.00, 0.08, 0.02), c(0.98, 0.14, 0.03),
    c(0.96, 0.16, 0.04), c(0.94, 0.19, 0.05),
    c(0.08, 1.00, 0.04), c(0.12, 0.98, 0.05),
    c(0.15, 0.96, 0.03), c(0.18, 0.94, 0.06)
  )
  embedding <- embedding / sqrt(rowSums(embedding^2))
  cos_mat <- tcrossprod(embedding)
  dimnames(cos_mat) <- list(items, items)
  item_df <- data.frame(item = items, type = factor, factor = factor)

  expect_equal(
    cos_mat,
    performance_baseline$inputs$cosine,
    tolerance = performance_baseline$manifest$numeric_tolerance
  )

  set.seed(101)
  out <- ACO_with_ESEM(
    cosine_sim_matrix = cos_mat,
    df = item_df,
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
    # This test compares against the frozen pre-0.2.7 objective baseline.
    # Pin the historical target estimator so the test isolates resource
    # controls rather than the intentional 0.2.7 method-default change.
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
  expect_equal(
    unname(out$duplicate_cluster_id),
    performance_baseline$semantic$duplicate_cluster_id
  )
  expect_equal(
    out$heuristic_cutoffs,
    performance_baseline$semantic$heuristic_cutoffs
  )
  expect_equal(
    out$evaluation_telemetry$esem_fits_started,
    performance_baseline$semantic$esem_search_jobs
  )
  expect_true(all(
    performance_baseline$semantic$required_result_fields %in% names(out)
  ))
})

test_that("parallel worker requests respect the visible allocation without a fixed cap", {
  expect_equal(SEMANTICA:::.semantica_max_workers(1L, available.cores = 16L), 1L)
  expect_equal(SEMANTICA:::.semantica_max_workers(2L, available.cores = 16L), 2L)
  expect_equal(SEMANTICA:::.semantica_max_workers(16L, available.cores = 16L), 16L)
})

test_that("PFA objective can run on a tunable search interval", {
  skip_if_not_installed("lavaan")

  items <- paste0("item_", seq_len(8L))
  factor <- rep(c("F1", "F2"), each = 4L)
  embedding <- rbind(
    c(1.00, 0.08, 0.02), c(0.98, 0.14, 0.03),
    c(0.96, 0.16, 0.04), c(0.94, 0.19, 0.05),
    c(0.08, 1.00, 0.04), c(0.12, 0.98, 0.05),
    c(0.15, 0.96, 0.03), c(0.18, 0.94, 0.06)
  )
  embedding <- embedding / sqrt(rowSums(embedding^2))
  cos_mat <- tcrossprod(embedding)
  dimnames(cos_mat) <- list(items, items)
  item_df <- data.frame(item = items, type = factor, factor = factor)

  set.seed(202)
  out <- ACO_with_ESEM(
    cosine_sim_matrix = cos_mat,
    df = item_df,
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
    # This test compares against the frozen pre-0.2.7 objective baseline.
    # Pin the historical target estimator so the test isolates resource
    # controls rather than the intentional 0.2.7 method-default change.
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
