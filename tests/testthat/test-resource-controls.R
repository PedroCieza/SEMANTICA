test_that("ACO resource controls bound work while returning a selected scale", {
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
})

test_that("parallel worker requests are capped for resource-conscious use", {
  expect_equal(SEMANTICA:::.semantica_max_workers(1L), 1L)
  expect_equal(SEMANTICA:::.semantica_max_workers(2L), 2L)
  expect_equal(SEMANTICA:::.semantica_max_workers(16L), 2L)
})
