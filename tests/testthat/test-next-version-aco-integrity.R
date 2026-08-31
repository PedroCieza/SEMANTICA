test_that("ACO requires usable named per-factor targets and enough candidates", {
  semantica_test_mock_esem_unavailable()
  fx <- semantica_test_three_factor_fixture("separable")
  args <- semantica_test_aco_args(fx)
  args$i.per.f <- c(2L, 2L, 2L)
  expect_error(do.call(ACO_with_ESEM, args))

  args <- semantica_test_aco_args(fx)
  args$i.per.f <- c(F1 = 99L, F2 = 2L, F3 = 2L)
  expect_error(do.call(ACO_with_ESEM, args), "candidate|eligible|available|enough", ignore.case = TRUE)
})

test_that("ACO best-found solution obeys selection and search-budget invariants", {
  out <- semantica_test_run_aco()
  selected_counts <- table(factor(unname(out$factor_assignment), levels = c("F1", "F2", "F3")))
  expect_identical(as.integer(selected_counts), c(2L, 2L, 2L))
  expect_equal(length(out$best_items), 6L)
  expect_identical(anyDuplicated(out$best_items), 0L)
  eligible_flat <- unlist(out$eligible_items, use.names = FALSE)
  expect_true(all(out$best_items %in% eligible_flat))
  expect_lte(out$total_iterations, out$max_total_iter)
  expect_lte(out$esem_attempts %||% 0L, out$max_esem_fits)
  expect_true(is.character(out$termination_reason) && nzchar(out$termination_reason))
})

test_that("stored semantic objective components recompute from final selected solution", {
  out <- semantica_test_run_aco()
  recomputed <- out$semantic_score * out$duplicate_penalty * out$facet_coverage_multiplier
  expect_equal(out$semantic_objective_score, recomputed, tolerance = 1e-12)
  # Diagnostic-only PFA must not contribute to the search objective.
  expect_equal(out$model_info$pfa_mode %||% "diagnostic", "diagnostic")
  expect_equal(out$search_objective_score, out$semantic_objective_score, tolerance = 1e-12)
})

test_that("same seed has contract-consistent ACO reproducibility without disabling stochasticity", {
  a <- semantica_test_run_aco(seed = 888L)
  b <- semantica_test_run_aco(seed = 888L)
  expect_equal(a$best_items, b$best_items)
  expect_equal(a$best_objective, b$best_objective, tolerance = 1e-12)
  expect_equal(a$termination_reason, b$termination_reason)
  expect_equal(a$total_iterations, b$total_iterations)
})

test_that("elite archive is unique, bounded, ordered consistently, and serializable", {
  out <- semantica_test_run_aco(seed = 77L)
  archive <- out$elite_archive
  scores <- out$elite_archive_scores
  expect_lte(length(archive), 3L)
  keys <- vapply(archive, function(z) {
    if (!is.null(z$vec)) {
      return(paste(which(as.integer(z$vec) == 1L), collapse = "|"))
    }
    paste(sort(unique(as.character(z$items %||% character()))), collapse = "|")
  }, character(1L))
  expect_identical(anyDuplicated(keys), 0L)
  if (length(scores) > 1L) expect_true(all(diff(scores) <= 1e-12))

  path <- tempfile(fileext = ".rds")
  saveRDS(list(archive = archive, scores = scores), path)
  reloaded <- readRDS(path)
  expect_equal(reloaded$archive, archive)
  expect_equal(reloaded$scores, scores)
})

test_that("negative semantic controls are not cleaned merely by optimization", {
  semantica_test_mock_esem_unavailable()
  pos <- semantica_test_three_factor_fixture("separable")
  # A constant off-diagonal representation contains no within-vs-between
  # ordering information.  This is a genuine negative control for the new
  # relative semantic objective; the older "overlapping" fixture still has
  # perfect stochastic ordering (.62 within > .55 between) and is therefore
  # intentionally not treated as semantic collapse.
  neg <- semantica_test_three_factor_fixture("overlapping")
  neg$matrix[,] <- 0.55
  diag(neg$matrix) <- 1
  p <- do.call(ACO_with_ESEM, semantica_test_aco_args(pos, seed = 55L))
  n <- do.call(ACO_with_ESEM, semantica_test_aco_args(neg, seed = 55L))
  expect_gt(p$semantic_score, n$semantic_score)
})
