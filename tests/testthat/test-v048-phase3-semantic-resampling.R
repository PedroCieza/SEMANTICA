test_that("semantic resampling uses stratified bootstrap and item jackknife without a binary cutoff", {
  ids <- paste0("i", 1:6)
  fa <- stats::setNames(rep(c("A", "B"), each = 3), ids)
  m <- matrix(0.20, 6, 6, dimnames = list(ids, ids))
  diag(m) <- 1
  m[fa == "A", fa == "A"] <- 0.80
  m[fa == "B", fa == "B"] <- 0.75
  diag(m) <- 1
  out <- semantica_semantic_resampling_stability(m, fa, reps = 50L, seed = 42L)
  expect_identical(out$status, "computed")
  expect_identical(out$evidence_family, "embedding_semantic")
  expect_true(is.na(out$binary_stability_classification))
  expect_equal(out$pair_bootstrap$reps, 50L)
  expect_true(out$observed$stochastic_superiority > 0.5)
  expect_equal(nrow(out$item_jackknife$estimates), 6L)
  expect_true(all(c("lower", "median", "upper") %in% names(out$pair_bootstrap$stochastic_superiority_interval)))
})

test_that("semantic resampling handles one-factor forms without inventing between-factor evidence", {
  ids <- paste0("i", 1:4)
  fa <- stats::setNames(rep("A", 4), ids)
  m <- matrix(.55, 4, 4, dimnames = list(ids, ids)); diag(m) <- 1
  out <- semantica_semantic_resampling_stability(m, fa, reps = 20L, seed = 7L)
  expect_identical(out$dimensionality_mode, "unidimensional")
  expect_true(is.na(out$observed$stochastic_superiority))
  expect_true(is.finite(out$observed$within_median))
})
