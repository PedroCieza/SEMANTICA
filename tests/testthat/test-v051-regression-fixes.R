test_that("ensemble clustering preserves square distance geometry", {
  ids <- paste0("i", 1:6)
  m1 <- matrix(.15, 6, 6, dimnames = list(ids, ids))
  m1[1:3, 1:3] <- .85
  m1[4:6, 4:6] <- .85
  diag(m1) <- 1
  m2 <- m1
  m2[lower.tri(m2)] <- m2[lower.tri(m2)] - .01
  m2[upper.tri(m2)] <- t(m2)[upper.tri(m2)]
  diag(m2) <- 1

  out <- semantica_ensemble_cluster_similarity(list(a = m1, b = m2), k = 2L)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(6L, 6L))
  expect_identical(rownames(out), ids)
  expect_identical(colnames(out), ids)
  expect_true(all(diag(out) == 1))
})
