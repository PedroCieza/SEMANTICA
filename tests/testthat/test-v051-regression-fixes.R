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

test_that("ensemble alignment rejects conflicting named item identities", {
  m1 <- diag(4)
  rownames(m1) <- colnames(m1) <- paste0("a", 1:4)
  m2 <- diag(4)
  rownames(m2) <- colnames(m2) <- paste0("b", 1:4)

  expect_error(
    semantica_ensemble_similarity(list(first = m1, second = m2)),
    "share at least two common item IDs"
  )
})

test_that("matrix repair telemetry records clipping against the true input baseline", {
  m <- matrix(1, 4L, 4L)
  rownames(m) <- colnames(m) <- paste0("i", 1:4)

  repaired <- SEMANTICA:::transform_cosine_for_esem(m)
  info <- attr(repaired, "semantica_matrix_repair", exact = TRUE)

  expect_true(isTRUE(info$repair_required))
  expect_identical(info$matrix_source, "repaired_semantic_proxy")
  expect_equal(info$min_eigen_before, 0, tolerance = 1e-10)
  expect_gt(info$clipping_frobenius_change, 0)
  expect_gt(info$frobenius_change, 0)
})

test_that("two-item representation ensembles preserve pair matrix dimensions", {
  ids <- c("i1", "i2")
  m1 <- matrix(c(1, .20, .20, 1), 2L, 2L, dimnames = list(ids, ids))
  m2 <- matrix(c(1, .40, .40, 1), 2L, 2L, dimnames = list(ids, ids))

  out <- semantica_ensemble_similarity(list(a = m1, b = m2), method = "mean")

  expect_equal(dim(out), c(2L, 2L))
  expect_identical(dimnames(out), list(ids, ids))
  expect_equal(out["i1", "i2"], .30)
})
