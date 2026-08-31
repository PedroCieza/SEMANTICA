test_that("cluster consensus recovers a clean intended semantic partition", {
  ids <- paste0("i", 1:6)
  m <- matrix(.15, 6, 6, dimnames = list(ids, ids))
  m[1:3, 1:3] <- .85
  m[4:6, 4:6] <- .85
  diag(m) <- 1
  fa <- setNames(rep(c("A", "B"), each = 3), ids)
  z <- semantica_cluster_consensus(m, fa)
  expect_true(z$available)
  expect_identical(z$evidence_family, "embedding_semantic")
  expect_equal(z$mean_intended_adjusted_rand, 1, tolerance = 1e-12)
  expect_equal(z$within_factor_consensus, 1, tolerance = 1e-12)
  expect_equal(z$between_factor_consensus, 0, tolerance = 1e-12)
})

test_that("cluster consensus explicitly remains sample-free embedding evidence", {
  ids <- paste0("i", 1:4)
  m <- diag(4); dimnames(m) <- list(ids, ids)
  m[1,2] <- m[2,1] <- .8
  m[3,4] <- m[4,3] <- .8
  m[1:2,3:4] <- m[3:4,1:2] <- .2
  z <- semantica_cluster_consensus(m, setNames(c("A","A","B","B"), ids))
  expect_false(z$participant_based)
  expect_match(z$interpretation, "not participant validation", ignore.case = TRUE)
})
