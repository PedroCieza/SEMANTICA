test_that("stochastic-superiority semantic discrimination has correct orientation and ties", {
  ids <- c("A1","A2","A3","B1","B2","B3")
  fa <- setNames(rep(c("A","B"), each = 3), ids)
  m <- matrix(.2, 6, 6, dimnames = list(ids, ids)); diag(m) <- 1
  m[1:3,1:3] <- .8; diag(m[1:3,1:3]) <- 1
  m[4:6,4:6] <- .8; diag(m[4:6,4:6]) <- 1
  x <- semantica_semantic_discrimination(m, fa)
  expect_identical(x$status, "computed")
  expect_equal(x$estimate, 1, tolerance = 0)

  t <- matrix(.5, 6, 6, dimnames = list(ids, ids)); diag(t) <- 1
  tied <- semantica_semantic_discrimination(t, fa)
  expect_equal(tied$estimate, .5, tolerance = 0)
})

test_that("semantic discrimination is invariant to strictly monotonic transforms", {
  ids <- c("A1","A2","A3","B1","B2","B3")
  fa <- setNames(rep(c("A","B"), each = 3), ids)
  z <- matrix(seq(.05,.95,length.out=36),6,6); z <- (z+t(z))/2; diag(z) <- 1
  dimnames(z) <- list(ids,ids)
  a <- semantica_semantic_discrimination(z, fa)
  b <- semantica_semantic_discrimination(exp(z), fa)
  expect_equal(a$estimate, b$estimate, tolerance = 0)
})

test_that("semantic discrimination returns explicit unavailable reasons", {
  ids <- c("I1","I2","I3")
  m <- diag(3); dimnames(m) <- list(ids,ids)
  one <- semantica_semantic_discrimination(m, setNames(rep("A",3),ids))
  expect_true(is.na(one$estimate))
  expect_identical(one$status, "unavailable")
  expect_match(one$reason, "between-factor")
})
