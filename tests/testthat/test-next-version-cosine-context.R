test_that("cosine context reports raw and representation-relative information", {
  ids <- paste0("I", 1:6)
  m <- matrix(c(
    1,.9,.8,.3,.2,.1,
    .9,1,.85,.25,.2,.15,
    .8,.85,1,.2,.15,.1,
    .3,.25,.2,1,.88,.82,
    .2,.2,.15,.88,1,.84,
    .1,.15,.1,.82,.84,1
  ), 6, byrow = TRUE, dimnames = list(ids, ids))
  fa <- setNames(rep(c("A","B"), each = 3), ids)
  x <- semantica_cosine_context(
    m, item_i = "I1", item_j = "I2", factor_assignment = fa,
    threshold = .90, threshold_source = "fixed_user_or_default",
    embedding_model = "fixture-model"
  )
  expect_equal(x$cosine, .9)
  expect_true(x$all_pairs_percentile >= 0 && x$all_pairs_percentile <= 1)
  expect_true(x$within_factor_percentile >= 0 && x$within_factor_percentile <= 1)
  expect_equal(x$threshold, .90)
  expect_identical(x$threshold_source, "fixed_user_or_default")
  expect_false(x$participant_based)
})

test_that("cosine percentiles are stable under a strictly monotonic transform", {
  ids <- paste0("I", 1:5)
  m <- matrix(c(
    1,.1,.2,.3,.4,
    .1,1,.5,.6,.7,
    .2,.5,1,.8,.9,
    .3,.6,.8,1,.95,
    .4,.7,.9,.95,1
  ), 5, byrow = TRUE, dimnames = list(ids, ids))
  fa <- setNames(c("A","A","B","B","B"), ids)
  a <- semantica_cosine_context(m, observed = .8, factor_assignment = fa)
  f <- function(x) exp(x)
  mt <- f(m); diag(mt) <- f(1)
  b <- semantica_cosine_context(mt, observed = f(.8), factor_assignment = fa)
  expect_equal(a$all_pairs_percentile, b$all_pairs_percentile, tolerance = 0)
  expect_equal(a$within_factor_percentile, b$within_factor_percentile, tolerance = 0)
  expect_equal(a$between_factor_percentile, b$between_factor_percentile, tolerance = 0)
})
