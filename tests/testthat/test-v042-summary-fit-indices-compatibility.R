test_that("summary accepts legacy atomic fit_indices after unidimensional reporting extension", {
  x <- list(
    best_items = c("i1", "i2"),
    semantic_score = 0.7,
    fit_indices = c(cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_),
    termination_reason = "archive_stable"
  )
  class(x) <- c("semantica_full_pipeline_result", "list")

  s <- expect_no_error(summary(x))
  expect_null(s$unidimensional_diagnostics)
  expect_equal(names(s$semantic_proxy_esem), c("cfi", "rmsea", "srmr"))
  expect_no_error(print(s))
})

test_that("summary still exposes rich unidimensional diagnostics from list fit_indices", {
  u <- list(status = "computed", ave = 0.55, mean_primary_loading = 0.70)
  x <- list(
    dimensionality_mode = "unidimensional",
    best_items = paste0("i", 1:4),
    semantic_score = 0.6,
    fit_indices = list(
      cfi = 0.97, rmsea = 0.04, srmr = 0.03,
      unidimensional_diagnostics = u
    ),
    termination_reason = "archive_stable"
  )
  class(x) <- c("semantica_full_pipeline_result", "list")

  s <- expect_no_error(summary(x))
  expect_identical(s$unidimensional_diagnostics, u)
  expect_equal(unlist(s$semantic_proxy_esem), c(cfi = 0.97, rmsea = 0.04, srmr = 0.03))
})
