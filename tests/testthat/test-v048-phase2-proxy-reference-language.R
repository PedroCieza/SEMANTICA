test_that("semantic-proxy cutoff comparisons are not labeled psychometric PASS/FAIL", {
  expect_identical(
    SEMANTICA:::.semantica_proxy_reference_status(0.99, 0.95, "higher"),
    "REFERENCE MET"
  )
  expect_identical(
    SEMANTICA:::.semantica_proxy_reference_status(0.90, 0.95, "higher"),
    "REFERENCE NOT MET"
  )
  expect_identical(
    SEMANTICA:::.semantica_proxy_reference_status(0.08, 0.09, "lower"),
    "REFERENCE MET"
  )
  expect_identical(
    SEMANTICA:::.semantica_proxy_reference_status(NA_real_, 0.09, "lower"),
    "N/A"
  )
})
