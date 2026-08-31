test_that("pair perturbation legacy boundary is explicitly heuristic", {
  below <- SEMANTICA:::.semantica_pair_perturbation_classify(.05)
  above <- SEMANTICA:::.semantica_pair_perturbation_classify(.15)
  boundary <- SEMANTICA:::.semantica_pair_perturbation_classify(.10)

  expect_true(below$stable)
  expect_identical(below$classification, "heuristically_stable")
  expect_false(above$stable)
  expect_identical(above$classification, "heuristically_unstable")
  expect_false(boundary$stable) # preserves the legacy strict '< 0.10' rule
  expect_identical(boundary$threshold_status, "legacy_uncalibrated_heuristic")
  expect_equal(boundary$threshold, .10)
})

test_that("pair perturbation unavailable state remains explicit", {
  x <- SEMANTICA:::.semantica_pair_perturbation_classify(NA_real_)
  expect_true(is.na(x$stable))
  expect_identical(x$classification, "unavailable")
})
