test_that("adjusted Rand partition agreement is label-permutation invariant", {
  intended <- c("A","A","B","B","C","C")
  same <- c("x","x","y","y","z","z")
  x <- SEMANTICA:::.semantica_adjusted_rand_index(intended, same)
  expect_equal(x$value, 1, tolerance = 0)
  expect_null(x$reason)
})

test_that("adjusted Rand detects shuffled partitions", {
  intended <- c("A","A","A","B","B","B","C","C","C")
  shuffled <- c("x","y","z","x","y","z","x","y","z")
  x <- SEMANTICA:::.semantica_adjusted_rand_index(intended, shuffled)
  expect_lt(x$value, 1)
})

test_that("adjusted Rand reports degenerate or missing cases explicitly", {
  one <- SEMANTICA:::.semantica_adjusted_rand_index(c("A","A","A"), c("x","x","x"))
  expect_true(is.na(one$value)); expect_match(one$reason, "degenerate")
  missing <- SEMANTICA:::.semantica_adjusted_rand_index(c("A", NA), c("x", "y"))
  expect_true(is.na(missing$value)); expect_match(missing$reason, "too few")
})

test_that("PFA preserves factor-presence recovery while exposing partition agreement", {
  fx <- make_semantica_control_fixture("shuffled")
  p <- SEMANTICA:::compute_pfa_diagnostics(
    fx$cosine, fx$assignment, fx$factors, extraction = "principal", rotation = "promax"
  )
  expect_identical(p$factor_presence_recovery, p$recovery_score)
  expect_true("partition_agreement_ari" %in% names(p))
  if (is.finite(p$factor_presence_recovery) && p$factor_presence_recovery > .9 &&
      is.finite(p$partition_agreement_ari)) {
    expect_lt(p$partition_agreement_ari, p$factor_presence_recovery)
  }
})
