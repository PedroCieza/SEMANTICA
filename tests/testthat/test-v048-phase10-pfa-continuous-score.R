test_that("PFA threshold attainment is separated from continuous structural quality", {
  ids <- paste0("i", 1:6)
  m <- matrix(.20, 6, 6, dimnames = list(ids, ids))
  m[1:3,1:3] <- .80
  m[4:6,4:6] <- .80
  diag(m) <- 1
  fa <- setNames(rep(c("A","B"), each = 3), ids)
  p <- compute_pfa_diagnostics(m, fa, c("A","B"), extraction = "principal", rotation = "promax")
  expect_true(p$available)
  expect_identical(p$score_schema, "pfa-continuous-geometry-v2")
  expect_true(is.finite(p$criterion_attainment_score))
  expect_true(is.finite(p$continuous_salience_score))
  expect_true(is.finite(p$continuous_clarity_score))
  expect_equal(p$salience_score_role, "threshold_attainment_descriptor")
  expect_lte(p$score, 1)
})

test_that("continuous PFA score cannot equal one merely because loading references are cleared", {
  # Directly exercise the scoring principle with realistic non-perfect geometry:
  # threshold descriptors can saturate while the raw loading/margin geometry cannot.
  recovery <- 1
  primary <- c(.70, .75, .80, .72, .77, .81)
  margin <- c(.50, .55, .60, .48, .58, .62)
  attainment <- pfa_harmonic_mean(c(
    recovery,
    mean(pmin(primary / .40, 1)),
    mean(pmin(margin / .20, 1))
  ))
  continuous <- pfa_harmonic_mean(c(recovery, mean(primary), mean(margin)))
  expect_equal(attainment, 1)
  expect_lt(continuous, 1)
})
