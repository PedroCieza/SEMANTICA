test_that("PFA/ESEM discrepancy preserves complementary evidence without averaging", {
  pfa <- list(
    available = TRUE, recovery_score = 1, factor_presence_recovery = 1,
    missing_factors = character(0), clarity_score = .9, partition_agreement_ari = .8
  )
  esem <- list(
    admissible = TRUE, htmt_max = .92, htmt_violations = 1,
    structure_diagnostics = list(correct_dominance = 1, no_large_cross_loading = .75)
  )
  x <- semantica_pfa_esem_discrepancy(pfa, esem)
  expect_identical(x$state, "grouping_recoverable__separation_weak")
  expect_false(x$participant_based)
  expect_null(x$score)
  expect_true(x$structural_flags[["htmt_overlap"]])
})

test_that("PFA/ESEM discrepancy distinguishes favorable and unavailable ESEM states", {
  pfa <- list(available = TRUE, recovery_score = 1, missing_factors = character(0))
  favorable <- semantica_pfa_esem_discrepancy(pfa, list(
    admissible = TRUE, htmt_violations = 0,
    structure_diagnostics = list(correct_dominance = 1, no_large_cross_loading = 1)
  ))
  expect_identical(favorable$state, "grouping_recoverable__esem_favorable")

  unavailable <- semantica_pfa_esem_discrepancy(pfa, list(admissible = FALSE))
  expect_match(unavailable$state, "esem_unavailable")
})
