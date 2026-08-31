test_that("ESEM state contract distinguishes request, failure and admissibility", {
  off <- semantica_esem_state(requested = FALSE)
  expect_identical(off$technical_state, "not_requested")

  failed <- semantica_esem_state(requested = TRUE, attempted = TRUE, esem_result = NULL,
                                 failure_reason = "solver failed")
  expect_identical(failed$technical_state, "attempted_failed")
  expect_match(failed$reason, "solver failed")

  inad <- semantica_esem_state(esem_result = list(
    converged = TRUE, admissible = FALSE,
    admissibility = list(details = list(converged = TRUE), reasons = "heywood")
  ))
  expect_identical(inad$technical_state, "converged_inadmissible")
  expect_identical(inad$structural_quality, "not_assessed")
})

test_that("admissible ESEM remains separate from structural quality", {
  mixed <- semantica_esem_state(esem_result = list(
    converged = TRUE, admissible = TRUE, htmt_violations = 1,
    structure_diagnostics = list(correct_dominance = .8, no_large_cross_loading = .7)
  ))
  expect_identical(mixed$technical_state, "admissible")
  expect_identical(mixed$structural_quality, "admissible_but_structurally_mixed")

  favorable <- semantica_esem_state(esem_result = list(
    converged = TRUE, admissible = TRUE, htmt_violations = 0,
    structure_diagnostics = list(correct_dominance = 1, no_large_cross_loading = 1)
  ))
  expect_identical(favorable$technical_state, "admissible")
  expect_match(favorable$structural_quality, "favorable")
})

test_that("inadmissible ESEM scoring result keeps derivative diagnostics unavailable", {
  x <- SEMANTICA:::extract_and_score_esem(NULL)
  expect_false(x$admissible)
  expect_true(is.na(x$htmt_max))
  expect_true(is.na(x$ave))
  expect_null(x$structure_diagnostics)
})
