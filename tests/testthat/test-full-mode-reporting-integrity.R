test_that("shared DFI calibration is not reported as a bootstrap failure", {
  expect_false(SEMANTICA:::.semantica_bootstrap_esem_unavailable(
    requested_esem_search = TRUE, dfi_enabled = TRUE,
    bootstrap_params = NULL, shared_calibration = TRUE
  ))
  expect_true(SEMANTICA:::.semantica_bootstrap_esem_unavailable(
    requested_esem_search = TRUE, dfi_enabled = TRUE,
    bootstrap_params = NULL, shared_calibration = FALSE
  ))
})

test_that("DFI completion output distinguishes an adaptive stop from failed fits", {
  msg <- SEMANTICA:::.semantica_dfi_completion_message(
    "ESEM-DFI",
    list(completed_reps = 450L, failed_fits = 0L,
         adaptive = list(stopped_early = TRUE)),
    requested_reps = 500L, successful_fits = 450L
  )
  expect_identical(
    msg,
    "[ESEM-DFI] completed refits = 450 / 500 requested (adaptive stop) | successful fits = 450 | failed fits = 0"
  )
})

test_that("phase-three summary separates structural guards from fit references", {
  out <- capture.output(SEMANTICA:::print_semantica_phase3_summary(list(
    esem_result = list(admissible = TRUE, cfi = .95, tli = .94, rmsea = .07, srmr = .06),
    active_cutoffs = list(cfi = .96, tli = .95, rmsea = .06, srmr = .05),
    proxy_quality = list(status = "proxy_guards_passed", reasons = character()),
    best_items = character(), factor_assignment = character(),
    esem_attempts = 0L, esem_failures = 0L, esem_successes = 0L
  )))
  expect_true(any(grepl("Global-fit context", out, fixed = TRUE)))
})
