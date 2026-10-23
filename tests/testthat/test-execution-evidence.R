test_that("equivalence diagnostics retain nonfinite outcomes and dependency warnings", {
  skip_if_not_installed("dynamic")
  local_mocked_bindings(equivTest = function(...) {
    warning("boundary calculation")
    list(cfi = "NaN", rmsea = "NaN")
  }, .package = "dynamic")
  x <- SEMANTICA:::compute_equivtest_final_diagnostic(list(), verbose = FALSE)
  expect_identical(x$status, "unavailable")
  expect_identical(x$reason, "nonfinite_equivalence_statistics")
  expect_equal(nrow(x$warning_events), 1L)
  expect_identical(x$warning_events$message, "boundary calculation")
  expect_identical(x$raw_result$cfi, "NaN")
})

test_that("equivalence diagnostic errors and partial results have explicit states", {
  skip_if_not_installed("dynamic")
  local_mocked_bindings(equivTest = function(...) stop("dependency error"), .package = "dynamic")
  x <- SEMANTICA:::compute_equivtest_final_diagnostic(list(), verbose = FALSE)
  expect_identical(x$status, "unavailable")
  expect_identical(x$reason, "dependency error")
  local_mocked_bindings(equivTest = function(...) list(cfi = ".01", rmsea = "NaN"), .package = "dynamic")
  expect_identical(SEMANTICA:::compute_equivtest_final_diagnostic(list(), FALSE)$status, "partial")
})

test_that("active calibration evidence works for semantic and parametric paths", {
  for (method in c("semantic_approx", "semantic_roc", "esem_parametric")) {
    cutoffs <- list(cfi = .95, tli = .94, rmsea = .05, srmr = .06,
      telemetry = list(requested_reps = 150L, completed_reps = 100L,
        successful_fits = 90L, failed_fits = 10L,
        adaptive = list(stop_reason = "stable"),
        execution = list(completion_backend = "parallel", recovery_events = list())))
    x <- SEMANTICA:::.semantica_calibration_telemetry(cutoffs, method)
    expect_identical(x$completed_reps, 100L)
    expect_identical(x$successful_fits, 90L)
    expect_identical(x$failed_fits, 10L)
    expect_identical(x$execution_backend, "parallel")
    expect_identical(x$stop_reason, "stable")
  }
  expect_identical(SEMANTICA:::.semantica_calibration_telemetry(list(), "heuristic")$status, "not_requested")
  expect_true(is.na(SEMANTICA:::.semantica_calibration_telemetry(list(), "heuristic")$completed_reps))
})

test_that("solver states and broker warnings distinguish rejection from absence", {
  expect_identical(SEMANTICA:::.semantica_solver_state(NULL, list(list()), list(list(converged = TRUE))), "converged_inadmissible")
  expect_identical(SEMANTICA:::.semantica_solver_state(NULL, list(list()), list(list(converged = FALSE))), "nonconverged")
  expect_identical(SEMANTICA:::.semantica_solver_state(NULL, list(), list()), "input_rejected")
  broker <- SEMANTICA:::.semantica_new_evaluation_broker()
  fit <- list(converged = TRUE, admissible = FALSE,
    solver_records = list(list(attempt = 1L, warnings = "negative variance", warning_calls = "lavaan()")))
  SEMANTICA:::.semantica_append_esem_event(broker, "search", "candidate", fit_result = fit)
  SEMANTICA:::.semantica_append_esem_event(broker, "search", "candidate", fit_result = fit, cache_hit = TRUE)
  w <- SEMANTICA:::.semantica_evaluation_snapshot(broker)$warning_events
  expect_equal(nrow(w), 1L)
  expect_identical(w$message, "negative variance")
  expect_identical(w$call, "lavaan()")
})

test_that("an optional nonfinite diagnostic is visible without rejecting a proper scale", {
  skip_if_not_installed("dynamic")
  local_mocked_bindings(equivTest = function(...) {
    warning("boundary diagnostic")
    list(cfi = "NaN", rmsea = "NaN")
  }, .package = "dynamic")
  ids <- paste0("control", 1:240); factors <- paste0("F", 1:6)
  lambda <- matrix(.025, 240, 6)
  lambda[cbind(1:240, rep(1:6, each = 40))] <- rep(rep(c(.74,.77,.80,.83),each=10),6)
  phi <- matrix(.15,6,6); diag(phi) <- 1
  shared <- lambda %*% phi %*% t(lambda)
  covariance <- shared + diag(1-diag(shared)); dimnames(covariance) <- list(ids,ids)
  x <- ACO_with_ESEM(covariance,
    data.frame(item = ids, factor = rep(factors, each = 40),
      facet = rep(rep(paste0("facet",1:4),each=10),6)),
    i.per.f = setNames(rep(4L,6),factors), ants = 2L, max_total_iter = 1L,
    max_esem_fits = 2L, dfi_mode = "heuristic_semantic", pfa_mode = "off",
    run_pfa_during_search = FALSE, esem_every = 1L,
    facet_constraint_mode = "presence", fast_esem = FALSE,
    psychometric_guard_action = "final_constraint", cohesion_retention = 1,
    semantic_n_sensitivity = FALSE, final_equivtest = TRUE,
    final_dddfi = FALSE, validation_n_diagnostic = FALSE,
    use_parallel = FALSE, seed = 731029L, verbose = FALSE)
  expect_true(x$esem_admissible)
  expect_true(x$structural_guard$passed)
  expect_identical(x$final_equivtest_diagnostic$status, "unavailable")
  expect_true(any(grepl("equivalence-test diagnostic unavailable", x$summary$warnings)))
  expect_true(any(x$warning_events$message == "boundary diagnostic"))
})

test_that("native validation errors retain a compact invocation call", {
  e <- tryCatch(ACO_with_ESEM(diag(2), data.frame(item = c("a","b"), factor = c("A","B")),
    i.per.f = c(A=0,B=0), verbose = FALSE), error = identity)
  expect_s3_class(e, "error")
  expect_match(conditionMessage(e), "rownames and colnames")
  expect_lt(nchar(paste(deparse(conditionCall(e)), collapse = " ")), 500L)
  expect_false(identical(conditionCall(e)[[1L]], quote(withCallingHandlers)))
})
