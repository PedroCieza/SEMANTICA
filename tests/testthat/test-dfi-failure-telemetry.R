test_that("DFI failed calibration records are counted but unusable", {
  failed <- SEMANTICA:::.semantica_dfi_failed_result(
    dfi_function = "test DFI",
    cutoff_calibration = "test",
    failure_reason = "not enough usable refits",
    start_time = proc.time()[["elapsed"]],
    requested_reps = 10L,
    completed_reps = 4L,
    successful_results = list(list(fit_attempt = 1L)),
    task_seeds = 1:4,
    parallel_workers = 2L
  )

  expect_true(SEMANTICA:::.semantica_dfi_stage_failed(failed))
  expect_false(SEMANTICA:::.semantica_dfi_stage_usable(failed))
  expect_true(isTRUE(failed$was_degenerate))
  expect_equal(failed$telemetry$completed_reps, 4L)
  expect_equal(failed$telemetry$successful_fits, 1L)
  expect_equal(failed$telemetry$parallel_workers, 2L)
  expect_equal(failed$failure_reason, "not enough usable refits")
})

test_that("DFI adaptive batching can stop on futility without accepting cutoffs", {
  jobs <- as.list(seq_len(10L))
  run <- SEMANTICA:::.semantica_dfi_adaptive_batches(
    jobs = jobs,
    run_batch = function(batch, first_batch = FALSE) {
      lapply(batch, function(i) if (i == 1L) list(cfi = 0.9) else NULL)
    },
    estimate_cutoffs = function(results) NULL,
    enabled = TRUE,
    min_reps = 4L,
    batch_reps = 2L,
    allow_stability_stop = FALSE,
    futility_check = function(results, all_jobs) {
      if (length(results) >= 4L) {
        list(stop = TRUE, reason = "test futility")
      } else {
        list(stop = FALSE)
      }
    }
  )

  expect_equal(length(run$results), 4L)
  expect_true(run$telemetry$stopped_early)
  expect_true(run$telemetry$futility_stopped)
  expect_equal(run$telemetry$stop_reason, "test futility")
})

test_that("strict search fallback respects the ESEM search calibration budget", {
  expect_equal(SEMANTICA:::.semantica_strict_search_reps(500L, 50L), 50L)
  expect_equal(SEMANTICA:::.semantica_strict_search_reps(10L, 5L), 20L)
  expect_equal(SEMANTICA:::.semantica_strict_search_reps(80L, NA_integer_), 80L)
})

test_that("population syntax retains its exact factor, residual, and correlation order", {
  common_args <- list(
    items_per_factor = c(A = 2L, B = 2L),
    loading_pattern = "uniform",
    mean_loading = 0.7,
    target_factor_cors = 0.25
  )

  simulation <- do.call(
    SEMANTICA:::build_population_syntax_modelbased,
    c(common_args, list(syntax_mode = "simulation"))
  )
  dfi_package <- do.call(
    SEMANTICA:::build_population_syntax_modelbased,
    c(common_args, list(syntax_mode = "dfi_package"))
  )

  expect_identical(simulation, paste(c(
    "F1 =~ 0.700*x1 + 0.700*x2",
    "x1 ~~ 0.5100*x1",
    "x2 ~~ 0.5100*x2",
    "F1 ~~ 1*F1",
    "F2 =~ 0.700*x3 + 0.700*x4",
    "x3 ~~ 0.5100*x3",
    "x4 ~~ 0.5100*x4",
    "F2 ~~ 1*F2",
    "F1 ~~ 0.250*F2"
  ), collapse = "\n"))
  expect_identical(dfi_package, paste(c(
    "F1 =~ 0.700*x1 + 0.700*x2",
    "F2 =~ 0.700*x3 + 0.700*x4",
    "F1 ~~ 0.250*F2"
  ), collapse = "\n"))
})

test_that("bounded strict search fallback passes its simulation budget downstream", {
  captured <- list()
  local_mocked_bindings(
    safe_compute_dfi = function(...) {
      args <- list(...)
      captured$reps <<- args$reps
      captured$sim_reps <<- args$sim_reps
      list(cfi = 0.95, tli = 0.94, rmsea = 0.04, srmr = 0.05, was_degenerate = FALSE)
    },
    .package = "SEMANTICA"
  )

  out <- SEMANTICA:::compute_dfi_cutoffs_from_model_spec(
    factors = c("F1", "F2"),
    items_per_factor = c(F1 = 3L, F2 = 3L),
    reps = 50L,
    sim_reps = 50L,
    prefer_simulation = TRUE,
    verbose = FALSE
  )

  expect_equal(captured$reps, 50L)
  expect_equal(captured$sim_reps, 50L)
  expect_equal(out$dfi_requested_reps, 50L)
  expect_equal(out$dfi_simulation_reps, 50L)
})

test_that("ordinary strict DFI retains the established simulation floor", {
  captured <- list()
  local_mocked_bindings(
    safe_compute_dfi = function(...) {
      args <- list(...)
      captured$sim_reps <<- args$sim_reps
      list(cfi = 0.95, tli = 0.94, rmsea = 0.04, srmr = 0.05, was_degenerate = FALSE)
    },
    .package = "SEMANTICA"
  )

  SEMANTICA:::compute_dfi_cutoffs_from_model_spec(
    factors = c("F1", "F2"),
    items_per_factor = c(F1 = 3L, F2 = 3L),
    reps = 50L,
    verbose = FALSE
  )

  expect_equal(captured$sim_reps, 200L)
})
