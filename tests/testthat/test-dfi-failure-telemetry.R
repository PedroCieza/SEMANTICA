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

test_that("native DFI tasks preserve diagnostic envelopes without extra fitter calls", {
  calls <- 0L
  local_mocked_bindings(sample_correlation_from_population = function(population, n_obs) population,
    run_esem_on_matrix = function(..., return_diagnostics) {
      calls <<- calls + 1L
      expect_true(return_diagnostics)
      envelope
    }, .package = "SEMANTICA")
  local_mocked_bindings(lavInspect = function(fit, what) fit$converged,
    fitMeasures = function(fit, ...) fit$metrics, .package = "lavaan")
  metrics <- c(cfi = .95, tli = .94, rmsea = .04, srmr = .05)
  rejected <- list(attempt = 1L, converged = FALSE, reasons = "nonconvergence")
  population <- diag(4L); dimnames(population) <- list(letters[1:4], letters[1:4])
  evaluate <- function() SEMANTICA:::.semantica_dfi_esem_task(131L, population,
    letters[1:4], "model", 1000L, "ML", "geomin", list(), 1000L)
  set.seed(29L); before <- .Random.seed
  for (reason in c("success", "nonconvergence", "negative_residual_variance", "solver_exception", "nonfinite")) {
    fit <- if (reason %in% c("success", "nonfinite")) list(converged = TRUE,
      metrics = if (reason == "nonfinite") replace(metrics, 1L, NA_real_) else metrics) else NULL
    if (!is.null(fit)) attr(fit, "semantica_fit_attempt") <- 2L
    envelope <- list(fit = fit, solver_attempts_started = 2L,
      accepted_attempt = if (is.null(fit)) NA_integer_ else 2L,
      rejection_assessment = if (is.null(fit)) list(reasons = reason) else NULL,
      rejected_attempts = list(rejected),
      solver_records = list(list(attempt = 1L, optimizer = "default", warnings = "recorded warning"),
        list(attempt = 2L, optimizer = "BFGS", error = if (reason == "solver_exception") "failed" else NULL)))
    record <- evaluate()
    expect_identical(.Random.seed, before)
    expect_identical(record$fit_diagnostics$seed, 131L)
    expect_equal(record$fit_diagnostics$solver_attempts_started, 2L)
    expect_length(record$fit_diagnostics$solver_records, 2L)
    if (reason == "success") {
      expect_identical(unlist(record$value[names(metrics)]), metrics)
      expect_identical(record$fit_diagnostics$outcome, "success")
      expect_equal(record$fit_diagnostics$accepted_attempt, 2L)
    } else {
      expect_null(record$value)
      expect_true(reason %in% record$fit_diagnostics$reasons || reason == "nonfinite")
    }
    path <- tempfile(fileext = ".rds"); saveRDS(record, path)
    expect_identical(readRDS(path), record); unlink(path)
  }
  expect_equal(calls, 5L)
  local_mocked_bindings(sample_correlation_from_population = function(...) NULL, .package = "SEMANTICA")
  expect_identical(evaluate()$failure_category, "sampling_failed")
  expect_equal(calls, 5L)
})

test_that("semantic calibration retains all failures, cache records and later-batch recoveries", {
  # Real reference fit only supplies the existing implied covariance boundary.
  population <- matrix(.4, 4L, 4L); diag(population) <- 1
  dimnames(population) <- list(letters[1:4], letters[1:4])
  fit <- suppressWarnings(lavaan::cfa("F =~ a+b+c+d", sample.cov = population, sample.nobs = 500L))
  stopifnot(lavaan::lavInspect(fit, "converged"))
  for (successful in c(TRUE, FALSE)) {
    calls <- 0L; stopped <- 0L
    local_mocked_bindings(
      .semantica_cluster_export_environment = function(...) invisible(NULL),
      .semantica_stop_cluster = function(...) { stopped <<- stopped + 1L },
      .semantica_dfi_esem_task = function(seed, ...) {
        ok <- successful || seed %% 2L == 0L
        list(value = if (ok) list(cfi = .95, tli = .94, rmsea = .04, srmr = .05, fit_attempt = 1L) else NULL,
          failure_category = if (!ok) "fit_failed_or_nonconverged" else NULL,
          fit_diagnostics = list(seed = seed, outcome = if (ok) "success" else "failed",
            reasons = if (ok) character() else "negative_residual_variance"))
      }, .package = "SEMANTICA")
    local_mocked_bindings(parLapplyLB = function(cl, x, fun) {
      calls <<- calls + 1L
      if (calls == 2L) stop("later adaptive batch lost connection")
      lapply(x, fun)
    }, .package = "parallel")
    cache <- SEMANTICA:::.semantica_new_dfi_cache()
    args <- list(esem_fit = fit, esem_syntax = "F =~ a+b+c+d", factors = "F",
      items_per_factor = c(F = 4L), observed_cor = population, reps = 30L,
      adaptive = TRUE, adaptive_min_reps = 20L, adaptive_batch_reps = 5L,
      adaptive_stable_batches = 100L, cluster = list(NULL, NULL),
      n_cores = 2L, progress = FALSE, verbose = FALSE, cache = cache)
    set.seed(631L); expected_seeds <- sample.int(.Machine$integer.max, 30L)
    set.seed(631L)
    out <- do.call(SEMANTICA:::compute_semantic_approx_dfi_cutoffs, args)
    expected_rng <- .Random.seed
    set.seed(631L); invisible(sample.int(.Machine$integer.max, 30L))
    expect_identical(expected_rng, .Random.seed)
    execution <- out$telemetry$execution
    expect_equal(execution$retained_parallel_results, 20L)
    expect_equal(execution$retained_serial_results, 10L)
    expect_identical(execution$completion_backend, "mixed")
    expect_length(execution$recovery_events, 1L)
    expect_equal(execution$recovery_events[[1L]]$tasks[[1L]]$replication, 21L)
    expect_identical(out$telemetry$task_seeds, expected_seeds)
    expect_equal(stopped, 1L) # Borrowed pool: fallback owns its existing stop.
    diagnostics <- out$simulation_provenance$fit_diagnostics
    expect_identical(names(diagnostics), as.character(seq_len(30L)))
    expect_identical(vapply(diagnostics, `[[`, integer(1L), "seed"), setNames(expected_seeds, names(diagnostics)))
    expected_success <- if (successful) 30L else sum(expected_seeds %% 2L == 0L)
    expect_equal(out$successful_fits, expected_success)
    expect_identical(isTRUE(out$calibration_failed), !successful)
    if (successful) expect_equal(unlist(out[c("cfi", "tli", "rmsea", "srmr")]),
      c(cfi = .95, tli = .94, rmsea = .04, srmr = .05))
    cached <- do.call(SEMANTICA:::compute_semantic_approx_dfi_cutoffs, args)
    expect_true(cached$telemetry$cache_hit)
    expect_identical(cached$telemetry$execution, execution)
    path <- tempfile(fileext = ".rds"); saveRDS(out, path)
    expect_identical(readRDS(path)$simulation_provenance, out$simulation_provenance); unlink(path)
  }
})

test_that("unexpected native DFI task exceptions remain terminal", {
  local_mocked_bindings(sample_correlation_from_population = function(...) stop("programming defect"),
    .package = "SEMANTICA")
  expect_error(SEMANTICA:::.semantica_dfi_esem_task(1L, diag(3), letters[1:3], "model",
    1000L, "ML", "geomin", list(), 1000L), "programming defect")
})

test_that("other native ESEM routes keep diagnostic records on success and failure", {
  population <- matrix(.4, 4L, 4L); diag(population) <- 1
  dimnames(population) <- list(letters[1:4], letters[1:4])
  fit <- suppressWarnings(lavaan::cfa("F =~ a+b+c+d", sample.cov = population, sample.nobs = 500L))
  local_mocked_bindings(
    make_semantic_approx_population = function(...) list(approx_cor = population, observed_cor = population,
      item_names = letters[1:4], residual_cap_quantile = .95, residual_q95 = 0,
      residual_max = 0, embed_reliability = 1),
    make_semantic_misspecified_population = function(...) population,
    .semantica_dfi_esem_task = function(seed, ..., kind = NULL) list(
      value = if (successful) c(if (!is.null(kind)) list(kind = kind),
        list(cfi = .95, tli = .94, rmsea = .04, srmr = .05, fit_attempt = 1L)) else NULL,
      fit_diagnostics = list(seed = seed, branch = kind, outcome = if (successful) "success" else "failed",
        reasons = if (successful) character() else "unknown")), .package = "SEMANTICA")
  for (successful in c(TRUE, FALSE)) for (route in c("compute_esem_parametric_dfi_cutoffs", "compute_semantic_roc_dfi_cutoffs")) {
    args <- list(esem_fit = fit, esem_syntax = "F =~ a+b+c+d", factors = "F",
      items_per_factor = c(F = 4L), n_cores = 1L, reps = 40L, verbose = FALSE)
    if (grepl("roc", route)) args$observed_cor <- population
    set.seed(811L)
    out <- do.call(getFromNamespace(route, "SEMANTICA"), args)
    diagnostics <- out$simulation_provenance$fit_diagnostics
    expect_length(diagnostics, out$telemetry$completed_reps)
    expect_identical(isTRUE(out$calibration_failed), !successful)
    expect_identical(out$telemetry$execution$completion_backend, "serial")
    expect_equal(out$telemetry$execution$retained_serial_results, out$telemetry$completed_reps)
    if (grepl("roc", route)) expect_equal(vapply(diagnostics[1:2], `[[`, character(1L), "branch"),
      c("1" = "acceptable", "2" = "misspecified"))
  }
})

test_that("model-based failed telemetry preserves the established heuristic fallback", {
  execution <- list(schema_version = "semantica-dfi-execution-1", completion_backend = "serial",
    retained_serial_results = 20L, retained_parallel_results = 0L, recovery_events = list())
  failed <- SEMANTICA:::.semantica_dfi_failed_result("test", "model-based", "no successful fits",
    proc.time()[["elapsed"]], 20L, 20L, execution = execution)
  local_mocked_bindings(compute_dfi_by_simulation = function(...) failed, .package = "SEMANTICA")
  expected <- SEMANTICA:::compute_heuristic_cutoffs(2L, c(A = 3L, B = 3L), 1000L)
  out <- SEMANTICA:::safe_compute_dfi(model_syntax = "A =~ a+b+c\nB =~ d+e+f", factors = c("A", "B"),
    items_per_factor = c(A = 3L, B = 3L), n_obs = 1000L, prefer_simulation = TRUE, verbose = FALSE)
  expect_identical(out[c("cfi", "tli", "rmsea", "srmr")], expected[c("cfi", "tli", "rmsea", "srmr")])
  expect_identical(out$simulation_failure, failed)
  expect_identical(out$telemetry$execution, execution)
})
