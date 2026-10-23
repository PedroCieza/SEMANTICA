test_that("the public NULL adaptive minimum reaches the declared refit batch", {
  config <- semantica_fit_calibration_config(
    mode = "semantic_approx_dfi", strategy = "adaptive", esem_reps = 150L)
  expect_null(config$adaptive_min_reps)
  sizes <- integer()
  run <- .semantica_dfi_adaptive_batches(seq_len(150L),
    function(batch, first_batch = FALSE) {
      sizes <<- c(sizes, length(batch))
      lapply(batch, function(i) list(cfi = .95, rmsea = .05))
    }, function(results) list(cfi = .95, rmsea = .05),
    enabled = identical(config$strategy, "adaptive"), min_reps = config$adaptive_min_reps,
    batch_reps = config$adaptive_batch_reps, cutoff_tol = config$adaptive_tol,
    stable_batches = config$adaptive_stable_batches)
  expect_identical(sizes, 150L)
  expect_length(run$results, 150L)
  expect_equal(run$telemetry$min_reps, 150L)
  expect_false(run$telemetry$stopped_early)
})

test_that("automatic minimum preserves job order and complete budgets above the minimum", {
  sizes <- integer()
  first <- logical()
  jobs <- as.list(seq_len(501L))
  run <- .semantica_dfi_adaptive_batches(jobs,
    function(batch, first_batch = FALSE) {
      sizes <<- c(sizes, length(batch))
      first <<- c(first, first_batch)
      batch
    }, function(results) NULL, enabled = TRUE)
  expect_identical(sizes, c(251L, 50L, 50L, 50L, 50L, 50L))
  expect_identical(first, c(TRUE, rep(FALSE, 5L)))
  expect_identical(run$results, jobs)
  expect_equal(run$telemetry$completed_reps, 501L)
  expect_equal(run$telemetry$min_reps, 251L)
  expect_false(run$telemetry$stopped_early)
})

test_that("default and explicit full budgets produce the same results and consume no extra RNG", {
  caller_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (is.null(caller_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", caller_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(20261013L)
  jobs <- sample.int(.Machine$integer.max, 150L)
  before <- .Random.seed
  refit <- function(batch, first_batch = FALSE) {
    lapply(batch, function(seed) .semantica_with_task_seed(seed, stats::runif(4L)))
  }
  cutoffs <- function(results) {
    values <- colMeans(do.call(rbind, results))
    as.list(stats::setNames(values, c("cfi", "tli", "rmsea", "srmr")))
  }
  default <- .semantica_dfi_adaptive_batches(jobs, refit, cutoffs, enabled = TRUE)
  expect_identical(.Random.seed, before)
  explicit <- .semantica_dfi_adaptive_batches(jobs, refit, cutoffs,
    enabled = TRUE, min_reps = 150L)
  fixed <- .semantica_dfi_adaptive_batches(jobs, refit, cutoffs, enabled = FALSE)
  expect_identical(default$results, explicit$results)
  expect_identical(default$results, fixed$results)
  expect_identical(cutoffs(default$results), cutoffs(fixed$results))
  expect_identical(.Random.seed, before)
})

test_that("explicit adaptive minima and stability stopping retain their behavior", {
  sizes <- integer()
  run <- .semantica_dfi_adaptive_batches(seq_len(100L),
    function(batch, first_batch = FALSE) {
      sizes <<- c(sizes, length(batch)); lapply(batch, function(i) list(cfi = .95))
    }, function(results) list(cfi = .95, tli = .95, rmsea = .05, srmr = .05), enabled = TRUE,
    min_reps = 40L, batch_reps = 20L, stable_batches = 2L)
  expect_identical(sizes, c(40L, 20L, 20L))
  expect_length(run$results, 80L)
  expect_equal(run$telemetry$min_reps, 40L)
  expect_identical(run$telemetry$stop_reason, "stable_cutoffs")
})

test_that("missing controls and all unsuccessful refits terminate with telemetry", {
  for (minimum in list(NULL, NA_integer_, integer(), 0L)) {
    run <- .semantica_dfi_adaptive_batches(seq_len(25L),
      function(batch, first_batch = FALSE) lapply(batch, function(i) NULL),
      function(results) NULL, enabled = TRUE, min_reps = minimum,
      batch_reps = NULL, cutoff_tol = NULL, stable_batches = NULL)
    expect_length(run$results, 25L)
    expect_true(all(vapply(run$results, is.null, logical(1L))))
    expect_equal(run$telemetry$completed_reps, 25L)
    expect_false(run$telemetry$stopped_early)
  }
})

test_that("non-adaptive batches and empty job sets keep their execution contract", {
  batches <- 0L
  run <- .semantica_dfi_adaptive_batches(seq_len(7L),
    function(batch, first_batch = FALSE) { batches <<- batches + 1L; as.list(batch) },
    function(results) stop("non-adaptive path must not estimate intermediate cutoffs"),
    enabled = FALSE)
  expect_equal(batches, 1L)
  expect_identical(run$results, as.list(seq_len(7L)))
  expect_false(run$telemetry$enabled)
  empty <- .semantica_dfi_adaptive_batches(integer(),
    function(batch, first_batch = FALSE) list(), function(results) NULL, enabled = TRUE)
  expect_length(empty$results, 0L)
  expect_equal(empty$telemetry$completed_reps, 0L)
})

test_that("empty adaptive controls fail with informative errors at the ACO entry point", {
  args <- list(cosine_sim_matrix = diag(6L), i.per.f = c(A = 2L, B = 2L),
    ants = 1L, max.iter = 1L, use_parallel = FALSE, verbose = FALSE)
  controls <- c("dfi_adaptive_min_reps", "dfi_adaptive_batch_reps",
    "dfi_adaptive_tol", "dfi_adaptive_stable_batches")
  for (control in controls) {
    for (empty in list(integer(), numeric())) {
      invalid <- args
      invalid[[control]] <- empty
      expect_error(do.call(ACO_with_ESEM, invalid), control, fixed = TRUE)
    }
  }
})
