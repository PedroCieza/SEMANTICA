test_that("task-level seeds are invariant to load-balanced worker scheduling", {
  caller_kind <- RNGkind()
  caller_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(caller_kind))
    if (is.null(caller_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", caller_seed, envir = .GlobalEnv)
  }, add = TRUE)
  task_seeds <- c(104729L, 130363L, 155921L, 180503L, 205019L, 229939L)
  task_seed_scope <- SEMANTICA:::.semantica_with_task_seed
  evaluate <- function(seed) {
    task_seed_scope(
      seed,
      c(stats::runif(3L), stats::rnorm(2L))
    )
  }

  set.seed(8675309L, kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  serial_rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  serial <- lapply(task_seeds, evaluate)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    serial_rng_before
  )
  plan <- semantica_resource_plan(
    n.cores = 3L,
    max.cores = 3L
  )
  skip_if(plan$effective_workers < 2L, "Fewer than two workers are available")

  cluster <- SEMANTICA:::.semantica_make_cluster(plan)
  on.exit(SEMANTICA:::.semantica_stop_cluster(cluster), add = TRUE)
  worker_kinds <- parallel::clusterCall(cluster, RNGkind)
  expect_true(all(vapply(worker_kinds, identical, logical(1L), RNGkind())))
  export_environment <- new.env(parent = emptyenv())
  export_environment$.hidden_helper <- function(value) value + 1L
  exported <- SEMANTICA:::.semantica_cluster_export_environment(
    cluster, export_environment
  )
  hidden_results <- parallel::clusterEvalQ(cluster, .hidden_helper(1L))
  parallel_rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  load_balanced <- parallel::parLapplyLB(cluster, task_seeds, evaluate)

  expect_contains(exported, ".hidden_helper")
  expect_equal(unlist(hidden_results), rep(2L, plan$effective_workers))
  expect_identical(load_balanced, serial)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    parallel_rng_before
  )
})

test_that("DFI seed ledgers and caller RNG agree in serial and parallel", {
  skip_if_not_installed("lavaan")
  plan <- semantica_resource_plan(n.cores = 2L, max.cores = 2L)
  skip_if(plan$effective_workers < 2L, "Fewer than two workers are available")

  run_dfi <- function(n_cores) {
    set.seed(271828L, kind = "L'Ecuyer-CMRG")
    result <- SEMANTICA:::compute_dfi_by_simulation(
      factors = c("F1", "F2"),
      items_per_factor = c(F1 = 3L, F2 = 3L),
      n_obs = 200L,
      reps = 2L,
      n_cores = n_cores,
      verbose = FALSE,
      progress = FALSE
    )
    list(
      result = result,
      rng_state = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    )
  }

  serial <- run_dfi(1L)
  load_balanced <- run_dfi(2L)

  expect_false(is.null(serial$result))
  expect_false(is.null(load_balanced$result))
  expect_identical(serial$result$telemetry$simulation_input, "covariance")
  expect_identical(
    serial$result$telemetry$task_seeds,
    load_balanced$result$telemetry$task_seeds
  )
  expect_identical(load_balanced$rng_state, serial$rng_state)
  expect_equal(
    unlist(load_balanced$result[c("cfi", "tli", "rmsea", "srmr")]),
    unlist(serial$result[c("cfi", "tli", "rmsea", "srmr")]),
    tolerance = 1e-10
  )
})

test_that("failed worker RNG initialization cleans up without registering a pool", {
  skip_if_not_installed("parallelly")
  active <- SEMANTICA:::.semantica_pool_registry$active
  before <- ls(active, all.names = TRUE)
  skip_if(length(before) > 0L, "Another test owns a live worker pool")
  stopped <- FALSE
  local_mocked_bindings(makeClusterPSOCK = function(...) structure(list(), class = "cluster"),
    .package = "parallelly")
  local_mocked_bindings(clusterCall = function(...) stop("planned initialization failure"),
    .package = "parallel")
  local_mocked_bindings(.semantica_stop_psock_cluster = function(cluster) {
    stopped <<- TRUE
    invisible(TRUE)
  }, .package = "SEMANTICA")
  plan <- semantica_resource_plan(n.cores = 3L, max.cores = 3L)
  skip_if(plan$effective_workers < 2L, "Fewer than two workers are available")
  expect_error(SEMANTICA:::.semantica_make_cluster(plan), "initialize worker RNG algorithms")
  expect_true(stopped)
  expect_identical(ls(active, all.names = TRUE), before)
})

test_that("Conda setup rejects unsafe or ambiguous accelerator requests early", {
  expect_error(
    semantica_setup_conda(env_name = "base", force = TRUE, verbose = FALSE),
    "Refusing to remove"
  )
  expect_error(
    semantica_setup_conda(
      env_name = "semantica-test",
      accelerator = "cuda",
      torch_index_url = NULL,
      verbose = FALSE
    ),
    "requires 'torch_index_url'"
  )

  previous_active <- Sys.getenv("CONDA_DEFAULT_ENV", unset = NA_character_)
  on.exit({
    if (is.na(previous_active)) {
      Sys.unsetenv("CONDA_DEFAULT_ENV")
    } else {
      Sys.setenv(CONDA_DEFAULT_ENV = previous_active)
    }
  }, add = TRUE)
  Sys.setenv(CONDA_DEFAULT_ENV = "C:/conda/envs/SEMANTICA-TEST")
  expect_error(
    semantica_setup_conda(
      env_name = "semantica-test",
      force = TRUE,
      verbose = FALSE
    ),
    "currently active"
  )
})
