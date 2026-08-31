test_that("explicit worker requests are allocation-aware without auto reserve", {
  exact <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = 8L,
    available.cores = 12L,
    reserve.cores = 3L,
    warn = FALSE
  )
  expect_equal(exact$effective_workers, 8L)
  expect_equal(exact$reserve_cores_applied, 0L)

  capped_available <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = 8L,
    available.cores = 4L,
    warn = FALSE
  )
  expect_equal(capped_available$effective_workers, 4L)
  expect_equal(capped_available$limited_by, "available_workers")

  capped_user <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = 8L,
    available.cores = 12L,
    max.cores = 5L,
    warn = FALSE
  )
  expect_equal(capped_user$effective_workers, 5L)
  expect_equal(capped_user$limited_by, "max_cores")
})

test_that("auto workers apply reserve and optional maximum", {
  automatic <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = "auto",
    available.cores = 8L,
    reserve.cores = 2L,
    warn = FALSE
  )
  expect_equal(automatic$request_mode, "auto")
  expect_equal(automatic$available_after_reserve, 5L)
  expect_equal(automatic$effective_workers, 5L)
  expect_equal(automatic$reserve_cores_applied, 2L)
  expect_equal(automatic$coordinator_cores_applied, 1L)

  capped <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = "auto",
    available.cores = 8L,
    reserve.cores = 1L,
    max.cores = 4L,
    warn = FALSE
  )
  expect_equal(capped$effective_workers, 4L)

  minimum_one <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = "auto",
    available.cores = 1L,
    reserve.cores = 1L,
    warn = FALSE
  )
  expect_equal(minimum_one$effective_workers, 1L)
  expect_equal(minimum_one$reserve_cores_requested, 1L)
  expect_equal(minimum_one$reserve_cores_applied, 0L)
  expect_equal(minimum_one$coordinator_cores_applied, 0L)
})

test_that("auto workers budget the coordinator separately from user headroom", {
  plan <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = "auto",
    available.cores = 8L,
    reserve.cores = 1L,
    warn = FALSE
  )
  expect_equal(plan$coordinator_cores_applied, 1L)
  expect_equal(plan$reserve_cores_applied, 1L)
  expect_equal(plan$effective_workers, 6L)
  expect_lte(plan$effective_workers + plan$coordinator_cores_applied, 7L)
})

test_that("serial mode always resolves to one worker", {
  serial <- SEMANTICA:::.semantica_worker_resolution(
    n.cores = "auto",
    use_parallel = FALSE,
    reserve.cores = 4L,
    available.cores = 16L,
    warn = FALSE
  )
  expect_equal(serial$request_mode, "serial")
  expect_equal(serial$effective_workers, 1L)
  expect_equal(serial$limited_by, "use_parallel_false")
})

test_that("worker requests and limits are validated", {
  expect_error(
    SEMANTICA:::.semantica_worker_resolution(0L, available.cores = 8L),
    "n.cores"
  )
  expect_error(
    SEMANTICA:::.semantica_worker_resolution("many", available.cores = 8L),
    "n.cores"
  )
  expect_error(
    SEMANTICA:::.semantica_worker_resolution(2L, reserve.cores = -1L,
                                             available.cores = 8L),
    "reserve.cores"
  )
  expect_error(
    SEMANTICA:::.semantica_worker_resolution(2L, max.cores = 0L,
                                             available.cores = 8L),
    "max.cores"
  )
  expect_warning(
    value <- SEMANTICA:::.semantica_resolve_workers(
      8L, available.cores = 3L, warn = TRUE
    ),
    "Requested 8 parallel workers"
  )
  expect_equal(value, 3L)
})

test_that("compatibility worker helper no longer imposes a two-core cap", {
  expect_equal(
    SEMANTICA:::.semantica_max_workers(16L, available.cores = 12L),
    12L
  )
  expect_equal(
    SEMANTICA:::.semantica_max_workers(4L, available.cores = 12L),
    4L
  )
})

test_that("strict-CFA simulation fallback receives the configured workers", {
  captured_workers <- NA_integer_
  local_mocked_bindings(
    safe_compute_dfi = function(...) {
      args <- list(...)
      captured_workers <<- args[[18L]]
      list(
        cfi = 0.95, tli = 0.94, rmsea = 0.06, srmr = 0.06,
        was_degenerate = FALSE
      )
    },
    .package = "SEMANTICA"
  )

  SEMANTICA:::compute_dfi_cutoffs_from_model_spec(
    factors = c("F1", "F2"),
    items_per_factor = c(F1 = 3L, F2 = 3L),
    reps = 20L,
    n_cores = 7L,
    verbose = FALSE
  )

  expect_equal(captured_workers, 7L)
})

test_that("resource plans expose deterministic policy metadata", {
  local_mocked_bindings(
    .semantica_available_cores = function(omit = 0L) max(1L, 10L - omit),
    .semantica_available_physical_cores = function() 10L,
    .package = "SEMANTICA"
  )
  plan <- SEMANTICA:::semantica_resource_plan(
    n.cores = "auto",
    reserve.cores = 2L,
    max.cores = 6L,
    memory_aware = FALSE
  )
  expect_s3_class(plan, "semantica_resource_plan")
  expect_equal(plan$available_workers, 10L)
  expect_equal(plan$effective_workers, 6L)
  expect_equal(plan$parallel_backend, "PSOCK")
  expect_equal(plan$worker_blas_threads, 1L)
  expect_equal(unname(plan$worker_environment), rep("1", 5L))

  telemetry <- SEMANTICA:::.semantica_resource_telemetry(plan)
  expect_equal(telemetry$effective_workers, 6L)
  expect_equal(telemetry$workers_created, 0L)
})

test_that("worker BLAS policy does not mutate the main R session", {
  variable_names <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS"
  )
  before <- Sys.getenv(variable_names, unset = NA_character_)
  policy <- SEMANTICA:::.semantica_worker_env()
  after <- Sys.getenv(variable_names, unset = NA_character_)

  expect_named(policy, variable_names)
  expect_equal(unname(policy), rep("1", length(variable_names)))
  expect_identical(after, before)
})

test_that("cluster cleanup is guaranteed when managed work errors", {
  plan <- structure(
    list(effective_workers = 2L),
    class = c("semantica_resource_plan", "list")
  )
  stopped <- FALSE
  local_mocked_bindings(
    .semantica_make_cluster = function(resource_plan, ...) {
      structure(list(worker_1 = NULL, worker_2 = NULL), class = "cluster")
    },
    .semantica_stop_cluster = function(cluster) {
      stopped <<- TRUE
      invisible(TRUE)
    },
    .package = "SEMANTICA"
  )

  expect_error(
    SEMANTICA:::.semantica_with_cluster(plan, function(cluster) stop("planned failure")),
    "planned failure"
  )
  expect_true(stopped)
})

test_that("serial plans do not start a PSOCK cluster", {
  plan <- structure(
    list(effective_workers = 1L),
    class = c("semantica_resource_plan", "list")
  )
  expect_null(SEMANTICA:::.semantica_make_cluster(plan))
  expect_false(SEMANTICA:::.semantica_stop_cluster(NULL))
})

test_that("failed cluster shutdown retains the nesting guard", {
  active <- SEMANTICA:::.semantica_pool_registry$active
  token <- "unit_test_failed_stop"
  had_token <- exists(token, envir = active, inherits = FALSE)
  previous <- if (had_token) get(token, envir = active, inherits = FALSE) else NULL
  on.exit({
    if (had_token) {
      assign(token, previous, envir = active)
    } else if (exists(token, envir = active, inherits = FALSE)) {
      rm(list = token, envir = active)
    }
  }, add = TRUE)
  assign(token, TRUE, envir = active)
  cluster <- structure(list(), class = "cluster")
  attr(cluster, "semantica_pool_token") <- token

  local_mocked_bindings(
    .semantica_stop_psock_cluster = function(cluster) {
      stop("simulated shutdown failure")
    },
    .package = "SEMANTICA"
  )

  expect_false(SEMANTICA:::.semantica_stop_cluster(cluster))
  expect_true(exists(token, envir = active, inherits = FALSE))
})

test_that("stale registered pools are reaped only when liveness is demonstrably false", {
  skip_if_not_installed("parallelly")
  active <- SEMANTICA:::.semantica_pool_registry$active
  token <- "unit_test_stale_pool"
  had <- exists(token, envir = active, inherits = FALSE)
  previous <- if (had) get(token, envir = active, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(token, previous, envir = active)
    else if (exists(token, envir = active, inherits = FALSE)) rm(list = token, envir = active)
  }, add = TRUE)
  assign(
    token,
    list(cluster = structure(list(), class = "cluster"), owner_pid = Sys.getpid(), created_at = Sys.time()),
    envir = active
  )
  local_mocked_bindings(
    .semantica_pool_entry_alive = function(entry, timeout = 0.25) FALSE,
    .package = "SEMANTICA"
  )
  reaped <- SEMANTICA:::.semantica_reap_stale_pools()
  expect_true(token %in% reaped)
  expect_false(exists(token, envir = active, inherits = FALSE))
})

test_that("a demonstrably live SEMANTICA pool still blocks nested creation", {
  skip_if_not_installed("parallelly")
  active <- SEMANTICA:::.semantica_pool_registry$active
  token <- "unit_test_live_pool"
  had <- exists(token, envir = active, inherits = FALSE)
  previous <- if (had) get(token, envir = active, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(token, previous, envir = active)
    else if (exists(token, envir = active, inherits = FALSE)) rm(list = token, envir = active)
  }, add = TRUE)
  assign(
    token,
    list(cluster = structure(list(), class = "cluster"), owner_pid = Sys.getpid(), created_at = Sys.time()),
    envir = active
  )
  plan <- structure(list(effective_workers = 2L), class = c("semantica_resource_plan", "list"))
  local_mocked_bindings(
    .semantica_reap_stale_pools = function(timeout = 0.25) invisible(character()),
    .package = "SEMANTICA"
  )
  expect_error(
    SEMANTICA:::.semantica_make_cluster(plan),
    "already owns an active CPU worker pool"
  )
})
