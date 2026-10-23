test_that("load-balanced ESEM dispatch falls back to seeded serial retry", {
  bad_cluster <- structure(list(worker_1 = NULL), class = "cluster")
  fallback_message <- NULL
  tasks <- list(
    first = list(seed = 104729L, offset = 1),
    second = list(seed = 130363L, offset = 2)
  )
  serial_eval <- function(task) {
    SEMANTICA:::.semantica_with_task_seed(
      task$seed,
      stats::runif(2L) + task$offset
    )
  }

  set.seed(8675309L)
  rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expected <- lapply(tasks, serial_eval)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    rng_before
  )

  out <- SEMANTICA:::.semantica_par_lapply_lb_or_serial(
    bad_cluster,
    tasks,
    fun = function(task) stop("unreachable worker computation"),
    serial_fun = serial_eval,
    on_parallel_error = function(error) {
      fallback_message <<- conditionMessage(error)
    }
  )

  expect_identical(out, expected)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    rng_before
  )
  expect_match(fallback_message, "sendData|connection|cluster|NULL")
})

test_that("progress DFI dispatch falls back to seeded serial retry", {
  bad_cluster <- structure(list(worker_1 = NULL), class = "cluster")
  fallback_message <- NULL
  tasks <- list(
    first = list(seed = 104729L, offset = 1),
    second = list(seed = 130363L, offset = 2),
    third = list(seed = 155921L, offset = 3),
    fourth = list(seed = 180503L, offset = 4)
  )
  serial_eval <- function(task) {
    SEMANTICA:::.semantica_with_task_seed(
      task$seed,
      stats::runif(2L) + task$offset
    )
  }

  set.seed(8675309L)
  rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expected <- lapply(tasks, serial_eval)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    rng_before
  )

  utils::capture.output({
    out <- SEMANTICA:::.semantica_progress_par_lapply(
      bad_cluster,
      tasks,
      fun = function(task) stop("unreachable worker computation"),
      serial_fun = serial_eval,
      progress = TRUE,
      on_parallel_error = function(error) {
        fallback_message <<- conditionMessage(error)
      }
    )
  })

  expect_identical(out, expected)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    rng_before
  )
  expect_match(fallback_message, "sendData|connection|conex|cluster|NULL", ignore.case = TRUE)
})

test_that("DFI export failure disables a stale cluster", {
  bad_cluster <- structure(list(worker_1 = NULL), class = "cluster")
  export_env <- new.env(parent = emptyenv())
  export_env$value <- 1L

  expect_message(
    cl <- SEMANTICA:::.semantica_dfi_export_or_serial(
      bad_cluster, export_env, verbose = TRUE, label = "[TEST-DFI]"
    ),
    "parallel worker communication failed"
  )
  expect_null(cl)
})

test_that("DFI observers retain chunks, task identities and RNG across recovery modes", {
  for (fail_at in c(1L, 2L)) for (progress in c(FALSE, TRUE)) {
    calls <- 0L; evaluated <- integer(); errors <- 0L
    local_mocked_bindings(parLapplyLB = function(cl, x, fun) {
      calls <<- calls + 1L
      if (calls == fail_at) stop("injected connection failure")
      lapply(x, fun)
    }, .package = "parallel")
    tasks <- as.list(seq_len(12L))
    evaluate <- function(seed) {
      evaluated <<- c(evaluated, seed)
      SEMANTICA:::.semantica_with_task_seed(seed, runif(2L))
    }
    expected <- lapply(tasks, function(seed) SEMANTICA:::.semantica_with_task_seed(seed, runif(2L)))
    cl <- list(NULL, NULL)
    state <- SEMANTICA:::.semantica_dfi_execution(2L, cl, tasks)
    set.seed(717L); before <- .Random.seed
    capture.output(out <- SEMANTICA:::.semantica_progress_par_lapply(
      cl, tasks, evaluate, progress = progress,
      observer = SEMANTICA:::.semantica_dfi_execution_observer(state),
      on_parallel_error = function(error) { errors <<- errors + 1L }))
    expect_identical(out, expected)
    expect_identical(.Random.seed, before)
    expect_identical(evaluated, seq_len(12L))
    snapshot <- SEMANTICA:::.semantica_dfi_execution_snapshot(state)
    expect_equal(snapshot$retained_parallel_results + snapshot$retained_serial_results, 12L)
    should_fail <- progress || fail_at == 1L
    expect_length(snapshot$recovery_events, as.integer(should_fail))
    expect_equal(errors, as.integer(should_fail))
    if (should_fail) {
      event <- snapshot$recovery_events[[1L]]
      completed <- if (progress && fail_at == 2L) 2L else 0L
      expect_equal(event$retained_completed_tasks, completed)
      expect_equal(vapply(event$tasks, `[[`, integer(1L), "seed"), seq.int(completed + 1L, 12L))
      expect_identical(event$stage, "dispatch")
      expect_true(event$serial_recovery_scheduled)
    }
  }
})

test_that("export observers record recovery and stop the cluster once", {
  stopped <- 0L
  local_mocked_bindings(
    .semantica_cluster_export_environment = function(...) stop("export unavailable"),
    .semantica_stop_cluster = function(cl) { stopped <<- stopped + 1L }, .package = "SEMANTICA")
  jobs <- list(list(seed = 1L, kind = "acceptable"), list(seed = 1L, kind = "misspecified"))
  state <- SEMANTICA:::.semantica_dfi_execution(3L, list(NULL), jobs)
  out <- SEMANTICA:::.semantica_dfi_export_or_serial(list(NULL), new.env(), FALSE, "test",
    observer = SEMANTICA:::.semantica_dfi_execution_observer(state))
  expect_null(out)
  expect_equal(stopped, 1L)
  event <- state$recovery_events[[1L]]
  expect_identical(event$stage, "export")
  expect_equal(event$requested_workers, 3L)
  expect_equal(vapply(event$tasks, `[[`, character(1L), "branch"), c("acceptable", "misspecified"))
})
