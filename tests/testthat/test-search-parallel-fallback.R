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
