test_that("automatic PSOCK workers are capped by measured memory", {
  gib <- 1024^3
  local_mocked_bindings(
    .semantica_available_cores = function(omit = 0L) 16L,
    .semantica_available_physical_cores = function() 8L,
    .semantica_memory_snapshot = function() list(
      available_bytes = 8 * gib,
      total_bytes = 16 * gib,
      process_rss_bytes = 1 * gib,
      source = "unit-test"
    ),
    .package = "SEMANTICA"
  )

  plan <- semantica_resource_plan(n.cores = "auto")
  expect_identical(plan$effective_workers, 4L)
  expect_identical(plan$memory_worker_cap, 4L)
  expect_identical(plan$auto_safety_reason, "memory_budget")
  expect_true("memory_budget" %in% plan$limited_by)
  expect_true(plan$memory_aware)
})

test_that("explicit worker counts remain user-authoritative with memory-aware planning", {
  gib <- 1024^3
  local_mocked_bindings(
    .semantica_available_cores = function(omit = 0L) 16L,
    .semantica_memory_snapshot = function() list(
      available_bytes = 2 * gib,
      total_bytes = 16 * gib,
      process_rss_bytes = 1 * gib,
      source = "unit-test"
    ),
    .package = "SEMANTICA"
  )

  plan <- semantica_resource_plan(n.cores = 6L)
  expect_identical(plan$effective_workers, 6L)
  expect_identical(plan$request_mode, "explicit")
  expect_true(is.na(plan$memory_worker_cap))
})

test_that("automatic planning reserves coordinator and headroom from physical cores when memory is unavailable", {
  local_mocked_bindings(
    .semantica_available_cores = function(omit = 0L) 16L,
    .semantica_available_physical_cores = function() 6L,
    .semantica_memory_snapshot = function() list(
      available_bytes = NA_real_,
      total_bytes = NA_real_,
      process_rss_bytes = NA_real_,
      source = "unavailable"
    ),
    .package = "SEMANTICA"
  )

  plan <- semantica_resource_plan(n.cores = "auto")
  expect_identical(plan$effective_workers, 4L)
  expect_identical(plan$physical_cores_detected, 6L)
  expect_identical(plan$physical_worker_cap, 4L)
  expect_identical(plan$physical_core_fallback, 4L)
  expect_identical(plan$coordinator_cores_applied, 1L)
  expect_identical(plan$auto_safety_reason, "physical_core_budget")
})

test_that("high-memory auto planning cannot consume every detected physical core", {
  gib <- 1024^3
  local_mocked_bindings(
    .semantica_available_cores = function(omit = 0L) 16L,
    .semantica_available_physical_cores = function() 8L,
    .semantica_memory_snapshot = function() list(
      available_bytes = 128 * gib,
      total_bytes = 128 * gib,
      process_rss_bytes = 0.5 * gib,
      source = "unit-test"
    ),
    .package = "SEMANTICA"
  )

  plan <- semantica_resource_plan(n.cores = "auto")
  expect_identical(plan$physical_worker_cap, 6L)
  expect_identical(plan$effective_workers, 6L)
  expect_identical(plan$coordinator_cores_applied, 1L)
  expect_identical(plan$reserve_cores_applied, 1L)
  expect_true("physical_core_budget" %in% plan$limited_by)
  expect_lte(plan$effective_workers + plan$coordinator_cores_applied, 7L)
})

test_that("casual wrapper exposes worker control without changing analytical presets", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(
        list(reproducibility = list(effective_workers = 3L)),
        class = c("semantica_full_pipeline_result", "list")
      )
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    "Worker test", "Two factors.",
    factors = list(A = "Factor A.", B = "Factor B."),
    llm = "ollama", workers = 3L, verbose = FALSE
  )

  expect_identical(captured$resources$cpu_cores, 3L)
  expect_identical(result$run_config$workers_requested, 3L)
  expect_identical(result$run_config$workers_effective, 3L)
  expect_identical(result$run_config$worker_policy, "explicit_or_serial")
})

test_that("tiny nonzero semantic losses remain visible in progress formatting", {
  tiny <- SEMANTICA:::.semantica_format_progress_number(4e-05, digits = 4L)
  ordinary <- SEMANTICA:::.semantica_format_progress_number(0.12345, digits = 4L)
  zero <- SEMANTICA:::.semantica_format_progress_number(0, digits = 4L)

  expect_match(tiny, "e")
  expect_identical(ordinary, "0.1235")
  expect_identical(zero, "0.0000")
})

test_that("ESEM checkpoint telemetry is checkpoint-local rather than cumulative", {
  batch <- list(
    request_indices = 1:3,
    keys = c("a", "b"),
    cached = c(TRUE, FALSE),
    jobs_started = c(FALSE, TRUE)
  )
  out <- SEMANTICA:::.semantica_esem_checkpoint_telemetry(
    batch,
    admissible_requests = c(TRUE, TRUE, FALSE)
  )

  expect_identical(out$requests, 3L)
  expect_identical(out$unique_candidates, 2L)
  expect_identical(out$cache_hits, 1L)
  expect_identical(out$coalesced_requests, 1L)
  expect_identical(out$new_fits, 1L)
  expect_identical(out$admissible_requests, 2L)
})
