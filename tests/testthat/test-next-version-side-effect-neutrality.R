analysis_fingerprint <- function(x) {
  list(
    best_items = sort(x$best_items %||% character()),
    factor_assignment = x$factor_assignment,
    semantic_score = x$semantic_score,
    semantic_objective_score = x$semantic_objective_score,
    best_objective = x$best_objective,
    termination_reason = x$termination_reason
  )
}

test_that("history collection mode does not change ACO output", {
  semantica_test_mock_esem_unavailable()
  fx <- semantica_test_three_factor_fixture("separable")
  a <- do.call(ACO_with_ESEM, semantica_test_aco_args(fx, seed = 515L, history_mode = "full"))
  b <- do.call(ACO_with_ESEM, semantica_test_aco_args(fx, seed = 515L, history_mode = "none"))
  expect_equal(analysis_fingerprint(a), analysis_fingerprint(b), tolerance = 1e-12)
})

test_that("summary/report construction is analysis- and RNG-neutral", {
  out <- semantica_test_run_aco(seed = 616L)
  class(out) <- unique(c("semantica_full_pipeline_result", class(out)))
  before_obj <- analysis_fingerprint(out)
  set.seed(2026)
  before_rng <- .Random.seed
  sx <- summary(out)
  capture.output(print(sx))
  after_rng <- .Random.seed
  expect_identical(after_rng, before_rng)
  expect_equal(analysis_fingerprint(out), before_obj, tolerance = 1e-12)
})

test_that("base serialization round-trip preserves analysis output", {
  out <- semantica_test_run_aco(seed = 717L)
  path <- tempfile(fileext = ".rds")
  saveRDS(out, path)
  restored <- readRDS(path)
  expect_equal(analysis_fingerprint(restored), analysis_fingerprint(out), tolerance = 1e-12)
})

test_that("SEMANTICA bundle save/load is RNG-neutral and preserves analysis output", {
  out <- semantica_test_run_aco(seed = 818L)
  path <- tempfile(fileext = ".rds")
  set.seed(12345)
  before_rng <- .Random.seed
  semantica_save_bundle(out, path, write_manifest = FALSE)
  restored <- semantica_load_bundle(path, verify = TRUE)
  after_rng <- .Random.seed
  expect_identical(after_rng, before_rng)
  expect_equal(analysis_fingerprint(restored), analysis_fingerprint(out), tolerance = 1e-12)
})

test_that("embedding cache get/set is analysis-data neutral", {
  cache_dir <- tempfile("semantica-cache-neutral-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  value <- list(embedding = c(.1,.2,.3), model = "mock", id = "x")
  key <- paste(rep("c", 32L), collapse = "")
  set.seed(321)
  before_rng <- .Random.seed
  SEMANTICA:::.semantica_embedding_cache_set(key, value, cache_dir)
  got <- SEMANTICA:::.semantica_embedding_cache_get(key, cache_dir)
  expect_identical(.Random.seed, before_rng)
  expect_identical(got, value)
})

test_that("serial versus parallel respects documented seeded result contract where supported", {
  skip_if(Sys.getenv("SEMANTICA_RUN_PARALLEL_INTEGRATION_TESTS") != "true",
          "full ACO serial/PSOCK integration is opt-in; task-seed scheduling has dedicated mandatory tests")
  skip_if(parallel::detectCores(logical = TRUE) < 2L, "parallel infrastructure unavailable")
  fx <- semantica_test_three_factor_fixture("separable")
  serial_args <- semantica_test_aco_args(fx, seed = 919L)
  serial_args$use_parallel <- FALSE
  parallel_args <- serial_args
  parallel_args$use_parallel <- TRUE
  parallel_args$n.cores <- 2L
  a <- do.call(ACO_with_ESEM, serial_args)
  b <- do.call(ACO_with_ESEM, parallel_args)
  # This asserts the package's declared seeded method contract, not bytewise
  # equality of runtime metadata or worker scheduling details.
  expect_equal(analysis_fingerprint(a), analysis_fingerprint(b), tolerance = 1e-12)
})
