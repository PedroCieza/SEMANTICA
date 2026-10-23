# Regression guard only: this is not a complete static effect/RNG analyzer.
test_that("direct RNG calls remain confined to reviewed stochastic code", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R source tree is unavailable to the source-level RNG guard")

  rng_pattern <- paste0(
    "\\b(set\\.seed|sample\\.int|sample|runif|rnorm|rbeta|rgamma|",
    "rbinom|rchisq|rexp|rpois)\\s*\\("
  )
  function_pattern <- "^([.A-Za-z][.A-Za-z0-9_]*)\\s*<-\\s*function\\b"
  allowed <- c(
    "evaluation_broker.R::.semantica_with_task_seed",
    "minimal_pipeline.R::.semantica_run_resolve_seed",
    "analysis_extensions.R::.semantica_bootstrap_semantic_resampling",
    "item_generation.R::semantica_wrap",
    "pipeline_core.R::single_rep",
    "pipeline_core.R::compute_esem_parametric_dfi_cutoffs",
    "pipeline_core.R::compute_semantic_approx_dfi_cutoffs",
    "pipeline_core.R::compute_semantic_roc_dfi_cutoffs",
    "pipeline_core.R::estimate_recommended_validation_n",
    "pipeline_core.R::sample_items_with_duplicate_guard",
    "pipeline_core.R::.semantica_sample_factor_items_feasible",
    # Intentional optimizer exploration: only the requested random shortlist
    # slot consumes the caller's optimizer RNG stream. Tested below.
    "pipeline_core.R::.semantica_esem_shortlist",
    "pipeline_core.R::ACO_with_ESEM",
    "pipeline_core.R::eval_sem_fn",
    "pipeline_core.R::get_final_dfi_cluster"
  )

  observed <- character(0L)
  for (path in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
    lines <- readLines(path, warn = FALSE)
    current <- "<top-level>"
    for (line in lines) {
      fn <- regexec(function_pattern, trimws(line), perl = TRUE)
      hit <- regmatches(trimws(line), fn)[[1L]]
      if (length(hit) >= 2L) current <- hit[[2L]]
      if (grepl(rng_pattern, line, perl = TRUE)) {
        observed <- c(observed, paste0(basename(path), "::", current))
      }
    }
  }
  observed <- unique(observed)
  unexpected <- setdiff(observed, allowed)
  missing_reviewed <- setdiff(allowed, observed)

  expect_identical(
    unexpected,
    character(0L),
    info = paste(
      "New direct RNG use requires classification as intentional stochasticity,",
      "task-local seed infrastructure, or incidental RNG before it is allowed."
    )
  )
  expect_identical(
    missing_reviewed,
    character(0L),
    info = "Update the reviewed RNG allow-list when intentional stochastic code is removed or renamed."
  )
})

test_that("shortlist exploration consumes RNG only for eligible requested exploration", {
  withr::local_preserve_seed()
  shortlist <- SEMANTICA:::.semantica_esem_shortlist
  scores <- c(10, 9, 8, 7, 6)
  set.seed(20261021)
  before <- .Random.seed
  expect_identical(shortlist(scores, 2L, 0L), c(1L, 2L))
  expect_identical(.Random.seed, before)

  exploratory <- shortlist(scores, 2L, 1L)
  expect_identical(exploratory[1], 1L)
  expect_true(exploratory[2] %in% 3:5)
  expect_false(identical(.Random.seed, before))
  set.seed(20261021)
  expect_identical(shortlist(scores, 2L, 1L), exploratory)

  solutions <- list(c(1, 0, 0), c(0, 1, 0), c(1, 0, 0), c(0, 0, 1), c(0, 0, 1))
  expect_identical(shortlist(scores, 2L, 1L, solutions), c(1L, 4L))
  solutions[4:5] <- solutions[1:2]
  before <- .Random.seed
  expect_identical(shortlist(scores, 2L, 1L, solutions), c(1L, 2L))
  expect_identical(.Random.seed, before)
})
