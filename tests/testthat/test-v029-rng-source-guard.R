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
    "item_generation.R::semantica_wrap",
    "pipeline_core.R::single_rep",
    "pipeline_core.R::compute_esem_parametric_dfi_cutoffs",
    "pipeline_core.R::compute_semantic_approx_dfi_cutoffs",
    "pipeline_core.R::compute_semantic_roc_dfi_cutoffs",
    "pipeline_core.R::estimate_recommended_validation_n",
    "pipeline_core.R::sample_items_with_duplicate_guard",
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
