test_that("casual robustness controls derive reproducible bounded plans", {
  seed <- SEMANTICA:::.semantica_run_resolve_seed(101L)
  restarts <- SEMANTICA:::.semantica_run_restart_seeds(seed$seed, "auto", "standard")
  expect_identical(seed$source, "user_supplied")
  expect_identical(restarts$seeds, c(101L, 104830L, 209559L))

  adaptive <- SEMANTICA:::.semantica_run_adapt_aco_budget(
    semantica_aco_config("standard"), pool_items = 15L,
    selected_items = 3L, n_factors = 3L
  )
  expect_identical(adaptive$budget_adaptation, "bounded_space_scaled")
  expect_gt(adaptive$ants, semantica_aco_config("standard")$ants)
  expect_identical(adaptive$evaporation$resolved_horizon %||% adaptive$evaporation$horizon,
                   adaptive$max_total_iter)

  explicit <- SEMANTICA:::.semantica_run_adapt_aco_budget(
    semantica_aco_config("standard", ants = 41L, search_patience = 17L, max_total_iter = 31L),
    pool_items = 15L, selected_items = 3L, n_factors = 3L
  )
  expect_identical(explicit$ants, 41L)
  expect_identical(explicit$search_patience, 17L)
  expect_identical(explicit$max_total_iter, 31L)
})

test_that("full semantica_run requests bounded adaptive DFI calibration", {
  captured <- NULL
  local_mocked_bindings(
    semantica_run_custom = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  semantica_run(
    "Full", "Two related constructs.",
    factors = list(A = "First construct.", B = "Second construct."), llm = "ollama",
    aco = "full", progress = "quiet", verbose = FALSE
  )

  expect_identical(captured$fit_calibration$strategy, "adaptive")
  expect_identical(captured$fit_calibration$esem_reps, 500L)
  expect_identical(captured$fit_calibration$adaptive_min_reps, 250L)
  expect_identical(captured$fit_calibration$adaptive_batch_reps, 100L)
  expect_identical(captured$fit_calibration$adaptive_tol, 0.001)
})

test_that("semantica_run supplies a recorded seed, frozen-pool restarts, and adaptive slack", {
  captured <- NULL
  local_mocked_bindings(
    semantica_run_custom = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    "Robust", "Two related constructs.",
    factors = list(A = "First construct.", B = "Second construct."),
    llm = "ollama", seed = 101L, restarts = "auto", verbose = FALSE
  )

  expect_identical(captured$seed, 101L)
  expect_identical(captured$restart_seeds, c(101L, 104830L, 209559L))
  expect_equal(captured$generation$temperature, 0.4)
  expect_identical(captured$generation$pool_topup_min_slack, 2L)
  expect_identical(captured$esem$cadence_mode, "adaptive")
  expect_identical(result$run_config$seed_source, "user_supplied")
  expect_identical(result$run_config$backend_contract$output_mode, "json")
})

test_that("executing a run plan retains its already adapted ACO budget", {
  captured <- NULL
  local_mocked_bindings(
    semantica_run_custom = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  plan <- semantica_run_plan(
    "Planned", "Two related constructs.",
    factors = list(A = "First construct.", B = "Second construct."),
    llm = "ollama", pool_items = 15L, selected_items = 3L,
    seed = 101L, progress = "quiet"
  )
  semantica_execute(plan, verbose = FALSE)

  expect_identical(captured$ants, plan$aco$ants)
  expect_identical(captured$search_patience, plan$aco$search_patience)
  expect_identical(captured$max_total_iter, plan$aco$max_total_iter)
})

test_that("full pipeline restarts reuse one prepared pool and select comparable evidence", {
  fx <- semantica_test_three_factor_fixture("separable")
  raw <- data.frame(
    ID = fx$ids, Dimension = fx$factors, Facet = fx$factors,
    item = paste("Item", fx$ids), item_id = fx$ids,
    factor = fx$factors, item_text = paste("Item", fx$ids), stringsAsFactors = FALSE
  )
  calls <- list()
  local_mocked_bindings(
    semantica_pipeline = function(...) list(
      cosine_sim_matrix = fx$matrix, df = raw, items_tbl_raw = raw, items_tbl = raw,
      generated_item_metadata = raw, generation_provenance = list(),
      embedding_diagnostics = list(), embedding_policy = list(), cosine_diagnostics = list(),
      cosine_adjustment_sensitivity = list(), representation_stability = list()
    ),
    ACO_with_ESEM = function(cosine_sim_matrix, df, i.per.f, seed, ...) {
      calls[[length(calls) + 1L]] <<- list(matrix = cosine_sim_matrix, df = df, seed = seed, dots = list(...))
      ids <- c("i1", "i2", "i5", "i6", "i9", "i10")
      list(
        best_items = ids,
        factor_assignment = stats::setNames(rep(c("F1", "F2", "F3"), each = 2L), ids),
        best_objective = as.numeric(seed),
        objective_context = list(evidence_regime = "semantic_only"),
        search_calibration = list(calibration_id = "shared-test-calibration"),
        search_guidance_status = "semantic_only",
        performance = list(), reproducibility = list(), summary = list()
      )
    },
    .package = "SEMANTICA"
  )

  out <- SEMANTICA:::.semantica_execute_pipeline(
    backend = "ollama", scale_name = "Restart test", scale_description = "Three constructs.",
    factors = stats::setNames(lapply(c("F1", "F2", "F3"), function(x) list(description = x)), c("F1", "F2", "F3")),
    n_per_factor = 4L, i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L),
    ants = 2L, max.iter = 1L, max_total_iter = 1L, restart_seeds = c(11L, 22L),
    pool_topup = FALSE, pool_topup_min_slack = 0L, content_alignment_mode = "off",
    polarity_action = "off", polarity_screen = FALSE, run_esem_during_search = FALSE,
    pfa_mode = "off", generate_plots = FALSE, checkpoint_dir = NULL, verbose = FALSE
  )

  expect_length(calls, 2L)
  expect_identical(calls[[1L]]$matrix, calls[[2L]]$matrix)
  expect_identical(calls[[1L]]$df, calls[[2L]]$df)
  expect_null(calls[[1L]]$dots$search_calibration)
  expect_identical(calls[[2L]]$dots$search_calibration$calibration_id,
                   "shared-test-calibration")
  expect_identical(out$multi_seed_stability$selected_seed, 22L)
  expect_identical(out$multi_seed_stability$selection_policy,
                   "highest_observed_objective_with_shared_calibration")
})

test_that("full-pipeline restart ranking rejects mismatched calibration identities", {
  fx <- semantica_test_three_factor_fixture("separable")
  raw <- data.frame(
    ID = fx$ids, Dimension = fx$factors, Facet = fx$factors,
    item = paste("Item", fx$ids), item_id = fx$ids,
    factor = fx$factors, item_text = paste("Item", fx$ids), stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    semantica_pipeline = function(...) list(
      cosine_sim_matrix = fx$matrix, df = raw, items_tbl_raw = raw, items_tbl = raw,
      generated_item_metadata = raw, generation_provenance = list(),
      embedding_diagnostics = list(), embedding_policy = list(), cosine_diagnostics = list(),
      cosine_adjustment_sensitivity = list(), representation_stability = list()
    ),
    ACO_with_ESEM = function(cosine_sim_matrix, df, i.per.f, seed, ...) {
      ids <- c("i1", "i2", "i5", "i6", "i9", "i10")
      list(
        best_items = ids,
        factor_assignment = stats::setNames(rep(c("F1", "F2", "F3"), each = 2L), ids),
        best_objective = as.numeric(seed),
        objective_context = list(evidence_regime = "semantic_only"),
        search_calibration = list(calibration_id = paste0("calibration-", seed)),
        performance = list(), reproducibility = list(), summary = list()
      )
    },
    .package = "SEMANTICA"
  )

  out <- SEMANTICA:::.semantica_execute_pipeline(
    backend = "ollama", scale_name = "Restart test", scale_description = "Three constructs.",
    factors = stats::setNames(lapply(c("F1", "F2", "F3"), function(x) list(description = x)), c("F1", "F2", "F3")),
    n_per_factor = 4L, i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L),
    ants = 2L, max.iter = 1L, max_total_iter = 1L, restart_seeds = c(11L, 22L),
    pool_topup = FALSE, pool_topup_min_slack = 0L, content_alignment_mode = "off",
    polarity_action = "off", polarity_screen = FALSE, run_esem_during_search = FALSE,
    pfa_mode = "off", generate_plots = FALSE, checkpoint_dir = NULL, verbose = FALSE
  )

  expect_false(out$multi_seed_stability$objective_comparable)
  expect_identical(out$multi_seed_stability$selected_seed, 11L)
  expect_identical(out$multi_seed_stability$selection_policy,
                   "primary_retained_noncomparable_objectives")
})
