test_that("semantica_run facade has six visible groups and preserves canonical result", {
  raw <- structure(list(
    best_items = c("i2", "i1"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("Planning", "Persistence"),
      item = c("Plan item", "Persist item"), stringsAsFactors = FALSE
    ),
    factor_assignment = c(i2 = "Persistence", i1 = "Planning"),
    dimensionality_mode = "multidimensional",
    best_objective = .79,
    optimization = list(best_items = c("i2", "i1"), response_validation = NULL),
    reproducibility = list(
      run_interface = list(interface = "semantica_run"),
      resolved_config = list(scale = list(
        scale_name = "Academic Self-Regulation",
        scale_description = "Regulation toward academic goals"
      ))
    ),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))

  fit <- .semantica_wrap_run_result(raw)
  expect_s3_class(fit, "semantica_run_result")
  expect_s3_class(fit, "semantica_full_pipeline_result")
  expect_identical(names(fit), c("scale", "items", "diagnostics", "plots", "provenance", "advanced"))
  expect_identical(length(fit), 6L)
  expect_identical(fit$advanced, raw)
  expect_identical(fit$scale$name, "Academic Self-Regulation")
  expect_identical(fit$items$item_id, c("i2", "i1"))
  expect_true("fit_indices" %in% names(fit$diagnostics))
})

test_that("regular-user plot surface exposes the stored core plots without recomputation", {
  markers <- lapply(
    c("summary", "fitness", "before", "after", "pfa"),
    function(id) structure(list(id = id), class = "semantica_test_plot")
  )
  raw <- structure(list(
    best_items = "i1",
    optimization = list(best_items = "i1"),
    plots = list(
      plot_summary_of_results = markers[[1L]],
      plot_fitness_evolution = markers[[2L]],
      plot_esem_before = markers[[3L]],
      plot_esem_after = markers[[4L]],
      plot_pfa_diagnostics = markers[[5L]]
    ),
    reproducibility = list(run_interface = list(interface = "semantica_run")),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))

  fit <- .semantica_wrap_run_result(raw)
  expect_identical(
    names(fit$plots),
    c(
      "plot_summary_of_results", "plot_fitness_evolution",
      "plot_esem_before", "plot_esem_after", "plot_pfa_diagnostics"
    )
  )
  expect_identical(fit$plots$plot_summary_of_results, markers[[1L]])
  expect_identical(fit$plots$plot_fitness_evolution, markers[[2L]])
  expect_identical(fit$plots$plot_esem_before, markers[[3L]])
  expect_identical(fit$plots$plot_esem_after, markers[[4L]])
  expect_identical(fit$plots$plot_pfa_diagnostics, markers[[5L]])
})

test_that("legacy direct field access resolves through the compact facade", {
  raw <- structure(list(
    best_items = c("i1", "i2"),
    optimization = list(best_items = c("i1", "i2"), score = .8),
    fit_indices = list(cfi = .97),
    reproducibility = list(run_interface = list(interface = "semantica_run")),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))
  fit <- .semantica_wrap_run_result(raw)

  expect_identical(fit$optimization, raw$optimization)
  expect_identical(fit[["fit_indices"]], raw$fit_indices)
  subset <- fit[c("best_items", "fit_indices")]
  expect_identical(subset$best_items, raw$best_items)
  expect_identical(subset$fit_indices, raw$fit_indices)
  fit[["fit_indices"]] <- list(cfi = .99)
  expect_identical(fit$fit_indices$cfi, .99)
  expect_identical(length(fit), 6L)
  expect_identical(semantica_view(fit, view = "raw")$fit_indices$cfi, .99)
})

test_that("advanced view maps the canonical result, not the six facade groups", {
  raw <- structure(list(
    best_items = "i1",
    optimization = list(),
    fit_indices = list(cfi = .96),
    reproducibility = list(run_interface = list(interface = "semantica_run")),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))
  fit <- .semantica_wrap_run_result(raw)
  v <- semantica_view(fit, view = "advanced")
  mapped <- unlist(v$sections, use.names = FALSE)

  expect_identical(v$retained_top_level_components, length(raw))
  expect_setequal(mapped, names(raw))
  expect_false(any(c("scale", "items", "diagnostics", "plots", "provenance", "advanced") %in% setdiff(mapped, names(raw))))
})

test_that("result info distinguishes visible and canonical sizes", {
  raw <- structure(list(
    best_items = "i1",
    optimization = list(),
    reproducibility = list(run_interface = list(interface = "semantica_run")),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))
  fit <- .semantica_wrap_run_result(raw)
  info <- semantica_result_info(fit)

  expect_true(info$compact_facade)
  expect_identical(info$top_level_components, 6L)
  expect_identical(info$canonical_top_level_components, length(raw))
  expect_identical(sum(info$presentation_sections), length(raw))
})

test_that("semantica_run returns the compact facade without changing full-pipeline data", {
  raw <- structure(list(
    best_items = c("i1", "i2"),
    optimization = list(best_items = c("i1", "i2")),
    reproducibility = list(),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("A", "B"), item = c("One", "Two"),
      stringsAsFactors = FALSE
    )
  ), class = c("semantica_full_pipeline_result", "list"))

  local_mocked_bindings(
    semantica_full_pipeline = function(...) raw,
    .package = "SEMANTICA"
  )

  fit <- semantica_run(
    "Test scale", "Test description",
    factors = list(A = "A", B = "B"),
    llm = list(backend = "ollama", chat_model = "x", embed_model = "y"),
    progress = "quiet"
  )

  expect_s3_class(fit, "semantica_run_result")
  expect_identical(length(fit), 6L)
  expect_identical(fit$advanced$best_items, raw$best_items)
  expect_identical(fit$run_config$interface, "semantica_run")
  expect_identical(fit$reproducibility$run_interface$interface, "semantica_run")
})

test_that("bundle round-trip stores canonical data and restores the regular facade", {
  raw <- structure(list(
    best_items = "i1",
    generation = list(cosine_sim_matrix = matrix(1, 1, 1)),
    optimization = list(best_items = "i1"),
    reproducibility = list(run_interface = list(interface = "semantica_run")),
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))
  fit <- .semantica_wrap_run_result(raw)
  path <- tempfile(fileext = ".rds")

  semantica_save_bundle(fit, path, write_manifest = FALSE)
  restored <- semantica_load_bundle(path, verify = TRUE)

  expect_s3_class(restored, "semantica_run_result")
  expect_identical(length(restored), 6L)
  expect_identical(restored$best_items, "i1")
  expect_true("bundle_manifest" %in% names(restored$advanced))
  expect_true(all(c("generation", "optimization", "reproducibility") %in% names(restored$advanced)))
})

test_that("post-hoc participant validation re-wraps a regular-user result", {
  raw <- structure(list(
    best_items = c("i1", "i2", "i3", "i4"),
    optimization = list(
      best_items = c("i1", "i2", "i3", "i4"),
      factor_assignment = c(i1 = "A", i2 = "A", i3 = "B", i4 = "B"),
      esem_syntax = "stored syntax",
      active_cutoffs = list(cfi = .90),
      model_info = list(
        rotation = "geomin", rotation_args = list(geomin.epsilon = .5),
        estimator = "ML", data_type = "continuous", full_esem_iter_max = 2000L,
        htmt_threshold = .85, semantic_esem_score_mode = "current"
      )
    ),
    reproducibility = list(
      run_interface = list(interface = "semantica_run"),
      resolved_config = list(scale = list(factors = list(A = "A", B = "B")))
    ),
    run_config = list(interface = "semantica_run"),
    evidence_profile = list(
      source_families = data.frame(
        family = c("theory_constraints", "embedding_semantic_structural", "participant_response"),
        status = c("available", "available", "not_supplied"),
        independent_of_embedding = c(TRUE, FALSE, TRUE),
        selection_conditioned = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE
      ),
      analysis_source_family_count = 2L,
      independent_empirical_evidence_family_count = 0L,
      participant_response_family_available = FALSE
    )
  ), class = c("semantica_full_pipeline_result", "list"))
  fit <- .semantica_wrap_run_result(raw)
  responses <- data.frame(i1 = 1:5, i2 = 2:6, i3 = 3:7, i4 = 4:8)

  local_mocked_bindings(
    prepare_esem_rotation_args = function(...) list(),
    run_esem_on_response_data = function(...) structure(list(), class = "mock_fit"),
    compute_response_cor = function(...) diag(4),
    extract_and_score_esem = function(...) list(converged = TRUE, admissible = TRUE),
    .package = "SEMANTICA"
  )

  out <- semantica_validate(fit, responses, verbose = FALSE)
  expect_s3_class(out, "semantica_run_result")
  expect_identical(length(out), 6L)
  expect_true(out$participant_validation_performed)
  expect_true(out$scale$participant_data)
  expect_identical(out$optimization$response_validation$result$admissible, TRUE)
})
