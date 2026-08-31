test_that("compact result view is read-only and regular-interface aware", {
  x <- structure(list(
    best_items = c("i2", "i1"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("Planning", "Persistence"),
      Facet = c("plan", "persist"), item = c("Plan item", "Persist item"),
      stringsAsFactors = FALSE
    ),
    factor_assignment = c(i2 = "Persistence", i1 = "Planning"),
    dimensionality_mode = "multidimensional",
    best_objective = .795,
    optimization = list(response_validation = NULL),
    participant_validation_performed = FALSE,
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))

  before <- serialize(x, NULL)
  v <- semantica_view(x)
  expect_s3_class(v, "semantica_result_view")
  expect_identical(v$view, "compact")
  expect_true("next_steps" %in% names(v))
  expect_false("next" %in% names(v))
  expect_identical(v$scale$selected_items, 2L)
  expect_identical(v$selected_scale$item_id, c("i2", "i1"))
  expect_identical(serialize(x, NULL), before)
  expect_identical(semantica_view(x, view = "raw"), x)
})

test_that("advanced result map covers every retained top-level component exactly once", {
  x <- structure(list(
    generation = list(pool = TRUE),
    optimization = list(search = TRUE),
    plots = list(),
    best_items = "i1",
    fit_indices = list(cfi = .95),
    evidence_records = list(),
    reproducibility = list(),
    future_component = list(preserved = TRUE)
  ), class = c("semantica_full_pipeline_result", "list"))

  v <- semantica_view(x, view = "advanced")
  mapped <- unlist(v$sections, use.names = FALSE)
  expect_s3_class(v, "semantica_result_view")
  expect_setequal(mapped, names(x))
  expect_identical(anyDuplicated(mapped), 0L)
  expect_true("other" %in% names(v$sections))
  expect_identical(v$sections$other, "future_component")
})

test_that("advanced sections expose original stored values without recomputation", {
  fit <- list(cfi = .95, rmsea = .04)
  pfa <- list(available = TRUE, score = .8)
  x <- structure(list(
    fit_indices = fit,
    pfa_diagnostics = pfa,
    best_items = c("i1", "i2"),
    reproducibility = list(master_seed = 1L)
  ), class = c("semantica_full_pipeline_result", "list"))

  structural <- semantica_view(x, view = "advanced", section = "structural")
  expect_s3_class(structural, "semantica_result_section")
  expect_identical(structural$fit_indices, fit)
  expect_identical(structural$pfa_diagnostics, pfa)
  expect_false("best_items" %in% names(structural))
})

test_that("direct full-pipeline print shows the advanced map rather than dumping fields", {
  x <- structure(list(
    best_items = c("i1", "i2"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("A", "B"), item = c("One", "Two"),
      stringsAsFactors = FALSE
    ),
    optimization = list(),
    fit_indices = list(cfi = NA_real_)
  ), class = c("semantica_full_pipeline_result", "list"))

  txt <- capture.output(print(x))
  expect_true(any(grepl("SEMANTICA advanced result map", txt, fixed = TRUE)))
  expect_true(any(grepl("Canonical result", txt, fixed = TRUE)))
  expect_false(any(grepl("List of", txt, fixed = TRUE)))
})

test_that("result info reports facade counts without changing the result", {
  x <- structure(list(
    best_items = "i1",
    fit_indices = list(),
    reproducibility = list(run_interface = list(interface = "semantica_run"))
  ), class = c("semantica_full_pipeline_result", "list"))
  info <- semantica_result_info(x)
  expect_identical(info$interface, "regular")
  expect_identical(info$top_level_components, length(x))
  expect_identical(sum(info$presentation_sections), length(x))
})
