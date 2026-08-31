test_that("configuration printing redacts credentials without mutating the object", {
  cfg <- semantica_llm_config(backend = "openai", api_key = "super-secret-key")
  txt <- capture.output(print(cfg))
  expect_true(any(grepl("<configured>", txt, fixed = TRUE)))
  expect_false(any(grepl("super-secret-key", txt, fixed = TRUE)))
  expect_identical(cfg$api_key, "super-secret-key")
})

test_that("printing a high-level result is compact while summary stays detailed", {
  x <- structure(list(
    best_items = c("i1", "i2"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("A", "B"),
      item = c("One", "Two"), stringsAsFactors = FALSE
    ),
    optimization = list(response_validation = NULL),
    fit_indices = list(cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_),
    participant_validation_performed = FALSE,
    run_config = list(interface = "semantica_run")
  ), class = c("semantica_full_pipeline_result", "list"))
  printed <- capture.output(print(x))
  detailed <- capture.output(print(summary(x)))
  expect_true(any(grepl("SEMANTICA overview", printed, fixed = TRUE)))
  expect_false(any(grepl("[7] Participant evidence", printed, fixed = TRUE)))
  expect_true(any(grepl("[7] Participant evidence", detailed, fixed = TRUE)))
})

test_that("summary section filtering only changes presentation", {
  x <- structure(list(
    semantic_score = .82, pfa_score = .77,
    optimization = list(response_validation = NULL),
    fit_indices = list(cfi = .95, rmsea = .05, srmr = .04),
    participant_validation_performed = FALSE
  ), class = c("semantica_full_pipeline_result", "list"))
  s <- summary(x, sections = "participant")
  txt <- capture.output(print(s))
  expect_true(any(grepl("[7] Participant evidence", txt, fixed = TRUE)))
  expect_false(any(grepl("[3] Semantic discrimination", txt, fixed = TRUE)))
  expect_identical(s$semantic_score, summary(x)$semantic_score)
})

test_that("evidence status preserves raw tokens and offers readable labels", {
  x <- list(participant_validation_performed = FALSE)
  raw <- semantica_evidence_status(x, labels = "raw")
  human <- semantica_evidence_status(x, labels = "human")
  both <- semantica_evidence_status(x, labels = "both")
  expect_true("not_established" %in% raw$status)
  expect_false(identical(raw$status, human$status))
  expect_true("raw_status" %in% names(both))
  expect_identical(both$raw_status, raw$status)
})

test_that("run plans can delegate execution without storing custom backend functions", {
  plan <- semantica_run_plan(
    scale_name = "Plan", scale_description = "Two dimensions",
    factors = list(A = "A", B = "B"), llm = "ollama", progress = "quiet"
  )
  captured <- NULL
  local_mocked_bindings(
    semantica_run = function(...) { captured <<- list(...); structure(list(ok = TRUE), class = c("semantica_full_pipeline_result", "list")) },
    .package = "SEMANTICA"
  )
  out <- semantica_execute(plan, seed = 99L)
  expect_s3_class(out, "semantica_full_pipeline_result")
  expect_identical(captured$seed, 99L)
  expect_identical(captured$scale_name, "Plan")
})

test_that("review and provenance accessors are read-only reorganizations", {
  x <- structure(list(
    best_items = c("i1"),
    selected_item_metadata = data.frame(ID = "i1", Dimension = "A", item = "One", stringsAsFactors = FALSE),
    reproducibility = list(semantica_version = "0.5.7.2", master_seed = 1234L, resolved_config_schema = "semantica-resolved-config-1", resolved_config_hash = "abc"),
    optimization = list(best_items = "i1")
  ), class = c("semantica_full_pipeline_result", "list"))
  items <- semantica_item_review(x)
  prov <- semantica_provenance(x)
  info <- semantica_result_info(x)
  expect_identical(items$item_id, "i1")
  expect_identical(prov$package_version, "0.5.7.2")
  expect_identical(prov$analysis_seed, 1234L)
  expect_s3_class(info, "semantica_result_info")
})

test_that("post-hoc participant validation reuses stored structure and refreshes evidence metadata", {
  x <- structure(list(
    optimization = list(
      best_items = c("i1", "i2", "i3", "i4"),
      factor_assignment = c(i1 = "A", i2 = "A", i3 = "B", i4 = "B"),
      esem_syntax = "stored syntax",
      active_cutoffs = list(cfi = .90),
      model_info = list(rotation = "geomin", rotation_args = list(geomin.epsilon = .5), estimator = "ML", data_type = "continuous", full_esem_iter_max = 2000L, htmt_threshold = .85, semantic_esem_score_mode = "current")
    ),
    reproducibility = list(resolved_config = list(scale = list(factors = list(A = "A", B = "B")))),
    evidence_profile = list(
      source_families = data.frame(family = c("theory_constraints", "embedding_semantic_structural", "participant_response"), status = c("available", "available", "not_supplied"), independent_of_embedding = c(TRUE, FALSE, TRUE), selection_conditioned = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE),
      analysis_source_family_count = 2L,
      independent_empirical_evidence_family_count = 0L,
      participant_response_family_available = FALSE
    )
  ), class = c("semantica_full_pipeline_result", "list"))
  responses <- data.frame(i1 = 1:5, i2 = 2:6, i3 = 3:7, i4 = 4:8)
  local_mocked_bindings(
    prepare_esem_rotation_args = function(...) list(),
    run_esem_on_response_data = function(...) structure(list(), class = "mock_fit"),
    compute_response_cor = function(...) diag(4),
    extract_and_score_esem = function(...) list(converged = TRUE, admissible = TRUE),
    .package = "SEMANTICA"
  )
  out <- semantica_validate(x, responses, verbose = FALSE)
  expect_identical(out$optimization$best_items, x$optimization$best_items)
  expect_true(out$participant_validation_performed)
  expect_true(out$evidence_profile$participant_response_family_available)
  expect_identical(out$evidence_profile$analysis_source_family_count, 3L)
  expect_identical(out$optimization$response_validation$result$admissible, TRUE)
})

test_that("multi-seed presentation class leaves the object list-compatible", {
  x <- structure(list(n_successful = 2L, requested_seeds = c(1L, 2L), n_unique_solutions = 1L, mean_pairwise_jaccard = .8, consensus_items = c("i1", "i2")), class = c("semantica_multi_seed_result", "list"))
  expect_true(is.list(x))
  expect_s3_class(summary(x), "summary.semantica_multi_seed_result")
  expect_true(any(grepl("multi-seed", capture.output(print(x)), ignore.case = TRUE)))
})
