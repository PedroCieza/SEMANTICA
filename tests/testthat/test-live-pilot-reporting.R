test_that("selected alignment has unique fields and separates raw from robust flags", {
  metadata <- data.frame(semantica_factor_alignment_status = c("clear_mismatch", "aligned", "clear_mismatch"),
    semantica_factor_alignment_status_centered = c("aligned", "clear_mismatch", "clear_mismatch"),
    semantica_content_guard_pass = c(TRUE, TRUE, FALSE))
  x <- SEMANTICA:::.semantica_selected_alignment_summary(metadata, "guard", 3L)
  expect_true(x$available)
  expect_false(anyDuplicated(names(x)) > 0L)
  expect_equal(x$clear_mismatch_n, 2L)
  expect_equal(x$robust_factor_mismatch_n, 1L)
  expect_equal(x$preprocessing_disagreement_n, 2L)
  expect_equal(x$guard_excluded_n, 1L)
  expect_match(x$warning, "Content mode: guard")
  expect_false(grepl("Use content_alignment_mode", x$warning, fixed = TRUE))
  missing <- SEMANTICA:::.semantica_selected_alignment_summary(NULL, "guard", 3L)
  expect_false(missing$available)
  metadata$semantica_factor_alignment_status_centered <- NULL
  expect_true(is.na(SEMANTICA:::.semantica_selected_alignment_summary(metadata, "off", 3)$robust_factor_mismatch_n))
})

test_that("unrequested ESEM does not claim failed calibration or weight attenuation", {
  for (requested in c(FALSE, TRUE)) {
    x <- SEMANTICA:::.semantica_dfi_search_weight_adjustment(NULL,
      if (requested) .75 else 0, active_stage = "heuristic",
      requested_mode = "semantic_approx_dfi")
    expect_identical(x$cap_applied, requested)
    expect_equal(x$effective_weight, 0)
    if (requested) {
      expect_match(x$note, "semantic_approx_dfi")
      expect_false(grepl("auto mode|strict-CFA", x$note))
    } else {
      expect_null(x$note)
      expect_identical(x$search_confidence, "esem_search_not_requested")
    }
  }
  x <- SEMANTICA:::.semantica_dfi_search_weight_adjustment(NULL, .75,
    active_stage = "heuristic", requested_mode = "semantic_approx_dfi", esem_search_requested = FALSE)
  expect_false(x$cap_applied)
})

test_that("failed fit aliases are unavailable measurements and still block acceptance", {
  fit <- list(converged = FALSE, admissible = FALSE, loading_quality = 0,
    ave = NA_real_, ave_warnings = character())
  p <- SEMANTICA:::compute_psychometric_guard_penalty(fit, guard_action = "final_constraint")
  d <- attr(p, "diagnostics")
  row <- d$criteria[d$criteria$criterion == "loading_quality", ]
  expect_equal(as.numeric(p), .05)
  expect_false(d$passed)
  expect_identical(row$state, "unavailable")
  expect_true(is.na(row$value))
  expect_equal(row$numeric_compatibility_alias, 0)
  expect_identical(row$availability, "unavailable")
})

test_that("reference-N evidence retains solver failures without refitting the reference", {
  calls <- 0L
  local_mocked_bindings(
    run_esem_on_matrix = function(..., return_diagnostics = FALSE) {
      calls <<- calls + 1L
      expect_true(return_diagnostics)
      list(fit = NULL, status = "converged_inadmissible",
        solver_records = list(list(attempt = 1L, warnings = "negative residual variance", warning_calls = "lavaan()")),
        rejection_assessment = list(admissible = FALSE, reasons = "negative_residual_variance", details = list(converged = TRUE)))
    },
    extract_and_score_esem = function(...) list(converged = FALSE, admissible = FALSE,
      cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_, loading_quality = 0, score = 0),
    .package = "SEMANTICA")
  m <- diag(2); dimnames(m) <- list(c("i1", "i2"), c("i1", "i2"))
  reference <- list(converged = TRUE, admissible = TRUE, cfi = .99, tli = .98,
    rmsea = .03, srmr = .04, loading_quality = .7, score = .8, solver_status = "accepted")
  set.seed(501); before <- .Random.seed
  x <- SEMANTICA:::evaluate_semantic_n_sensitivity("model", m, c(i1 = "A", i2 = "B"),
    c("A", "B"), c(10L, 20L), reference_n = 10L,
    precomputed_reference = list(n_obs = 10L, scored = reference))
  expect_identical(.Random.seed, before)
  expect_equal(calls, 1L)
  expect_true(x$fit_diagnostics[["10"]]$reused_reference)
  expect_identical(x$fit_diagnostics[["20"]]$status, "converged_inadmissible")
  expect_identical(x$fit_diagnostics[["20"]]$rejection_assessment$reasons, "negative_residual_variance")
  expect_equal(nrow(x$warning_events), 1L)
  expect_identical(x$warning_events$candidate_id, "20")
  expect_true(is.na(x$grid_results$loading_quality[2]))
  expect_equal(x$grid_results$cfi[1], .99)
  # The solver converged at both anchors; the second solution is inadmissible
  # and therefore unavailable for successful-fit summaries or acceptance.
  expect_identical(x$grid_results$converged, c(TRUE, TRUE))
  expect_identical(x$grid_results$admissible, c(TRUE, FALSE))
  expect_identical(x$grid_results$usable, c(TRUE, FALSE))
  expect_false(x$grid_results$fit_references_met[2])
  expect_equal(x$summary$successful_fits, 1L)
})

test_that("generation separates conceptual exclusions from previous sentence data", {
  prompt <- SEMANTICA:::.build_factor_prompt("Sentimentality", "Deep emotional bonds", 3L,
    forbidden_concepts = "status seeking", previous_items = "I comfort a close friend.")
  expect_match(prompt, "EXCLUDED CONCEPTS:\\n  - status seeking")
  expect_match(prompt, "<previous_items>\\n\"I comfort a close friend.\"\\n</previous_items>")
  expect_match(prompt, "CURRENT TARGET: Sentimentality")
  expect_match(prompt, "Their topics are not automatically excluded", fixed = TRUE)
  expect_false(grepl("DO NOT write about", prompt, fixed = TRUE))
})

test_that("generation uses the separated context on initial calls and replenishment", {
  prompts <- character()
  responses <- c("1. I enjoy painting quiet landscapes.",
    "1. I enjoy painting quiet landscapes.", "1. I explore unfamiliar scientific questions.")
  local_mocked_bindings(.call_chat = function(session, messages, ...) {
    prompts <<- c(prompts, messages[[1]]$content)
    responses[[length(prompts)]]
  }, .package = "SEMANTICA")
  x <- SEMANTICA::semantica_generate_items(
    structure(list(supports_structured_output = FALSE), class = "semantica_session"),
    "Offline", "Offline", factors = list(Openness = list(description = "Openness",
      facets = list(Art = list(description = "Art", forbidden = "status seeking"),
        Curiosity = list(description = "Curiosity")))),
    n_per_factor = 2L, overgenerate = 1, max_retries = 2L,
    structured_output = "numbered", verbose = FALSE)
  expect_length(prompts, 3L)
  expect_match(prompts[1], "EXCLUDED CONCEPTS")
  expect_match(prompts[2], "<previous_items>")
  expect_match(prompts[3], "CURRENT TARGET: Curiosity")
  expect_false(any(grepl("DO NOT write about", prompts, fixed = TRUE)))
  expect_equal(nrow(x), 2L)
  meta <- attr(x, "semantica_generation_metadata")
  expect_identical(meta$prompt_policy, "separate_concepts_and_previous_items_v2")
  expect_identical(meta$generation_spec_fingerprint_schema, "semantica-generation-spec-md5-v3")
  expect_equal(meta$attempts[[2]]$duplicate_previous_units, 1L)
})

test_that("the saved live pilot exposes the corrected alignment and penalty contract", {
  path <- test_path("..", "..", "simulation_results", "hexaco_readiness_pilot",
    "pilot_540cc7c7e70", "runs", "W01_R001", "outcome.rds")
  skip_if_not(file.exists(path))
  a <- readRDS(path)$result$advanced
  x <- SEMANTICA:::.semantica_selected_alignment_summary(a$selected_item_metadata, "guard", 24L)
  expect_true(x$available)
  expect_equal(x$raw_clear_mismatch_n, 5L)
  expect_equal(x$robust_factor_mismatch_n, 0L)
  expect_equal(x$preprocessing_disagreement_n, 7L)
  expect_equal(x$guard_excluded_n, 0L)
  p <- SEMANTICA:::compute_psychometric_guard_penalty(a$optimization$esem_result, guard_action = "final_constraint")
  expect_equal(as.numeric(p), a$structural_guard$diagnostics$penalty)
  expect_identical(attr(p, "diagnostics")$passed, a$structural_guard$passed)
})
