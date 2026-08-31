test_that("factor-specific semantic diagnostics expose a locally weak factor", {
  ids <- c(paste0("F1_", 1:3), paste0("F2_", 1:3), paste0("F3_", 1:3))
  fa <- setNames(rep(c("F1", "F2", "F3"), each = 3), ids)
  m <- matrix(.20, 9, 9, dimnames = list(ids, ids))
  diag(m) <- 1
  m[1:3, 1:3] <- .80; diag(m[1:3, 1:3]) <- 1
  m[4:6, 4:6] <- .75; diag(m[4:6, 4:6]) <- 1
  m[7:9, 7:9] <- .20; diag(m[7:9, 7:9]) <- 1

  d <- semantica_factor_semantic_diagnostics(m, fa)
  expect_s3_class(d, "semantica_factor_semantic_diagnostics")
  expect_equal(nrow(d), 3L)
  expect_identical(d$factor[which.min(d$gap)], "F3")
  expect_equal(d$gap[d$factor == "F3"], 0, tolerance = 1e-12)
  expect_equal(d$stochastic_superiority[d$factor == "F3"], .5, tolerance = 1e-12)
})

test_that("selection context keeps pool and selected semantic evidence separate", {
  ids <- c(paste0("F1_", 1:4), paste0("F2_", 1:4), paste0("F3_", 1:4))
  fa <- setNames(rep(c("F1", "F2", "F3"), each = 4), ids)
  m <- matrix(.25, 12, 12, dimnames = list(ids, ids))
  diag(m) <- 1

  # Each factor has one deliberately weak item. Selecting the first three per
  # factor should improve separation without changing the underlying pool.
  for (idx in list(1:4, 5:8, 9:12)) {
    m[idx, idx] <- .75
    diag(m[idx, idx]) <- 1
    weak <- idx[[4L]]
    strong <- idx[1:3]
    # Keep these weak within-factor links below the .25 between-factor
    # similarity so the candidate pool is not already saturated at A = 1.
    # Removing the weak item must then increase both A and the mean gap.
    m[weak, strong] <- .10
    m[strong, weak] <- .10
  }

  selected <- c("F1_1", "F1_2", "F1_3", "F2_1", "F2_2", "F2_3", "F3_1", "F3_2", "F3_3")
  ctx <- semantica_selection_context(m, fa, selected)

  expect_s3_class(ctx, "semantica_selection_context")
  expect_true(ctx$selection_conditioned)
  expect_identical(ctx$pool_scope, "input_similarity_matrix")
  expect_true(is.finite(ctx$pool$estimate))
  expect_true(is.finite(ctx$selected$estimate))
  expect_gt(ctx$selected$estimate, ctx$pool$estimate)
  expect_gt(ctx$selected_gap, ctx$pool_gap)
  expect_match(ctx$note, "not selection-adjusted", fixed = TRUE)
})

test_that("selection context permits stochastic-superiority saturation at one", {
  ids <- c(paste0("F1_", 1:4), paste0("F2_", 1:4), paste0("F3_", 1:4))
  fa <- setNames(rep(c("F1", "F2", "F3"), each = 4), ids)
  m <- matrix(.25, 12, 12, dimnames = list(ids, ids))
  diag(m) <- 1

  # Every within-factor off-diagonal similarity exceeds every between-factor
  # similarity. The pool is therefore already at the upper bound A = 1.
  for (idx in list(1:4, 5:8, 9:12)) {
    m[idx, idx] <- .75
    diag(m[idx, idx]) <- 1
  }

  selected <- c("F1_1", "F1_2", "F1_3", "F2_1", "F2_2", "F2_3", "F3_1", "F3_2", "F3_3")
  ctx <- semantica_selection_context(m, fa, selected)

  expect_equal(ctx$pool$estimate, 1, tolerance = 1e-12)
  expect_equal(ctx$selected$estimate, 1, tolerance = 1e-12)
  expect_equal(ctx$stochastic_superiority_gain, 0, tolerance = 1e-12)

  # The block fixture has the same .75 within-factor and .25 between-factor
  # similarities before and after selection. Its mean separation gap is
  # therefore already saturated for this geometry as well: .75 - .25 = .50.
  # Selection context must preserve this legitimate zero-gain case rather than
  # require every descriptive metric to increase after optimization.
  expect_equal(ctx$pool_gap, 0.50, tolerance = 1e-12)
  expect_equal(ctx$selected_gap, 0.50, tolerance = 1e-12)
  expect_equal(ctx$gap_gain, 0, tolerance = 1e-12)
  expect_true(ctx$selection_conditioned)
})

test_that("ESEM mixed state carries an explicit anti-misinterpretation message", {
  mixed <- semantica_esem_state(esem_result = list(
    converged = TRUE, admissible = TRUE, htmt_violations = 1,
    structure_diagnostics = list(correct_dominance = .75, no_large_cross_loading = .75)
  ))
  expect_identical(mixed$technical_state, "admissible")
  expect_identical(mixed$structural_quality, "admissible_but_structurally_mixed")
  expect_match(mixed$quality_message, "do not interpret admissibility", fixed = TRUE)
})

test_that("PFA discrepancy makes chance-adjusted partition state explicit", {
  pfa <- list(
    available = TRUE,
    factor_presence_recovery = 1,
    recovery_score = 1,
    missing_factors = character(0),
    clarity_score = .8,
    partition_agreement_ari = 0
  )
  esem <- list(
    admissible = TRUE,
    htmt_violations = 0,
    structure_diagnostics = list(correct_dominance = 1, no_large_cross_loading = 1)
  )
  d <- semantica_pfa_esem_discrepancy(pfa, esem)
  expect_identical(d$pfa_partition_state, "at_or_below_chance")
  expect_equal(d$metrics$pfa_partition_agreement_ari, 0)
})

test_that("compact summary surfaces selection, objective, and mixed-structure context", {
  factor_diag <- data.frame(
    factor = c("F1", "F2", "F3"),
    n_items = c(3L, 3L, 3L),
    n_within_pairs = c(3L, 3L, 3L),
    n_between_pairs = c(18L, 18L, 18L),
    within_mean = c(.7, .65, .25),
    between_mean = c(.2, .2, .2),
    gap = c(.5, .45, .05),
    stochastic_superiority = c(1, 1, .6),
    status = "computed",
    participant_based = FALSE,
    stringsAsFactors = FALSE
  )
  selection_ctx <- list(
    status = "computed",
    pool = list(estimate = .60),
    selected = list(estimate = .88),
    stochastic_superiority_gain = .28,
    pool_gap = .08,
    selected_gap = .33,
    gap_gain = .25,
    selected_factor_diagnostics = factor_diag,
    selection_conditioned = TRUE,
    participant_based = FALSE,
    note = "Selected values are not selection-adjusted inference."
  )
  pfa <- list(
    factor_presence_recovery = 1,
    partition_agreement_ari = .40,
    factor_presence_role = "intended_factor_presence_only",
    partition_agreement_role = "primary_chance_adjusted_partition_agreement_descriptor"
  )
  esem_state <- list(
    technical_state = "admissible",
    structural_quality = "admissible_but_structurally_mixed",
    quality_flags = c(htmt_overlap = TRUE, dominance_mismatch = TRUE, cross_loading = FALSE),
    quality_message = "Technically admissible but mixed."
  )
  x <- list(
    best_items = paste0("i", 1:9),
    semantic_score = .7,
    fit_indices = list(
      cfi = .96, rmsea = .05, srmr = .04,
      structure_diagnostics = list(
        factor_diagnostics = data.frame(
          factor = c("F1", "F2", "F3"),
          correct_dominance = c(1, 1, .5),
          simple_structure = c(1, .75, .25),
          stringsAsFactors = FALSE
        )
      )
    ),
    pfa_diagnostics = pfa,
    esem_state = esem_state,
    pfa_esem_discrepancy = list(state = "grouping_recoverable__separation_weak", pfa_partition_state = "positive_incomplete"),
    selection_semantic_context = selection_ctx,
    factor_semantic_diagnostics = factor_diag,
    best_objective = .82,
    objective_context = list(
      evidence_regime = "semantic_fallback_no_admissible_archive_esem",
      universal_quality_score = FALSE
    ),
    termination_reason = "archive_stable"
  )
  class(x) <- c("semantica_full_pipeline_result", "list")

  txt <- capture.output(print(summary(x)))
  expect_true(any(grepl("Candidate-pool superiority A", txt, fixed = TRUE)))
  expect_true(any(grepl("Selected-set superiority A", txt, fixed = TRUE)))
  expect_true(any(grepl("Weakest selected factor", txt, fixed = TRUE)))
  expect_true(any(grepl("ESEM STRUCTURAL WARNING", txt, fixed = TRUE)))
  expect_true(any(grepl("PFA partition agreement (ARI)", txt, fixed = TRUE)))
  expect_true(any(grepl("OBJECTIVE CONTEXT WARNING", txt, fixed = TRUE)))
})

test_that("ACO exposes objective evidence regime without changing the objective", {
  out <- semantica_test_run_aco(seed = 421L)

  expect_true(is.list(out$objective_context))
  expect_identical(out$objective_context$type, "optimization_utility")
  expect_identical(out$objective_context$evidence_regime, "semantic_only")
  expect_false(isTRUE(out$objective_context$universal_quality_score))
  expect_equal(out$objective_context$value, out$best_objective, tolerance = 1e-12)

  expect_true(is.list(out$selection_semantic_context))
  expect_true(isTRUE(out$selection_semantic_context$selection_conditioned))
  expect_identical(out$selection_semantic_context$pool_scope, "aco_eligible_candidate_pool_after_guards")
  expect_true(is.data.frame(out$factor_semantic_diagnostics))
  expect_equal(nrow(out$factor_semantic_diagnostics), 3L)

  expect_identical(
    out$pfa_diagnostics$partition_agreement_role,
    "primary_chance_adjusted_partition_agreement_descriptor"
  )
  expect_identical(
    out$pfa_diagnostics$factor_presence_role,
    "intended_factor_presence_only"
  )
})
