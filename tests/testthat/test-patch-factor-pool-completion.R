test_that("relaxation policy normalization preserves factor names across repeated normalization", {
  scalar <- SEMANTICA:::.semantica_normalize_relaxation_policy(
    "strict", c("A", "B")
  )
  expect_identical(scalar, c(A = "strict", B = "strict"))

  repeated <- SEMANTICA:::.semantica_normalize_relaxation_policy(
    scalar, c("A", "B")
  )
  expect_identical(repeated, scalar)

  mixed <- c(A = "wording_forbidden_relaxed", B = "strict")
  expect_identical(
    SEMANTICA:::.semantica_normalize_relaxation_policy(mixed, c("A", "B")),
    mixed
  )

  one <- c(F1 = "strict")
  expect_identical(
    SEMANTICA:::.semantica_normalize_relaxation_policy(one, "F1"),
    one
  )
})

test_that("hard pre-ACO gate requires slack and independent redundancy units", {
  fx <- semantica_test_three_factor_fixture("separable", per_factor = 4L)
  gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    cosine_sim_matrix = fx$matrix,
    df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L),
    min_slack = 2L,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_true(gate$feasible)
  expect_true(all(gate$table$eligible_n >= gate$table$selected_n + 2L))
  expect_true(all(gate$table$independent_duplicate_units >= gate$table$selected_n))

  thin <- semantica_test_three_factor_fixture("separable", per_factor = 2L)
  thin_gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    cosine_sim_matrix = thin$matrix,
    df = thin$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L),
    min_slack = 1L,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_false(thin_gate$feasible)
  expect_true(all(thin_gate$table$eligible_slack_deficit >= 1L))
})

test_that("UVA-inspired redundancy graph blocks pools without independent units", {
  ids <- paste0("i", 1:4)
  sim <- matrix(0.95, 4L, 4L, dimnames = list(ids, ids))
  diag(sim) <- 1
  df <- data.frame(id = ids, factor = "F1", item = paste("Item", ids), stringsAsFactors = FALSE)
  graph <- SEMANTICA:::identify_duplicate_clusters(
    sim, ids, threshold = 0.90,
    factor_assignment = stats::setNames(rep("F1", 4L), ids)
  )
  expect_identical(graph$method, "weighted_topological_overlap_graph")
  expect_equal(graph$n_clusters, 1L)

  gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    cosine_sim_matrix = sim,
    df = df,
    i.per.f = c(F1 = 2L),
    min_slack = 0L,
    dup_threshold = 0.90,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_false(gate$feasible)
  expect_lt(gate$table$independent_duplicate_units, gate$table$selected_n)
  expect_gt(gate$table$duplicate_unit_deficit, 0L)
})

test_that("top-up diagnostics inherit structural feasibility deficits", {
  ids <- paste0("i", 1:4)
  items <- data.frame(
    item_id = ids,
    factor = "F1",
    item_text = paste("Item", ids),
    stringsAsFactors = FALSE
  )
  sim <- matrix(0.95, 4L, 4L, dimnames = list(ids, ids))
  diag(sim) <- 1
  out <- SEMANTICA:::.semantica_pool_topup_combined_diagnostics(
    items_tbl = items,
    cosine_sim_matrix = sim,
    factors = list(F1 = "definition"),
    selected_targets = c(F1 = 2L),
    min_slack = 1L,
    structural_gate = TRUE,
    content_alignment_mode = "off",
    polarity_action = "off"
  )
  expect_false(out$structural_feasible[[1L]])
  expect_gt(out$recommended_additional[[1L]], 0L)
  expect_identical(out$operational_status[[1L]], "topup_recommended_structural")
})

test_that("explicit relaxation relaxes forbidden conflicts but keeps construct mismatch and polarity", {
  ids <- paste0("i", 1:6)
  sim <- diag(6L)
  dimnames(sim) <- list(ids, ids)
  df <- data.frame(
    item_id = ids,
    factor = "F1",
    item_text = paste("Item", ids),
    # i1 is a wording-only conflict; i2 is a robust forbidden-concept conflict;
    # i3 is a robust factor mismatch; i4 is a polarity failure.
    semantica_content_guard_pass = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    semantica_exclusion_conflict = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    semantica_exclusion_conflict_centered = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    semantica_factor_clear_mismatch = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    semantica_factor_clear_mismatch_centered = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    semantica_polarity_flag = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  strict <- SEMANTICA:::.semantica_preaco_pool_gate(
    sim, df, c(F1 = 1L), min_slack = 0L,
    content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = "strict"
  )
  relaxed <- SEMANTICA:::.semantica_preaco_pool_gate(
    sim, df, c(F1 = 1L), min_slack = 0L,
    content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = "wording_forbidden_relaxed"
  )
  expect_gt(relaxed$table$guarded_n, strict$table$guarded_n)
  # The recovery rung admits i1 and i2, but mismatch (i3) and polarity failure
  # (i4) remain excluded.
  expect_equal(strict$table$guarded_n, 2L)
  expect_equal(relaxed$table$guarded_n, 4L)
  expect_true(relaxed$guard_policy$relaxed_forbidden_conflicts)
})

test_that("top-up target is selected plus slack and ambiguity alone does not force regeneration", {
  items <- data.frame(
    item_id = paste0("i", 1:8),
    factor = "F1",
    item_text = paste("Item", 1:8),
    semantica_factor_alignment_status = rep("ambiguous", 8L),
    semantica_content_guard_pass = TRUE,
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  out <- SEMANTICA:::.semantica_pool_topup_diagnostics(
    items_tbl = items,
    factors = list(F1 = "definition"),
    selected_targets = c(F1 = 6L),
    n_per_factor = 30L,
    n_per_factor_override = TRUE,
    min_slack = 2L,
    content_alignment_mode = "guard",
    polarity_action = "guard",
    relaxation_level = "strict"
  )
  expect_equal(out$base_target, 30L)
  expect_equal(out$desired_usable, 8L)
  expect_equal(out$usable_n, 8L)
  expect_equal(out$aligned_n, 0L)
  expect_equal(out$recommended_additional, 0L)
})

test_that("factor-specific recovery relaxation does not leak to healthy factors", {
  df <- data.frame(
    item_id = c("a1", "a2", "b1", "b2"),
    factor = c("A", "A", "B", "B"),
    semantica_factor_alignment_status = "aligned",
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_exclusion_conflict = c(TRUE, FALSE, TRUE, FALSE),
    semantica_exclusion_conflict_centered = c(TRUE, FALSE, TRUE, FALSE),
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  policy <- c(A = "wording_forbidden_relaxed", B = "strict")
  guard <- SEMANTICA:::.semantica_preaco_guard_mask(
    df, content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = policy
  )
  expect_true(unname(guard$pass["a1"]))
  expect_false(unname(guard$pass["b1"]))
  expect_identical(guard$relaxed_factors, "A")
})

test_that("factor-specific policy tolerates requested factors absent from the current pool", {
  df <- data.frame(
    item_id = c("a1", "a2"), factor = c("A", "A"),
    semantica_factor_alignment_status = "aligned",
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_exclusion_conflict = FALSE,
    semantica_exclusion_conflict_centered = FALSE,
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  guard <- SEMANTICA:::.semantica_preaco_guard_mask(
    df, content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = c(A = "strict", B = "wording_forbidden_relaxed")
  )
  expect_identical(names(guard$relaxation_level), "A")
  expect_identical(unname(guard$relaxation_level), "strict")
  expect_true(all(guard$pass))
})

test_that("unresolved factor alignment gets one bounded refresh but is not item invalidity", {
  margins <- c(.01, -.02, -.03)
  cls <- SEMANTICA:::.semantica_classify_alignment_margins(margins, rep("F1", 3L))
  expect_identical(cls$status[2:3], rep("alignment_unresolved", 2L))

  ids <- paste0("i", 1:3)
  sim <- matrix(.40, 3L, 3L, dimnames = list(ids, ids)); diag(sim) <- 1
  df <- data.frame(
    item_id = ids, factor = "F1", item_text = paste("Item", ids),
    semantica_factor_alignment_status = cls$status,
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_exclusion_conflict = FALSE,
    semantica_exclusion_conflict_centered = FALSE,
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  guard <- SEMANTICA:::.semantica_preaco_guard_mask(
    df, content_alignment_mode = "guard", polarity_action = "guard"
  )
  expect_true(all(guard$pass))

  before <- SEMANTICA:::.semantica_pool_topup_diagnostics(
    df, list(F1 = list(description = "factor")), c(F1 = 1L),
    min_slack = 0L, content_alignment_mode = "guard",
    polarity_action = "guard"
  )
  expect_equal(before$alignment_recovery_deficit, 1L)
  expect_gt(before$recommended_additional, 0L)

  after <- SEMANTICA:::.semantica_pool_topup_diagnostics(
    df, list(F1 = list(description = "factor")), c(F1 = 1L),
    min_slack = 0L, content_alignment_mode = "guard",
    polarity_action = "guard", alignment_refresh_done = "F1"
  )
  expect_equal(after$alignment_recovery_deficit, 0L)
  expect_equal(after$recommended_additional, 0L)

  combined_before <- SEMANTICA:::.semantica_pool_topup_combined_diagnostics(
    df, sim, list(F1 = list(description = "factor")), c(F1 = 1L),
    min_slack = 0L, content_alignment_mode = "guard", polarity_action = "guard"
  )
  combined_after <- SEMANTICA:::.semantica_pool_topup_combined_diagnostics(
    df, sim, list(F1 = list(description = "factor")), c(F1 = 1L),
    min_slack = 0L, content_alignment_mode = "guard", polarity_action = "guard",
    alignment_refresh_done = "F1"
  )
  expect_equal(combined_before$recommended_additional, 1L)
  expect_equal(combined_after$recommended_additional, 0L)
  expect_true(combined_after$structural_feasible)

  gate <- SEMANTICA:::.semantica_preaco_pool_gate(
    sim, df, c(F1 = 1L), min_slack = 0L,
    content_alignment_mode = "guard", polarity_action = "guard"
  )
  expect_true(gate$feasible)
  expect_equal(gate$table$alignment_unresolved_n, 2L)
  expect_equal(gate$table$alignment_recovery_deficit, 0L)
})

test_that("unresolved alignment is operational only when content alignment is a guard", {
  df <- data.frame(
    item_id = c("i1", "i2", "i3"), factor = "F1",
    item_text = c("one", "two", "three"),
    semantica_factor_alignment_status = c("aligned", "alignment_unresolved", "alignment_unresolved"),
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_exclusion_conflict = FALSE,
    semantica_exclusion_conflict_centered = FALSE,
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  diagnostic <- SEMANTICA:::.semantica_pool_topup_diagnostics(
    df, list(F1 = list(description = "factor")), c(F1 = 1L),
    min_slack = 0L, content_alignment_mode = "diagnostic",
    polarity_action = "guard"
  )
  expect_equal(diagnostic$alignment_recovery_deficit, 0L)
  expect_equal(diagnostic$recommended_additional, 0L)
})

test_that("forbidden contrast severity is pool-calibrated rather than zero-thresholded", {
  cls <- SEMANTICA:::.semantica_classify_alignment_margins(
    c(.30, .20, -.05, -.40), rep("F1", 4L)
  )
  expect_identical(cls$status[3], "ambiguous")
  expect_identical(cls$status[4], "clear_mismatch")
})

test_that("forbidden-conflict recovery can make a pool feasible without relaxing factor mismatch or polarity", {
  ids <- paste0("i", 1:12)
  sim <- matrix(0.40, 12L, 12L, dimnames = list(ids, ids))
  diag(sim) <- 1
  df <- data.frame(
    item_id = ids,
    factor = "F1",
    item_text = paste("Item", ids),
    semantica_content_guard_pass = c(rep(FALSE, 8L), rep(TRUE, 4L)),
    semantica_exclusion_conflict = c(rep(TRUE, 8L), rep(FALSE, 4L)),
    semantica_exclusion_conflict_centered = c(rep(TRUE, 8L), rep(FALSE, 4L)),
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_polarity_flag = FALSE,
    stringsAsFactors = FALSE
  )
  strict <- SEMANTICA:::.semantica_preaco_pool_gate(
    sim, df, c(F1 = 6L), min_slack = 2L,
    content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = "strict"
  )
  relaxed <- SEMANTICA:::.semantica_preaco_pool_gate(
    sim, df, c(F1 = 6L), min_slack = 2L,
    content_alignment_mode = "guard", polarity_action = "guard",
    relaxation_level = "wording_forbidden_relaxed"
  )
  expect_false(strict$feasible)
  expect_true(relaxed$feasible)
  expect_lt(strict$table$eligible_n, strict$table$required_with_slack)
  expect_true(relaxed$table$eligible_n >= relaxed$table$required_with_slack)
  expect_true(relaxed$table$independent_duplicate_units >= relaxed$table$selected_n)
})

test_that("ACO exposes the effective pre-ACO relaxation policy explicitly", {
  expect_true("preaco_relaxation_level" %in% names(formals(ACO_with_ESEM)))
  expect_identical(
    eval(formals(ACO_with_ESEM)$preaco_relaxation_level),
    c("strict", "wording_forbidden_relaxed")
  )
})

test_that("Pareto archive balances validity dimensions without a structural-first tie-break", {
  evals <- list(
    list(
      score = .60, sem_score = .90, duplicate_penalty = .90, facet_coverage = 1,
      guard_penalty = .40, guard_diagnostics = list(passed = FALSE),
      content_coverage = list(
        facet_coverage = 1, redundancy_control = .90,
        mean_preservation = -.01, dispersion_preservation = -.01
      ),
      esem_result = list(score = .95)
    ),
    list(
      score = .80, sem_score = .82, duplicate_penalty = .95, facet_coverage = 1,
      guard_penalty = .90, guard_diagnostics = list(passed = TRUE),
      content_coverage = list(
        facet_coverage = 1, redundancy_control = .95,
        mean_preservation = -.02, dispersion_preservation = -.02
      ),
      esem_result = list(score = .84)
    ),
    list(
      score = .70, sem_score = .70, duplicate_penalty = .80, facet_coverage = .8,
      guard_penalty = .80, guard_diagnostics = list(passed = TRUE),
      content_coverage = list(
        facet_coverage = .8, redundancy_control = .80,
        mean_preservation = -.05, dispersion_preservation = -.05
      ),
      esem_result = list(score = .70)
    )
  )
  choice <- SEMANTICA:::.semantica_choose_pareto_archive(evals)
  expect_true(all(c(1L, 2L) %in% choice$front))
  expect_false(3L %in% choice$front)
  expect_equal(choice$index, 2L)
  expect_true(all(c(
    "content_mean_preservation", "content_dispersion_preservation",
    "structural_guard_attainment"
  ) %in% names(choice$metrics)))
  expect_true(all(c(
    "primary_loading_quality", "cross_loading_control",
    "factor_coverage", "redundancy_control"
  ) %in% names(choice$metrics)))
})

test_that("target and partial-target ESEM preserve or construct target matrices", {
  ids <- c("a1", "a2", "b1", "b2")
  fa <- stats::setNames(c("A", "A", "B", "B"), ids)
  auto <- SEMANTICA:::prepare_esem_rotation_args(
    "target", list(), ids, fa, c("A", "B")
  )
  expect_true(is.matrix(auto$target))
  expect_true(is.na(auto$target["a1", "A"]))
  expect_equal(auto$target["a1", "B"], 0)

  partial <- matrix(NA_real_, 4L, 2L, dimnames = list(ids, c("A", "B")))
  partial["a1", "B"] <- 0
  supplied <- SEMANTICA:::prepare_esem_rotation_args(
    "pst", list(target = partial), ids, fa, c("A", "B")
  )
  expect_identical(supplied$target, partial)
  expect_true(is.matrix(supplied$target.mask))
  expect_equal(supplied$target.mask["a1", "B"], 1)
  expect_equal(supplied$target.mask["a1", "A"], 0)

  custom_mask <- matrix(0, 4L, 2L, dimnames = dimnames(partial))
  custom_mask["a1", "B"] <- 1
  preserved <- SEMANTICA:::prepare_esem_rotation_args(
    "pst", list(target = partial, target.mask = custom_mask), ids, fa, c("A", "B")
  )
  expect_identical(preserved$target.mask, custom_mask)
})

test_that("parallel helper accepts one successful self-healing retry before serial fallback", {
  bad_cluster <- structure(list(worker_1 = NULL), class = "cluster")
  fallback_called <- FALSE
  retry_called <- FALSE
  tasks <- list(1L, 2L)
  out <- SEMANTICA:::.semantica_par_lapply_lb_or_serial(
    bad_cluster,
    tasks,
    fun = identity,
    serial_fun = function(x) -x,
    retry_parallel = function(error) {
      retry_called <<- TRUE
      list(ok = TRUE, value = lapply(tasks, function(x) x + 10L))
    },
    on_parallel_error = function(error) fallback_called <<- TRUE
  )
  expect_true(retry_called)
  expect_false(fallback_called)
  expect_equal(unlist(out), c(11L, 12L))
})

test_that("repair defaults are bounded and Pareto selection is the quality default", {
  cfg <- semantica_quality_config()
  expect_equal(cfg$final_structure_repair_max_seconds, 60)
  expect_equal(cfg$final_structure_repair_max_evals, 20L)
  expect_equal(cfg$final_structure_repair_prefilter_top_k, 10L)
  expect_identical(cfg$final_selection_mode, "pareto")
})


test_that("DFI search influence requires model-matched calibration in auto mode", {
  telemetry <- list(telemetry = list(completed_reps = 120L, successful_fits = 100L))
  matched <- SEMANTICA:::.semantica_dfi_search_weight_adjustment(
    telemetry, current_esem_weight = 0.55, configured_esem_weight = 0.60,
    active_stage = "esem_parametric", requested_mode = "auto"
  )
  expect_equal(matched$success_rate, 100 / 120)
  expect_false(matched$cap_applied)
  expect_equal(matched$effective_weight, 0.55)
  expect_identical(matched$search_confidence, "model_matched_esem_calibration")

  mismatched <- SEMANTICA:::.semantica_dfi_search_weight_adjustment(
    NULL, current_esem_weight = 0.55, configured_esem_weight = 0.60,
    active_stage = "heuristic", requested_mode = "auto"
  )
  expect_true(mismatched$cap_applied)
  expect_equal(mismatched$effective_weight, 0)
  expect_identical(mismatched$search_confidence, "esem_search_disabled_no_model_matched_dfi")

  explicit <- SEMANTICA:::.semantica_dfi_search_weight_adjustment(
    NULL, current_esem_weight = 0.55, configured_esem_weight = 0.60,
    active_stage = "strict_cfa", requested_mode = "strict_cfa_dfi"
  )
  expect_false(explicit$cap_applied)
  expect_equal(explicit$effective_weight, 0.55)
})

test_that("warm-up ESEM bootstrap archive keeps the best distinct candidates", {
  v1 <- c(1L, 1L, 0L, 0L)
  v2 <- c(1L, 0L, 1L, 0L)
  v3 <- c(1L, 0L, 0L, 1L)
  out <- SEMANTICA:::.semantica_update_warmup_candidates(
    list(), list(v1, v2, v1, v3), c(.20, .40, .50, .30), keep = 3L
  )
  expect_length(out, 3L)
  expect_equal(unname(vapply(out, `[[`, numeric(1L), "score")), c(.50, .40, .30))
  expect_identical(out[[1L]]$vector, v1)
})

test_that("deadline helper does not launch work after the deadline", {
  past <- proc.time()[["elapsed"]] - 1
  expect_length(SEMANTICA:::.semantica_lapply_before_deadline(1:3, identity, past), 0L)
  future <- proc.time()[["elapsed"]] + 60
  expect_identical(
    unlist(SEMANTICA:::.semantica_lapply_before_deadline(1:3, identity, future)),
    1:3
  )
})

test_that("parallel dispatch recovery is independent of localized error text", {
  retry_received_condition <- FALSE
  out <- SEMANTICA:::.semantica_par_lapply_lb_or_serial(
    cl = structure(list(), class = "cluster"),
    x = list(1L),
    fun = identity,
    serial_fun = identity,
    parallel_fun = function(...) stop(paste0(
      "conexi", intToUtf8(0x00F3), "n inv", intToUtf8(0x00E1), "lida"
    )),
    retry_parallel = function(error) {
      retry_received_condition <<- inherits(error, "condition")
      list(ok = TRUE, value = list(99L))
    }
  )
  expect_true(retry_received_condition)
  expect_identical(out, list(99L))
})

test_that("ESEM runtime influence follows observed admissible-fit coverage without a new cutoff", {
  expect_equal(SEMANTICA:::.semantica_esem_reliability_weight(0.45, 13, 40), 0.14625)
  expect_equal(SEMANTICA:::.semantica_esem_reliability_weight(0.45, 40, 40), 0.45)
  expect_equal(SEMANTICA:::.semantica_esem_reliability_weight(0.45, 0, 40), 0)
  expect_equal(SEMANTICA:::.semantica_esem_reliability_weight(0.45, 0, 0), 0.45)
})

test_that("parallel dispatch recovery remains available after an earlier recovered failure", {
  recoveries <- 0L
  recover_once <- function() {
    SEMANTICA:::.semantica_par_lapply_lb_or_serial(
      cl = structure(list(), class = "cluster"),
      x = list(1L), fun = identity, serial_fun = identity,
      parallel_fun = function(...) stop("socket failure"),
      retry_parallel = function(error) {
        recoveries <<- recoveries + 1L
        list(ok = TRUE, value = list(7L))
      }
    )
  }
  expect_identical(recover_once(), list(7L))
  expect_identical(recover_once(), list(7L))
  expect_equal(recoveries, 2L)
})


test_that("content coverage preserves pool-relative mean and dispersion as separate evidence", {
  ids <- c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4")
  sim <- diag(1, length(ids))
  dimnames(sim) <- list(ids, ids)
  sim["a1", "a2"] <- sim["a2", "a1"] <- .20
  sim["a1", "a3"] <- sim["a3", "a1"] <- .40
  sim["a1", "a4"] <- sim["a4", "a1"] <- .60
  sim["a2", "a3"] <- sim["a3", "a2"] <- .30
  sim["a2", "a4"] <- sim["a4", "a2"] <- .50
  sim["a3", "a4"] <- sim["a4", "a3"] <- .70
  sim["b1", "b2"] <- sim["b2", "b1"] <- .10
  sim["b1", "b3"] <- sim["b3", "b1"] <- .30
  sim["b1", "b4"] <- sim["b4", "b1"] <- .50
  sim["b2", "b3"] <- sim["b3", "b2"] <- .20
  sim["b2", "b4"] <- sim["b4", "b2"] <- .40
  sim["b3", "b4"] <- sim["b4", "b3"] <- .60
  fa <- stats::setNames(c("A", "A", "B", "B"), c("a1", "a4", "b1", "b4"))
  profile <- SEMANTICA:::.semantica_content_coverage_profile(
    names(fa), fa,
    list(A = ids[1:4], B = ids[5:8]), sim,
    facet_coverage = .75, duplicate_penalty = .90
  )
  expect_equal(profile$facet_coverage, .75)
  expect_equal(profile$redundancy_control, .90)
  expect_true(is.finite(profile$within_mean_abs_shift))
  expect_true(is.finite(profile$within_dispersion_abs_shift))
  expect_equal(profile$mean_preservation, -profile$within_mean_abs_shift)
  expect_equal(profile$dispersion_preservation, -profile$within_dispersion_abs_shift)
  expect_equal(nrow(profile$by_factor), 2L)
})

test_that("guard failure reasons use the complete configured criteria table including AVE", {
  diagnostics <- list(
    passed = FALSE,
    criteria = data.frame(
      criterion = c("ave", "min_primary_loading", "simple_structure"),
      value = c(.21, .48, .80),
      threshold = c(.35, .45, .72),
      direction = c(">=", ">=", ">="),
      multiplier = c(.60, 1, 1),
      passed = c(FALSE, TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  )
  reasons <- SEMANTICA:::.semantica_guard_failure_reasons(diagnostics)
  expect_length(reasons, 1L)
  expect_match(reasons, "AVE")
  expect_match(reasons, "0.210")
  expect_match(reasons, "0.350")
})

test_that("proxy validity dimensions keep structural reference failure separate from global validity", {
  guard <- list(
    passed = FALSE,
    criteria = data.frame(
      criterion = "ave", value = .21, threshold = .35,
      direction = ">=", multiplier = .60, passed = FALSE,
      stringsAsFactors = FALSE
    )
  )
  profile <- SEMANTICA:::.semantica_proxy_validity_dimensions(
    structural_diagnostics = guard,
    content_coverage = list(facet_coverage = 1),
    operational_feasible = TRUE,
    participant_response_available = FALSE
  )
  expect_identical(profile$operational$status, "feasible")
  expect_identical(profile$content$status, "descriptive_proxy_available")
  expect_identical(profile$structural$status, "configured_references_not_met")
  expect_identical(profile$criterion$status, "not_observed")
  expect_match(profile$overall_interpretation, "multidimensional")
})
