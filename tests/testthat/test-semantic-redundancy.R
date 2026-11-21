.redundancy_fixture <- function(panel = "panel", seed = 1L) {
  withr::local_preserve_seed(); set.seed(seed)
  E <- matrix(rnorm(48), 8, 6); E[2, ] <- E[1, ] + 1e-7
  E <- E / sqrt(rowSums(E^2))
  S <- tcrossprod(E); ids <- paste0(panel, seq_len(8))
  dimnames(S) <- list(ids, ids)
  items <- data.frame(item_id = ids, item_text = paste(panel, "synthetic wording", seq_len(8)), factor = "F")
  audit <- semantica_redundancy_audit(S, items, "synthetic-encoder-v1", panel_id = panel)
  j <- audit$pairs[c(1L, 22L), c("item_a", "item_b", "text_a", "text_b", "decision", "source", "reviewer", "rationale")]
  j$decision <- c("conflict", "compatible"); j$source <- "other"
  j$reviewer <- "synthetic test"; j$rationale <- "Controlled vectors; not human evidence"
  list(S = S, items = items, judgments = j,
    audit = semantica_redundancy_audit(S, items, "synthetic-encoder-v1", j, panel))
}

test_that("pair features exclude both self coordinates and preserve score-scale invariance", {
  f <- .redundancy_fixture(); before <- f$S
  a <- f$audit
  b <- semantica_redundancy_audit(.25 * f$S + .75, f$items)
  expect_identical(f$S, before)
  expect_equal(a$pairs$mutual_rank, b$pairs$mutual_rank)
  expect_equal(a$pairs$profile, b$pairs$profile)
  expect_equal(a$pairs$profile[1], cor(f$S[1, -(1:2)], f$S[2, -(1:2)], method = "spearman"))
  S <- matrix(.99, 8, 8, dimnames = dimnames(f$S)); diag(S) <- 1
  flat <- .semantica_duplicate_evidence(S, f$items$item_id, .9, NULL,
    semantica_redundancy_policy(), f$items)
  expect_true(all(is.na(flat$pair_evidence$pairs$profile)))
  expect_false(any(flat$pair_evidence$pairs$hard_conflict))
  expect_equal(compute_duplicate_penalty(f$items$item_id, NULL, NULL, S, .9, flat$item_cluster), 1)
  expect_equal(nrow(semantica_redundancy_audit(f$S[1, 1, drop = FALSE],
    f$items[1, , drop = FALSE])$pairs), 0L)
})

test_that("exact wording and explicit judgments bind conservative pair constraints", {
  f <- .redundancy_fixture(); f$items$item_text[2] <- paste0("  ", f$items$item_text[1], "  ")
  e <- .semantica_duplicate_evidence(f$S, f$items$item_id, .9, NULL,
    semantica_redundancy_policy(), f$items)
  expect_true(.semantica_duplicate_conflict(f$items$item_id[1:2], e$item_cluster))
  expect_false(.semantica_duplicate_conflict(f$items$item_id[c(1, 3)], e$item_cluster))
  stale <- f$judgments
  expect_error(semantica_redundancy_audit(f$S, f$items, judgments = stale), "stale")
  f <- .redundancy_fixture()
  reverse <- f$judgments
  reverse[c("item_a", "item_b", "text_a", "text_b")] <- reverse[c("item_b", "item_a", "text_b", "text_a")]
  expect_equal(semantica_redundancy_audit(f$S, f$items, judgments = reverse)$pairs$decision,
    f$audit$pairs$decision)
  expect_error(semantica_redundancy_audit(f$S, f$items,
    judgments = rbind(f$judgments, reverse)), "unique")
  expect_identical(.semantica_duplicate_evidence(f$S, f$items$item_id, .9, NULL),
    identify_duplicate_clusters(f$S, f$items$item_id, .9, .9995, NULL))
})

.pair_constraints_fixture <- function(edges, n = 8L, max_nodes = 10000L) {
  neighbors <- setNames(rep(list(character()), n), letters[seq_len(n)])
  for (i in seq_len(nrow(edges))) {
    a <- edges[i, 1]; b <- edges[i, 2]
    neighbors[[a]] <- c(neighbors[[a]], b); neighbors[[b]] <- c(neighbors[[b]], a)
  }
  structure(list(neighbors = neighbors, max_nodes = max_nodes), class = "semantica_pair_constraints")
}

test_that("direct chains permit compatible endpoints and budget exhaustion remains unknown", {
  g <- .pair_constraints_fixture(rbind(c("a", "b"), c("b", "c")), 3)
  x <- .semantica_joint_pool_selection(list(F = letters[1:3]), c(F = 2L), g)
  expect_true(x$feasible); expect_setequal(x$items, c("a", "c"))
  for (i in 1:10) {
    draw <- sample_items_with_duplicate_guard(letters[1:3], 2L, c(1, 0, 1), g)
    expect_setequal(draw$items, c("a", "c"))
  }
  dense <- .pair_constraints_fixture(t(combn(letters[1:3], 2)), 3, 1L)
  expect_error(.semantica_joint_pool_selection(list(F = letters[1:3]), c(F = 2L), dense),
    class = "semantica_error_constraint_budget")
})

test_that("pair/blueprint feasibility agrees with an exhaustive small-form oracle", {
  withr::local_preserve_seed(); set.seed(42)
  ids <- letters[1:8]; pool <- list(A = ids[1:4], B = ids[5:8]); counts <- c(A = 2L, B = 2L)
  facets <- setNames(rep(c("x", "x", "y", "y"), 2), ids)
  blueprint <- list(A = c("x", "y"), B = c("x", "y"))
  forms <- expand.grid(a = ids[1:2], b = ids[3:4], c = ids[5:6], d = ids[7:8], stringsAsFactors = FALSE)
  all_edges <- t(combn(ids, 2))
  for (i in 1:30) {
    edges <- all_edges[runif(nrow(all_edges)) < .25, , drop = FALSE]
    g <- .pair_constraints_fixture(edges)
    oracle <- any(apply(forms, 1, function(form) !.semantica_duplicate_conflict(form, g)))
    fit <- .semantica_joint_pool_selection(pool, counts, g, facets, blueprint, "balanced", 1L, 0L)
    expect_identical(fit$feasible, oracle)
    if (fit$feasible) expect_true(.semantica_facet_constraint_diagnostics(fit$items,
      setNames(rep(c("A", "B"), each = 4), ids), facets, blueprint, counts,
      "balanced", 1L, 0L, g)$passed)
  }
})

test_that("calibration uses held-out panels and activation remains explicitly separate", {
  d <- .redundancy_fixture("development", 11); v <- .redundancy_fixture("validation", 12)
  fit <- semantica_fit_redundancy(list(d$audit), list(v$audit), max_false_merge = 0)
  expect_true(fit$usable)
  expect_identical(fit$status, "experimental_empirical_control_evaluation")
  expect_error(semantica_fit_redundancy(list(d$audit), list(d$audit), 0), "overlap")
  expect_error(semantica_fit_redundancy(list(d$audit), list(v$audit), 0, max_rules = 1), "budget")
  diagnostic <- semantica_redundancy_policy(calibration = fit, representation = "synthetic-encoder-v1")
  active <- semantica_redundancy_policy(calibration = fit, automatic = TRUE,
    representation = "synthetic-encoder-v1")
  a <- semantica_redundancy_audit(v$S, v$items)
  expect_false(any(.semantica_pair_constraints(a, diagnostic)$audit$pairs$hard_conflict))
  expect_true(any(.semantica_pair_constraints(a, active)$audit$pairs$hard_conflict))
  compatible <- v$judgments[1, , drop = FALSE]; compatible$decision <- "compatible"
  override <- semantica_redundancy_audit(v$S, v$items, judgments = compatible)
  expect_false(.semantica_pair_constraints(override, active)$audit$pairs$hard_conflict[1])
  reversed <- v$judgments; reversed$decision <- rev(reversed$decision)
  held_out_failure <- semantica_redundancy_audit(v$S, v$items, "synthetic-encoder-v1",
    reversed, "validation")
  failed <- semantica_fit_redundancy(list(d$audit), list(held_out_failure), max_false_merge = 0)
  expect_identical(failed$rule, fit$rule)
  expect_false(failed$usable)
  expect_error(semantica_redundancy_policy(calibration = failed, automatic = TRUE,
    representation = "synthetic-encoder-v1"), "usable")
  expect_length(fit$panel_metrics$validation$panel_id, 1L)
  expect_error(semantica_redundancy_policy(calibration = fit, automatic = TRUE, representation = "other"), "match")
  fit$rule$rank_max <- 1
  expect_error(semantica_redundancy_policy(calibration = fit, representation = "synthetic-encoder-v1"), "match")
  expect_error(semantica_redundancy_policy(automatic = TRUE), "require")
  expect_null(semantica_quality_config()$redundancy)
  expect_s3_class(semantica_quality_config(redundancy = diagnostic)$redundancy, "semantica_redundancy_policy")
})

test_that("native proposal auditing honors manual pair constraints without provider or fit calls", {
  prohibit <- function(...) stop("Unexpected provider/fit call")
  local_mocked_bindings(.call_embed = prohibit, .call_chat = prohibit,
    run_esem_on_matrix = prohibit, .package = "SEMANTICA")
  ids <- letters[1:12]; L <- matrix(0, 12, 2)
  L[1:6, 1] <- .7; L[7:12, 2] <- .7
  S <- tcrossprod(L) + diag(.51, 12); dimnames(S) <- list(ids, ids)
  items <- data.frame(item_id = ids, item_text = paste("Distinct synthetic claim", ids),
    factor = rep(c("A", "B"), each = 6))
  j <- data.frame(item_a = c("a", "b"), item_b = c("b", "c"),
    text_a = items$item_text[c(1, 2)], text_b = items$item_text[c(2, 3)],
    decision = "conflict", source = "other", reviewer = "synthetic", rationale = "chain control")
  result <- semantica_audit_solutions(S, items, c(A = 4L, B = 4L),
    list(compatible = ids[c(1, 3, 4, 5, 7:10)], forbidden = ids[c(1, 2, 4, 5, 7:10)]),
    stage = "proposal", execute = TRUE, seed = 81, pfa_mode = "off",
    redundancy = semantica_redundancy_policy(judgments = j))
  expect_true(result$results$native_legal[1]); expect_false(result$results$native_legal[2])
})

test_that("configuration forwarding and recovery errors preserve the chosen policy", {
  captured <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    captured <<- list(...); stop("captured policy")
  }, .package = "SEMANTICA")
  p <- semantica_redundancy_policy()
  expect_error(semantica_run_custom("fixture", "fixture", list(A = "a", B = "b"),
    quality = semantica_quality_config(redundancy = p), checkpoint_dir = NULL,
    verbose = FALSE), "captured policy")
  expect_identical(captured$redundancy, p)
  # Serialized configurations predating this option must not partially match
  # their numeric redundancy_threshold field.
  old <- semantica_quality_config(); old$redundancy <- NULL
  expect_error(semantica_run_custom("fixture", "fixture", list(A = "a", B = "b"),
    quality = old, checkpoint_dir = NULL, verbose = FALSE), "captured policy")
  expect_null(captured$redundancy)
  expect_error(semantica_pipeline(pool_topup_redundancy = list()), "Use semantica_redundancy_policy")
  local_mocked_bindings(.semantica_preaco_pool_gate = function(...) stop(structure(
    list(message = "unknown feasibility", call = NULL),
    class = c("semantica_error_constraint_budget", "error", "condition"))), .package = "SEMANTICA")
  f <- .redundancy_fixture()
  expect_error(.semantica_pool_topup_combined_diagnostics(f$items, f$S,
    list(F = "factor"), c(F = 2L), redundancy = p), class = "semantica_error_constraint_budget")
})

test_that("content admission does not change the frozen pair reference or stale valid judgments", {
  f <- .redundancy_fixture()
  policy <- semantica_redundancy_policy(judgments = f$judgments)
  full <- .semantica_preaco_pool_gate(f$S, f$items, c(F = 2L), min_slack = 0L,
    cohesion_retention = 1, redundancy = policy)
  restricted <- .semantica_preaco_pool_gate(f$S, f$items[-1, ], c(F = 2L), min_slack = 0L,
    cohesion_retention = 1, redundancy = policy, redundancy_data = f$items)
  expect_identical(full$redundancy_graph$pair_evidence,
    restricted$redundancy_graph$pair_evidence)
  expect_false(f$items$item_id[1] %in% unlist(restricted$eligible_items))
})

test_that("a bounded ACO search enforces pair conflicts in serial and PSOCK modes", {
  ids <- paste0("pair", seq_len(14))
  assignment <- rep(c("A", "B"), each = 7L)
  L <- matrix(.03, 14, 2)
  L[cbind(seq_len(14), rep(1:2, each = 7))] <- rep(seq(.45, .85, length.out = 7), 2)
  S <- tcrossprod(L); diag(S) <- 1; dimnames(S) <- list(ids, ids)
  items <- data.frame(item_id = ids, item_text = paste("Synthetic claim", ids), factor = assignment)
  j <- data.frame(item_a = ids[1], item_b = ids[8], text_a = items$item_text[1],
    text_b = items$item_text[8], decision = "conflict", source = "other",
    reviewer = "synthetic test", rationale = "Cross-domain test edge")
  for (parallel in c(FALSE, TRUE)) {
    result <- suppressWarnings(ACO_with_ESEM(S, items, i.per.f = c(A = 4L, B = 4L),
      ants = 2L, max.iter = 1L, max_total_iter = 1L, run_esem_during_search = FALSE,
      esem_weight = 0, pfa_mode = "off", run_pfa_during_search = FALSE, dfi_warmup_iters = 0L,
      cohesion_retention = 1, semantic_reference_policy = "admitted_pool_lower_guard",
      within_similarity_target = c(A = .3, B = .3), facet_constraint_mode = "soft",
      semantic_esem_score_mode = "continuous_balanced", psychometric_guard_action = "report_only",
      semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE, final_dddfi = FALSE,
      final_dfi_recalibrate = FALSE, final_equivtest = FALSE, use_parallel = parallel,
      n.cores = if (parallel) 2L else 1L, seed = 100L, verbose = FALSE,
      redundancy = semantica_redundancy_policy(judgments = j)))
    expect_length(result$best_items, 8L)
    expect_false(.semantica_duplicate_conflict(result$best_items, result$duplicate_cluster_id))
    expect_true(any(result$duplicate_clusters$pair_evidence$pairs$hard_conflict))
    expect_equal(nrow(result$reproducibility$search_esem_parallel_fallbacks), 0L)
    expect_equal(nrow(result$reproducibility$search_esem_parallel_recoveries), 0L)
  }
})
