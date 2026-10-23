# Exercise the actual native proposal function, independently of construct names.
repair_test_function <- function(name, env) {
  find <- function(x) {
    if (missing(x) || (!is.call(x) && !is.expression(x) && !is.pairlist(x))) return(NULL)
    if (is.call(x) && identical(x[[1L]], quote(`<-`)) && identical(x[[2L]], as.name(name))) return(x[[3L]])
    for (i in seq_along(x)) {
      if (identical(x[[i]], quote(expr = ))) next
      y <- find(x[[i]])
      if (!is.null(y)) return(y)
    }
    NULL
  }
  code <- find(body(SEMANTICA::ACO_with_ESEM))
  stopifnot(!is.null(code))
  eval(code, env)
}

repair_test_context <- function(mode = "balanced") {
  e <- new.env(parent = asNamespace("SEMANTICA"))
  e$item.vector <- c("selected_a", "selected_b", "other_b1", "other_b2", "legal_a", "uncredited")
  e$item.factor.lookup <- stats::setNames(rep("Arbitrary domain", 6), e$item.vector)
  e$item.facet.lookup <- stats::setNames(c("A", "B", "B", "B", "A", "Arbitrary domain"), e$item.vector)
  e$eligible.items <- list("Arbitrary domain" = e$item.vector)
  e$item_heuristics <- list("Arbitrary domain" = stats::setNames(c(0, 0, 9, 8, 1, 10), e$item.vector))
  e$facets.by.factor <- list("Arbitrary domain" = c("A", "B"))
  e$i.per.f <- c("Arbitrary domain" = 2L)
  e$facet_constraint_mode <- mode
  e$facet_min_per_facet <- 1L
  e$facet_max_imbalance <- 0L
  e$final_structure_repair_candidates_per_factor <- 1L
  e
}

test_that("facet feasibility precedes the broad-factor repair cap", {
  for (mode in c("balanced", "presence")) {
    e <- repair_test_context(mode)
    candidates <- repair_test_function(".repair_candidates_for", e)
    expect_identical(candidates(c(1L, 1L, 0L, 0L, 0L, 0L), "Arbitrary domain", "selected_a"), "legal_a")
    # The broad-factor-only cap would have selected the uncredited item.
    e$eligible.items <- list("Arbitrary domain" = setdiff(e$item.vector, "legal_a"))
    expect_length(candidates(c(1L, 1L, 0L, 0L, 0L, 0L), "Arbitrary domain", "selected_a"), 0L)
  }
})

test_that("soft mode and relaxed balance use the declared policy, not a same-facet shortcut", {
  e <- repair_test_context("soft")
  candidates <- repair_test_function(".repair_candidates_for", e)
  expect_identical(candidates(c(1L, 1L, 0L, 0L, 0L, 0L), "Arbitrary domain", "selected_a"), "uncredited")
  e$facet_constraint_mode <- "balanced"; e$facet_min_per_facet <- 0L; e$facet_max_imbalance <- 2L
  e$eligible.items <- list("Arbitrary domain" = setdiff(e$item.vector, "uncredited"))
  expect_identical(candidates(c(1L, 1L, 0L, 0L, 0L, 0L), "Arbitrary domain", "selected_a"), "other_b1")
  expect_length(candidates(c(1L, 1L, 0L, 0L, 0L, 0L), "Arbitrary domain", "missing"), 0L)
})

test_that("risk ranking preserves fallback targets within a deficient factor", {
  e <- repair_test_context()
  e$psychometric_guard_min_loading <- .4; e$psychometric_guard_max_cross_loading <- Inf
  e$pfa_max_abs_loading <- Inf
  rank <- repair_test_function(".rank_repair_targets", e)
  evaluation <- list(esem_result = list(structure_diagnostics = list(item_diagnostics = data.frame(
    ID = c("selected_a", "selected_b"), assigned_factor = "Arbitrary domain",
    dominant_factor = "Arbitrary domain", primary_loading = c(.2, .3),
    max_cross_loading = 0, simple_structure = TRUE))))
  expect_identical(rank(evaluation, c(1L, 1L, 0L, 0L, 0L, 0L)), c("selected_a", "selected_b"))
})

test_that("shortlist and fit limits interleave ranked moves across arbitrary factors", {
  order_proposals <- repair_test_function(".repair_proposal_order", new.env(parent = asNamespace("SEMANTICA")))
  tasks <- lapply(seq_len(7L), function(i) list(factor = c("A", "A", "A", "B", "B", "C", "C")[i],
    proxy_score = c(100, 90, 80, 2, 1, .2, .1)[i]))
  order <- order_proposals(tasks)
  expect_identical(order, c(1L, 4L, 6L, 2L, 5L, 7L, 3L))
  expect_setequal(vapply(tasks[head(order, 3L)], `[[`, "", "factor"), c("A", "B", "C"))
  expect_true(all(table(vapply(tasks[head(order, 6L)], `[[`, "", "factor")) == 2L))
})

test_that("failed aggregate guards retain every item in affected factors", {
  e <- repair_test_context()
  e$item.vector <- c("a", "b", "c", "d")
  e$psychometric_guard_min_loading <- .4
  e$psychometric_guard_max_cross_loading <- NULL
  e$pfa_max_abs_loading <- Inf
  rank <- repair_test_function(".rank_repair_targets", e)
  evaluation <- list(esem_result = list(factor_ave = c(U = .29, V = .5),
    structure_diagnostics = list(item_diagnostics = data.frame(
      ID = e$item.vector, assigned_factor = c("U", "U", "V", "V"),
      dominant_factor = c("U", "U", "U", "V"), primary_loading = c(.5, .6, .7, .8),
      max_cross_loading = c(.1, .1, .8, .1), simple_structure = c(TRUE, TRUE, FALSE, TRUE)))),
    guard_diagnostics = list(passed = FALSE), guard_violation = list(criteria = data.frame(
      criterion = c("ave", "primary_ge_50", "max_cross_loading"),
      threshold = c(.3, .7, .3), constraint_active = c(TRUE, TRUE, FALSE), violation = c(.01, 0, 0))))
  expect_identical(rank(evaluation, rep(1L, 4)), c("a", "b"))
  # A second genuinely active failed criterion must retain its factor too.
  evaluation$guard_violation$criteria$constraint_active[3] <- TRUE
  evaluation$guard_violation$criteria$violation[3] <- .5
  expect_setequal(rank(evaluation, rep(1L, 4)), e$item.vector)
  evaluation$guard_violation$criteria <- data.frame(criterion = "primary_ge_50",
    threshold = .7, constraint_active = TRUE, violation = .1)
  evaluation$esem_result$structure_diagnostics$item_diagnostics$primary_loading[1] <- .45
  expect_identical(rank(evaluation, rep(1L, 4)), c("a", "b"))
  evaluation$guard_violation$criteria$criterion <- "loading_quality"
  expect_setequal(rank(evaluation, rep(1L, 4)), e$item.vector)
  # No arbitrary factor choice when the factor-level decomposition is absent.
  evaluation$guard_violation$criteria$criterion <- "ave"
  evaluation$esem_result$factor_ave <- NULL
  expect_setequal(rank(evaluation, rep(1L, 4)), e$item.vector)
})

test_that("proposal budgets cover targets within factors before repeating targets", {
  order_proposals <- repair_test_function(".repair_proposal_order", new.env(parent = asNamespace("SEMANTICA")))
  tasks <- lapply(seq_len(8L), function(i) list(
    factor = c(rep("U", 6), "V", "V")[i],
    removed_item = c("a", "a", "b", "b", "c", "c", "d", "d")[i],
    proxy_score = c(100, 99, 10, 9, 1, 0, -1, -2)[i]))
  ordering <- order_proposals(tasks)
  expect_identical(ordering, c(1L, 7L, 3L, 8L, 5L, 2L, 4L, 6L))
  expect_setequal(vapply(tasks[head(ordering, 5)], `[[`, "", "removed_item"), c("a", "b", "c", "d"))
  expect_identical(order_proposals(list()), integer())
})

# Use the native batch evaluator/cache and preference rules with deterministic
# evaluations. This isolates reachability and limits from numerical fit variation.
repair_round_context <- function(values, budget = 60L, width = 10L) {
  e <- new.env(parent = asNamespace("SEMANTICA"))
  make_eval <- function(value) {
    guard <- list(passed = value >= .4, criteria = data.frame(
      criterion = "min_primary_loading", value = value, threshold = .4,
      direction = ">=", multiplier = 1, passed = value >= .4, constraint_active = TRUE))
    list(score = if (guard$passed) .5 else -Inf, proposal_score = .5,
      guard_diagnostics = guard, guard_violation = SEMANTICA:::.semantica_guard_violation(guard),
      esem_result = list(converged = TRUE, admissible = TRUE))
  }
  e$current_evaluation <- make_eval(.3)
  e$current_repair_utility <- .5
  e$hard_guard_repair_pending <- TRUE
  e$final_selection_mode <- "scalar"
  e$final_structure_repair_min_delta <- 1e-6
  e$final_structure_repair_prefilter_top_k <- width
  e$final_structure_repair_max_evals <- budget
  e$repair_deadline <- Inf
  e$repair_evaluated_swaps <- 0L
  e$repair_cache <- new.env(parent = emptyenv())
  e$final_structure_repair_diagnostics <- list(time_budget_exhausted = FALSE)
  e$cl <- NULL
  e$.record_archive_evaluation <- identity
  e$evaluations <- lapply(values, make_eval)
  e$launched <- integer()
  e$repair_eval_raw <- function(task) {
    e$launched <- c(e$launched, task$index)
    e$evaluations[[task$index]]
  }
  for (name in c("repair_utility", "repair_violation", ".repair_proposal_order", "repair_eval_many", ".repair_search_round")) {
    e[[name]] <- repair_test_function(name, e)
  }
  e$tasks <- lapply(seq_along(values), function(i) list(key = as.character(i), index = i,
    vector = as.integer(seq_along(values) == i), factor = "arbitrary", removed_item = "incumbent",
    added_item = paste0("candidate", i), proxy_score = -i))
  e
}

test_that("stalled repair widens to a legal improvement beyond its first block", {
  e <- repair_round_context(c(rep(.2, 12), .35, rep(.2, 11), .45))
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$best$added_item, "candidate13")
  expect_equal(out$record$best_rank, 13)
  expect_equal(out$record$batches, 2)
  expect_equal(out$record$tested, 20)
  expect_identical(e$launched, 1:20)
  expect_identical(out$reason, "preferred_move_found")
  expect_identical(out$record$phase, "feasibility")
  # The later passing form is examinable if preceding moves do not improve.
  e <- repair_round_context(c(rep(.2, 24), .45))
  out <- e$.repair_search_round(e$tasks)
  expect_true(out$best$guard_violation$passed)
  expect_equal(out$record$best_rank, 25)
  expect_equal(e$repair_evaluated_swaps, 25)
})

test_that("a successful first block keeps its best move without extra batches", {
  e <- repair_round_context(c(.35, .45, rep(.2, 23)))
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$best$added_item, "candidate2")
  expect_identical(e$launched, 1:10)
  expect_equal(out$record$batches, 1)
  expect_equal(out$record$untested, 15)
})

test_that("widening respects the exact-fit budget and reports bounded failure", {
  e <- repair_round_context(c(rep(.2, 12), .45), budget = 12L)
  out <- e$.repair_search_round(e$tasks)
  expect_null(out$best)
  expect_identical(out$reason, "max_evals_exhausted")
  expect_equal(out$record$exact_evals, 12)
  expect_identical(e$launched, 1:12)
  expect_equal(out$record$untested, 1)
  e <- repair_round_context(rep(.2, 13), budget = Inf, width = Inf)
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "proposal_list_exhausted")
  expect_equal(out$record$tested, 13)
  expect_equal(out$record$batches, 1)
})

test_that("cache hits and duplicate proposals use no additional exact-fit allowance", {
  e <- repair_round_context(c(.2, .2, .45, .2), budget = 1L, width = 4L)
  assign("1", e$evaluations[[1]], e$repair_cache)
  assign("2", e$evaluations[[2]], e$repair_cache)
  e$tasks[[4]] <- e$tasks[[3]]
  out <- e$.repair_search_round(e$tasks)
  expect_identical(e$launched, 3L)
  expect_equal(out$record$cached, 3)
  expect_equal(out$record$tested, 4)
  expect_equal(out$record$exact_evals, 1)
  expect_true(out$best$guard_violation$passed)
  e <- repair_round_context(rep(.2, 3), budget = 1L, width = 1L)
  for (i in 1:3) assign(as.character(i), e$evaluations[[i]], e$repair_cache)
  out <- e$.repair_search_round(e$tasks)
  expect_equal(out$record$cached, 3)
  expect_equal(out$record$exact_evals, 0)
  expect_identical(out$reason, "proposal_list_exhausted")
})

test_that("repair launches no new work after the deadline including between blocks", {
  e <- repair_round_context(rep(.2, 13))
  e$repair_deadline <- 0
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "max_seconds_exhausted")
  expect_length(e$launched, 0)
  e$clock <- 0
  e$proc.time <- function() c(elapsed = e$clock)
  e$.semantica_lapply_before_deadline <- SEMANTICA:::.semantica_lapply_before_deadline
  environment(e$.semantica_lapply_before_deadline) <- e
  e$repair_eval_raw <- function(task) {
    e$launched <- c(e$launched, task$index)
    e$clock <- 2
    e$evaluations[[task$index]]
  }
  # Advance a deterministic clock during one in-flight evaluation.
  e$repair_deadline <- 1
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "max_seconds_exhausted")
  expect_equal(e$repair_evaluated_swaps, 1)
  expect_equal(out$record$tested, 1)
})

test_that("soft utility repair preserves its threshold and labels refinement", {
  e <- repair_round_context(rep(.2, 13))
  e$hard_guard_repair_pending <- FALSE
  e$final_structure_repair_min_delta <- .1
  e$current_evaluation$score <- .5
  e$evaluations <- lapply(1:13, function(i) {
    x <- e$current_evaluation
    x$score <- if (i == 13) .7 else .55
    x
  })
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$best$added_item, "candidate13")
  expect_identical(out$record$phase, "refinement")
  expect_equal(out$record$tested, 13)
})

test_that("fractional legacy limits cannot stall the batch cursor", {
  e <- repair_round_context(rep(.2, 4), budget = 2.5, width = 2.5)
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "max_evals_exhausted")
  expect_equal(e$repair_evaluated_swaps, 2)
  e <- repair_round_context(rep(.2, 4), width = .5)
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "empty_prefilter")
  expect_length(e$launched, 0)
})

test_that("expiry during the final batch keeps the actual round stop reason", {
  e <- repair_round_context(rep(.2, 3), width = Inf)
  e$clock <- 0
  e$proc.time <- function() c(elapsed = e$clock)
  e$.semantica_lapply_before_deadline <- SEMANTICA:::.semantica_lapply_before_deadline
  environment(e$.semantica_lapply_before_deadline) <- e
  e$repair_deadline <- 1
  e$repair_eval_raw <- function(task) {
    e$clock <- 2
    e$evaluations[[task$index]]
  }
  out <- e$.repair_search_round(e$tasks)
  expect_identical(out$reason, "max_seconds_exhausted")
  expect_equal(out$record$tested, 1)
  expect_equal(out$record$untested, 2)
  expect_equal(e$repair_evaluated_swaps, 1)
})
