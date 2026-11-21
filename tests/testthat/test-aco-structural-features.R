.structural_feature_fixture <- function() {
  ids <- paste0("s", 1:14)
  assignment <- setNames(rep(c("Trait A", "Trait B"), each = 7L), ids)
  lambda <- matrix(.03, 14, 2, dimnames = list(ids, c("Trait A", "Trait B")))
  lambda[cbind(1:14, rep(1:2, each = 7))] <- rep(seq(.45, .85, length.out = 7), 2)
  observed <- tcrossprod(lambda); diag(observed) <- 1
  list(ids = ids, assignment = assignment, observed = observed,
    df = data.frame(item = ids, factor = unname(assignment), facet = rep(c("a", "b", "a", "b", "a", "b", "a"), 2)))
}

.structural_feature_aco <- function(...) {
  x <- .structural_feature_fixture()
  args <- list(cosine_sim_matrix = x$observed, df = x$df,
    i.per.f = c("Trait A" = 4L, "Trait B" = 4L), ants = 4L,
    max.iter = 2L, max_total_iter = 2L, esem_every = 1L, esem_eval_top_k = 2L,
    max_esem_fits = 3L, esem_cadence_mode = "fixed",
    semantic_esem_score_mode = "continuous_balanced", esem_failure_policy = "semantic_fallback",
    dfi_warmup_iters = 0L, pfa_mode = "off", run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE,
    final_dddfi = FALSE, final_dfi_recalibrate = FALSE, final_equivtest = FALSE,
    rotation = "oblimin", use_parallel = FALSE, n.cores = 1L, seed = 246L, verbose = FALSE)
  do.call(ACO_with_ESEM, utils::modifyList(args, list(...)))
}

test_that("forced native refitting bypasses every cached ESEM score", {
  x <- .structural_feature_fixture(); item.vector <- x$ids[c(1:4, 8:11)]
  item.factor.lookup <- x$assignment[item.vector]; factors <- unique(unname(item.factor.lookup))
  list.items <- split(item.vector, item.factor.lookup)
  cosine_sim_matrix <- x$observed[item.vector, item.vector]
  model_info <- list(pfa_mode = "off", pfa_max_abs_loading = Inf,
    facet_constraint_mode = "soft", psychometric_guard_weight = 0,
    psychometric_guard_action = "report_only", n_obs = 500L, estimator = "ML",
    rotation = "oblimin", rotation_args = list(), full_esem_iter_max = 2000L,
    fast_esem_iter_max = 100L, semantic_esem_score_mode = "continuous_balanced")
  vector <- setNames(rep(1L, 8), item.vector); key <- make_solution_key(vector)
  solution_cache <- new.env(parent = emptyenv()); calls <- list()
  native_fit <- run_esem_on_matrix
  local_mocked_bindings(run_esem_on_matrix = function(..., iter_max, fallback) {
    calls[[length(calls) + 1L]] <<- c(iter_max = iter_max, fallback = fallback)
    native_fit(..., iter_max = iter_max, fallback = fallback)
  }, .package = "SEMANTICA")
  for (cached_score in c(.8, NA_real_, 0)) {
    cache_set(solution_cache, key, list(sem_score = .6, esem_score = cached_score,
      guard_penalty = 1, fit_result = list(converged = FALSE, admissible = FALSE)))
    expect_equal(fit.function.v2(vector, run_esem_now = TRUE, effective_esem_weight = .5,
      run_pfa_now = FALSE, solution_cache = solution_cache), .3 + .5 * cached_score)
    payload <- fit.function.v2(vector, run_esem_now = TRUE, effective_esem_weight = .5,
      run_pfa_now = FALSE, solution_cache = solution_cache, force_full_esem = TRUE,
      force_esem_refit = TRUE, return_payload = TRUE)
    expect_true(payload$cache_entry$esem_fit_started)
    expect_true(payload$cache_entry$fit_result$admissible)
    expect_true(is.finite(payload$score))
  }
  expect_length(calls, 3L)
  expect_true(all(vapply(calls, function(z) z[["iter_max"]] == 2000 && z[["fallback"]] == 1, logical(1))))
})

test_that("distinct shortlist budgets forms and retains duplicate-ant score mapping", {
  a <- c(1L, 1L, 0L, 0L); b <- c(1L, 0L, 1L, 0L); c <- c(0L, 0L, 1L, 1L)
  vectors <- c(rep(list(a), 20L), list(b, c)); scores <- c(rep(.9, 20), .8, .7)
  legacy <- .semantica_esem_shortlist(scores, 2L, solutions = vectors)
  chosen <- .semantica_esem_shortlist(scores, 2L, solutions = vectors, policy = "distinct")
  expect_identical(legacy, 1:2)
  expect_identical(chosen, 1:21)
  broker <- .semantica_new_evaluation_broker(1L)
  plan <- .semantica_plan_esem_batch(broker, chosen, vectors, new.env(parent = emptyenv()))
  expect_length(plan$indices, 1L)
  expect_length(plan$request_indices, 20L)
  expect_equal(.semantica_evaluation_snapshot(broker)$esem_fits_started, 1L)
  expect_length(.semantica_esem_shortlist(rep(NA_real_, 22), 2L, solutions = vectors, policy = "distinct"), 0L)
  set.seed(9)
  explored <- .semantica_esem_shortlist(scores, 2L, 1L, vectors, policy = "distinct")
  expect_equal(length(unique(vapply(vectors[explored], make_solution_key, character(1)))), 2L)
})

test_that("native failed-fast checkpoint actually launches and accounts for its full retry", {
  native_fit <- run_esem_on_matrix
  local_mocked_bindings(run_esem_on_matrix = function(..., fallback = TRUE) {
    if (!isTRUE(fallback)) return(list(fit = NULL, solver_attempts_started = 1L,
      attempts = list(), accepted_attempt = NA_integer_, fallback_used = FALSE))
    native_fit(..., fallback = fallback)
  }, .package = "SEMANTICA")
  result <- .structural_feature_aco(ants = 2L, max_total_iter = 1L, max_esem_fits = 1L,
    esem_eval_top_k = 1L, esem_failure_policy = "stop")
  expect_equal(result$evaluation_telemetry$esem_fits_started, 1L)
  expect_equal(result$evaluation_telemetry$esem_full_retries_started, 1L)
  expect_equal(result$evaluation_telemetry$esem_full_retries_admissible, 1L)
  expect_true(any(result$evaluation_telemetry$esem_events$stage == "search_full_retry"))
})

test_that("distinct pheromone elites are invariant to cloned ants", {
  vectors <- lapply(1:20, function(i) as.integer(seq_len(20) == i))
  objectives <- seq(1, .1, length.out = 20)
  pheromone <- matrix(1, 20, 2, dimnames = list(NULL, c("not_selected", "selected")))
  distinct <- update_pheromone(pheromone, vectors, objectives, .1, 20, "top_elite_distinct")
  cloned <- update_pheromone(pheromone, c(rep(vectors[1], 70), vectors[-1]),
    c(rep(1, 70), objectives[-1]), .1, 89, "top_elite_distinct")
  expect_equal(distinct, cloned)
  expect_gt(sum(distinct[, "selected"] > .9), 1L)
  expect_true(all(distinct >= .01 & distinct <= 50))
  empty <- update_pheromone(pheromone, vectors, rep(NA_real_, 20), .1, 20, "top_elite_distinct")
  expect_identical(dimnames(empty), dimnames(pheromone))
  expect_equal(unname(empty), unname(pheromone * .9))
})

test_that("bounded polishing preserves constraints and the incumbent", {
  vector <- setNames(c(1L, 0L, 1L, 0L, 0L), letters[1:5])
  pools <- list(F1 = letters[1:2], F2 = letters[3:5]); h <- list(F1 = c(a = 1, b = 2), F2 = c(c = 1, d = 3, e = 2))
  objective <- function(v) list(score = sum(which(v == 1L)))
  checked <- list()
  feasible <- function(v) {
    expect_equal(sum(v[1:2]), 1L); expect_equal(sum(v[3:5]), 1L)
    !isTRUE(v["d"] == 1L)
  }
  evaluate <- function(v) { checked[[length(checked) + 1L]] <<- v; objective(v) }
  out <- .semantica_objective_polish(vector, objective(vector), pools, h, evaluate, feasible, 2L, 10)
  expect_lte(length(checked), 2L); expect_gt(out$evaluation$score, objective(vector)$score)
  expect_true(all(vapply(checked, function(v) v["d"] == 0L, logical(1))))
  expect_true(all(diff(c(out$diagnostics$score_before,
    out$diagnostics$history$candidate_score[out$diagnostics$history$accepted])) > 0))
  bad <- .semantica_objective_polish(vector, list(score = 10), pools, h,
    function(v) list(score = -Inf), feasible, 2L, 10)
  expect_identical(bad$vector, vector); expect_equal(bad$diagnostics$accepted, 0L)
  timed <- .semantica_objective_polish(vector, objective(vector), pools, h,
    function(v) stop("Must not launch"), feasible, 2L, .Machine$double.eps)
  expect_equal(timed$diagnostics$evaluated, 0L)
  expect_identical(timed$diagnostics$reason, "max_seconds_exhausted")
  no_moves <- .semantica_objective_polish(vector, objective(vector),
    list(F1 = "a", F2 = "c"), h, evaluate, feasible, 2L, 10)
  expect_equal(no_moves$diagnostics$evaluated, 0L)
})

test_that("native target and PST rotations preserve the fitted covariance", {
  x <- .structural_feature_fixture(); ids <- x$ids[c(1:4, 8:11)]
  assignment <- x$assignment[ids]; factors <- unique(unname(assignment))
  syntax <- build_esem_syntax_safe(ids, assignment, factors)
  fits <- lapply(c("oblimin", "target", "pst"), function(rotation) {
    args <- prepare_esem_rotation_args(rotation, list(rstarts = 5L), ids, assignment, factors)
    set.seed(42L)
    out <- run_esem_on_matrix(syntax, x$observed[ids, ids], 500L, rotation = rotation,
      rotation_args = args, return_diagnostics = TRUE, fallback = FALSE)
    expect_true(is_admissible_esem_fit(out$fit))
    out$fit
  })
  for (i in 2:3) expect_equal(lavaan::fitted(fits[[i]])$cov, lavaan::fitted(fits[[1]])$cov, tolerance = 1e-5)
  for (i in seq_along(fits)) {
    result <- extract_and_score_esem(fits[[i]], x$observed[ids, ids], assignment, factors,
      score_mode = "continuous_balanced")
    expect_true(result$continuous_utility$available)
    expect_equal(nrow(result$continuous_utility$per_factor), 2L)
  }
})

test_that("native opt-in polishing respects report-only references and fit budgets", {
  original <- .structural_feature_aco()
  polished <- .structural_feature_aco(objective_polish = TRUE, objective_polish_max_evals = 3L,
    objective_polish_max_seconds = 30, psychometric_guard_min_loading = .99)
  expect_equal(polished$structural_guard$action, "report_only")
  expect_gte(polished$objective_context$value + 1e-7, original$objective_context$value)
  expect_lte(polished$objective_polish$evaluated, 3L)
  expect_lte(polished$objective_polish$esem_fits, 3L)
  expect_lte(polished$evaluation_telemetry$esem_fits_started, 3L)
  counts <- table(.structural_feature_fixture()$assignment[polished$best_items])
  expect_equal(as.integer(counts[c("Trait A", "Trait B")]), c(4L, 4L))
})

test_that("new search policies are reproducible in serial and worker execution", {
  args <- list(esem_shortlist_policy = "distinct", esem_rng_policy = "candidate",
    pheromone_update = "top_elite_distinct", objective_polish = TRUE,
    objective_polish_max_evals = 2L, objective_polish_max_seconds = 30)
  serial <- do.call(.structural_feature_aco, args)
  parallel <- do.call(.structural_feature_aco, c(args, list(use_parallel = TRUE, n.cores = 2L)))
  expect_identical(parallel$best_items, serial$best_items)
  expect_equal(parallel$objective_context$value, serial$objective_context$value, tolerance = 1e-5)
  # Numerical rotation differences can change later proposals. Shared forms
  # must retain their fitting seeds regardless of their checkpoint or worker.
  a <- serial$reproducibility$search_esem_task_seeds
  b <- parallel$reproducibility$search_esem_task_seeds
  shared <- merge(a, b, by = "candidate_key")
  expect_gte(nrow(shared), 2L)
  expect_identical(shared$seed.x, shared$seed.y)
  expect_identical(a$candidate_key[a$iteration == 1L], b$candidate_key[b$iteration == 1L])
  expect_equal(nrow(parallel$reproducibility$search_esem_parallel_fallbacks), 0L)
  expect_equal(nrow(parallel$reproducibility$final_parallel_fallbacks), 0L)
})

test_that("polishing supports unequal counts and hard facets without an ESEM search objective", {
  result <- .structural_feature_aco(i.per.f = c("Trait A" = 3L, "Trait B" = 4L),
    run_esem_during_search = FALSE, esem_weight = 0, facet_constraint_mode = "presence",
    objective_polish = TRUE, objective_polish_max_evals = 2L)
  x <- .structural_feature_fixture(); rows <- x$df[match(result$best_items, x$ids), ]
  expect_equal(as.integer(table(rows$factor)), c(3L, 4L))
  expect_true(all(table(rows$factor, rows$facet) >= 1L))
  expect_lte(result$objective_polish$evaluated, 2L)
  expect_equal(result$objective_polish$esem_fits, 0L)
})

test_that("objective polishing retains hard guard feasibility and a consistent Pareto portfolio", {
  result <- .structural_feature_aco(final_selection_mode = "pareto", objective_polish = TRUE,
    objective_polish_max_evals = 2L, psychometric_guard_action = "final_constraint",
    psychometric_guard_min_ave = .01, psychometric_guard_min_loading = .01,
    psychometric_guard_min_primary_ge_50 = 0)
  expect_true(result$structural_guard$passed)
  expect_equal(sum(result$pareto_archive$selected), 1L)
  expect_true(result$pareto_archive$pareto_front[result$pareto_archive$selected])
  expect_identical(result$pareto_archive$archive_index[result$pareto_archive$selected], 1L)
  expect_gte(result$objective_polish$score_after, result$objective_polish$score_before)
})

test_that("new options reject invalid values and preserve compatibility defaults", {
  expect_identical(semantica_esem_config()$shortlist_policy, "ant")
  expect_identical(semantica_esem_config()$rng_policy, "legacy")
  expect_false(semantica_quality_config()$objective_polish)
  expect_error(semantica_esem_config(shortlist_policy = "unknown"), "arg")
  for (value in list(NULL, NA, Inf, 0, -1, c(1, 2), "2"))
    expect_error(semantica_quality_config(objective_polish_max_seconds = value), "objective_polish_max_seconds")
  expect_error(semantica_quality_config(objective_polish_max_evals = 1.5), "objective_polish_max_evals")
  expect_error(semantica_quality_config(objective_polish = 1), "objective_polish")
})

test_that("unseeded optional master seeds do not advance the initial proposal stream", {
  states <- list()
  local_mocked_bindings(.semantica_sample_factor_items_feasible = function(...) {
    states[[length(states) + 1L]] <<- .Random.seed
    stop("initial_proposal_rng_probe")
  }, .package = "SEMANTICA")
  for (options in list(list(), list(objective_polish = TRUE), list(esem_rng_policy = "candidate"))) {
    set.seed(709L)
    expect_error(do.call(.structural_feature_aco, c(list(seed = NULL), options)), "initial_proposal_rng_probe")
  }
  expect_length(states, 3L)
  expect_identical(states[[2L]], states[[1L]])
  expect_identical(states[[3L]], states[[1L]])
})
