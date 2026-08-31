test_that("evaporation schedule is independent of stopping patience", {
  cfg <- SEMANTICA:::.semantica_resolve_evaporation(
    semantica_evaporation_config(
      mode = "adaptive", rho_start = 0.35, rho_end = 0.10, horizon = 20L
    ),
    max_total_iter = 20L,
    fixed_evaporation = NULL
  )
  expect_equal(SEMANTICA:::.semantica_evaporation_rho(cfg, 1L), 0.3375)
  expect_equal(SEMANTICA:::.semantica_evaporation_rho(cfg, 20L), 0.10)
  expect_equal(cfg$resolved_horizon, 20L)
})

test_that("changing only search patience preserves the search prefix", {
  semantica_test_mock_esem_unavailable()
  fx <- semantica_test_three_factor_fixture("separable")
  base <- semantica_test_aco_args(fx, seed = 3020L, history_mode = "summary")
  # Remove the legacy alias so this test changes *only* search_patience.
  base$max.iter <- NULL
  base$max_total_iter <- 3L
  base$archive_stable_window <- 100L
  base$structural_archive_stable_window <- 100L
  base$evaporation <- semantica_evaporation_config(horizon = 20L)

  a <- do.call(ACO_with_ESEM, c(base, list(search_patience = 10L)))
  b <- do.call(ACO_with_ESEM, c(base, list(search_patience = 100L)))

  expect_identical(a$total_iterations, b$total_iterations)
  expect_identical(sort(a$best_items), sort(b$best_items))
  expect_equal(a$best_objective, b$best_objective, tolerance = 1e-12)
  expect_equal(a$pheromone, b$pheromone, tolerance = 1e-12)
})

test_that("evidence archives reject cross-schema score competition", {
  v1 <- c(1L, 0L, 1L, 0L)
  v2 <- c(0L, 1L, 0L, 1L)
  v3 <- c(1L, 1L, 0L, 0L)
  names(v1) <- names(v2) <- names(v3) <- paste0("i", 1:4)

  semantic <- list(
    list(vec = v1, semantic_score = 0.20, score_schema = "semantic-v1"),
    list(vec = v2, semantic_score = 0.40, score_schema = "semantic-v1"),
    # Numerically huge but methodologically incompatible and therefore excluded.
    list(vec = v3, semantic_score = 999, score_schema = "pfa-proposal-v1")
  )
  out <- SEMANTICA:::update_elite_archive(
    list(), semantic, elite_k = 2L,
    rank_field = "semantic_score", score_schema = "semantic-v1"
  )
  expect_length(out, 2L)
  expect_identical(SEMANTICA:::solution_signature(out[[1L]]$vec),
                   SEMANTICA:::solution_signature(v2))
  expect_false(any(vapply(out, function(x) identical(x$score_schema, "pfa-proposal-v1"), logical(1L))))
})

test_that("stratified finalists deduplicate identities without cross-schema ranking", {
  v1 <- c(1L, 0L, 1L, 0L)
  v2 <- c(0L, 1L, 0L, 1L)
  v3 <- c(1L, 1L, 0L, 0L)
  names(v1) <- names(v2) <- names(v3) <- paste0("i", 1:4)
  archives <- list(
    semantic = list(
      list(vec = v1, semantic_score = 999, score_schema = "semantic-v1"),
      list(vec = v3, semantic_score = 998, score_schema = "semantic-v1")
    ),
    pfa = list(list(vec = v1, proposal_score = 0.01, score_schema = "pfa-proposal-v1")),
    esem = list(list(vec = v2, esem_guided_score = -100, score_schema = "esem-guided-v1"))
  )
  out <- SEMANTICA:::.semantica_stratified_finalists(
    archives, c("semantic", "pfa", "esem"), budget = 2L
  )
  expect_length(out, 2L)
  # ESEM and PFA/semantic representation is based on archive rank/round-robin,
  # not the incomparable raw magnitudes 999, 0.01, and -100.
  expect_true(SEMANTICA:::solution_signature(v2) %in%
                vapply(out, function(x) SEMANTICA:::solution_signature(x$vec), character(1L)))
  expect_identical(anyDuplicated(vapply(out, function(x) SEMANTICA:::solution_signature(x$vec), character(1L))), 0L)
})

test_that("PFA unavailability is distinct from a zero PFA score", {
  semantica_test_mock_esem_unavailable()
  testthat::local_mocked_bindings(
    compute_pfa_diagnostics = function(...) list(
      available = FALSE,
      score = NA_real_,
      note = "injected unavailable PFA",
      missing_factors = character(0L)
    ),
    .package = "SEMANTICA"
  )
  fx <- semantica_test_three_factor_fixture("separable")
  args <- semantica_test_aco_args(fx, seed = 3021L, history_mode = "none")
  args$pfa_mode <- "objective"
  args$pfa_weight <- 0.40
  args$pfa_every <- 1L
  args$run_pfa_during_search <- TRUE
  args$pfa_failure_policy <- "semantic_fallback"
  args$archive_stable_window <- 100L
  args$structural_archive_stable_window <- 100L

  out <- do.call(ACO_with_ESEM, args)
  expect_equal(out$pfa_search_successes, 0L)
  expect_true(is.na(out$pfa_score))
  expect_equal(out$search_objective_score, out$semantic_objective_score, tolerance = 1e-12)

  args$pfa_failure_policy <- "stop"
  expect_error(
    do.call(ACO_with_ESEM, args),
    "PFA was unavailable|PFA was unavailable for every candidate"
  )
})


test_that("embedding capabilities change preparation policy, not psychometric thresholds", {
  generic <- SEMANTICA:::.semantica_embedding_policy("unknown-future-model", task = "auto")
  expect_identical(generic$analysis_intent, "psychometric_similarity")
  expect_identical(generic$capability_source, "generic_safe_default")
  expect_null(generic$prefix)

  nomic <- SEMANTICA:::.semantica_embedding_policy("nomic-embed-text", task = "auto")
  expect_identical(nomic$resolved_task, "clustering")
  expect_match(nomic$prefix, "^clustering:")
  expect_identical(nomic$capability_source, "registered_model_capability")

  custom <- semantica_embedding_spec(
    instruction_mode = "prefix",
    task_map = c(psychometric_similarity = "similarity"),
    prefix_template = "{task} | "
  )
  custom_policy <- SEMANTICA:::.semantica_embedding_policy(
    "future-custom-model", task = "auto", embedding_spec = custom
  )
  expect_identical(custom_policy$resolved_task, "similarity")
  expect_match(custom_policy$prefix, "^similarity \\|")
})

test_that("representation spectral health is descriptive and finite for a valid cosine Gram matrix", {
  x <- rbind(c(1, 0, 0), c(.9, .1, 0), c(0, 1, 0), c(0, .9, .1))
  x <- x / sqrt(rowSums(x^2))
  m <- tcrossprod(x)
  rownames(m) <- colnames(m) <- paste0("i", 1:4)
  d <- SEMANTICA:::.cosine_diagnostics(m)
  expect_true(is.finite(d$effective_rank))
  expect_gte(d$effective_rank, 1)
  expect_lte(d$effective_rank, 4)
  expect_true(is.finite(d$top_eigen_share))
  expect_true(is.na(d$possible_anisotropy))
})

test_that("resource reset is a no-op when SEMANTICA owns no worker pool", {
  skip_if_not_installed("parallelly")
  out <- semantica_reset_resources()
  expect_named(out, c("stopped", "reaped", "failed"))
  expect_length(out$failed, 0L)
})

test_that("typed evidence records distinguish computed, fallback, and not-requested states", {
  computed <- SEMANTICA:::.semantica_evidence_record(
    "computed", value = 1, participant_based = FALSE,
    selection_conditioned = TRUE, evidence_scope = "selected set"
  )
  fallback <- SEMANTICA:::.semantica_evidence_record(
    "fallback", reason = "injected unavailable structural proxy"
  )
  not_requested <- SEMANTICA:::.semantica_evidence_record("not_requested")

  expect_s3_class(computed, "semantica_evidence_record")
  expect_identical(computed$status, "computed")
  expect_identical(computed$value, 1)
  expect_true(computed$selection_conditioned)
  expect_identical(fallback$status, "fallback")
  expect_match(fallback$reason, "unavailable")
  expect_identical(not_requested$status, "not_requested")
})

test_that("finalist reservoir represents every active archive when budget permits", {
  mk <- function(sig, schema, field, score) {
    v <- integer(6L); v[as.integer(strsplit(sig, "-")[[1L]])] <- 1L
    names(v) <- paste0("i", seq_along(v))
    x <- list(vec = v, score_schema = schema)
    x[[field]] <- score
    x
  }
  archives <- list(
    semantic = list(mk("1-2", "semantic-v1", "semantic_score", 0.9)),
    pfa = list(mk("3-4", "pfa-proposal-v1", "proposal_score", 0.2)),
    esem = list(mk("5-6", "esem-guided-v1", "esem_guided_score", -10))
  )
  out <- SEMANTICA:::.semantica_stratified_finalists(
    archives, c("semantic", "pfa", "esem"), budget = 3L
  )
  expect_length(out, 3L)
  expect_setequal(
    unlist(lapply(out, `[[`, "source_tracks"), use.names = FALSE),
    c("semantic", "pfa", "esem")
  )
})
