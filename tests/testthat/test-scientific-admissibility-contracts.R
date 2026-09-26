test_that("content guard is fail-closed without alignment evidence", {
  df <- data.frame(
    ID = c("a1", "a2", "a3", "b1", "b2", "b3"),
    type = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  )
  expect_error(
    .semantica_preaco_guard_mask(
      df,
      content_alignment_mode = "guard",
      polarity_action = "off"
    ),
    class = "semantica_error_content_guard_unavailable"
  )
})

test_that("quality profiles resolve facet constraints scientifically", {
  expect_identical(semantica_quality_config(profile = "lenient")$facet_constraint_mode, "soft")
  expect_identical(semantica_quality_config(profile = "standard")$facet_constraint_mode, "presence")
  expect_identical(semantica_quality_config(profile = "strict")$facet_constraint_mode, "balanced")
})

test_that("explicit psychometric thresholds imply final admissibility in auto mode", {
  cfg <- semantica_quality_config(
    profile = "standard",
    psychometric_guard_min_ave = 0.35
  )
  expect_identical(cfg$psychometric_guard_action, "final_constraint")

  legacy <- semantica_quality_config(
    profile = "standard",
    psychometric_guard_action = "penalty",
    psychometric_guard_min_ave = 0.35
  )
  expect_identical(legacy$psychometric_guard_action, "penalty")
  expect_identical(cfg$psychometric_guard_ave_warning_action, "penalty")
  expect_true("psychometric_guard_ave_warning_action" %in% names(formals(ACO_with_ESEM)))
})

test_that("uncalibrated ESEM policy prevents a semantic-only hard-guard search", {
  blocked <- .semantica_resolve_uncalibrated_esem_policy(
    "fail_early", dfi_weight_cap_applied = TRUE,
    esem_search_requested = TRUE, psychometric_guard_action = "final_constraint"
  )
  expect_true(blocked$active)
  expect_true(blocked$fail_early)

  screened <- .semantica_resolve_uncalibrated_esem_policy(
    "guard_screen", dfi_weight_cap_applied = TRUE,
    esem_search_requested = TRUE, psychometric_guard_action = "final_constraint"
  )
  expect_true(screened$run_guard_screen)
  expect_false(screened$fail_early)

  ordinary <- .semantica_resolve_uncalibrated_esem_policy(
    "fail_early", dfi_weight_cap_applied = TRUE,
    esem_search_requested = TRUE, psychometric_guard_action = "penalty"
  )
  expect_false(ordinary$active)
  expect_true("uncalibrated_esem_policy" %in% names(formals(semantica_run_custom)))
  expect_true("uncalibrated_esem_policy" %in% names(formals(semantica_esem_config)))
  expect_true("uncalibrated_esem_policy" %in% names(formals(ACO_with_ESEM)))
})

test_that("guard repair seed uses admissible ESEM candidates, not -Inf guard scores", {
  archive <- list(
    list(
      score = -Inf, proposal_score = 0.91,
      esem_result = list(converged = TRUE, admissible = TRUE)
    ),
    list(
      score = -Inf, proposal_score = 0.99,
      esem_result = list(converged = FALSE, admissible = FALSE)
    ),
    list(
      score = -Inf, proposal_score = 0.86,
      esem_result = list(converged = TRUE, admissible = TRUE)
    )
  )
  seed <- .semantica_guard_repair_seed(archive)
  expect_identical(seed$index, 1L)
  expect_equal(seed$proposal_score, 0.91)
  expect_null(.semantica_guard_repair_seed(list(archive[[2L]])))
})

test_that("guard feasibility compares partial repair progress before utility", {
  make_eval <- function(value, proposal) {
    diagnostics <- list(
      passed = value >= .35,
      criteria = data.frame(
        criterion = "ave", value = value, threshold = .35,
        direction = ">=", multiplier = 1,
        passed = value >= .35, constraint_active = TRUE,
        stringsAsFactors = FALSE
      )
    )
    list(
      score = if (value >= .35) .50 else -Inf,
      proposal_score = proposal,
      guard_diagnostics = diagnostics,
      guard_violation = .semantica_guard_violation(diagnostics)
    )
  }
  seed <- make_eval(.15, .95)
  partial <- make_eval(.30, .40)
  passing <- make_eval(.36, .10)

  expect_true(.semantica_compare_guard_feasibility(partial, seed))
  expect_false(.semantica_compare_guard_feasibility(seed, partial))
  expect_true(.semantica_compare_guard_feasibility(passing, partial))
})

test_that("guard archive and repair seed retain the least-violating ESEM candidate", {
  make_entry <- function(value, proposal, id) {
    diagnostics <- list(
      passed = FALSE,
      criteria = data.frame(
        criterion = "ave", value = value, threshold = .35,
        direction = ">=", multiplier = 1, passed = FALSE,
        constraint_active = TRUE, stringsAsFactors = FALSE
      )
    )
    list(
      vec = c(as.integer(id == 1L), as.integer(id == 2L)),
      proposal_score = proposal,
      guard_diagnostics = diagnostics,
      guard_violation = .semantica_guard_violation(diagnostics),
      esem_result = list(converged = TRUE, admissible = TRUE)
    )
  }
  severe <- make_entry(.10, .99, 1L)
  near <- make_entry(.31, .20, 2L)
  archive <- .semantica_update_guard_archive(list(), list(severe, near), elite_k = 2L)
  expect_identical(archive[[1L]]$vec, near$vec)
  seed <- .semantica_guard_repair_seed(list(severe, near))
  expect_identical(seed$index, 2L)
  summary <- .semantica_guard_failure_summary(list(severe, near))
  expect_identical(summary$best_index, 2L)
  expect_equal(summary$failed_criteria$failed_candidates, 2L)

  unresolved <- list(score = -Inf, proposal_score = .99)
  unresolved_summary <- .semantica_guard_failure_summary(list(unresolved, near))
  expect_identical(unresolved_summary$best_index, 2L)
  expect_true(.semantica_compare_guard_feasibility(near, unresolved))
})

test_that("AVE warnings are soft unless explicitly configured as final constraints", {
  fit <- list(
    converged = TRUE, loading_quality = .90, ave = .50, htmt_max = .20,
    ave_warnings = "nonfatal numerical diagnostic",
    structure_diagnostics = list(min_primary_loading = .60, primary_ge_50 = 1)
  )
  soft <- compute_psychometric_guard_penalty(
    fit, min_ave = .30, min_primary_loading = .40,
    min_primary_prop_ge_50 = .70
  )
  strict <- compute_psychometric_guard_penalty(
    fit, min_ave = .30, min_primary_loading = .40,
    min_primary_prop_ge_50 = .70,
    ave_warning_action = "final_constraint"
  )
  soft_diag <- attr(soft, "diagnostics", exact = TRUE)
  strict_diag <- attr(strict, "diagnostics", exact = TRUE)
  expect_true(soft_diag$passed)
  expect_false(strict_diag$passed)
  expect_false(soft_diag$criteria$constraint_active[soft_diag$criteria$criterion == "ave_warnings"])
  expect_true(strict_diag$criteria$constraint_active[strict_diag$criteria$criterion == "ave_warnings"])
})

test_that("facet presence and balance are hard candidate constraints", {
  ids <- paste0("i", 1:6)
  fa <- setNames(rep("F", 6), ids)
  facets <- setNames(c("a", "a", "b", "b", "c", "c"), ids)
  declared <- list(F = c("a", "b", "c", "d"))

  missing <- .semantica_facet_constraint_diagnostics(
    ids, fa, facets, declared, c(F = 6L), mode = "presence"
  )
  expect_false(missing$passed)

  facets2 <- setNames(c("a", "a", "b", "b", "c", "d"), ids)
  balanced <- .semantica_facet_constraint_diagnostics(
    ids, fa, facets2, declared, c(F = 6L), mode = "balanced"
  )
  expect_true(balanced$passed)

  facets3 <- setNames(c("a", "a", "a", "b", "c", "d"), ids)
  imbalanced <- .semantica_facet_constraint_diagnostics(
    ids, fa, facets3, declared, c(F = 6L), mode = "balanced"
  )
  expect_false(imbalanced$passed)
})

test_that("facet pool feasibility rejects an impossible blueprint", {
  eligible <- list(F = c("a1", "a2", "b1", "b2", "c1", "c2", "c3"))
  lookup <- c(a1 = "a", a2 = "a", b1 = "b", b2 = "b", c1 = "c", c2 = "c", c3 = "c")
  out <- .semantica_facet_pool_feasibility(
    eligible,
    item_facet_lookup = lookup,
    facets_by_factor = list(F = c("a", "b", "c", "d")),
    i_per_f = c(F = 6L),
    mode = "presence"
  )
  expect_true(out$active)
  expect_false(out$feasible)
})

test_that("hard facet sampler constructs presence and balanced proposals", {
  ids <- paste0("i", 1:8)
  facets <- setNames(c("a", "a", "a", "b", "b", "b", "c", "c"), ids)
  declared <- c("a", "b", "c")

  set.seed(42)
  presence <- .semantica_sample_factor_items_feasible(
    ids, 6L, item_facet_lookup = facets, declared_facets = declared,
    mode = "presence", min_per_facet = 1L
  )
  expect_length(presence$items, 6L)
  expect_true(all(declared %in% unname(facets[presence$items])))

  set.seed(42)
  balanced <- .semantica_sample_factor_items_feasible(
    ids, 6L, item_facet_lookup = facets, declared_facets = declared,
    mode = "balanced", min_per_facet = 1L, max_imbalance = 1L
  )
  counts <- table(factor(facets[balanced$items], levels = declared))
  expect_length(balanced$items, 6L)
  expect_true(all(counts >= 1L))
  expect_lte(max(counts) - min(counts), 1L)
})

test_that("hard facet sampler fails closed when duplicate guard blocks a required facet", {
  ids <- c("a1", "a2", "b1", "b2")
  facets <- setNames(c("a", "a", "b", "b"), ids)
  clusters <- setNames(c("shared", "a2", "shared", "shared"), ids)
  out <- .semantica_sample_factor_items_feasible(
    ids, 2L, probs = c(100, 0, 100, 0), duplicate_cluster_id = clusters,
    item_facet_lookup = facets, declared_facets = c("a", "b"),
    mode = "presence", min_per_facet = 1L
  )
  expect_lt(length(out$items), 2L)
})

test_that("embedding cache identity includes expected dimensionality", {
  base <- list(
    backend = "custom",
    protocol = "openai_compat",
    embed_url = "http://localhost:9999/v1/embeddings",
    embed_model = "custom-embed",
    embedding_task = "clustering"
  )
  s384 <- c(base, list(embed_dim = 384L))
  s768 <- c(base, list(embed_dim = 768L))
  k384 <- .semantica_text_cache_key("same text", s384, cache_namespace = "x")
  k768 <- .semantica_text_cache_key("same text", s768, cache_namespace = "x")
  expect_false(identical(k384, k768))
})

test_that("cached embeddings with the wrong declared dimension are cache misses", {
  cache_dir <- tempfile("semantica-dim-cache-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  session <- list(
    backend = "custom",
    protocol = "openai_compat",
    embed_url = "http://localhost:9999/v1/embeddings",
    embed_model = "custom-embed",
    embed_dim = 3L,
    embedding_task = "clustering"
  )
  key <- .semantica_text_cache_key("alpha", session, cache_namespace = "dim-test")
  .semantica_embedding_cache_set(key, c(1, 2), cache_dir)
  calls <- 0L
  local_mocked_bindings(
    .call_embed = function(session, texts) {
      calls <<- calls + 1L
      matrix(c(1, 2, 3), nrow = 1L)
    },
    .package = "SEMANTICA"
  )
  out <- semantica_embed(
    data.frame(item_id = "i1", item_text = "alpha"),
    session,
    batch_size = 1L,
    normalize = FALSE,
    cache = TRUE,
    cache_dir = cache_dir,
    cache_namespace = "dim-test",
    verbose = FALSE
  )
  expect_equal(calls, 1L)
  expect_equal(out$embedding_diagnostics$cache_hits, 0L)
  expect_equal(out$embedding_diagnostics$cache_misses, 1L)
  expect_equal(out$embed_dim, 3L)
})

test_that("declared embedding dimension can fail before representation analysis", {
  session <- list(
    backend = "custom", protocol = "openai_compat",
    embed_url = "http://localhost:9999/v1/embeddings",
    embed_model = "custom-embed", embed_dim = 3L,
    embedding_task = "clustering"
  )
  local_mocked_bindings(
    .call_embed = function(session, texts) matrix(c(1, 2), nrow = 1L),
    .package = "SEMANTICA"
  )
  expect_error(
    semantica_embed(
      data.frame(item_id = "i1", item_text = "alpha"), session,
      batch_size = 1L, cache = FALSE, verbose = FALSE,
      embedding_dimension_action = "error"
    ),
    class = "semantica_error_embedding_dimension_mismatch"
  )
  expect_true("embedding_dimension_action" %in% names(formals(semantica_llm_config)))
})

test_that("content evidence distinguishes unavailable alignment from coverage", {
  vd <- .semantica_proxy_validity_dimensions(
    structural_diagnostics = NULL,
    content_coverage = list(facet_coverage = 1),
    content_alignment_available = FALSE
  )
  expect_identical(vd$content$status, "partial_proxy_alignment_unavailable")
  expect_identical(vd$content$definition_alignment, "unavailable")
})

test_that("pipeline execution owns admissibility arguments instead of forwarding them to generation", {
  custom_formals <- names(formals(SEMANTICA:::.semantica_execute_pipeline))
  expect_true(all(c(
    "facet_constraint_mode",
    "facet_min_per_facet",
    "facet_max_imbalance",
    "psychometric_guard_action"
  ) %in% custom_formals))

  generation_formals <- names(formals(semantica_generate_items))
  expect_false(any(c(
    "facet_constraint_mode",
    "facet_min_per_facet",
    "facet_max_imbalance",
    "psychometric_guard_action"
  ) %in% generation_formals))
})
