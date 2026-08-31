test_that("casual interface detects and adapts a unidimensional model", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    scale_name = "One factor scale",
    scale_description = "One intended latent dimension.",
    factors = list(Trait = "A single substantive latent trait."),
    llm = "ollama",
    aco = "standard",
    verbose = FALSE
  )

  expect_identical(captured$item_counts$selected, 4L)
  expect_identical(captured$pfa$mode, "off")
  expect_false(captured$pfa$during_search)
  expect_equal(captured$pfa$weight, 0)
  expect_false(captured$pfa$unit_diagnostics)
  expect_identical(captured$esem$rotation, "none")
  expect_identical(captured$esem$rotation_args, list())
  expect_identical(captured$esem$proxy_reference_n, "auto")
  expect_identical(captured$esem$score_mode, "structure_weighted")
  expect_identical(captured$esem$cadence_mode, "fixed")
  expect_true(captured$run_esem_during_search)
  expect_identical(captured$esem_every, 10L)
  expect_identical(captured$ants, 60L)
  expect_identical(captured$search_patience, 40L)
  expect_identical(captured$max_total_iter, 60L)

  expect_identical(result$run_config$dimensionality, "unidimensional")
  expect_true(result$run_config$unidimensional_adaptation)
  expect_true(result$run_config$selected_items_auto)
  expect_identical(result$run_config$selected_items_default_rule,
                   "4_for_overidentified_one_factor_proxy")
  expect_identical(result$run_config$pfa_status, "not_applicable_unidimensional")
  expect_identical(result$run_config$esem_rotation, "none")
})

test_that("casual one-factor mode rejects a saturated three-item final form", {
  expect_error(
    semantica_run(
      "One factor", "Description",
      factors = list(Trait = "A substantive trait definition."),
      selected_items = 3L,
      llm = "ollama", verbose = FALSE
    ),
    "at least 4 selected items"
  )
})

test_that("multidimensional casual defaults remain three items per factor", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    "Two factor", "Description",
    factors = list(A = "Factor A definition.", B = "Factor B definition."),
    llm = "ollama", verbose = FALSE
  )

  expect_identical(captured$item_counts$selected, 3L)
  expect_identical(captured$pfa$mode, "objective")
  expect_true(captured$pfa$during_search)
  expect_identical(captured$pfa$rotation, "oblimin")
  expect_identical(captured$esem$rotation, "geomin")
  expect_identical(result$run_config$dimensionality, "multidimensional")
  expect_true(result$run_config$selected_items_auto)
  expect_identical(result$run_config$selected_items_default_rule, "3_per_factor")
})

test_that("one-factor semantic objective does not divide by absent between-factor evidence", {
  ids <- paste0("i", 1:4)
  m <- matrix(0.20, 4, 4, dimnames = list(ids, ids))
  diag(m) <- 1
  fa <- stats::setNames(rep("F", 4), ids)

  out <- SEMANTICA:::compute_semantic_sim_index_v2(
    sim_matrix = m,
    selected_items = ids,
    factor_assignment = fa,
    factors = "F",
    within_similarity_target = 0.60,
    within_similarity_band = 0.08,
    within_similarity_weight = 1.15,
    between_similarity_weight = 1.00
  )

  expect_identical(out$dimensionality_mode, "unidimensional")
  expect_true(unname(out$evidence_components[["within"]]))
  expect_false(unname(out$evidence_components[["between"]]))
  expect_true(is.na(out$mean_between))
  expect_true(is.na(out$q90_between))
  expect_true(is.na(out$max_between))
  expect_equal(out$similarity_index, out$within_target_loss, tolerance = 1e-10)
})

test_that("HTMT is explicitly not applicable for one factor", {
  out <- SEMANTICA:::compute_htmt_esem(
    esem_fit = NULL,
    factors = "Trait",
    threshold = 0.85
  )
  expect_true(is.na(out$max_cor))
  expect_true(is.na(out$violations))
  expect_identical(out$status, "not_applicable")
  expect_identical(out$method, "not_applicable_unidimensional")
  expect_match(out$reason, "at least two")
})

test_that("factor semantic diagnostics retain within-only information for one factor", {
  ids <- paste0("i", 1:4)
  m <- matrix(c(
    1,.5,.4,.3,
    .5,1,.45,.35,
    .4,.45,1,.55,
    .3,.35,.55,1
  ), 4, 4, byrow = TRUE, dimnames = list(ids, ids))
  fa <- stats::setNames(rep("Trait", 4), ids)

  out <- semantica_factor_semantic_diagnostics(m, fa)
  expect_equal(nrow(out), 1L)
  expect_identical(out$status[[1L]], "within_only")
  expect_true(is.finite(out$within_mean[[1L]]))
  expect_true(is.na(out$between_mean[[1L]]))
  expect_true(is.na(out$gap[[1L]]))
  expect_true(is.na(out$stochastic_superiority[[1L]]))
})

test_that("selection context remains computed for one-factor cohesion", {
  ids <- paste0("i", 1:6)
  m <- matrix(0.30, 6, 6, dimnames = list(ids, ids))
  diag(m) <- 1
  m[1:4, 1:4] <- 0.45
  diag(m) <- 1
  fa <- stats::setNames(rep("Trait", 6), ids)

  out <- semantica_selection_context(m, fa, ids[1:4])
  expect_identical(out$status, "computed")
  expect_identical(out$dimensionality_mode, "unidimensional")
  expect_true(is.finite(out$pool$within_mean))
  expect_true(is.finite(out$selected$within_mean))
  expect_true(is.na(out$pool_gap))
  expect_true(is.na(out$selected_gap))
  expect_true(is.finite(out$within_cohesion_change))
})

test_that("one-factor structural evidence avoids vacuous comparative flags", {
  fake_esem <- list(
    converged = TRUE,
    admissible = TRUE,
    dimensionality_mode = "unidimensional",
    cfi = .97, tli = .96, rmsea = .04, srmr = .03, ave = .55,
    structure_diagnostics = list(
      dimensionality_mode = "unidimensional",
      mean_primary_loading = .70,
      median_primary_loading = .71,
      min_primary_loading = .55,
      primary_ge_40 = 1,
      primary_ge_50 = 1,
      mean_abs_residual = .03,
      q95_abs_residual = .06,
      max_abs_residual = .08,
      mean_residual = -.01,
      max_centered_residual = .07,
      q95_centered_residual = .05,
      max_abs_centered_residual = .07
    ),
    admissibility = list(details = list(converged = TRUE), reasons = character())
  )
  m <- matrix(.30, 4, 4)
  diag(m) <- 1
  dimnames(m) <- list(paste0("i", 1:4), paste0("i", 1:4))
  ctx <- list(
    pool = list(within_mean = .40),
    selected = list(within_mean = .45),
    within_cohesion_change = .05
  )

  u <- SEMANTICA:::.semantica_unidimensional_proxy_diagnostics(fake_esem, m, ctx)
  expect_identical(u$dimensionality_mode, "unidimensional")
  expect_identical(u$htmt_status, "not_applicable_unidimensional")
  expect_identical(u$pfa_partition_status, "not_applicable_unidimensional")
  expect_true(is.finite(u$eigenvalue_ratio_1_to_2))
  expect_true(is.finite(u$first_eigenvalue_share))
  expect_true(is.finite(u$max_abs_centered_residual))

  state <- semantica_esem_state(esem_result = fake_esem)
  expect_identical(state$structural_quality, "admissible_unidimensional_proxy")
  expect_length(state$quality_flags, 0L)

  d <- semantica_pfa_esem_discrepancy(NULL, fake_esem)
  expect_identical(d$state, "not_applicable_unidimensional")
  expect_true(is.na(d$metrics$esem_htmt_max))
})

test_that("evidence status marks construct separation not applicable in one-factor mode", {
  result <- list(
    dimensionality_mode = "unidimensional",
    construct_coverage = list(semantic_alignment_available = TRUE),
    content_alignment = list(available = TRUE)
  )
  tab <- semantica_evidence_status(result)
  row <- tab[tab$evidence == "semantic_construct_separation", , drop = FALSE]
  expect_identical(row$status[[1L]], "not_applicable_unidimensional")
})

test_that("multidimensional HTMT failure numerics retain the pre-0.4.1 contract", {
  out <- SEMANTICA:::compute_htmt_esem(
    esem_fit = NULL,
    factors = c("A", "B"),
    threshold = 0.85
  )
  expect_equal(out$max_cor, 1.0)
  expect_true(is.infinite(out$violations))
  expect_identical(out$status, "unavailable")
})
