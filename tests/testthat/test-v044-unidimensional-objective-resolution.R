test_that("unidimensional semantic target loss has ranking resolution inside the band", {
  ids <- paste0("i", 1:4)
  fa <- stats::setNames(rep("Trait", 4), ids)

  mk <- function(offdiag) {
    m <- matrix(offdiag, 4, 4, dimnames = list(ids, ids))
    diag(m) <- 1
    m
  }

  at_target <- SEMANTICA:::compute_semantic_sim_index_v2(
    sim_matrix = mk(0.60), selected_items = ids, factor_assignment = fa,
    factors = "Trait", within_similarity_target = 0.60,
    within_similarity_band = 0.08, redundancy_threshold = 0.90
  )
  inside_band <- SEMANTICA:::compute_semantic_sim_index_v2(
    sim_matrix = mk(0.64), selected_items = ids, factor_assignment = fa,
    factors = "Trait", within_similarity_target = 0.60,
    within_similarity_band = 0.08, redundancy_threshold = 0.90
  )

  expect_identical(at_target$within_target_loss_mode, "huber_target_centered")
  expect_equal(at_target$within_target_loss, 0, tolerance = 1e-12)
  expect_gt(inside_band$within_target_loss, at_target$within_target_loss)
  expect_gt(at_target$sem_score, inside_band$sem_score)
  expect_lt(inside_band$within_target_loss, 0.08)
})

test_that("multidimensional semantic target band remains backward compatible", {
  ids <- paste0("i", 1:6)
  fa <- stats::setNames(c(rep("A", 3), rep("B", 3)), ids)
  m <- matrix(0.20, 6, 6, dimnames = list(ids, ids))
  diag(m) <- 1
  m[1:3, 1:3] <- 0.62
  m[4:6, 4:6] <- 0.62
  diag(m) <- 1

  out <- SEMANTICA:::compute_semantic_sim_index_v2(
    sim_matrix = m, selected_items = ids, factor_assignment = fa,
    factors = c("A", "B"),
    within_similarity_target = c(A = 0.60, B = 0.60),
    within_similarity_band = 0.08, redundancy_threshold = 0.90
  )

  expect_identical(out$within_target_loss_mode, "band_violation")
  expect_equal(out$within_target_loss, 0, tolerance = 1e-12)
  expect_true(is.finite(out$mean_between))
})

test_that("fixed casual ESEM cadence is exact while advanced adaptive cadence is preserved", {
  expect_identical(
    SEMANTICA:::.semantica_resolve_esem_interval(10L, 0.80, mode = "fixed"),
    10L
  )
  expect_identical(
    SEMANTICA:::.semantica_resolve_esem_interval(10L, 0.80, mode = "adaptive"),
    8L
  )
  expect_identical(semantica_esem_config()$cadence_mode, "adaptive")
  expect_identical(semantica_aco_config("standard")$esem_cadence_mode, "fixed")
  expect_identical(semantica_aco_config("full")$esem_cadence_mode, "fixed")
})

test_that("casual wrapper delegates fixed ESEM cadence and records it", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    "One factor", "Description",
    factors = list(Trait = "One substantive latent trait."),
    llm = "ollama", aco = "standard", verbose = FALSE
  )

  expect_identical(captured$esem$cadence_mode, "fixed")
  expect_identical(captured$esem_every, 10L)
  expect_identical(result$run_config$esem_cadence_mode, "fixed")
})

test_that("one-factor stochastic superiority preserves within-pair provenance", {
  ids <- paste0("i", 1:4)
  m <- matrix(0.5, 4, 4, dimnames = list(ids, ids))
  diag(m) <- 1
  fa <- stats::setNames(rep("Trait", 4), ids)

  out <- semantica_semantic_discrimination(m, fa)
  expect_identical(out$status, "unavailable")
  expect_identical(out$n_within_pairs, 6L)
  expect_identical(out$n_between_pairs, 0L)
  expect_true(is.finite(out$within_mean))
  expect_true(is.na(out$estimate))
})

test_that("unidimensional summary does not report tautological top-factor alignment", {
  obj <- structure(list(
    dimensionality_mode = "unidimensional",
    selected_item_metadata = data.frame(
      ID = paste0("i", 1:4),
      semantica_factor_aligned = rep(TRUE, 4),
      semantica_factor_score = c(.55, .60, .65, .70),
      semantica_factor_margin = rep(NA_real_, 4),
      semantica_factor_clear_mismatch = rep(FALSE, 4),
      semantica_factor_alignment_status = rep(NA_character_, 4),
      semantica_exclusion_conflict = rep(FALSE, 4),
      stringsAsFactors = FALSE
    ),
    optimization = list(dimensionality_mode = "unidimensional"),
    best_items = paste0("i", 1:4),
    reproducibility = list(),
    fit_indices = list()
  ), class = c("semantica_full_pipeline_result", "list"))

  s <- summary(obj)
  expect_true(is.na(s$content_factor_alignment))
  expect_equal(s$content_median_definition_similarity, 0.625)
})
