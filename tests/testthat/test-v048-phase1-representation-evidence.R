test_that("representation evidence state uses relative/reference diagnostics without changing embeddings", {
  rs <- list(
    common_direction_strength = 0.80,
    effective_rank = 4,
    effective_rank_ratio = 4 / 45,
    top_eigen_share = 0.70,
    cosine_adjustment_sensitivity = list(
      top_pair_overlap_vs_random = "above_random_reference"
    )
  )
  st <- SEMANTICA:::.semantica_representation_evidence_state(
    representation_stability = rs,
    cosine_diagnostics = list(),
    embedding_diagnostics = list(n_items = 45L, embed_dim = 768L)
  )
  expect_identical(st$evidence_family, "embedding_semantic")
  expect_false(st$participant_based)
  expect_identical(st$status, "representation_concentrated")
  expect_true(is.finite(st$isotropic_top_eigen_share_reference))
  expect_true(st$spectral_concentration_ratio > 1)
  expect_match(st$calibration_status, "not_validity_cutoff")
})

test_that("preprocessing sensitivity is propagated as a representation qualifier", {
  st <- SEMANTICA:::.semantica_representation_evidence_state(
    representation_stability = list(
      top_eigen_share = 0.05,
      cosine_adjustment_sensitivity = list(
        top_pair_overlap_vs_random = "at_or_below_random_reference"
      )
    ),
    embedding_diagnostics = list(n_items = 20L, embed_dim = 768L)
  )
  expect_true(st$representation_sensitive)
  expect_match(st$status, "representation_sensitive")
  expect_true(length(st$qualifiers) >= 1L)
})
