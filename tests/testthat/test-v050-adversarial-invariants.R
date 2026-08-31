test_that("stochastic superiority is invariant to strictly monotone similarity transforms", {
  within <- c(.31, .44, .56, .63, .78, .81)
  between <- c(.10, .18, .22, .29, .39, .52)
  a1 <- .semantica_stochastic_superiority_vectors(within, between)
  f <- function(x) exp(2 * x)
  a2 <- .semantica_stochastic_superiority_vectors(f(within), f(between))
  expect_equal(a1, a2, tolerance = 1e-15)
})

test_that("collapsed multidimensional representations cannot be normalized into semantic quality", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  fac <- setNames(c(rep("A", 3), rep("B", 3)), ids)
  sim <- matrix(.95, 6, 6, dimnames = list(ids, ids))
  diag(sim) <- 1

  out <- compute_semantic_sim_index_v2(
    sim, ids, fac, c("A", "B"),
    within_similarity_target = c(A = .95, B = .95),
    within_similarity_band = .08,
    semantic_objective_mode = "relative_conservative"
  )

  expect_equal(out$stochastic_superiority, .5, tolerance = 1e-15)
  expect_equal(out$robust_median_gap, 0, tolerance = 1e-15)
  expect_equal(out$relative_discrimination_score, 0, tolerance = 1e-15)
  expect_equal(out$sem_score, 0, tolerance = 1e-15)
})

test_that("representation qualification flags concentration without declaring universal invalidity", {
  state <- .semantica_representation_evidence_state(
    representation_stability = list(
      top_eigen_share = .80,
      effective_rank = 2.2,
      effective_rank_ratio = .05,
      common_direction_strength = .91,
      cosine_adjustment_sensitivity = list(
        top_pair_overlap_vs_random = "at_or_below_random_reference"
      )
    ),
    cosine_diagnostics = list(n_items = 45L),
    embedding_diagnostics = list(n_items = 45L, embed_dim = 768L)
  )

  expect_identical(state$status, "representation_sensitive_and_concentrated")
  expect_true(state$representation_sensitive)
  expect_true(state$concentrated_relative_to_isotropic)
  expect_identical(state$calibration_status, "descriptive_reference_not_validity_cutoff")
  expect_match(state$note, "does not automatically alter embeddings", fixed = TRUE)
})

test_that("contrastive alignment guard treats small negative margins as ambiguity, not automatic mismatch", {
  margins <- c(.30, .25, -.05, -.40)
  cls <- .semantica_classify_alignment_margins(margins, rep("A", 4))
  expect_identical(cls$status[3], "ambiguous")
  expect_identical(cls$status[4], "clear_mismatch")
  expect_false(cls$clear_mismatch[3])
  expect_true(cls$clear_mismatch[4])
})

test_that("relative discrimination remains meaningful for legitimately correlated factors", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  fac <- setNames(c(rep("A", 3), rep("B", 3)), ids)
  sim <- matrix(.62, 6, 6, dimnames = list(ids, ids))
  sim[1:3, 1:3] <- .78
  sim[4:6, 4:6] <- .79
  diag(sim) <- 1

  out <- compute_semantic_sim_index_v2(
    sim, ids, fac, c("A", "B"),
    within_similarity_target = c(A = .78, B = .79),
    within_similarity_band = .08,
    semantic_objective_mode = "relative_conservative"
  )

  expect_gt(out$stochastic_superiority, .5)
  expect_gt(out$relative_discrimination_score, 0)
  expect_lt(out$relative_discrimination_score, 1)
})
