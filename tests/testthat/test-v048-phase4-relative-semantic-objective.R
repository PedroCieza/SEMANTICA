test_that("multidimensional relative semantic objective is shift-invariant when its declared cohesion reference shifts with the representation", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  fac <- setNames(c(rep("A", 3), rep("B", 3)), ids)
  sim <- matrix(0.25, 6, 6, dimnames = list(ids, ids))
  diag(sim) <- 1
  sim[1:3, 1:3] <- 0.72
  sim[4:6, 4:6] <- 0.74
  diag(sim) <- 1
  factors <- c("A", "B")

  x <- compute_semantic_sim_index_v2(
    sim, ids, fac, factors,
    within_similarity_target = c(A = 0.72, B = 0.74),
    within_similarity_band = 0.08,
    semantic_objective_mode = "relative_conservative"
  )

  shifted <- sim
  shifted[row(shifted) != col(shifted)] <- shifted[row(shifted) != col(shifted)] + 0.15
  y <- compute_semantic_sim_index_v2(
    shifted, ids, fac, factors,
    within_similarity_target = c(A = 0.87, B = 0.89),
    within_similarity_band = 0.08,
    semantic_objective_mode = "relative_conservative"
  )

  expect_equal(x$stochastic_superiority, y$stochastic_superiority, tolerance = 1e-12)
  expect_equal(x$standardized_robust_gap, y$standardized_robust_gap, tolerance = 1e-10)
  expect_equal(x$sem_score, y$sem_score, tolerance = 1e-10)
})

test_that("perfect ordering with a tiny relative gap cannot masquerade as perfect multidimensional separation", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  fac <- setNames(c(rep("A", 3), rep("B", 3)), ids)
  sim <- matrix(0.800, 6, 6, dimnames = list(ids, ids))
  sim[1:3, 1:3] <- 0.801
  sim[4:6, 4:6] <- 0.801
  diag(sim) <- 1

  out <- compute_semantic_sim_index_v2(
    sim, ids, fac, c("A", "B"),
    within_similarity_target = c(A = 0.801, B = 0.801),
    within_similarity_band = 0.08,
    semantic_objective_mode = "relative_conservative"
  )

  expect_equal(out$stochastic_superiority, 1)
  expect_true(is.finite(out$gap_discrimination_component))
  expect_lt(out$sem_score, 0.75)
  expect_gt(out$sem_score, 0)
})

test_that("legacy semantic objective remains exactly available by explicit mode", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  fac <- setNames(c(rep("A", 3), rep("B", 3)), ids)
  sim <- matrix(0.30, 6, 6, dimnames = list(ids, ids))
  sim[1:3, 1:3] <- 0.70
  sim[4:6, 4:6] <- 0.73
  diag(sim) <- 1
  args <- list(
    sim_matrix = sim,
    selected_items = ids,
    factor_assignment = fac,
    factors = c("A", "B"),
    within_similarity_target = c(A = 0.70, B = 0.73),
    within_similarity_band = 0.08
  )
  old <- do.call(.compute_semantic_sim_index_legacy, args)
  compat <- do.call(compute_semantic_sim_index_v2, c(args, list(semantic_objective_mode = "legacy_target_burden")))

  expect_equal(compat$sem_score, old$sem_score, tolerance = 1e-15)
  expect_equal(compat$similarity_index, old$similarity_index, tolerance = 1e-15)
})

test_that("unidimensional scales keep the target-centered semantic objective", {
  ids <- c("a1", "a2", "a3")
  fac <- setNames(rep("A", 3), ids)
  sim <- matrix(0.65, 3, 3, dimnames = list(ids, ids)); diag(sim) <- 1
  out <- compute_semantic_sim_index_v2(
    sim, ids, fac, "A",
    within_similarity_target = c(A = 0.65),
    semantic_objective_mode = "relative_conservative"
  )
  expect_identical(out$semantic_objective_mode, "target_centered_unidimensional")
  expect_true(is.na(out$relative_discrimination_score))
})

test_that("relative candidate heuristics prefer construct discrimination over raw within-factor height", {
  ids <- c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4")
  sim <- matrix(0.20, 8, 8, dimnames = list(ids, ids)); diag(sim) <- 1
  # a1 is highly cohesive but also nearly as similar to the competing factor.
  sim["a1", c("a2", "a3", "a4")] <- sim[c("a2", "a3", "a4"), "a1"] <- 0.90
  sim["a1", c("b1", "b2", "b3", "b4")] <- sim[c("b1", "b2", "b3", "b4"), "a1"] <- 0.88
  # a2/a3/a4 are somewhat less cohesive but substantially better separated.
  sim[c("a2", "a3", "a4"), c("a2", "a3", "a4")] <- 0.75
  diag(sim) <- 1
  sim["a2", c("b1", "b2", "b3", "b4")] <- sim[c("b1", "b2", "b3", "b4"), "a2"] <- 0.25
  sim["a3", c("b1", "b2", "b3", "b4")] <- sim[c("b1", "b2", "b3", "b4"), "a3"] <- 0.25
  sim["a4", c("b1", "b2", "b3", "b4")] <- sim[c("b1", "b2", "b3", "b4"), "a4"] <- 0.25
  sim[c("b1", "b2", "b3", "b4"), c("b1", "b2", "b3", "b4")] <- 0.75
  diag(sim) <- 1

  items <- list(A = ids[1:4], B = ids[5:8])
  h <- compute_item_heuristics(
    items, sim, c("A", "B"),
    within_similarity_target = c(A = 0.78, B = 0.75),
    semantic_objective_mode = "relative_conservative"
  )
  expect_lt(unname(h$A["a1"]), unname(h$A["a2"]))
  expect_identical(attr(h, "selection_basis"), "ranked_relative_within_between_discrimination_with_target_guard")
})
