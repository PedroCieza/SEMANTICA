test_that("ESEM alignment is invariant to factor permutations and sign flips", {
  items <- paste0("item_", seq_len(9L))
  factors <- c("Clarity", "Flexibility", "Control")
  factor_assignment <- stats::setNames(rep(factors, each = 3L), items)

  lambda <- rbind(
    c(0.82, 0.12, 0.06), c(0.76, 0.18, 0.04), c(0.71, 0.09, 0.10),
    c(0.08, 0.79, 0.15), c(0.13, 0.74, 0.11), c(0.05, 0.69, 0.18),
    c(0.11, 0.07, 0.81), c(0.16, 0.12, 0.73), c(0.04, 0.14, 0.68)
  )
  dimnames(lambda) <- list(items, paste0("axis_", seq_len(3L)))
  psi <- matrix(c(
    1.00, 0.28, 0.19,
    0.28, 1.00, 0.24,
    0.19, 0.24, 1.00
  ), nrow = 3L, byrow = TRUE, dimnames = list(colnames(lambda), colnames(lambda)))

  baseline <- SEMANTICA:::align_esem_to_intended_structure(
    lambda, factor_assignment, factors, psi
  )

  permutation <- c(3L, 1L, 2L)
  flips <- c(-1, 1, -1)
  permuted_lambda <- sweep(lambda[, permutation, drop = FALSE], 2L, flips, `*`)
  permuted_psi <- psi[permutation, permutation, drop = FALSE] * tcrossprod(flips)
  transformed <- SEMANTICA:::align_esem_to_intended_structure(
    permuted_lambda, factor_assignment, factors, permuted_psi
  )

  expect_equal(transformed$lambda, baseline$lambda, tolerance = 1e-12)
  expect_equal(transformed$psi, baseline$psi, tolerance = 1e-12)
  expect_equal(transformed$total_match_score, baseline$total_match_score, tolerance = 1e-12)
  expect_identical(transformed$mapping$intended_factor, factors)
  expect_true(transformed$globally_optimal_assignment)
  expect_identical(transformed$assignment_method, "exact_dynamic_programming")
  expect_true(transformed$diagnostics$globally_optimal_assignment)
})

test_that("factor-axis assignment is globally optimal and deterministic", {
  scores <- matrix(c(
    10, 9, 0,
     9, 0, 0,
     0, 8, 7
  ), nrow = 3L, byrow = TRUE)
  solved <- SEMANTICA:::.semantica_solve_factor_assignment(scores)

  expect_identical(solved$assignment, c(2L, 1L, 3L))
  expect_equal(solved$score, 25)
  expect_true(solved$globally_optimal)

  tied <- SEMANTICA:::.semantica_solve_factor_assignment(matrix(1, 3L, 3L))
  expect_identical(tied$assignment, 1:3)
})

test_that("large-factor fallback is explicit, deterministic, and one-to-one", {
  scores <- matrix(seq_len(25L), 5L, 5L)
  first <- SEMANTICA:::.semantica_solve_factor_assignment(
    scores, max_exact_factors = 3L, large_strategy = "greedy_2opt"
  )
  second <- SEMANTICA:::.semantica_solve_factor_assignment(
    scores, max_exact_factors = 3L, large_strategy = "greedy_2opt"
  )

  expect_identical(first$assignment, second$assignment)
  expect_length(unique(first$assignment), 5L)
  expect_false(first$globally_optimal)
  expect_match(first$method, "deterministic")
  expect_match(first$note, "exceeded the exact-assignment limit")

  expect_error(
    SEMANTICA:::.semantica_solve_factor_assignment(
      scores, max_exact_factors = 3L, large_strategy = "error"
    ),
    "limited to 3 factors"
  )
})

test_that("zero-mean intended loadings receive a stable sign anchor", {
  lambda <- matrix(c(
    0.70, 0.10,
   -0.70, 0.05,
    0.05, 0.80,
    0.10, 0.75
  ), nrow = 4L, byrow = TRUE)
  dimnames(lambda) <- list(paste0("i", 1:4), c("a", "b"))
  assignment <- stats::setNames(c("F1", "F1", "F2", "F2"), rownames(lambda))

  aligned <- SEMANTICA:::align_esem_to_intended_structure(
    lambda, assignment, c("F1", "F2")
  )
  flipped <- lambda
  flipped[, 1L] <- -flipped[, 1L]
  aligned_flipped <- SEMANTICA:::align_esem_to_intended_structure(
    flipped, assignment, c("F1", "F2")
  )

  expect_equal(aligned_flipped$lambda, aligned$lambda, tolerance = 1e-12)
  expect_true("largest_intended_loading" %in% aligned$mapping$anchor_method)
})

test_that("a real lavaan solution can be extracted through the aligned helper", {
  skip_if_not_installed("lavaan")
  data("HolzingerSwineford1939", package = "lavaan")
  model <- paste(
    "visual =~ x1 + x2 + x3",
    "textual =~ x4 + x5 + x6",
    "speed =~ x7 + x8 + x9",
    sep = "\n"
  )
  fit <- lavaan::cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)
  intended <- stats::setNames(
    rep(c("visual", "textual", "speed"), each = 3L),
    paste0("x", seq_len(9L))
  )

  out <- SEMANTICA:::extract_aligned_esem_solution(
    fit, intended, c("visual", "textual", "speed")
  )

  expect_identical(colnames(out$lambda), c("visual", "textual", "speed"))
  expect_identical(dim(out$psi), c(3L, 3L))
  expect_s3_class(out$admissibility, "semantica_esem_admissibility")
})
