test_that("normalized lexical identity is distinct from semantic duplicate screening", {
  x <- c("I feel capable today.", "  I FEEL capable today!  ", "I can master difficult tasks.")
  z <- SEMANTICA:::.dedup_items(x)
  expect_length(z, 2L)
  expect_identical(z[[1]], x[[1]])
})

test_that("overgenerated candidates are curated deterministically for lexical diversity", {
  x <- c(
    "I can handle difficult tasks.",
    "I am able to handle difficult tasks.",
    "I develop my skills through practice.",
    "I can solve challenging problems.",
    "I feel effective when I use my abilities."
  )
  a <- SEMANTICA:::.semantica_select_diverse_generated_items(x, 3L)
  b <- SEMANTICA:::.semantica_select_diverse_generated_items(x, 3L)
  expect_identical(unname(a), unname(b))
  expect_length(a, 3L)
  expect_identical(attr(a, "generation_diversity")$policy, "deterministic_lexical_maxmin_v1")
})

test_that("casual generation default matches the established full-pipeline overgeneration default", {
  expect_equal(eval(formals(semantica_run)$overgenerate), 2)
  expect_equal(semantica_generation_config()$overgenerate, 2)
})
