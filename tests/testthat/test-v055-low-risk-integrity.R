test_that("persistent embedding cache distinguishes sanitized endpoints", {
  base <- list(
    backend = "generic_openai",
    protocol = "openai_compat",
    embed_model = "shared-model-label",
    embedding_task = "auto",
    embedding_instruction = NULL
  )

  s1 <- c(base, list(embed_url = "https://provider-a.example/v1/embeddings?token=secret-a"))
  s1_equiv <- c(base, list(embed_url = "https://provider-a.example/v1/embeddings?token=secret-b"))
  s2 <- c(base, list(embed_url = "https://provider-b.example/v1/embeddings?token=secret-a"))

  k1 <- SEMANTICA:::.semantica_text_cache_key("same text", s1)
  k1_equiv <- SEMANTICA:::.semantica_text_cache_key("same text", s1_equiv)
  k2 <- SEMANTICA:::.semantica_text_cache_key("same text", s2)

  # Query credentials are intentionally removed before hashing, while endpoint
  # identity still separates otherwise compatible providers/servers.
  expect_identical(k1, k1_equiv)
  expect_false(identical(k1, k2))
  expect_match(k1, "^[0-9a-f]{32}$")
  expect_false(grepl("provider|secret", k1, ignore.case = TRUE))
})

test_that("recommended validation-N helper restores an absent caller RNG state", {
  caller_had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  caller_seed <- if (caller_had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (caller_had_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  items <- c("i1", "i2")
  factors <- c("A", "B")
  pop_cor <- diag(2L)
  dimnames(pop_cor) <- list(items, items)
  pop_phi <- diag(2L)
  dimnames(pop_phi) <- list(factors, factors)
  pop <- list(
    cor = pop_cor,
    lambda = matrix(
      c(0.70, 0.10,
        0.10, 0.70),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(items, factors)
    ),
    phi = pop_phi
  )

  local_mocked_bindings(
    build_pfa_population_correlation = function(...) pop,
    run_esem_on_matrix = function(...) NULL,
    .package = "SEMANTICA"
  )

  invisible(SEMANTICA:::estimate_recommended_validation_n(
    pfa_diagnostics = list(),
    factor_assignment = c(i1 = "A", i2 = "B"),
    factors = factors,
    syntax = "mock syntax",
    n_grid = 20L,
    reps = 5L,
    max_n = 20L,
    seed = 20260828L,
    verbose = FALSE,
    progress = FALSE
  ))

  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("recommended validation-N helper restores an existing caller RNG state", {
  caller_had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  caller_seed <- if (caller_had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (caller_had_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(31337)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  items <- c("i1", "i2")
  factors <- c("A", "B")
  pop_cor <- diag(2L)
  dimnames(pop_cor) <- list(items, items)
  pop_phi <- diag(2L)
  dimnames(pop_phi) <- list(factors, factors)
  pop <- list(
    cor = pop_cor,
    lambda = matrix(
      c(0.70, 0.10,
        0.10, 0.70),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(items, factors)
    ),
    phi = pop_phi
  )

  local_mocked_bindings(
    build_pfa_population_correlation = function(...) pop,
    run_esem_on_matrix = function(...) NULL,
    .package = "SEMANTICA"
  )

  invisible(SEMANTICA:::estimate_recommended_validation_n(
    pfa_diagnostics = list(),
    factor_assignment = c(i1 = "A", i2 = "B"),
    factors = factors,
    syntax = "mock syntax",
    n_grid = 20L,
    reps = 5L,
    max_n = 20L,
    seed = 20260828L,
    verbose = FALSE,
    progress = FALSE
  ))

  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(after, before)
})
