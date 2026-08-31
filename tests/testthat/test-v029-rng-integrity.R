.rng_snapshot <- function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    list(exists = TRUE, value = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  } else {
    list(exists = FALSE, value = NULL)
  }
}

.restore_rng_snapshot <- function(x) {
  if (isTRUE(x$exists)) {
    assign(".Random.seed", x$value, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

test_that("embedding cache writes are RNG-neutral and round-trip", {
  caller <- .rng_snapshot()
  on.exit(.restore_rng_snapshot(caller), add = TRUE)

  cache_dir <- tempfile("semantica-cache-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  set.seed(2029)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  key <- paste(rep("a", 32L), collapse = "")
  value <- list(embedding = c(0.1, 0.2, 0.3), model = "mock")
  path <- SEMANTICA:::.semantica_embedding_cache_set(key, value, cache_dir)
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  expect_identical(after, before)
  expect_true(file.exists(path))
  expect_identical(
    SEMANTICA:::.semantica_embedding_cache_get(key, cache_dir),
    value
  )
  expect_false(any(grepl("\\.tmp-", list.files(cache_dir, recursive = TRUE))))

  rm(".Random.seed", envir = .GlobalEnv)
  key2 <- paste(rep("b", 32L), collapse = "")
  SEMANTICA:::.semantica_embedding_cache_set(key2, list(x = 1), cache_dir)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("ESEM path plotting is RNG-neutral and deterministic", {
  caller <- .rng_snapshot()
  on.exit(.restore_rng_snapshot(caller), add = TRUE)

  local_mocked_bindings(
    .viz_get_function = function(name) NULL,
    .package = "SEMANTICA"
  )

  ids <- c("A1", "A2", "B1", "B2")
  fa <- c(A1 = "A", A2 = "A", B1 = "B", B2 = "B")
  cosine <- matrix(
    c(
      1.00, 0.65, 0.15, 0.10,
      0.65, 1.00, 0.12, 0.18,
      0.15, 0.12, 1.00, 0.62,
      0.10, 0.18, 0.62, 1.00
    ),
    nrow = 4L, byrow = TRUE,
    dimnames = list(ids, ids)
  )
  result <- list(
    factor_assignment = fa,
    best_items = ids,
    eligible_items = list(A = c("A1", "A2"), B = c("B1", "B2")),
    esem_fit = NULL,
    model_info = list(rotation = "geomin")
  )

  set.seed(42)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  p1 <- plot_esem_path_diagrams(result, cosine, before_model = "proxy")
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(after, before)
  expect_s3_class(p1$p10a, "ggplot")
  expect_s3_class(p1$p10b, "ggplot")

  p2 <- plot_esem_path_diagrams(result, cosine, before_model = "proxy")
  expect_identical(ggplot2::ggplot_build(p1$p10a)$data,
                   ggplot2::ggplot_build(p2$p10a)$data)

  rm(".Random.seed", envir = .GlobalEnv)
  invisible(plot_esem_path_diagrams(result, cosine, before_model = "proxy"))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
