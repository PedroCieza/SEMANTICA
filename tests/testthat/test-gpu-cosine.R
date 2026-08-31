test_that("base-R cosine preserves numerical and naming invariants", {
  x <- matrix(
    c(
      1, 2, 3,
      2, 1, 0,
      0, 1, 2,
      3, 0, 1
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(paste0("item_", 1:4), NULL)
  )

  out <- SEMANTICA:::.semantica_cosine_cpu(
    x,
    already_normalized = FALSE,
    adjustment = "none"
  )
  normalized <- x / sqrt(rowSums(x^2))
  expected <- tcrossprod(normalized)
  diag(expected) <- 1
  dimnames(expected) <- list(rownames(x), rownames(x))

  expect_equal(out, expected, tolerance = 1e-14)
  expect_equal(rownames(out), rownames(x))
  expect_equal(colnames(out), rownames(x))
  expect_equal(out, t(out), tolerance = 0)
  expect_identical(unname(diag(out)), rep(1, nrow(x)))
  expect_true(all(out >= -1 & out <= 1))
})

test_that("mean-centered cosine retains matrix invariants", {
  x <- matrix(
    c(1, 2, 4, 2, 5, 1, 4, 1, 3, 6, 2, 1),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(LETTERS[1:4], NULL)
  )
  out <- SEMANTICA:::.semantica_cosine_cpu(
    x,
    already_normalized = FALSE,
    adjustment = "mean_center"
  )

  expect_equal(dim(out), c(4L, 4L))
  expect_equal(dimnames(out), list(LETTERS[1:4], LETTERS[1:4]))
  expect_true(all(is.finite(out)))
  expect_equal(out, t(out), tolerance = 0)
  expect_identical(unname(diag(out)), rep(1, 4L))
  expect_true(all(out >= -1 & out <= 1))
})

test_that("the cosine dispatcher returns conventional matrix telemetry", {
  x <- matrix(
    c(1, 0, 0, 1, 1, 1),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(c("a", "b", "c"), NULL)
  )
  out <- SEMANTICA:::.semantica_compute_cosine(
    x,
    already_normalized = FALSE,
    compute_device = "auto"
  )

  expect_true(is.matrix(out$matrix))
  expect_type(out$matrix, "double")
  expect_equal(out$telemetry$requested_device, "auto")
  expect_equal(out$telemetry$resolved_device, "cpu")
  expect_equal(out$telemetry$backend, "base_r")
  expect_false(out$telemetry$used_fallback)
})

test_that("R torch CPU double cosine agrees closely when torch is usable", {
  skip_if_not(SEMANTICA:::.semantica_torch_available())
  x <- matrix(
    c(1, 2, 3, 2, 1, 0, 0, 1, 2, 3, 0, 1),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(paste0("i", 1:4), NULL)
  )
  expected <- SEMANTICA:::.semantica_cosine_cpu(
    x,
    already_normalized = FALSE,
    adjustment = "mean_center"
  )
  actual <- SEMANTICA:::.semantica_cosine_torch(
    x,
    already_normalized = FALSE,
    adjustment = "mean_center",
    device = "cpu",
    precision = "double"
  )

  expect_equal(actual, expected, tolerance = 1e-12)
  expect_equal(dimnames(actual), dimnames(expected))
  expect_identical(unname(diag(actual)), rep(1, nrow(x)))
})

test_that("CUDA cosine parity is conditional on a compatible GPU", {
  caps <- SEMANTICA:::semantica_capabilities()
  skip_if_not(isTRUE(caps$r_torch$cuda_available))
  x <- matrix(
    seq_len(60L),
    nrow = 10L,
    dimnames = list(paste0("cuda_", 1:10), NULL)
  )
  cpu <- SEMANTICA:::.semantica_cosine_cpu(x, already_normalized = FALSE)
  gpu <- SEMANTICA:::.semantica_cosine_torch(
    x,
    already_normalized = FALSE,
    device = "cuda",
    precision = "double"
  )
  expect_equal(gpu, cpu, tolerance = 1e-10)
})

test_that("MPS cosine requires explicit single precision", {
  caps <- SEMANTICA:::semantica_capabilities()
  skip_if_not(isTRUE(caps$r_torch$mps_available))
  x <- matrix(
    seq_len(60L),
    nrow = 10L,
    dimnames = list(paste0("mps_", 1:10), NULL)
  )
  cpu <- SEMANTICA:::.semantica_cosine_cpu(x, already_normalized = FALSE)
  gpu <- SEMANTICA:::.semantica_cosine_torch(
    x,
    already_normalized = FALSE,
    device = "mps",
    precision = "single"
  )
  expect_equal(gpu, cpu, tolerance = 1e-5)
})
