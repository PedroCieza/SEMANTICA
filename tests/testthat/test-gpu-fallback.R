.mock_cuda_capabilities <- function() {
  list(
    r_torch = list(
      package_installed = TRUE,
      runtime_available = TRUE,
      cuda_available = TRUE,
      cuda_device_count = 1L,
      mps_available = FALSE
    )
  )
}

test_that("an unavailable explicit CUDA request fails by default", {
  unavailable <- list(
    r_torch = list(
      package_installed = TRUE,
      runtime_available = TRUE,
      cuda_available = FALSE,
      cuda_device_count = 0L,
      mps_available = FALSE
    )
  )
  x <- diag(3L)

  expect_error(
    SEMANTICA:::.semantica_compute_cosine(
      x,
      compute_device = "cuda",
      capabilities = unavailable
    ),
    "CUDA was requested"
  )
})

test_that("an explicit CPU fallback is recorded when CUDA is unavailable", {
  unavailable <- list(
    r_torch = list(
      package_installed = FALSE,
      runtime_available = FALSE,
      cuda_available = FALSE,
      cuda_device_count = 0L,
      mps_available = FALSE
    )
  )
  x <- diag(3L)
  dimnames(x) <- list(letters[1:3], letters[1:3])

  out <- SEMANTICA:::.semantica_compute_cosine(
    x,
    compute_device = "cuda",
    gpu_fallback = "cpu",
    capabilities = unavailable
  )
  expect_equal(out$telemetry$resolved_device, "cpu")
  expect_true(out$telemetry$used_fallback)
  expect_match(out$telemetry$fallback_reason, "torch")
  expect_equal(dimnames(out$matrix), list(letters[1:3], letters[1:3]))
})

test_that("torch execution errors follow the requested fallback policy", {
  local_mocked_bindings(
    .semantica_cosine_torch = function(...) {
      stop("CUDA out of memory (mock)")
    },
    .semantica_release_torch_memory = function(...) invisible(NULL),
    .package = "SEMANTICA"
  )
  x <- matrix(c(1, 0, 0, 1, 1, 1), nrow = 3L, byrow = TRUE)

  expect_error(
    SEMANTICA:::.semantica_compute_cosine(
      x,
      already_normalized = FALSE,
      compute_device = "cuda",
      gpu_fallback = "error",
      capabilities = .mock_cuda_capabilities()
    ),
    "will not silently fall back"
  )

  fallback <- SEMANTICA:::.semantica_compute_cosine(
    x,
    already_normalized = FALSE,
    compute_device = "cuda",
    gpu_fallback = "cpu",
    capabilities = .mock_cuda_capabilities()
  )
  expect_equal(fallback$telemetry$resolved_device, "cpu")
  expect_true(fallback$telemetry$used_fallback)
  expect_match(fallback$telemetry$torch_error, "out of memory")
  expect_true(is.matrix(fallback$matrix))
})

test_that("the optional memory limit guards accelerator and CPU fallback allocation", {
  x <- matrix(seq_len(120L), nrow = 20L)
  estimate <- SEMANTICA:::.semantica_estimate_cosine_memory(
    n_items = nrow(x),
    embedding_dimensions = ncol(x)
  )

  expect_error(
    SEMANTICA:::.semantica_compute_cosine(
      x,
      already_normalized = FALSE,
      compute_device = "cuda",
      gpu_fallback = "error",
      memory_limit = estimate$device_bytes - 1,
      capabilities = .mock_cuda_capabilities()
    ),
    "exceeds memory_limit"
  )

  expect_error(
    SEMANTICA:::.semantica_compute_cosine(
      x,
      already_normalized = FALSE,
      compute_device = "cuda",
      gpu_fallback = "cpu",
      memory_limit = estimate$device_bytes - 1,
      capabilities = .mock_cuda_capabilities()
    ),
    "CPU fallback.*exceeds memory_limit"
  )
})

test_that("a torch error cannot bypass the CPU memory ceiling", {
  local_mocked_bindings(
    .semantica_cosine_torch = function(...) {
      stop("CUDA out of memory (mock)")
    },
    .semantica_release_torch_memory = function(...) invisible(NULL),
    .package = "SEMANTICA"
  )
  x <- matrix(seq_len(120L), nrow = 20L)
  estimate <- SEMANTICA:::.semantica_estimate_cosine_memory(
    n_items = nrow(x),
    embedding_dimensions = ncol(x)
  )
  between_limits <- mean(c(
    estimate$device_bytes,
    estimate$conservative_total_bytes
  ))

  expect_error(
    SEMANTICA:::.semantica_compute_cosine(
      x,
      already_normalized = FALSE,
      compute_device = "cuda",
      gpu_fallback = "cpu",
      memory_limit = between_limits,
      capabilities = .mock_cuda_capabilities()
    ),
    "CPU fallback.*exceeds memory_limit"
  )
})
