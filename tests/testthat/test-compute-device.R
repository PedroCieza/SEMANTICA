test_that("compute devices and precision are normalized strictly", {
  expect_equal(SEMANTICA:::.semantica_normalize_device(" CUDA:1 "), "cuda:1")
  expect_equal(SEMANTICA:::.semantica_normalize_precision("float64"), "double")
  expect_equal(SEMANTICA:::.semantica_normalize_precision("float32"), "single")
  expect_error(
    SEMANTICA:::.semantica_normalize_device("gpu"),
    "must be one of"
  )
  expect_error(
    SEMANTICA:::.semantica_normalize_precision("half"),
    "must be"
  )
})

test_that("CPU and auto resolution preserve the compatibility policy", {
  gpu_caps <- list(
    r_torch = list(
      package_installed = TRUE,
      runtime_available = TRUE,
      cuda_available = TRUE,
      cuda_device_count = 2L,
      mps_available = TRUE
    )
  )

  cpu <- SEMANTICA:::.semantica_resolve_device(
    "cpu",
    capabilities = gpu_caps
  )
  auto <- SEMANTICA:::.semantica_resolve_device(
    "auto",
    capabilities = gpu_caps
  )

  expect_equal(cpu$resolved_device, "cpu")
  expect_equal(cpu$backend, "base_r")
  expect_false(cpu$used_fallback)
  expect_equal(auto$resolved_device, "cpu")
  expect_false(auto$used_fallback)
  expect_match(auto$fallback_reason, "crossover benchmarks")
})

test_that("explicit accelerators require availability or an explicit fallback", {
  no_gpu <- list(
    r_torch = list(
      package_installed = FALSE,
      runtime_available = FALSE,
      cuda_available = FALSE,
      cuda_device_count = 0L,
      mps_available = FALSE
    )
  )

  expect_error(
    SEMANTICA:::.semantica_resolve_device(
      "cuda",
      gpu_fallback = "error",
      capabilities = no_gpu
    ),
    "will not silently fall back"
  )
  fallback <- SEMANTICA:::.semantica_resolve_device(
    "cuda",
    gpu_fallback = "cpu",
    capabilities = no_gpu
  )
  expect_equal(fallback$resolved_device, "cpu")
  expect_true(fallback$used_fallback)
  expect_match(fallback$fallback_reason, "torch")
})

test_that("MPS single precision is explicit and double precision is rejected", {
  mps_caps <- list(
    r_torch = list(
      package_installed = TRUE,
      runtime_available = TRUE,
      cuda_available = FALSE,
      cuda_device_count = 0L,
      mps_available = TRUE
    )
  )

  expect_error(
    SEMANTICA:::.semantica_resolve_device(
      "mps",
      gpu_precision = "double",
      capabilities = mps_caps
    ),
    "does not support the float64"
  )
  single <- SEMANTICA:::.semantica_resolve_device(
    "mps",
    gpu_precision = "single",
    capabilities = mps_caps
  )
  expect_equal(single$resolved_device, "mps")
  expect_equal(single$resolved_precision, "single")
})

test_that("capability inspection does not inspect Python by default", {
  caps <- SEMANTICA:::semantica_capabilities()
  expect_s3_class(caps, "semantica_capabilities")
  expect_true(caps$cpu$available)
  expect_gte(caps$cpu$available_cores, 1L)
  expect_false(caps$python$checked)
  expect_equal(caps$policy$default_compute_device, "cpu")
  expect_equal(caps$policy$auto_compute_device, "cpu")
})

test_that("cosine memory estimates include device work and host output", {
  estimate <- SEMANTICA:::.semantica_estimate_cosine_memory(
    n_items = 100L,
    embedding_dimensions = 384L,
    precision = "double",
    adjustment = "mean_center"
  )
  expect_gt(estimate$device_bytes, 8 * 100 * 100)
  expect_equal(estimate$host_output_bytes, 8 * 100 * 100)
  expect_gt(estimate$conservative_total_bytes, estimate$device_bytes)
})
