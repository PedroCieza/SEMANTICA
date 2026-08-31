test_that("optional Torch capability detection is session-cached and refreshable", {
  calls <- 0L
  local_mocked_bindings(
    .semantica_torch_capabilities_uncached = function() {
      calls <<- calls + 1L
      list(package_installed=TRUE, runtime_available=FALSE, version="mock",
           cuda_available=FALSE, cuda_device_count=0L, cuda_runtime=NA_character_,
           mps_available=FALSE, detection_error="mock runtime unavailable")
    },
    .package = "SEMANTICA"
  )
  rm(list = ls(SEMANTICA:::.semantica_compute_capability_cache),
     envir = SEMANTICA:::.semantica_compute_capability_cache)
  a <- SEMANTICA:::.semantica_torch_capabilities()
  b <- SEMANTICA:::.semantica_torch_capabilities()
  expect_equal(calls, 1L)
  expect_false(a$runtime_available)
  expect_identical(a, b)
  SEMANTICA:::.semantica_torch_capabilities(refresh = TRUE)
  expect_equal(calls, 2L)
})

test_that("capabilities remain CPU-first when optional Torch runtime is unavailable", {
  local_mocked_bindings(
    .semantica_torch_capabilities = function(refresh=FALSE) list(
      package_installed=TRUE, runtime_available=FALSE, version="mock",
      cuda_available=FALSE, cuda_device_count=0L, cuda_runtime=NA_character_,
      mps_available=FALSE, detection_error="Lantern unavailable"
    ),
    .package = "SEMANTICA"
  )
  caps <- semantica_capabilities()
  expect_true(caps$cpu$available)
  expect_equal(caps$policy$default_compute_device, "cpu")
  expect_false(caps$r_torch$runtime_available)
})

test_that("GPU numerical equivalence remains optional when infrastructure is absent", {
  skip_if_not(SEMANTICA:::.semantica_torch_available(), "R Torch/Lantern runtime unavailable")
  caps <- semantica_capabilities(refresh = TRUE)
  skip_if_not(isTRUE(caps$r_torch$cuda_available), "CUDA hardware unavailable")
  # Low-level CPU/GPU equivalence is covered by test-gpu-cosine.R when the
  # optional runtime is actually available; this test intentionally records a skip otherwise.
  expect_true(caps$r_torch$cuda_available)
})
