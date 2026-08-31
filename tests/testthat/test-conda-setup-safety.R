test_that("Conda setup verification enforces explicit CUDA semantics", {
  cpu_only <- list(
    available = TRUE,
    cuda_available = FALSE,
    torch_version = "2.7.0"
  )

  expect_invisible(
    SEMANTICA:::.semantica_validate_conda_setup_verification(
      cpu_only,
      accelerator = "cpu"
    )
  )
  expect_error(
    SEMANTICA:::.semantica_validate_conda_setup_verification(
      cpu_only,
      accelerator = "cuda"
    ),
    "torch.cuda.is_available\\(\\) = FALSE"
  )
  expect_invisible(
    SEMANTICA:::.semantica_validate_conda_setup_verification(
      modifyList(cpu_only, list(cuda_available = TRUE)),
      accelerator = "cuda"
    )
  )
  expect_error(
    SEMANTICA:::.semantica_validate_conda_setup_verification(
      list(available = FALSE, error = "mock import failure"),
      accelerator = "cpu"
    ),
    "mock import failure"
  )
})

test_that("Conda recreation guard recognizes names, prefixes, and active Python", {
  existing <- data.frame(
    name = c("base", "active", "inactive"),
    python = c(
      "/opt/conda/bin/python",
      "/opt/conda/envs/active/bin/python",
      "/opt/conda/envs/inactive/bin/python"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(
    SEMANTICA:::.semantica_conda_environment_matches(
      "/opt/conda",
      existing
    ),
    1L
  )
  expect_error(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "base",
      existing = existing,
      conda_bin = "/opt/conda/bin/conda"
    ),
    "base/root"
  )
  expect_error(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "/opt/conda",
      existing = existing,
      conda_bin = "/opt/conda/bin/conda"
    ),
    "base/root"
  )
  expect_error(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "/opt/conda/envs/active",
      existing = existing,
      active_env = "active"
    ),
    "currently active"
  )
  expect_error(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "active",
      active_env = "/opt/conda/envs/ACTIVE"
    ),
    "currently active"
  )
  expect_error(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "/opt/conda/envs/active",
      active_python = "/opt/conda/envs/active/bin/python"
    ),
    "currently active"
  )
  expect_invisible(
    SEMANTICA:::.semantica_assert_safe_conda_recreate(
      "/opt/conda/envs/inactive",
      existing = existing,
      active_env = "active",
      active_prefix = "/opt/conda/envs/active",
      conda_bin = "/opt/conda/bin/conda"
    )
  )
})
