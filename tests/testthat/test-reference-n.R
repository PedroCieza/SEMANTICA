test_that("RMSEA reference N does not collapse to the minimum when max_n is underpowered", {
  info <- SEMANTICA:::estimate_esem_reference_sample_size(
    items_per_factor = c(F1 = 3L, F2 = 3L, F3 = 3L),
    n_factors = 3L,
    rmsea_null = 0.05,
    rmsea_alt = 0.06,
    power = 0.80,
    alpha = 0.05,
    max_n = 5000L
  )

  expect_equal(info$p, 9L)
  expect_equal(info$n_obs, 5000L)
  expect_gt(info$n_obs, info$p + 3L)
  expect_false(info$target_power_reached)
  expect_true(info$underpowered_at_max_n)
  expect_lt(info$max_n_power, info$power)
  expect_match(info$method, "underpowered fallback")
})

test_that("RMSEA reference N expands beyond the legacy ceiling by default", {
  info <- SEMANTICA:::estimate_esem_reference_sample_size(
    items_per_factor = c(F1 = 3L, F2 = 3L, F3 = 3L),
    n_factors = 3L,
    rmsea_null = 0.05,
    rmsea_alt = 0.06,
    power = 0.80,
    alpha = 0.05
  )

  expect_equal(info$p, 9L)
  expect_gt(info$n_obs, 5000L)
  expect_true(info$target_power_reached)
  expect_false(info$underpowered_at_max_n)
  expect_gte(info$achieved_power, info$power)
  expect_equal(info$method, "MacCallum-Browne-Sugawara RMSEA power")
})

test_that("RMSEA reference N still uses the power solution when reachable", {
  info <- SEMANTICA:::estimate_esem_reference_sample_size(
    items_per_factor = c(F1 = 6L, F2 = 9L, F3 = 3L),
    n_factors = 3L,
    rmsea_null = 0.05,
    rmsea_alt = 0.06,
    power = 0.80,
    alpha = 0.05,
    max_n = 5000L
  )

  achieved_power <- SEMANTICA:::rmsea_power(
    info$n_obs,
    info$df,
    info$rmsea_null,
    info$rmsea_alt,
    info$alpha
  )

  expect_true(info$target_power_reached)
  expect_false(info$underpowered_at_max_n)
  expect_gt(info$n_obs, info$p + 3L)
  expect_lte(info$n_obs, 5000L)
  expect_gte(achieved_power, info$power)
  expect_equal(info$method, "MacCallum-Browne-Sugawara RMSEA power")
})
