test_that("context-aware summary omits absent blueprint and keeps proxy boundary once", {
  x <- list(
    best_items = c("i1", "i2"),
    semantic_score = 0.7,
    fit_indices = c(cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_),
    cosine_diagnostics = list(stochastic_superiority = 0.8, stochastic_superiority_status = "computed"),
    termination_reason = "archive_stable"
  )
  class(x) <- c("semantica_full_pipeline_result", "list")
  sx <- summary(x)
  expect_null(sx$diagnostic_sections$content_blueprint)
  expect_equal(sx$diagnostic_sections$participant_evidence$status, "not_requested")
  txt <- capture.output(print(sx))
  expect_false(any(grepl("Content / blueprint", txt, fixed = TRUE)))
  expect_equal(sum(grepl("sample-free proxy evidence only", txt, fixed = TRUE)), 1L)
})

test_that("hard ACO ceiling is surfaced before selected winner", {
  x <- list(best_items = c("i1", "i2"), semantic_score = .5,
            fit_indices = c(cfi = NA, rmsea = NA, srmr = NA),
            termination_reason = "max_total_iter_reached")
  class(x) <- c("semantica_full_pipeline_result", "list")
  txt <- capture.output(print(summary(x)))
  warn_i <- grep("hard ceiling reached", txt, fixed = TRUE)
  sel_i <- grep("Selected items", txt, fixed = TRUE)
  expect_true(length(warn_i) == 1L && length(sel_i) == 1L && warn_i < sel_i)
})

test_that("inadmissible ESEM reason is reported once without derivative NA cascade", {
  x <- list(
    best_items = "i1", semantic_score = .4,
    fit_indices = c(cfi = NA, rmsea = NA, srmr = NA),
    esem_state = list(technical_state = "converged_inadmissible", reason = "non_positive_definite"),
    termination_reason = "archive_stable"
  )
  class(x) <- c("semantica_full_pipeline_result", "list")
  txt <- capture.output(print(summary(x)))
  expect_equal(sum(grepl("non_positive_definite", txt, fixed = TRUE)), 1L)
  expect_false(any(grepl("ESEM fit proxies", txt, fixed = TRUE)))
})
