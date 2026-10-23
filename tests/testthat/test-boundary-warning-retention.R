test_that("boundary and nonfatal solver warnings coexist in a completed native run", {
  ids <- c("a", "b", "c")
  K <- matrix(c(1, .8, .8, .8, 1, .3, .8, .3, 1), 3,
    dimnames = list(ids, ids))
  original <- SEMANTICA:::.semantica_try_lavaan
  local_mocked_bindings(.semantica_try_lavaan = function(fit_args) {
    out <- original(fit_args)
    out$warnings <- c(out$warnings, "offline nonfatal solver warning")
    out$warning_calls <- c(out$warning_calls, "offline warning call")
    out
  }, .package = "SEMANTICA")
  # Select the entire analytically known three-item fixture; no pool search.
  out <- ACO_with_ESEM(K, data.frame(item = ids, factor = "F"), preaco_min_slack = 0L,
    i.per.f = c(F = 3L), ants = 1L, max.iter = 1L, max_total_iter = 1L,
    run_esem_during_search = FALSE, esem_weight = 0,
    esem_bounds = "pos.var", dfi_mode = "heuristic_semantic",
    pfa_mode = "off", run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE,
    use_parallel = FALSE, seed = 625L, verbose = FALSE)
  expect_true(out$esem_admissibility$details$variance_boundary)
  expect_true(any(grepl("zero boundary", out$summary$warnings, fixed = TRUE)))
  expect_true(any(grepl("offline nonfatal solver warning", out$summary$warnings, fixed = TRUE)))
})
