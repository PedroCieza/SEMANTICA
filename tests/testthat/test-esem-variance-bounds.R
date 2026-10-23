# A positive-definite, exactly soluble example: one-factor ML needs
# psi_a = 1 - .8*.8/.3 < 0. No provider, external files or tuned item rules.
.bounds_fixture <- function() {
  ids <- c("a", "b", "c")
  K <- matrix(c(1, .8, .8, .8, 1, .3, .8, .3, 1), 3, dimnames = list(ids, ids))
  fa <- setNames(rep("F", 3), ids)
  list(K = K, fa = fa, syntax = SEMANTICA:::build_esem_syntax_safe(ids, fa, "F"))
}

test_that("bounds are explicit, opt-in, and reject unsupported calibration", {
  expect_identical(semantica_esem_config()$bounds, "none")
  expect_identical(semantica_esem_config(bounds = "pos.var")$bounds, "pos.var")
  expect_error(semantica_esem_config(bounds = "wide"), "arg")
  expect_error(SEMANTICA:::.semantica_validate_esem_bounds("pos.var", "strict_cfa_dfi"), "native ESEM")
  expect_error(SEMANTICA:::.semantica_validate_esem_bounds("pos.var", "auto", TRUE), "final_dddfi")
})

test_that("nonnegative estimation preserves the matrix and exposes its boundary", {
  skip_if_not_installed("lavaan")
  x <- .bounds_fixture(); original <- x$K
  plain <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L,
    rotation = "none", fallback = FALSE, return_diagnostics = TRUE)
  constrained <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L,
    rotation = "none", bounds = "pos.var", fallback = FALSE, return_diagnostics = TRUE)
  expect_identical(x$K, original)
  expect_identical(plain$status, "converged_inadmissible")
  expect_lt(plain$rejection_assessment$details$residual_variances[["a"]], -1)
  expect_true(SEMANTICA:::is_admissible_esem_fit(constrained$fit))
  expect_equal(lavaan::lavInspect(constrained$fit, "sampstat")$cov, x$K,
    ignore_attr = TRUE, tolerance = 1e-12)
  details <- SEMANTICA:::is_admissible_esem_fit(constrained$fit, return_assessment = TRUE)$details
  expect_true(details$variance_boundary)
  expect_identical(details$boundary_items, "a")
  pt <- lavaan::parTable(constrained$fit)
  residuals <- pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% names(x$fa)
  expect_true(all(pt$lower[residuals] == 0))
  expect_true(all(pt$free[residuals] > 0)) # estimated jointly, not fixed afterwards
})

test_that("every numerical rescue retains bounds and a compatible optimizer", {
  x <- .bounds_fixture(); calls <- list()
  local_mocked_bindings(.semantica_try_lavaan = function(fit_args) {
    calls[[length(calls) + 1L]] <<- fit_args
    list(fit = NULL, error = "forced failure", warnings = character(), warning_calls = character())
  }, .package = "SEMANTICA")
  SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L, bounds = "pos.var")
  expect_length(calls, 3L)
  expect_true(all(vapply(calls, function(z) identical(z$bounds, "pos.var") &&
    identical(z$optim.method, "nlminb"), logical(1L))))
})

test_that("all native DFI cache identities distinguish the bounds policy", {
  x <- .bounds_fixture()
  fit <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L, rotation = "none", bounds = "pos.var")
  seen <- list()
  local_mocked_bindings(.semantica_dfi_cache_get = function(cache, target) {
    seen[[length(seen) + 1L]] <<- target
    list(cache_probe = TRUE)
  }, .package = "SEMANTICA")
  for (fun in c("compute_esem_parametric_dfi_cutoffs", "compute_semantic_approx_dfi_cutoffs", "compute_semantic_roc_dfi_cutoffs")) {
    args <- list(esem_fit = fit, esem_syntax = x$syntax, factors = "F", items_per_factor = c(F = 3L), verbose = FALSE)
    if (fun != "compute_esem_parametric_dfi_cutoffs") args$observed_cor <- x$K
    if (fun == "compute_semantic_roc_dfi_cutoffs") args$factor_assignment <- x$fa
    for (b in c("none", "pos.var")) expect_true(do.call(getFromNamespace(fun, "SEMANTICA"), c(args, list(bounds = b)))$cache_probe)
  }
  expect_identical(vapply(seen, `[[`, "", "bounds"), rep(c("none", "pos.var"), 3L))
})

test_that("reference-N diagnostics separate convergence from admissibility", {
  x <- .bounds_fixture()
  plain <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L,
    rotation = "none", fallback = FALSE, return_diagnostics = TRUE)
  local_mocked_bindings(run_esem_on_matrix = function(...) plain, .package = "SEMANTICA")
  out <- SEMANTICA:::evaluate_semantic_n_sensitivity(x$syntax, x$K, x$fa, "F", c(250L, 500L), rotation = "none")
  expect_true(all(out$grid_results$converged))
  expect_false(any(out$grid_results$admissible))
  expect_false(any(out$grid_results$usable))
  expect_identical(out$summary$successful_fits, 0L)
})

test_that("guided ACO uses bounded fits through archive and final selection", {
  ids <- paste0("item_", seq_len(12L)); factors <- rep(c("F1", "F2"), each = 6L)
  lambda <- matrix(.08, 12L, 2L)
  lambda[cbind(seq_len(12L), rep(1:2, each = 6L))] <- .68
  K <- lambda %*% matrix(c(1, .25, .25, 1), 2L) %*% t(lambda)
  diag(K) <- 1; dimnames(K) <- list(ids, ids)
  original_fit <- SEMANTICA:::run_esem_on_matrix; policies <- character()
  local_mocked_bindings(run_esem_on_matrix = function(..., bounds = "none") {
    policies <<- c(policies, bounds)
    original_fit(..., bounds = bounds)
  }, .package = "SEMANTICA")
  out <- ACO_with_ESEM(K, data.frame(item = ids, factor = factors),
    i.per.f = c(F1 = 4L, F2 = 4L), ants = 4L, max.iter = 1L, max_total_iter = 1L,
    esem_every = 1L, run_esem_during_search = TRUE, max_esem_fits = 2L,
    esem_failure_policy = "stop", dfi_mode = "heuristic_semantic", esem_bounds = "pos.var",
    pfa_mode = "off", run_pfa_during_search = FALSE, semantic_n_sensitivity = FALSE,
    validation_n_diagnostic = FALSE, final_dddfi = FALSE, final_equivtest = FALSE,
    use_parallel = FALSE, seed = 20260820L, verbose = FALSE)
  expect_equal(out$evaluation_telemetry$esem_fits_started, 2L)
  expect_true(out$esem_admissible)
  expect_true(length(policies) > 2L)
  expect_true(all(head(policies, -1L) == "pos.var"))
  expect_identical(tail(policies, 1L), "none") # separate post-selection diagnostic
  expect_true(out$esem_unconstrained_diagnostic$admissible)
})

test_that("native DFI refits and isolated worker rescues retain the policy", {
  skip_on_cran()
  x <- .bounds_fixture()
  fit <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$K, 500L, rotation = "none", bounds = "pos.var")
  set.seed(8102)
  out <- SEMANTICA:::compute_semantic_approx_dfi_cutoffs(fit, x$syntax, "F", c(F = 3L),
    observed_cor = x$K, n_obs = 500L, rotation = "none", reps = 20L,
    n_cores = 1L, bounds = "pos.var", verbose = FALSE)
  specification <- out$simulation_provenance$calibration_specification
  expect_identical(specification$bounds, "pos.var")
  records <- out$simulation_provenance$fit_diagnostics
  expect_length(records, 20L)
  expect_true(all(vapply(records, function(z) length(z$solver_records) > 0L &&
    all(vapply(z$solver_records, function(s) identical(s$bounds, "pos.var"), logical(1L))), logical(1L))))
  # Real fits are verified above. Exercise transport and fallback arguments in
  # a lightweight worker without loading the package's plotting/LLM dependencies.
  helpers <- c(
    "run_esem_on_matrix", ".semantica_solver_state", "%||%",
    ".semantica_fast_lavaan_se", ".semantica_try_lavaan", "is_admissible_esem_fit",
    ".semantica_assess_esem_fit", "assess_esem_admissibility", ".semantica_safe_lav_inspect",
    ".semantica_collect_numeric", ".semantica_numeric_matrix")
  exports <- new.env(parent = baseenv())
  for (name in helpers) {
    f <- getFromNamespace(name, "SEMANTICA"); environment(f) <- globalenv()
    exports[[name]] <- f
  }
  cl <- parallel::makePSOCKcluster(1L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  parallel::clusterExport(cl, helpers, envir = exports)
  entry <- function(args) {
    assign(".semantica_try_lavaan", function(fit_args) list(fit = NULL,
      error = paste(fit_args$bounds, fit_args$optim.method), warnings = character()), globalenv())
    do.call(get("run_esem_on_matrix", globalenv()), args)
  }
  environment(entry) <- baseenv()
  worker <- parallel::clusterCall(cl, entry, list(syntax = x$syntax, cor_matrix = x$K,
    n_obs = 500L, bounds = "pos.var", return_diagnostics = TRUE))[[1L]]
  expect_length(worker$solver_records, 3L)
  expect_true(all(vapply(worker$solver_records,
    function(s) identical(s$error, "pos.var nlminb"), logical(1L))))
})
