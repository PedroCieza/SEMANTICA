.early_failure_fixture <- function() {
  ids <- paste0("v", seq_len(8L))
  fa <- setNames(rep(c("F1", "F2"), each = 4L), ids)
  lambda <- matrix(.08, 8L, 2L)
  lambda[cbind(seq_len(8L), rep(1:2, each = 4L))] <- .70
  cor <- lambda %*% matrix(c(1, .2, .2, 1), 2L) %*% t(lambda)
  diag(cor) <- 1; dimnames(cor) <- list(ids, ids)
  list(cor = cor, fa = fa,
    syntax = SEMANTICA:::build_esem_syntax_safe(ids, fa, c("F1", "F2")))
}

test_that("terminal optimizer failure cancels rotation and restores the entry seed", {
  reached_rotation <- FALSE
  local_mocked_bindings(sem = function(..., warn = FALSE) {
    expect_true(warn)
    runif(3L)
    warning("lavaan: the optimizer warns that a solution has NOT been found!")
    reached_rotation <<- TRUE
    stop("rotation must not execute")
  }, .package = "lavaan")
  set.seed(604729L)
  before <- .Random.seed
  out <- SEMANTICA:::.semantica_try_lavaan(list())
  expect_false(reached_rotation)
  expect_null(out$fit)
  expect_identical(out$failure_reason, "optimizer_nonconvergence_before_rotation")
  expect_match(out$error, "NOT been found")
  expect_length(out$warnings, 1L)
  expect_identical(.Random.seed, before)
})

test_that("failed estimation does not create a seed when the caller had none", {
  before <- get0(".Random.seed", .GlobalEnv, inherits = FALSE)
  withr::defer({
    if (is.null(before)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", before, .GlobalEnv)
  })
  if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  local_mocked_bindings(sem = function(...) {
    runif(1L)
    warning("Model estimation FAILED! Returning starting values.")
    stop("unreachable rotation")
  }, .package = "lavaan")
  out <- SEMANTICA:::.semantica_try_lavaan(list())
  expect_null(out$fit)
  expect_identical(out$failure_reason, "optimizer_nonconvergence_before_rotation")
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("rotation-start and ordinary warnings do not cancel a fit", {
  marker <- list(completed = TRUE)
  local_mocked_bindings(sem = function(...) {
    warning("GP rotation algorithm did not converge after 10000 iterations")
    warning("some estimated ov variances are negative")
    marker
  }, .package = "lavaan")
  out <- SEMANTICA:::.semantica_try_lavaan(list())
  expect_identical(out$fit, marker)
  expect_null(out$failure_reason)
  expect_null(out$error)
  expect_length(out$warnings, 2L)
})

test_that("lavaan's original-marker recovery is not interrupted", {
  marker <- list(recovered = TRUE)
  local_mocked_bindings(sem = function(...) {
    warning("the first indicator is a poor item; switching to another marker item")
    warning("the optimizer warns that a solution has NOT been found!")
    # lavaan can now retry the original marker and return a valid fit.
    marker
  }, .package = "lavaan")
  out <- SEMANTICA:::.semantica_try_lavaan(list())
  expect_identical(out$fit, marker)
  expect_null(out$failure_reason)
  expect_length(out$warnings, 2L)
})

test_that("a successful rotated fit retains native coefficients and fit measures", {
  x <- .early_failure_fixture()
  map <- attr(x$syntax, "semantica_lavaan_name_map")
  cor <- x$cor
  dimnames(cor) <- list(unname(map$item_to_internal[rownames(cor)]),
                       unname(map$item_to_internal[colnames(cor)]))
  set.seed(604730L, kind = "L'Ecuyer-CMRG")
  before <- .Random.seed
  reference <- lavaan::sem(x$syntax, sample.cov = cor, sample.nobs = 500L,
    estimator = "ML", rotation = "geomin", rotation.args = list(geomin.epsilon = .5),
    sample.cov.rescale = FALSE, se = "none", warn = FALSE,
    check.post = TRUE, control = list(iter.max = 2000L))
  expect_true(lavaan::lavInspect(reference, "converged"))
  expect_identical(.Random.seed, before)
  out <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$cor, n_obs = 500L,
    fallback = FALSE, return_diagnostics = TRUE)
  expect_true(SEMANTICA:::is_admissible_esem_fit(out$fit))
  expect_equal(lavaan::coef(out$fit), lavaan::coef(reference), tolerance = 1e-12)
  expect_equal(lavaan::fitMeasures(out$fit, c("cfi", "tli", "rmsea", "srmr")),
    lavaan::fitMeasures(reference, c("cfi", "tli", "rmsea", "srmr")), tolerance = 1e-12)
  expect_identical(.Random.seed, before)
  expect_equal(out$accepted_attempt, 1L)
  expect_true(out$solver_records[[1L]]$admissible)
})

test_that("a skipped failed attempt still permits numerical rescue", {
  x <- .early_failure_fixture()
  fit <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$cor, n_obs = 500L, fallback = FALSE)
  calls <- list()
  local_mocked_bindings(.semantica_try_lavaan = function(args) {
    calls[[length(calls) + 1L]] <<- args
    if (length(calls) == 1L) return(list(fit = NULL,
      error = "optimizer failed", warnings = "optimizer failed",
      failure_reason = "optimizer_nonconvergence_before_rotation"))
    list(fit = fit, error = NULL, warnings = character(), failure_reason = NULL)
  }, .package = "SEMANTICA")
  out <- SEMANTICA:::run_esem_on_matrix(x$syntax, x$cor, n_obs = 500L, return_diagnostics = TRUE)
  expect_length(calls, 2L)
  expect_identical(calls[[2L]]$optim.method, "BFGS")
  expect_equal(out$accepted_attempt, 2L)
  expect_true(SEMANTICA:::is_admissible_esem_fit(out$fit))
  expect_false(out$solver_records[[1L]]$admissible)
  expect_true(out$solver_records[[2L]]$admissible)
})

test_that("bootstrap forwards its iteration limit and distinguishes inadmissibility", {
  x <- .early_failure_fixture()
  received <- NULL
  local_mocked_bindings(run_esem_on_matrix = function(..., iter_max,
      return_diagnostics, verbose) {
    received <<- list(iter_max = iter_max, diagnostics = return_diagnostics)
    list(fit = NULL, rejection_assessment = list(reasons = "negative_residual_variance"),
      solver_records = list(list(admissible = FALSE)))
  }, .package = "SEMANTICA")
  expect_message(out <- SEMANTICA:::extract_fitted_dfi_params_esem(names(x$fa), x$fa,
    c("F1", "F2"), x$cor, iter_max = 175L, return_diagnostics = TRUE),
    "No admissible fit: negative_residual_variance")
  expect_identical(received, list(iter_max = 175L, diagnostics = TRUE))
  expect_null(out$parameters)
  expect_identical(out$failure_reason, "negative_residual_variance")
  expect_false(out$solver$solver_records[[1L]]$admissible)
  expect_null(SEMANTICA:::extract_fitted_dfi_params_esem(names(x$fa), x$fa,
    c("F1", "F2"), x$cor, verbose = FALSE))
})

test_that("participant-response fits also stop before rotating failed estimation", {
  reached_rotation <- FALSE
  local_mocked_bindings(sem = function(...) {
    warning("Model estimation FAILED! Returning starting values.")
    reached_rotation <<- TRUE
    NULL
  }, .package = "lavaan")
  x <- .early_failure_fixture()
  dat <- as.data.frame(matrix(1, 10L, 8L)); names(dat) <- names(x$fa)
  expect_null(SEMANTICA:::run_esem_on_response_data(x$syntax, dat, names(x$fa), fallback = FALSE))
  expect_false(reached_rotation)
})
