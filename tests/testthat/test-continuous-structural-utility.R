.continuous_fixture <- function() {
  ids <- paste0("item_", seq_len(16L)); factors <- c("First trait", "Second trait")
  assignment <- setNames(rep(factors, each = 8L), ids)
  lambda <- matrix(.08, 16L, 2L, dimnames = list(ids, factors))
  lambda[cbind(seq_len(16L), rep(1:2, each = 8L))] <- rep(seq(.45, .80, length.out = 8L), 2L)
  observed <- tcrossprod(lambda); diag(observed) <- 1
  list(lambda = lambda, assignment = assignment, factors = factors, observed = observed)
}

test_that("continuous utilities respond to declared preferences without loading-band jumps", {
  x <- .continuous_fixture(); evaluate <- SEMANTICA:::.semantica_continuous_utility
  score <- function(primary, cross = .08) {
    lambda <- x$lambda; lambda[,] <- cross
    lambda[cbind(seq_len(nrow(lambda)), match(x$assignment, x$factors))] <- primary
    evaluate(lambda, x$assignment, x$factors, mode = "continuous_structure")$score
  }
  expect_gt(score(.96), score(.95))
  expect_gt(score(.50), score(.49))
  expect_lt(score(.60, .30), score(.60, .10))
  expect_lt(abs(score(.5 + 1e-9) - score(.5 - 1e-9)), 1e-7)
  expect_equal(score(.2, .2), 0)
  expect_gt(score(1e-10, 0), 0)
  expect_lt(score(1e-10, 0), 1e-9)
})

test_that("domain aggregation respects unequal counts, identifiers and axis signs", {
  x <- .continuous_fixture(); evaluate <- SEMANTICA:::.semantica_continuous_utility
  base <- evaluate(x$lambda, x$assignment, x$factors, mode = "continuous_structure")
  expect_true(base$available)
  flipped <- x$lambda[, 2:1, drop = FALSE]; flipped[, 1] <- -flipped[, 1]
  expect_equal(evaluate(flipped[16:1, ], x$assignment, rev(x$factors),
    mode = "continuous_structure")$score, base$score)
  extra <- x$lambda[1:8, , drop = FALSE]; rownames(extra) <- paste0("extra", 1:8)
  augmented <- rbind(x$lambda, extra)
  assignment <- c(x$assignment, setNames(rep(x$factors[1], 8L), rownames(extra)))
  expect_equal(evaluate(augmented, assignment, x$factors,
    mode = "continuous_structure")$score, base$score)
  invalid <- x$assignment; invalid[1] <- NA_character_
  expect_false(evaluate(x$lambda, invalid, x$factors)$available)
  expect_false(evaluate(x$lambda[, 1, drop = FALSE], x$assignment, x$factors)$available)
  expect_false(evaluate(cbind(x$lambda, x$lambda[, 1, drop = FALSE]),
    x$assignment, x$factors)$available)
})

test_that("reconstruction uses off-diagonal improvement over identity and preserves failures", {
  x <- .continuous_fixture(); evaluate <- SEMANTICA:::.semantica_continuous_utility
  calc <- function(implied, observed = x$observed) evaluate(x$lambda, x$assignment,
    x$factors, observed, implied, mode = "continuous_reconstruction")
  expect_equal(calc(x$observed)$score, 1)
  identity <- diag(16L); dimnames(identity) <- dimnames(x$observed)
  expect_equal(calc(identity)$score, 0)
  half <- x$observed / 2; diag(half) <- 1
  expect_equal(calc(half)$score, .75)
  worse <- -x$observed; diag(worse) <- 1
  expect_equal(calc(worse)$score, 0)
  expect_lt(calc(worse)$reconstruction_r2, 0)
  zero <- calc(identity, identity)
  expect_equal(zero$score, 0)
  expect_true(is.na(zero$reconstruction_r2))
  expect_identical(zero$reason, "no_offdiagonal_signal")
  expect_false(calc(NULL)$available)
  expect_false(calc(matrix(NA_real_, 16L, 16L))$available)
  expect_equal(calc(x$observed[16:1, 16:1])$score, 1)
})

test_that("one-factor utilities omit vacuous separation and balanced combines available evidence", {
  ids <- c("a", "b", "c", "d")
  lambda <- matrix(.7, 4L, 1L, dimnames = list(ids, "Only trait"))
  assignment <- setNames(rep("Only trait", 4L), ids)
  observed <- tcrossprod(lambda); diag(observed) <- 1
  evaluate <- SEMANTICA:::.semantica_continuous_utility
  structure <- evaluate(lambda, assignment, "Only trait", mode = "continuous_structure")
  balanced <- evaluate(lambda, assignment, "Only trait", observed, observed)
  expect_true(is.na(structure$components[["separation"]]))
  expect_equal(structure$score, .7)
  expect_equal(balanced$score, 2 / (1 / .7 + 1))
  expect_false(evaluate(lambda, assignment, "Only trait")$available)
})

test_that("report-only guards preserve reference failures but contribute no penalty", {
  fit <- list(converged = TRUE, admissible = TRUE, loading_quality = .1, ave = .1,
    ave_warnings = "descriptive", structure_diagnostics = list(min_primary_loading = .1, primary_ge_50 = .1))
  guard <- SEMANTICA:::compute_psychometric_guard_penalty(fit, guard_action = "report_only")
  expect_equal(as.numeric(guard), 1)
  d <- attr(guard, "diagnostics")
  expect_false(d$passed)
  expect_true(all(d$criteria$multiplier == 1))
  expect_true(all(d$criteria$role[d$criteria$constraint_active & d$criteria$criterion != "converged"] == "diagnostic"))
  missing <- SEMANTICA:::compute_psychometric_guard_penalty(list(converged = FALSE), guard_action = "report_only")
  expect_equal(as.numeric(missing), 1)
  expect_true(any(attr(missing, "diagnostics")$criteria$state == "unavailable"))
  expect_lt(as.numeric(SEMANTICA:::compute_psychometric_guard_penalty(fit)), 1)
})

test_that("new modes are opt-in and public configuration reaches the pipeline", {
  expect_identical(semantica_esem_config()$score_mode, "current")
  expect_identical(semantica_quality_config()$psychometric_guard_action, "penalty")
  for (mode in c("continuous_structure", "continuous_reconstruction", "continuous_balanced"))
    expect_identical(semantica_esem_config(score_mode = mode)$score_mode, mode)
  expect_error(semantica_esem_config(score_mode = "automatic_validity"), "arg")
  captured <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    captured <<- list(...); list()
  }, .semantica_wrap_run_result = identity, .package = "SEMANTICA")
  semantica_run_custom("Any scale", "Description", list(Trait = "Any construct"),
    esem = semantica_esem_config(score_mode = "continuous_balanced"), plots = "none")
  expect_identical(captured$semantic_esem_score_mode, "continuous_balanced")
  expect_identical(captured$psychometric_guard_action, "report_only")
  expect_identical(captured$final_selection_mode, "scalar")
  semantica_run_custom("Any scale", "Description", list(Trait = "Any construct"),
    esem = semantica_esem_config(score_mode = "continuous_structure"),
    quality = semantica_quality_config(psychometric_guard_action = "final_constraint"), plots = "none")
  expect_identical(captured$psychometric_guard_action, "final_constraint")
})

test_that("utility boundaries are finite across factor counts and sparse loading patterns", {
  withr::local_seed(137L)
  for (k in c(1L, 2L, 5L, 17L)) {
    factors <- paste0("F", seq_len(k)); ids <- paste0("i", seq_len(k * 3L))
    assignment <- setNames(rep(factors, each = 3L), ids)
    for (scale in c(0, 1e-10, .4, 1, 2)) {
      lambda <- matrix(stats::runif(length(ids) * k, -scale, scale),
        nrow = length(ids), dimnames = list(ids, factors))
      observed <- tcrossprod(lambda) + diag(length(ids)); observed <- stats::cov2cor(observed)
      for (mode in c("continuous_structure", "continuous_reconstruction", "continuous_balanced")) {
        z <- SEMANTICA:::.semantica_continuous_utility(lambda, assignment, factors, observed, observed, mode)
        expect_true(z$available)
        expect_true(is.finite(z$score) && z$score >= 0 && z$score <= 1)
      }
    }
  }
})

test_that("continuous archive seed identity is independent of locale and item ordering", {
  original <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", original)), add = TRUE)
  items <- enc2utf8(c("item_\u00e1", "item_\u00f1", "item_1"))
  seed <- SEMANTICA:::.semantica_utility_fit_seed(101L, items)
  expect_identical(SEMANTICA:::.semantica_utility_fit_seed(101, rev(items)), seed)
  for (locale in if (.Platform$OS.type == "windows") c("C", "Spanish_Peru.utf8") else c("C", "C.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale))))
      expect_identical(SEMANTICA:::.semantica_utility_fit_seed(101L, items), seed)
  }
})

test_that("actual ESEM utility is independent of descriptive fit references", {
  x <- .continuous_fixture(); ids <- c(1:4, 9:12)
  observed <- x$observed[ids, ids]; assignment <- x$assignment[ids]
  syntax <- SEMANTICA:::build_esem_syntax_safe(rownames(observed), assignment, x$factors)
  fit <- SEMANTICA:::run_esem_on_matrix(syntax, observed, 500L, rotation = "oblimin")
  expect_true(SEMANTICA:::is_admissible_esem_fit(fit))
  for (mode in c("continuous_structure", "continuous_reconstruction", "continuous_balanced")) {
    a <- SEMANTICA:::extract_and_score_esem(fit, observed, assignment, x$factors, score_mode = mode)
    b <- SEMANTICA:::extract_and_score_esem(fit, observed, assignment, x$factors,
      cutoffs = list(cfi = .5, tli = .5, rmsea = .5, srmr = .5),
      htmt_threshold = .1, htmt_objective_role = "penalty", score_mode = mode)
    expect_true(a$continuous_utility$available)
    expect_equal(a$score, b$score)
    expect_equal(a$score, a$continuous_utility$score)
    expect_true(a$score >= 0 && a$score <= 1)
  }
  missing <- SEMANTICA:::extract_and_score_esem(fit, NULL, assignment, x$factors,
    score_mode = "continuous_balanced")
  expect_true(missing$admissible)
  expect_false(missing$objective_available)
  expect_true(is.na(missing$score))
  structure_only <- SEMANTICA:::extract_and_score_esem(fit, NULL, assignment, x$factors,
    score_mode = "continuous_structure")
  expect_true(structure_only$objective_available)
})

.continuous_aco <- function(mode, use_parallel = FALSE, ...) {
  x <- .continuous_fixture()
  ACO_with_ESEM(x$observed, data.frame(item = names(x$assignment), factor = unname(x$assignment)),
    i.per.f = setNames(c(4L, 4L), x$factors), ants = 3L, max.iter = 1L, max_total_iter = 1L,
    esem_every = 1L, esem_eval_top_k = 2L, max_esem_fits = 2L,
    semantic_esem_score_mode = mode, esem_failure_policy = "semantic_fallback",
    dfi_warmup_iters = 0L, pfa_mode = "off", run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE,
    final_dddfi = FALSE, final_dfi_recalibrate = FALSE, final_equivtest = FALSE,
    rotation = "oblimin", use_parallel = use_parallel, n.cores = if (use_parallel) 2L else 1L,
    seed = 101L, verbose = FALSE, ...)
}

test_that("native search and finalization use continuous utility without search DFI or guard repair", {
  local_mocked_bindings(extract_fitted_dfi_params_esem = function(...) stop("Unneeded DFI bootstrap"),
    compute_dfi_cutoffs_from_model_spec = function(...) stop("Unneeded DFI fallback"),
    compute_semantic_approx_dfi_cutoffs = function(...) stop("Unneeded DFI simulation"),
    .package = "SEMANTICA")
  for (mode in c("continuous_structure", "continuous_reconstruction", "continuous_balanced")) {
    result <- .continuous_aco(mode, psychometric_guard_min_loading = .99,
      psychometric_guard_min_ave = .99, final_structure_repair = TRUE)
    expect_length(result$best_items, 8L)
    expect_identical(result$structural_guard$action, "report_only")
    expect_false(result$structural_guard$selection_active)
    expect_false(result$structural_guard$passed)
    expect_identical(result$validation_status, "not_assessed_report_only")
    expect_true(is.na(result$eligible_for_participant_validation))
    expect_identical(result$dfi_calibration_path$active_stage, "not_required_continuous_utility")
    expect_equal(result$evaluation_telemetry$dfi_fits_started, 0)
    policy <- result$objective_context$objective_schema$utility_policy
    expect_false(policy$effective$final_structure_repair)
    expect_false(policy$effective$elite_multicriteria_rerank)
    expect_equal(policy$effective$guard_weight, 0)
    expect_identical(result$esem_result$continuous_utility$mode, mode)
  }
})

test_that("native one-factor search supports continuous structure and reports its single component", {
  ids <- paste0("u", 1:8); loadings <- seq(.45, .8, length.out = 8)
  observed <- tcrossprod(loadings); diag(observed) <- 1; dimnames(observed) <- list(ids, ids)
  result <- ACO_with_ESEM(observed, data.frame(item = ids, factor = "One trait"),
    i.per.f = c("One trait" = 4L), ants = 2L, max.iter = 1L, max_total_iter = 1L,
    esem_every = 1L, max_esem_fits = 2L, semantic_esem_score_mode = "continuous_balanced",
    pfa_mode = "off", run_pfa_during_search = FALSE, dfi_warmup_iters = 0L,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE, final_dddfi = FALSE,
    final_dfi_recalibrate = FALSE, final_equivtest = FALSE, use_parallel = FALSE,
    seed = 101L, verbose = FALSE)
  expect_length(result$best_items, 4L)
  expect_true(result$esem_result$continuous_utility$available)
  expect_true(is.na(result$esem_result$continuous_utility$components[["separation"]]))
})

test_that("report-only references cannot change native selection or utility", {
  a <- .continuous_aco("continuous_balanced", psychometric_guard_min_loading = .1,
    psychometric_guard_min_ave = .1)
  b <- .continuous_aco("continuous_balanced", psychometric_guard_min_loading = .99,
    psychometric_guard_min_ave = .99, psychometric_guard_min_primary_ge_50 = .99)
  expect_identical(a$best_items, b$best_items)
  expect_equal(a$objective_context$value, b$objective_context$value)
  expect_false(b$structural_guard$passed)
  expect_identical(a$search_calibration$semantic_esem_score_mode, "continuous_balanced")
  expect_error(.continuous_aco("continuous_structure", search_calibration = a$search_calibration),
    "different ESEM utility policy")
})

test_that("continuous scoring works in native parallel search and archive workers", {
  serial <- .continuous_aco("continuous_balanced")
  parallel <- .continuous_aco("continuous_balanced", use_parallel = TRUE)
  expect_identical(parallel$best_items, serial$best_items)
  # The utility depends on rotated loadings; compare at the configured GPA
  # convergence tolerance rather than demanding extra precision from the solver.
  rotation_tolerance <- serial$esem_fit@Options$rotation.args$gpa_tol
  expect_lt(abs(parallel$esem_result$score - serial$esem_result$score), rotation_tolerance)
  expect_identical(parallel$esem_result$continuous_utility$fit_seed,
    serial$esem_result$continuous_utility$fit_seed)
  expect_equal(nrow(parallel$reproducibility$search_esem_parallel_fallbacks), 0L)
  expect_equal(nrow(parallel$reproducibility$final_parallel_fallbacks), 0L)
})

test_that("continuous public prepared-input runs need no providers or search calibration", {
  x <- readRDS(test_path("fixtures", "benchmark-prepared-C01.rds"))
  p <- semantica_prepare_inputs(x$selection$items, x$selection$embeddings,
    x$task$factors, x$selection$references)
  args <- x$arguments
  args$prepared_inputs <- p; args$checkpoint_dir <- NULL
  args$resources <- semantica_resource_config(cpu_cores = 1L)
  args$esem <- semantica_esem_config(score_mode = "continuous_balanced", cadence_mode = "fixed")
  args$quality <- semantica_quality_config(psychometric_guard_action = "report_only",
    final_selection_mode = "scalar")
  args$diagnostics <- semantica_diagnostics_config(final_fit = "off", semantic_stability = FALSE)
  args$max_total_iter <- 1L; args$ants <- 2L; args$max_esem_fits <- 2L
  local_mocked_bindings(.call_chat = function(...) stop("Provider prohibited"),
    semantica_embed = function(...) stop("Provider prohibited"),
    extract_fitted_dfi_params_esem = function(...) stop("Search DFI prohibited"),
    .package = "SEMANTICA")
  result <- suppressWarnings(do.call(semantica_run_custom, args))
  expect_s3_class(result, "semantica_run_result")
  raw <- result$advanced
  expect_identical(raw$structural_guard$action, "report_only")
  expect_true(raw$optimization$esem_result$continuous_utility$available)
  expect_equal(raw$evaluation_telemetry$dfi_fits_started, 0L)
  expect_true(is.na(summary(raw)$eligible_for_participant_validation))
})
