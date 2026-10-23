test_that("guard retains required rows and distinguishes missing evidence", {
  f <- list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = .5)
  guard <- function(x, ...) attr(SEMANTICA:::compute_psychometric_guard_penalty(x, ...), "diagnostics")
  g <- guard(f)
  expect_false(g$passed)
  expect_setequal(g$criteria$criterion[g$criteria$state == "unavailable"], c("min_primary_loading", "primary_ge_50"))
  expect_true(all(is.na(g$criteria$value[g$criteria$state == "unavailable"])))
  f$structure_diagnostics <- list(min_primary_loading = .7, primary_ge_50 = .9)
  expect_true(guard(f)$passed)
  expect_equal(as.numeric(SEMANTICA:::compute_psychometric_guard_penalty(f)), 1)
  for (bad in list(NULL, numeric(), NA_real_, Inf, c(.8, .9))) {
    f$structure_diagnostics$min_primary_loading <- bad
    expect_false(guard(f)$passed)
  }
  f$structure_diagnostics$min_primary_loading <- .7
  expect_false(guard(f, pfa_max_abs_loading = 1)$passed)
  expect_true(guard(f, pfa_max_abs_loading = Inf)$passed)
  f$htmt_status <- "unavailable"; f$htmt_max <- 0
  unavailable_htmt <- guard(f, htmt_guard_threshold = .85)
  expect_false(unavailable_htmt$passed)
  expect_true(is.na(unavailable_htmt$criteria$value[unavailable_htmt$criteria$criterion == "htmt_max"]))
  expect_equal(unavailable_htmt$criteria$numeric_compatibility_alias[unavailable_htmt$criteria$criterion == "htmt_max"], 0)
  f$htmt_status <- "not_applicable"; f$htmt_max <- NA_real_
  expect_true(guard(f, htmt_guard_threshold = .85)$passed)
  f$converged <- FALSE
  expect_false(guard(f)$passed)
})

test_that("numerical prototype ties are order invariant", {
  classify <- SEMANTICA:::.semantica_classify_alignment_margins
  expect_identical(classify(c(0, .1, .2))$status, c("tied", "aligned", "aligned"))
  expect_identical(classify(c(1e-12, NA_real_))$status, c("tied", "unavailable"))
  m <- rbind(c(.4, .4), c(.5, .1))
  top <- SEMANTICA:::.semantica_unique_top_prototype
  expect_identical(unname(top(m, c("A", "B"))), c(NA_character_, "A"))
  expect_identical(unname(top(m[, 2:1], c("B", "A"))), c(NA_character_, "A"))
})

test_that("gauges use evaluated values and do not inherit response labels", {
  f <- list(converged = TRUE, loading_quality = .8, ave = .18813, htmt_max = 0,
    htmt_status = "unavailable", structure_diagnostics = list(min_primary_loading = .7, primary_ge_50 = .9))
  g <- attr(SEMANTICA:::compute_psychometric_guard_penalty(f, min_ave = .3), "diagnostics")
  r <- list(esem_result = c(f, list(cfi = .96, tli = .96, rmsea = .04, srmr = .04)),
    active_cutoffs = list(cfi = .95, tli = .95, rmsea = .06, srmr = .08),
    structural_diagnostics = g)
  p <- plot_dfi_gauges(r)
  expect_equal(p$data$cutoff[p$data$metric == "AVE"], .3)
  expect_false(p$data$pass[p$data$metric == "AVE"])
  expect_identical(p$data$state[p$data$metric == "HTMT"], "inactive")
  expect_true(is.na(p$data$pass[p$data$metric == "TLI"]))
  r$response_validation <- list(result = list(ave = .7), criteria = NULL)
  expect_identical(plot_dfi_gauges(r)$data, p$data)
  rp <- plot_dfi_gauges(r, "response")
  expect_equal(rp$data$observed[rp$data$metric == "AVE"], .7)
  expect_true(all(is.na(rp$data$pass)))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("response measurements do not borrow semantic references", {
  captured <- list()
  missing_cfi <- FALSE
  local_mocked_bindings(
    run_esem_on_response_data = function(...) list(),
    compute_response_cor = function(...) diag(4),
    extract_and_score_esem = function(..., cutoffs, score_mode) {
      args <- list(...)
      captured[[length(captured) + 1L]] <<- args
      list(cfi = if (missing_cfi) NA_real_ else .96, tli = .95, rmsea = .04, srmr = .05, converged = TRUE, admissible = TRUE)
    }, .package = "SEMANTICA")
  ids <- paste0("i", 1:4); fa <- setNames(c("A", "A", "B", "B"), ids)
  dat <- as.data.frame(matrix(1:40, 10, dimnames = list(NULL, ids)))
  run <- function(a = NULL) SEMANTICA:::.semantica_validate_responses("model", dat, ids, fa, c("A", "B"), analysis = a)
  a <- run()
  expect_true(a$performed); expect_true(a$converged); expect_true(a$admissible)
  expect_true(is.na(a$criteria_met)); expect_true(is.na(a$result$score))
  expect_true(all(a$criteria$state == "unavailable"))
  b <- run(list(cutoffs = list(cfi=.9,tli=.9,rmsea=.06,srmr=.08), criterion_provenance="prespecified fixture"))
  expect_true(b$criteria_met)
  expect_true(is.na(b$result$score))
  expect_null(b$result$score_decomp)
  expect_true(is.na(b$result$htmt_reference_exceeded))
  expect_identical(a$result$cfi, b$result$cfi)
  expect_identical(a$dataset_role, "unspecified")
  expect_equal(a$input_n_obs, nrow(dat))
  expect_true(is.na(a$n_obs))
  expect_false(a$held_out)
  missing_cfi <- TRUE
  uncertain <- run(list(cutoffs = list(cfi=.9,tli=.9,rmsea=.06,srmr=.08), criterion_provenance="prespecified fixture"))
  expect_true(uncertain$converged)
  expect_true(uncertain$admissible)
  expect_true(is.na(uncertain$criteria_met))
})


test_that("loading-square summary is complete and preserves complete-case arithmetic", {
  lambda <- rbind(i1 = c(.8, .1), i2 = c(.6, .2), i3 = c(.1, .7))
  colnames(lambda) <- c("A", "B")
  theta <- diag(3); dimnames(theta) <- list(rownames(lambda), rownames(lambda))
  local_mocked_bindings(extract_aligned_esem_solution = function(...) list(lambda = lambda, theta = theta), .package = "SEMANTICA")
  fa <- c(i1 = "A", i2 = "A", i3 = "B")
  metric <- function() SEMANTICA:::compute_ave_esem(list(), fa, c("A", "B"))
  x <- metric()
  expect_equal(as.numeric(x), .49)
  expect_equal(unname(attr(x, "factor_ave")), c(.5, .49))
  expect_identical(attr(x, "limiting_factor"), "B")
  expect_equal(attr(x, "indicator_counts")$expected, c(2, 1))
  lambda <- lambda[1:2, , drop = FALSE]
  expect_true(is.na(metric()))
  expect_identical(attr(metric(), "status"), "unavailable")
  lambda <- rbind(lambda, i3 = c(.1, 1.2))
  expect_equal(as.numeric(metric()), .5)
  expect_length(attr(metric(), "ave_warnings"), 1L)
  expect_equal(attr(metric(), "unclipped_loading_squares")$B, c(i3 = 1.44))
  lambda[1, 1] <- NA_real_
  expect_true(is.na(metric()))
})

test_that("benchmark failures retain every scheduled method and seed", {
  out <- semantica_compare_selection_methods(
    list(broken = function(...) stop("method failed"), ok = function(...) list(value = .8, evaluation_cost = 3)),
    problem = NULL, budget = 5, evaluator = function(x) c(score = x$value), seeds = c(1L, 2L))
  expect_equal(nrow(out$results), 4L)
  expect_equal(sum(out$results$status == "method_error"), 2L)
  expect_length(out$raw, 4L)
  expect_equal(out$results$actual_evaluation_cost[out$results$status == "completed"], c(3, 3))
  expect_false(any(out$results$budget_enforced))
})

test_that("DFI quantile uncertainty uses successful refits and requested levels cannot change", {
  tab <- data.frame(CFI = .96, RMSEA = .05, SRMR = .06, row.names = "Level-2")
  missing <- SEMANTICA:::extract_cutoffs_from_dfi_result(list(cutoffs = tab), 1, "Sensitivity", FALSE)
  expect_true(missing$calibration_failed)
  expect_identical(missing$requested_level_status, "request_mismatch")
  expect_equal(missing$requested_level, 1)
  tab[1, "CFI"] <- .7
  x <- SEMANTICA:::extract_cutoffs_from_dfi_result(list(cutoffs = tab), 2, "Sensitivity", FALSE)
  expect_equal(x$raw_cutoffs$cfi, .7)
  expect_true(x$was_degenerate)
  q <- SEMANTICA:::.semantica_dfi_quantile_intervals(data.frame(cfi = seq(.9, .99, length.out = 100)), .05)
  expect_equal(q$n_successful, 100L)
  expect_lte(q$lower, as.numeric(quantile(seq(.9, .99, length.out = 100), .05)))
  expect_gte(q$upper, as.numeric(quantile(seq(.9, .99, length.out = 100), .05)))
})


test_that("stress tests retain manifest and distinguish experiment targets", {
  variants <- data.frame(original_index = 1L, variant_text = "wrong facet", manipulation = "facet", severity = .5)
  manifest <- list(label_independence = "expert_adjudicated", split = "development")
  x <- semantica_counterfactual_stress_test("original", variants, function(...) .2, benchmark_manifest = manifest)
  expect_identical(attr(x, "experiment"), "frozen_form")
  expect_identical(attr(x, "benchmark_manifest"), manifest)
  expect_equal(x$severity, .5)
  y <- semantica_counterfactual_stress_test("original", variants, function(...) .2,
    benchmark_manifest = manifest, experiment = "pool_reselection")
  expect_false(identical(attr(x, "benchmark_fingerprint"), attr(y, "benchmark_fingerprint")))
})


test_that("guard roles distinguish search utility, hard enforcement and numerical validity", {
  f <- list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = .5,
    structure_diagnostics = list(min_primary_loading = .7, primary_ge_50 = .9))
  soft <- attr(SEMANTICA:::compute_psychometric_guard_penalty(f), "diagnostics")$criteria
  hard <- attr(SEMANTICA:::compute_psychometric_guard_penalty(f, guard_action = "final_constraint"), "diagnostics")$criteria
  expect_identical(soft$role[soft$criterion == "ave"], "optimization_utility")
  expect_identical(hard$role[hard$criterion == "ave"], "final_constraint")
  expect_identical(hard$role[hard$criterion == "converged"], "numerical_admissibility")
  expect_false(identical(soft$resolved_configuration_hash, hard$resolved_configuration_hash))
})


test_that("gauge measurements never display scoring imputations as observed fit", {
  r <- list(esem_result = list(cfi = 0, tli = 0, rmsea = 1, srmr = 1,
    raw_fit_metrics = list(cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_)),
    active_cutoffs = list(cfi = .95, tli = .95, rmsea = .06, srmr = .08))
  p <- plot_dfi_gauges(r)
  expect_true(all(is.na(p$data$observed)))
  expect_identical(p$data$state[p$data$metric == "CFI"], "unavailable")
  expect_true(is.na(p$data$pass[p$data$metric == "CFI"]))
})


test_that("PSOCK source exports resolve current policy helpers and preserve evidence states", {
  cl <- parallel::makePSOCKcluster(1L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  # Legacy metadata hashes include R's serialization encoding. Match locales
  # for this exact export comparison; continuous fit seeds have their own
  # cross-locale regression coverage.
  locale <- Sys.getlocale("LC_CTYPE")
  worker_locale <- parallel::clusterCall(cl, function(x) Sys.setlocale("LC_CTYPE", x), locale)
  expect_identical(worker_locale[[1L]], locale)
  env <- new.env(parent = emptyenv())
  helpers <- c("compute_psychometric_guard_penalty", ".semantica_metric_scalar",
    ".semantica_evaluate_criteria", ".semantica_criterion_config_hash",
    ".semantica_decision_policy", ".semantica_object_md5",
    ".semantica_lower_guard_multiplier", ".semantica_upper_guard_multiplier")
  for (name in helpers) env[[name]] <- get(name, asNamespace("SEMANTICA"))
  env$.SEMANTICA_DECISION_POLICY <- SEMANTICA:::.semantica_decision_policy()
  SEMANTICA:::.semantica_cluster_export_environment(cl, env)
  fits <- list(list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = .5),
    list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = 0, htmt_status = "unavailable",
      structure_diagnostics = list(min_primary_loading = .7, primary_ge_50 = .9)),
    list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = .5,
      structure_diagnostics = list(min_primary_loading = .7, primary_ge_50 = .9)))
  worker <- parallel::parLapply(cl, fits, function(f) attr(get("compute_psychometric_guard_penalty", .GlobalEnv, inherits = FALSE)(f, htmt_guard_threshold = .85), "diagnostics"))
  serial <- lapply(fits, function(f) attr(SEMANTICA:::compute_psychometric_guard_penalty(f, htmt_guard_threshold = .85), "diagnostics"))
  expect_identical(worker, serial)
  expect_identical(vapply(worker, function(x) x$passed, logical(1L)), c(FALSE, FALSE, TRUE))
  expect_identical(parallel::clusterCall(cl, function() get(".semantica_decision_policy", .GlobalEnv, inherits = FALSE)()$schema_version)[[1]],
    SEMANTICA:::.semantica_decision_policy()$schema_version)
})


test_that("warning constraints are resolved before warning measurements", {
  expect_error(SEMANTICA:::compute_psychometric_guard_penalty(list(), warning_penalty = NA_real_), "warning_penalty")
  f <- list(converged = TRUE, loading_quality = .8, ave = .6, htmt_max = .5,
    structure_diagnostics = list(min_primary_loading = .7, primary_ge_50 = .9))
  run <- function(x, action) attr(SEMANTICA:::compute_psychometric_guard_penalty(x,
    ave_warning_action = action), "diagnostics")
  expect_false(run(f, "final_constraint")$passed)
  missing <- run(f, "final_constraint")$criteria
  expect_identical(missing$state[missing$criterion == "ave_warnings"], "unavailable")
  f$ave_warnings <- character()
  clear <- run(f, "final_constraint")
  expect_true(clear$passed)
  f$ave_warnings <- "improper standardized loading"
  warned <- run(f, "final_constraint")
  expect_false(warned$passed)
  expect_identical(clear$resolved_configuration_hash, warned$resolved_configuration_hash)
  expect_equal(warned$penalty, .85)
  expect_true(run(f, "penalty")$passed)
  expect_equal(run(f, "penalty")$penalty, .85)
})
