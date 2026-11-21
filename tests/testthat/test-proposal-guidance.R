test_that("signed guidance extends scalar domain scores without hiding negative evidence", {
  relative <- list(factor_table = data.frame(factor = c("a", "b", "c"),
    stochastic_superiority = c(.8, .4, .6), standardized_gap = c(.5, -.7, NA_real_)))
  q <- .semantica_signed_guidance(relative)
  expect_equal(q["a"], c(a = 1/3))
  expect_equal(q["b"], c(b = -.7/1.7))
  expect_true(is.na(q["c"]))
  expect_false(.semantica_guidance_available(q))
  expect_length(.semantica_signed_guidance(NULL), 0L)
  expected <- pmin(pmax(0, 2*relative$factor_table$stochastic_superiority-1),
    pmax(0, relative$factor_table$standardized_gap)/(1+pmax(0, relative$factor_table$standardized_gap)))
  expect_equal(unname(pmax(0, q)), expected)
})

test_that("proposal ties use worst-first information while unequal scalars retain priority", {
  row <- function(score, q) list(score = score, proposal_guidance = setNames(q, letters[seq_along(q)]))
  a <- row(0, c(-.1, .2)); b <- row(0, c(-.4, .8))
  expect_true(.semantica_proposal_better(a, b))
  expect_false(.semantica_proposal_better(b, a))
  expect_false(.semantica_proposal_better(a, a))
  expect_false(.semantica_proposal_better(a, row(.001, c(-.9, -.8))))
  expect_true(.semantica_proposal_better(row(.001, c(-.9, -.8)), a))
  expect_false(.semantica_proposal_better(row(0, c(NA, .9)), b))
  permuted <- a; permuted$proposal_guidance <- rev(permuted$proposal_guidance)
  expect_true(.semantica_proposal_better(permuted, b))
  expect_false(.semantica_proposal_better(a, permuted))
  expect_false(.semantica_proposal_better(row(0, c(.9)), b))
})

test_that("proposal summaries limit their claims to observed comparisons", {
  x <- .semantica_proposal_summary(c(0, 0, -Inf), list(c(a = -.1), c(a = -.5), NULL))
  expect_equal(x$zero, 2L); expect_equal(x$best_ties, 2L)
  expect_identical(x$status, "no_scalar_discrimination_among_evaluated_proposals")
  expect_match(x$scope, "not_full_pool")
  expect_identical(.semantica_proposal_summary(0, list(NULL))$status, "insufficient_comparisons")
  expect_identical(.semantica_proposal_summary(-Inf, list(NULL))$status, "no_finite_proposals")
})

test_that("bounded rescue preserves scalar improvement and a better native final baseline", {
  v <- setNames(c(1L, 0L, 0L, 1L), letters[1:4])
  screen <- function(x) {
    i <- which(x[1:3] == 1L)
    list(score = c(0, 0, .1)[i], proposal_guidance = c(A = c(-.4, -.1, .1)[i], B = .2))
  }
  calls <- 0L
  final <- function(x, allow_fit) {
    calls <<- calls + 1L; expect_false(allow_fit); runif(1)
    list(score = .5)
  }
  withr::local_preserve_seed(); set.seed(91); before <- .Random.seed
  x <- .semantica_run_proposal_rescue(v, list(score = .8), list(A = letters[1:3], B = "d"),
    list(), screen, final, function(x) TRUE, 10L, 60)
  expect_identical(.Random.seed, before)
  expect_equal(x$diagnostics$proposal_after, .1)
  expect_equal(x$diagnostics$guidance_after[["A"]], .1)
  expect_equal(calls, 1L); expect_false(x$diagnostics$selected)
  expect_equal(x$diagnostics$esem_fits, 0L)
  expect_identical(x$diagnostics$certificate_scope, "none")
  y <- .semantica_run_proposal_rescue(v, list(score = 0), list(A = letters[1:3], B = "d"),
    list(), screen, function(x, allow_fit) list(score = screen(x)$score), function(x) TRUE, 10L, 60)
  expect_true(y$diagnostics$selected)
  expect_lte(y$diagnostics$evaluated, 10L)
})

test_that("signed ties cannot divert the scalar-first budget", {
  v <- c(a = 1L, b = 0L, c = 0L, d = 1L, e = 0L)
  screen <- function(x) {
    key <- paste(names(x)[x == 1L], collapse = "")
    score <- c(ad = .4, bd = .4, cd = .5, ae = .8, be = .3, ce = .3)[[key]]
    list(score = score, proposal_guidance = c(A = if (key == "bd") .2 else -.4, B = .3))
  }
  x <- .semantica_run_proposal_rescue(v, list(score = .4), list(A = c("a", "b", "c"), B = c("d", "e")),
    list(), screen, function(v, allow_fit) list(score = screen(v)$score), function(v) TRUE, 3L, 60)
  expect_equal(x$diagnostics$proposal_after, .8)
  expect_equal(names(x$vector)[x$vector == 1L], c("a", "e"))
  expect_false(x$diagnostics$history$accepted[1L])
  expect_true(x$diagnostics$scalar_only_score_preserved)
  expect_false(x$diagnostics$secondary$enabled)
  expect_identical(x$diagnostics$secondary$reason, "primary_neighborhood_unfinished")
  expect_equal(x$diagnostics$evaluated, 3L)
  expect_true(x$diagnostics$selected)
})

test_that("a verified scalar stall may use remaining budget to cross a genuine plateau", {
  v <- c(a = 1L, b = 0L, c = 1L, d = 0L)
  screens <- finals <- 0L
  screen <- function(x) {
    screens <<- screens + 1L
    list(score = if (x["b"] == 1L && x["d"] == 1L) .3 else 0,
      proposal_guidance = c(A = if (x["b"] == 1L) .2 else -.4, B = if (x["d"] == 1L) .2 else -.4))
  }
  x <- .semantica_run_proposal_rescue(v, list(score = 0), list(A = c("a", "b"), B = c("c", "d")),
    list(), screen, function(v, allow_fit) {
      finals <<- finals + 1L; expect_false(allow_fit); list(score = .3)
    }, function(v) TRUE, 12L, 60)
  expect_true(x$diagnostics$primary$neighborhood_complete)
  expect_equal(x$diagnostics$scalar_only_score, 0)
  expect_true(x$diagnostics$secondary$enabled)
  expect_equal(x$diagnostics$proposal_after, .3)
  expect_true(x$diagnostics$scalar_only_score_preserved)
  expect_gt(x$diagnostics$screen_cache_hits, 0L)
  expect_equal(screens, x$diagnostics$screen_computations + 1L)
  expect_lte(x$diagnostics$evaluated, 12L)
  expect_equal(nrow(x$diagnostics$history), x$diagnostics$evaluated)
  expect_equal(finals, 1L)
  expect_true(x$diagnostics$selected)
  expect_identical(x$diagnostics$schema, "proposal-rescue-v2")
})

test_that("completed scalar neighborhoods cannot borrow more than their shared budget", {
  v <- c(a = 1L, b = 0L, c = 1L, d = 0L)
  x <- .semantica_run_proposal_rescue(v, list(score = .4), list(A = c("a", "b"), B = c("c", "d")),
    list(), function(v) list(score = 0, proposal_guidance = c(A = v["b"], B = v["d"])),
    function(...) stop("No final evaluation should be requested"), function(v) TRUE, 2L, 60)
  expect_true(x$diagnostics$primary$neighborhood_complete)
  expect_false(x$diagnostics$secondary$enabled)
  expect_identical(x$diagnostics$secondary$reason, "no_remaining_budget")
  expect_equal(x$diagnostics$evaluated, 2L)
  expect_identical(x$diagnostics$final_status, "not_attempted")
})

test_that("scalar-first rescue works without inventing unavailable signed guidance", {
  v <- c(a = 1L, b = 0L)
  x <- .semantica_run_proposal_rescue(v, list(score = .1), list(A = c("a", "b")), list(),
    function(v) list(score = if (v["b"] == 1L) .2 else .1),
    function(v, allow_fit) list(score = .2), function(v) TRUE, 5L, 60)
  expect_equal(x$diagnostics$proposal_after, .2)
  expect_false(x$diagnostics$secondary$enabled)
  expect_identical(x$diagnostics$secondary$reason, "signed_guidance_unavailable")
  expect_true(x$diagnostics$selected)
  expect_null(x$diagnostics$guidance_after)
})

test_that("a scalar-phase improvement is evaluated even when the signed phase accepts nothing", {
  v <- c(a = 1L, b = 0L); calls <- 0L
  x <- .semantica_run_proposal_rescue(v, list(score = .1), list(A = c("a", "b")), list(),
    function(v) list(score = if (v["b"] == 1L) .2 else .1, proposal_guidance = c(A = .5)),
    function(v, allow_fit) { calls <<- calls + 1L; list(score = .2) }, function(v) TRUE, 5L, 60)
  expect_equal(x$diagnostics$primary$accepted, 1L)
  expect_equal(x$diagnostics$secondary$accepted, 0L)
  expect_true(x$diagnostics$selected)
  expect_equal(calls, 1L)
})

test_that("unavailable primary comparisons cannot be called a verified scalar stall", {
  v <- c(a = 1L, b = 0L)
  x <- .semantica_run_proposal_rescue(v, list(score = .1), list(A = c("a", "b")), list(),
    function(v) list(score = if (v["b"] == 1L) NA_real_ else .1, proposal_guidance = c(A = .5)),
    function(...) stop("No final evaluation should be requested"), function(v) TRUE, 5L, 60)
  expect_equal(x$diagnostics$primary$unavailable, 1L)
  expect_false(x$diagnostics$secondary$enabled)
  expect_identical(x$diagnostics$secondary$reason, "primary_evaluations_unavailable")
})

test_that("scalar-first rescue preserves its bounded scalar reference across tied landscapes", {
  withr::local_preserve_seed(); set.seed(841L)
  v <- c(a = 1L, b = 0L, c = 1L, d = 0L, e = 1L, f = 0L)
  items <- list(A = c("a", "b"), B = c("c", "d"), C = c("e", "f"))
  for (landscape in seq_len(6L)) {
    scores <- sample(c(0, 0, .1, .2, .3), 8L, replace = TRUE)
    guidance <- matrix(runif(24L, -.8, .8), 8L, 3L)
    screen <- function(x) {
      j <- 1L + x["b"] + 2L*x["d"] + 4L*x["f"]
      list(score = scores[j], proposal_guidance = setNames(guidance[j, ], names(items)))
    }
    for (budget in seq_len(8L)) {
      initial <- screen(v)
      reference <- .semantica_objective_polish(v, initial, items, list(), screen,
        function(v) TRUE, budget, 60, better = function(a, b) is.finite(a$score) && a$score > b$score)
      actual <- .semantica_run_proposal_rescue(v, initial, items, list(), screen,
        function(v, allow_fit) screen(v), function(v) TRUE, budget, 60)
      expect_equal(actual$diagnostics$scalar_only_score, reference$evaluation$score)
      expect_gte(actual$diagnostics$proposal_after, reference$evaluation$score)
      expect_lte(actual$diagnostics$evaluated, budget)
      expect_lte(actual$diagnostics$screen_computations, actual$diagnostics$evaluated)
    }
  }
})

test_that("rescue never invents a final score when its fit budget is unavailable", {
  v <- c(a = 1L, b = 0L)
  screen <- function(x) list(score = 0, proposal_guidance = c(A = if (x["a"] == 1L) -.5 else -.1))
  x <- .semantica_run_proposal_rescue(v, list(score = .7), list(A = c("a", "b")), list(),
    screen, function(v, allow_fit) {
      expect_false(allow_fit); list(score = NA_real_, budget_exhausted = TRUE)
    }, function(v) TRUE, 2L, 60)
  expect_identical(x$diagnostics$final_status, "fit_budget_unavailable")
  expect_false(x$diagnostics$selected)
  expect_true(is.na(x$diagnostics$candidate_final_score))
})

test_that("rescue options are explicit, bounded and validated in the public quality configuration", {
  x <- semantica_quality_config()
  expect_false(x$proposal_rescue); expect_false(x$proposal_rescue_allow_fit)
  x <- semantica_quality_config(proposal_rescue = TRUE, proposal_rescue_max_evals = 7L)
  expect_true(x$proposal_rescue); expect_equal(x$proposal_rescue_max_evals, 7L)
  expect_error(semantica_quality_config(proposal_rescue_max_evals = Inf))
  expect_error(semantica_quality_config(proposal_rescue_max_seconds = 0))
  expect_error(semantica_quality_config(proposal_rescue_allow_fit = NA))
})

test_that("native rescue preserves construction, scalar quality, RNG and its zero-fit default", {
  ids <- letters[1:12]
  m <- matrix(.15, 12, 12, dimnames = list(ids, ids))
  m[1:6, 1:6] <- .55; m[7:12, 7:12] <- .5; diag(m) <- 1
  args <- list(cosine_sim_matrix = m, df = data.frame(ID = ids, Dimension = rep(c("A", "B"), each = 6)),
    i.per.f = c(A = 4L, B = 4L), ants = 2L, max.iter = 1L, max_total_iter = 1L,
    run_esem_during_search = FALSE, esem_weight = 0, pfa_mode = "off", dfi_warmup_iters = 0L,
    semantic_esem_score_mode = "continuous_balanced", psychometric_guard_action = "report_only",
    elite_multicriteria_rerank = FALSE, final_selection_mode = "scalar", final_structure_repair = FALSE,
    semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE, final_dfi_recalibrate = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE, use_parallel = FALSE, verbose = FALSE, seed = 606L)
  reporting_calls <- 0L
  local_mocked_bindings(run_esem_on_matrix = function(...) {
    expect_identical(list(...)$progress$stage, "final")
    reporting_calls <<- reporting_calls + 1L
    list(fit = NULL)
  }, compute_pfa_diagnostics = function(...) stop("No PFA authorized"),
    .package = "SEMANTICA")
  withr::local_preserve_seed(); set.seed(902L)
  control <- do.call(ACO_with_ESEM, args)
  after_control <- .Random.seed; set.seed(902L)
  result <- do.call(ACO_with_ESEM, c(args, list(proposal_rescue = TRUE, proposal_rescue_max_evals = 3L)))
  expect_identical(.Random.seed, after_control)
  expect_identical(result$pheromone, control$pheromone)
  expect_gte(result$objective_context$value, control$objective_context$value)
  expect_lte(result$proposal_rescue$evaluated, 3L)
  expect_equal(result$proposal_rescue$esem_fits, 0L)
  expect_true(result$proposal_rescue$enabled)
  expect_equal(reporting_calls, 2L)
  expect_identical(result$proposal_diagnostics$scope,
    "evaluated_proposals_only_not_full_pool_or_participant_validation")
})

test_that("an explicit rescue fit authorization is passed once and remains separately counted", {
  v <- c(a = 1L, b = 0L)
  calls <- 0L
  x <- .semantica_run_proposal_rescue(v, list(score = .2), list(A = c("a", "b")), list(),
    function(v) list(score = if (v["a"] == 1L) 0 else .1, proposal_guidance = c(A = -.1)),
    function(v, allow_fit) {
      expect_true(allow_fit); calls <<- calls + 1L
      list(score = .3, archive_esem_fit_started = TRUE)
    }, function(v) TRUE, 2L, 60, allow_fit = TRUE)
  expect_equal(calls, 1L); expect_equal(x$diagnostics$esem_fits, 1L)
  expect_true(x$diagnostics$selected)
})

test_that("public prepared pipelines forward the rescue policy and content requirement", {
  f <- readRDS(test_path("fixtures", "benchmark-prepared-C01.rds"))
  captured <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    captured <<- list(...); stop("policy captured")
  }, .package = "SEMANTICA")
  args <- f$arguments
  args$quality <- semantica_quality_config(proposal_rescue = TRUE,
    proposal_rescue_max_evals = 9L, proposal_rescue_max_seconds = 12, proposal_rescue_allow_fit = TRUE)
  args$content_requirement <- "declared_supported"
  args$content_review <- function(items) semantica_content_review(items)
  args$checkpoint_dir <- NULL; args$verbose <- FALSE
  expect_error(do.call(semantica_run_custom, args), "policy captured")
  expect_true(captured$proposal_rescue); expect_true(captured$proposal_rescue_allow_fit)
  expect_equal(captured$proposal_rescue_max_evals, 9L)
  expect_equal(captured$proposal_rescue_max_seconds, 12)
  expect_identical(captured$content_requirement, "declared_supported")
})
