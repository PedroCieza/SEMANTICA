test_that("automatic within-factor targets use the current nonredundant pool geometry", {
  ids <- paste0("i", 1:6)
  m <- diag(6)
  dimnames(m) <- list(ids, ids)

  f1 <- c(.72, .75, .78)
  f2 <- c(.68, .70, .74)
  m[1,2] <- m[2,1] <- f1[1]
  m[1,3] <- m[3,1] <- f1[2]
  m[2,3] <- m[3,2] <- f1[3]
  m[4,5] <- m[5,4] <- f2[1]
  m[4,6] <- m[6,4] <- f2[2]
  m[5,6] <- m[6,5] <- f2[3]
  m[1:3,4:6] <- .20
  m[4:6,1:3] <- .20

  targets <- SEMANTICA:::estimate_within_similarity_targets(
    list(F1 = ids[1:3], F2 = ids[4:6]),
    m,
    c("F1", "F2"),
    redundancy_threshold = .85,
    within_similarity_band = .08,
    method = "nonredundant_median"
  )

  expect_equal(unname(targets["F1"]), median(f1))
  expect_equal(unname(targets["F2"]), median(f2))
  expect_true(all(targets > .55))
  expect_identical(attr(targets, "method"), "nonredundant_median")
})

test_that("legacy within-target mode remains available only for compatibility", {
  ids <- paste0("i", 1:4)
  m <- matrix(.75, 4, 4, dimnames = list(ids, ids))
  diag(m) <- 1
  targets <- SEMANTICA:::estimate_within_similarity_targets(
    list(F1 = ids[1:2], F2 = ids[3:4]),
    m,
    c("F1", "F2"),
    method = "legacy_q40"
  )
  expect_true(all(targets <= .55))
  expect_identical(attr(targets, "method"), "legacy_q40")
})

test_that("quality and validation-planning defaults favor diagnostics over automatic exclusion", {
  q <- semantica_quality_config()
  d <- semantica_diagnostics_config()
  expect_identical(q$content_alignment_mode, "diagnostic")
  expect_identical(q$polarity_action, "diagnostic")
  expect_identical(q$within_target_method, "nonredundant_median")
  expect_identical(d$validation_planning_on_inadmissible, "skip")

  aco_formals <- formals(ACO_with_ESEM)
  expect_identical(
    eval(aco_formals$content_alignment_mode),
    c("diagnostic", "guard", "off")
  )
  expect_identical(
    eval(aco_formals$within_target_method),
    c("nonredundant_median", "legacy_q40")
  )
})

test_that("ambiguous facet alignment retains coverage while clear mismatch does not", {
  factors <- list(
    A = list(
      description = "Factor A",
      facets = list(
        x = list(description = "facet x"),
        y = list(description = "facet y")
      )
    )
  )
  bp <- semantica_construct_blueprint(factors)
  d <- data.frame(
    item_id = c("i1", "i2"),
    factor = c("A", "A"),
    Facet = c("x", "y"),
    semantica_facet_clear_mismatch = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  z <- semantica_assess_construct_coverage(d, bp)
  expect_equal(z$metadata_overall_coverage, 1)
  expect_equal(z$semantic_overall_coverage, .5)
})

test_that("representation sensitivity reports rank-relative top-pair agreement", {
  e <- rbind(
    c(1, 0, 0),
    c(.9, .1, 0),
    c(.7, .3, 0),
    c(0, 1, 0),
    c(0, .9, .1),
    c(0, .7, .3)
  )
  rownames(e) <- paste0("i", seq_len(nrow(e)))
  z <- SEMANTICA:::.compute_cosine_adjustment_sensitivity(e)
  expect_true(z$available)
  expect_true(is.finite(z$top_pair_jaccard))
  expect_gte(z$top_pair_jaccard, 0)
  expect_lte(z$top_pair_jaccard, 1)
  expect_gt(z$top_pair_n, 0)
  expect_gt(z$top_pair_fraction, 0)
  expect_true(is.finite(z$top_pair_jaccard_random_baseline))
  expect_equal(
    z$top_pair_jaccard_random_baseline,
    z$top_pair_fraction_effective / (2 - z$top_pair_fraction_effective)
  )
})

test_that("adaptive pool calibration no longer imposes a universal .70 cohesion cap", {
  ids <- paste0("i", 1:8)
  m <- matrix(.72, 8, 8, dimnames = list(ids, ids))
  diag(m) <- 1
  # Strong but non-duplicate within-factor relations. The experimental
  # calibration should remain on this pool's scale instead of forcing <= .70.
  m[1:4, 1:4] <- .82
  m[5:8, 5:8] <- .80
  diag(m) <- 1
  fa <- stats::setNames(rep(c("F1", "F2"), each = 4), ids)
  z <- semantica_calibrate_similarity_thresholds(
    m, fa, redundancy_quantile = .95, duplicate_quantile = .99,
    min_redundancy = .70, max_redundancy = .95
  )
  expect_true(any(z$within_similarity_target > .70))
  expect_identical(z$within_similarity_target_method, "nonredundant_median")
})

test_that("semantic fallback keeps trying later ESEM checkpoints and final archive refits", {
  search_fit_calls <- 0L
  archive_fit_calls <- 0L

  local_mocked_bindings(
    run_esem_on_matrix = function(..., iter_max = NULL, fallback = TRUE,
                                  return_diagnostics = FALSE) {
      stage <- if (isTRUE(fallback)) "archive" else "search"
      if (identical(stage, "archive")) {
        archive_fit_calls <<- archive_fit_calls + 1L
      } else {
        search_fit_calls <<- search_fit_calls + 1L
      }
      if (isTRUE(return_diagnostics)) {
        return(list(
          fit = list(stage = stage),
          accepted_attempt = 1L,
          solver_attempts_started = 1L
        ))
      }
      list(stage = stage)
    },
    extract_and_score_esem = function(esem_fit, ...) {
      list(
        converged = TRUE,
        admissible = FALSE,
        admissibility = list(
          admissible = FALSE,
          reasons = paste0("mock_", esem_fit$stage, "_inadmissible")
        ),
        score = 0,
        loading_quality = 0,
        ave = NA_real_,
        htmt_max = NA_real_,
        structure_diagnostics = NULL
      )
    },
    .package = "SEMANTICA"
  )

  ids <- paste0("item_", seq_len(10L))
  intended <- rep(c("F1", "F2"), each = 5L)
  lambda <- matrix(0.08, nrow = 10L, ncol = 2L)
  lambda[cbind(seq_len(10L), rep(seq_len(2L), each = 5L))] <- 0.68
  phi <- matrix(c(1, .25, .25, 1), 2L)
  similarity <- lambda %*% phi %*% t(lambda)
  diag(similarity) <- 1
  dimnames(similarity) <- list(ids, ids)

  result <- ACO_with_ESEM(
    cosine_sim_matrix = similarity,
    df = data.frame(item = ids, factor = intended),
    i.per.f = c(F1 = 4L, F2 = 4L),
    ants = 3L,
    max.iter = 10L,
    max_total_iter = 3L,
    esem_every = 1L,
    esem_eval_top_k = 1L,
    run_esem_during_search = TRUE,
    max_esem_fits = 10L,
    esem_failure_policy = "semantic_fallback",
    dfi_mode = "heuristic_semantic",
    pfa_mode = "off",
    run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE,
    validation_n_diagnostic = FALSE,
    final_dddfi = FALSE,
    final_equivtest = FALSE,
    use_parallel = FALSE,
    seed = 20260822L,
    verbose = FALSE
  )

  expect_gte(search_fit_calls, 2L)
  expect_equal(archive_fit_calls, length(result$elite_archive))
  expect_false(result$esem_admissible)
  expect_match(result$search_guidance_status, "fallback")
  expect_match(result$archive_selection_mode, "fallback")
})

test_that("alignment guard requires a pool-relative positive reference before exclusion", {
  z <- SEMANTICA:::.semantica_classify_alignment_margins(
    c(.20, .30, -.05, -.40),
    rep("F1", 4L)
  )
  expect_identical(z$status[1:2], c("aligned", "aligned"))
  expect_identical(z$status[3], "ambiguous")
  expect_identical(z$status[4], "clear_mismatch")
  expect_equal(z$scale[1], .25)

  # With too little positively separated evidence, negative ranks stay
  # diagnostic rather than becoming automatic exclusions.
  z2 <- SEMANTICA:::.semantica_classify_alignment_margins(
    c(.01, -.02, -.03),
    rep("F1", 3L)
  )
  expect_false(any(z2$clear_mismatch))
  expect_true(all(z2$status[2:3] == "ambiguous"))
})
