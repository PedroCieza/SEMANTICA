test_that("representation ensembles are explicitly robustness evidence, not participant validation", {
  ids <- paste0("i", 1:4)
  m1 <- diag(1, 4); dimnames(m1) <- list(ids, ids); m1[m1 == 0] <- .2; diag(m1) <- 1
  m2 <- diag(1, 4); dimnames(m2) <- list(ids, ids); m2[m2 == 0] <- .3; diag(m2) <- 1
  ens <- semantica_ensemble_similarity(list(a = m1, b = m2))
  meta <- attr(ens, "semantica_ensemble")
  expect_identical(meta$source_family, "embedding_semantic_ensemble")
  expect_false(meta$participant_based)
})

test_that("empirical calibration is marked as sample-dependent training evidence", {
  ids <- paste0("i", 1:4)
  sm <- matrix(c(
    1,.6,.2,.1,
    .6,1,.25,.15,
    .2,.25,1,.55,
    .1,.15,.55,1
  ), 4, 4, byrow = TRUE, dimnames = list(ids, ids))
  rm <- matrix(c(
    1,.5,.15,.10,
    .5,1,.20,.12,
    .15,.20,1,.45,
    .10,.12,.45,1
  ), 4, 4, byrow = TRUE, dimnames = list(ids, ids))
  cal <- semantica_fit_empirical_calibration(sm, response_matrix = rm, method = "linear")
  expect_identical(cal$validation_status, "training_fit_only_requires_independent_validation")
  expect_true(cal$participant_based)
  expect_identical(cal$source_families, c("embedding_semantic", "response_data"))
})

test_that("leave-one-scale-out calibration preserves whole-instrument holdout provenance", {
  mk <- function(prefix, shift = 0) {
    ids <- paste0(prefix, 1:4)
    sm <- matrix(.2 + shift, 4, 4, dimnames = list(ids, ids)); diag(sm) <- 1
    sm[1,2] <- sm[2,1] <- .7 + shift/2
    sm[3,4] <- sm[4,3] <- .65 + shift/2
    rm <- matrix(.1, 4, 4, dimnames = list(ids, ids)); diag(rm) <- 1
    rm[1,2] <- rm[2,1] <- .55
    rm[3,4] <- rm[4,3] <- .50
    list(semantic_matrix = sm, response_matrix = rm)
  }
  dat <- list(scale_A = mk("a"), scale_B = mk("b", .02), scale_C = mk("c", -.02))
  out <- semantica_leave_one_scale_out_calibration(dat, method = "linear")
  expect_identical(out$leakage_control, "entire_scale_holdout_no_within_instrument_pair_split")
  expect_true(out$participant_based)
  expect_equal(out$per_scale$n_train_scales, rep(2L, 3))
  expect_true(all(out$per_scale$held_out_scale == out$per_scale$scale))
  leaked <- vapply(seq_len(nrow(out$per_scale)), function(i) {
    grepl(out$per_scale$held_out_scale[[i]], out$per_scale$training_scales[[i]], fixed = TRUE)
  }, logical(1L))
  expect_false(any(leaked))
})

test_that("leave-one-scale-out calibration rejects ambiguous duplicate scale names", {
  sm <- diag(3); dimnames(sm) <- list(letters[1:3], letters[1:3])
  rm <- diag(3); dimnames(rm) <- list(letters[1:3], letters[1:3])
  dummy <- list(semantic_matrix = sm, response_matrix = rm)
  x <- list(dummy, dummy); names(x) <- c("same", "same")
  expect_error(semantica_leave_one_scale_out_calibration(x), "unique, non-empty scale names")
})
