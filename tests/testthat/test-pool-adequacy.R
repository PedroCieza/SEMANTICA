test_that("the response audit distinguishes available, infeasible and capped forms", {
  ids <- paste0("i", 1:6)
  assignment <- setNames(rep(c("A", "B"), each = 3L), ids)
  m <- matrix(.05, 6L, 6L, dimnames = list(ids, ids))
  m[outer(assignment, assignment, "==")] <- .70
  diag(m) <- 1
  counts <- c(A = 2L, B = 2L)
  audit <- semantica_pool_adequacy(m[, rev(ids)], assignment, counts, 9L,
    feasible = function(items) "i1" %in% items)
  expect_identical(audit$status, "supported_forms_available")
  expect_equal(audit$evaluated_forms, 9)
  expect_equal(audit$supported_before_constraints, 9)
  expect_equal(audit$supported_forms, 6)
  expect_equal(audit$factors$max_alpha_before_constraints, rep(2 * .70 / 1.70, 2))
  expect_true(all(lengths(audit$forms$items) == 4L))
  infeasible <- semantica_pool_adequacy(m, assignment, counts, 9L, feasible = function(items) FALSE)
  expect_identical(infeasible$status, "no_feasible_forms")
  expect_equal(infeasible$supported_before_constraints, 9)
  capped <- semantica_pool_adequacy(m, assignment, counts, 8L, feasible = function(items) stop("visited"))
  expect_identical(capped$status, "budget_exceeded")
  expect_equal(capped$evaluated_forms, 0)
  expect_true(is.na(capped$supported_forms))
  none <- semantica_pool_adequacy(m, assignment, counts, 1L, eligible_items = ids[-(1:2)])
  expect_identical(none$status, "no_feasible_forms")
  expect_error(semantica_pool_adequacy(m, assignment, counts, 9L, feasible = function(items) NA),
    "single TRUE or FALSE")
})

test_that("weak alpha and unavailable response evidence receive different statuses", {
  ids <- paste0("i", 1:4)
  assignment <- setNames(rep(c("A", "B"), each = 2L), ids)
  x <- diag(4)
  dimnames(x) <- list(ids, ids)
  x[3L, 4L] <- x[4L, 3L] <- .80
  x[1L, 2L] <- x[2L, 1L] <- .30
  counts <- c(A = 2L, B = 2L)
  weak <- semantica_pool_adequacy(x, assignment, counts, 1L)
  expect_identical(weak$status, "no_supported_forms")
  expect_equal(weak$forms$min_alpha, .6 / 1.3)
  x[1L, 2L] <- x[2L, 1L] <- -1
  undefined <- semantica_pool_adequacy(x, assignment, counts, 1L)
  expect_identical(undefined$status, "response_evidence_unavailable")
  expect_equal(undefined$unresolved_feasible_forms, 1)
})

test_that("fixed-form adequacy separates weak response evidence from PFA and predictions", {
  ids <- paste0("i", 1:8)
  assignment <- setNames(rep(c("A", "B"), each = 4L), ids)
  S <- matrix(.03, 8L, 8L, dimnames = list(ids, ids))
  S[outer(assignment, assignment, "==")] <- .50
  diag(S) <- 1
  observed <- semantica_assess_form_adequacy(S[, rev(ids)], assignment, "advance")
  expect_identical(observed$joint_disposition, "advance")
  expect_equal(unname(observed$alpha), rep(.8, 2L))
  expect_identical(semantica_assess_form_adequacy(NULL, assignment, "advance")$joint_disposition,
    "unavailable")
  prediction <- S
  attr(prediction, "semantica_calibration") <- list(method = "fisher_linear")
  p <- semantica_assess_form_adequacy(prediction, assignment, "advance")
  expect_identical(p$evidence_source, "calibrated_prediction")
  expect_identical(p$response_status, "supported")
  expect_identical(p$joint_disposition, "unavailable")
  S[1:4, 1:4] <- .23
  diag(S) <- 1
  weak <- semantica_assess_form_adequacy(S, assignment, "advance")
  expect_identical(weak$response_status, "unsupported")
  expect_identical(weak$joint_disposition, "defer")
  expect_error(semantica_assess_form_adequacy(S[-1L, -1L], assignment), "cover every")
})
