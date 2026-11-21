reference_fixture <- function() {
  ids <- c(letters[1:6], paste0("r", 1:4))
  L <- matrix(0, 10, 2); L[1:3, 1] <- c(.8, .75, .2); L[4:6, 2] <- c(.8, .75, .2)
  L[7:8, 1] <- .8; L[9:10, 2] <- .8
  Phi <- matrix(c(1, .3, .3, 1), 2)
  S <- L %*% Phi %*% t(L); diag(S) <- 1; dimnames(S) <- list(ids, ids)
  p <- semantica_reference_evidence(list(A = ids[7:8], B = ids[9:10]), covariance = S,
    source = "software control", method = "separate reference", justification = "known control model", source_family = "synthetic")
  assignment <- setNames(rep(c("A", "B"), each = 3L), letters[1:6])
  rules <- list(item.facet.lookup = setNames(rep(c("x", "y", "y"), 2), letters[1:6]),
    facets.by.factor = list(A = c("x", "y"), B = c("x", "y")), facet_constraint_mode = "presence",
    facet_min_per_facet = 1L, facet_max_imbalance = 1L, duplicate_cluster_id = setNames(letters[1:6], letters[1:6]))
  context <- .semantica_reference_context(p, assignment, c("A", "B"), c(A = 2L, B = 2L), rules)
  list(panel = p, context = context, covariance = S,
    good = c(a = 1L, b = 1L, c = 0L, d = 1L, e = 1L, f = 0L),
    weak = c(a = 1L, b = 0L, c = 1L, d = 1L, e = 0L, f = 1L))
}

test_that("declared reference objects are immutable, ordered and explicit about evidence", {
  x <- reference_fixture(); p <- x$panel
  q <- semantica_reference_evidence(rev(p$references), covariance = p$covariance[10:1, 10:1],
    source = p$source, method = p$method, justification = p$justification, source_family = "synthetic")
  expect_identical(p$id, q$id)
  p$role <- "sensitivity"
  expect_error(.semantica_reference_context(p, x$context$assignment, c("A", "B"), c(A = 2L, B = 2L), x$context$constraints), "modified")
  q <- x$panel; q$covariance[1, 1] <- 2
  expect_error(.semantica_reference_context(q, x$context$assignment, c("A", "B"), c(A = 2L, B = 2L), x$context$constraints), "modified")
  expect_identical(semantica_quality_config(reference_evidence = x$panel)$reference_evidence, x$panel)
  expect_null(semantica_quality_config()$reference_evidence)
})

test_that("reference criteria equal direct composite covariance arithmetic", {
  x <- reference_fixture(); ids <- names(x$good)[x$good == 1L]
  r <- .semantica_reference_evaluate(x$context, ids)
  for (f in c("A", "B")) {
    a <- ids[x$context$assignment[ids] == f]; b <- x$panel$references[[f]]
    own <- sum(x$covariance[a, b])/sqrt(sum(x$covariance[a, a])*sum(x$covariance[b, b]))
    other <- x$panel$references[[setdiff(c("A", "B"), f)]]
    cross <- sum(x$covariance[a, other])/sqrt(sum(x$covariance[a, a])*sum(x$covariance[other, other]))
    expect_equal(r$panels$convergence[r$panels$factor == f], own)
    expect_equal(r$panels$discrimination[r$panels$factor == f], own - abs(cross))
  }
  expect_equal(.semantica_reference_evaluate(x$context, rev(ids)), r)
  changed <- .semantica_reference_evaluate(x$context, c("a", "b", "d", "f"))
  expect_equal(r$panels[r$panels$factor == "A", ], changed$panels[changed$panels$factor == "A", ])
  bad <- .semantica_reference_evaluate(x$context, names(x$weak)[x$weak == 1L])
  expect_true(.semantica_reference_dominates(r$criteria, bad$criteria))
  expect_false(.semantica_reference_dominates(bad$criteria, r$criteria))
  expect_null(.semantica_reference_evaluate(x$context, c("a", "b", "c", "d")))
  expect_null(.semantica_reference_evaluate(x$context, c("a", "a", "d", "e")))
})

test_that("the custom pipeline forwards reference declarations without providers", {
  x <- reference_fixture(); captured <- NULL
  local_mocked_bindings(.semantica_execute_pipeline = function(...) {
    captured <<- list(...); stop("captured reference configuration")
  }, .package = "SEMANTICA")
  expect_error(semantica_run_custom("fixture", "fixture", list(A = "a", B = "b"),
    quality = semantica_quality_config(reference_evidence = x$panel),
    checkpoint_dir = NULL, verbose = FALSE), "captured reference configuration")
  expect_identical(captured$reference_evidence, x$panel)
})

test_that("one-domain references do not invent a discriminant comparison", {
  x <- reference_fixture(); ids <- c("a", "b", "c", "r1", "r2")
  p <- semantica_reference_evidence(list(A = c("r1", "r2")), covariance = x$covariance[ids, ids],
    source = "one-domain control", method = "separate reference", justification = "control", source_family = "synthetic")
  ctx <- .semantica_reference_context(p, x$context$assignment[1:3], "A", c(A = 2L), x$context$constraints)
  r <- .semantica_reference_evaluate(ctx, c("a", "b"))
  expect_length(r$criteria, 1L)
  expect_true(is.na(r$panels$discrimination))
  expect_true(is.finite(r$panels$convergence))
})

test_that("sensitivity panels report disagreement without blocking primary precedence", {
  x <- reference_fixture(); S <- x$covariance
  p <- c(1, 3, 2, 4, 6, 5, 7:10); changed <- S[p, p]; dimnames(changed) <- dimnames(S)
  sensitivity <- semantica_reference_evidence(x$panel$references, covariance = changed,
    source = "contrary control", method = "shared nuisance", justification = "sensitivity control",
    role = "sensitivity", source_family = "synthetic")
  ctx <- .semantica_reference_context(list(x$panel, sensitivity), x$context$assignment,
    c("A", "B"), c(A = 2L, B = 2L), x$context$constraints)
  a <- .semantica_reference_evaluate(ctx, names(x$good)[x$good == 1L])
  b <- .semantica_reference_evaluate(ctx, names(x$weak)[x$weak == 1L])
  expect_equal(a$criteria, .semantica_reference_evaluate(x$context, names(x$good)[x$good == 1L])$criteria)
  expect_true(.semantica_reference_dominates(a$criteria, b$criteria))
  expect_equal(nrow(a$panels), 4L)
  primary <- semantica_reference_evidence(sensitivity$references, covariance = changed,
    source = sensitivity$source, method = sensitivity$method, justification = sensitivity$justification, source_family = "synthetic")
  ctx <- .semantica_reference_context(list(x$panel, primary), x$context$assignment,
    c("A", "B"), c(A = 2L, B = 2L), x$context$constraints)
  a <- .semantica_reference_evaluate(ctx, names(x$good)[x$good == 1L])
  b <- .semantica_reference_evaluate(ctx, names(x$weak)[x$weak == 1L])
  expect_false(.semantica_reference_dominates(a$criteria, b$criteria))
  state <- list2env(list(entries = list(), seen = new.env(), observed = 0L, truncated = FALSE, changed = FALSE))
  .semantica_reference_observe(state, ctx, list(x$weak, x$good), 10L)
  result <- .semantica_reference_result(ctx, state)
  expect_length(result$shortlist, 2L); expect_null(result$recommended_items)
  expect_identical(result$status, "unresolved_reference_tradeoff")
  limited <- list2env(list(entries = list(), seen = new.env(), observed = 0L, truncated = FALSE, changed = FALSE))
  .semantica_reference_observe(limited, ctx, list(x$weak, x$good), 1L)
  expect_true(limited$truncated); expect_null(.semantica_reference_result(ctx, limited)$recommended_items)
})

test_that("reference archive cannot be overridden by semantic fields or restored after exclusion", {
  x <- reference_fixture()
  state <- list2env(list(entries = list(), seen = new.env(), observed = 0L, truncated = FALSE, changed = FALSE))
  .semantica_reference_observe(state, x$context, list(x$weak, x$good), 10L)
  expect_length(state$entries, 1L); expect_identical(state$entries[[1]]$vec, x$good)
  state$entries[[1]]$semantic_score <- -1e6
  .semantica_reference_observe(state, x$context, list(x$weak), 10L)
  expect_identical(state$entries[[1]]$vec, x$good)
  result <- .semantica_reference_result(x$context, state)
  expect_identical(result$recommended_items, c("a", "b", "d", "e"))
  expect_match(result$uncertainty, "no_calibrated")
  state$truncated <- TRUE
  result <- .semantica_reference_result(x$context, state)
  expect_null(result$recommended_items); expect_match(result$status, "archive_limit")
  expect_false(semantica_evidence_status(list(reference_selection = result))$participant_based[14L])
  pheromone <- matrix(1, 6, 2)
  expect_equal(.semantica_reference_pheromone(pheromone, state$entries, .2),
    matrix(.8, 6, 2) + unname(cbind(1-x$good, x$good)))
})

test_that("missing, overlapping, malformed and undeclared reference inputs fail explicitly", {
  x <- reference_fixture(); p <- x$panel
  build <- function(...) semantica_reference_evidence(..., source = "control", method = "measurement", justification = "control")
  expect_error(build(p$references), "exactly one")
  expect_error(build(p$references, data = matrix(NA_real_, 3, 10)), "complete")
  expect_error(build(list(A = "a", B = "a"), covariance = x$covariance), "repeat")
  S <- x$covariance; S[1, 2] <- S[2, 1] <- 3
  expect_error(build(p$references, covariance = S), "indefinite")
  expect_error(.semantica_reference_context(p, setNames(c("A", "B"), c("a", "r1")),
    c("A", "B"), c(A = 1L, B = 1L), x$context$constraints), "disjoint")
  expect_error(.semantica_reference_context(list(), x$context$assignment,
    c("A", "B"), c(A = 2L, B = 2L), x$context$constraints), "declared")
  expect_error(run_multi_seed_semantica(seeds = 1:2, reference_evidence = p), "reference frontiers")
  expect_error(.semantica_execute_pipeline(restart_seeds = 1:2, reference_evidence = p), "one optimizer seed")
})
