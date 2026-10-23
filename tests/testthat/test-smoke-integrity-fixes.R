test_that("archived colliding syntax fits the requested items without changing estimation", {
  skip_if_not_installed("lavaan")
  pair <- readRDS(test_path("fixtures", "lavaan-parser-collision.rds"))
  make_args <- function(syntax, ids) {
    lambda <- matrix(.08, length(ids), 2L)
    lambda[cbind(seq_along(ids), rep(1:2, each = length(ids) / 2))] <- .68
    K <- tcrossprod(lambda); diag(K) <- 1; dimnames(K) <- list(ids, ids)
    list(model = syntax, sample.cov = K, sample.nobs = 500L,
      sample.cov.rescale = FALSE, std.lv = TRUE, estimator = "ML",
      se = "none", rotation = "geomin", rotation.args = list(rstarts = 1L))
  }
  a <- make_args(pair$syntax_a, pair$expected_a)
  b <- make_args(pair$syntax_b, pair$expected_b)
  set.seed(616)
  baseline <- suppressWarnings(do.call(lavaan::sem, c(a, list(parser = "new"))))
  # Prime the dependency's cache with the actual distinct colliding strings.
  suppressWarnings(lavaan::lavaanify(pair$syntax_a))
  suppressWarnings(lavaan::lavaanify(pair$syntax_b))
  set.seed(616)
  fixed <- SEMANTICA:::.semantica_try_lavaan(a)
  expect_null(fixed$error)
  expect_equal(lavaan::coef(fixed$fit), lavaan::coef(baseline), tolerance = 1e-8)
  expect_identical(fixed$parser, "old")
  second <- SEMANTICA:::.semantica_try_lavaan(b)
  expect_null(second$error)
  expect_setequal(lavaan::lavNames(second$fit, "ov"), pair$expected_b)
  expect_equal(lavaan::lavInspect(second$fit, "sampstat")$cov,
    b$sample.cov, ignore_attr = TRUE, tolerance = 1e-12)
})

test_that("an unexpected fitted identity is rejected at the shared solver boundary", {
  K <- diag(3L); dimnames(K) <- list(letters[1:3], letters[1:3])
  fit <- lavaan::cfa("F =~ a + b + c", sample.cov = K, sample.nobs = 500,
    std.lv = TRUE, se = "none", parser = "old")
  local_mocked_bindings(lavNames = function(...) c("a", "b", "wrong"), .package = "lavaan")
  local_mocked_bindings(sem = function(...) fit, .package = "lavaan")
  out <- SEMANTICA:::.semantica_try_lavaan(list(sample.cov = K))
  expect_null(out$fit)
  expect_identical(out$failure_reason, "fitted_item_identity_mismatch")
})

test_that("the parser correction also survives isolated solver-worker transport", {
  skip_on_cran()
  pair <- readRDS(test_path("fixtures", "lavaan-parser-collision.rds"))
  wrapper <- SEMANTICA:::.semantica_try_lavaan
  environment(wrapper) <- baseenv()
  cl <- parallel::makePSOCKcluster(1L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  result <- parallel::clusterCall(cl, function(pair, wrapper) {
    lavaan::lavaanify(pair$syntax_a)
    lavaan::lavaanify(pair$syntax_b)
    ids <- pair$expected_b
    lambda <- matrix(.08, length(ids), 2L)
    lambda[cbind(seq_along(ids), rep(1:2, each = length(ids) / 2))] <- .68
    K <- tcrossprod(lambda); diag(K) <- 1; dimnames(K) <- list(ids, ids)
    out <- wrapper(list(model = pair$syntax_b, sample.cov = K, sample.nobs = 500L,
      sample.cov.rescale = FALSE, std.lv = TRUE, estimator = "ML", se = "none",
      rotation = "geomin", rotation.args = list(rstarts = 1L), bounds = "pos.var"))
    list(ids = lavaan::lavNames(out$fit, "ov"), parser = out$parser, error = out$error)
  }, pair, wrapper)[[1L]]
  expect_setequal(result$ids, pair$expected_b)
  expect_identical(result$parser, "old")
  expect_null(result$error)
})

test_that("supported local seed schedules reach request bodies without claiming exact replay", {
  bodies <- list()
  local_mocked_bindings(.call_chat = function(session, ...) {
    bodies[[length(bodies) + 1L]] <<- SEMANTICA:::.semantica_openai_chat_body(session, ...)
    "1. I explore unfamiliar scientific questions with curiosity."
  }, .package = "SEMANTICA")
  run <- function(backend) semantica_generate_items(
    structure(list(protocol = "openai_compat", backend = backend, chat_model = "offline",
      supports_structured_output = FALSE), class = "semantica_session"),
    "Offline", "Offline", list(A = list(description = "Curiosity")),
    n_per_factor = 1L, overgenerate = 1, max_retries = 1L, seed = 160L, verbose = FALSE)
  for (backend in c("unsloth", "llamacpp")) {
    a <- attr(run(backend), "semantica_generation_metadata")
    b <- attr(run(backend), "semantica_generation_metadata")
    expect_identical(tail(bodies, 1L)[[1L]]$seed, a$task_seed_ledger$generation_task_seed[[1L]])
    expect_identical(a$generation_seed_schedule, b$generation_seed_schedule)
    expect_true(a$generation_seed_controlled)
    expect_false(a$exact_text_replay_guaranteed)
    expect_identical(a$generation_seed_mechanism, "llamacpp_request_seed")
  }
  other <- attr(run("custom"), "semantica_generation_metadata")
  expect_false(other$generation_seed_controlled)
  expect_null(tail(bodies, 1L)[[1L]]$seed)
})

.validator_session <- function() structure(list(protocol = "openai_compat", backend = "custom",
  chat_model = "offline", supports_structured_output = FALSE), class = "semantica_session")

test_that("candidate screening records unchanged rejected text and replenishes within the budget", {
  bad <- "I need a wording review before this statement is used."
  good <- "I explore unfamiliar scientific questions with curiosity."
  extra <- "I carefully organize my workspace before starting each task."
  calls <- 0L; contexts <- list()
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) paste0("1. ", bad, "\n2. ", good) else paste0("1. ", extra)
  }, .package = "SEMANTICA")
  screen <- function(items, context) {
    contexts[[length(contexts) + 1L]] <<- context
    ifelse(items == bad, "failed declared wording assessment", NA_character_)
  }
  out <- semantica_generate_items(.validator_session(), "Offline", "Offline",
    list(A = list(description = "Construct definition")), n_per_factor = 2L,
    overgenerate = 1, max_retries = 2L, verbose = FALSE,
    retain_responses = TRUE, candidate_validator = screen)
  m <- attr(out, "semantica_generation_metadata")
  expect_equal(calls, 2L)
  expect_setequal(out$item_text, c(good, extra))
  expect_identical(m$candidate_validation_status, "callback_screened")
  expect_equal(m$candidate_validation_rejected, 1L)
  expect_identical(m$attempts[[1L]]$candidate_validation$item_text, c(bad, good))
  expect_identical(m$attempts[[1L]]$candidate_validation$accepted, c(FALSE, TRUE))
  expect_identical(m$attempts[[1L]]$duplicate_existing, 0L)
  expect_match(m$response_archive[[1L]]$raw_response, bad, fixed = TRUE)
  expect_identical(contexts[[1L]]$dimension_description, "Construct definition")
})

test_that("invalid validators stop rather than bypass screening; no parsed items remains a shortage", {
  calls <- 0L
  local_mocked_bindings(.call_chat = function(...) {
    calls <<- calls + 1L
    "1. I explore unfamiliar scientific questions with curiosity."
  }, .package = "SEMANTICA")
  run <- function(validator = NULL) semantica_generate_items(.validator_session(), "Offline", "Offline",
    list(A = list(description = "Curiosity")), n_per_factor = 1L, overgenerate = 1,
    max_retries = 2L, verbose = FALSE, retain_responses = TRUE, candidate_validator = validator)
  for (validator in list(function(items, context) TRUE,
    function(items, context) character(), function(items, context) matrix(NA_character_, 1L),
    function(items, context) stop("assessment unavailable"))) {
    before <- calls
    error <- tryCatch(run(validator), error = identity)
    expect_s3_class(error, "semantica_error_candidate_validation")
    expect_equal(calls - before, 1L)
    expect_length(error$generation_failure$candidate_items, 1L)
    expect_length(error$generation_failure$response_archive, 1L)
  }
  error <- tryCatch(run(function(items, context) rep("not supported", length(items))), error = identity)
  expect_s3_class(error, "semantica_error_generation_incomplete")
  expect_length(error$generation_failure$attempts, 2L)
  expect_true(all(vapply(error$generation_failure$attempts,
    function(a) a$candidate_validation_rejected == 1L, logical(1L))))
  ordinary <- attr(run(), "semantica_generation_metadata")
  expect_identical(ordinary$candidate_validation_status, "not_assessed")
  local_mocked_bindings(.call_chat = function(...) "", .package = "SEMANTICA")
  expect_error(run(), class = "semantica_error_generation_incomplete")
})

test_that("the real facade forwards the declared candidate validator to native generation", {
  seen <- NULL
  screen <- function(items, context) rep(NA_character_, length(items))
  local_mocked_bindings(semantica_connect = function(...) .validator_session(),
    semantica_generate_items = function(...) {
      seen <<- list(...)
      stop("offline native boundary")
    }, .package = "SEMANTICA")
  expect_error(semantica_run_custom("Offline", "Offline", list(A = list(description = "Curiosity")),
    llm = semantica_llm_config(backend = "ollama", embed_backend = "ollama", preflight = FALSE),
    chat_model = "offline", embed_model = "offline",
    generation = semantica_generation_config(candidate_validator = screen, pool_topup = FALSE),
    checkpoint_dir = tempfile("validator-pipeline-"), plots = "none", verbose = FALSE), "offline native boundary")
  expect_identical(seen$candidate_validator, screen)
})

test_that("global fit references remain visible separately from numerical admissibility", {
  score <- list(converged = TRUE, admissible = TRUE, htmt_violations = 0L,
    structure_diagnostics = list(correct_dominance = 1, no_large_cross_loading = 1),
    fit_reference_diagnostics = data.frame(metric = c("cfi", "tli", "rmsea", "srmr"),
      met = c(FALSE, FALSE, FALSE, TRUE)))
  state <- semantica_esem_state(esem_result = score)
  expect_identical(state$technical_state, "admissible")
  expect_identical(state$fit_reference_status, "not_met")
  expect_true(state$quality_flags[["global_fit_reference_not_met"]])
  expect_match(state$quality_message, "guard acceptance does not imply", fixed = TRUE)
  expect_identical(state$fit_reference_diagnostics, score$fit_reference_diagnostics)
})
