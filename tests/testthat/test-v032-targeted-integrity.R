test_that("0.3.2 diagnostics default to the requested .05-.06 RMSEA reference contrast", {
  cfg <- semantica_diagnostics_config()
  expect_equal(cfg$reference_rmsea_close, 0.05)
  expect_equal(cfg$reference_rmsea_poor, 0.06)

  expect_equal(eval(formals(SEMANTICA:::rmsea_power)$rmsea_null), 0.05)
  expect_equal(eval(formals(SEMANTICA:::rmsea_power)$rmsea_alt), 0.06)
  expect_equal(eval(formals(SEMANTICA:::required_n_for_rmsea_power)$rmsea_alt), 0.06)
  expect_equal(eval(formals(SEMANTICA:::estimate_esem_reference_sample_size)$rmsea_alt), 0.06)
})


test_that("Ollama preflight recovers from one transient registry transport failure", {
  session <- semantica_connect(
    "ollama",
    chat_model = "llama3.1:8b",
    purpose = "chat",
    preflight = FALSE,
    verbose = FALSE
  )

  calls <- 0L
  local_mocked_bindings(
    .semantica_get_json = function(session, url, timeout_s = 10L) {
      calls <<- calls + 1L
      if (calls == 1L) stop("mock transient socket reset")
      list(
        status = 200L,
        body = list(models = list(list(name = "llama3.1:8b")))
      )
    },
    .package = "SEMANTICA"
  )

  out <- semantica_backend_preflight(session, verify_models = TRUE, strict = FALSE)
  expect_true(out$reachable)
  expect_true(out$ok)
  expect_true(out$probe_recovered_after_retry)
  expect_identical(out$chat_model_available, TRUE)
  expect_length(out$warnings, 0L)
})


test_that("persistent Ollama registry failure is reported as an unconfirmed preflight with its real cause", {
  session <- semantica_connect(
    "ollama",
    chat_model = "llama3.1:8b",
    purpose = "chat",
    preflight = FALSE,
    verbose = FALSE
  )

  local_mocked_bindings(
    .semantica_get_json = function(session, url, timeout_s = 10L) {
      stop("mock connection refused")
    },
    .package = "SEMANTICA"
  )

  out <- semantica_backend_preflight(session, verify_models = TRUE, strict = FALSE)
  expect_false(out$reachable)
  expect_false(out$ok)
  expect_match(paste(out$warnings, collapse = " "), "could not confirm availability")
  expect_match(paste(out$warnings, collapse = " "), "mock connection refused")
  expect_false(grepl("server is not reachable", paste(out$warnings, collapse = " "), fixed = TRUE))
})


test_that("legacy max.iter conflict warning is emitted only when values disagree", {
  fx <- semantica_test_three_factor_fixture("separable")
  semantica_test_mock_esem_unavailable()

  base <- semantica_test_aco_args(fx, seed = 991L, history_mode = "none")
  base$max.iter <- 10L
  expect_no_warning(do.call(ACO_with_ESEM, c(base, list(search_patience = 10L))))
  expect_warning(
    do.call(ACO_with_ESEM, c(base, list(search_patience = 11L))),
    "supplied with different values"
  )
})


test_that("compact summary uses the concise title", {
  x <- structure(
    list(
      selected_items = 0L,
      semantic_proxy_score = NA_real_,
      diagnostic_sections = list(),
      evidence_status = data.frame(),
      notice = "Proxy evidence only."
    ),
    class = c("summary.semantica_full_pipeline_result", "list")
  )
  txt <- capture.output(print(x))
  expect_true(any(grepl("SEMANTICA summary", txt, fixed = TRUE)))
  expect_true(any(grepl("SEMANTICA summary", txt, fixed = TRUE)))
})



test_that("phase-3 reporting reads the stored proposal objective, not the semantic-only objective", {
  src <- paste(deparse(body(SEMANTICA:::print_semantica_phase3_summary)), collapse = "\n")
  expect_match(src, "num\\(result\\$proposal_objective_score\\)")
  expect_false(grepl(
    "proposal objective %s.*semantic_objective_score",
    src,
    perl = TRUE
  ))
})


test_that("objective provenance allows PFA to remain a component inside ESEM-guided selection", {
  src <- paste(deparse(body(ACO_with_ESEM)), collapse = "\n")
  expect_match(src, "pfa_component_used")
  expect_match(src, "pfa = pfa_component_used", fixed = TRUE)
  expect_match(src, "final-esem-multicriteria-rerank-v2", fixed = TRUE)
})


test_that("deprecated Pareto-named rerank alias remains a warning-only compatibility path", {
  fx <- semantica_test_three_factor_fixture("separable")
  semantica_test_mock_esem_unavailable()
  args <- semantica_test_aco_args(fx, seed = 992L, history_mode = "none")
  args$elite_multicriteria_rerank <- NULL
  args$elite_pareto_rerank <- FALSE
  expect_warning(
    do.call(ACO_with_ESEM, args),
    "deprecated.*elite_multicriteria_rerank"
  )
})
