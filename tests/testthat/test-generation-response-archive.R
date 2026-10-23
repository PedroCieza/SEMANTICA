test_that("optional response archives preserve requests, curation and generation fingerprints", {
  response <- paste(c("1. I explore unfamiliar scientific questions with curiosity.",
    "2. I organize my workspace before starting any task.", "3. I enjoy painting quiet landscapes."), collapse = "\n")
  local_mocked_bindings(.call_chat = function(...) response, .package = "SEMANTICA")
  session <- structure(list(protocol = "openai_compat", backend = "unsloth", chat_model = "offline",
    supports_structured_output = FALSE), class = "semantica_session")
  run <- function(archive) semantica_generate_items(session, "Offline", "Offline",
    list(A = list(description = "Offline")), n_per_factor = 2L, overgenerate = 1.5,
    max_retries = 1L, verbose = FALSE, prompt_history = "omit", history_dedup = "lexical", retain_responses = archive)
  a <- run(FALSE); b <- run(TRUE)
  ma <- attr(a, "semantica_generation_metadata"); mb <- attr(b, "semantica_generation_metadata")
  expect_identical(a$item_text, b$item_text)
  expect_identical(ma$generation_spec_fingerprint, mb$generation_spec_fingerprint)
  expect_identical(ma$item_pool_fingerprint, mb$item_pool_fingerprint)
  expect_null(ma$response_archive)
  expect_length(mb$response_archive, 1L)
  expect_identical(mb$response_archive[[1L]]$raw_response, response)
  expect_match(mb$response_archive[[1L]]$prompt, "Generate exactly 3", fixed = TRUE)
  expect_identical(mb$diversity_curation[[1L]]$discarded_n, 1L)
  expect_error(run(NA), "retain_responses")
})

test_that("an optional failure archive contains preceding facets and the failed unit", {
  local_mocked_bindings(.call_chat = function(...) "1. I enjoy painting quiet landscapes.", .package = "SEMANTICA")
  session <- structure(list(supports_structured_output = FALSE), class = "semantica_session")
  error <- tryCatch(semantica_generate_items(session, "Offline", "Offline",
    list(A = list(description = "Offline", facets = list(First = list(description = "First"),
      Second = list(description = "Second")))), n_per_factor = 2L,
    overgenerate = 1, max_retries = 2L, verbose = FALSE, retain_responses = TRUE), error = identity)
  expect_s3_class(error, "semantica_error_generation_incomplete")
  expect_length(error$generation_failure$response_archive, 3L)
  expect_identical(vapply(error$generation_failure$response_archive, `[[`, character(1L), "facet"),
    c("First", "Second", "Second"))
})
