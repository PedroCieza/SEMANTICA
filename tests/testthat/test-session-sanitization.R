.raw_contains_text <- function(haystack, needle) {
  needle_raw <- charToRaw(enc2utf8(needle))
  if (length(needle_raw) == 0L) return(TRUE)
  if (length(haystack) < length(needle_raw)) return(FALSE)
  starts <- seq_len(length(haystack) - length(needle_raw) + 1L)
  any(vapply(starts, function(start) {
    identical(
      haystack[start:(start + length(needle_raw) - 1L)],
      needle_raw
    )
  }, logical(1L)))
}

test_that("session sanitization uses safe metadata and does not mutate input", {
  secret <- "SEMANTICA_TEST_SECRET_123"
  session <- structure(
    list(
      backend = "openai",
      protocol = "openai_compat",
      label = "OpenAI API",
      api_key = secret,
      hf_token = paste0("hf_", secret),
      auth_header = "Bearer",
      extra_headers = list(Authorization = paste("Bearer", secret)),
      chat_url = paste0(
        "https://user:", secret,
        "@example.com/v1/chat/completions?api_key=", secret,
        "#private"
      ),
      embed_url = paste0("https://example.com/v1/embeddings?token=", secret),
      chat_model = "example-chat",
      embed_model = "example-embed",
      embedding_device = "cuda",
      gpu_precision = "double",
      gguf_path = file.path("private", secret, "model.gguf"),
      configuration = list(device = "cuda", api_key = secret)
    ),
    class = c("semantica_session", "list")
  )

  safe <- SEMANTICA:::sanitize_session_for_result(session)

  expect_s3_class(safe, "semantica_session_metadata")
  expect_false(any(c("api_key", "hf_token", "auth_header", "extra_headers") %in% names(safe)))
  expect_equal(safe$chat_url, "https://example.com/v1/chat/completions")
  expect_equal(safe$embed_url, "https://example.com/v1/embeddings")
  expect_equal(safe$gguf_file, "model.gguf")
  expect_false("api_key" %in% names(safe$configuration))
  expect_identical(session$api_key, secret)
  expect_identical(session$hf_token, paste0("hf_", secret))
})

test_that("nested result sessions and credential fields are removed", {
  secret <- "SEMANTICA_TEST_SECRET_123"
  session <- structure(
    list(
      backend = "python_hf",
      protocol = "python_hf",
      api_key = secret,
      hf_token = secret,
      chat_model = "safe-model",
      embed_model = "safe-embed",
      chat_url = paste0("https://example.test/chat?authorization=", secret)
    ),
    class = c("semantica_session", "list")
  )
  result <- list(
    generation = list(session = session, embed_session = session),
    optimization = list(score = 0.75),
    api_key = secret,
    nested = list(
      Authorization = paste("Bearer", secret),
      retained = "safe"
    )
  )

  safe <- SEMANTICA:::sanitize_result_for_serialization(result)

  expect_false("api_key" %in% names(safe))
  expect_false("Authorization" %in% names(safe$nested))
  expect_identical(safe$nested$retained, "safe")
  expect_s3_class(safe$generation$session, "semantica_session_metadata")
  expect_s3_class(safe$generation$embed_session, "semantica_session_metadata")
  expect_identical(result$generation$session$api_key, secret)
})

test_that("sanitized serialized results contain no sentinel credential bytes", {
  secret <- "SEMANTICA_TEST_SECRET_123"
  result <- list(
    session = structure(
      list(
        backend = "openai",
        protocol = "openai_compat",
        api_key = secret,
        hf_token = secret,
        chat_model = "safe-chat",
        embed_model = "safe-embed",
        extra_headers = list(Authorization = paste("Bearer", secret))
      ),
      class = c("semantica_session", "list")
    ),
    score = 1
  )
  safe <- SEMANTICA:::sanitize_result_for_serialization(result)
  serialized <- serialize(safe, NULL, xdr = FALSE)

  expect_false(.raw_contains_text(serialized, secret))
  expect_true(.raw_contains_text(serialized, "safe-chat"))
})

test_that("result sanitization omits environments and functions", {
  secret <- "SEMANTICA_ENVIRONMENT_SECRET_789"
  captured <- new.env(parent = emptyenv())
  captured$api_key <- secret
  callback <- local({
    private_token <- secret
    function() private_token
  })

  safe <- SEMANTICA:::sanitize_result_for_serialization(list(
    runtime_cache = captured,
    callback = callback,
    retained = "safe"
  ))

  expect_identical(
    safe$runtime_cache,
    "<non-serializable result component omitted>"
  )
  expect_identical(
    safe$callback,
    "<non-serializable result component omitted>"
  )
  expect_identical(safe$retained, "safe")
  expect_false(.raw_contains_text(serialize(safe, NULL), secret))
})

test_that("result sanitization does not treat ordinary tibbles as sessions", {
  tbl <- tibble::tibble(item = "item001", factor = "F1")

  expect_warning(
    safe <- SEMANTICA:::sanitize_result_for_serialization(list(items = tbl)),
    NA
  )
  expect_s3_class(safe$items, class(tbl)[[1L]])
  expect_false("protocol" %in% names(safe$items))
})

test_that("named atomic headers and URLs are sanitized without touching opaque S3", {
  secret <- "SEMANTICA_ATOMIC_SECRET_246"
  widget <- structure(list(value = "safe"), class = "mock_widget")
  result <- list(
    headers = c(
      Authorization = paste("Bearer", secret),
      `X-API-Key` = secret,
      Accept = "application/json"
    ),
    metadata = c(
      endpoint = paste0(
        "https://user:", secret,
        "@example.test/v1?token=", secret,
        "#private"
      ),
      label = "safe"
    ),
    widget = widget
  )

  safe <- SEMANTICA:::sanitize_result_for_serialization(result)

  expect_identical(safe$headers, c(Accept = "application/json"))
  expect_identical(
    safe$metadata,
    c(endpoint = "https://example.test/v1", label = "safe")
  )
  expect_identical(safe$widget, widget)
  expect_false(.raw_contains_text(serialize(safe, NULL, xdr = FALSE), secret))
})

test_that("pipeline attachment is sanitized and keeps its result class", {
  secret <- "SEMANTICA_PIPELINE_SECRET_456"
  items <- data.frame(
    item_id = paste0("id_", 1:4),
    factor = rep(c("F1", "F2"), each = 2L),
    item_text = paste("Item", 1:4),
    stringsAsFactors = FALSE
  )
  live_session <- structure(
    list(
      backend = "openai",
      protocol = "openai_compat",
      api_key = secret,
      hf_token = secret,
      chat_model = "safe-chat",
      embed_model = "safe-embed",
      has_embed = TRUE
    ),
    class = c("semantica_session", "list")
  )
  local_mocked_bindings(
    semantica_connect = function(...) live_session,
    semantica_generate_items = function(...) items,
    semantica_embed = function(...) list(
      embeddings = diag(4L),
      items_tbl = items,
      embed_model = "safe-embed",
      embedding_diagnostics = list(normalized = TRUE)
    ),
    semantica_wrap = function(...) list(
      cosine_sim_matrix = diag(4L),
      df = data.frame(item = items$item_id, factor = items$factor),
      i.per.f = c(F1 = 2L, F2 = 2L),
      compute_telemetry = list(resolved_device = "cpu")
    ),
    .package = "SEMANTICA"
  )

  result <- semantica_pipeline(
    backend = "openai",
    api_key = secret,
    scale_name = "Safe example",
    scale_description = "A mocked serialization test.",
    factors = list(F1 = list(), F2 = list()),
    verbose = FALSE
  )

  expect_s3_class(result, "semantica_pipeline_result")
  expect_s3_class(result$session, "semantica_session_metadata")
  expect_false(any(c("api_key", "hf_token") %in% names(result$session)))
  expect_false(.raw_contains_text(serialize(result, NULL), secret))
  expect_identical(live_session$api_key, secret)
})
