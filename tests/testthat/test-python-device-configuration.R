.fake_python_torch <- function(cuda = FALSE, count = 0L, mps = FALSE) {
  list(
    cuda = list(
      is_available = function() cuda,
      device_count = function() count
    ),
    backends = list(
      mps = list(is_available = function() mps)
    ),
    float32 = "float32",
    float16 = "float16",
    bfloat16 = "bfloat16"
  )
}

test_that("Python inference devices resolve explicitly and deterministically", {
  local_mocked_bindings(
    .py_get = function(...) .fake_python_torch(cuda = TRUE, count = 2L),
    .package = "SEMANTICA"
  )

  expect_equal(
    SEMANTICA:::.semantica_resolve_python_inference_device("auto"),
    "cuda:0"
  )
  expect_equal(
    SEMANTICA:::.semantica_resolve_python_inference_device("cuda:1"),
    "cuda:1"
  )
  expect_error(
    SEMANTICA:::.semantica_resolve_python_inference_device("cuda:2"),
    "reports 2 device"
  )
})

test_that("Python auto may select MPS but explicit unavailable devices error", {
  fake <- .fake_python_torch(mps = TRUE)
  local_mocked_bindings(
    .py_get = function(...) fake,
    .package = "SEMANTICA"
  )
  expect_equal(
    SEMANTICA:::.semantica_resolve_python_inference_device("auto"),
    "mps"
  )

  fake <- .fake_python_torch()
  expect_equal(
    SEMANTICA:::.semantica_resolve_python_inference_device("auto"),
    "cpu"
  )
  expect_error(
    SEMANTICA:::.semantica_resolve_python_inference_device("cuda"),
    "reports no CUDA"
  )
  expect_error(
    SEMANTICA:::.semantica_resolve_python_inference_device("mps"),
    "reports no MPS"
  )
})

test_that("session device configuration is normalized without leaking tokens", {
  local_mocked_bindings(
    .ping_backend = function(...) TRUE,
    .package = "SEMANTICA"
  )
  expect_error(
    semantica_connect(
      "ollama", chat_device = "cuda:0", device_map = "auto",
      verbose = FALSE
    ),
    "either an explicit 'chat_device' or 'device_map'"
  )
  session <- semantica_connect(
    "ollama",
    embedding_device = "cuda:1",
    chat_device = "cpu",
    gpu_layers = 0L,
    model_precision = "float32",
    verbose = FALSE
  )
  expect_equal(session$embedding_device, "cuda:1")
  expect_equal(session$chat_device, "cpu")
  expect_null(session$device_map)
  expect_equal(session$gpu_layers, 0L)
  expect_equal(session$model_precision, "float32")
})

test_that("a distinct embedding HF token creates and reaches a separate session", {
  chat_token <- "SEMANTICA_CHAT_TOKEN_135"
  embed_token <- "SEMANTICA_EMBED_TOKEN_246"
  connections <- list()
  embedded_with <- NULL
  items <- data.frame(
    item_id = paste0("id_", 1:4),
    factor = rep(c("F1", "F2"), each = 2L),
    item_text = paste("Item", 1:4),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    semantica_connect = function(...) {
      args <- list(...)
      connections[[length(connections) + 1L]] <<- args
      structure(
        list(
          backend = args$backend,
          protocol = "python_hf",
          hf_token = args$hf_token,
          chat_model = "mock-chat",
          embed_model = if (is.null(args$embed_model)) {
            "mock-embed"
          } else {
            args$embed_model
          },
          has_embed = TRUE
        ),
        class = c("semantica_session", "list")
      )
    },
    semantica_generate_items = function(...) items,
    semantica_embed = function(items_tbl, session, embed_session, ...) {
      embedded_with <<- embed_session
      list(
        embeddings = diag(4L),
        items_tbl = items_tbl,
        embed_model = "mock-embed",
        embedding_diagnostics = list(normalized = TRUE)
      )
    },
    semantica_wrap = function(...) list(
      cosine_sim_matrix = diag(4L),
      df = data.frame(item = items$item_id, factor = items$factor),
      i.per.f = c(F1 = 2L, F2 = 2L),
      compute_telemetry = list(resolved_device = "cpu")
    ),
    .package = "SEMANTICA"
  )

  result <- semantica_pipeline(
    backend = "python_hf",
    hf_token = chat_token,
    embed_hf_token = embed_token,
    embed_model = "mock-embed",
    scale_name = "Token routing",
    scale_description = "A mocked token-routing test.",
    factors = list(F1 = list(), F2 = list()),
    verbose = FALSE
  )

  expect_length(connections, 2L)
  expect_identical(connections[[1L]]$hf_token, chat_token)
  expect_identical(connections[[2L]]$hf_token, embed_token)
  expect_identical(embedded_with$hf_token, embed_token)
  expect_s3_class(result$embed_session, "semantica_session_metadata")
  expect_false("hf_token" %in% names(result$embed_session))
})

test_that("pipeline model release targets only llama.cpp cache entries", {
  cleared_pattern <- NULL
  items <- data.frame(
    item_id = paste0("id_", 1:4),
    factor = rep(c("F1", "F2"), each = 2L),
    item_text = paste("Item", 1:4),
    stringsAsFactors = FALSE
  )
  live_session <- structure(
    list(
      backend = "python_llamacpp",
      protocol = "python_llamacpp",
      chat_model = "mock-chat",
      embed_model = "mock-embed",
      has_embed = TRUE
    ),
    class = c("semantica_session", "list")
  )

  local_mocked_bindings(
    semantica_connect = function(...) live_session,
    semantica_generate_items = function(...) items,
    semantica_embed = function(items_tbl, ...) list(
      embeddings = diag(4L),
      items_tbl = items_tbl,
      embed_model = "mock-embed",
      embedding_diagnostics = list(normalized = TRUE)
    ),
    semantica_wrap = function(...) list(
      cosine_sim_matrix = diag(4L),
      df = data.frame(item = items$item_id, factor = items$factor),
      i.per.f = c(F1 = 2L, F2 = 2L),
      compute_telemetry = list(resolved_device = "cpu")
    ),
    .semantica_clear_python_model_cache = function(pattern = NULL, ...) {
      cleared_pattern <<- pattern
      invisible(character(0L))
    },
    .package = "SEMANTICA"
  )

  semantica_pipeline(
    backend = "python_llamacpp",
    release_local_models = TRUE,
    scale_name = "Cache release",
    scale_description = "A mocked cache-release test.",
    factors = list(F1 = list(), F2 = list()),
    verbose = FALSE
  )

  expect_identical(cleared_pattern, '^"llamacpp"\\|')
  expect_true(grepl(
    cleared_pattern,
    SEMANTICA:::.semantica_python_cache_key(
      "llamacpp", "model.gguf", "chat", 4096L, -1L, "auto"
    )
  ))
  expect_false(grepl(
    cleared_pattern,
    SEMANTICA:::.semantica_python_cache_key(
      "hf_pipe", "org/model", "cuda:0", NULL, "float16"
    )
  ))
})
