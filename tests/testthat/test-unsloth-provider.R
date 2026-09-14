test_that("Unsloth is registered as a local OpenAI-compatible chat provider", {
  session <- semantica_connect(
    "unsloth",
    api_key = "sk-unsloth-test",
    chat_model = "mock-model",
    purpose = "chat",
    preflight = FALSE,
    verbose = FALSE
  )

  expect_s3_class(session, "semantica_session")
  expect_identical(session$backend, "unsloth")
  expect_identical(session$protocol, "openai_compat")
  expect_identical(session$auth_header, "Bearer")
  expect_identical(session$api_key, "sk-unsloth-test")
  expect_false(isTRUE(session$has_embed))
  expect_null(session$embed_url)
})

test_that("Unsloth preflight uses the OpenAI-compatible model registry", {
  session <- semantica_connect(
    "unsloth",
    api_key = "sk-unsloth-test",
    chat_model = "mock-model",
    purpose = "chat",
    preflight = FALSE,
    verbose = FALSE
  )
  requested <- NULL

  local_mocked_bindings(
    .semantica_get_json = function(session, url, timeout_s) {
      requested <<- url
      list(status = 200L, body = list(data = list(list(id = "mock-model"))))
    },
    .package = "SEMANTICA"
  )

  out <- semantica_backend_preflight(session, verify_models = TRUE)
  expect_identical(requested, "http://localhost:8888/v1/models")
  expect_true(out$ok)
  expect_identical(out$chat_model_available, TRUE)
})

test_that("Unsloth requires a separate embedding backend for full setup", {
  chk <- semantica_check_setup(
    llm = list(
      backend = "unsloth",
      api_key = "sk-unsloth-test",
      chat_model = "mock-model"
    ),
    probe = FALSE
  )

  expect_false(chk$ready)
  expect_true(any(grepl("does not provide embeddings", chk$issues, fixed = TRUE)))

  paired <- semantica_check_setup(
    llm = list(
      backend = "unsloth",
      embed_backend = "ollama",
      api_key = "sk-unsloth-test",
      chat_model = "mock-model",
      embed_model = "nomic-embed-text"
    ),
    probe = FALSE
  )
  expect_true(paired$ready)
})
