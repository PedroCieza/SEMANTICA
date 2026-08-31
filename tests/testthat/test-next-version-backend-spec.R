test_that("explicit custom OpenAI-compatible backend contracts are accepted", {
  spec <- semantica_backend_spec(
    protocol = "openai_compat", label = "Test service",
    chat_url = "http://localhost:9999/v1/chat/completions",
    embed_url = "http://localhost:9999/v1/embeddings",
    can_chat = TRUE, can_embed = TRUE, supports_structured_output = TRUE
  )
  sess <- semantica_connect(
    backend = "test_service", backend_spec = spec,
    chat_model = "chat-x", embed_model = "embed-x",
    preflight = FALSE, verbose = FALSE
  )
  expect_identical(sess$protocol, "openai_compat")
  expect_true(sess$capabilities$can_chat)
  expect_true(sess$capabilities$can_embed)
  expect_true(sess$supports_structured_output)
})

test_that("backend capabilities prevent accidental cross-use", {
  embed_only <- semantica_backend_spec(
    "openai_compat", label = "Embed only",
    embed_url = "http://localhost:9999/v1/embeddings",
    can_chat = FALSE, can_embed = TRUE
  )
  expect_error(
    semantica_connect("embed_only", backend_spec = embed_only,
                      purpose = "chat", preflight = FALSE, verbose = FALSE),
    class = "semantica_error_backend_capability"
  )
  expect_no_error(
    semantica_connect("embed_only", backend_spec = embed_only, embed_model = "e",
                      purpose = "embed", preflight = FALSE, verbose = FALSE)
  )

  chat_only <- semantica_backend_spec(
    "openai_compat", label = "Chat only",
    chat_url = "http://localhost:9999/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE
  )
  expect_error(
    semantica_connect("chat_only", backend_spec = chat_only,
                      purpose = "embed", preflight = FALSE, verbose = FALSE),
    class = "semantica_error_backend_capability"
  )
})

test_that("backend specifications do not carry credentials", {
  spec <- semantica_backend_spec(
    "openai_compat", chat_url = "http://localhost/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE, auth_env = "CUSTOM_API_KEY"
  )
  expect_false(any(grepl("key|token|secret|password", names(spec), ignore.case = TRUE) & names(spec) != "auth_env"))
  expect_false(any(vapply(spec, function(x) is.character(x) && any(grepl("Bearer [A-Za-z0-9]", x)), logical(1L))))
})


test_that("custom backend specs reject credential-bearing extra headers", {
  expect_error(
    semantica_backend_spec(
      "openai_compat", chat_url = "http://localhost/v1/chat/completions",
      can_chat = TRUE, can_embed = FALSE,
      extra_headers = list(Authorization = "Bearer placeholder")
    ),
    "must not contain credentials"
  )
  expect_error(
    semantica_backend_spec(
      "openai_compat", chat_url = "http://localhost/v1/chat/completions",
      can_chat = TRUE, can_embed = FALSE,
      extra_headers = list(`X-API-Key` = "should-not-be-stored")
    ),
    "must not contain credentials"
  )
})

test_that("batch-embedding capability is false for chat-only contracts", {
  spec <- semantica_backend_spec(
    "openai_compat", chat_url = "http://localhost/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE, supports_batch_embeddings = TRUE
  )
  caps <- SEMANTICA:::.semantica_backend_capabilities(spec)
  expect_false(caps$supports_batch_embeddings)
})
