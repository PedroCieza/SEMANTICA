test_that("new cloud providers expose their documented capabilities", {
  expected <- list(
    gemini = list(embed = TRUE, auth = "GEMINI_API_KEY"),
    nvidia_nim = list(embed = TRUE, auth = "NVIDIA_API_KEY"),
    huggingface = list(embed = FALSE, auth = "HF_TOKEN")
  )
  for (backend in names(expected)) {
    spec <- SEMANTICA:::SEMANTICA_BACKENDS[[backend]]
    caps <- SEMANTICA:::.semantica_backend_capabilities(spec)
    expect_true(isTRUE(caps$can_chat))
    expect_identical(isTRUE(caps$can_embed), expected[[backend]][["embed"]])
    expect_identical(spec$auth_env, expected[[backend]][["auth"]])
  }
})

test_that("new cloud-provider sessions use documented endpoints without a network request", {
  sessions <- list(
    gemini = semantica_connect("gemini", api_key = "test", purpose = "both", preflight = FALSE, verbose = FALSE),
    nvidia_nim = semantica_connect("nvidia_nim", api_key = "test", purpose = "both", preflight = FALSE, verbose = FALSE),
    huggingface = semantica_connect("huggingface", api_key = "test", purpose = "chat", preflight = FALSE, verbose = FALSE)
  )
  expect_match(sessions$gemini$chat_url, "generativelanguage.googleapis.com", fixed = TRUE)
  expect_match(sessions$nvidia_nim$embed_url, "ai.api.nvidia.com/v1/retrieval/nvidia/embeddings", fixed = TRUE)
  expect_match(sessions$huggingface$chat_url, "router.huggingface.co/v1/chat/completions", fixed = TRUE)
  expect_error(semantica_connect("huggingface", api_key = "test", purpose = "embed", preflight = FALSE, verbose = FALSE), "not embedding-capable")
})

test_that("cloud-provider credentials are not forwarded to an override by default", {
  for (backend in c("gemini", "nvidia_nim", "huggingface")) {
    expect_error(
      semantica_connect(backend, api_key = "test", base_url = "https://proxy.example", purpose = "chat", preflight = FALSE, verbose = FALSE),
      "Refusing to forward"
    )
  }
})
