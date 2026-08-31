test_that("mandatory mocked compatibility matrix covers every built-in backend", {
  path <- test_path("fixtures", "backend-compatibility.csv")
  matrix <- utils::read.csv(path, stringsAsFactors=FALSE)
  expect_setequal(matrix$backend, names(SEMANTICA_BACKENDS))
  expect_true(all(matrix$mandatory_mocked))
  for (i in seq_len(nrow(matrix))) {
    spec <- SEMANTICA_BACKENDS[[matrix$backend[[i]]]]
    caps <- SEMANTICA:::.semantica_backend_capabilities(spec)
    expect_identical(matrix$generation[[i]], isTRUE(caps$can_chat))
    expect_identical(matrix$embeddings[[i]], isTRUE(caps$can_embed))
    expect_identical(matrix$structured_output[[i]], isTRUE(caps$supports_structured_output))
  }
})

test_that("embedding-only OpenAI-compatible custom backend has a valid preflight registry endpoint", {
  spec <- semantica_backend_spec(
    protocol="openai_compat", label="embed only",
    chat_url=NULL, embed_url="http://example.invalid/v1/embeddings",
    can_chat=FALSE, can_embed=TRUE
  )
  session <- semantica_connect(
    backend="custom_embed", backend_spec=spec, purpose="embed",
    embed_model="mock", preflight=FALSE, verbose=FALSE
  )
  requested <- NULL
  local_mocked_bindings(
    .semantica_get_json = function(session, url, timeout_s) {
      requested <<- url
      list(status=200L, body=list(data=list(list(id="mock"))))
    },
    .package="SEMANTICA"
  )
  out <- semantica_backend_preflight(session, verify_models=TRUE)
  expect_identical(requested, "http://example.invalid/v1/models")
  expect_true(out$ok)
})

test_that("backend typo cannot silently pass mandatory mocked compatibility", {
  expect_error(semantica_connect("openaii", preflight=FALSE, verbose=FALSE), class="semantica_error_backend")
})

test_that("live provider checks are explicitly optional without credentials", {
  skip_if(Sys.getenv("SEMANTICA_RUN_LIVE_TESTS") != "true", "live integration tests are opt-in")
  skip("Live provider matrix requires provider-specific credentials/services and is not part of mandatory CI.")
})

test_that("local-service compatibility checks are explicitly optional", {
  skip_if(Sys.getenv("SEMANTICA_RUN_LOCAL_SERVICE_TESTS") != "true", "local service tests are opt-in")
  skip("Local Ollama/llama.cpp service availability is environment-specific.")
})

test_that("simplified pipeline forwards separate embedding base URL to custom pipeline", {
  body_txt <- paste(deparse(body(semantica_full_pipeline)), collapse="\n")
  hits <- gregexpr("embed_base_url = llm\\$embed_base_url", body_txt, perl = TRUE)[[1L]]
  expect_identical(sum(hits > 0L), 1L)
  custom_formals <- names(formals(semantica_full_pipeline_custom))
  expect_true("embed_base_url" %in% custom_formals)
})
