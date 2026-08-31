test_that("backend registry reflects current embedding capability contracts", {
  expect_false(isTRUE(SEMANTICA_BACKENDS$groq$has_embed))
  expect_null(SEMANTICA_BACKENDS$groq$embed_url)
  expect_true(grepl("/api/embed$", SEMANTICA_BACKENDS$ollama$embed_url))
  expect_true(isTRUE(SEMANTICA_BACKENDS$ollama$has_embed))
})
