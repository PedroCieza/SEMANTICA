test_that("embedding policy separates analysis intent from provider task", {
  nomic <- SEMANTICA:::.semantica_embedding_policy("nomic-embed-text", "auto", NULL)
  expect_identical(nomic$analysis_intent, "psychometric_similarity")
  expect_identical(nomic$provider_task, "clustering")
  expect_true(nomic$instruction_applied)
  expect_identical(nomic$source, "model_card")
  expect_true(is.character(nomic$instruction_fingerprint))

  unknown <- SEMANTICA:::.semantica_embedding_policy("future-embedding-model", "auto", NULL)
  expect_identical(unknown$analysis_intent, "psychometric_similarity")
  expect_null(unknown$provider_task)
  expect_false(unknown$instruction_applied)
  expect_identical(unknown$source, "none")
})

test_that("explicit instructions are represented truthfully without fabricating revisions", {
  x <- SEMANTICA:::.semantica_embedding_policy(
    "future-model", "psychometric_similarity", "custom instruction"
  )
  expect_true(x$instruction_applied)
  expect_identical(x$source, "user")
  expect_null(x$provider_task)
  expect_true(is.character(x$instruction_fingerprint))
})

test_that("embedding policy provenance does not alter prepared vectors", {
  session <- structure(list(
    embed_model = "future-model", embedding_task = "auto",
    embedding_instruction = NULL
  ), class = c("semantica_session", "list"))
  texts <- c("alpha", "beta")
  expect_identical(SEMANTICA:::.semantica_prepare_embedding_texts(session, texts), texts)
})
