test_that("semantica_standardize_item_metadata returns canonical columns", {
  x <- data.frame(
    item_id = c("A1", "A2"),
    factor = c("Awareness", "Awareness"),
    facet = c("Attention", "Attention"),
    item_text = c(
      "I notice shifts in my attention.",
      "I can describe what I am focusing on."
    )
  )

  out <- semantica_standardize_item_metadata(x)

  expect_named(out, c("ID", "Dimension", "Facet", "item"))
  expect_equal(out$ID, c("A1", "A2"))
  expect_equal(out$Dimension, c("Awareness", "Awareness"))
  expect_equal(out$Facet, c("Attention", "Attention"))
})

test_that("semantica_wrap creates optimizer inputs from manual embeddings", {
  items_tbl <- data.frame(
    item_id = paste0("item_", 1:6),
    factor = rep(c("Clarity", "Flexibility"), each = 3),
    item_text = paste("Item", 1:6)
  )
  embeddings <- matrix(
    c(
      0.90, 0.10, 0.20,
      0.88, 0.12, 0.18,
      0.86, 0.15, 0.22,
      0.15, 0.88, 0.25,
      0.12, 0.86, 0.28,
      0.18, 0.84, 0.23
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(items_tbl$item_id, NULL)
  )
  embed_result <- list(
    embeddings = embeddings,
    items_tbl = items_tbl,
    embed_model = "manual-test",
    embedding_diagnostics = list(normalized = FALSE)
  )

  wrapped <- semantica_wrap(embed_result, min_items_per_factor = 3, verbose = FALSE)

  expect_true(is.matrix(wrapped$cosine_sim_matrix))
  expect_equal(dim(wrapped$cosine_sim_matrix), c(6L, 6L))
  expect_equal(names(wrapped$i.per.f), c("Clarity", "Flexibility"))
  expect_equal(wrapped$i.per.f, c(Clarity = 3L, Flexibility = 3L))
})

test_that("optional cosine sensitivity can be skipped without changing optimizer input", {
  items_tbl <- data.frame(
    item_id = paste0("item_", 1:6),
    factor = rep(c("Clarity", "Flexibility"), each = 3),
    item_text = paste("Item", 1:6)
  )
  embeddings <- matrix(
    c(
      0.90, 0.10, 0.20,
      0.88, 0.12, 0.18,
      0.86, 0.15, 0.22,
      0.15, 0.88, 0.25,
      0.12, 0.86, 0.28,
      0.18, 0.84, 0.23
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(items_tbl$item_id, NULL)
  )
  embed_result <- list(
    embeddings = embeddings,
    items_tbl = items_tbl,
    embed_model = "manual-test",
    embedding_diagnostics = list(normalized = FALSE)
  )

  full <- semantica_wrap(embed_result, verbose = FALSE)
  lean <- semantica_wrap(
    embed_result,
    compute_cosine_sensitivity = FALSE,
    verbose = FALSE
  )

  expect_equal(lean$cosine_sim_matrix, full$cosine_sim_matrix)
  expect_false(lean$cosine_adjustment_sensitivity$available)
})

test_that("semantica_embed fills its result incrementally in requested batches", {
  batch_sizes <- integer(0L)
  local_mocked_bindings(
    .call_embed = function(session, texts) {
      batch_sizes <<- c(batch_sizes, length(texts))
      cbind(text_length = nchar(texts), marker = rep(1, length(texts)))
    },
    .package = "SEMANTICA"
  )
  items <- data.frame(
    item_id = paste0("id_", seq_len(5L)),
    item_text = c("a", "bb", "ccc", "dddd", "eeeee")
  )
  session <- list(protocol = "python_hf", embed_url = NULL, embed_model = "manual-test")

  out <- semantica_embed(
    items, session,
    batch_size = 2L,
    normalize = FALSE,
    verbose = FALSE
  )

  expect_equal(batch_sizes, c(2L, 2L, 1L))
  expect_equal(unname(out$embeddings[, "text_length"]), nchar(items$item_text))
  expect_equal(rownames(out$embeddings), items$item_id)
})
