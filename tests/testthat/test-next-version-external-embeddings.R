make_external_embedding_case <- function(dim = 16L) {
  items <- data.frame(
    item_id = paste0("I", 1:6),
    factor = rep(c("A", "B"), each = 3L),
    item_text = paste("Item", 1:6),
    stringsAsFactors = FALSE
  )
  m <- matrix(seq_len(6L * dim), nrow = 6L, ncol = dim)
  m <- sweep(m, 1L, sqrt(rowSums(m^2)), `/`)
  rownames(m) <- items$item_id
  list(items = items, matrix = m)
}

for (d in c(16L, 127L, 768L, 1024L)) {
  test_that(sprintf("external embedding contract accepts arbitrary dimension %d", d), {
    fx <- make_external_embedding_case(d)
    x <- semantica_import_embeddings(
      fx$matrix, fx$items, model = paste0("external-", d),
      provider = "test", task = "psychometric_similarity"
    )
    expect_s3_class(x, "semantica_embedding_result")
    expect_identical(dim(x$embeddings), c(6L, d))
    expect_identical(rownames(x$embeddings), fx$items$item_id)
    wrapped <- semantica_wrap(x, min_items_per_factor = 3L,
                              compute_cosine_sensitivity = FALSE, verbose = FALSE)
    expect_identical(rownames(wrapped$cosine_sim_matrix), fx$items$item_id)
  })
}

test_that("external embedding contract aligns explicitly by IDs", {
  fx <- make_external_embedding_case(16L)
  ord <- c(6, 2, 4, 1, 5, 3)
  shuffled <- fx$matrix[ord, , drop = FALSE]
  x <- semantica_import_embeddings(shuffled, fx$items)
  expect_equal(x$embeddings, fx$matrix, tolerance = 0)
})

test_that("external embedding contract rejects malformed identity and vectors", {
  fx <- make_external_embedding_case(16L)
  no_ids <- unname(fx$matrix)
  rownames(no_ids) <- NULL
  expect_error(semantica_import_embeddings(no_ids, fx$items), "identity is required")

  dup <- fx$matrix; rownames(dup)[2] <- rownames(dup)[1]
  expect_error(semantica_import_embeddings(dup, fx$items), "must be unique")

  missing <- fx$matrix[-1, , drop = FALSE]
  expect_error(semantica_import_embeddings(missing, fx$items), "ID sets differ")

  extra <- rbind(fx$matrix, X = fx$matrix[1, ])
  expect_error(semantica_import_embeddings(extra, fx$items), "ID sets differ")

  bad <- fx$matrix; bad[1, 1] <- NA_real_
  expect_error(semantica_import_embeddings(bad, fx$items), "NA, NaN, or Inf")
  bad <- fx$matrix; bad[1, 1] <- Inf
  expect_error(semantica_import_embeddings(bad, fx$items), "NA, NaN, or Inf")
  bad <- fx$matrix; bad[1, ] <- 0
  expect_error(semantica_import_embeddings(bad, fx$items), "zero or invalid")
  expect_error(semantica_import_embeddings(matrix(letters[1:12], 6), fx$items), "numeric")
  expect_error(semantica_import_embeddings(matrix(numeric(0), 0, 0), fx$items), "at least one row")
})

test_that("external normalization is explicit and provenance is secret-free", {
  fx <- make_external_embedding_case(16L)
  raw <- fx$matrix * 3
  x <- semantica_import_embeddings(
    raw, fx$items, normalize = TRUE,
    provider = "custom", model = "model-x", model_version = "rev1",
    task = "psychometric_similarity", instruction = "safe instruction",
    provenance = list(api_key = "must-not-survive", endpoint = "https://example.test/v1")
  )
  expect_true(x$embedding_diagnostics$normalization_applied)
  expect_true(x$embedding_diagnostics$normalized)
  expect_false("api_key" %in% names(x$representation_provenance))
  expect_equal(unname(sqrt(rowSums(x$embeddings^2))), rep(1, 6), tolerance = 1e-12)
})

test_that("external constructor is numerically identical to equivalent embedding result", {
  fx <- make_external_embedding_case(16L)
  imported <- semantica_import_embeddings(fx$matrix, fx$items)
  legacy_shape <- list(
    embeddings = fx$matrix, items_tbl = fx$items, embed_model = "unknown",
    embedding_diagnostics = list(normalized = TRUE)
  )
  a <- semantica_wrap(imported, compute_cosine_sensitivity = FALSE, verbose = FALSE)
  b <- semantica_wrap(legacy_shape, compute_cosine_sensitivity = FALSE, verbose = FALSE)
  expect_equal(a$cosine_sim_matrix, b$cosine_sim_matrix, tolerance = 0)
})
