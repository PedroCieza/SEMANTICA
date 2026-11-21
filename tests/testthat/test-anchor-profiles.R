make_anchor_case <- function() {
  ids <- c(paste0("i", 1:6), paste0("a", 1:3))
  vectors <- rbind(matrix(c(1, 2, 4, 1, 3, 5, 2, 3, 4, 5, 2, 1, 4, 3, 1, 5, 3, 2),
    ncol = 3L, byrow = TRUE), diag(3))
  rownames(vectors) <- ids
  items <- data.frame(item_id = ids, item_text = paste("Wording", ids),
    factor = c(rep(c("A", "B"), each = 3L), rep(NA_character_, 3L)))
  semantica_import_embeddings(vectors, items, model = "test-encoder", model_version = "r1")
}

test_that("anchor profiles reach the optimizer input wrapper with geometry and provenance", {
  raw <- make_anchor_case()
  ids <- raw$items_tbl$item_id[1:6]
  anchors <- c("a3", "a1", "a2")
  baseline <- semantica_import_embeddings(raw$embeddings[ids, , drop = FALSE],
    raw$items_tbl[1:6, ], model = "test-encoder", model_version = "r1")
  baseline_wrapped <- semantica_wrap(baseline, compute_cosine_sensitivity = FALSE, verbose = FALSE)
  expect_equal(baseline_wrapped$cosine_sim_matrix,
    stats::cov2cor(tcrossprod(baseline$embeddings)), tolerance = 1e-12)
  expect_null(baseline_wrapped$embedding_diagnostics$representation_provenance$anchor_profile)
  transformed <- semantica_anchor_profiles(raw, anchors)
  expect_s3_class(transformed, "semantica_embedding_result")
  expect_identical(rownames(transformed$embeddings), ids)
  expect_identical(transformed$items_tbl, raw$items_tbl[1:6, ])
  expect_identical(colnames(transformed$embeddings), anchors)
  expect_equal(rowMeans(transformed$embeddings), setNames(rep(0, 6L), ids), tolerance = 1e-12)
  expect_identical(transformed$embed_model, raw$embed_model)

  wrapped <- semantica_wrap(transformed, compute_cosine_sensitivity = FALSE, verbose = FALSE)
  reference <- stats::cor(t(raw$embeddings[ids, , drop = FALSE]))
  expect_identical(rownames(wrapped$cosine_sim_matrix), ids)
  expect_equal(wrapped$cosine_sim_matrix, reference, tolerance = 1e-12)
  provenance <- wrapped$embedding_diagnostics$representation_provenance
  expect_identical(provenance$model_version, "r1")
  expect_identical(provenance$anchor_profile$anchor_ids, anchors)
  expect_identical(provenance$anchor_profile$anchor_texts,
    setNames(paste("Wording", anchors), anchors))

  path <- withr::local_tempfile()
  saveRDS(raw, path)
  replay <- semantica_anchor_profiles(readRDS(path), anchors)
  expect_identical(replay$representation_provenance$anchor_profile, provenance$anchor_profile)
  expect_error(semantica_anchor_profiles(transformed, anchors), "original encoder vectors")
})

test_that("invalid anchors and flat profiles produce actionable errors", {
  raw <- make_anchor_case()
  anchors <- c("a1", "a2", "a3")
  expect_error(semantica_anchor_profiles(raw, c("a1", "a1", "a3")), "unique anchor IDs")
  expect_error(semantica_anchor_profiles(raw, c("a1", "a2", "missing")), "unique anchor IDs")
  wrong <- raw
  wrong$items_tbl$item_text[7L] <- wrong$items_tbl$item_text[1L]
  expect_error(semantica_anchor_profiles(wrong, anchors), "distinct wording")
  wrong <- raw
  wrong$representation_provenance$model_version <- NULL
  expect_error(semantica_anchor_profiles(wrong, anchors), "encoder model and revision")
  wrong <- raw
  wrong$embeddings[1L, ] <- 1
  expect_error(semantica_anchor_profiles(wrong, anchors), "flat anchor profile")
})
