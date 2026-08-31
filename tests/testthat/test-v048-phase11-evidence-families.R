test_that("evidence records carry source-family provenance", {
  a <- SEMANTICA:::.semantica_evidence_record("computed", value = 1, participant_based = FALSE)
  b <- SEMANTICA:::.semantica_evidence_record("computed", value = 1, participant_based = TRUE)
  expect_identical(a$source_family, "embedding_semantic")
  expect_identical(a$evidence_family, "embedding_semantic")
  expect_identical(b$source_family, "response_data")
})

test_that("evidence status does not count PFA and ESEM as independent sources", {
  result <- list(
    dimensionality_mode = "multidimensional",
    cosine_diagnostics = list(mean = .5),
    representation_evidence_state = list(status = "representation_sensitive"),
    pfa_diagnostics = list(available = TRUE),
    esem_state = list(status = "computed"),
    semantic_cluster_consensus = list(selected = list(available = TRUE)),
    content_alignment = list(available = TRUE),
    construct_coverage = list(semantic_alignment_available = TRUE),
    participant_validation_performed = FALSE
  )
  z <- semantica_evidence_status(result)
  emb <- z[z$evidence %in% c("embedding_representation", "semantic_construct_separation", "sample_free_pfa_proxy", "sample_free_esem_proxy", "semantic_cluster_consensus"), ]
  expect_true(all(emb$source_family == "embedding_semantic"))
  expect_true(all(grepl("not independent", emb$dependency_note, ignore.case = TRUE)))
})
