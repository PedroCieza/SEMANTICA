test_that("SEMANTICA bundles round-trip and preserve integrity metadata", {
  x <- structure(list(
    semantic_score=.8,
    participant_validation_performed=FALSE,
    reproducibility=list(seed=1L),
    api_key="should_be_sanitized_if_supported"
  ), class=c("semantica_full_pipeline_result","list"))
  path <- tempfile(fileext=".rds")
  semantica_save_bundle(x, path, write_manifest=FALSE)
  y <- semantica_load_bundle(path)
  expect_true(file.exists(path))
  expect_equal(y$bundle_manifest$bundle_schema_version, 4L)
  expect_equal(y$bundle_manifest$checksum_purpose, "accidental_corruption_detection")
  expect_true(nzchar(y$bundle_manifest$object_md5))
})
