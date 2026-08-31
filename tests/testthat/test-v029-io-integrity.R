.make_reload_fixture <- function(prefix) {
  ids <- c("A1", "A2", "B1", "B2")
  items <- data.frame(
    item_id = ids,
    factor = c("A", "A", "B", "B"),
    item_text = paste("Item", ids),
    stringsAsFactors = FALSE
  )
  df <- data.frame(
    item = ids,
    type = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
  cosine <- matrix(
    c(
      1, .6, .1, .2,
      .6, 1, .2, .1,
      .1, .2, 1, .7,
      .2, .1, .7, 1
    ),
    nrow = 4L, byrow = TRUE,
    dimnames = list(ids, ids)
  )
  semantica_export(
    list(items_tbl = items, df = df, cosine_sim_matrix = cosine),
    prefix = prefix
  )
  invisible(list(ids = ids, items = items, df = df, cosine = cosine))
}

test_that("legacy CSV reload validates cross-file structure", {
  td <- tempfile("semantica-reload-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  prefix <- file.path(td, "valid")
  fx <- .make_reload_fixture(prefix)

  valid <- semantica_reload(prefix, default_i_per_f = 1L)
  expect_identical(dim(valid$cosine_sim_matrix), c(4L, 4L))
  expect_identical(valid$i.per.f, c(A = 1L, B = 1L))

  asym_prefix <- file.path(td, "asym")
  .make_reload_fixture(asym_prefix)
  asym <- fx$cosine
  asym[1, 2] <- asym[1, 2] + 0.2
  write.csv(asym, paste0(asym_prefix, "_cosine_matrix.csv"))
  cond <- tryCatch(semantica_reload(asym_prefix), error = identity)
  expect_s3_class(cond, "semantica_error_integrity")
  expect_identical(cond$stage, "reload")
  expect_identical(cond$invariant, "cosine_symmetric")

  missing_prefix <- file.path(td, "missing")
  .make_reload_fixture(missing_prefix)
  missing_df <- fx$df[-1, , drop = FALSE]
  write.csv(missing_df, paste0(missing_prefix, "_df.csv"), row.names = FALSE)
  expect_error(semantica_reload(missing_prefix), class = "semantica_error_integrity")

  duplicate_prefix <- file.path(td, "duplicate")
  .make_reload_fixture(duplicate_prefix)
  dup_items <- fx$items
  dup_items$item_id[2] <- dup_items$item_id[1]
  write.csv(dup_items, paste0(duplicate_prefix, "_items.csv"), row.names = FALSE)
  expect_error(semantica_reload(duplicate_prefix), class = "semantica_error_integrity")

  expect_error(
    semantica_reload(prefix, i.per.f = c(A = 3L, B = 1L)),
    class = "semantica_error_integrity"
  )
})

test_that("bundle MD5 remains backward compatible and is described as a checksum", {
  td <- tempfile("semantica-bundle-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(td, "new.rds")

  result <- structure(list(value = c(3L, 1L, 2L)), class = c("semantica_mock_result", "list"))
  semantica_save_bundle(result, path, write_manifest = FALSE)
  restored <- semantica_load_bundle(path)
  expect_identical(restored$bundle_manifest$checksum_algorithm, "md5")
  expect_identical(restored$bundle_manifest$checksum_purpose,
                   "accidental_corruption_detection")

  # Emulate a genuine pre-metadata 0.2.9 bundle by starting from a bundle
  # produced by SEMANTICA's writer, then removing only the descriptive checksum
  # fields introduced by the integrity hardening. The protected object and its
  # stored object_md5 remain exactly under the real bundle-writing contract.
  legacy <- readRDS(path)
  legacy$bundle_manifest$bundle_schema_version <- 3L
  legacy$bundle_manifest$primary_checksum <- NULL
  legacy$bundle_manifest$checksum_algorithm <- NULL
  legacy$bundle_manifest$checksum_purpose <- NULL
  legacy_path <- file.path(td, "legacy.rds")
  saveRDS(legacy, legacy_path, version = 3L)
  expect_silent(semantica_load_bundle(legacy_path, verify = TRUE))

  corrupted <- legacy
  corrupted$value <- 99L
  saveRDS(corrupted, legacy_path, version = 3L)
  cond <- tryCatch(semantica_load_bundle(legacy_path, verify = TRUE), error = identity)
  expect_s3_class(cond, "semantica_error_integrity")
  expect_identical(cond$stage, "bundle_load")
  expect_identical(cond$invariant, "object_md5")
})
