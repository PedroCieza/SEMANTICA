test_that("bundle schema records exact and canonical consistency checksums", {
  path <- tempfile(fileext = ".rds")
  x <- structure(list(generation=list(a=1), optimization=list(b=2), reproducibility=list(c=3)),
                 class=c("semantica_mock_result","list"))
  semantica_save_bundle(x, path, write_manifest=FALSE)
  stored <- readRDS(path)
  expect_equal(stored$bundle_manifest$bundle_schema_version, 4L)
  expect_equal(stored$bundle_manifest$checksum_purpose, "accidental_corruption_detection")
  expect_equal(stored$bundle_manifest$canonical_checksum_purpose,
               "stable_analysis_projection_accidental_corruption_detection")
  expect_identical(stored$bundle_manifest$primary_checksum, "object_canonical_md5")
  expect_true(nzchar(stored$bundle_manifest$object_md5))
  expect_true(nzchar(stored$bundle_manifest$object_canonical_md5))
  expect_equal(sort(stored$bundle_manifest$required_components),
               sort(c("generation","optimization","reproducibility")))
})

test_that("bundle verification distinguishes reordered named lists from content corruption", {
  path <- tempfile(fileext = ".rds")
  x <- structure(list(generation=list(z=1,a=2), optimization=list(b=2), reproducibility=list(c=3)),
                 class=c("semantica_mock_result","list"))
  semantica_save_bundle(x, path, write_manifest=FALSE, compress=FALSE)
  stored <- readRDS(path)
  # Reorder a named list without changing its semantic named content. Exact MD5
  # changes while the canonical checksum remains the same.
  stored$generation <- stored$generation[c("a","z")]
  saveRDS(stored, path, version=3L, compress=FALSE)
  expect_silent(restored <- semantica_load_bundle(path, verify=TRUE))
  expect_equal(restored$generation$a, 2)

  corrupted <- stored
  corrupted$generation$a <- 999
  saveRDS(corrupted, path, version=3L, compress=FALSE)
  expect_error(semantica_load_bundle(path, verify=TRUE), class="semantica_error_integrity")
})

test_that("bundle reload detects missing manifest-declared components", {
  path <- tempfile(fileext = ".rds")
  x <- list(generation=list(a=1), optimization=list(b=2), reproducibility=list(c=3))
  semantica_save_bundle(x, path, write_manifest=FALSE)
  stored <- readRDS(path)
  stored$optimization <- NULL
  saveRDS(stored, path, version=3L)
  cond <- tryCatch(semantica_load_bundle(path, verify=TRUE), error=identity)
  expect_s3_class(cond, "semantica_error_integrity")
  expect_identical(cond$invariant, "required_components")
})

test_that("canonical resolved-config hashes ignore irrelevant named-list order", {
  a <- list(beta=list(y=2,x=1), alpha=3)
  b <- list(alpha=3, beta=list(x=1,y=2))
  ha <- SEMANTICA:::.semantica_object_md5(SEMANTICA:::.semantica_canonicalize_config(a))
  hb <- SEMANTICA:::.semantica_object_md5(SEMANTICA:::.semantica_canonicalize_config(b))
  expect_identical(ha, hb)
})


test_that("schema-4 bundle verification tolerates runtime-only S4 state but protects analysis slots", {
  cls <- "semantica_bundle_s4_regression"
  if (!methods::isClass(cls)) {
    methods::setClass(cls, slots = c(value = "numeric", runtime = "environment"))
  }
  runtime <- new.env(parent = emptyenv())
  runtime$transient <- 1L
  fit <- methods::new(cls, value = 2, runtime = runtime)
  x <- list(generation = list(a = 1), optimization = list(fit = fit), reproducibility = list(seed = 1L))

  path <- tempfile(fileext = ".rds")
  semantica_save_bundle(x, path, write_manifest = FALSE, compress = FALSE)
  expect_silent(restored <- semantica_load_bundle(path, verify = TRUE))
  expect_equal(methods::slot(restored$optimization$fit, "value"), 2)

  stored <- readRDS(path)
  methods::slot(stored$optimization$fit, "runtime")$transient <- 999L
  saveRDS(stored, path, version = 3L, compress = FALSE)
  expect_silent(semantica_load_bundle(path, verify = TRUE))

  stored <- readRDS(path)
  methods::slot(stored$optimization$fit, "value") <- 999
  saveRDS(stored, path, version = 3L, compress = FALSE)
  cond <- tryCatch(semantica_load_bundle(path, verify = TRUE), error = identity)
  expect_s3_class(cond, "semantica_error_integrity")
  expect_identical(cond$invariant, "object_canonical_md5")
})
