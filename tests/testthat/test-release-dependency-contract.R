test_that("optional categorical dynamic helpers are not hard namespace imports", {
  imports <- getNamespaceImports("SEMANTICA")$dynamic
  expect_false("catHB" %in% names(imports))
  expect_false("catOne" %in% names(imports))

  # The released CRAN build of dynamic may omit these categorical helpers.
  # SEMANTICA must still load; safe_compute_dfi() feature-detects them and
  # retains the pre-existing simulation fallback when they are unavailable.
  expect_true(all(c("cfaHB", "cfaOne") %in% names(imports)))
})
