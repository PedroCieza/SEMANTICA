test_that("sample-free HTMT visualization uses proxy language", {
  src <- paste(deparse(body(plot_dfi_gauges)), collapse="\n")
  expect_false(grepl('title = "Discriminant Validity"', src, fixed=TRUE))
  expect_true(grepl("semantic separation proxy", src, fixed=TRUE))
})

test_that("every exported next-version API has an Rd alias", {
  next_exports <- c(
    "semantica_backend_spec", "semantica_import_embeddings", "semantica_cosine_context",
    "semantica_semantic_discrimination", "semantica_factor_semantic_diagnostics",
    "semantica_selection_context", "semantica_pfa_esem_discrepancy",
    "semantica_esem_state", "semantica_esem_telemetry"
  )
  exports <- getNamespaceExports("SEMANTICA")
  for (fn in next_exports) {
    expect_true(fn %in% exports, info = paste(fn, "must remain exported"))
    topic <- utils::help(fn, package = "SEMANTICA")
    expect_s3_class(topic, "help_files_with_topic")
    expect_true(length(topic) > 0L, info = paste(fn, "must have installed help"))
  }
})
