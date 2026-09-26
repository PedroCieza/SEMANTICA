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

    # Installed checks resolve aliases from the help database. Source-mode test
    # runs can instead see a separately installed SEMANTICA version, so inspect
    # generated source Rd files only when no installed topic is available.
    topic <- suppressWarnings(utils::help(fn, package = "SEMANTICA"))
    source_alias <- FALSE
    if (!length(topic)) {
      man_dir <- testthat::test_path("..", "..", "man")
      rd_files <- if (dir.exists(man_dir)) {
        list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
      } else {
        character(0L)
      }
      alias_pattern <- paste0("\\\\alias\\{", fn, "\\}")
      source_alias <- any(vapply(rd_files, function(path) {
        any(grepl(alias_pattern, readLines(path, warn = FALSE)))
      }, logical(1L)))
    }
    expect_true(
      length(topic) > 0L || source_alias,
      info = paste(fn, "must have an installed or source Rd alias")
    )
  }
})
