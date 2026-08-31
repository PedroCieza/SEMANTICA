test_that("casual QoL generation controls only populate existing generation config", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list(), best_items = c("i1", "i2", "i3", "i4", "i5", "i6")),
                class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )

  semantica_run(
    scale_name = "QoL test",
    scale_description = "Two dimensions.",
    factors = list(A = "Definition A", B = "Definition B"),
    llm = "ollama",
    language = "Spanish",
    response_format = "7-point Likert",
    item_style = "brief first-person statement",
    temperature = 0.35,
    structured_output = "numbered",
    progress = "quiet",
    verbose = TRUE
  )

  expect_identical(captured$generation$language, "Spanish")
  expect_identical(captured$generation$response_format, "7-point Likert")
  expect_identical(captured$generation$item_style, "brief first-person statement")
  expect_equal(captured$generation$temperature, 0.35)
  expect_identical(captured$generation$structured_output, "numbered")
  expect_false(captured$verbose)
})

test_that("run plan exposes counts without executing a backend", {
  plan <- semantica_run_plan(
    scale_name = "Plan",
    scale_description = "Planning only",
    factors = list(
      A = list(description = "A", facets = list(A1 = "A1", A2 = "A2")),
      B = "B"
    ),
    pool_items = 5L,
    selected_items = 2L,
    overgenerate = 2,
    llm = "ollama"
  )
  expect_s3_class(plan, "semantica_run_plan")
  expect_identical(plan$retained_candidates, 10L)
  expect_identical(plan$selected_total, 4L)
  expect_identical(sum(plan$allocation$retained_target[plan$allocation$factor == "A"]), 5L)
  expect_identical(plan$backends$embedding$name, "ollama")
})

test_that("selected item accessor normalizes metadata names and order", {
  x <- structure(list(
    best_items = c("i2", "i1"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"),
      Dimension = c("A", "B"),
      Facet = c("A1", "B1"),
      item = c("First item", "Second item"),
      semantica_factor_score = c(.8, .7),
      stringsAsFactors = FALSE
    )
  ), class = c("semantica_full_pipeline_result", "list"))
  items <- semantica_items(x)
  expect_identical(items$item_id, c("i2", "i1"))
  expect_identical(items$item_text, c("Second item", "First item"))
  detailed <- semantica_items(x, details = TRUE)
  expect_true("semantica_factor_score" %in% names(detailed))
})

test_that("resolved config and model accessors read stored provenance only", {
  x <- structure(list(
    reproducibility = list(
      models = list(
        generation_backend = "ollama",
        embedding_backend = "ollama",
        resolved_chat_model = "chat",
        resolved_embedding_model = "embed"
      ),
      resolved_config = list(
        generation = list(language = "Spanish"),
        optimizer = list(ants = 60L)
      )
    )
  ), class = c("semantica_full_pipeline_result", "list"))
  expect_identical(semantica_config(x, "generation")$language, "Spanish")
  mods <- semantica_models(x)
  expect_identical(mods$resolved_model, c("chat", "embed"))
})

test_that("setup checker catches generation-only backend pairing before network work", {
  chk <- semantica_check_setup(
    llm = list(backend = "anthropic", api_key = "placeholder"),
    probe = FALSE
  )
  expect_s3_class(chk, "semantica_setup_check")
  expect_false(chk$ready)
  expect_true(any(grepl("does not provide embeddings", chk$issues, fixed = TRUE)))

  local_chk <- semantica_check_setup(llm = "ollama", probe = FALSE)
  expect_true(local_chk$ready)
})

test_that("high-level export targets selected scale and preserves legacy component export", {
  td <- tempfile("semantica-qol-export-")
  dir.create(td)
  prefix <- file.path(td, "scale")
  x <- structure(list(
    best_items = c("i1", "i2"),
    selected_item_metadata = data.frame(
      ID = c("i1", "i2"), Dimension = c("A", "B"),
      item = c("One", "Two"), stringsAsFactors = FALSE
    ),
    fit_indices = c(cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_),
    reproducibility = list(resolved_config = list(generation = list(language = "English")))
  ), class = c("semantica_full_pipeline_result", "list"))
  paths <- semantica_export(x, prefix = prefix)
  expect_true(file.exists(paths$selected_items))
  expect_true(file.exists(paths$summary))
  expect_true(file.exists(paths$config))

  wrapped <- list(
    items_tbl = data.frame(item_id = "i1"),
    df = data.frame(item = "i1"),
    cosine_sim_matrix = matrix(1, 1, 1)
  )
  legacy_prefix <- file.path(td, "legacy")
  expect_identical(semantica_export(wrapped, legacy_prefix), legacy_prefix)
  expect_true(file.exists(paste0(legacy_prefix, "_items.csv")))
})

test_that("plot method reuses stored summary plot without rerunning analysis", {
  marker <- structure(list(id = "stored-summary"), class = "semantica_test_plot")
  x <- structure(list(plots = list(plot_summary_of_results = marker)),
                 class = c("semantica_full_pipeline_result", "list"))
  expect_identical(plot(x), marker)
})

test_that("core result plot wrapper keeps the requested stored plot set compact", {
  seen <- NULL
  markers <- lapply(
    c("fitness", "before", "after", "pfa", "summary"),
    function(id) structure(list(id = id), class = "semantica_test_plot")
  )
  local_mocked_bindings(
    semantica_plot_all = function(...) {
      seen <<- list(...)
      structure(
        list(
          p02_fitness = markers[[1L]],
          p10a_path_before = markers[[2L]],
          p10b_path_after = markers[[3L]],
          p13_pfa = markers[[4L]],
          plot_summary_of_results = markers[[5L]]
        ),
        semantica_plot_failures = character(0L),
        semantica_plot_manifest = list(generated = names(markers))
      )
    },
    .package = "SEMANTICA"
  )
  cfg <- semantica_plot_config(level = "summary", progress = FALSE)
  x <- structure(list(), class = c("semantica_full_pipeline_result", "list"))
  got <- .semantica_core_result_plots(x, cfg, progress = FALSE)

  expect_identical(
    names(got),
    c(
      "plot_summary_of_results", "plot_fitness_evolution",
      "plot_esem_before", "plot_esem_after", "plot_pfa_diagnostics"
    )
  )
  expect_identical(got$plot_summary_of_results, markers[[5L]])
  expect_identical(got$plot_fitness_evolution, markers[[1L]])
  expect_identical(got$plot_esem_before, markers[[2L]])
  expect_identical(got$plot_esem_after, markers[[3L]])
  expect_identical(got$plot_pfa_diagnostics, markers[[4L]])
  expect_identical(attr(got, "semantica_plot_failures"), character(0L))
  expect_identical(seen$before_path_model, "proxy")
  expect_setequal(seen$which, c("fitness", "paths", "pfa", "summary"))
})

test_that("cache helpers are explicit and bounded to the cache directory", {
  td <- tempfile("semantica-cache-")
  dir.create(file.path(td, "aa"), recursive = TRUE)
  saveRDS(c(1, 2, 3), file.path(td, "aa", "entry.rds"))
  writeLines("do not delete", file.path(td, "user-note.txt"))
  info <- semantica_cache_info(cache_dir = td)
  expect_identical(info$entries, 1L)
  expect_error(semantica_clear_cache(td), "confirm = TRUE")
  expect_invisible(semantica_clear_cache(td, confirm = TRUE))
  expect_identical(semantica_cache_info(cache_dir = td)$entries, 0L)
  expect_true(file.exists(file.path(td, "user-note.txt")))
})
