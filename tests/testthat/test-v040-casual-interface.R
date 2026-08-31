test_that("ACO presets preserve intended evidence regimes", {
  fast <- semantica_aco_config("fast")
  standard <- semantica_aco_config("standard")
  full <- semantica_aco_config("full")

  expect_identical(fast$ants, 20L)
  expect_identical(fast$pfa_mode, "diagnostic")
  expect_false(fast$pfa_during_search)
  expect_identical(fast$esem_every, 10L)
  expect_identical(fast$fit_calibration_mode, "fast")

  expect_identical(standard$ants, 60L)
  expect_identical(standard$search_patience, 40L)
  expect_identical(standard$max_total_iter, 60L)
  expect_identical(standard$pfa_mode, "objective")
  expect_true(standard$pfa_during_search)
  expect_identical(standard$pfa_every, 5L)
  expect_identical(standard$esem_every, 10L)
  expect_identical(standard$esem_cadence_mode, "fixed")
  expect_identical(standard$fit_calibration_mode, "fast")

  expect_identical(full$ants, 60L)
  expect_identical(full$search_patience, 40L)
  expect_identical(full$max_total_iter, 80L)
  expect_identical(full$pfa_mode, "objective")
  expect_true(full$pfa_during_search)
  expect_identical(full$pfa_every, 5L)
  expect_identical(full$esem_every, 5L)
  expect_identical(full$fit_calibration_mode, "strict")
})

test_that("ACO preset overrides keep evaporation independent from patience", {
  a <- semantica_aco_config("standard", search_patience = 75L, max_total_iter = 90L)
  b <- semantica_aco_config("standard", search_patience = 15L, max_total_iter = 90L)
  expect_identical(a$evaporation$resolved_horizon %||% a$evaporation$horizon, 90L)
  expect_identical(b$evaporation$resolved_horizon %||% b$evaporation$horizon, 90L)
  expect_equal(a$evaporation$rho_start, b$evaporation$rho_start)
  expect_equal(a$evaporation$rho_end, b$evaporation$rho_end)
})

test_that("casual factors accept description shorthand and prompt augmentation", {
  f <- SEMANTICA:::.semantica_run_normalize_factors(list(
    A = "Factor A definition.",
    B = list(description = "Factor B definition.", extra_instructions = "Existing instruction.")
  ))
  expect_identical(f$A$description, "Factor A definition.")

  p <- SEMANTICA:::.semantica_run_normalize_prompts(
    list(global = "Global addition.", by_factor = list(B = "B addition.")),
    names(f)
  )
  z <- SEMANTICA:::.semantica_run_apply_prompts(f, p)
  expect_match(z$A$extra_instructions, "Global addition")
  expect_match(z$B$extra_instructions, "Existing instruction")
  expect_match(z$B$extra_instructions, "Global addition")
  expect_match(z$B$extra_instructions, "B addition")
})

test_that("semantica_run standard mode delegates to full pipeline with requested defaults", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(
        list(reproducibility = list()),
        class = c("semantica_full_pipeline_result", "list")
      )
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run(
    scale_name = "Test scale",
    scale_description = "Two related dimensions.",
    factors = list(A = "Definition A.", B = "Definition B."),
    pool_items = 12L,
    selected_items = 4L,
    overgenerate = 1,
    prompts = "Keep wording concrete.",
    aco = "standard",
    llm = list(
      backend = "ollama",
      chat_model = "chat-model",
      embed_model = "embed-model"
    ),
    seed = 11L,
    verbose = FALSE
  )

  expect_identical(captured$item_counts$pool, 12L)
  expect_identical(captured$item_counts$selected, 4L)
  expect_equal(captured$generation$overgenerate, 1)
  expect_identical(captured$ants, 60L)
  expect_identical(captured$search_patience, 40L)
  expect_identical(captured$max_total_iter, 60L)
  expect_identical(captured$pfa$mode, "objective")
  expect_true(captured$pfa$during_search)
  expect_identical(captured$pfa$every, 5L)
  expect_identical(captured$pfa$extraction, "ml")
  expect_identical(captured$pfa$final_extraction, "ml")
  expect_identical(captured$pfa$rotation, "oblimin")
  expect_identical(captured$esem_every, 10L)
  expect_identical(captured$esem$proxy_reference_n, "auto")
  expect_identical(captured$esem$rotation, "geomin")
  expect_identical(captured$esem$score_mode, "structure_weighted")
  expect_identical(captured$esem$cadence_mode, "fixed")
  expect_identical(captured$fit_calibration$mode, "fast")
  expect_identical(captured$quality$content_alignment_mode, "guard")
  expect_identical(captured$quality$semantic_objective_mode, "relative_conservative")
  expect_identical(captured$llm$backend, "ollama")
  expect_identical(captured$chat_model, "chat-model")
  expect_identical(captured$embed_model, "embed-model")
  expect_match(captured$factors$A$extra_instructions, "Keep wording concrete")
  expect_identical(result$run_config$aco_mode, "standard")
  expect_identical(result$reproducibility$run_interface$interface, "semantica_run")
})

test_that("fast and full casual modes alter evidence depth without replacing the engine", {
  calls <- list()
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )
  base <- list(
    scale_name = "x",
    scale_description = "description",
    factors = list(A = "A definition", B = "B definition"),
    llm = "ollama",
    verbose = FALSE
  )
  do.call(semantica_run, c(base, list(aco = "fast")))
  do.call(semantica_run, c(base, list(aco = "full")))

  expect_identical(calls[[1]]$pfa$mode, "diagnostic")
  expect_false(calls[[1]]$pfa$during_search)
  expect_true(calls[[1]]$run_esem_during_search)
  expect_identical(calls[[1]]$fit_calibration$mode, "fast")

  expect_identical(calls[[2]]$pfa$mode, "objective")
  expect_true(calls[[2]]$pfa$during_search)
  expect_identical(calls[[2]]$pfa$every, 5L)
  expect_identical(calls[[2]]$esem_every, 5L)
  expect_identical(calls[[2]]$fit_calibration$mode, "strict")
})

test_that("casual interface rejects empty factor definitions", {
  expect_error(
    semantica_run(
      "x", "description",
      factors = list(A = list(), B = "B definition"),
      llm = "ollama",
      verbose = FALSE
    ),
    "needs a non-empty substantive"
  )
  expect_error(
    semantica_run(
      "x", "description",
      factors = list(A = "A definition", B = "B definition"),
      pool_items = 3L, selected_items = 4L,
      llm = "ollama", verbose = FALSE
    ),
    "cannot exceed"
  )
})
