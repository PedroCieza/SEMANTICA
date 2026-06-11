test_that("full pipeline option groups resolve to legacy arguments", {
  factors <- list(Clarity = list(description = "Clear thinking."))

  args <- SEMANTICA:::.semantica_full_pipeline_resolve_args(
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive thinking.",
    factors = factors,
    backend = "openai",
    candidate_items_per_factor = 15L,
    candidate_items_per_factor_supplied = FALSE,
    items_per_factor = NULL,
    generation_options = list(
      embed_backend = "openai",
      candidate_items_per_factor = 12L,
      temperature = 0.7,
      rate_limit_policy = "auto",
      api_max_retries = 8L,
      api_max_wait_s = 180,
      request_spacing_s = "auto"
    ),
    optimization_options = list(
      ants = 12L,
      max.iter = 4L,
      use_parallel = FALSE
    ),
    dfi_options = list(dfi_mode = "heuristic_semantic"),
    pfa_options = list(pfa_mode = "off"),
    validation_options = list(validation_n_diagnostic = FALSE),
    plot_options = list(generate = FALSE, include_interactive = FALSE),
    verbose = FALSE,
    legacy_args = list(i.per.f = c(Clarity = 3L), n.cores = 1L)
  )

  expect_equal(args$n_per_factor, 12L)
  expect_true(args$n_per_factor_override)
  expect_equal(args$i.per.f, c(Clarity = 3L))
  expect_equal(args$embed_backend, "openai")
  expect_equal(args$temperature, 0.7)
  expect_equal(args$rate_limit_policy, "auto")
  expect_equal(args$api_max_retries, 8L)
  expect_equal(args$api_max_wait_s, 180)
  expect_equal(args$request_spacing_s, "auto")
  expect_equal(args$ants, 12L)
  expect_false(args$use_parallel)
  expect_equal(args$dfi_mode, "heuristic_semantic")
  expect_equal(args$pfa_mode, "off")
  expect_false(args$validation_n_diagnostic)
  expect_false(args$generate_plots)
  expect_false(args$include_interactive_plot)
  expect_equal(args$n.cores, 1L)
})

test_that("legacy full pipeline arguments remain accepted by name", {
  factors <- list(Flexibility = list(description = "Adaptive thinking."))

  args <- SEMANTICA:::.semantica_full_pipeline_resolve_args(
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive thinking.",
    factors = factors,
    backend = "openai",
    candidate_items_per_factor = 15L,
    candidate_items_per_factor_supplied = FALSE,
    items_per_factor = NULL,
    generation_options = list(),
    optimization_options = list(),
    dfi_options = list(),
    pfa_options = list(),
    validation_options = list(),
    plot_options = list(),
    verbose = FALSE,
    legacy_args = list(
      n_per_factor = 10L,
      i.per.f = c(Flexibility = 4L),
      ants = 20L,
      max.iter = 5L,
      dfi_mode = "heuristic_semantic",
      generate_plots = FALSE
    )
  )

  expect_equal(args$n_per_factor, 10L)
  expect_true(args$n_per_factor_override)
  expect_equal(args$i.per.f, c(Flexibility = 4L))
  expect_equal(args$ants, 20L)
  expect_equal(args$max.iter, 5L)
  expect_equal(args$dfi_mode, "heuristic_semantic")
  expect_false(args$generate_plots)
})

test_that("omitted candidate count preserves nested factor counts", {
  factors <- list(Clarity = list(description = "Clear thinking.", n_items = 8L))

  args <- SEMANTICA:::.semantica_full_pipeline_resolve_args(
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive thinking.",
    factors = factors,
    backend = "openai",
    candidate_items_per_factor = 15L,
    candidate_items_per_factor_supplied = FALSE,
    items_per_factor = NULL,
    generation_options = list(),
    optimization_options = list(),
    dfi_options = list(),
    pfa_options = list(),
    validation_options = list(),
    plot_options = list(),
    verbose = FALSE,
    legacy_args = list()
  )

  expect_equal(args$n_per_factor, 15L)
  expect_false(args$n_per_factor_override)
  expect_null(args$i.per.f)
})

test_that("non-generation option groups reject unknown names", {
  expect_error(
    SEMANTICA:::.semantica_full_pipeline_resolve_args(
      scale_name = "Cognitive Agility",
      scale_description = "Clear and adaptive thinking.",
      factors = list(Clarity = list(description = "Clear thinking.")),
      backend = "openai",
      candidate_items_per_factor = 15L,
      candidate_items_per_factor_supplied = FALSE,
      items_per_factor = NULL,
      generation_options = list(),
      optimization_options = list(unknown_search_knob = TRUE),
      dfi_options = list(),
      pfa_options = list(),
      validation_options = list(),
      plot_options = list(),
      verbose = FALSE,
      legacy_args = list()
    ),
    "Unknown option"
  )
})

test_that("rate limit helpers parse wait durations and choose safe auto spacing", {
  expect_equal(SEMANTICA:::.semantica_parse_wait_s("2"), 2)
  expect_equal(SEMANTICA:::.semantica_parse_wait_s("2m3.5s"), 123.5)
  expect_equal(SEMANTICA:::.semantica_parse_wait_s("250ms"), 0.25)
  expect_true(is.na(SEMANTICA:::.semantica_parse_wait_s("")))

  groq_session <- list(
    backend = "groq",
    chat_url = "https://api.groq.com/openai/v1/chat/completions",
    chat_model = "llama-3.3-70b-versatile"
  )
  local_session <- list(
    backend = "ollama",
    chat_url = "http://localhost:11434/api/chat",
    chat_model = "llama3.2"
  )

  expect_equal(
    SEMANTICA:::.semantica_default_request_spacing_s(groq_session, 0.85),
    (60 / 30) / 0.85
  )
  expect_equal(SEMANTICA:::.semantica_default_request_spacing_s(local_session, 0.85), 0)
})
