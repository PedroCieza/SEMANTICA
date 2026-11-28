test_that("the public pipeline forwards grouped options to execution", {
  capture <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .semantica_execute_pipeline = function(...) {
      capture$args <- list(...)
      list()
    },
    .semantica_wrap_run_result = identity,
    .package = "SEMANTICA"
  )
  factors <- list(Clarity = list(description = "Clear thinking.", n_items = 8L))
  semantica_run_custom(
    "Cognitive Agility", "Clear and adaptive thinking.", factors,
    llm = semantica_llm_config(backend = "ollama", embed_backend = "openai"),
    item_counts = semantica_item_count_config(pool = 12L, selected = c(Clarity = 3L),
      override_pool_counts = FALSE),
    generation = semantica_generation_config(temperature = .7),
    ants = 12L, max.iter = 4L,
    fit_calibration = semantica_fit_calibration_config(mode = "heuristic_semantic"),
    pfa = semantica_pfa_config(mode = "off"),
    plots = "none", verbose = FALSE, checkpoint_dir = NULL
  )
  args <- capture$args
  expect_identical(args$n_per_factor, 12L)
  expect_false(args$n_per_factor_override)
  expect_identical(args$factors, factors)
  expect_equal(args$i.per.f, c(Clarity = 3L))
  expect_identical(args$embed_backend, "openai")
  expect_equal(args$temperature, .7)
  expect_identical(args$ants, 12L)
  expect_identical(args$max.iter, 4L)
  expect_identical(args$dfi_mode, "heuristic_semantic")
  expect_identical(args$pfa_mode, "off")
  expect_false(args$generate_plots)
})

test_that("the public pipeline rejects unknown configuration fields before execution", {
  expect_error(
    semantica_run_custom("Test", "Description", list(Clarity = "Clear thinking."),
      quality = list(unknown_search_knob = TRUE), verbose = FALSE, checkpoint_dir = NULL),
    "Unknown 'quality' configuration field.*unknown_search_knob",
    class = "semantica_error_config"
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
