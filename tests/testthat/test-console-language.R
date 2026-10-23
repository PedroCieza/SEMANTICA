test_that("console feedback defaults to the existing English text", {
  expect_identical(SEMANTICA:::.semantica_console_language(), "English")
  expect_identical(
    SEMANTICA:::.semantica_console_text(
      "English", "run_starting", "Test scale", 2L, 30L, 60L, 6L
    ),
    "[SEMANTICA] Starting Test scale: 2 factor(s), 30 retained candidates, ~60 raw-generation target, 6 final items."
  )
  expect_identical(
    SEMANTICA:::.semantica_console_text("English", "custom_step_generation"),
    "\n[SEMANTICA] Step 1/3: LLM Generation & Semantic Embedding...\n"
  )
})

test_that("semantica_run selects and forwards Spanish console feedback", {
  captured <- NULL
  local_mocked_bindings(
    semantica_run_custom = function(...) {
      captured <<- list(...)
      structure(
        list(
          best_items = c("i1", "i2", "i3", "i4", "i5", "i6"),
          reproducibility = list()
        ),
        class = c("semantica_full_pipeline_result", "list")
      )
    },
    .package = "SEMANTICA"
  )

  spanish_output <- capture.output(
    result <- semantica_run(
      scale_name = "Test scale",
      scale_description = "Two related dimensions.",
      factors = list(A = "Definition A.", B = "Definition B."),
      llm = "ollama",
      console_language = "Spanish"
    ),
    type = "message"
  )

  expect_match(paste(spanish_output, collapse = "\n"), "Iniciando Test scale")
  expect_match(paste(spanish_output, collapse = "\n"), "Completado: 6")
  expect_identical(captured$console_language, "Spanish")
  expect_identical(result$run_config$console_language, "Spanish")

  english_output <- capture.output(
    invisible(semantica_run(
      scale_name = "Test scale",
      scale_description = "Two related dimensions.",
      factors = list(A = "Definition A.", B = "Definition B."),
      llm = "ollama",
      language = "Spanish"
    )),
    type = "message"
  )
  expect_match(paste(english_output, collapse = "\n"), "Starting Test scale")
  expect_identical(captured$console_language, "English")
  expect_identical(captured$generation$language, "Spanish")
})

test_that("semantica_run_custom exposes Spanish console feedback controls", {
  expect_identical(
    eval(formals(SEMANTICA::semantica_run_custom)$console_language),
    c("English", "Spanish")
  )
  expect_match(
    SEMANTICA:::.semantica_console_text("Spanish", "custom_step_generation"),
    "Paso 1/3"
  )

  plan <- semantica_resource_plan(n.cores = 1L, memory_aware = FALSE)
  english_plan <- capture.output(print(plan))
  expect_match(paste(english_plan, collapse = "\n"), "Requested workers")
  attr(plan, "semantica_console_language") <- "Spanish"
  spanish_plan <- capture.output(print(plan))
  expect_match(paste(spanish_plan, collapse = "\n"), "Trabajadores solicitados")

  expect_error(
    SEMANTICA:::.semantica_console_language("French"),
    "English.*Spanish"
  )
  expect_error(
    semantica_run_custom(
      scale_name = "Test scale",
      scale_description = "Description.",
      factors = list(A = "Definition A."),
      console_language = "French",
      verbose = FALSE
    ),
    "English.*Spanish"
  )
})

test_that("semantica_run_custom forwards the selected console language", {
  captured <- NULL
  local_mocked_bindings(
    .semantica_execute_pipeline = function(...) {
      captured <<- list(...)
      structure(
        list(
          best_items = c("i1", "i2", "i3"),
          reproducibility = list(),
          performance = list(resource = list())
        ),
        class = c("semantica_full_pipeline_result", "list")
      )
    },
    .package = "SEMANTICA"
  )

  result <- semantica_run_custom(
    scale_name = "Test scale",
    scale_description = "A test construct.",
    factors = list(A = "Definition A."),
    llm = "ollama",
    plots = FALSE,
    verbose = FALSE,
    console_language = "Spanish"
  )

  expect_identical(captured$console_language, "Spanish")
  expect_identical(result$reproducibility$resolved_config$console$language, "Spanish")
})

test_that("semantica_run_custom emits Spanish feedback before a safe backend failure", {
  custom_output <- capture.output(
    custom_error <- try(
      semantica_run_custom(
        scale_name = "Test scale",
        scale_description = "A test construct.",
        factors = list(A = "Definition A."),
        llm = "not-a-backend",
        plots = FALSE,
        checkpoint_dir = NULL,
        verbose = TRUE,
        console_language = "Spanish"
      ),
      silent = TRUE
    )
  )

  expect_true(inherits(custom_error, "try-error"))
  expect_match(paste(custom_output, collapse = "\n"), "Trabajadores solicitados")
})
