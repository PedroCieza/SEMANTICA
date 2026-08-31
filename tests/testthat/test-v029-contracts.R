test_that("config constructors reject invalid scalar flags instead of repairing them", {
  for (bad in list(NA, 1, 0, "FALSE", c(TRUE, FALSE))) {
    expect_error(
      semantica_llm_config(embedding_cache = bad),
      class = "semantica_error_config"
    )
  }

  cfg <- semantica_llm_config(
    embedding_cache = FALSE,
    retry_on_failure = FALSE,
    preflight = FALSE
  )
  expect_identical(cfg$embedding_cache, FALSE)
  expect_identical(cfg$retry_on_failure, FALSE)
  expect_identical(cfg$preflight, FALSE)
})

test_that("config numeric domains are validated without undocumented coercion", {
  expect_error(semantica_item_count_config(pool = 3.5), class = "semantica_error_config")
  expect_error(semantica_item_count_config(pool = 0), class = "semantica_error_config")
  expect_error(semantica_item_count_config(selected = c(3, NA)), class = "semantica_error_config")
  expect_error(semantica_generation_config(max_retries = -1), class = "semantica_error_config")
  expect_error(semantica_generation_config(temperature = Inf), class = "semantica_error_config")
  expect_error(semantica_plot_config(width = NaN), class = "semantica_error_config")

  expect_identical(semantica_resource_config(cpu_cores = "serial")$cpu_cores, "serial")
  expect_identical(semantica_resource_config(cpu_cores = "none")$cpu_cores, "none")
  expect_identical(semantica_resource_config(cpu_cores = "off")$cpu_cores, "off")
  expect_identical(semantica_esem_config(esem_eval_top_k = Inf)$esem_eval_top_k, Inf)
})

test_that("config merge rejects unknown fields to prevent silent method misspecification", {
  default <- semantica_llm_config()
  expect_error(
    SEMANTICA:::.semantica_merge_config(
      list(embeddng_cache = FALSE), default, "llm"
    ),
    regexp = "Unknown 'llm' configuration field",
    class = "semantica_error_config"
  )

  expect_error(
    SEMANTICA:::.semantica_merge_config(list(FALSE), default, "llm"),
    class = "semantica_error_config"
  )
  dup <- structure(list(FALSE, TRUE), names = c("embedding_cache", "embedding_cache"))
  expect_error(
    SEMANTICA:::.semantica_merge_config(dup, default, "llm"),
    class = "semantica_error_config"
  )
})

test_that("ACO method controls reject truncation and clamping", {
  ids <- c("A1", "A2", "B1", "B2")
  cosine <- diag(4L)
  dimnames(cosine) <- list(ids, ids)
  df <- data.frame(item = ids, type = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
  target <- c(A = 1L, B = 1L)

  base_args <- list(
    cosine_sim_matrix = cosine,
    df = df,
    i.per.f = target,
    run_esem_during_search = FALSE,
    pfa_mode = "off",
    use_parallel = FALSE,
    verbose = FALSE
  )

  expect_error(do.call(ACO_with_ESEM, c(base_args, list(ants = 10.9))),
               class = "semantica_error_input")
  expect_error(do.call(ACO_with_ESEM, c(base_args, list(ants = -20))),
               class = "semantica_error_input")
  expect_error(do.call(ACO_with_ESEM, c(base_args, list(max.iter = 2.5))),
               class = "semantica_error_input")
  expect_error(do.call(ACO_with_ESEM, c(base_args, list(esem_every = 0))),
               class = "semantica_error_input")
  expect_error(do.call(ACO_with_ESEM, c(base_args, list(cfa_every = 1.5))),
               class = "semantica_error_input")
})

test_that("SEMANTICA conditions retain machine-readable fields", {
  cond <- tryCatch(
    SEMANTICA:::.semantica_assert_flag(NA, "embedding_cache"),
    error = identity
  )
  expect_s3_class(cond, "semantica_error_config")
  expect_s3_class(cond, "semantica_error")
  expect_s3_class(cond, "error")
  expect_identical(cond$argument, "embedding_cache")
  expect_match(conditionMessage(cond), "embedding_cache")
})

test_that("unknown backends require an explicit extension contract", {
  expect_error(
    semantica_connect(
      backend = "typo_backend", base_url = "http://localhost:1234",
      preflight = FALSE, verbose = FALSE
    ),
    class = "semantica_error_backend"
  )

  expect_no_warning(
    explicit <- semantica_connect(
      backend = "generic_openai",
      base_url = "http://localhost:1234",
      preflight = FALSE,
      verbose = FALSE
    )
  )
  expect_identical(explicit$protocol, "openai_compat")
})

test_that("optional diagnostic failures are explicit but remain nonfatal", {
  failed <- SEMANTICA:::.semantica_optional_diagnostic(
    function() stop("mock optional failure"),
    requested = TRUE,
    applicable = TRUE
  )
  expect_null(failed$value)
  expect_identical(failed$status$status, "failed")
  expect_match(failed$status$message, "mock optional failure")

  skipped <- SEMANTICA:::.semantica_optional_diagnostic(
    function() stop("must not run"),
    requested = FALSE
  )
  expect_identical(skipped$status$status, "not_requested")
})

test_that("nullable documented config fields are validated against schema, not runtime defaults", {
  default <- semantica_quality_config()
  expect_false("construct_blueprint" %in% names(default))
  expect_true("construct_blueprint" %in% attr(default, "semantica_schema_fields", exact = TRUE))

  expect_no_warning(
    merged <- SEMANTICA:::.semantica_merge_config(
      list(construct_blueprint = NULL), default, "quality"
    )
  )
  expect_true("construct_blueprint" %in% names(merged))
  expect_null(merged$construct_blueprint)

  expect_error(
    SEMANTICA:::.semantica_merge_config(
      list(construct_blueprnt = NULL), default, "quality"
    ),
    regexp = "Unknown 'quality' configuration field",
    class = "semantica_error_config"
  )
})

test_that("config schema metadata is excluded from resolved-config semantics", {
  a <- SEMANTICA:::.semantica_canonicalize_config(
    SEMANTICA:::.semantica_sanitize_config_provenance(semantica_quality_config())
  )
  b <- semantica_quality_config()
  attr(b, "semantica_schema_fields") <- rev(attr(b, "semantica_schema_fields", exact = TRUE))
  b <- SEMANTICA:::.semantica_canonicalize_config(
    SEMANTICA:::.semantica_sanitize_config_provenance(b)
  )
  expect_identical(a, b)
})
