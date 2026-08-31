.status_for <- function(result, evidence = "participant_internal_structure") {
  out <- semantica_evidence_status(result)
  out$status[out$evidence == evidence][1L]
}

test_that("participant evidence states reflect attempted, converged, and admissible metadata", {
  expect_identical(.status_for(list()), "not_established")

  expect_identical(
    .status_for(list(participant_validation_performed = TRUE)),
    "participant_data_supplied"
  )

  failed <- list(response_validation = list(result = list(
    converged = FALSE, admissible = FALSE
  )))
  expect_identical(.status_for(failed), "participant_model_attempted_failed")

  inadmissible <- list(response_validation = list(result = list(
    converged = TRUE, admissible = FALSE
  )))
  expect_identical(.status_for(inadmissible), "participant_model_converged_inadmissible")

  admissible <- list(response_validation = list(result = list(
    converged = TRUE, admissible = TRUE
  )))
  evidence <- semantica_evidence_status(admissible)
  expect_identical(
    evidence$status[evidence$evidence == "participant_internal_structure"],
    "participant_model_converged_admissible"
  )
  expect_true(all(
    evidence$status[evidence$evidence != "participant_internal_structure"] != "established"
  ))
})

test_that("canonical configuration hashing is order-stable and secret-safe", {
  x <- list(beta = 2, alpha = list(z = 3, a = 1))
  y <- list(alpha = list(a = 1, z = 3), beta = 2)
  cx <- SEMANTICA:::.semantica_canonicalize_config(x)
  cy <- SEMANTICA:::.semantica_canonicalize_config(y)
  expect_identical(cx, cy)
  expect_identical(
    SEMANTICA:::.semantica_object_md5(cx),
    SEMANTICA:::.semantica_object_md5(cy)
  )
  expect_false(identical(
    SEMANTICA:::.semantica_object_md5(cx),
    SEMANTICA:::.semantica_object_md5(
      SEMANTICA:::.semantica_canonicalize_config(list(beta = 3, alpha = x$alpha))
    )
  ))

  safe <- SEMANTICA:::.semantica_sanitize_config_provenance(list(
    api_key = "SECRET_SENTINEL",
    nested = list(token = "SECRET_SENTINEL", model = "safe-model"),
    endpoint = "https://user:pass@example.com/v1"
  ))
  rendered <- paste(capture.output(str(safe)), collapse = "\n")
  expect_false(grepl("SECRET_SENTINEL", rendered, fixed = TRUE))
  expect_identical(safe$nested$model, "safe-model")
})

test_that("model identity provenance does not invent immutable revisions", {
  expect_identical(
    SEMANTICA:::.semantica_model_identity_status("provider-alias", NULL),
    "mutable_alias"
  )
  expect_identical(
    SEMANTICA:::.semantica_model_identity_status("provider-alias", "abc123"),
    "revision_pinned"
  )
  expect_identical(
    SEMANTICA:::.semantica_model_identity_status(NULL, NULL),
    "unknown"
  )
})

test_that("simplified pipeline records sanitized effective configuration", {
  local_mocked_bindings(
    semantica_full_pipeline_custom = function(...) {
      list(
        reproducibility = list(
          models = list(
            requested_chat_model = "chat-alias",
            resolved_chat_model = "chat-alias",
            chat_model_identity_status = "mutable_alias",
            requested_embedding_model = "embed-alias",
            resolved_embedding_model = "embed-alias",
            embedding_model_identity_status = "mutable_alias"
          )
        ),
        resource_plan = list(effective_workers = 1L),
        performance = list()
      )
    },
    .package = "SEMANTICA"
  )

  secret <- "DO_NOT_SERIALIZE_ME"
  args <- list(
    scale_name = "Mock scale",
    scale_description = "Configuration provenance test",
    factors = list(A = list(definition = "A"), B = list(definition = "B")),
    llm = semantica_llm_config(
      api_key = secret,
      embedding_cache = FALSE,
      preflight = FALSE
    ),
    chat_model = "chat-alias",
    embed_model = "embed-alias",
    resources = semantica_resource_config(cpu_cores = 1L),
    plots = semantica_plot_config(level = "none"),
    verbose = FALSE
  )
  out <- do.call(semantica_full_pipeline, args)

  expect_identical(out$reproducibility$resolved_config_schema,
                   "semantica-resolved-config-1")
  expect_true(is.list(out$reproducibility$resolved_config))
  expect_true(nzchar(out$reproducibility$resolved_config_hash))
  expect_false(grepl(
    secret,
    paste(capture.output(str(out$reproducibility$resolved_config)), collapse = "\n"),
    fixed = TRUE
  ))
  expect_identical(
    out$reproducibility$resolved_config$resources$effective$effective_workers,
    1L
  )

  out2 <- do.call(semantica_full_pipeline, args)
  expect_identical(out$reproducibility$resolved_config,
                   out2$reproducibility$resolved_config)
  expect_identical(out$reproducibility$resolved_config_hash,
                   out2$reproducibility$resolved_config_hash)
})

test_that("preregistration manifest builder is deterministic for explicit inputs", {
  safe_config <- list(alpha = 1, beta = 2)
  x <- SEMANTICA:::.semantica_build_preregistration_manifest(
    safe_config = safe_config,
    config_hash = "abc",
    inputs_hash = "def",
    package_version = "0.2.9",
    r_version = "R mock",
    created_utc = "2026-08-23 12:00:00 UTC"
  )
  y <- SEMANTICA:::.semantica_build_preregistration_manifest(
    safe_config = safe_config,
    config_hash = "abc",
    inputs_hash = "def",
    package_version = "0.2.9",
    r_version = "R mock",
    created_utc = "2026-08-23 12:00:00 UTC"
  )
  expect_identical(x, y)
})
