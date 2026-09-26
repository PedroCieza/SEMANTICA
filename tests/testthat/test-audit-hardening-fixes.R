test_that("lavaan transport names preserve safe IDs and remap unsafe/colliding IDs", {
  items <- c("safe_item", "item with space", "FactorA")
  factors <- c("FactorA", "Self Control", "Self-Control")
  assignment <- c("FactorA", "Self Control", "Self-Control")
  names(assignment) <- items

  syntax <- SEMANTICA:::build_esem_syntax_safe(items, assignment, factors)
  map <- attr(syntax, "semantica_lavaan_name_map", exact = TRUE)

  expect_identical(unname(map$item_to_internal[["safe_item"]]), "safe_item")
  expect_false(unname(map$item_to_internal[["item with space"]]) == "item with space")
  expect_false(unname(map$factor_to_internal[["FactorA"]]) == "FactorA")
  expect_length(unique(unname(map$factor_to_internal)), length(factors))
  expect_false(grepl("item with space", syntax, fixed = TRUE))
  expect_false(grepl("Self-Control", syntax, fixed = TRUE))

  target <- SEMANTICA:::build_esem_target_matrix(items, assignment, factors)
  expect_identical(rownames(target), unname(map$item_to_internal[items]))
  expect_identical(colnames(target), unname(map$factor_to_internal[factors]))
})

test_that("lavaan fitted observed matrices restore external item identities", {
  items <- c("item with space", "safe_item")
  factors <- c("Factor One")
  map <- SEMANTICA:::.semantica_make_lavaan_name_map(items, factors)
  internal <- unname(map$item_to_internal[items])
  fitted_cov <- matrix(
    c(1, .2, .2, 1), nrow = 2,
    dimnames = list(internal, internal)
  )
  restored <- SEMANTICA:::.semantica_restore_lavaan_observed_names(fitted_cov, map)
  expect_identical(rownames(restored), items)
  expect_identical(colnames(restored), items)
  expect_equal(unname(restored), unname(fitted_cov))
})

test_that("user-supplied target matrices are translated only at the lavaan boundary", {
  items <- c("item one", "item_two")
  factors <- c("Factor One", "Factor_Two")
  assignment <- c("Factor One", "Factor_Two")
  names(assignment) <- items
  external_target <- matrix(
    c(NA, 0, 0, NA), nrow = 2,
    dimnames = list(items, factors)
  )

  out <- SEMANTICA:::prepare_esem_rotation_args(
    "target", list(target = external_target), items, assignment, factors
  )
  map <- SEMANTICA:::.semantica_make_lavaan_name_map(items, factors)
  expect_identical(rownames(out$target), unname(map$item_to_internal[items]))
  expect_identical(colnames(out$target), unname(map$factor_to_internal[factors]))
  expect_equal(unname(out$target), unname(external_target))
})

test_that("i.per.f validation rejects duplicate factors at the API boundary", {
  expect_error(
    SEMANTICA:::.semantica_validate_i_per_f(c(F1 = 2L, F1 = 3L)),
    "must be unique"
  )
  expect_identical(
    SEMANTICA:::.semantica_validate_i_per_f(c(F1 = 2, F2 = 3)),
    c(F1 = 2L, F2 = 3L)
  )
})

test_that("registered provider keys are not silently forwarded to another host", {
  expect_error(
    semantica_connect(
      "openai", api_key = "test-key", base_url = "https://proxy.example",
      purpose = "chat", preflight = FALSE, verbose = FALSE
    ),
    "Refusing to forward"
  )

  sess <- semantica_connect(
    "openai", api_key = "test-key", base_url = "https://proxy.example",
    purpose = "chat", preflight = FALSE, verbose = FALSE,
    allow_provider_key_forwarding = TRUE
  )
  expect_true(isTRUE(sess$credential_policy$provider_origin_changed))

  # Userinfo syntax must not disguise the actual destination host.
  expect_error(
    semantica_connect(
      "openai", api_key = "test-key",
      base_url = "https://api.openai.com:443@evil.example",
      purpose = "chat", preflight = FALSE, verbose = FALSE
    ),
    "Refusing to forward"
  )
})

test_that("credential routing blocks non-loopback HTTP but permits loopback HTTP", {
  custom <- semantica_backend_spec(
    protocol = "openai_compat",
    chat_url = "http://service.internal/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE,
    auth_header = "Bearer", auth_env = NULL
  )
  expect_error(
    semantica_connect(
      "private_service", backend_spec = custom, api_key = "test-key",
      purpose = "chat", preflight = FALSE, verbose = FALSE
    ),
    "non-loopback HTTP"
  )

  local <- semantica_backend_spec(
    protocol = "openai_compat",
    chat_url = "http://127.0.0.1:1234/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE,
    auth_header = "Bearer", auth_env = NULL
  )
  sess <- semantica_connect(
    "local_private", backend_spec = local, api_key = "test-key",
    purpose = "chat", preflight = FALSE, verbose = FALSE
  )
  expect_false(isTRUE(sess$credential_policy$insecure_destination))

  secure_custom <- semantica_backend_spec(
    protocol = "openai_compat",
    chat_url = "https://models.example/v1/chat/completions",
    can_chat = TRUE, can_embed = FALSE,
    auth_header = "Bearer", auth_env = NULL
  )
  secure_session <- semantica_connect(
    "secure_private", backend_spec = secure_custom, api_key = "test-key",
    purpose = "chat", preflight = FALSE, verbose = FALSE
  )
  expect_false(isTRUE(secure_session$credential_policy$insecure_destination))
})

test_that("Groq developer default uses a current production model", {
  expect_identical(
    SEMANTICA::SEMANTICA_BACKENDS$groq$default_chat_model,
    "openai/gpt-oss-120b"
  )
  explicit_legacy <- semantica_connect(
    "groq", api_key = "test-key",
    chat_model = "llama-3.3-70b-versatile",
    purpose = "chat", preflight = FALSE, verbose = FALSE
  )
  expect_identical(explicit_legacy$chat_model, "llama-3.3-70b-versatile")
})
