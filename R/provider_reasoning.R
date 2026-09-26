# Internal provider/model generation capabilities and rate-limit state.
# These helpers deliberately do not add public configuration surface: provider-
# specific request fields are sent only when SEMANTICA has an explicit capability
# contract for the active backend/model. Unknown models keep legacy behavior.

.semantica_reasoning_model_profile <- function(session = NULL, backend = NULL, model = NULL,
                                               reasoning_tokens = NA_real_,
                                               runtime_reasoning_tokens = NA_real_, ...) {
  dots <- list(...)
  # Accept either the normal session-list form or explicit backend/model values.
  # The explicit form is intentionally permissive because this is an internal
  # capability helper used by tests and provider adapters rather than a public API.
  known_backends <- c(
    "openai", "anthropic", "groq", "gemini", "nvidia_nim", "huggingface",
    "ollama", "unsloth", "llamacpp",
    "generic_openai", "python_hf", "python_llamacpp"
  )
  if (is.character(session) && length(session) == 1L && !is.na(session)) {
    first <- as.character(session[[1L]])
    first_norm <- tolower(trimws(first))
    second_norm <- if (!is.null(backend) && length(backend) == 1L && !is.na(backend)) {
      tolower(trimws(as.character(backend[[1L]])))
    } else ""
    if (nzchar(second_norm) && first_norm %in% known_backends &&
        !second_norm %in% known_backends && is.null(model)) {
      model <- backend
      backend <- first_norm
    } else if (is.null(model)) {
      model <- first
    }
    session <- list()
  }
  if (is.null(session)) session <- list()
  if (!is.list(session)) session <- list()

  backend_value <- as.character(backend %||% session$backend %||% "")
  model_value <- as.character(model %||% session$chat_model %||% "")
  backend_value <- if (length(backend_value)) tolower(trimws(backend_value[[1L]])) else ""
  model_value <- if (length(model_value)) tolower(trimws(model_value[[1L]])) else ""
  if (is.na(backend_value)) backend_value <- ""
  if (is.na(model_value)) model_value <- ""

  out <- list(
    name = "default",
    reasoning_model = FALSE,
    reasoning_detected_by = character(0L),
    provider_reasoning_fields = FALSE,
    reasoning_effort = NULL,
    include_reasoning = NULL,
    prefer_user_prompt = FALSE,
    completion_field = "max_tokens",
    strict_json_schema = FALSE,
    min_completion_tokens = NA_integer_
  )

  # Generic model-family recognition is deliberately separate from the wire
  # contract below. A compatible server may host a reasoning model without
  # supporting provider-specific request fields.
  model_reasoning <- nzchar(model_value) && grepl(
    "(^|/)(gpt-oss|qwen3)|(^|[-_/])o[134]([-_/]|$)|(^|/)gpt-5",
    model_value
  )
  if (isTRUE(model_reasoning)) {
    out$reasoning_model <- TRUE
    out$reasoning_detected_by <- c(out$reasoning_detected_by, "model_family")
    out$name <- "reasoning_model_unconfigured_transport"
  }

  state <- .semantica_rate_state(session)
  observed_state <- if (is.null(state)) NA_real_ else {
    suppressWarnings(as.numeric(state$observed_reasoning_tokens %||% NA_real_))
  }
  runtime_values <- suppressWarnings(as.numeric(c(
    reasoning_tokens, runtime_reasoning_tokens, observed_state,
    dots$observed_reasoning_tokens, dots$reasoning_reserve,
    session$observed_reasoning_tokens, session$reasoning_tokens_observed
  )))
  runtime_values <- runtime_values[is.finite(runtime_values) & runtime_values > 0]
  runtime_flag <- isTRUE(dots$reasoning_evidence) ||
    isTRUE(dots$runtime_reasoning_evidence) ||
    isTRUE(session$reasoning_evidence) || isTRUE(session$reasoning_observed)
  if (length(runtime_values) || isTRUE(runtime_flag)) {
    out$reasoning_model <- TRUE
    out$reasoning_detected_by <- unique(c(out$reasoning_detected_by, "runtime_usage"))
    if (identical(out$name, "default")) out$name <- "runtime_reasoning_evidence"
  }

  # Groq documents these request fields for its GPT-OSS and Qwen 3.8 models.
  # The contract is provider-specific: never forward these controls merely
  # because an unknown OpenAI-compatible server exposes a similarly named model.
  if (identical(backend_value, "groq") && grepl("^openai/gpt-oss-(20b|120b)$", model_value)) {
    out$name <- "groq_gpt_oss"
    out$reasoning_model <- TRUE
    out$reasoning_detected_by <- unique(c(out$reasoning_detected_by, "provider_contract"))
    out$provider_reasoning_fields <- TRUE
    out$reasoning_effort <- "low"
    out$include_reasoning <- FALSE
    out$prefer_user_prompt <- TRUE
    out$completion_field <- "max_completion_tokens"
    out$strict_json_schema <- TRUE
    return(out)
  }

  if (identical(backend_value, "groq") && grepl("^qwen/qwen3\\.8-27b$", model_value)) {
    out$name <- "groq_qwen38"
    out$reasoning_model <- TRUE
    out$reasoning_detected_by <- unique(c(out$reasoning_detected_by, "provider_contract"))
    out$provider_reasoning_fields <- TRUE
    out$reasoning_effort <- "low"
    out$include_reasoning <- FALSE
    out$prefer_user_prompt <- TRUE
    out$completion_field <- "max_completion_tokens"
    out$strict_json_schema <- TRUE
    return(out)
  }

  out
}

# Backward-compatible name retained for the already released internal surface.
.semantica_model_generation_profile <- function(session, ...) {
  .semantica_reasoning_model_profile(session = session, ...)
}

.semantica_items_response_schema <- function() {
  list(
    type = "json_schema",
    json_schema = list(
      name = "semantica_item_generation",
      strict = TRUE,
      schema = list(
        type = "object",
        properties = list(
          items = list(type = "array", items = list(type = "string"))
        ),
        required = list("items"),
        additionalProperties = FALSE
      )
    )
  )
}

# Backward-compatible singular alias.
.semantica_item_response_schema <- function() .semantica_items_response_schema()

.semantica_chat_response_format <- function(session, response_format = NULL) {
  if (!identical(response_format, "json") || !isTRUE(session$supports_structured_output)) {
    return(NULL)
  }
  profile <- .semantica_model_generation_profile(session)
  if (isTRUE(profile$strict_json_schema)) return(.semantica_items_response_schema())
  list(type = "json_object")
}

.semantica_generation_visible_token_budget <- function(visible_tokens = NULL,
                                                        reasoning_tokens = NA_real_,
                                                        reasoning_model = NULL,
                                                        session = NULL,
                                                        max_tokens = NULL,
                                                        observed_reasoning_tokens = NA_real_,
                                                        reasoning_reserve = NA_real_, ...) {
  dots <- list(...)
  profile <- dots$profile %||% dots$reasoning_profile
  if (is.list(reasoning_tokens) && !is.null(reasoning_tokens$reasoning_model)) {
    profile <- reasoning_tokens
    reasoning_tokens <- NA_real_
  }
  if (is.list(reasoning_model) && !is.null(reasoning_model$reasoning_model)) {
    profile <- reasoning_model
    reasoning_model <- isTRUE(profile$reasoning_model)
  }
  if (is.list(profile) && is.null(reasoning_model)) {
    reasoning_model <- isTRUE(profile$reasoning_model)
  }
  # `visible_tokens` is the amount the caller needs for user-visible output.
  # Reasoning reserve is added only when it is observed/evidenced; recognizing a
  # reasoning-capable model alone does not justify an arbitrary fixed cushion.
  base <- visible_tokens
  if (is.null(base)) base <- max_tokens
  if (is.null(base)) base <- dots$visible_token_budget %||% dots$requested_visible_tokens
  if (is.null(base)) base <- 1L
  base <- suppressWarnings(as.numeric(base))
  base <- if (length(base)) base[[1L]] else NA_real_
  if (!is.finite(base) || base < 1) base <- 1

  state <- .semantica_rate_state(session)
  state_reasoning <- if (is.null(state)) NA_real_ else {
    suppressWarnings(as.numeric(state$observed_reasoning_tokens %||% NA_real_))
  }
  reserve <- suppressWarnings(as.numeric(c(
    reasoning_tokens, observed_reasoning_tokens, reasoning_reserve, state_reasoning,
    dots$runtime_reasoning_tokens, dots$evidenced_reasoning_tokens,
    dots$observed_reasoning_cost, dots$reasoning_tokens_observed,
    dots$observed_reasoning
  )))
  reserve <- reserve[is.finite(reserve) & reserve > 0]
  reserve <- if (length(reserve)) max(reserve) else 0

  # Keep the argument for diagnostics/forward compatibility. Runtime token
  # evidence is sufficient on its own, including for previously unknown models.
  if (is.null(reasoning_model) && !is.null(session)) {
    reasoning_model <- isTRUE(.semantica_reasoning_model_profile(session)$reasoning_model)
  }

  total <- ceiling(base + reserve)
  total <- min(total, .Machine$integer.max)
  as.integer(total)
}

.semantica_reasoning_completion_budget <- function(session, max_tokens) {
  # `max_tokens` at the transport boundary is already the total completion
  # budget. Retry logic decides whether an evidence-based reasoning reserve is
  # needed before calling the transport, so never add a second reserve here.
  out <- suppressWarnings(as.numeric(max_tokens))
  out <- if (length(out)) out[[1L]] else NA_real_
  if (!is.finite(out) || out < 1) out <- 1
  as.integer(min(ceiling(out), .Machine$integer.max))
}

.semantica_new_rate_limit_state <- function() {
  state <- new.env(parent = emptyenv())
  state$last_request_at <- NA_real_
  state$limit_tokens <- NA_real_
  state$remaining_tokens <- NA_real_
  state$reset_tokens_s <- NA_real_
  state$token_state_observed_at <- NA_real_
  state$remaining_requests <- NA_real_
  state$reset_requests_s <- NA_real_
  state$request_state_observed_at <- NA_real_
  state$chars_per_prompt_token <- NA_real_
  state$observed_reasoning_tokens <- NA_real_
  state$reasoning_evidence_count <- 0L
  state$retryable_responses <- 0L
  state$rate_limit_responses <- 0L
  state$wait_count <- 0L
  state$total_wait_s <- 0
  state
}

.semantica_rate_state <- function(session) {
  state <- session$rate_limit_state
  if (is.environment(state)) return(state)
  NULL
}

.semantica_header_number <- function(resp, name) {
  x <- tryCatch(httr2::resp_header(resp, name), error = function(e) NULL)
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]]) || !nzchar(trimws(as.character(x[[1L]])))) {
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(x[[1L]]))
  if (length(out) == 1L && is.finite(out)) out else NA_real_
}

.semantica_header_wait <- function(resp, name) {
  x <- tryCatch(httr2::resp_header(resp, name), error = function(e) NULL)
  if (is.null(x)) return(NA_real_)
  .semantica_parse_wait_s(x)
}

.semantica_update_rate_limit_state <- function(session, resp) {
  state <- .semantica_rate_state(session)
  if (is.null(state) || is.null(resp)) return(invisible(NULL))
  now <- as.numeric(Sys.time())

  limit_tokens <- .semantica_header_number(resp, "x-ratelimit-limit-tokens")
  remaining_tokens <- .semantica_header_number(resp, "x-ratelimit-remaining-tokens")
  reset_tokens <- .semantica_header_wait(resp, "x-ratelimit-reset-tokens")
  remaining_requests <- .semantica_header_number(resp, "x-ratelimit-remaining-requests")
  reset_requests <- .semantica_header_wait(resp, "x-ratelimit-reset-requests")

  if (is.finite(limit_tokens)) state$limit_tokens <- limit_tokens
  if (is.finite(remaining_tokens)) {
    state$remaining_tokens <- remaining_tokens
    state$token_state_observed_at <- now
  }
  if (is.finite(reset_tokens)) {
    state$reset_tokens_s <- reset_tokens
    state$token_state_observed_at <- now
  }
  if (is.finite(remaining_requests)) {
    state$remaining_requests <- remaining_requests
    state$request_state_observed_at <- now
  }
  if (is.finite(reset_requests)) {
    state$reset_requests_s <- reset_requests
    state$request_state_observed_at <- now
  }

  status <- tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_)
  if (is.finite(status) && status %in% c(408L, 425L, 429L, 500L, 502L, 503L, 504L)) {
    state$retryable_responses <- as.integer(state$retryable_responses %||% 0L) + 1L
  }
  if (identical(as.integer(status), 429L)) {
    state$rate_limit_responses <- as.integer(state$rate_limit_responses %||% 0L) + 1L
  }
  invisible(NULL)
}


.semantica_reasoning_tokens_from_usage <- function(usage) {
  if (!is.list(usage)) return(NA_real_)
  candidates <- list(
    usage$completion_tokens_details$reasoning_tokens,
    usage$output_tokens_details$reasoning_tokens,
    usage$reasoning_tokens,
    usage$completion_details$reasoning_tokens
  )
  vals <- suppressWarnings(as.numeric(unlist(candidates, recursive = TRUE, use.names = FALSE)))
  vals <- vals[is.finite(vals) & vals >= 0]
  if (length(vals)) max(vals) else NA_real_
}

.semantica_record_reasoning_evidence <- function(session, reasoning_tokens) {
  state <- .semantica_rate_state(session)
  value <- suppressWarnings(as.numeric(reasoning_tokens))
  value <- if (length(value)) value[[1L]] else NA_real_
  if (is.null(state) || !is.finite(value) || value <= 0) return(invisible(FALSE))
  previous <- suppressWarnings(as.numeric(state$observed_reasoning_tokens %||% NA_real_))
  if (!is.finite(previous) || value > previous) state$observed_reasoning_tokens <- value
  state$reasoning_evidence_count <- as.integer(state$reasoning_evidence_count %||% 0L) + 1L
  invisible(TRUE)
}

.semantica_update_usage_state <- function(session, usage, request_chars = NA_real_) {
  state <- .semantica_rate_state(session)
  if (is.null(state) || !is.list(usage)) return(invisible(NULL))
  .semantica_record_reasoning_evidence(session, .semantica_reasoning_tokens_from_usage(usage))
  prompt_tokens <- suppressWarnings(as.numeric(usage$prompt_tokens %||% usage$input_tokens %||% NA_real_))
  request_chars <- suppressWarnings(as.numeric(request_chars[[1L]]))
  if (is.finite(prompt_tokens) && prompt_tokens > 0 && is.finite(request_chars) && request_chars > 0) {
    ratio <- request_chars / prompt_tokens
    if (is.finite(ratio) && ratio > 0) state$chars_per_prompt_token <- ratio
  }
  invisible(NULL)
}

.semantica_estimate_chat_request_tokens <- function(session, messages, max_completion_tokens) {
  max_completion_tokens <- suppressWarnings(as.numeric(max_completion_tokens[[1L]]))
  if (!is.finite(max_completion_tokens) || max_completion_tokens < 0) max_completion_tokens <- 0
  state <- .semantica_rate_state(session)
  ratio <- if (is.null(state)) NA_real_ else suppressWarnings(as.numeric(state$chars_per_prompt_token %||% NA_real_))
  contents <- vapply(messages, function(x) as.character(x$content %||% ""), character(1L))
  chars <- sum(nchar(contents, type = "chars"), na.rm = TRUE)
  prompt_est <- if (is.finite(ratio) && ratio > 0 && chars > 0) ceiling(chars / ratio) else 0
  as.numeric(prompt_est + max_completion_tokens)
}

.semantica_rate_limit_plan <- function(session, estimated_tokens = NA_real_, now = Sys.time()) {
  now_num <- suppressWarnings(as.numeric(now))
  if (!is.finite(now_num)) now_num <- as.numeric(Sys.time())
  state <- .semantica_rate_state(session)
  spacing <- .semantica_default_request_spacing_s(session)
  spacing_wait <- 0
  token_wait <- 0

  if (!is.null(state)) {
    last <- suppressWarnings(as.numeric(state$last_request_at %||% NA_real_))
    if (is.finite(last) && is.finite(spacing) && spacing > 0) {
      spacing_wait <- max(0, spacing - (now_num - last))
    }

    remaining <- suppressWarnings(as.numeric(state$remaining_tokens %||% NA_real_))
    estimated_tokens <- suppressWarnings(as.numeric(estimated_tokens[[1L]]))
    observed <- suppressWarnings(as.numeric(state$token_state_observed_at %||% NA_real_))
    reset_s <- suppressWarnings(as.numeric(state$reset_tokens_s %||% NA_real_))
    if (is.finite(remaining) && is.finite(estimated_tokens) && estimated_tokens > remaining &&
        is.finite(observed) && is.finite(reset_s) && reset_s > 0) {
      token_wait <- max(0, reset_s - (now_num - observed))
    }
  }

  reasons <- character(0L)
  if (spacing_wait > 0) reasons <- c(reasons, "request spacing")
  if (token_wait > 0) reasons <- c(reasons, "token budget")
  list(
    wait_s = max(spacing_wait, token_wait),
    spacing_wait_s = spacing_wait,
    token_wait_s = token_wait,
    reasons = reasons,
    estimated_tokens = estimated_tokens
  )
}

# Alias kept internal so tests/callers can describe the helper by purpose.
.semantica_rate_limit_wait_plan <- .semantica_rate_limit_plan

.semantica_rate_limit_delay_s <- function(session = NULL, estimated_tokens = NA_real_,
                                           requested_tokens = NULL, now = Sys.time(),
                                           state = NULL, remaining_tokens = NA_real_,
                                           reset_tokens_s = NA_real_, observed_at = NULL,
                                           headers = NULL, ...) {
  dots <- list(...)
  if (!is.null(requested_tokens)) estimated_tokens <- requested_tokens
  if (!is.null(dots$completion_budget)) estimated_tokens <- dots$completion_budget
  if (!is.null(dots$estimated_total_tokens)) estimated_tokens <- dots$estimated_total_tokens
  if (!is.null(dots$tokens_needed)) estimated_tokens <- dots$tokens_needed
  if (!is.null(dots$needed_tokens)) estimated_tokens <- dots$needed_tokens

  header_value <- function(x, keys) {
    if (is.null(x)) return(NULL)
    if (is.environment(x)) x <- as.list(x)
    if (!is.list(x)) return(NULL)
    nms <- tolower(gsub("_", "-", base::names(x), fixed = TRUE))
    for (nm in tolower(keys)) {
      i <- match(nm, nms)
      if (!is.na(i)) return(x[[i]])
    }
    NULL
  }
  if (!is.null(headers)) {
    rem <- header_value(headers, c("x-ratelimit-remaining-tokens", "remaining-tokens"))
    rst <- header_value(headers, c("x-ratelimit-reset-tokens", "reset-tokens"))
    if (!is.null(rem)) remaining_tokens <- rem
    if (!is.null(rst)) {
      parsed_reset <- .semantica_parse_wait_s(rst)
      reset_tokens_s <- if (is.finite(parsed_reset)) parsed_reset else rst
    }
    if (is.null(observed_at)) observed_at <- now
  }

  # Normal path: use the same stateful plan as the guard. This consumes the
  # provider-observed remaining/reset headers and never invents a fixed TPM cap.
  if (!is.null(session)) {
    return(as.numeric(.semantica_rate_limit_plan(
      session, estimated_tokens = estimated_tokens, now = now
    )$wait_s))
  }

  # Lightweight explicit-state path is useful for tests/adapters that already
  # extracted headers but do not have a full semantica_session.
  if (is.environment(state) || is.list(state)) {
    remaining_tokens <- state$remaining_tokens %||% remaining_tokens
    reset_tokens_s <- state$reset_tokens_s %||% reset_tokens_s
    observed_at <- state$token_state_observed_at %||% observed_at
  }
  est <- suppressWarnings(as.numeric(estimated_tokens))
  est <- if (length(est)) est[[1L]] else NA_real_
  remaining <- suppressWarnings(as.numeric(remaining_tokens))
  remaining <- if (length(remaining)) remaining[[1L]] else NA_real_
  reset_s <- suppressWarnings(as.numeric(reset_tokens_s))
  reset_s <- if (length(reset_s)) reset_s[[1L]] else NA_real_
  now_num <- suppressWarnings(as.numeric(now))
  if (!is.finite(now_num)) now_num <- as.numeric(Sys.time())
  obs <- if (is.null(observed_at)) now_num else {
    x <- suppressWarnings(as.numeric(observed_at))
    if (length(x)) x[[1L]] else NA_real_
  }
  if (!is.finite(obs)) obs <- now_num
  if (is.finite(est) && is.finite(remaining) && est > remaining &&
      is.finite(reset_s) && reset_s > 0) {
    return(max(0, reset_s - (now_num - obs)))
  }
  0
}

.semantica_rate_limit_guard <- function(session, estimated_tokens = NA_real_, verbose = FALSE) {
  state <- .semantica_rate_state(session)
  plan <- .semantica_rate_limit_plan(session, estimated_tokens = estimated_tokens)
  if (is.finite(plan$wait_s) && plan$wait_s > 0) {
    if (isTRUE(verbose)) {
      reason <- if (length(plan$reasons)) paste(plan$reasons, collapse = " + ") else "provider budget"
      cat(sprintf("SEMANTICA rate-limit guard: waiting %.1fs (%s).\n", plan$wait_s, reason))
    }
    Sys.sleep(plan$wait_s)
    if (!is.null(state)) {
      state$wait_count <- as.integer(state$wait_count %||% 0L) + 1L
      state$total_wait_s <- as.numeric(state$total_wait_s %||% 0) + plan$wait_s
      if (plan$token_wait_s > 0) {
        limit_tokens <- suppressWarnings(as.numeric(state$limit_tokens %||% NA_real_))
        state$remaining_tokens <- if (is.finite(limit_tokens)) limit_tokens else NA_real_
        state$reset_tokens_s <- NA_real_
        state$token_state_observed_at <- as.numeric(Sys.time())
      }
    }
  }
  if (!is.null(state)) state$last_request_at <- as.numeric(Sys.time())
  invisible(plan)
}

.semantica_rate_limit_snapshot <- function(session) {
  state <- .semantica_rate_state(session)
  if (is.null(state)) return(NULL)
  list(
    limit_tokens = as.numeric(state$limit_tokens %||% NA_real_),
    remaining_tokens = as.numeric(state$remaining_tokens %||% NA_real_),
    reset_tokens_s = as.numeric(state$reset_tokens_s %||% NA_real_),
    observed_reasoning_tokens = as.numeric(state$observed_reasoning_tokens %||% NA_real_),
    reasoning_evidence_count = as.integer(state$reasoning_evidence_count %||% 0L),
    retryable_responses = as.integer(state$retryable_responses %||% 0L),
    rate_limit_responses = as.integer(state$rate_limit_responses %||% 0L),
    wait_count = as.integer(state$wait_count %||% 0L),
    total_wait_s = as.numeric(state$total_wait_s %||% 0)
  )
}

.semantica_merge_system_into_user <- function(messages, system_prompt) {
  if (is.null(system_prompt) || !nzchar(trimws(as.character(system_prompt)))) return(messages)
  out <- messages
  user_idx <- which(vapply(out, function(x) identical(as.character(x$role %||% ""), "user"), logical(1L)))
  if (length(user_idx)) {
    i <- user_idx[[1L]]
    out[[i]]$content <- paste(
      "INSTRUCTIONS", as.character(system_prompt),
      "TASK", as.character(out[[i]]$content %||% ""),
      sep = "\n\n"
    )
  } else {
    out <- c(list(list(role = "user", content = as.character(system_prompt))), out)
  }
  out
}


.semantica_openai_chat_body <- function(session, messages, max_tokens = 2048L,
                                         temperature = 0.7, system_prompt = NULL,
                                         response_format = NULL, seed = NULL, ...) {
  dots <- list(...)
  if (!is.null(dots$completion_budget)) max_tokens <- dots$completion_budget
  if (!is.null(dots$max_completion_tokens)) max_tokens <- dots$max_completion_tokens
  if (is.null(response_format)) {
    structured <- dots$structured_output %||% dots$output_mode
    if (isTRUE(structured) || identical(structured, "json")) response_format <- "json"
  }
  profile <- .semantica_reasoning_model_profile(session)
  msgs <- messages
  if (!is.null(system_prompt)) {
    if (isTRUE(profile$prefer_user_prompt)) {
      msgs <- .semantica_merge_system_into_user(msgs, system_prompt)
    } else {
      msgs <- c(list(list(role = "system", content = system_prompt)), msgs)
    }
  }

  completion_budget <- .semantica_reasoning_completion_budget(session, max_tokens)
  body <- list(model = session$chat_model, messages = msgs, temperature = temperature)
  body[[profile$completion_field %||% "max_tokens"]] <- completion_budget

  # Provider/model-specific controls are present only when the profile declares
  # an explicit transport contract. Generic reasoning detection never adds them.
  if (!is.null(profile$reasoning_effort)) body$reasoning_effort <- profile$reasoning_effort
  if (!is.null(profile$include_reasoning)) body$include_reasoning <- isTRUE(profile$include_reasoning)

  format_spec <- if (is.list(response_format)) {
    response_format
  } else {
    .semantica_chat_response_format(session, response_format)
  }
  proto <- as.character(session$protocol %||% "openai_compat")
  if (!is.null(format_spec)) {
    if (identical(proto, "ollama")) {
      # Ollama's native /api/chat contract uses `format`, not OpenAI's
      # `response_format`. Keep the native field even when structured output is
      # requested through SEMANTICA's provider-neutral API.
      body$format <- "json"
    } else {
      body$response_format <- format_spec
    }
  }
  if (identical(proto, "ollama")) {
    body$stream <- FALSE
    body$options <- list(temperature = temperature, num_predict = completion_budget)
    seed <- .semantica_normalize_generation_seed(seed)
    if (!is.null(seed)) body$options$seed <- seed
    body$max_tokens <- NULL
    body$max_completion_tokens <- NULL
  }
  body
}

.semantica_generation_failure_summary <- function(attempts) {
  if (is.null(attempts) || !length(attempts)) return("")
  parts <- vapply(seq_along(attempts), function(i) {
    x <- attempts[[i]]
    reasons <- as.character(x$rejection_reasons %||% character(0L))
    reasons <- reasons[nzchar(reasons)]
    backend_error <- as.character(x$backend_error %||% "")
    if (length(backend_error) > 1L) backend_error <- backend_error[[1L]]
    fields <- sprintf(
      "attempt %d: requested=%d, parsed=%d, retained=%d",
      as.integer(x$attempt %||% i),
      as.integer(x$requested %||% 0L),
      as.integer(x$parsed %||% 0L),
      as.integer(x$newly_retained %||% x$retained %||% 0L)
    )
    if (length(reasons)) fields <- paste0(fields, ", reasons=", paste(reasons, collapse = "+"))
    if (length(backend_error) && !is.na(backend_error) && nzchar(backend_error)) {
      fields <- paste0(fields, ", backend_error=", backend_error)
    }
    fields
  }, character(1L))
  paste(parts, collapse = "; ")
}
