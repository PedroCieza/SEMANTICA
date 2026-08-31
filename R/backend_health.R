# Provider capability and health checks.

.semantica_add_auth_headers <- function(req, session) {
  if (!is.null(session$auth_header) && !is.null(session$api_key)) {
    req <- if (identical(session$auth_header, "Bearer")) {
      httr2::req_auth_bearer_token(req, session$api_key)
    } else {
      do.call(httr2::req_headers, c(list(req), stats::setNames(list(session$api_key), session$auth_header)))
    }
  }
  if (!is.null(session$extra_headers) && length(session$extra_headers)) {
    req <- do.call(httr2::req_headers, c(list(req), session$extra_headers))
  }
  req
}

.semantica_get_json <- function(session, url, timeout_s = 10L) {
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_retry(
      max_tries = 3L,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(408L, 425L, 429L, 500L, 502L, 503L, 504L)
    )
  req <- .semantica_add_auth_headers(req, session)
  resp <- httr2::req_error(req, is_error = function(r) FALSE) |> httr2::req_perform()
  list(status = httr2::resp_status(resp), body = tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL))
}

#' Check backend availability and declared capabilities
#'
#' The preflight avoids expensive generation when a model is unavailable or an
#' embedding backend is not supported. It is diagnostic: remote providers can
#' still change after the check, so actual requests remain authoritative. For
#' Ollama, one transport-level registry failure is confirmed once before a
#' warning is returned, and any persistent probe error is preserved in the
#' diagnostic instead of being collapsed into a generic reachability message.
#'
#' @param session A `semantica_session`.
#' @param verify_models Check provider/local model registries when available.
#' @param strict Stop on preflight failures instead of returning diagnostics.
#' @param timeout_s Network timeout for the check.
#' @return A `semantica_backend_preflight` list.
#' @export
semantica_backend_preflight <- function(session, verify_models = TRUE,
                                        strict = FALSE, timeout_s = 10L) {
  if (!inherits(session, "semantica_session")) stop("'session' must be a semantica_session.")
  purpose <- session$purpose %||% "both"
  need_chat <- purpose %in% c("both", "chat")
  need_embed <- purpose %in% c("both", "embed")
  out <- list(
    backend = session$backend, purpose = purpose,
    protocol = session$protocol,
    reachable = NA,
    chat_model = session$chat_model,
    chat_model_available = NA,
    embed_model = session$embed_model,
    embed_model_available = if (isTRUE(session$has_embed)) NA else FALSE,
    has_embed = isTRUE(session$has_embed),
    supports_structured_output = isTRUE(session$supports_structured_output),
    model_ids = character(0L),
    warnings = character(0L)
  )

  fail <- function(msg) {
    if (isTRUE(strict)) stop(msg, call. = FALSE)
    out$warnings <<- c(out$warnings, msg)
  }
  if (need_embed && !isTRUE(session$has_embed)) {
    fail(paste0("Backend '", session$backend, "' does not provide embeddings for an embedding-purpose session."))
  }

  if (session$protocol %in% c("python_hf", "python_llamacpp")) {
    ok <- tryCatch(.ping_backend(session), error = function(e) FALSE)
    out$reachable <- isTRUE(ok)
    out$chat_model_available <- if (need_chat) isTRUE(ok) else NA
    out$embed_model_available <- if (need_embed) isTRUE(ok) && isTRUE(session$has_embed) else NA
    if (!isTRUE(ok)) fail("Local Python backend preflight failed.")
  } else if (identical(session$protocol, "ollama")) {
    registry_endpoint <- session$chat_url %||% session$embed_url
    tags_url <- sub("/api/(chat|embed|embeddings)$", "/api/tags", registry_endpoint)
    safe_tags_url <- if (exists(".semantica_sanitize_url", mode = "function")) {
      .semantica_sanitize_url(tags_url)
    } else {
      tags_url
    }

    probe_error <- NULL
    res <- tryCatch(
      .semantica_get_json(session, tags_url, timeout_s),
      error = function(e) {
        probe_error <<- conditionMessage(e)
        NULL
      }
    )

    # A local registry probe can fail transiently while the Ollama service is
    # otherwise healthy. Confirm one transport failure before reporting it.
    out$probe_recovered_after_retry <- FALSE
    if (is.null(res) && !is.null(probe_error)) {
      first_probe_error <- probe_error
      Sys.sleep(0.15)
      probe_error <- NULL
      res <- tryCatch(
        .semantica_get_json(session, tags_url, timeout_s),
        error = function(e) {
          probe_error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(res) && res$status < 400L) {
        out$probe_recovered_after_retry <- TRUE
        out$transient_probe_error <- first_probe_error
      }
    }

    out$registry_endpoint <- safe_tags_url
    out$reachable <- !is.null(res) && res$status < 400L
    if (isTRUE(out$reachable) && isTRUE(verify_models)) {
      models <- res$body$models %||% list()
      ids <- vapply(models, function(x) as.character(x$name %||% x$model %||% ""), character(1L))
      ids <- ids[nzchar(ids)]
      out$model_ids <- ids
      matches <- function(model) {
        if (is.null(model) || !nzchar(model)) return(NA)
        model %in% ids || paste0(model, ":latest") %in% ids || sub(":latest$", "", model) %in% sub(":latest$", "", ids)
      }
      out$chat_model_available <- if (need_chat) matches(session$chat_model) else NA
      if (isTRUE(session$has_embed) && need_embed) out$embed_model_available <- matches(session$embed_model) else if (!need_embed) out$embed_model_available <- NA
      if (need_chat && identical(out$chat_model_available, FALSE)) fail(paste0("Ollama chat model is not installed: ", session$chat_model))
      if (need_embed && identical(out$embed_model_available, FALSE)) fail(paste0("Ollama embedding model is not installed: ", session$embed_model))
    }
    if (!isTRUE(out$reachable)) {
      out$probe_error <- probe_error
      if (!is.null(res)) {
        fail(sprintf(
          "Ollama model-registry preflight returned HTTP %d at %s. Actual Ollama requests remain authoritative.",
          res$status, safe_tags_url
        ))
      } else {
        detail <- if (!is.null(probe_error) && nzchar(probe_error)) paste0(": ", probe_error) else ""
        fail(paste0(
          "Ollama model-registry preflight could not confirm availability at ",
          safe_tags_url, detail,
          ". Actual Ollama requests remain authoritative."
        ))
      }
    }
  } else if (identical(session$protocol, "openai_compat")) {
    registry_endpoint <- session$chat_url %||% session$embed_url
    models_url <- sub("/(chat/completions|embeddings)$", "/models", registry_endpoint)
    res <- tryCatch(.semantica_get_json(session, models_url, timeout_s), error = function(e) NULL)
    out$reachable <- !is.null(res) && res$status < 400L
    if (isTRUE(out$reachable) && isTRUE(verify_models)) {
      data <- res$body$data %||% list()
      ids <- vapply(data, function(x) as.character(x$id %||% ""), character(1L))
      ids <- ids[nzchar(ids)]
      out$model_ids <- ids
      if (length(ids)) {
        out$chat_model_available <- if (!need_chat || is.null(session$chat_model)) NA else session$chat_model %in% ids
        if (need_embed && isTRUE(session$has_embed) && !is.null(session$embed_model)) {
          # Most /models endpoints do not expose per-model embedding capability,
          # but absence of the configured model is still actionable.
          out$embed_model_available <- session$embed_model %in% ids
        }
      }
      if (need_chat && identical(out$chat_model_available, FALSE)) fail(paste0("Configured chat model was not returned by the provider: ", session$chat_model))
      if (need_embed && identical(out$embed_model_available, FALSE)) fail(paste0("Configured embedding model was not returned by the provider: ", session$embed_model))
    }
    if (!isTRUE(out$reachable)) fail("OpenAI-compatible model registry is not reachable; the real request may still succeed on custom servers.")
  } else {
    ok <- tryCatch(.ping_backend(session), error = function(e) FALSE)
    out$reachable <- isTRUE(ok)
    out$chat_model_available <- if (isTRUE(ok)) NA else FALSE
    if (!isTRUE(ok)) fail("Backend connection probe failed.")
  }

  out$ok <- isTRUE(out$reachable) &&
    (!need_chat || !identical(out$chat_model_available, FALSE)) &&
    (!need_embed || (isTRUE(out$has_embed) && !identical(out$embed_model_available, FALSE)))
  class(out) <- c("semantica_backend_preflight", "list")
  out
}

#' @export
print.semantica_backend_preflight <- function(x, ...) {
  cat(sprintf("<semantica_backend_preflight> %s | reachable: %s | ok: %s\n",
              x$backend %||% "?", as.character(x$reachable), as.character(x$ok)))
  cat(sprintf("  chat model : %s [%s]\n", x$chat_model %||% "(none)", as.character(x$chat_model_available)))
  cat(sprintf("  embed model: %s [%s]\n", x$embed_model %||% "(none)", as.character(x$embed_model_available)))
  if (length(x$warnings)) for (w in x$warnings) cat("  warning    : ", w, "\n", sep = "")
  invisible(x)
}
