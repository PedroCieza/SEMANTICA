# Credential-safe metadata helpers for returned SEMANTICA objects.

.semantica_security_or <- function(x, default) {
  if (!is.null(x) && length(x) > 0L) x else default
}

.semantica_sensitive_field_name <- function(name) {
  if (is.null(name) || length(name) != 1L || is.na(name)) return(FALSE)
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(name)))
  normalized %in% c(
    "apikey", "embedapikey", "hftoken", "huggingfacetoken",
    "token", "accesstoken", "refreshtoken", "authtoken", "bearertoken",
    "authorization", "authorizationheader", "credentials", "credential",
    "secret", "clientsecret", "password", "passwd", "privatekey"
  ) || grepl(
    "(apikey|token|authorization|credential|secret|password|privatekey)$",
    normalized
  )
}

.semantica_url_field_name <- function(name) {
  if (is.null(name) || length(name) != 1L || is.na(name)) return(FALSE)
  grepl(
    "(url|uri|endpoint)$",
    gsub("[^a-z0-9]", "", tolower(as.character(name)))
  )
}

.semantica_sanitize_url <- function(url) {
  if (is.null(url)) return(NULL)
  if (!is.character(url)) return(url)
  out <- vapply(url, function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    value <- sub("[?#].*$", "", value, perl = TRUE)
    # Strip URL user information while preserving scheme, host, and path.
    sub(
      "^([A-Za-z][A-Za-z0-9+.-]*://)[^/@]*@",
      "\\1",
      value,
      perl = TRUE
    )
  }, character(1L), USE.NAMES = FALSE)
  if (length(out) == 1L) out[[1L]] else out
}

.semantica_sanitize_named_atomic <- function(x) {
  if (is.object(x)) return(x)
  nms <- names(x)
  if (is.null(nms) || length(x) == 0L) return(x)
  keep <- !vapply(nms, function(name) {
    !is.na(name) && nzchar(name) && .semantica_sensitive_field_name(name)
  }, logical(1L))
  out <- x[keep]
  out_names <- names(out)
  if (is.character(out) && length(out) > 0L && !is.null(out_names)) {
    for (i in seq_along(out)) {
      name <- out_names[[i]]
      if (!is.na(name) && nzchar(name) && .semantica_url_field_name(name)) {
        out[[i]] <- .semantica_sanitize_url(out[[i]])
      }
    }
  }
  out
}

.semantica_is_session_object <- function(x) {
  nms <- if (is.list(x)) names(x) else NULL
  inherits(x, "semantica_session") ||
    (is.list(x) &&
       "protocol" %in% nms &&
       ("backend" %in% nms || "provider" %in% nms) &&
       any(c("api_key", "hf_token", "chat_model", "embed_model") %in% nms))
}

.semantica_sanitize_metadata_value <- function(x, depth = 0L) {
  if (depth > 20L) return("<metadata depth omitted>")
  if (is.null(x)) return(x)
  if (is.atomic(x)) return(.semantica_sanitize_named_atomic(x))
  if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") {
    return("<non-serializable metadata omitted>")
  }
  if (is.list(x)) {
    nms <- names(x)
    out <- vector("list", 0L)
    out_names <- character(0L)
    for (i in seq_along(x)) {
      nm <- if (!is.null(nms)) nms[[i]] else ""
      if (nzchar(nm) && .semantica_sensitive_field_name(nm)) next
      value <- .semantica_sanitize_metadata_value(x[[i]], depth = depth + 1L)
      if (nzchar(nm) && .semantica_url_field_name(nm)) {
        value <- .semantica_sanitize_url(value)
      }
      # Single-bracket assignment preserves explicit NULL values instead of
      # shortening the list and desynchronizing its names.
      out[length(out) + 1L] <- list(value)
      out_names[[length(out_names) + 1L]] <- nm
    }
    if (!is.null(nms)) names(out) <- out_names
    return(out)
  }
  as.character(x)
}

.semantica_sanitize_configuration <- function(configuration) {
  if (is.null(configuration) || !is.list(configuration)) return(NULL)
  allowed <- c(
    "backend", "provider", "protocol", "model", "chat_model",
    "embed_model", "embedding_model", "device", "embedding_device", "chat_device",
    "resolved_embedding_device", "resolved_chat_device", "device_map",
    "gpu_layers", "model_precision", "gpu_precision", "accelerator",
    "n_ctx", "batch_size", "timeout_s"
  )
  keep <- intersect(names(configuration), allowed)
  if (length(keep) == 0L) return(list())
  out <- configuration[keep]
  for (name in names(out)) {
    out[[name]] <- .semantica_sanitize_metadata_value(out[[name]])
    if (.semantica_url_field_name(name)) {
      out[[name]] <- .semantica_sanitize_url(out[[name]])
    }
  }
  out
}

#' Sanitize a SEMANTICA session before attaching it to a result
#'
#' This helper uses an allow-list. It returns reproducibility-safe metadata and
#' deliberately omits credentials, authorization headers, tokens, and live
#' Python objects. The input session is never modified.
#'
#' @param session A `semantica_session` or session-like list.
#' @return A credential-free `semantica_session_metadata` list.
#' @keywords internal
sanitize_session_for_result <- function(session) {
  if (is.null(session)) return(NULL)
  if (!is.list(session)) {
    stop("'session' must be a session-like list or NULL.", call. = FALSE)
  }

  scalar_fields <- c(
    "backend", "provider", "protocol", "label",
    "chat_model", "embed_model", "embedding_model", "embed_dim",
    "has_embed", "timeout_s", "py_available",
    "embedding_device", "resolved_embedding_device",
    "chat_device", "resolved_chat_device", "device_map",
    "gpu_layers", "gpu_layers_requested", "model_precision",
    "gpu_precision", "accelerator", "device_status",
    "python_version", "python_package_versions", "torch_version",
    "cuda_available", "mps_available"
  )
  url_fields <- c("base_url", "chat_url", "embed_url", "endpoint")
  out <- list()

  for (field in scalar_fields) {
    if (!is.null(session[[field]])) {
      out[[field]] <- .semantica_sanitize_metadata_value(session[[field]])
    }
  }
  for (field in url_fields) {
    if (!is.null(session[[field]])) {
      out[[field]] <- .semantica_sanitize_url(session[[field]])
    }
  }
  if (!is.null(session$gguf_path)) {
    # Full local paths can disclose usernames or private directory layouts.
    out$gguf_file <- basename(as.character(session$gguf_path[[1L]]))
  }
  if (!is.null(session$configuration)) {
    out$configuration <- .semantica_sanitize_configuration(
      session$configuration
    )
  }

  class(out) <- c("semantica_session_metadata", "list")
  out
}

.semantica_sanitize_result_for_serialization <- function(x, depth = 0L) {
  if (depth > 100L) return("<result depth omitted>")
  if (.semantica_is_session_object(x)) {
    return(sanitize_session_for_result(x))
  }
  if (is.null(x)) return(x)
  if (is.atomic(x)) return(.semantica_sanitize_named_atomic(x))
  if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") {
    return("<non-serializable result component omitted>")
  }

  # Preserve complex third-party S3 objects (for example ggplot widgets), since
  # recursively rebuilding them would corrupt their semantics. Credential
  # guarantees therefore apply to SEMANTICA sessions, known result/plain-list
  # fields, and named atomic fields, not payloads deliberately hidden inside an
  # opaque third-party object.
  classes <- class(x)
  if (length(classes) > 0L &&
      !identical(classes, "list") &&
      !any(classes %in% c(
        "semantica_result", "semantica_pipeline_result",
        "semantica_full_pipeline_result"
      ))) {
    return(x)
  }
  if (!is.list(x)) return(x)

  nms <- names(x)
  out <- vector("list", 0L)
  out_names <- character(0L)
  for (i in seq_along(x)) {
    nm <- if (!is.null(nms)) nms[[i]] else ""
    if (nzchar(nm) && .semantica_sensitive_field_name(nm)) next

    value <- x[[i]]
    if (nzchar(nm) && nm %in% c("session", "embed_session") &&
        is.list(value)) {
      value <- sanitize_session_for_result(value)
    } else {
      value <- .semantica_sanitize_result_for_serialization(
        value,
        depth = depth + 1L
      )
    }
    if (nzchar(nm) && .semantica_url_field_name(nm)) {
      value <- .semantica_sanitize_url(value)
    }
    # Single-bracket assignment preserves explicit NULL values instead of
    # shortening the list and desynchronizing its names.
    out[length(out) + 1L] <- list(value)
    out_names[[length(out_names) + 1L]] <- nm
  }
  if (!is.null(nms)) names(out) <- out_names
  # Only restore SEMANTICA's known result classes. Arbitrary attributes can
  # themselves contain environments or credentials, so they are deliberately
  # not copied from an untrusted nested object.
  safe_classes <- classes[classes %in% c(
    "semantica_result", "semantica_pipeline_result",
    "semantica_full_pipeline_result", "list"
  )]
  if (length(safe_classes) > 0L) class(out) <- safe_classes
  out
}

#' Sanitize SEMANTICA result sessions for safe serialization
#'
#' Recursively replaces SEMANTICA sessions with allow-listed metadata and
#' removes explicitly named credential fields from plain result lists and named
#' atomic values. Opaque third-party S3 objects are preserved to avoid corrupting
#' them and must not be used to carry credentials. The input object is not
#' modified.
#'
#' @param result A SEMANTICA result or nested plain list.
#' @return A credential-sanitized copy.
#' @keywords internal
sanitize_result_for_serialization <- function(result) {
  .semantica_sanitize_result_for_serialization(result)
}
