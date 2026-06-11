# SEMANTICA item generation and embedding helpers.

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

# Avoid R CMD check notes for non-standard evaluation
utils::globalVariables(c("item_id", "factor", "item_text", "attempt", "ID", "Dimension", "Facet", "item"))

# =================================================================
# P0  CONDA / PYTHON ENVIRONMENT
# =================================================================
#' Bootstrap a Conda environment for SEMANTICA Python backends
#'
#' @param env_name   Name of the Conda environment (default `"semantica"`).
#' @param conda      Path to the `conda` executable. `NULL` = auto-detect.
#' @param python_ver Python version string (default `"3.11"`).
#' @param packages   Character vector of additional `pip` packages to install.
#' @param force      Recreate the environment even if it already exists.
#' @param verbose    Print progress messages.
#'
#' @details
#' Core packages always installed: `torch` (CPU), `transformers`,
#' `sentence-transformers`, `accelerate`, `einops`, `llama-cpp-python`.
#' After calling this function, restart your R session and call
#' `semantica_activate_conda()` to attach the environment.
#'
#' @return Invisibly returns the environment name.
#' @export
#' @examples
#' \dontrun{
#' semantica_setup_conda(
#'   env_name = "semantica",
#'   python_ver = "3.11",
#'   packages = character(0L),
#'   force = FALSE
#' )
#' }
semantica_setup_conda <- function(env_name = "semantica", conda = NULL,
                                  python_ver = "3.11", packages = character(0L),
                                  force = FALSE, verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install with: install.packages('reticulate')")
  }
  conda_bin <- if (!is.null(conda)) conda else tryCatch(
    reticulate::conda_binary(),
    error = function(e) stop("No conda executable found.\n  Install Miniconda with: reticulate::install_miniconda()\n  Or pass conda='/path/to/conda' explicitly.")
  )

  if (verbose) {
    cat("============================================================\n")
    cat("STEP 0 -- CONDA ENVIRONMENT SETUP\n")
    cat("============================================================\n")
    cat(sprintf("  Environment : %s\n  Python      : %s\n  Conda       : %s\n", env_name, python_ver, conda_bin))
  }

  existing <- tryCatch(reticulate::conda_list(conda = conda_bin), error = function(e) NULL)
  env_exists <- !is.null(existing) && env_name %in% existing$name

  if (env_exists && !force) {
    if (verbose) cat(sprintf("  Status      : environment '%s' already exists (skipping creation).\n", env_name))
  } else {
    if (env_exists && force) {
      if (verbose) cat("  Removing existing environment...\n")
      reticulate::conda_remove(envname = env_name, conda = conda_bin)
    }
    if (verbose) cat("  Creating environment...\n")
    reticulate::conda_create(envname = env_name, packages = paste0("python=", python_ver), conda = conda_bin)
    if (verbose) cat("  Environment created.\n")
  }

  core_pkgs <- c("torch", "transformers", "sentence-transformers", "accelerate", "einops", "llama-cpp-python", "numpy", "scipy")
  all_pkgs <- unique(c(core_pkgs, packages))
  if (verbose) cat(sprintf("  Installing %d packages (pip)...\n", length(all_pkgs)))

  tryCatch(
    reticulate::conda_install(envname = env_name, packages = all_pkgs, pip = TRUE, conda = conda_bin),
    error = function(e) warning("Package installation reported an error: ", e$message, "\n  Some packages may still have installed correctly.")
  )

  if (verbose) {
    cat("  Installation complete.\n")
    cat(sprintf("  NEXT STEPS:\n  1. Restart your R session.\n  2. Call: semantica_activate_conda('%s')\n  3. Then run your pipeline.\n", env_name))
    cat("============================================================\n\n")
  }
  invisible(env_name)
}

#' Activate a Conda environment for use in the current R session
#'
#' @param env_name   Name of the Conda environment.
#' @param conda      Path to `conda` executable. `NULL` = auto-detect.
#' @param verbose    Print confirmation.
#' @return Invisibly returns `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' semantica_activate_conda(env_name = "semantica", conda = NULL, verbose = TRUE)
#' }
semantica_activate_conda <- function(env_name = "semantica", conda = NULL, verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install with: install.packages('reticulate')")
  }
  conda_bin <- if (!is.null(conda)) conda else tryCatch(reticulate::conda_binary(), error = function(e) NULL)
  reticulate::use_condaenv(env_name, conda = conda_bin, required = TRUE)
  if (verbose) {
    py_conf <- reticulate::py_config()
    cat(sprintf("Conda environment '%s' activated.\n  Python: %s\n", env_name, py_conf$python))
  }
  invisible(TRUE)
}

# =================================================================
# P1  BACKEND REGISTRY
# =================================================================
#' Available LLM backends for SEMANTICA
#'
#' @format A named list. Each element contains protocol, endpoints,
#'   default models, and auth settings.
#' @export
#' @examples
#' names(SEMANTICA_BACKENDS)
#' SEMANTICA_BACKENDS$openai$default_embed_model
SEMANTICA_BACKENDS <- list(
  openai = list(label = "OpenAI API", protocol = "openai_compat", chat_url = "https://api.openai.com/v1/chat/completions",
                embed_url = "https://api.openai.com/v1/embeddings", default_chat_model = "gpt-4o", default_embed_model = "text-embedding-3-small",
                embed_dim = 1536L, auth_header = "Bearer", auth_env = "OPENAI_API_KEY", extra_headers = NULL, has_embed = TRUE),
  anthropic = list(label = "Anthropic API (Claude)", protocol = "anthropic", chat_url = "https://api.anthropic.com/v1/messages",
                   embed_url = NULL, default_chat_model = "claude-opus-4-5", default_embed_model = NULL, embed_dim = NA,
                   auth_header = "x-api-key", auth_env = "ANTHROPIC_API_KEY", extra_headers = list("anthropic-version" = "2023-06-01"), has_embed = FALSE),
  groq = list(label = "Groq API", protocol = "openai_compat", chat_url = "https://api.groq.com/openai/v1/chat/completions",
              embed_url = "https://api.groq.com/openai/v1/embeddings", default_chat_model = "llama-3.3-70b-versatile",
              default_embed_model = "nomic-embed-text-v1.5", embed_dim = 768L, auth_header = "Bearer", auth_env = "GROQ_API_KEY",
              extra_headers = NULL, has_embed = TRUE),
  ollama = list(label = "Ollama (local)", protocol = "ollama", chat_url = "http://localhost:11434/api/chat",
                embed_url = "http://localhost:11434/api/embeddings", default_chat_model = "llama3.2", default_embed_model = "nomic-embed-text",
                embed_dim = 768L, auth_header = NULL, auth_env = NULL, extra_headers = NULL, has_embed = TRUE),
  llamacpp = list(label = "llama.cpp server", protocol = "openai_compat", chat_url = "http://localhost:8080/v1/chat/completions",
                  embed_url = "http://localhost:8080/v1/embeddings", default_chat_model = "local-model", default_embed_model = "local-model",
                  embed_dim = NA, auth_header = NULL, auth_env = NULL, extra_headers = NULL, has_embed = TRUE),
  generic_openai = list(label = "Generic OpenAI-compatible", protocol = "openai_compat", chat_url = "http://localhost:1234/v1/chat/completions",
                        embed_url = "http://localhost:1234/v1/embeddings", default_chat_model = "local-model", default_embed_model = "local-model",
                        embed_dim = NA, auth_header = NULL, auth_env = NULL, extra_headers = NULL, has_embed = TRUE),
  python_hf = list(label = "HuggingFace Transformers (Conda)", protocol = "python_hf", chat_url = NULL, embed_url = NULL,
                   default_chat_model = "meta-llama/Llama-3.2-1B-Instruct", default_embed_model = "sentence-transformers/all-MiniLM-L6-v2",
                   embed_dim = 384L, auth_header = NULL, auth_env = "HF_TOKEN", extra_headers = NULL, has_embed = TRUE),
  python_llamacpp = list(label = "llama-cpp-python (GGUF)", protocol = "python_llamacpp", chat_url = NULL, embed_url = NULL,
                         default_chat_model = NULL, default_embed_model = NULL, embed_dim = NA, auth_header = NULL, auth_env = NULL,
                         extra_headers = NULL, has_embed = TRUE)
)

.canonicalize_embedding_model <- function(model) {
  if (is.null(model) || length(model) == 0L) return(model)
  model <- as.character(model[[1L]])
  aliases <- c(
    text_embedding_3_small = "text-embedding-3-small",
    text_embedding_3_large = "text-embedding-3-large",
    text_embeddings_3_small = "text-embedding-3-small",
    text_embeddings_3_large = "text-embedding-3-large",
    text_embedding_ada_002 = "text-embedding-ada-002"
  )
  key <- tolower(trimws(model))
  if (key %in% names(aliases)) aliases[[key]] else model
}

.expected_embedding_dim <- function(model) {
  model <- .canonicalize_embedding_model(model)
  if (is.null(model)) return(NA_integer_)
  switch(
    tolower(model),
    "text-embedding-3-small" = 1536L,
    "text-embedding-3-large" = 3072L,
    "text-embedding-ada-002" = 1536L,
    NA_integer_
  )
}

# =================================================================
# P2  CONNECT
# =================================================================
#' Create a SEMANTICA LLM session
#'
#' @param backend      One of the keys in `SEMANTICA_BACKENDS`, or any custom string
#'                     (uses `generic_openai` protocol).
#' @param api_key      API key string. `NULL` = read from environment variable.
#' @param chat_model   Override default chat model name.
#' @param embed_model  Override default embedding model name.
#' @param base_url     Override host:port (required for `generic_openai`).
#' @param gguf_path    Path to a `.gguf` model file (`python_llamacpp` only).
#' @param hf_token     HuggingFace token for gated models (`python_hf` only).
#' @param timeout_s    HTTP timeout in seconds (default `120`).
#' @param verbose      Print connection details and status.
#'
#' @return A `semantica_session` S3 object.
#' @export
#' @examples
#' \dontrun{
#' # Connect to OpenAI (reads OPENAI_API_KEY from env)
#' sess <- semantica_connect("openai", timeout_s = 120L)
#'
#' # Connect to a local LM Studio instance
#' local_session <- semantica_connect(
#'   "generic_openai",
#'   base_url = "http://localhost:1234",
#'   verbose = FALSE
#' )
#' }
semantica_connect <- function(backend = c("openai", "anthropic", "groq", "ollama", "llamacpp", "generic_openai", "python_hf", "python_llamacpp"),
                              api_key = NULL, chat_model = NULL, embed_model = NULL, base_url = NULL, gguf_path = NULL,
                              hf_token = NULL, timeout_s = 120L, verbose = TRUE) {
  if (length(backend) != 1L) backend <- backend[[1L]]
  backend <- as.character(backend)
  if (!nzchar(backend)) stop("'backend' must be a non-empty string.")

  known <- names(SEMANTICA_BACKENDS)
  if (!backend %in% known) {
    message(sprintf("Backend '%s' not in registry -- using generic_openai protocol.", backend))
    spec <- SEMANTICA_BACKENDS[["generic_openai"]]
    spec$label <- paste0("Custom server (", backend, ")")
  } else {
    spec <- SEMANTICA_BACKENDS[[backend]]
  }

  chat_url <- spec$chat_url; embed_url <- spec$embed_url
  if (!is.null(base_url)) {
    base_url <- sub("/$", "", base_url)
    if (!is.null(chat_url)) chat_url <- sub("^https?://[^/]+", base_url, chat_url)
    if (!is.null(embed_url)) embed_url <- sub("^https?://[^/]+", base_url, embed_url)
  }

  key <- api_key
  if (is.null(key) && !is.null(spec$auth_env) && nchar(spec$auth_env) > 0L) {
    key <- Sys.getenv(spec$auth_env, unset = NA_character_)
    if (is.na(key) || nchar(trimws(key)) == 0L) {
      if (backend %in% c("openai", "anthropic", "groq")) {
        stop("No API key found for backend '", backend, "'.\n  Set environment variable ", spec$auth_env, "\n  or pass api_key= directly.")
      }
      key <- NULL
    }
  }

  hf_tok <- hf_token %||% Sys.getenv("HF_TOKEN", unset = NA_character_)
  if (is.na(hf_tok)) hf_tok <- NULL

  cm <- chat_model %||% spec$default_chat_model
  em <- .canonicalize_embedding_model(embed_model %||% spec$default_embed_model)
  py_available <- FALSE
  if (spec$protocol %in% c("python_hf", "python_llamacpp")) {
    if (!requireNamespace("reticulate", quietly = TRUE)) stop("Backend '", backend, "' requires 'reticulate'.")
    py_available <- tryCatch({ reticulate::py_available(initialize = TRUE); TRUE }, error = function(e) FALSE)
    if (!py_available) stop("Python not available. Call semantica_activate_conda() first.")
  }

  session <- list(backend = backend, protocol = spec$protocol, label = spec$label, api_key = key, auth_header = spec$auth_header,
                  extra_headers = spec$extra_headers, chat_url = chat_url, embed_url = embed_url, chat_model = cm, embed_model = em,
                  embed_dim = spec$embed_dim, has_embed = spec$has_embed, timeout_s = timeout_s, gguf_path = gguf_path, hf_token = hf_tok,
                  py_available = py_available, verbose = verbose)
  class(session) <- c("semantica_session", "list")

  if (verbose) {
    cat("\n============================================================\nSEMANTICA PIPELINE v2 -- LLM CONNECTION\n============================================================\n")
    cat(sprintf("  Backend       : %s\n  Protocol      : %s\n  Chat model    : %s\n  Embed model   : %s\n", session$label, session$protocol, cm %||% "(none)", em %||% "(none)"))
    if (!is.null(chat_url)) cat(sprintf("  Chat endpoint : %s\n", chat_url))
    if (!is.null(embed_url)) cat(sprintf("  Embed endpoint: %s\n", embed_url))
  }

  ok <- tryCatch(.ping_backend(session), error = function(e) { if (verbose) message("  WARNING: Connection test failed -- ", e$message); FALSE })
  if (verbose) {
    cat(sprintf("  Status        : %s\n============================================================\n\n", if (isTRUE(ok)) "CONNECTED" else "UNREACHABLE"))
  }
  session
}

#' Print a SEMANTICA backend session
#'
#' @param x A `semantica_session` object returned by [semantica_connect()].
#' @param ... Additional arguments ignored by this method.
#' @return Invisibly returns `x`.
#' @export
#' @examples
#' sess <- structure(
#'   list(backend = "demo", chat_model = "demo-model"),
#'   class = c("semantica_session", "list")
#' )
#' print(sess)
print.semantica_session <- function(x, ...) {
  cat(sprintf("<semantica_session>  backend: %s  model: %s\n", x$backend, x$chat_model %||% "?"))
  invisible(x)
}

# =================================================================
# P3  LOW-LEVEL HELPERS (HTTP + Python dispatch)
# =================================================================
#' @keywords internal
.build_request <- function(session, url, body_list) {
  req <- httr2::request(url) |>
    httr2::req_timeout(session$timeout_s) |>
    httr2::req_body_json(body_list) |>
    httr2::req_headers("Content-Type" = "application/json")
  if (!is.null(session$auth_header) && !is.null(session$api_key)) {
    req <- if (session$auth_header == "Bearer") httr2::req_auth_bearer_token(req, session$api_key) else httr2::req_headers(req, !!session$auth_header := session$api_key)
  }
  if (!is.null(session$extra_headers) && length(session$extra_headers) > 0L) {
    req <- do.call(httr2::req_headers, c(list(req), session$extra_headers))
  }
  req
}

#' @keywords internal
.semantica_request_clock <- new.env(parent = emptyenv())

#' @keywords internal
.semantica_resp_header <- function(resp, name) {
  headers <- tryCatch(httr2::resp_headers(resp), error = function(e) NULL)
  if (is.null(headers) || length(headers) == 0L) return(NULL)
  idx <- which(tolower(names(headers)) == tolower(name))
  if (length(idx) == 0L) return(NULL)
  headers[[idx[[1L]]]]
}

#' @keywords internal
.semantica_parse_wait_s <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(NA_real_)
  x <- trimws(as.character(x[[1L]]))
  if (!nzchar(x)) return(NA_real_)

  numeric_wait <- suppressWarnings(as.numeric(x))
  if (is.finite(numeric_wait)) return(max(0, numeric_wait))

  date_wait <- suppressWarnings(as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
  if (!is.na(date_wait)) return(max(0, as.numeric(difftime(date_wait, Sys.time(), units = "secs"))))
  date_wait <- suppressWarnings(as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))
  if (!is.na(date_wait)) return(max(0, as.numeric(difftime(date_wait, Sys.time(), units = "secs"))))

  matches <- gregexpr("[0-9]+(?:\\.[0-9]+)?\\s*(ms|s|m|h)", tolower(x), perl = TRUE)
  parts <- regmatches(tolower(x), matches)[[1L]]
  if (length(parts) == 0L || identical(parts, character(0L))) return(NA_real_)

  total <- 0
  for (part in parts) {
    value <- suppressWarnings(as.numeric(sub("^([0-9]+(?:\\.[0-9]+)?).*", "\\1", part, perl = TRUE)))
    unit <- sub("^[0-9]+(?:\\.[0-9]+)?\\s*", "", part, perl = TRUE)
    if (!is.finite(value)) next
    total <- total + switch(unit, ms = value / 1000, s = value, m = value * 60, h = value * 3600, 0)
  }
  if (is.finite(total) && total >= 0) total else NA_real_
}

#' @keywords internal
.semantica_default_request_spacing_s <- function(session, rate_limit_margin = 0.85) {
  margin <- suppressWarnings(as.numeric(rate_limit_margin[[1L]]))
  if (!is.finite(margin) || margin <= 0 || margin > 1) margin <- 0.85
  backend <- tolower(session$backend %||% "")
  model <- tolower(session$chat_model %||% "")
  chat_url <- tolower(session$chat_url %||% "")

  if (backend %in% c("ollama", "llamacpp", "python_hf", "python_llamacpp") ||
      grepl("localhost|127\\.0\\.0\\.1", chat_url)) {
    return(0)
  }

  if (identical(backend, "groq") || grepl("groq\\.com", chat_url)) {
    rpm <- if (grepl("qwen/qwen3-32b", model)) 60 else 30
    return((60 / rpm) / margin)
  }

  0
}

#' @keywords internal
.semantica_normalize_request_spacing_s <- function(request_spacing_s, session, rate_limit_margin = 0.85) {
  if (is.null(request_spacing_s)) return(0)
  if (is.character(request_spacing_s) &&
      length(request_spacing_s) == 1L &&
      identical(tolower(trimws(request_spacing_s)), "auto")) {
    return(.semantica_default_request_spacing_s(session, rate_limit_margin))
  }
  out <- suppressWarnings(as.numeric(request_spacing_s[[1L]]))
  if (!is.finite(out) || out < 0) stop("'request_spacing_s' must be a non-negative number or \"auto\".")
  out
}

#' @keywords internal
.semantica_wait_for_request_slot <- function(session, request_spacing_s, rate_limit_margin = 0.85) {
  spacing <- .semantica_normalize_request_spacing_s(request_spacing_s, session, rate_limit_margin)
  if (!is.finite(spacing) || spacing <= 0) return(invisible(0))

  key <- paste(session$backend %||% "", session$chat_url %||% "", session$chat_model %||% "", sep = "|")
  last <- get0(key, envir = .semantica_request_clock, inherits = FALSE, ifnotfound = NA_real_)
  now <- as.numeric(Sys.time())
  if (is.finite(last)) {
    wait_s <- spacing - (now - last)
    if (is.finite(wait_s) && wait_s > 0) Sys.sleep(wait_s)
  }
  assign(key, as.numeric(Sys.time()), envir = .semantica_request_clock)
  invisible(spacing)
}

#' @keywords internal
.semantica_normalize_retry_policy <- function(rate_limit_policy) {
  if (is.null(rate_limit_policy)) return("auto")
  match.arg(as.character(rate_limit_policy[[1L]]), c("auto", "none"))
}

#' @keywords internal
.semantica_retry_wait_s <- function(resp = NULL, retry_index = 1L,
                                   api_initial_wait_s = 1,
                                   api_max_wait_s = 120) {
  initial <- suppressWarnings(as.numeric(api_initial_wait_s[[1L]]))
  if (!is.finite(initial) || initial < 0) initial <- 1
  max_wait <- suppressWarnings(as.numeric(api_max_wait_s[[1L]]))
  if (!is.finite(max_wait) || max_wait < 0) max_wait <- 120

  header_wait <- NA_real_
  if (!is.null(resp)) {
    retry_after <- .semantica_parse_wait_s(.semantica_resp_header(resp, "retry-after"))
    reset_tokens <- .semantica_parse_wait_s(.semantica_resp_header(resp, "x-ratelimit-reset-tokens"))
    reset_requests <- .semantica_parse_wait_s(.semantica_resp_header(resp, "x-ratelimit-reset-requests"))
    if (is.finite(retry_after)) {
      header_wait <- retry_after
    } else if (is.finite(reset_tokens)) {
      header_wait <- reset_tokens
    } else if (is.finite(reset_requests)) {
      header_wait <- reset_requests
    }
  }

  if (is.finite(header_wait)) {
    wait_s <- header_wait
  } else {
    retry_index <- suppressWarnings(as.integer(retry_index[[1L]]))
    if (!is.finite(retry_index) || retry_index < 1L) retry_index <- 1L
    wait_s <- initial * (2 ^ min(retry_index - 1L, 10L))
  }

  wait_s <- min(max_wait, max(0, wait_s))
  jitter <- stats::runif(1L, min = 0, max = min(1, max(0, wait_s * 0.15)))
  wait_s + jitter
}

#' @keywords internal
.semantica_perform_request <- function(session, req,
                                       rate_limit_policy = "auto",
                                       api_max_retries = 6L,
                                       api_initial_wait_s = 1,
                                       api_max_wait_s = 120,
                                       api_retry_statuses = c(408L, 409L, 429L, 500L, 502L, 503L, 504L),
                                       request_spacing_s = 0,
                                       rate_limit_margin = 0.85,
                                       verbose = session$verbose) {
  rate_limit_policy <- .semantica_normalize_retry_policy(rate_limit_policy)
  api_max_retries <- suppressWarnings(as.integer(api_max_retries[[1L]]))
  if (!is.finite(api_max_retries) || api_max_retries < 0L) stop("'api_max_retries' must be a non-negative integer.")
  retry_statuses <- suppressWarnings(as.integer(api_retry_statuses))
  retry_statuses <- retry_statuses[is.finite(retry_statuses)]

  attempt <- 1L
  repeat {
    .semantica_wait_for_request_slot(session, request_spacing_s, rate_limit_margin)
    resp <- tryCatch(
      req |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform(),
      error = function(e) e
    )

    retries_used <- attempt - 1L
    can_retry <- identical(rate_limit_policy, "auto") && retries_used < api_max_retries

    if (inherits(resp, "error")) {
      if (can_retry) {
        wait_s <- .semantica_retry_wait_s(NULL, attempt, api_initial_wait_s, api_max_wait_s)
        if (isTRUE(verbose)) {
          message(sprintf("  API transport error. Waiting %.1fs before retry %d/%d: %s",
                          wait_s, retries_used + 1L, api_max_retries, conditionMessage(resp)))
        }
        Sys.sleep(wait_s)
        attempt <- attempt + 1L
        next
      }
      stop(conditionMessage(resp), call. = FALSE)
    }

    status <- httr2::resp_status(resp)
    if (!identical(rate_limit_policy, "auto") || !(status %in% retry_statuses) || !can_retry) {
      return(resp)
    }

    wait_s <- .semantica_retry_wait_s(resp, attempt, api_initial_wait_s, api_max_wait_s)
    if (isTRUE(verbose)) {
      message(sprintf("  API status %d. Waiting %.1fs before retry %d/%d.",
                      status, wait_s, retries_used + 1L, api_max_retries))
    }
    Sys.sleep(wait_s)
    attempt <- attempt + 1L
  }
}

#' @keywords internal
.semantica_error_message <- function(prefix, parsed, resp) {
  status <- tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_)
  status_desc <- tryCatch(httr2::resp_status_desc(resp), error = function(e) NULL)
  err <- parsed$error
  msg <- NULL
  if (is.list(err)) {
    msg <- err$message %||% err$type %||% unlist(err, use.names = FALSE)
  } else if (!is.null(err)) {
    msg <- err
  }
  msg <- msg %||% status_desc %||% status
  if (is.list(msg)) msg <- unlist(msg, use.names = FALSE)
  msg <- paste(as.character(msg), collapse = " ")
  paste0(prefix, msg)
}

#' @keywords internal
.ping_backend <- function(session) {
  proto <- session$protocol
  if (proto == "python_hf") { reticulate::import("transformers"); return(TRUE) }
  if (proto == "python_llamacpp") { reticulate::import("llama_cpp"); return(TRUE) }
  if (proto == "ollama") {
    ping_url <- sub("/api/chat$", "/api/tags", session$chat_url)
    resp <- tryCatch(httr2::request(ping_url) |> httr2::req_timeout(8L) |> httr2::req_perform(), error = function(e) NULL)
    return(!is.null(resp) && httr2::resp_status(resp) == 200L)
  }
  if (proto == "openai_compat" && grepl("localhost|127\\.0\\.0\\.1", session$chat_url %||% "")) {
    ping_url <- sub("/v1/chat/completions$", "/health", session$chat_url)
    resp <- tryCatch(httr2::request(ping_url) |> httr2::req_timeout(8L) |> httr2::req_perform(), error = function(e) NULL)
    if (!is.null(resp) && httr2::resp_status(resp) < 400L) return(TRUE)
  }
  .call_chat(session, messages = list(list(role = "user", content = "ping")), max_tokens = 1L,
             rate_limit_policy = "none", api_max_retries = 0L)
  TRUE
}

#' @keywords internal
.call_chat <- function(session, messages, max_tokens = 2048L, temperature = 0.7, system_prompt = NULL,
                       rate_limit_policy = "auto", api_max_retries = 6L,
                       api_initial_wait_s = 1, api_max_wait_s = 120,
                       api_retry_statuses = c(408L, 409L, 429L, 500L, 502L, 503L, 504L),
                       request_spacing_s = 0, rate_limit_margin = 0.85) {
  proto <- session$protocol
  if (proto == "python_hf") return(.py_hf_chat(session, messages, max_tokens, temperature, system_prompt))
  if (proto == "python_llamacpp") return(.py_llamacpp_chat(session, messages, max_tokens, temperature, system_prompt))

  if (proto == "anthropic") {
    body <- list(model = session$chat_model, max_tokens = max_tokens, messages = messages)
    if (!is.null(system_prompt)) body$system <- system_prompt
    resp <- .semantica_perform_request(
      session, .build_request(session, session$chat_url, body),
      rate_limit_policy = rate_limit_policy, api_max_retries = api_max_retries,
      api_initial_wait_s = api_initial_wait_s, api_max_wait_s = api_max_wait_s,
      api_retry_statuses = api_retry_statuses, request_spacing_s = request_spacing_s,
      rate_limit_margin = rate_limit_margin
    )
    parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (httr2::resp_status(resp) >= 400L) stop(.semantica_error_message("Anthropic error: ", parsed, resp))
    txt <- parsed$content[[1L]]$text
    if (is.null(txt) || length(txt) == 0L) stop("Empty Anthropic response.")
    return(as.character(txt))
  }

  msgs <- messages
  if (!is.null(system_prompt)) msgs <- c(list(list(role = "system", content = system_prompt)), msgs)
  body <- list(model = session$chat_model, messages = msgs, max_tokens = max_tokens, temperature = temperature)
  if (proto == "ollama") { body$stream <- FALSE; body$options <- list(temperature = temperature, num_predict = max_tokens); body$max_tokens <- NULL }

  resp <- .semantica_perform_request(
    session, .build_request(session, session$chat_url, body),
    rate_limit_policy = rate_limit_policy, api_max_retries = api_max_retries,
    api_initial_wait_s = api_initial_wait_s, api_max_wait_s = api_max_wait_s,
    api_retry_statuses = api_retry_statuses, request_spacing_s = request_spacing_s,
    rate_limit_margin = rate_limit_margin
  )
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (httr2::resp_status(resp) >= 400L) stop(.semantica_error_message("LLM API error: ", parsed, resp))
  txt <- if (proto == "ollama") parsed$message$content else parsed$choices[[1L]]$message$content
  if (is.null(txt) || length(txt) == 0L) stop("Empty response from backend '", session$backend, "'.")
  as.character(txt)
}

#' @keywords internal
.call_embed <- function(session, texts) {
  proto <- session$protocol
  if (proto == "python_hf") return(.py_sentence_transformers(session, texts))
  if (proto == "python_llamacpp") return(.py_llamacpp_embed(session, texts))
  if (is.null(session$embed_url)) stop("Backend '", session$backend, "' has no embedding endpoint. Use a separate embed_session.")

  if (proto == "ollama") {
    rows <- lapply(texts, function(txt) {
      body <- list(model = session$embed_model, prompt = txt)
      resp <- .build_request(session, session$embed_url, body) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
      if (httr2::resp_status(resp) >= 400L) stop("Ollama embed error: ", httr2::resp_status(resp))
      unlist(httr2::resp_body_json(resp, simplifyVector = FALSE)$embedding)
    })
    mat <- do.call(rbind, rows); rownames(mat) <- NULL; return(mat)
  }

  body <- list(model = session$embed_model, input = as.list(texts))
  resp <- .build_request(session, session$embed_url, body) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
  if (httr2::resp_status(resp) >= 400L) stop("Embedding API error: ", httr2::resp_status(resp))
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (is.null(parsed$data) || length(parsed$data) == 0L) stop("Embedding API returned no data.")
  # OpenAI-compatible embedding responses include `index`; sorting protects the
  # item-text alignment if a backend returns rows out of order.
  if (all(vapply(parsed$data, function(d) !is.null(d$index), logical(1L)))) {
    ord <- order(vapply(parsed$data, function(d) as.integer(d$index), integer(1L)))
    parsed$data <- parsed$data[ord]
  }
  mat <- do.call(rbind, lapply(parsed$data, function(d) {
    if (is.null(d$embedding)) stop("Embedding API row missing `embedding` field.")
    as.numeric(unlist(d$embedding, use.names = FALSE))
  }))
  rownames(mat) <- NULL; mat
}

.py_modules <- new.env(parent = emptyenv())
.py_model_cache <- new.env(parent = emptyenv())

#' Clear cached Python model instances retained by SEMANTICA
#'
#' Local Python-backed models can occupy substantial CPU or GPU memory. This
#' internal helper removes cached model references after a completed pipeline
#' or between model changes; callers that still need a cached model should not
#' invoke it until their work is complete.
#'
#' @param pattern Optional regular expression matching cache keys to remove.
#'   `NULL` removes every cached Python model instance.
#' @param collect Logical; request garbage collection after removing models.
#' @return Invisibly returns the removed cache keys.
#' @keywords internal
.semantica_clear_python_model_cache <- function(pattern = NULL, collect = TRUE) {
  keys <- ls(.py_model_cache, all.names = TRUE)
  if (!is.null(pattern)) keys <- grep(pattern, keys, value = TRUE)
  if (length(keys) > 0L) rm(list = keys, envir = .py_model_cache)
  if (isTRUE(collect)) invisible(gc(FALSE))
  invisible(keys)
}

#' @keywords internal
.py_get <- function(module_name) {
  if (!exists(module_name, envir = .py_modules, inherits = FALSE)) {
    assign(module_name, reticulate::import(module_name, delay_load = FALSE), envir = .py_modules)
  }
  get(module_name, envir = .py_modules, inherits = FALSE)
}

#' @keywords internal
.py_hf_chat <- function(session, messages, max_tokens, temperature, system_prompt) {
  transformers <- .py_get("transformers")
  model_id <- session$chat_model
  cache_key <- paste0("hf_pipe_", model_id)
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading HF pipeline for '", model_id, "'...")
    kwargs <- list(task = "text-generation", model = model_id, device_map = "auto")
    if (!is.null(session$hf_token)) kwargs$token <- session$hf_token
    pipe <- do.call(transformers$pipeline, kwargs)
    assign(cache_key, pipe, envir = .py_model_cache)
  }
  pipe <- get(cache_key, envir = .py_model_cache)
  py_messages <- reticulate::r_to_py(if (!is.null(system_prompt)) c(list(list(role="system", content=system_prompt)), messages) else messages)
  out <- pipe(py_messages, max_new_tokens = as.integer(max_tokens), temperature = temperature, do_sample = TRUE, return_full_text = FALSE)
  txt <- tryCatch(out[[1L]]$generated_text, error = function(e) out[[1L]][[1L]]$generated_text)
  if (is.null(txt)) stop("HuggingFace pipeline returned NULL text.")
  as.character(txt)
}

#' @keywords internal
.py_llamacpp_chat <- function(session, messages, max_tokens, temperature, system_prompt) {
  llama_cpp <- .py_get("llama_cpp")
  if (is.null(session$gguf_path) || !file.exists(session$gguf_path)) stop("python_llamacpp requires a valid gguf_path.")
  cache_key <- paste0("llamacpp_chat_", session$gguf_path)
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading GGUF model '", basename(session$gguf_path), "'...")
    llm <- llama_cpp$Llama(model_path = session$gguf_path, n_ctx = 4096L, n_gpu_layers = -1L, verbose = FALSE)
    assign(cache_key, llm, envir = .py_model_cache)
  }
  llm <- get(cache_key, envir = .py_model_cache)
  msgs <- messages
  if (!is.null(system_prompt)) msgs <- c(list(list(role="system", content=system_prompt)), msgs)
  out <- llm$create_chat_completion(messages = reticulate::r_to_py(msgs), max_tokens = as.integer(max_tokens), temperature = temperature)
  txt <- out$choices[[1L]]$message$content
  if (is.null(txt)) stop("llama-cpp-python returned NULL text.")
  as.character(txt)
}

#' @keywords internal
.py_sentence_transformers <- function(session, texts) {
  st <- .py_get("sentence_transformers")
  model_id <- session$embed_model %||% "sentence-transformers/all-MiniLM-L6-v2"
  cache_key <- paste0("st_", model_id)
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading sentence-transformer '", model_id, "'...")
    model <- st$SentenceTransformer(model_id)
    assign(cache_key, model, envir = .py_model_cache)
  }
  model <- get(cache_key, envir = .py_model_cache)
  embs <- model$encode(reticulate::r_to_py(as.list(texts)), normalize_embeddings = TRUE, show_progress_bar = FALSE)
  mat <- reticulate::py_to_r(embs)
  if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(texts))
  rownames(mat) <- NULL; mat
}

#' @keywords internal
.py_llamacpp_embed <- function(session, texts) {
  llama_cpp <- .py_get("llama_cpp")
  if (is.null(session$gguf_path) || !file.exists(session$gguf_path)) stop("python_llamacpp embed requires gguf_path.")
  cache_key <- paste0("llamacpp_embed_", session$gguf_path)
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    llm <- llama_cpp$Llama(model_path = session$gguf_path, n_ctx = 512L, n_gpu_layers = -1L, embedding = TRUE, verbose = FALSE)
    assign(cache_key, llm, envir = .py_model_cache)
  }
  llm <- get(cache_key, envir = .py_model_cache)
  rows <- lapply(texts, function(txt) {
    out <- llm$embed(txt)
    if (inherits(out, "python.builtin.list")) unlist(reticulate::py_to_r(out)) else as.numeric(reticulate::py_to_r(out))
  })
  mat <- do.call(rbind, rows); rownames(mat) <- NULL; mat
}

# =================================================================
# P4  ITEM GENERATION HELPERS
# =================================================================
#' @keywords internal
.build_system_prompt <- function(scale_name, scale_description, response_format, item_style, language) {
  sprintf('You are an expert psychometrician...\nSCALE CONTEXT\nName: %s\nDescription: %s\nResponse: %s\nStyle: %s\nLanguage: %s\n\nOUTPUT CONTRACT:\nReturn ONLY a numbered list. No preamble. Each line: "<number>. <item text>". Avoid double-barrelled items, jargon, negations.', scale_name, scale_description, response_format, item_style, language)
}

#' @keywords internal
.build_factor_prompt <- function(factor_name, factor_description, n_items,
                                 user_examples = NULL, forbidden_concepts = NULL,
                                 extra_instructions = NULL,
                                 dimension_name = NULL, dimension_description = NULL,
                                 facet_name = NULL) {
  if (!is.null(dimension_name) && !is.null(facet_name)) {
    lines <- c(
      sprintf("Generate exactly %d psychometric items for the following facet within a broader dimension.", n_items),
      "",
      sprintf("DIMENSION NAME       : %s", dimension_name),
      sprintf("DIMENSION DEFINITION : %s", dimension_description %||% paste("Items for", dimension_name)),
      sprintf("FACET NAME           : %s", facet_name),
      sprintf("FACET DEFINITION     : %s", factor_description)
    )
  } else {
    lines <- c(sprintf("Generate exactly %d psychometric items for the following factor.", n_items), "",
               sprintf("FACTOR NAME       : %s", factor_name), sprintf("FACTOR DEFINITION : %s", factor_description))
  }
  if (!is.null(user_examples) && length(user_examples) > 0L) lines <- c(lines, "", "EXAMPLE ITEMS:", paste0("  - ", user_examples))
  if (!is.null(forbidden_concepts) && length(forbidden_concepts) > 0L) lines <- c(lines, "", "DO NOT write about:", paste0("  - ", forbidden_concepts))
  if (!is.null(extra_instructions) && nchar(trimws(extra_instructions)) > 0L) lines <- c(lines, "", "ADDITIONAL INSTRUCTIONS:", extra_instructions)
  lines <- c(lines, "", sprintf("Output exactly %d items, numbered 1 to %d.", n_items, n_items))
  paste(lines, collapse = "\n")
}

#' @keywords internal
.parse_items <- function(raw_text, expected_n, factor_name, minimum_n = expected_n) {
  if (is.null(raw_text) || length(raw_text) == 0L || is.na(raw_text)) {
    warning("Empty LLM response for '", factor_name, "'.")
    return(character(0L))
  }

  txt <- as.character(raw_text[[1L]])
  txt <- gsub("\r\n?", "\n", txt)
  txt <- gsub("```[A-Za-z]*", "", txt)
  txt <- gsub("```", "", txt)
  # Some LLMs place all numbered items on one line; force a line break
  # before recognizable list markers before parsing.
  txt <- gsub("(?m)(^|\\s)(\\d+\\s*[\\.)\\:\\-]\\s+)", "\n\\2", txt, perl = TRUE)

  lines <- stringr::str_trim(stringr::str_split(txt, "\n")[[1L]])
  lines <- lines[nchar(lines) > 0L]
  if (length(lines) == 0L) {
    warning("Could not parse items for '", factor_name, "'.")
    return(character(0L))
  }

  strip_marker <- function(x) {
    bullet <- intToUtf8(0x2022)
    left_quote <- intToUtf8(0x201c)
    right_quote <- intToUtf8(0x201d)

    x <- gsub("^\\s*\\d+\\s*[\\.)\\:\\-]\\s+", "", x, perl = TRUE)
    x <- gsub(paste0("^\\s*[-*", bullet, "]\\s+"), "", x, perl = TRUE)
    x <- gsub(
      paste0("^\\s*[\"'", left_quote, right_quote, "]+|[\"'", left_quote, right_quote, "]+\\s*$"),
      "",
      x,
      perl = TRUE
    )
    x <- gsub("\\s+", " ", x)
    stringr::str_trim(x)
  }

  items <- strip_marker(lines)
  prefix_text <- iconv(tolower(items), from = "", to = "ASCII//TRANSLIT")
  bad_prefix <- grepl(
    "^(here|sure|certainly|below|these|the following|aqui|claro|por supuesto|lista|nota|note)\\b",
    prefix_text
  )
  items <- items[!bad_prefix]
  items <- items[nchar(items) > 5L]
  items <- unique(items)

  if (length(items) == 0L) warning("Could not parse items for '", factor_name, "'.")
  if (length(items) < minimum_n) {
    warning(sprintf("Factor '%s': needed %d usable items, parsed %d.", factor_name, minimum_n, length(items)))
  }
  if (length(items) > expected_n) items <- items[seq_len(expected_n)]
  items
}

#' @keywords internal
.dedup_items <- function(items, threshold = 0.92) {
  if (length(items) <= 1L) return(items)
  bigrams <- function(s) { ch <- strsplit(tolower(s), "")[[1L]]; if (length(ch) < 2L) return(character(0L)); paste0(ch[-length(ch)], ch[-1L]) }
  jaccard <- function(a, b) { ba <- bigrams(a); bb <- bigrams(b); if (!length(ba) || !length(bb)) return(0); length(intersect(ba, bb)) / length(union(ba, bb)) }
  keep <- rep(TRUE, length(items))
  for (i in seq_len(length(items) - 1L)) { if (!keep[i]) next; for (j in (i+1L):length(items)) { if (!keep[j]) next; if (jaccard(items[i], items[j]) >= threshold) keep[j] <- FALSE } }
  items[keep]
}

#' @keywords internal
.as_positive_int <- function(x, what) {
  out <- suppressWarnings(as.integer(x))
  if (length(out) != 1L || is.na(out) || out < 1L) stop(what, " must be a positive integer.")
  out
}

#' @keywords internal
.allocate_counts <- function(total, n_groups) {
  total <- .as_positive_int(total, "'total'")
  n_groups <- .as_positive_int(n_groups, "'n_groups'")
  if (total < n_groups) stop("Total requested items must be at least the number of facets.")
  base <- rep(total %/% n_groups, n_groups)
  rem <- total %% n_groups
  if (rem > 0L) base[seq_len(rem)] <- base[seq_len(rem)] + 1L
  base
}

#' @keywords internal
.expand_generation_plan <- function(factors, n_per_factor = NULL,
                                    n_per_factor_override = !is.null(n_per_factor)) {
  if (!is.list(factors) || is.null(names(factors)) || any(!nzchar(names(factors)))) {
    stop("'factors' must be a named list.")
  }
  override_total <- NULL
  if (isTRUE(n_per_factor_override) && !is.null(n_per_factor)) {
    override_total <- .as_positive_int(n_per_factor, "'n_per_factor'")
  }

  plan <- list()
  for (dimension in names(factors)) {
    dim_spec <- factors[[dimension]]
    if (!is.list(dim_spec)) dim_spec <- list(description = as.character(dim_spec))

    dim_desc <- dim_spec$description %||% dim_spec$Definition %||% dim_spec$definition %||% paste("Items for", dimension)
    facets_raw <- dim_spec$facets %||% dim_spec$Facets

    if (is.null(facets_raw) || length(facets_raw) == 0L) {
      n_items <- override_total %||% dim_spec$n_items %||% n_per_factor %||% stop("Specify n_items or n_per_factor for dimension '", dimension, "'.")
      facet <- dim_spec$facet %||% dim_spec$Facet %||% dimension
      plan[[length(plan) + 1L]] <- list(
        dimension = dimension, dimension_description = dim_desc,
        facet = as.character(facet), facet_description = dim_desc,
        n_items = .as_positive_int(n_items, sprintf("n_items for dimension '%s'", dimension)),
        examples = dim_spec$examples, forbidden = dim_spec$forbidden,
        extra_instructions = dim_spec$extra_instructions
      )
      next
    }

    facet_specs <- if (is.character(facets_raw)) {
      facet_names <- if (!is.null(names(facets_raw)) && all(nzchar(names(facets_raw)))) names(facets_raw) else as.character(facets_raw)
      lapply(seq_along(facet_names), function(i) list(description = as.character(facets_raw[[i]])))
    } else if (is.list(facets_raw)) {
      facet_names <- names(facets_raw)
      if (is.null(facet_names) || any(!nzchar(facet_names))) {
        facet_names <- vapply(facets_raw, function(x) {
          if (is.list(x)) as.character(x$name %||% x$facet %||% x$Facet %||% NA_character_) else as.character(x)
        }, character(1L))
      }
      if (anyNA(facet_names) || any(!nzchar(facet_names))) stop("All facets in dimension '", dimension, "' must be named.")
      lapply(facets_raw, function(x) if (is.list(x)) x else list(description = as.character(x)))
    } else {
      stop("'facets' for dimension '", dimension, "' must be a character vector or named list.")
    }
    names(facet_specs) <- facet_names

    if (!is.null(override_total)) {
      counts <- .allocate_counts(override_total, length(facet_specs))
    } else {
      n_total <- dim_spec$n_items
      per_facet <- dim_spec$n_items_per_facet %||% dim_spec$n_per_facet
      counts <- rep(NA_integer_, length(facet_specs))
      for (i in seq_along(facet_specs)) {
        if (!is.null(facet_specs[[i]]$n_items)) counts[i] <- .as_positive_int(facet_specs[[i]]$n_items, sprintf("n_items for facet '%s'", names(facet_specs)[i]))
      }

      missing <- which(is.na(counts))
      if (length(missing) > 0L) {
        if (!is.null(per_facet)) {
          counts[missing] <- .as_positive_int(per_facet, sprintf("n_items_per_facet for dimension '%s'", dimension))
        } else {
          base_total <- n_total %||% n_per_factor %||% stop("Specify n_items, n_items_per_facet, or n_per_factor for dimension '", dimension, "'.")
          remaining <- .as_positive_int(base_total, sprintf("n_items for dimension '%s'", dimension)) - sum(counts, na.rm = TRUE)
          counts[missing] <- .allocate_counts(remaining, length(missing))
        }
      }
    }

    for (i in seq_along(facet_specs)) {
      fs <- facet_specs[[i]]
      facet_name <- names(facet_specs)[i]
      plan[[length(plan) + 1L]] <- list(
        dimension = dimension, dimension_description = dim_desc,
        facet = facet_name,
        facet_description = fs$description %||% fs$Definition %||% fs$definition %||% paste("Facet", facet_name, "within", dimension),
        n_items = counts[i],
        examples = fs$examples %||% dim_spec$examples,
        forbidden = c(dim_spec$forbidden, fs$forbidden),
        extra_instructions = paste(c(dim_spec$extra_instructions, fs$extra_instructions), collapse = "\n")
      )
    }
  }

  plan
}

#' Return standardized SEMANTICA item metadata
#'
#' Creates a deterministic four-column item table for generated, wrapped, or
#' selected item data. The output always contains exactly `ID`, `Dimension`,
#' `Facet`, and `item`.
#'
#' @param x Data frame containing item metadata.
#' @param id_col,dimension_col,facet_col,item_col Optional source column names.
#' @param strict If `TRUE`, require non-missing IDs, dimensions, facets, and item text.
#' @return Data frame with exactly `ID`, `Dimension`, `Facet`, and `item`.
#' @export
#' @examples
#' raw_items <- data.frame(
#'   item_id = c("A1", "A2"),
#'   factor = c("Attention", "Attention"),
#'   facet = c("Focus", "Focus"),
#'   item_text = c(
#'     "I notice when my attention drifts.",
#'     "I can return my focus to the task."
#'   )
#' )
#' semantica_standardize_item_metadata(raw_items)
semantica_standardize_item_metadata <- function(x, id_col = NULL, dimension_col = NULL,
                                                facet_col = NULL, item_col = NULL,
                                                strict = TRUE) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame or tibble.")
  pick_col <- function(explicit, candidates) {
    if (!is.null(explicit)) {
      if (!explicit %in% names(x)) stop("Column not found: ", explicit)
      return(explicit)
    }
    hit <- intersect(candidates, names(x))
    if (length(hit) == 0L) NULL else hit[1L]
  }

  id_col <- pick_col(id_col, c("ID", "item_id", "item", "Item", "id", "ID"))
  dimension_col <- pick_col(dimension_col, c("Dimension", "dimension", "factor", "type", "Factor", "Type"))
  facet_col <- pick_col(facet_col, c("Facet", "facet", "subfacet", "domain", "Domain"))
  item_col <- pick_col(item_col, c("item", "item_text", "text", "wording", "item_wording", "label"))
  if (is.null(id_col) || is.null(dimension_col) || is.null(item_col)) {
    stop("Could not infer ID, Dimension, and item columns from metadata.")
  }

  out <- data.frame(
    ID = as.character(x[[id_col]]),
    Dimension = as.character(x[[dimension_col]]),
    Facet = if (!is.null(facet_col)) as.character(x[[facet_col]]) else as.character(x[[dimension_col]]),
    item = as.character(x[[item_col]]),
    stringsAsFactors = FALSE
  )
  if (strict) {
    bad <- !stats::complete.cases(out) | !nzchar(trimws(out$ID)) |
      !nzchar(trimws(out$Dimension)) | !nzchar(trimws(out$Facet)) |
      !nzchar(trimws(out$item))
    if (any(bad)) stop("Item metadata contains missing or empty ID, Dimension, Facet, or item values.")
    if (anyDuplicated(out$ID)) stop("Item metadata IDs must be unique.")
  }
  rownames(out) <- NULL
  out
}

# =================================================================
# P4  MAIN GENERATION FUNCTION
# =================================================================
#' Generate psychometric items for all factors using an LLM
#'
#' @param session          A `semantica_session` from `semantica_connect()`.
#' @param scale_name       Short name of the scale.
#' @param scale_description One-paragraph construct description.
#' @param factors          Named list of dimension specs (`$description`, `$n_items`,
#'   optional `$facets`, `$examples`, `$forbidden`, `$extra_instructions`).
#' @param response_format  e.g., `"5-point Likert"`.
#' @param item_style       e.g., `"first-person declarative sentence"`.
#' @param language         Language for item text.
#' @param n_per_factor     Global target number of retained items per factor.
#' @param n_per_factor_override Logical; when `TRUE` and `n_per_factor` is
#'   supplied, distribute that total across facets even when the factor
#'   specification contains facet-level item counts. Defaults to `TRUE` for
#'   direct calls that supply `n_per_factor`.
#' @param overgenerate     LLM overgeneration multiplier.
#' @param max_retries      Retries on short responses.
#' @param global_forbidden_max Maximum number of previously generated items to
#'   include as anti-duplicate examples in subsequent prompts.
#' @param temperature      LLM temperature.
#' @param rate_limit_policy Transport retry policy. `"auto"` retries temporary
#'   API rate limits and transient server errors using provider headers and
#'   exponential backoff; `"none"` preserves fail-fast behavior.
#' @param api_max_retries Maximum number of transport-level retries for one LLM
#'   request. These retries do not consume `max_retries`, which remains reserved
#'   for short or unusable LLM content.
#' @param api_initial_wait_s,api_max_wait_s Initial and maximum wait, in seconds,
#'   for exponential backoff when a provider does not return retry timing.
#' @param api_retry_statuses HTTP status codes treated as temporary transport
#'   failures when `rate_limit_policy = "auto"`.
#' @param request_spacing_s Minimum seconds between chat request starts for the
#'   same backend/model. `NULL` uses `"auto"` when `rate_limit_policy = "auto"`
#'   and no spacing when `rate_limit_policy = "none"`. Use `"auto"` explicitly
#'   for conservative backend-specific spacing.
#' @param rate_limit_margin Fraction of an auto-detected request rate to use
#'   when `request_spacing_s = "auto"`.
#' @param verbose          Print progress.
#' @return Tibble with legacy columns (`item_id`, `factor`, `item_text`,
#'   `attempt`) and facet-aware columns (`ID`, `Dimension`, `Facet`, `item`).
#' @export
#' @examples
#' \dontrun{
#' session <- semantica_connect("openai")
#' factors <- list(
#'   Clarity = list(n_items = 8L, description = "Clear thinking."),
#'   Flexibility = list(n_items = 8L, description = "Adaptive thinking.")
#' )
#' items <- semantica_generate_items(
#'   session = session,
#'   scale_name = "Cognitive Agility",
#'   scale_description = "Clear and adaptive thinking.",
#'   factors = factors,
#'   n_per_factor_override = FALSE,
#'   verbose = FALSE
#' )
#' }
semantica_generate_items <- function(session, scale_name, scale_description, factors,
                                     response_format = "5-point Likert", item_style = "first-person declarative sentence",
                                     language = "English", n_per_factor = NULL,
                                     n_per_factor_override = !is.null(n_per_factor),
                                     overgenerate = 2.0, max_retries = 3L,
                                     global_forbidden_max = 40L, temperature = 0.8, verbose = TRUE,
                                     rate_limit_policy = c("auto", "none"),
                                     api_max_retries = 6L,
                                     api_initial_wait_s = 1,
                                     api_max_wait_s = 120,
                                     api_retry_statuses = c(408L, 409L, 429L, 500L, 502L, 503L, 504L),
                                     request_spacing_s = NULL,
                                     rate_limit_margin = 0.85) {
  if (!inherits(session, "semantica_session")) stop("'session' must be created with semantica_connect().")
  if (!is.list(factors) || is.null(names(factors))) stop("'factors' must be a named list.")
  rate_limit_policy <- .semantica_normalize_retry_policy(rate_limit_policy)
  if (is.null(request_spacing_s)) {
    request_spacing_s <- if (identical(rate_limit_policy, "auto")) "auto" else 0
  }
  if (!is.numeric(overgenerate) || length(overgenerate) != 1L || !is.finite(overgenerate) || overgenerate <= 0) {
    stop("'overgenerate' must be a positive finite number.")
  }
  if (!is.numeric(max_retries) || length(max_retries) != 1L || max_retries < 1L) {
    stop("'max_retries' must be a positive integer.")
  }
  global_forbidden_max <- suppressWarnings(as.integer(global_forbidden_max))
  if (length(global_forbidden_max) != 1L || is.na(global_forbidden_max) || global_forbidden_max < 0L) {
    stop("'global_forbidden_max' must be a non-negative integer.")
  }
  api_max_retries <- suppressWarnings(as.integer(api_max_retries[[1L]]))
  if (!is.finite(api_max_retries) || api_max_retries < 0L) stop("'api_max_retries' must be a non-negative integer.")
  api_initial_wait_s <- suppressWarnings(as.numeric(api_initial_wait_s[[1L]]))
  if (!is.finite(api_initial_wait_s) || api_initial_wait_s < 0) stop("'api_initial_wait_s' must be a non-negative number.")
  api_max_wait_s <- suppressWarnings(as.numeric(api_max_wait_s[[1L]]))
  if (!is.finite(api_max_wait_s) || api_max_wait_s < 0) stop("'api_max_wait_s' must be a non-negative number.")
  rate_limit_margin <- suppressWarnings(as.numeric(rate_limit_margin[[1L]]))
  if (!is.finite(rate_limit_margin) || rate_limit_margin <= 0 || rate_limit_margin > 1) {
    stop("'rate_limit_margin' must be a number greater than 0 and no greater than 1.")
  }
  .semantica_normalize_request_spacing_s(request_spacing_s, session, rate_limit_margin)

  generation_plan <- .expand_generation_plan(factors, n_per_factor, n_per_factor_override)
  factor_names <- unique(vapply(generation_plan, `[[`, character(1L), "dimension"))
  system_prompt <- .build_system_prompt(scale_name, scale_description, response_format, item_style, language)
  if (verbose) {
    cat("============================================================\nSTEP 2 -- ITEM GENERATION\n============================================================\n")
    cat(sprintf("  Scale         : %s\n  Dimensions    : %d\n  Generation units: %d\n", scale_name, length(factor_names), length(generation_plan)))
    if (isTRUE(n_per_factor_override) && !is.null(n_per_factor)) {
      cat(sprintf("  Count override: %d retained items per dimension, allocated across facets\n",
                  .as_positive_int(n_per_factor, "'n_per_factor'")))
    }
  }

  all_rows <- list(); item_counter <- 1L; global_forbidden <- character(0L)
  for (unit in generation_plan) {
    n_target <- unit$n_items
    n_request <- ceiling(n_target * overgenerate)
    if (verbose) cat(sprintf("\n  [%s / %s] Requesting %d items...\n", unit$dimension, unit$facet, n_request))

    prompt_forbidden <- unique(c(unit$forbidden, tail(global_forbidden, global_forbidden_max)))
    user_prompt <- .build_factor_prompt(
      factor_name = unit$facet,
      factor_description = unit$facet_description,
      n_items = n_request,
      user_examples = unit$examples,
      forbidden_concepts = prompt_forbidden,
      extra_instructions = paste(
        unit$extra_instructions,
        "Return only a plain numbered list. Do not add headings, explanations, markdown tables, JSON, or bullet points."
      ),
      dimension_name = unit$dimension,
      dimension_description = unit$dimension_description,
      facet_name = unit$facet
    )
    collected <- character(0L); attempt <- 1L; request_n <- n_request; last_error <- NULL
    while (length(collected) < n_target && attempt <= max_retries) {
      raw <- tryCatch(
        .call_chat(session, messages = list(list(role="user", content=user_prompt)),
                   max_tokens = max(256L, request_n * 90L), temperature = temperature,
                   system_prompt = system_prompt,
                   rate_limit_policy = rate_limit_policy,
                   api_max_retries = api_max_retries,
                   api_initial_wait_s = api_initial_wait_s,
                   api_max_wait_s = api_max_wait_s,
                   api_retry_statuses = api_retry_statuses,
                   request_spacing_s = request_spacing_s,
                   rate_limit_margin = rate_limit_margin),
        error = function(e) {
          last_error <<- conditionMessage(e)
          if (verbose) message(sprintf("    Attempt %d failed for %s/%s: %s", attempt, unit$dimension, unit$facet, last_error))
          NULL
        }
      )
      if (!is.null(raw)) {
        parsed <- .dedup_items(.parse_items(raw, request_n, unit$facet, minimum_n = min(n_target, request_n)))
        collected <- unique(c(collected, parsed))
      }
      attempt <- attempt + 1L
      if (length(collected) < n_target && attempt <= max_retries) {
        n_miss <- n_target - length(collected)
        request_n <- max(n_miss + 4L, n_target)
        prompt_forbidden <- unique(c(unit$forbidden, collected, tail(global_forbidden, global_forbidden_max)))
        user_prompt <- .build_factor_prompt(
          factor_name = unit$facet,
          factor_description = unit$facet_description,
          n_items = request_n,
          user_examples = unit$examples,
          forbidden_concepts = prompt_forbidden,
          extra_instructions = paste(
            "Generate NEW items only.",
            "Return exactly the requested number of items as '1. item text', one item per line, with no preamble.",
            unit$extra_instructions
          ),
          dimension_name = unit$dimension,
          dimension_description = unit$dimension_description,
          facet_name = unit$facet
        )
      }
    }
    if (length(collected) > n_target) collected <- collected[seq_len(n_target)]
    if (length(collected) < n_target) {
      # Continuing with too few items breaks the downstream ACO/ESEM constraints.
      err_suffix <- if (!is.null(last_error)) paste0(" Last backend error: ", last_error) else ""
      stop(sprintf("Facet '%s' in dimension '%s': generated only %d/%d usable items after %d attempt(s).%s",
                   unit$facet, unit$dimension, length(collected), n_target, max_retries, err_suffix))
    }
    if (verbose) cat(sprintf("    --> Kept %d items\n", length(collected)))
    for (txt in collected) {
      id <- sprintf("item%03d", item_counter)
      all_rows[[length(all_rows)+1L]] <- list(
        ID = id, Dimension = unit$dimension, Facet = unit$facet, item = txt,
        item_id = id, factor = unit$dimension, type = unit$dimension,
        item_text = txt, attempt = attempt - 1L
      )
      item_counter <- item_counter + 1L
    }
    global_forbidden <- c(global_forbidden, collected)
  }

  if (length(all_rows) == 0L) stop("No items were generated.")
  out <- tibble::as_tibble(do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors=FALSE)))
  out$ID <- as.character(out$ID); out$Dimension <- as.character(out$Dimension); out$Facet <- as.character(out$Facet); out$item <- as.character(out$item)
  out$item_id <- as.character(out$item_id); out$factor <- as.character(out$factor); out$type <- as.character(out$type); out$item_text <- as.character(out$item_text); out$attempt <- as.integer(out$attempt)
  if (verbose) cat(sprintf("\n  Total items: %d\n", nrow(out)))
  out
}

# =================================================================
# P5  EMBEDDINGS
# =================================================================
#' Extract dense embeddings for all generated items
#'
#' @param items_tbl     Tibble from `semantica_generate_items()`.
#' @param session       `semantica_session`.
#' @param embed_session Optional separate embedding session.
#' @param text_col      Column with item text.
#' @param id_col        Column with item IDs.
#' @param batch_size    Items per API call.
#' @param normalize     L2-normalise vectors.
#' @param verbose       Print progress.
#' @return List: `$embeddings`, `$items_tbl`, `$embed_model`, `$embed_dim`.
#' @export
#' @examples
#' \dontrun{
#' session <- semantica_connect("openai")
#' embedded <- semantica_embed(
#'   items_tbl = items,
#'   session = session,
#'   batch_size = 32L,
#'   normalize = TRUE,
#'   verbose = FALSE
#' )
#' }
semantica_embed <- function(items_tbl, session, embed_session = NULL, text_col = "item_text", id_col = "item_id", batch_size = 64L, normalize = TRUE, verbose = TRUE) {
  esess <- embed_session %||% session
  if (is.null(esess$embed_url) && !esess$protocol %in% c("python_hf", "python_llamacpp")) stop("Backend has no embedding endpoint. Pass embed_session.")
  if (!is.data.frame(items_tbl)) stop("'items_tbl' must be a data.frame or tibble.")
  if (!text_col %in% names(items_tbl) && "item" %in% names(items_tbl)) text_col <- "item"
  if (!id_col %in% names(items_tbl) && "ID" %in% names(items_tbl)) id_col <- "ID"
  missing_cols <- setdiff(c(text_col, id_col), names(items_tbl))
  if (length(missing_cols) > 0L) stop("Missing required item column(s): ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(batch_size) || length(batch_size) != 1L || batch_size < 1L) stop("'batch_size' must be a positive integer.")

  texts <- as.character(items_tbl[[text_col]]); ids <- as.character(items_tbl[[id_col]]); n <- length(texts)
  if (n == 0L) stop("'items_tbl' has no rows to embed.")
  if (anyNA(texts) || any(!nzchar(trimws(texts)))) stop("All item texts must be non-empty.")
  if (anyNA(ids) || any(!nzchar(trimws(ids)))) stop("All item IDs must be non-empty.")
  if (anyDuplicated(ids)) stop("Item IDs must be unique before embedding.")
  if (verbose) cat("============================================================\nSTEP 3 -- EMBEDDING GENERATION\n============================================================\n")

  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  emb_matrix <- NULL
  for (bi in seq_along(batches)) {
    idx <- batches[[bi]]
    batch_matrix <- tryCatch(
      .call_embed(esess, texts[idx]),
      error = function(e) stop(sprintf("Embedding batch %d failed: %s", bi, e$message))
    )
    if (is.null(dim(batch_matrix))) batch_matrix <- matrix(batch_matrix, nrow = 1L)
    batch_matrix <- as.matrix(batch_matrix)
    storage.mode(batch_matrix) <- "double"
    if (nrow(batch_matrix) != length(idx)) {
      stop(sprintf("Embedding batch %d returned %d row(s) for %d text(s).",
                   bi, nrow(batch_matrix), length(idx)))
    }
    if (is.null(emb_matrix)) {
      emb_matrix <- matrix(
        NA_real_, nrow = n, ncol = ncol(batch_matrix),
        dimnames = list(NULL, colnames(batch_matrix))
      )
    } else if (ncol(batch_matrix) != ncol(emb_matrix)) {
      stop(sprintf("Embedding batch %d returned %d dimensions; expected %d.",
                   bi, ncol(batch_matrix), ncol(emb_matrix)))
    }
    emb_matrix[idx, ] <- batch_matrix
  }

  if (nrow(emb_matrix) != n) {
    stop(sprintf("Embedding backend returned %d row(s) for %d text(s).", nrow(emb_matrix), n))
  }
  if (any(!is.finite(emb_matrix))) stop("Embedding backend returned non-finite values.")
  raw_norms <- sqrt(rowSums(emb_matrix^2))
  if (any(!is.finite(raw_norms) | raw_norms <= .Machine$double.eps)) {
    stop("Embedding backend returned at least one zero or invalid vector.")
  }
  expected_dim <- .expected_embedding_dim(esess$embed_model)
  dim_warning <- character(0L)
  if (is.finite(expected_dim) && ncol(emb_matrix) != expected_dim) {
    dim_warning <- sprintf(
      "Embedding dimension %d differs from expected %d for model '%s'.",
      ncol(emb_matrix), expected_dim, esess$embed_model %||% "unknown"
    )
    warning(dim_warning, call. = FALSE)
  }
  if (normalize) {
    emb_matrix <- emb_matrix / raw_norms
  }
  final_norms <- sqrt(rowSums(emb_matrix^2))
  rownames(emb_matrix) <- ids
  embedding_diagnostics <- list(
    model = esess$embed_model %||% "unknown",
    n_items = nrow(emb_matrix),
    embed_dim = ncol(emb_matrix),
    expected_dim = expected_dim,
    normalized = isTRUE(normalize),
    raw_norm_min = min(raw_norms),
    raw_norm_median = stats::median(raw_norms),
    raw_norm_max = max(raw_norms),
    final_norm_min = min(final_norms),
    final_norm_median = stats::median(final_norms),
    final_norm_max = max(final_norms),
    warnings = dim_warning
  )
  if (verbose) {
    cat(sprintf("  Embedding matrix: %d x %d\n", nrow(emb_matrix), ncol(emb_matrix)))
    cat(sprintf("  Vector norms    : median %.4f%s\n",
                embedding_diagnostics$final_norm_median,
                if (isTRUE(normalize)) " (L2-normalized)" else ""))
  }
  list(
    embeddings = emb_matrix, items_tbl = items_tbl,
    embed_model = esess$embed_model %||% "unknown",
    embed_dim = ncol(emb_matrix),
    embedding_diagnostics = embedding_diagnostics
  )
}

# =================================================================
# P6 & P7  COSINE & WRAPPER
# =================================================================
#' @keywords internal
.compute_cosine_matrix <- function(emb_matrix, already_normalized = TRUE,
                                   adjustment = c("none", "mean_center")) {
  adjustment <- match.arg(adjustment)
  emb_matrix <- as.matrix(emb_matrix)
  storage.mode(emb_matrix) <- "double"
  if (adjustment == "mean_center") {
    # Optional anisotropy diagnostic path: subtract the common embedding
    # direction before cosine calculation, then renormalize rows.
    emb_matrix <- sweep(emb_matrix, 2L, colMeans(emb_matrix), FUN = "-")
    already_normalized <- FALSE
  }
  if (!already_normalized) {
    norms <- sqrt(rowSums(emb_matrix^2))
    if (any(!is.finite(norms) | norms <= .Machine$double.eps)) {
      stop("Cannot compute cosine matrix from zero or invalid embedding vectors.")
    }
    emb_matrix <- emb_matrix / norms
  }
  cos_mat <- tcrossprod(emb_matrix)
  cos_mat <- (cos_mat + t(cos_mat)) / 2
  cos_mat[cos_mat > 1] <- 1; cos_mat[cos_mat < -1] <- -1
  diag(cos_mat) <- 1
  cos_mat
}

.cosine_diagnostics <- function(cos_mat, factor_assignment = NULL) {
  off <- cos_mat[upper.tri(cos_mat)]
  off <- off[is.finite(off)]
  out <- list(
    n_items = nrow(cos_mat),
    offdiag_mean = if (length(off)) mean(off) else NA_real_,
    offdiag_median = if (length(off)) stats::median(off) else NA_real_,
    offdiag_sd = if (length(off) > 1L) stats::sd(off) else NA_real_,
    offdiag_q05 = if (length(off)) as.numeric(stats::quantile(off, 0.05, names = FALSE, na.rm = TRUE)) else NA_real_,
    offdiag_q95 = if (length(off)) as.numeric(stats::quantile(off, 0.95, names = FALSE, na.rm = TRUE)) else NA_real_,
    offdiag_min = if (length(off)) min(off) else NA_real_,
    offdiag_max = if (length(off)) max(off) else NA_real_,
    possible_anisotropy = if (length(off)) mean(off) > 0.35 else NA
  )
  if (!is.null(factor_assignment)) {
    factors <- unique(as.character(factor_assignment))
    within <- between <- numeric(0L)
    for (f_idx in seq_along(factors)) {
      f <- factors[f_idx]
      f_items <- names(factor_assignment[factor_assignment == f])
      f_items <- intersect(f_items, rownames(cos_mat))
      if (length(f_items) >= 2L) {
        within_block <- cos_mat[f_items, f_items, drop = FALSE]
        within <- c(within, within_block[lower.tri(within_block)])
      }
      if (f_idx < length(factors) && length(f_items) > 0L) {
        for (g_idx in (f_idx + 1L):length(factors)) {
          g_items <- intersect(names(factor_assignment[factor_assignment == factors[g_idx]]), rownames(cos_mat))
          if (length(g_items) > 0L) between <- c(between, as.vector(cos_mat[f_items, g_items, drop = FALSE]))
        }
      }
    }
    out$within_mean <- if (length(within)) mean(within, na.rm = TRUE) else NA_real_
    out$between_mean <- if (length(between)) mean(between, na.rm = TRUE) else NA_real_
    out$within_between_gap <- out$within_mean - out$between_mean
  }
  out
}

.compute_cosine_adjustment_sensitivity <- function(emb_matrix, already_normalized = TRUE,
                                                   factor_assignment = NULL,
                                                   high_similarity_threshold = 0.85,
                                                   standard_cosine = NULL,
                                                   centered_cosine = NULL) {
  fail <- list(
    available = FALSE,
    source = "none_vs_mean_center",
    note = "Cosine-adjustment sensitivity unavailable."
  )
  if (is.null(emb_matrix) || !is.matrix(emb_matrix) || nrow(emb_matrix) < 2L) return(fail)
  standard <- standard_cosine
  if (is.null(standard)) {
    standard <- tryCatch(
      .compute_cosine_matrix(emb_matrix, already_normalized = already_normalized, adjustment = "none"),
      error = function(e) NULL
    )
  }
  centered <- centered_cosine
  if (is.null(centered)) {
    centered <- tryCatch(
      .compute_cosine_matrix(emb_matrix, already_normalized = already_normalized, adjustment = "mean_center"),
      error = function(e) NULL
    )
  }
  if (is.null(standard) || is.null(centered)) return(fail)
  dimnames(standard) <- dimnames(centered) <- list(rownames(emb_matrix), rownames(emb_matrix))
  off_idx <- upper.tri(standard)
  off_standard <- standard[off_idx]
  off_centered <- centered[off_idx]
  finite <- is.finite(off_standard) & is.finite(off_centered)
  if (!any(finite)) return(fail)
  abs_delta <- abs(off_standard[finite] - off_centered[finite])
  high_similarity_threshold <- suppressWarnings(as.numeric(high_similarity_threshold))
  if (!is.finite(high_similarity_threshold)) high_similarity_threshold <- 0.85
  high_standard <- off_standard[finite] >= high_similarity_threshold
  high_centered <- off_centered[finite] >= high_similarity_threshold
  high_union <- sum(high_standard | high_centered)
  fa <- factor_assignment
  if (!is.null(fa) && is.null(names(fa)) && length(fa) == nrow(standard)) {
    fa <- stats::setNames(as.character(fa), rownames(standard))
  }
  d_standard <- .cosine_diagnostics(standard, fa)
  d_centered <- .cosine_diagnostics(centered, fa)
  list(
    available = TRUE,
    source = "none_vs_mean_center",
    offdiag_correlation = if (sum(finite) > 1L) stats::cor(off_standard[finite], off_centered[finite]) else NA_real_,
    mean_abs_delta = mean(abs_delta),
    q95_abs_delta = as.numeric(stats::quantile(abs_delta, 0.95, na.rm = TRUE, names = FALSE)),
    max_abs_delta = max(abs_delta),
    high_similarity_threshold = high_similarity_threshold,
    high_pair_jaccard = if (high_union > 0L) sum(high_standard & high_centered) / high_union else 1,
    none = d_standard,
    mean_center = d_centered,
    within_between_gap_delta = (d_centered$within_between_gap %||% NA_real_) -
      (d_standard$within_between_gap %||% NA_real_),
    note = "Sensitivity compares standard normalized cosine with centroid-subtracted mean-centered cosine; it is a semantic-proxy diagnostic, not response-data uncertainty."
  )
}

.apply_semantic_calibration <- function(cos_mat, semantic_calibration = NULL,
                                        item_metadata = NULL,
                                        factor_assignment = NULL) {
  untouched <- list(
    matrix = cos_mat,
    info = list(
      applied = FALSE,
      method = "none",
      note = "Raw embedding cosine matrix used as the semantic proxy."
    )
  )
  if (is.null(semantic_calibration)) return(untouched)
  calibrated <- if (is.function(semantic_calibration)) {
    semantic_calibration(
      cos_mat,
      item_metadata = item_metadata,
      factor_assignment = factor_assignment
    )
  } else {
    semantic_calibration
  }
  info <- list(
    applied = TRUE,
    method = if (is.function(semantic_calibration)) "function" else "matrix",
    note = "Semantic proxy was calibrated before ESEM; calibration must be justified by external response or domain evidence."
  )
  if (is.list(calibrated) && !is.null(calibrated$matrix)) {
    info <- utils::modifyList(info, calibrated$info %||% list())
    calibrated <- calibrated$matrix
  }
  if (!is.matrix(calibrated)) calibrated <- as.matrix(calibrated)
  if (!is.matrix(calibrated) || any(dim(calibrated) != dim(cos_mat)) || any(!is.finite(calibrated))) {
    stop("'semantic_calibration' must return a finite square matrix with the same dimensions as the cosine matrix.")
  }
  if (!is.null(rownames(calibrated)) && !is.null(colnames(calibrated)) &&
      all(rownames(cos_mat) %in% rownames(calibrated)) &&
      all(colnames(cos_mat) %in% colnames(calibrated))) {
    calibrated <- calibrated[rownames(cos_mat), colnames(cos_mat), drop = FALSE]
  }
  if (is.null(rownames(calibrated))) rownames(calibrated) <- rownames(cos_mat)
  if (is.null(colnames(calibrated))) colnames(calibrated) <- colnames(cos_mat)
  calibrated <- (calibrated + t(calibrated)) / 2
  calibrated[calibrated > 1] <- 1
  calibrated[calibrated < -1] <- -1
  diag(calibrated) <- 1
  storage.mode(calibrated) <- "double"
  list(matrix = calibrated, info = info)
}

#' Prepare cosine_sim_matrix and df for SEMANTICA optimizers
#'
#' @param embed_result         Output of `semantica_embed()`.
#' @param items_tbl            Override tibble.
#' @param id_col               Item identifier column.
#' @param factor_col           Factor assignment column.
#' @param min_items_per_factor Factors with fewer items are dropped.
#' @param cosine_adjustment    `"none"` for standard cosine or `"mean_center"`
#'   for a centroid-subtracted anisotropy sensitivity check.
#' @param semantic_calibration Optional square matrix or function used to
#'   calibrate the embedding cosine proxy before ACO/ESEM. A calibration
#'   function receives the cosine matrix plus `item_metadata` and
#'   `factor_assignment`; the result must preserve matrix dimensions.
#' @param compute_cosine_sensitivity Logical; compute the additional
#'   none-versus-mean-centered cosine sensitivity diagnostic. Set to `FALSE`
#'   when only the selected cosine representation is required and memory is
#'   constrained; this does not affect the matrix supplied to optimization.
#' @param verbose              Print summary.
#' @return Named list: `$cosine_sim_matrix`, `$df`, `$i.per.f` + metadata.
#' @export
#' @examples
#' items_tbl <- data.frame(
#'   item_id = paste0("item_", 1:6),
#'   factor = rep(c("Clarity", "Flexibility"), each = 3),
#'   item_text = paste("Example item", 1:6)
#' )
#' embeddings <- matrix(
#'   c(
#'     0.90, 0.10, 0.20,
#'     0.88, 0.12, 0.18,
#'     0.86, 0.15, 0.22,
#'     0.15, 0.88, 0.25,
#'     0.12, 0.86, 0.28,
#'     0.18, 0.84, 0.23
#'   ),
#'   nrow = 6,
#'   byrow = TRUE,
#'   dimnames = list(items_tbl$item_id, NULL)
#' )
#' embed_result <- list(
#'   embeddings = embeddings,
#'   items_tbl = items_tbl,
#'   embed_model = "manual-example",
#'   embedding_diagnostics = list(normalized = FALSE)
#' )
#' wrapped <- semantica_wrap(embed_result, verbose = FALSE)
#' names(wrapped)
semantica_wrap <- function(embed_result, items_tbl = NULL, id_col = "item_id", factor_col = "factor",
                           min_items_per_factor = 3L,
                           cosine_adjustment = c("none", "mean_center"),
                           semantic_calibration = NULL,
                           compute_cosine_sensitivity = TRUE,
                           verbose = TRUE) {
  cosine_adjustment <- match.arg(cosine_adjustment)
  itbl <- items_tbl %||% embed_result$items_tbl; emb <- embed_result$embeddings
  if (!is.data.frame(itbl)) stop("'items_tbl' must be a data.frame or tibble.")
  if (!id_col %in% names(itbl) && "ID" %in% names(itbl)) id_col <- "ID"
  if (!factor_col %in% names(itbl) && "Dimension" %in% names(itbl)) factor_col <- "Dimension"
  missing_cols <- setdiff(c(id_col, factor_col), names(itbl))
  if (length(missing_cols) > 0L) stop("Missing required item column(s): ", paste(missing_cols, collapse = ", "))
  if (is.null(rownames(emb))) stop("'embed_result$embeddings' must have item IDs as row names.")
  common_ids <- intersect(as.character(itbl[[id_col]]), rownames(emb))
  if (!length(common_ids)) stop("No matching item IDs.")
  if (length(common_ids) < nrow(itbl)) warning(sprintf("%d items dropped (no embedding).", nrow(itbl)-length(common_ids)))

  itbl_a <- itbl[match(common_ids, as.character(itbl[[id_col]])), ]; emb_a <- emb[common_ids, , drop=FALSE]
  factor_counts <- table(as.character(itbl_a[[factor_col]]))
  small <- names(factor_counts[factor_counts < min_items_per_factor])
  if (length(small)) { warning(sprintf("Dropping factor(s) with < %d items: %s", min_items_per_factor, paste(small, collapse=", "))); ok <- !as.character(itbl_a[[factor_col]]) %in% small; itbl_a <- itbl_a[ok, ]; emb_a <- emb_a[ok, , drop=FALSE]; common_ids <- common_ids[ok] }
  if (nrow(itbl_a) < 4L) stop("Fewer than 4 items remain.")

  factors_v <- as.character(itbl_a[[factor_col]]); factors_u <- unique(factors_v)
  emb_norms <- sqrt(rowSums(emb_a^2))
  if (any(!is.finite(emb_norms) | emb_norms <= .Machine$double.eps)) {
    stop("Embedding matrix contains zero or invalid vectors.")
  }
  normalized_flag <- embed_result$embedding_diagnostics$normalized
  already_normalized <- if (is.null(normalized_flag) || is.na(normalized_flag)) {
    all(abs(emb_norms - 1) < 1e-4)
  } else {
    isTRUE(normalized_flag)
  }
  if (already_normalized && any(abs(emb_norms - 1) > 1e-3)) {
    # Trust the vectors over stale metadata: cosine similarity requires L2 rows.
    warning("Embedding diagnostics indicate normalized vectors, but row norms deviate from 1; renormalizing for cosine similarity.", call. = FALSE)
    already_normalized <- FALSE
  }
  factor_lookup <- stats::setNames(factors_v, common_ids)
  cos_mat <- .compute_cosine_matrix(emb_a, already_normalized = already_normalized, adjustment = cosine_adjustment)
  rownames(cos_mat) <- colnames(cos_mat) <- common_ids; storage.mode(cos_mat) <- "double"
  cosine_adjustment_sensitivity <- if (isTRUE(compute_cosine_sensitivity)) {
    .compute_cosine_adjustment_sensitivity(
      emb_a,
      already_normalized = already_normalized,
      factor_assignment = factor_lookup,
      standard_cosine = if (cosine_adjustment == "none") cos_mat else NULL,
      centered_cosine = if (cosine_adjustment == "mean_center") cos_mat else NULL
    )
  } else {
    list(
      available = FALSE,
      source = "none_vs_mean_center",
      note = "Cosine-adjustment sensitivity skipped by request."
    )
  }

  item_text <- if ("item_text" %in% names(itbl_a)) as.character(itbl_a[["item_text"]]) else if ("item" %in% names(itbl_a)) as.character(itbl_a[["item"]]) else common_ids
  metadata <- semantica_standardize_item_metadata(itbl_a, id_col = id_col, dimension_col = factor_col, item_col = if ("item_text" %in% names(itbl_a)) "item_text" else if ("item" %in% names(itbl_a)) "item" else id_col)
  calibration <- .apply_semantic_calibration(
    cos_mat,
    semantic_calibration = semantic_calibration,
    item_metadata = metadata,
    factor_assignment = factor_lookup
  )
  cos_mat <- calibration$matrix
  cosine_diagnostics <- .cosine_diagnostics(cos_mat, factor_lookup)
  df_s <- data.frame(item = common_ids, type = factors_v, factor = factors_v, item_text = item_text,
                     ID = metadata$ID, Dimension = metadata$Dimension, Facet = metadata$Facet,
                     stringsAsFactors = FALSE)
  rownames(df_s) <- common_ids
  counts <- table(factors_v)
  i_per_f <- setNames(pmin(3L, as.integer(counts[factors_u])), factors_u)
  if (verbose) {
    cat("============================================================\nSTEP 4 -- SEMANTICA INPUT WRAPPER\n============================================================\n")
    cat(sprintf("  Cosine adjustment: %s\n", cosine_adjustment))
    cat(sprintf("  Cosine offdiag   : mean %.4f | q95 %.4f | max %.4f\n",
                cosine_diagnostics$offdiag_mean,
                cosine_diagnostics$offdiag_q95,
                cosine_diagnostics$offdiag_max))
    if (isTRUE(cosine_diagnostics$possible_anisotropy)) {
      cat("  Note             : high mean cosine; consider cosine_adjustment = 'mean_center' as a sensitivity check.\n")
    }
    if (isTRUE(cosine_adjustment_sensitivity$available)) {
      cat(sprintf("  Cosine sensitivity: none vs mean_center offdiag r=%.3f | q95 |delta|=%.3f | high-pair J=%.3f\n",
                  cosine_adjustment_sensitivity$offdiag_correlation,
                  cosine_adjustment_sensitivity$q95_abs_delta,
                  cosine_adjustment_sensitivity$high_pair_jaccard))
    }
    if (isTRUE(calibration$info$applied)) {
      cat(sprintf("  Calibration      : %s semantic proxy calibration applied before ACO/ESEM.\n",
                  calibration$info$method %||% "external"))
    }
  }
  list(cosine_sim_matrix = cos_mat, df = df_s, items_tbl = itbl_a, item_metadata = metadata,
       generated_item_metadata = metadata, n_items = nrow(itbl_a), n_factors = length(factors_u),
       i.per.f = i_per_f, embed_model = embed_result$embed_model,
       embedding_diagnostics = embed_result$embedding_diagnostics %||% list(
         model = embed_result$embed_model %||% "unknown",
         n_items = nrow(emb_a),
         embed_dim = ncol(emb_a),
         expected_dim = .expected_embedding_dim(embed_result$embed_model),
         normalized = already_normalized,
         warnings = character(0L)
       ),
       cosine_diagnostics = cosine_diagnostics,
       cosine_adjustment_sensitivity = cosine_adjustment_sensitivity,
       semantic_calibration = calibration$info,
       cosine_adjustment = cosine_adjustment)
}

# =================================================================
# P8  ONE-SHOT PIPELINE
# =================================================================
#' Run all four pipeline steps in sequence
#'
#' @param backend          Generation backend.
#' @param embed_backend    Embedding backend. `NULL` = same as backend.
#' @param base_url         Override host:port for generation.
#' @param embed_base_url   Override host:port for embedding.
#' @param api_key          Generation API key.
#' @param embed_api_key    Embedding API key.
#' @param chat_model       Override default chat model.
#' @param embed_model      Override default embed model.
#' @param embed_batch_size Items per embedding backend request. Smaller values
#'   reduce per-request memory pressure for local embedding models.
#' @param cosine_adjustment Cosine preprocessing for embeddings. `"none"`
#'   preserves standard normalized cosine; `"mean_center"` subtracts the pool
#'   centroid before cosine as an anisotropy sensitivity check.
#' @param semantic_calibration Optional matrix or function passed to
#'   `semantica_wrap()` to calibrate the embedding cosine proxy.
#' @param compute_cosine_sensitivity Logical; compute the optional
#'   none-versus-mean-centered cosine sensitivity diagnostic.
#' @param release_local_models Logical; after embedding and wrapping, remove
#'   cached `python_llamacpp` model instances and request garbage collection.
#'   This reduces retained RAM for one-shot local runs at the cost of reloading
#'   models in a subsequent call.
#' @param retain_embeddings Logical; retain the dense embedding matrix in the
#'   returned `embed_result`. Set to `FALSE` after the cosine matrix has been
#'   built when later facet/unit embedding diagnostics are not needed.
#' @param gguf_path        Path to `.gguf` file.
#' @param scale_name       Short scale name.
#' @param scale_description Overall construct description.
#' @param factors          Named list of factor specs.
#' @param n_per_factor     Global items per factor target.
#' @param n_per_factor_override Logical; when an explicit `n_per_factor` is
#'   supplied, override nested factor/facet item counts. If `n_per_factor` is
#'   omitted, item counts stored in `factors` keep their existing precedence.
#' @param verbose          Print progress.
#' @param ...              Further args to `semantica_generate_items()`.
#' @return Named list ready for `ACO_with_ESEM()`.
#' @export
#' @examples
#' \dontrun{
#' factors <- list(
#'   Clarity = list(description = "Clear thinking.", n_items = 8L),
#'   Flexibility = list(description = "Adaptive thinking.", n_items = 8L)
#' )
#' prepared <- semantica_pipeline(
#'   backend = "openai",
#'   scale_name = "Cognitive Agility",
#'   scale_description = "Adaptive, clear thinking.",
#'   factors = factors,
#'   embed_batch_size = 32L,
#'   cosine_adjustment = "none",
#'   verbose = FALSE
#' )
#' }
semantica_pipeline <- function(backend = "openai", embed_backend = NULL, base_url = NULL, embed_base_url = NULL, api_key = NULL,
                               embed_api_key = NULL, chat_model = NULL, embed_model = NULL, embed_batch_size = 64L,
                               gguf_path = NULL, scale_name,
                               scale_description, factors, n_per_factor = 15L,
                               n_per_factor_override = !missing(n_per_factor),
                               cosine_adjustment = c("none", "mean_center"),
                               semantic_calibration = NULL,
                               compute_cosine_sensitivity = TRUE,
                               release_local_models = FALSE,
                               retain_embeddings = TRUE,
                               verbose = TRUE, ...) {
  cosine_adjustment <- match.arg(cosine_adjustment)
  embed_backend_eff <- embed_backend %||% backend
  if (backend == "anthropic" && is.null(embed_backend)) stop("Anthropic has no embed API. Specify embed_backend.")

  separate_embed <- !identical(embed_backend_eff, backend) || !is.null(embed_api_key) || !is.null(embed_base_url)
  session <- semantica_connect(
    backend = backend, api_key = api_key, chat_model = chat_model,
    embed_model = if (separate_embed) NULL else embed_model,
    base_url = base_url, gguf_path = gguf_path, verbose = verbose
  )

  embed_key <- if (!is.null(embed_api_key)) embed_api_key else if (identical(embed_backend_eff, backend)) api_key else NULL
  embed_session <- if (separate_embed) {
    semantica_connect(
      backend = embed_backend_eff, api_key = embed_key, embed_model = embed_model,
      base_url = embed_base_url %||% base_url, gguf_path = gguf_path, verbose = verbose
    )
  } else NULL
  items_tbl <- semantica_generate_items(
    session, scale_name, scale_description, factors,
    n_per_factor = n_per_factor,
    n_per_factor_override = n_per_factor_override,
    verbose = verbose, ...
  )
  generated_item_metadata <- semantica_standardize_item_metadata(items_tbl)
  embed_result <- semantica_embed(
    items_tbl, session, embed_session,
    batch_size = embed_batch_size,
    verbose = verbose
  )
  wrapped <- semantica_wrap(
    embed_result,
    cosine_adjustment = cosine_adjustment,
    semantic_calibration = semantic_calibration,
    compute_cosine_sensitivity = compute_cosine_sensitivity,
    verbose = verbose
  )
  result <- c(wrapped, list(session = session, embed_session = embed_session, items_tbl_raw = items_tbl,
                            generated_item_metadata = generated_item_metadata, item_metadata = generated_item_metadata,
                            embed_result = embed_result))
  if (!isTRUE(retain_embeddings)) {
    result$embed_result$embeddings <- NULL
  }
  if (isTRUE(release_local_models) &&
      any(c(session$protocol, embed_session$protocol %||% NA_character_) == "python_llamacpp", na.rm = TRUE)) {
    .semantica_clear_python_model_cache()
  }
  result
}

# =================================================================
# P10  UTILITIES
# =================================================================
#' Print generated items to console
#'
#' Displays all generated items grouped by factor, truncating long item text
#' for readability in the console.
#'
#' @param items_tbl Tibble of generated items (output from \code{\link{semantica_generate_items}}).
#' @param max_chars Maximum number of characters to display per item before truncation.
#' @return Invisibly returns the input tibble.
#' @export
#' @examples
#' items_tbl <- data.frame(
#'   item_id = c("A1", "A2"),
#'   factor = c("Attention", "Attention"),
#'   item_text = c("A short item.", "A much longer item that can be truncated.")
#' )
#' semantica_print_items(items_tbl, max_chars = 30)
semantica_print_items <- function(items_tbl, max_chars = 80L) {
  factors_u <- unique(items_tbl$factor)
  cat("\n", rep("=", 72), "\n", sprintf("GENERATED ITEMS (%d items, %d factors)\n", nrow(items_tbl), length(factors_u)), rep("=", 72), "\n", sep = "")
  for (f in factors_u) { f_rows <- items_tbl[items_tbl$factor == f, ]; cat(sprintf("\n  [%s] (%d items)\n", f, nrow(f_rows))); for (i in seq_len(nrow(f_rows))) { txt <- f_rows$item_text[i]; if (nchar(txt) > max_chars) txt <- paste0(substr(txt,1L,max_chars-3L), "..."); cat(sprintf("  %s  %s\n", f_rows$item_id[i], txt)) } }
  invisible(items_tbl)
}

#' Export pipeline results to CSV files
#'
#' Saves the generated item table, metadata dataframe, and cosine similarity
#' matrix to separate CSV files in the working directory for offline use or sharing.
#'
#' @param pipeline_result Output list from \code{\link{semantica_pipeline}} or \code{\link{semantica_wrap}}.
#' @param prefix Character string to prepend to output filenames.
#' @return Invisibly returns the prefix used.
#' @export
#' @examples
#' \donttest{
#' items_tbl <- data.frame(
#'   item_id = paste0("item_", 1:6),
#'   factor = rep(c("A", "B"), each = 3),
#'   item_text = paste("Item", 1:6)
#' )
#' embeddings <- diag(6)
#' rownames(embeddings) <- items_tbl$item_id
#' wrapped <- semantica_wrap(
#'   list(
#'     embeddings = embeddings,
#'     items_tbl = items_tbl,
#'     embed_model = "manual",
#'     embedding_diagnostics = list(normalized = TRUE)
#'   ),
#'   verbose = FALSE
#' )
#' semantica_export(wrapped, prefix = file.path(tempdir(), "example_scale"))
#' }
semantica_export <- function(pipeline_result, prefix = "SEMANTICA") {
  write.csv(pipeline_result$items_tbl, paste0(prefix,"_items.csv"), row.names=FALSE)
  write.csv(pipeline_result$df, paste0(prefix,"_df.csv"), row.names=FALSE)
  write.csv(pipeline_result$cosine_sim_matrix, paste0(prefix,"_cosine_matrix.csv"))
  cat(sprintf("Exported: %s_items.csv | %s_df.csv | %s_cosine_matrix.csv\n", prefix, prefix, prefix))
  invisible(prefix)
}

#' Reload previously exported pipeline results
#'
#' Reads CSV files created by \code{\link{semantica_export}} back into an R
#' list ready for the ACO optimizer.
#'
#' @param prefix Character string matching the prefix used during export.
#' @param i.per.f Optional named integer vector overriding the number of items
#'   to select per factor after reload.
#' @param default_i_per_f Default number of items to select per factor when
#'   `i.per.f` is not supplied; capped at the number of available items.
#' @return Named list containing \code{cosine_sim_matrix}, \code{df},
#'   \code{items_tbl}, and \code{i.per.f}.
#' @export
#' @examples
#' \dontrun{
#' # Following semantica_export(wrapped, prefix = "example_scale"):
#' reloaded <- semantica_reload(
#'   prefix = "example_scale",
#'   i.per.f = c(Clarity = 3L, Flexibility = 3L),
#'   default_i_per_f = 3L
#' )
#' }
semantica_reload <- function(prefix = "SEMANTICA", i.per.f = NULL, default_i_per_f = 3L) {
  items_tbl <- read.csv(paste0(prefix, "_items.csv"), stringsAsFactors=FALSE)
  df <- read.csv(paste0(prefix, "_df.csv"), stringsAsFactors=FALSE)
  cos_mat <- as.matrix(read.csv(paste0(prefix, "_cosine_matrix.csv"), row.names=1, stringsAsFactors=FALSE))
  storage.mode(cos_mat) <- "double"
  counts <- table(df$type)
  if (is.null(i.per.f)) {
    # `i.per.f` is a selection target, not the number of available items.
    i_per_f <- setNames(pmin(as.integer(default_i_per_f), as.integer(counts)), names(counts))
  } else {
    i_per_f <- i.per.f
  }
  list(cosine_sim_matrix=cos_mat, df=df, items_tbl=tibble::as_tibble(items_tbl), i.per.f=i_per_f)
}

#' List all available backends
#'
#' Prints a compact registry of SEMANTICA generation and embedding backends.
#'
#' @return Invisibly returns `SEMANTICA_BACKENDS`.
#' @export
#' @examples
#' backends <- semantica_list_backends()
#' names(backends)
semantica_list_backends <- function() {
  cat("\nSEMANTICA v2 -- available backends\n", rep("=", 70), "\n", sep="")
  fmt <- "  %-20s  %-18s  %-28s  %s\n"
  cat(sprintf(fmt, "BACKEND", "PROTOCOL", "DEFAULT CHAT MODEL", "EMBED?"))
  for (k in names(SEMANTICA_BACKENDS)) { b <- SEMANTICA_BACKENDS[[k]]; cat(sprintf(fmt, k, b$protocol, b$default_chat_model %||% "(user-specified)", if (isTRUE(b$has_embed)) "YES" else "NO")) }
  cat("For custom servers pass backend='generic_openai' and base_url='http://...'.\n\n")
  invisible(SEMANTICA_BACKENDS)
}
