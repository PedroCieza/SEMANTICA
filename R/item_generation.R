# SEMANTICA item generation and embedding helpers.

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

# Avoid R CMD check notes for non-standard evaluation
utils::globalVariables(c("item_id", "factor", "item_text", "attempt", "ID", "Dimension", "Facet", "item"))

# =================================================================
# P0  CONDA / PYTHON ENVIRONMENT
# =================================================================
.semantica_verify_conda_torch <- function(env_name, conda, install_llamacpp = TRUE) {
  python <- tryCatch(
    reticulate::conda_python(envname = env_name, conda = conda),
    error = function(e) NULL
  )
  if (is.null(python) || !file.exists(python)) {
    return(list(available = FALSE, error = "Conda Python executable was not found."))
  }
  probe_file <- tempfile("semantica-python-probe-", fileext = ".py")
  on.exit(unlink(probe_file, force = TRUE), add = TRUE)
  imports <- c(
    "torch", "transformers", "sentence_transformers", "accelerate",
    "einops", "numpy", "scipy"
  )
  if (isTRUE(install_llamacpp)) imports <- c(imports, "llama_cpp")
  probe <- c(
    "import importlib, platform, torch",
    sprintf("mods = %s", paste0("[", paste(sprintf("'%s'", imports), collapse = ","), "]")),
    "loaded = {name: importlib.import_module(name) for name in mods}",
    "cuda = bool(torch.cuda.is_available())",
    "mps_obj = getattr(torch.backends, 'mps', None)",
    "mps = bool(mps_obj is not None and mps_obj.is_available())",
    "names = [torch.cuda.get_device_name(i) for i in range(torch.cuda.device_count())] if cuda else []",
    "print('python\\t' + platform.python_version())",
    "print('torch_version\\t' + str(torch.__version__))",
    "print('cuda_available\\t' + str(cuda).lower())",
    "print('cuda_runtime\\t' + str(torch.version.cuda or ''))",
    "print('gpu_names\\t' + '||'.join(names))",
    "print('mps_available\\t' + str(mps).lower())",
    "print('package_versions\\t' + '||'.join(name + '=' + str(getattr(module, '__version__', 'unknown')) for name, module in loaded.items()))"
  )
  writeLines(probe, probe_file, useBytes = TRUE)
  output <- tryCatch(
    system2(python, shQuote(probe_file), stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    return(list(
      available = FALSE,
      python = python,
      error = paste(as.character(output), collapse = "\n")
    ))
  }
  fields <- strsplit(as.character(output), "\t", fixed = TRUE)
  fields <- fields[lengths(fields) >= 2L]
  values <- setNames(
    vapply(fields, function(x) paste(x[-1L], collapse = "\t"), character(1L)),
    vapply(fields, `[[`, character(1L), 1L)
  )
  package_parts <- if (nzchar(values[["package_versions"]] %||% "")) {
    strsplit(values[["package_versions"]], "||", fixed = TRUE)[[1L]]
  } else {
    character(0L)
  }
  package_versions <- if (length(package_parts) > 0L) {
    names_out <- sub("=.*$", "", package_parts)
    values_out <- sub("^[^=]*=", "", package_parts)
    stats::setNames(values_out, names_out)
  } else {
    character(0L)
  }
  list(
    available = TRUE,
    python = python,
    python_version = unname(values[["python"]] %||% NA_character_),
    torch_version = unname(values[["torch_version"]] %||% NA_character_),
    cuda_available = identical(values[["cuda_available"]], "true"),
    cuda_runtime = unname(values[["cuda_runtime"]] %||% NA_character_),
    gpu_names = if (nzchar(values[["gpu_names"]] %||% "")) {
      strsplit(values[["gpu_names"]], "||", fixed = TRUE)[[1L]]
    } else {
      character(0L)
    },
    mps_available = identical(values[["mps_available"]], "true"),
    package_versions = package_versions
  )
}

.semantica_validate_conda_setup_verification <- function(
    verification,
    accelerator = c("cpu", "cuda")) {
  accelerator <- match.arg(accelerator)
  if (!is.list(verification) || !isTRUE(verification$available)) {
    detail <- if (is.list(verification)) {
      verification$error %||% "unknown error"
    } else {
      "verification did not return a result list"
    }
    stop(
      "Python environment verification failed: ",
      detail,
      call. = FALSE
    )
  }
  if (identical(accelerator, "cuda") &&
      !isTRUE(verification$cuda_available)) {
    stop(
      paste(
        "CUDA environment verification failed:",
        "accelerator='cuda' was requested, but the installed Python",
        "PyTorch runtime reports torch.cuda.is_available() = FALSE.",
        "Check the selected wheel index, GPU driver, and CUDA runtime;",
        "SEMANTICA will not report this setup as successful."
      ),
      call. = FALSE
    )
  }
  invisible(verification)
}

.semantica_conda_path_like <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    return(FALSE)
  }
  value <- trimws(as.character(value))
  grepl("[/\\\\]", value) ||
    grepl("^[A-Za-z]:", value) ||
    startsWith(value, ".") ||
    startsWith(value, "~")
}

.semantica_normalize_conda_path <- function(path) {
  if (is.null(path)) return(character(0L))
  vapply(as.character(path), function(value) {
    if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
    value <- path.expand(trimws(value))
    normalized <- tryCatch(
      normalizePath(value, winslash = "/", mustWork = FALSE),
      error = function(e) gsub("\\\\", "/", value)
    )
    normalized <- gsub("\\\\", "/", normalized)
    if (!identical(normalized, "/") &&
        !grepl("^[A-Za-z]:/$", normalized)) {
      normalized <- sub("/+$", "", normalized)
    }
    if (identical(.Platform$OS.type, "windows")) {
      normalized <- tolower(normalized)
    }
    normalized
  }, character(1L), USE.NAMES = FALSE)
}

.semantica_conda_prefix_from_executable <- function(executable) {
  normalized <- .semantica_normalize_conda_path(executable)
  vapply(normalized, function(path) {
    if (is.na(path)) return(NA_character_)
    parent <- dirname(path)
    if (tolower(basename(parent)) %in% c("bin", "scripts", "condabin")) {
      parent <- dirname(parent)
    }
    .semantica_normalize_conda_path(parent)[[1L]]
  }, character(1L), USE.NAMES = FALSE)
}

.semantica_conda_reference_names <- function(reference) {
  if (is.null(reference)) return(character(0L))
  names_out <- vapply(as.character(reference), function(value) {
    if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
    value <- trimws(value)
    if (.semantica_conda_path_like(value)) {
      value <- basename(.semantica_normalize_conda_path(value)[[1L]])
    }
    tolower(value)
  }, character(1L), USE.NAMES = FALSE)
  unique(names_out[!is.na(names_out) & nzchar(names_out)])
}

.semantica_conda_environment_prefixes <- function(existing) {
  if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0L) {
    return(character(0L))
  }
  prefixes <- rep(NA_character_, nrow(existing))
  if ("prefix" %in% names(existing)) {
    prefixes <- .semantica_normalize_conda_path(existing$prefix)
  }
  if ("python" %in% names(existing)) {
    missing <- is.na(prefixes) | !nzchar(prefixes)
    if (any(missing)) {
      prefixes[missing] <- .semantica_conda_prefix_from_executable(
        existing$python[missing]
      )
    }
  }
  prefixes
}

.semantica_conda_environment_matches <- function(env_name, existing) {
  if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0L) {
    return(integer(0L))
  }
  matches <- integer(0L)
  if ("name" %in% names(existing)) {
    target_name <- as.character(env_name)
    existing_names <- as.character(existing$name)
    if (identical(.Platform$OS.type, "windows")) {
      target_name <- tolower(target_name)
      existing_names <- tolower(existing_names)
    }
    matches <- which(!is.na(existing_names) & existing_names == target_name)
  }
  if (.semantica_conda_path_like(env_name)) {
    target_path <- .semantica_normalize_conda_path(env_name)[[1L]]
    prefixes <- .semantica_conda_environment_prefixes(existing)
    matches <- union(
      matches,
      which(!is.na(prefixes) & prefixes == target_path)
    )
  }
  as.integer(matches)
}

.semantica_assert_safe_conda_recreate <- function(
    env_name,
    existing = NULL,
    active_env = "",
    active_prefix = "",
    active_python = "",
    conda_bin = NULL) {
  matches <- .semantica_conda_environment_matches(env_name, existing)
  matched_names <- if (length(matches) > 0L && "name" %in% names(existing)) {
    as.character(existing$name[matches])
  } else {
    character(0L)
  }
  target_names <- unique(c(
    .semantica_conda_reference_names(env_name),
    .semantica_conda_reference_names(matched_names)
  ))
  target_paths <- character(0L)
  if (.semantica_conda_path_like(env_name)) {
    target_paths <- .semantica_normalize_conda_path(env_name)
  }
  prefixes <- .semantica_conda_environment_prefixes(existing)
  if (length(matches) > 0L && length(prefixes) > 0L) {
    target_paths <- c(target_paths, prefixes[matches])
  }
  target_paths <- unique(target_paths[!is.na(target_paths)])

  protected_paths <- character(0L)
  if (!is.null(existing) && is.data.frame(existing) &&
      nrow(existing) > 0L && "name" %in% names(existing)) {
    protected_rows <- which(tolower(as.character(existing$name)) %in%
                              c("base", "root"))
    if (length(protected_rows) > 0L && length(prefixes) > 0L) {
      protected_paths <- c(protected_paths, prefixes[protected_rows])
    }
  }
  if (!is.null(conda_bin) && .semantica_conda_path_like(conda_bin)) {
    protected_paths <- c(
      protected_paths,
      .semantica_conda_prefix_from_executable(conda_bin)
    )
  }
  protected_paths <- unique(protected_paths[!is.na(protected_paths)])

  active_names <- character(0L)
  active_paths <- character(0L)
  if (length(active_env) > 0L && !is.na(active_env[[1L]]) &&
      nzchar(trimws(active_env[[1L]]))) {
    if (.semantica_conda_path_like(active_env[[1L]])) {
      active_paths <- c(
        active_paths,
        .semantica_normalize_conda_path(active_env[[1L]])
      )
    }
    active_names <- c(
      active_names,
      .semantica_conda_reference_names(active_env[[1L]])
    )
  }
  if (length(active_prefix) > 0L && !is.na(active_prefix[[1L]]) &&
      nzchar(trimws(active_prefix[[1L]]))) {
    active_paths <- c(
      active_paths,
      .semantica_normalize_conda_path(active_prefix[[1L]])
    )
    active_names <- c(
      active_names,
      .semantica_conda_reference_names(active_prefix[[1L]])
    )
  }
  if (length(active_python) > 0L && !is.na(active_python[[1L]]) &&
      nzchar(trimws(active_python[[1L]]))) {
    active_paths <- c(
      active_paths,
      .semantica_conda_prefix_from_executable(active_python[[1L]])
    )
    active_names <- c(
      active_names,
      .semantica_conda_reference_names(
        .semantica_conda_prefix_from_executable(active_python[[1L]])
      )
    )
  }
  active_names <- unique(active_names)
  active_paths <- unique(active_paths[!is.na(active_paths)])

  protected_name <- any(target_names %in% c("base", "root"))
  active_name <- length(intersect(target_names, active_names)) > 0L
  protected_path <- length(intersect(target_paths, protected_paths)) > 0L
  active_path <- length(intersect(target_paths, active_paths)) > 0L
  if (protected_name || active_name || protected_path || active_path) {
    stop(
      "Refusing to remove the base/root or currently active Conda environment. ",
      "Choose a dedicated inactive environment name or prefix.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Bootstrap a Conda environment for SEMANTICA Python backends
#'
#' @param env_name   Name of the Conda environment (default `"semantica"`).
#' @param conda      Path to the `conda` executable. `NULL` = auto-detect.
#' @param python_ver Python version string (default `"3.11"`).
#' @param packages   Character vector of additional `pip` packages to install.
#' @param accelerator Python PyTorch installation profile. `"cpu"` is the
#'   reproducible compatibility default. `"auto"` currently resolves to CPU
#'   and reports that decision; `"cuda"` requires an explicit
#'   `torch_index_url` appropriate for the host CUDA/runtime combination.
#' @param torch_index_url Optional PyTorch wheel index. This is required for
#'   `accelerator = "cuda"`; for CPU on Windows/Linux, SEMANTICA uses the
#'   official CPU wheel index unless overridden.
#' @param install_llamacpp Logical; install `llama-cpp-python`. Its optional GPU
#'   build flags are platform-specific and are not inferred by this function.
#' @param verify Logical; probe the installed Python PyTorch runtime without
#'   loading a language or embedding model.
#' @param force      Recreate the environment even if it already exists.
#' @param verbose    Print progress messages.
#'
#' @details
#' Core packages use the tested version ranges recorded in
#' `inst/python/requirements-compatible.txt`. CUDA installation is never
#' selected merely because a GPU appears to be present.
#' After calling this function, restart your R session and call
#' `semantica_activate_conda()` to attach the environment.
#'
#' @section Side effects:
#' Creates/removes Conda environments when requested, installs Python packages,
#' may access package indexes over the network, and probes the local Python/PyTorch
#' runtime.
#'
#' @section Reproducibility:
#' The requested/resolved accelerator and verification metadata are returned as
#' attributes. Exact Python package resolution can still depend on the configured
#' package indexes and their available artifacts.
#'
#' @return Invisibly returns the environment name with setup and verification
#'   metadata stored as attributes.
#' @export
#' @examples
#' \dontrun{
#' semantica_setup_conda(
#'   env_name = "semantica",
#'   python_ver = "3.11",
#'   packages = character(0L),
#'   accelerator = "cpu",
#'   force = FALSE
#' )
#' }
semantica_setup_conda <- function(env_name = "semantica", conda = NULL,
                                  python_ver = "3.11", packages = character(0L),
                                  accelerator = c("cpu", "auto", "cuda"),
                                  torch_index_url = NULL,
                                  install_llamacpp = TRUE, verify = TRUE,
                                  force = FALSE, verbose = TRUE) {
  accelerator_requested <- match.arg(accelerator)
  accelerator_resolved <- if (accelerator_requested == "auto") "cpu" else accelerator_requested
  env_name <- trimws(as.character(env_name[1L]))
  if (length(env_name) != 1L || is.na(env_name) || !nzchar(env_name)) {
    stop("'env_name' must be one non-empty Conda environment name.")
  }
  if (isTRUE(force)) {
    # Perform the name/basename checks before discovering Conda so an obviously
    # unsafe request cannot initialize tooling or reach any mutating operation.
    .semantica_assert_safe_conda_recreate(
      env_name = env_name,
      active_env = Sys.getenv("CONDA_DEFAULT_ENV", unset = ""),
      active_prefix = Sys.getenv("CONDA_PREFIX", unset = "")
    )
  }
  if (accelerator_resolved == "cuda" &&
      (is.null(torch_index_url) || !nzchar(trimws(torch_index_url)))) {
    stop(
      "'accelerator = \"cuda\"' requires 'torch_index_url' for a PyTorch ",
      "wheel index compatible with this operating system and CUDA runtime. ",
      "SEMANTICA will not guess or silently install a CUDA stack."
    )
  }
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
    cat(sprintf("  Accelerator : %s%s\n", accelerator_resolved,
                if (accelerator_requested == "auto") " (auto resolved conservatively)" else ""))
  }

  existing <- tryCatch(reticulate::conda_list(conda = conda_bin), error = function(e) NULL)
  environment_matches <- .semantica_conda_environment_matches(
    env_name,
    existing
  )
  env_exists <- length(environment_matches) > 0L
  if (isTRUE(force)) {
    active_python <- ""
    python_is_active <- isTRUE(tryCatch(
      reticulate::py_available(initialize = FALSE),
      error = function(e) FALSE
    ))
    if (python_is_active) {
      active_python <- tryCatch(
        as.character(reticulate::py_config()$python %||% ""),
        error = function(e) ""
      )
    }
    .semantica_assert_safe_conda_recreate(
      env_name = env_name,
      existing = existing,
      active_env = Sys.getenv("CONDA_DEFAULT_ENV", unset = ""),
      active_prefix = Sys.getenv("CONDA_PREFIX", unset = ""),
      active_python = active_python,
      conda_bin = conda_bin
    )
  }

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

  torch_index_eff <- torch_index_url
  if (is.null(torch_index_eff) && accelerator_resolved == "cpu" &&
      !identical(Sys.info()[["sysname"]], "Darwin")) {
    torch_index_eff <- "https://download.pytorch.org/whl/cpu"
  }
  core_pkgs <- c(
    "transformers>=4.40,<6", "sentence-transformers>=3,<6",
    "accelerate>=0.30,<2", "einops>=0.7,<1", "numpy>=1.26,<3",
    "scipy>=1.11,<2"
  )
  if (isTRUE(install_llamacpp)) {
    core_pkgs <- c(core_pkgs, "llama-cpp-python>=0.2.90,<1")
  }
  all_pkgs <- unique(c(core_pkgs, packages))
  if (verbose) cat(sprintf("  Installing PyTorch (%s profile)...\n", accelerator_resolved))

  torch_options <- if (!is.null(torch_index_eff)) c("--index-url", torch_index_eff) else character(0L)
  tryCatch(
    reticulate::conda_install(
      envname = env_name, packages = "torch>=2.2,<3", pip = TRUE,
      pip_options = torch_options, conda = conda_bin
    ),
    error = function(e) stop("PyTorch installation failed: ", conditionMessage(e))
  )
  if (verbose) cat(sprintf("  Installing %d compatible Python package(s) (pip)...\n", length(all_pkgs)))

  tryCatch(
    reticulate::conda_install(envname = env_name, packages = all_pkgs, pip = TRUE, conda = conda_bin),
    error = function(e) stop("Required Python package installation failed: ", conditionMessage(e))
  )

  verification <- NULL
  if (isTRUE(verify)) {
    verification <- .semantica_verify_conda_torch(
      env_name, conda_bin, install_llamacpp = install_llamacpp
    )
    .semantica_validate_conda_setup_verification(
      verification,
      accelerator = accelerator_resolved
    )
  }

  if (verbose) {
    cat("  Installation complete.\n")
    if (!is.null(verification) && isTRUE(verification$available)) {
      cat(sprintf("  PyTorch     : %s\n", verification$torch_version))
      cat(sprintf("  CUDA        : %s%s\n", verification$cuda_available,
                  if (!is.na(verification$cuda_runtime %||% NA_character_)) paste0(" (runtime ", verification$cuda_runtime, ")") else ""))
      cat(sprintf("  GPU(s)      : %s\n", if (length(verification$gpu_names)) paste(verification$gpu_names, collapse = ", ") else "none reported"))
      cat(sprintf("  MPS         : %s\n", verification$mps_available))
      if (length(verification$package_versions) > 0L) {
        cat(sprintf(
          "  Packages    : %s\n",
          paste(
            names(verification$package_versions),
            verification$package_versions,
            sep = "=", collapse = ", "
          )
        ))
      }
    } else if (!is.null(verification)) {
      cat(sprintf("  Verification: unavailable (%s)\n", verification$error %||% "unknown error"))
    }
    cat(sprintf("  NEXT STEPS:\n  1. Restart your R session.\n  2. Call: semantica_activate_conda('%s')\n  3. Then run your pipeline.\n", env_name))
    cat("============================================================\n\n")
  }
  out <- env_name
  attr(out, "accelerator_requested") <- accelerator_requested
  attr(out, "accelerator_resolved") <- accelerator_resolved
  attr(out, "verification") <- verification
  invisible(out)
}

#' Activate a Conda environment for use in the current R session
#'
#' @param env_name   Name of the Conda environment.
#' @param conda      Path to `conda` executable. `NULL` = auto-detect.
#' @param verbose    Print confirmation.
#' @section Side effects:
#' Selects a Conda/Python environment for the current R session and may initialize
#' Python through `reticulate`.
#'
#' @section Reproducibility:
#' The active Python executable is environment-dependent; record the environment
#' and package versions when reproducing a local-model analysis.
#'
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
                embed_dim = 1536L, auth_header = "Bearer", auth_env = "OPENAI_API_KEY", extra_headers = NULL,
                has_embed = TRUE, supports_structured_output = TRUE),
  anthropic = list(label = "Anthropic API (Claude)", protocol = "anthropic", chat_url = "https://api.anthropic.com/v1/messages",
                   embed_url = NULL, default_chat_model = "claude-opus-4-5", default_embed_model = NULL, embed_dim = NA,
                   auth_header = "x-api-key", auth_env = "ANTHROPIC_API_KEY", extra_headers = list("anthropic-version" = "2023-06-01"),
                   has_embed = FALSE, supports_structured_output = FALSE),
  groq = list(label = "Groq API", protocol = "openai_compat", chat_url = "https://api.groq.com/openai/v1/chat/completions",
              embed_url = NULL, default_chat_model = "llama-3.3-70b-versatile",
              default_embed_model = NULL, embed_dim = NA_integer_, auth_header = "Bearer", auth_env = "GROQ_API_KEY",
              extra_headers = NULL, has_embed = FALSE, supports_structured_output = TRUE),
  ollama = list(label = "Ollama (local)", protocol = "ollama", chat_url = "http://localhost:11434/api/chat",
                embed_url = "http://localhost:11434/api/embed", default_chat_model = "llama3.2", default_embed_model = "nomic-embed-text",
                embed_dim = NA_integer_, auth_header = NULL, auth_env = NULL, extra_headers = NULL,
                has_embed = TRUE, supports_structured_output = TRUE),
  llamacpp = list(label = "llama.cpp server", protocol = "openai_compat", chat_url = "http://localhost:8080/v1/chat/completions",
                  embed_url = "http://localhost:8080/v1/embeddings", default_chat_model = "local-model", default_embed_model = "local-model",
                  embed_dim = NA, auth_header = NULL, auth_env = NULL, extra_headers = NULL, has_embed = TRUE, supports_structured_output = FALSE),
  generic_openai = list(label = "Generic OpenAI-compatible", protocol = "openai_compat", chat_url = "http://localhost:1234/v1/chat/completions",
                        embed_url = "http://localhost:1234/v1/embeddings", default_chat_model = "local-model", default_embed_model = "local-model",
                        embed_dim = NA, auth_header = NULL, auth_env = NULL, extra_headers = NULL, has_embed = TRUE, supports_structured_output = FALSE),
  python_hf = list(label = "HuggingFace Transformers (Conda)", protocol = "python_hf", chat_url = NULL, embed_url = NULL,
                   default_chat_model = "meta-llama/Llama-3.2-1B-Instruct", default_embed_model = "sentence-transformers/all-MiniLM-L6-v2",
                   embed_dim = 384L, auth_header = NULL, auth_env = "HF_TOKEN", extra_headers = NULL, has_embed = TRUE, supports_structured_output = FALSE),
  python_llamacpp = list(label = "llama-cpp-python (GGUF)", protocol = "python_llamacpp", chat_url = NULL, embed_url = NULL,
                         default_chat_model = NULL, default_embed_model = NULL, embed_dim = NA, auth_header = NULL, auth_env = NULL,
                         extra_headers = NULL, has_embed = TRUE, supports_structured_output = FALSE)
)


#' Define an explicit custom SEMANTICA backend contract
#'
#' Creates an immutable transport/capability specification for a custom service
#' that uses one of SEMANTICA's supported wire protocols. This avoids provider-
#' name guessing and keeps provider transport metadata outside the analysis
#' core.
#'
#' @param protocol Supported transport protocol: `"openai_compat"`,
#'   `"anthropic"`, `"ollama"`, `"python_hf"`, or `"python_llamacpp"`.
#' @param label Human-readable backend label.
#' @param chat_url,embed_url Explicit service endpoints where applicable.
#' @param can_chat,can_embed Declared capabilities.
#' @param supports_structured_output,supports_batch_embeddings Capability flags.
#' @param default_chat_model,default_embed_model Optional model defaults.
#' @param embed_dim Optional documented embedding dimension; `NA` means unknown.
#' @param auth_header,auth_env Authentication metadata. Credentials themselves
#'   are never stored in the specification.
#' @param extra_headers Optional non-secret protocol headers.
#' @return A validated `semantica_backend_spec` object.
#' @export
semantica_backend_spec <- function(
  protocol, label = "Custom backend", chat_url = NULL, embed_url = NULL,
  can_chat = !is.null(chat_url), can_embed = !is.null(embed_url),
  supports_structured_output = FALSE, supports_batch_embeddings = TRUE,
  default_chat_model = NULL, default_embed_model = NULL, embed_dim = NA_integer_,
  auth_header = NULL, auth_env = NULL, extra_headers = NULL
) {
  protocol <- match.arg(as.character(protocol)[1L],
                        c("openai_compat", "anthropic", "ollama", "python_hf", "python_llamacpp"))
  can_chat <- .semantica_assert_flag(can_chat, "can_chat", condition_class = "semantica_error_config")
  can_embed <- .semantica_assert_flag(can_embed, "can_embed", condition_class = "semantica_error_config")
  supports_structured_output <- .semantica_assert_flag(
    supports_structured_output, "supports_structured_output", condition_class = "semantica_error_config"
  )
  supports_batch_embeddings <- .semantica_assert_flag(
    supports_batch_embeddings, "supports_batch_embeddings", condition_class = "semantica_error_config"
  )
  if (!can_chat && !can_embed) stop("A backend specification must declare at least one capability.")
  if (can_chat && !protocol %in% c("python_hf", "python_llamacpp") && is.null(chat_url)) {
    stop("A chat-capable HTTP backend specification requires 'chat_url'.")
  }
  if (can_embed && !protocol %in% c("python_hf", "python_llamacpp") && is.null(embed_url)) {
    stop("An embedding-capable HTTP backend specification requires 'embed_url'.")
  }
  if (!is.null(extra_headers)) {
    if (!is.list(extra_headers) || is.null(names(extra_headers)) || anyNA(names(extra_headers)) || any(!nzchar(names(extra_headers)))) {
      stop("'extra_headers' must be NULL or a named list of non-secret headers.")
    }
    sensitive_header_names <- c(
      "authorization", "proxy-authorization", "x-api-key", "api-key", "apikey",
      "x-auth-token", "cookie", "set-cookie"
    )
    header_names <- tolower(trimws(names(extra_headers)))
    header_values <- vapply(extra_headers, function(x) paste(as.character(x), collapse = " "), character(1L))
    credential_value <- grepl("^\\s*(bearer|basic)\\s+", header_values, ignore.case = TRUE)
    credential_value[is.na(credential_value)] <- FALSE
    if (any(header_names %in% sensitive_header_names) || any(credential_value)) {
      stop(
        "'extra_headers' must not contain credentials or authentication headers. ",
        "Use 'auth_env' and runtime credential handling instead."
      )
    }
  }
  out <- list(
    label = as.character(label)[1L], protocol = protocol,
    chat_url = chat_url, embed_url = embed_url,
    default_chat_model = default_chat_model, default_embed_model = default_embed_model,
    embed_dim = if (length(embed_dim)) suppressWarnings(as.integer(embed_dim[1L])) else NA_integer_,
    auth_header = auth_header, auth_env = auth_env, extra_headers = extra_headers,
    has_chat = can_chat, has_embed = can_embed,
    supports_structured_output = supports_structured_output,
    supports_batch_embeddings = supports_batch_embeddings,
    explicit_custom_contract = TRUE
  )
  class(out) <- c("semantica_backend_spec", "list")
  out
}

.semantica_backend_capabilities <- function(spec) {
  list(
    can_chat = isTRUE(spec$has_chat) || (!is.null(spec$chat_url) && nzchar(spec$chat_url)) ||
      spec$protocol %in% c("python_hf", "python_llamacpp"),
    can_embed = isTRUE(spec$has_embed),
    supports_structured_output = isTRUE(spec$supports_structured_output),
    supports_batch_embeddings = isTRUE(spec$has_embed) && isTRUE(spec$supports_batch_embeddings %||% TRUE),
    protocol = spec$protocol
  )
}

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
#' Advanced session constructor for a specific generation or embedding backend.
#' The main workflow accepts `llm`, `chat_model`, and `embed_model` directly
#' to [semantica_run()] and use [semantica_check_setup()] before a long run.
#'
#' @param backend      One of the keys in `SEMANTICA_BACKENDS`. For a custom
#'                     OpenAI-compatible server, use `"generic_openai"` with
#'                     `base_url`, or provide an explicit `backend_spec` for a
#'                     custom backend name. Unknown names never silently inherit
#'                     another provider protocol.
#' @param api_key      API key string. `NULL` = read from environment variable.
#' @param chat_model   Override default chat model name.
#' @param embed_model  Override default embedding model name.
#' @param base_url     Override host:port (required for `generic_openai`).
#' @param gguf_path    Path to a `.gguf` model file (`python_llamacpp` only).
#' @param hf_token     HuggingFace token for gated models (`python_hf` only).
#' @param embedding_device Device requested for local Python embedding models:
#'   `"auto"`, `"cpu"`, `"cuda"`, `"cuda:N"`, or `"mps"`.
#' @param chat_device Device requested for local Python generation. For the
#'   Hugging Face backend, an explicit device is mutually exclusive with an
#'   explicit `device_map`.
#' @param device_map Optional Hugging Face Transformers device map. `NULL`
#'   resolves to `"auto"` only when `chat_device = "auto"`.
#' @param gpu_layers llama.cpp GPU-layer configuration: `"auto"` (all layers),
#'   `0L` (CPU), a positive layer count, or `-1L` (all layers).
#' @param model_precision Reproducibility label for local model precision. It is
#'   included in model cache keys and does not affect R cosine precision.
#' @param timeout_s HTTP timeout in seconds (default `120`).
#' @param retry_max_tries Maximum attempts for retryable HTTP failures.
#' @param retry_on_failure Retry connection-level failures as well as retryable HTTP statuses.
#' @param preflight Check provider/local model availability before a long run when possible.
#' @param purpose Session capability scope: `"auto"`, `"both"`, `"chat"`, or `"embed"`. `"auto"` uses chat-only mode for providers without embeddings and both capabilities otherwise. Embedding-only sessions do not preflight irrelevant chat models.
#' @param embedding_task Embedding task policy. `"auto"` applies model-specific documented instructions when required.
#' @param embedding_instruction Optional explicit prefix/instruction prepended to embedding text; overrides model-specific automatic instructions.
#' @param embedding_spec Optional model capability contract from
#'   [semantica_embedding_spec()]. This changes text/task preparation only; it
#'   never changes psychometric thresholds or score definitions.
#' @param backend_spec Optional explicit custom backend contract created by
#'   [semantica_backend_spec()]. Required when `backend` is not a built-in name.
#' @param verbose      Print connection details and status.
#'
#' @usage semantica_connect(
#'   backend = c(
#'     "openai", "anthropic", "groq", "ollama", "llamacpp", "generic_openai",
#'     "python_hf", "python_llamacpp"
#'   ),
#'   api_key = NULL,
#'   chat_model = NULL,
#'   embed_model = NULL,
#'   base_url = NULL,
#'   gguf_path = NULL,
#'   hf_token = NULL,
#'   embedding_device = "auto",
#'   chat_device = "auto",
#'   device_map = NULL,
#'   gpu_layers = "auto",
#'   model_precision = "auto",
#'   timeout_s = 120L,
#'   retry_max_tries = 4L,
#'   retry_on_failure = TRUE,
#'   preflight = TRUE,
#'   verbose = TRUE,
#'   purpose = c("auto", "both", "chat", "embed"),
#'   embedding_task = "auto",
#'   embedding_instruction = NULL,
#'   embedding_spec = NULL,
#'   backend_spec = NULL
#' )
#' @section Side effects:
#' Reads provider credentials from environment variables when explicit keys are
#' absent, may initialize Python for local backends, and may perform provider/model
#' preflight network or local-runtime checks.
#'
#' @section Reproducibility:
#' The returned session records resolved backend/model/device metadata but never
#' guarantees that a remote provider alias identifies an immutable model revision.
#' Prefer pinned local artifacts or recorded provider revisions when available.
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
                              hf_token = NULL,
                              embedding_device = "auto", chat_device = "auto",
                              device_map = NULL, gpu_layers = "auto",
                              model_precision = "auto",
                              timeout_s = 120L, retry_max_tries = 4L,
                              retry_on_failure = TRUE, preflight = TRUE,
                              verbose = TRUE,
                              purpose = c("auto", "both", "chat", "embed"),
                              embedding_task = "auto", embedding_instruction = NULL,
                              embedding_spec = NULL,
                              backend_spec = NULL) {
  if (length(backend) != 1L) backend <- backend[[1L]]
  backend <- as.character(backend)
  if (!nzchar(backend)) stop("'backend' must be a non-empty string.")

  normalize_local_device <- function(x, arg) {
    x <- tolower(trimws(as.character(x[1L])))
    if (length(x) != 1L || is.na(x) ||
        !grepl("^(auto|cpu|cuda(:[0-9]+)?|mps)$", x)) {
      stop("'", arg, "' must be one of 'auto', 'cpu', 'cuda', 'cuda:N', or 'mps'.")
    }
    x
  }
  embedding_device <- normalize_local_device(embedding_device, "embedding_device")
  chat_device <- normalize_local_device(chat_device, "chat_device")
  if (!is.null(device_map) && chat_device != "auto") {
    stop("Use either an explicit 'chat_device' or 'device_map', not both.")
  }
  device_map_resolved <- if (!is.null(device_map)) {
    device_map
  } else if (chat_device == "auto") {
    "auto"
  } else {
    NULL
  }
  if (is.character(gpu_layers) && length(gpu_layers) == 1L &&
      identical(tolower(gpu_layers), "auto")) {
    gpu_layers_resolved <- -1L
  } else {
    gpu_layers_resolved <- suppressWarnings(as.integer(gpu_layers[1L]))
    if (length(gpu_layers_resolved) != 1L || !is.finite(gpu_layers_resolved) ||
        gpu_layers_resolved < -1L) {
      stop("'gpu_layers' must be 'auto', -1L, 0L, or a positive integer.")
    }
  }
  model_precision <- tolower(trimws(as.character(model_precision[1L])))
  if (length(model_precision) != 1L || is.na(model_precision) ||
      !model_precision %in% c("auto", "float32", "float16", "bfloat16")) {
    stop("'model_precision' must be 'auto', 'float32', 'float16', or 'bfloat16'.")
  }
  retry_max_tries <- suppressWarnings(as.integer(retry_max_tries[1L]))
  if (!is.finite(retry_max_tries) || retry_max_tries < 1L) {
    stop("'retry_max_tries' must be a positive integer.")
  }
  retry_on_failure <- isTRUE(retry_on_failure)
  preflight <- isTRUE(preflight)
  purpose_requested <- match.arg(purpose)
  if (!is.null(embedding_spec) && !inherits(embedding_spec, "semantica_embedding_spec")) {
    stop("'embedding_spec' must be NULL or created by semantica_embedding_spec().")
  }

  known <- names(SEMANTICA_BACKENDS)
  if (!is.null(backend_spec)) {
    if (!inherits(backend_spec, "semantica_backend_spec")) {
      stop("'backend_spec' must be created by semantica_backend_spec().")
    }
    if (backend %in% known) {
      stop("Do not combine a registered built-in backend name with 'backend_spec'; use an explicit custom backend name.")
    }
    spec <- unclass(backend_spec)
    spec$label <- spec$label %||% paste0("Custom backend (", backend, ")")
  } else if (!backend %in% known) {
    .semantica_abort(
      sprintf("Backend '%s' is not registered. Use a built-in backend, backend = 'generic_openai', or provide an explicit semantica_backend_spec().", backend),
      subclass = "semantica_error_backend", backend = backend
    )
  } else {
    spec <- SEMANTICA_BACKENDS[[backend]]
  }
  capabilities <- .semantica_backend_capabilities(spec)
  purpose <- if (identical(purpose_requested, "auto")) {
    if (capabilities$can_chat && capabilities$can_embed) "both"
    else if (capabilities$can_chat) "chat" else "embed"
  } else purpose_requested
  if (purpose %in% c("both", "chat") && !capabilities$can_chat) {
    .semantica_abort(sprintf("Backend '%s' is not chat-capable.", backend), subclass = "semantica_error_backend_capability", backend = backend, capability = "chat")
  }
  if (purpose %in% c("both", "embed") && !capabilities$can_embed) {
    .semantica_abort(sprintf("Backend '%s' is not embedding-capable.", backend), subclass = "semantica_error_backend_capability", backend = backend, capability = "embed")
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

  cm <- if (purpose == "embed" && is.null(chat_model)) NULL else chat_model %||% spec$default_chat_model
  em <- if (purpose == "chat" && is.null(embed_model)) NULL else .canonicalize_embedding_model(embed_model %||% spec$default_embed_model)
  py_available <- FALSE
  python_caps <- NULL
  if (spec$protocol %in% c("python_hf", "python_llamacpp")) {
    if (!requireNamespace("reticulate", quietly = TRUE)) stop("Backend '", backend, "' requires 'reticulate'.")
    py_available <- tryCatch(
      isTRUE(reticulate::py_available(initialize = TRUE)),
      error = function(e) FALSE
    )
    if (!py_available) stop("Python not available. Call semantica_activate_conda() first.")
    python_caps <- .semantica_python_capabilities(deep_python = TRUE)
  }

  session <- list(backend = backend, protocol = spec$protocol, label = spec$label, api_key = key, auth_header = spec$auth_header,
                  extra_headers = spec$extra_headers, chat_url = chat_url, embed_url = embed_url, chat_model = cm, embed_model = em,
                  embed_dim = spec$embed_dim, has_chat = capabilities$can_chat, has_embed = capabilities$can_embed,
                  supports_structured_output = capabilities$supports_structured_output,
                  supports_batch_embeddings = capabilities$supports_batch_embeddings,
                  capabilities = capabilities,
                  timeout_s = timeout_s, retry_max_tries = retry_max_tries,
                  retry_on_failure = retry_on_failure, gguf_path = gguf_path, hf_token = hf_tok,
                  embedding_device = embedding_device, chat_device = chat_device,
                  resolved_embedding_device = if (embedding_device == "auto") {
                    "pending_backend_resolution"
                  } else {
                    embedding_device
                  },
                  resolved_chat_device = if (!is.null(device_map_resolved)) {
                    paste0("device_map:", paste(device_map_resolved, collapse = ","))
                  } else {
                    chat_device
                  },
                  device_map = device_map_resolved,
                  gpu_layers = gpu_layers_resolved,
                  gpu_layers_requested = gpu_layers,
                  model_precision = model_precision,
                  device_status = "configured_not_runtime_verified",
                  python_version = python_caps$version %||% NA_character_,
                  python_package_versions = python_caps$package_versions %||%
                    character(0L),
                  torch_version = python_caps$torch_version %||% NA_character_,
                  cuda_available = python_caps$cuda_available %||% NA,
                  mps_available = python_caps$mps_available %||% NA,
                  py_available = py_available, purpose = purpose,
                  embedding_task = embedding_task, embedding_instruction = embedding_instruction,
                  embedding_spec = embedding_spec,
                  verbose = verbose)
  class(session) <- c("semantica_session", "list")

  if (verbose) {
    cat("\n============================================================\nSEMANTICA -- LLM CONNECTION\n============================================================\n")
    cat(sprintf("  Backend       : %s\n  Protocol      : %s\n  Session role  : %s\n  Chat model    : %s\n  Embed model   : %s\n", session$label, session$protocol, purpose, cm %||% "(not requested)", em %||% "(not requested)"))
    safe_url <- function(x) if (exists(".semantica_sanitize_url", mode = "function")) .semantica_sanitize_url(x) else x
    if (!is.null(chat_url)) cat(sprintf("  Chat endpoint : %s\n", safe_url(chat_url)))
    if (!is.null(embed_url)) cat(sprintf("  Embed endpoint: %s\n", safe_url(embed_url)))
    if (spec$protocol %in% c("python_hf", "python_llamacpp")) {
      cat(sprintf("  Chat device   : %s\n  Embed device  : %s\n", chat_device, embedding_device))
      if (spec$protocol == "python_llamacpp") {
        cat(sprintf("  GPU layers    : %d (configured; runtime acceleration not yet verified)\n", gpu_layers_resolved))
      }
    }
  }

  preflight_result <- if (isTRUE(preflight)) {
    tryCatch(
      semantica_backend_preflight(session, verify_models = TRUE, strict = FALSE),
      error = function(e) list(ok = FALSE, reachable = FALSE, warnings = conditionMessage(e))
    )
  } else {
    list(ok = NA, reachable = NA, warnings = "preflight disabled")
  }
  session$preflight <- preflight_result
  if (verbose) {
    status_label <- if (isTRUE(preflight_result$ok)) "PREFLIGHT OK" else if (identical(preflight_result$ok, FALSE)) "PREFLIGHT WARNING" else "PREFLIGHT SKIPPED"
    cat(sprintf("  Status        : %s\n", status_label))
    if (length(preflight_result$warnings %||% character(0L))) {
      for (w in preflight_result$warnings) cat(sprintf("  Note          : %s\n", w))
    }
    if (!isTRUE(spec$has_embed)) {
      cat("  Embeddings    : not provided by this backend; use a separate embed_session/embed_backend.\n")
    }
    cat("============================================================\n\n")
  }
  session
}

#' Print a SEMANTICA backend session
#'
#' @param x A `semantica_session` object returned by [semantica_connect()].
#' @param ... Additional arguments ignored by this method.
#' @return Invisibly returns `x`.
#' @method print semantica_session
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
.build_request <- function(session, url, body_list = NULL) {
  req <- httr2::request(url) |>
    httr2::req_timeout(session$timeout_s %||% 120L) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_retry(
      max_tries = session$retry_max_tries %||% 4L,
      retry_on_failure = isTRUE(session$retry_on_failure %||% TRUE),
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(408L, 425L, 429L, 500L, 502L, 503L, 504L)
      }
    )
  if (!is.null(body_list)) req <- httr2::req_body_json(req, body_list)
  if (!is.null(session$auth_header) && !is.null(session$api_key)) {
    req <- if (session$auth_header == "Bearer") httr2::req_auth_bearer_token(req, session$api_key) else do.call(httr2::req_headers, c(list(req), stats::setNames(list(session$api_key), session$auth_header)))
  }
  if (!is.null(session$extra_headers) && length(session$extra_headers) > 0L) {
    req <- do.call(httr2::req_headers, c(list(req), session$extra_headers))
  }
  req
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
  .call_chat(session, messages = list(list(role = "user", content = "ping")), max_tokens = 1L)
  TRUE
}

#' @keywords internal
.semantica_normalize_generation_seed <- function(seed) {
  if (is.null(seed)) return(NULL)
  if (length(seed) != 1L) {
    stop("'seed' must be NULL or one nonnegative integer.", call. = FALSE)
  }
  seed_num <- suppressWarnings(as.numeric(seed))
  if (length(seed_num) != 1L || !is.finite(seed_num) || seed_num < 0 ||
      seed_num > .Machine$integer.max || abs(seed_num - round(seed_num)) > sqrt(.Machine$double.eps)) {
    stop("'seed' must be NULL or one nonnegative integer.", call. = FALSE)
  }
  as.integer(round(seed_num))
}

.semantica_generation_seed_capability <- function(session) {
  proto <- as.character(session$protocol %||% "unknown")
  supported <- identical(proto, "ollama")
  list(
    supported = supported,
    protocol = proto,
    mechanism = if (supported) "ollama_options_seed" else "not_implemented_for_protocol",
    guarantee = if (supported) "seed_option_supported" else "not_controlled"
  )
}

.semantica_derive_generation_task_seed <- function(master_seed, dimension, facet, attempt, request_n) {
  master_seed <- .semantica_normalize_generation_seed(master_seed)
  if (is.null(master_seed)) return(NA_integer_)
  key <- paste(
    "semantica-generation-task-seed-v1", master_seed,
    enc2utf8(as.character(dimension)), enc2utf8(as.character(facet)),
    as.integer(attempt), as.integer(request_n),
    sep = "\u001f"
  )
  bytes <- as.integer(charToRaw(key))
  # Polynomial modular hashing keeps all intermediate integers far below 2^53,
  # so the result is deterministic under R's exact integer-valued doubles and
  # does not depend on serialized-object bytes or caller RNG state.
  modulus <- 2147483646
  h <- 0
  for (b in bytes) h <- (h * 131 + b + 1) %% modulus
  as.integer(h + 1)
}

#' @keywords internal
.call_chat <- function(session, messages, max_tokens = 2048L, temperature = 0.7, system_prompt = NULL, response_format = NULL, seed = NULL) {
  proto <- session$protocol
  seed <- .semantica_normalize_generation_seed(seed)
  if (proto == "python_hf") return(.py_hf_chat(session, messages, max_tokens, temperature, system_prompt))
  if (proto == "python_llamacpp") return(.py_llamacpp_chat(session, messages, max_tokens, temperature, system_prompt))

  if (proto == "anthropic") {
    body <- list(model = session$chat_model, max_tokens = max_tokens, messages = messages)
    if (!is.null(system_prompt)) body$system <- system_prompt
    resp <- .build_request(session, session$chat_url, body) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
    parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (httr2::resp_status(resp) >= 400L) stop("Anthropic error: ", parsed$error$message %||% httr2::resp_status(resp))
    txt <- parsed$content[[1L]]$text
    if (is.null(txt) || length(txt) == 0L) stop("Empty Anthropic response.")
    return(as.character(txt))
  }

  msgs <- messages
  if (!is.null(system_prompt)) msgs <- c(list(list(role = "system", content = system_prompt)), msgs)
  body <- list(model = session$chat_model, messages = msgs, max_tokens = max_tokens, temperature = temperature)
  if (identical(response_format, "json") && isTRUE(session$supports_structured_output)) {
    if (proto == "ollama") body$format <- "json" else body$response_format <- list(type = "json_object")
  }
  if (proto == "ollama") {
    body$stream <- FALSE
    body$options <- list(temperature = temperature, num_predict = max_tokens)
    if (!is.null(seed)) body$options$seed <- seed
    body$max_tokens <- NULL
  }

  resp <- .build_request(session, session$chat_url, body) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (httr2::resp_status(resp) >= 400L) stop("LLM API error: ", parsed$error$message %||% parsed$error %||% httr2::resp_status(resp))
  txt <- if (proto == "ollama") parsed$message$content else parsed$choices[[1L]]$message$content
  if (is.null(txt) || length(txt) == 0L) stop("Empty response from backend '", session$backend, "'.")
  as.character(txt)
}

#' @keywords internal
.call_embed <- function(session, texts) {
  texts <- .semantica_prepare_embedding_texts(session, texts)
  proto <- session$protocol
  if (proto == "python_hf") return(.py_sentence_transformers(session, texts))
  if (proto == "python_llamacpp") return(.py_llamacpp_embed(session, texts))
  if (is.null(session$embed_url)) stop("Backend '", session$backend, "' has no embedding endpoint. Use a separate embed_session.")

  if (proto == "ollama") {
    body <- list(model = session$embed_model, input = as.list(texts))
    resp <- .build_request(session, session$embed_url, body) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    parsed <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
    if (status < 400L && !is.null(parsed$embeddings)) {
      mat <- do.call(rbind, lapply(parsed$embeddings, function(x) as.numeric(unlist(x, use.names = FALSE))))
      rownames(mat) <- NULL
      return(mat)
    }
    # Backward-compatible fallback for older Ollama versions using /api/embeddings.
    if (status %in% c(404L, 405L)) {
      legacy_url <- sub("/api/embed$", "/api/embeddings", session$embed_url)
      rows <- lapply(texts, function(txt) {
        legacy_body <- list(model = session$embed_model, prompt = txt)
        legacy_resp <- .build_request(session, legacy_url, legacy_body) |>
          httr2::req_error(is_error = function(r) FALSE) |>
          httr2::req_perform()
        if (httr2::resp_status(legacy_resp) >= 400L) stop("Ollama legacy embed error: ", httr2::resp_status(legacy_resp))
        as.numeric(unlist(httr2::resp_body_json(legacy_resp, simplifyVector = FALSE)$embedding, use.names = FALSE))
      })
      mat <- do.call(rbind, rows); rownames(mat) <- NULL; return(mat)
    }
    stop("Ollama embed error: ", status)
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

.semantica_resolve_python_inference_device <- function(requested_device) {
  requested <- tolower(trimws(as.character(requested_device[1L])))
  if (!grepl("^(auto|cpu|cuda(:[0-9]+)?|mps)$", requested)) {
    stop("Unsupported Python inference device: ", requested)
  }
  if (identical(requested, "cpu")) return("cpu")

  torch <- .py_get("torch")
  cuda_available <- isTRUE(tryCatch(
    reticulate::py_to_r(torch$cuda$is_available()),
    error = function(e) FALSE
  ))
  mps_available <- isTRUE(tryCatch({
    backend <- torch$backends$mps
    reticulate::py_to_r(backend$is_available())
  }, error = function(e) FALSE))

  if (identical(requested, "auto")) {
    if (cuda_available) return("cuda:0")
    if (mps_available) return("mps")
    return("cpu")
  }
  if (startsWith(requested, "cuda")) {
    if (!cuda_available) {
      stop(
        "CUDA inference was requested, but Python PyTorch reports no CUDA device."
      )
    }
    index <- if (grepl(":", requested, fixed = TRUE)) {
      as.integer(sub("^cuda:", "", requested))
    } else {
      0L
    }
    count <- as.integer(tryCatch(
      reticulate::py_to_r(torch$cuda$device_count()),
      error = function(e) 0L
    ))
    if (!is.finite(count) || index >= count) {
      stop(sprintf(
        "CUDA inference device %d was requested, but Python reports %d device(s).",
        index, if (is.finite(count)) count else 0L
      ))
    }
    return(sprintf("cuda:%d", index))
  }
  if (!mps_available) {
    stop("MPS inference was requested, but Python PyTorch reports no MPS device.")
  }
  "mps"
}

.semantica_python_torch_dtype <- function(torch, precision) {
  switch(
    precision,
    float32 = torch$float32,
    float16 = torch$float16,
    bfloat16 = torch$bfloat16,
    NULL
  )
}

#' @keywords internal
.semantica_python_cache_key <- function(...) {
  parts <- vapply(list(...), function(x) {
    if (is.null(x)) return("<null>")
    paste(utils::capture.output(dput(x)), collapse = "")
  }, character(1L))
  paste(parts, collapse = "|")
}

#' @keywords internal
.py_hf_chat <- function(session, messages, max_tokens, temperature, system_prompt) {
  transformers <- .py_get("transformers")
  model_id <- session$chat_model
  cache_key <- .semantica_python_cache_key(
    "hf_pipe", model_id, session$chat_device,
    session$device_map, session$model_precision
  )
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading HF pipeline for '", model_id, "'...")
    kwargs <- list(task = "text-generation", model = model_id)
    if (!is.null(session$device_map)) {
      kwargs$device_map <- session$device_map
    } else {
      device <- session$chat_device %||% "cpu"
      kwargs$device <- if (identical(device, "cpu")) {
        -1L
      } else if (identical(device, "cuda")) {
        0L
      } else if (grepl("^cuda:[0-9]+$", device)) {
        as.integer(sub("^cuda:", "", device))
      } else {
        device
      }
    }
    if (!identical(session$model_precision %||% "auto", "auto")) {
      kwargs$torch_dtype <- .semantica_python_torch_dtype(
        .py_get("torch"), session$model_precision
      )
    }
    if (!is.null(session$hf_token)) kwargs$token <- session$hf_token
    pipe <- do.call(transformers$pipeline, kwargs)
    assign(cache_key, pipe, envir = .py_model_cache)
  }
  pipe <- get(cache_key, envir = .py_model_cache)
  py_messages <- reticulate::r_to_py(if (!is.null(system_prompt)) c(list(list(role="system", content=system_prompt)), messages) else messages)
  generation_args <- list(
    py_messages,
    max_new_tokens = as.integer(max_tokens),
    return_full_text = FALSE
  )
  # Hugging Face/Transformers treats temperature as a sampling control.  A
  # zero-temperature request is therefore routed to greedy decoding rather than
  # asking the backend to sample from a zero-temperature distribution, which is
  # invalid or warning-prone across Transformers versions.
  if (is.finite(temperature) && temperature > 0) {
    generation_args$temperature <- temperature
    generation_args$do_sample <- TRUE
  } else {
    generation_args$do_sample <- FALSE
  }
  out <- do.call(pipe, generation_args)
  txt <- tryCatch(out[[1L]]$generated_text, error = function(e) out[[1L]][[1L]]$generated_text)
  if (is.null(txt)) stop("HuggingFace pipeline returned NULL text.")
  as.character(txt)
}

#' @keywords internal
.py_llamacpp_chat <- function(session, messages, max_tokens, temperature, system_prompt) {
  llama_cpp <- .py_get("llama_cpp")
  if (is.null(session$gguf_path) || !file.exists(session$gguf_path)) stop("python_llamacpp requires a valid gguf_path.")
  model_path <- normalizePath(session$gguf_path, winslash = "/", mustWork = TRUE)
  cache_key <- .semantica_python_cache_key(
    "llamacpp", model_path, "chat", 4096L, session$gpu_layers,
    session$model_precision
  )
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading GGUF model '", basename(session$gguf_path), "'...")
    llm <- llama_cpp$Llama(
      model_path = model_path, n_ctx = 4096L,
      n_gpu_layers = as.integer(session$gpu_layers %||% -1L), verbose = FALSE
    )
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
  requested_device <- session$embedding_device %||% "auto"
  resolved_device <- .semantica_resolve_python_inference_device(
    requested_device
  )
  cache_key <- .semantica_python_cache_key(
    "sentence_transformer", model_id, requested_device, resolved_device,
    session$model_precision
  )
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    if (session$verbose) message("  Loading sentence-transformer '", model_id, "'...")
    constructor_args <- list(model_id, device = resolved_device)
    if (!identical(session$model_precision %||% "auto", "auto")) {
      constructor_args$model_kwargs <- list(
        torch_dtype = .semantica_python_torch_dtype(
          .py_get("torch"), session$model_precision
        )
      )
    }
    if (!is.null(session$hf_token)) constructor_args$token <- session$hf_token
    model <- do.call(st$SentenceTransformer, constructor_args)
    assign(cache_key, model, envir = .py_model_cache)
  }
  model <- get(cache_key, envir = .py_model_cache)
  embs <- model$encode(reticulate::r_to_py(as.list(texts)), normalize_embeddings = TRUE, show_progress_bar = FALSE)
  mat <- reticulate::py_to_r(embs)
  if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(texts))
  rownames(mat) <- NULL
  attr(mat, "semantica_embedding_device") <- list(
    requested = requested_device,
    resolved = resolved_device,
    precision = session$model_precision %||% "auto",
    status = "resolved_by_python_torch"
  )
  mat
}

#' @keywords internal
.py_llamacpp_embed <- function(session, texts) {
  llama_cpp <- .py_get("llama_cpp")
  if (is.null(session$gguf_path) || !file.exists(session$gguf_path)) stop("python_llamacpp embed requires gguf_path.")
  model_path <- normalizePath(session$gguf_path, winslash = "/", mustWork = TRUE)
  cache_key <- .semantica_python_cache_key(
    "llamacpp", model_path, "embed", 512L, session$gpu_layers,
    session$model_precision
  )
  if (!exists(cache_key, envir = .py_model_cache, inherits = FALSE)) {
    llm <- llama_cpp$Llama(
      model_path = model_path, n_ctx = 512L,
      n_gpu_layers = as.integer(session$gpu_layers %||% -1L),
      embedding = TRUE, verbose = FALSE
    )
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
.build_system_prompt <- function(scale_name, scale_description, response_format, item_style, language,
                                 output_mode = c("numbered", "json")) {
  output_mode <- match.arg(output_mode)
  contract <- if (output_mode == "json") {
    'Return ONLY valid JSON with one top-level key "items" whose value is an array of item strings. No markdown or commentary.'
  } else {
    'Return ONLY a numbered list. No preamble. Each line: "<number>. <item text>".'
  }
  sprintf('You are an expert psychometrician...\nSCALE CONTEXT\nName: %s\nDescription: %s\nResponse: %s\nStyle: %s\nLanguage: %s\n\nOUTPUT CONTRACT:\n%s Avoid double-barrelled items, jargon, and avoid negations unless the construct definition explicitly requires negative polarity.', scale_name, scale_description, response_format, item_style, language, contract)
}

#' @keywords internal
.build_factor_prompt <- function(factor_name, factor_description, n_items,
                                 user_examples = NULL, forbidden_concepts = NULL,
                                 extra_instructions = NULL,
                                 dimension_name = NULL, dimension_description = NULL,
                                 facet_name = NULL,
                                 output_mode = c("numbered", "json")) {
  output_mode <- match.arg(output_mode)
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
  output_instruction <- if (output_mode == "json") {
    sprintf('Output valid JSON only: {"items":["item 1", ..., "item %d"]}. Include exactly %d item strings.', n_items, n_items)
  } else {
    sprintf("Output exactly %d items, numbered 1 to %d.", n_items, n_items)
  }
  lines <- c(
    lines,
    "",
    "ITEM-SET QUALITY REQUIREMENTS:",
    "  - Every item must directly instantiate the stated factor/facet definition, not merely the broad scale topic.",
    "  - Avoid paraphrases or near-duplicates of other items; vary the behavioral manifestation while preserving the same construct meaning.",
    "  - Do not drift into adjacent constructs, consequences, causes, abilities, outcomes, or contextual features unless the definition explicitly includes them.",
    "  - Prefer one clear psychological proposition per item.",
    "",
    output_instruction
  )
  paste(lines, collapse = "\n")
}

# Normalize provider text line endings without turning a newline into the
# literal character "n". Keep this internal so every fallback parser uses the
# same transport-neutral text contract.
.semantica_normalize_newlines <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- as.character(x[[1L]])
  gsub("\\r\\n?", "\n", x, perl = TRUE)
}

#' @keywords internal
.parse_items_json <- function(raw_text, expected_n, factor_name, minimum_n = expected_n) {
  if (is.null(raw_text) || !length(raw_text)) return(character(0L))
  txt <- as.character(raw_text[[1L]])
  candidates <- unique(c(
    txt,
    gsub("^\\s*```(?:json)?\\s*|\\s*```\\s*$", "", txt, ignore.case = TRUE, perl = TRUE),
    if (grepl("\\{", txt) && grepl("\\}", txt)) sub("^[^{]*(\\{.*\\})[^}]*$", "\\1", txt, perl = TRUE) else character(0L)
  ))
  parsed <- NULL
  for (candidate in candidates) {
    parsed <- tryCatch(jsonlite::fromJSON(candidate, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) break
  }
  if (is.null(parsed)) return(.parse_items(raw_text, expected_n, factor_name, minimum_n))
  items <- parsed$items %||% parsed$item %||% NULL
  if (is.data.frame(items)) {
    candidate_col <- intersect(c("text", "item_text", "item"), names(items))
    items <- if (length(candidate_col)) items[[candidate_col[[1L]]]] else unlist(items, use.names = FALSE)
  }
  items <- as.character(unlist(items, use.names = FALSE))
  items <- stringr::str_trim(items)
  items <- items[nchar(items) > 5L]
  items <- unique(items)
  if (length(items) < minimum_n) {
    warning(sprintf("Structured response for '%s' yielded %d usable items; expected at least %d.", factor_name, length(items), minimum_n))
  }
  if (length(items) > expected_n) items <- items[seq_len(expected_n)]
  items
}

#' @keywords internal
.parse_items <- function(raw_text, expected_n, factor_name, minimum_n = expected_n) {
  if (is.null(raw_text) || length(raw_text) == 0L || is.na(raw_text)) {
    warning("Empty LLM response for '", factor_name, "'.")
    return(character(0L))
  }

  txt <- .semantica_normalize_newlines(raw_text)
  txt <- gsub("```[A-Za-z]*", "", txt)
  txt <- gsub("```", "", txt)
  # Some LLMs place all numbered items on one line; force a line break
  # before recognizable list markers before parsing.
  txt <- gsub("(?m)(^|\\s)(\\d+\\s*[\\.)\\:\\-]\\s+)", "\n\\2", txt, perl = TRUE)

  lines <- stringr::str_trim(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nchar(lines) > 0L]
  if (length(lines) == 0L) {
    warning("Could not parse items for '", factor_name, "'.")
    return(character(0L))
  }

  bullet <- intToUtf8(0x2022)
  list_marker <- grepl("^\\s*\\d+\\s*[\\.)\\:\\-]\\s+", lines, perl = TRUE) |
    grepl(paste0("^\\s*[-*", bullet, "]\\s+"), lines, perl = TRUE)
  # If the response contains an actual list, prose before/after it is context,
  # not item content. This prevents introductions/conclusions from becoming
  # silently valid items while preserving plain one-item-per-line fallbacks.
  if (any(list_marker)) lines <- lines[list_marker]

  strip_marker <- function(x) {
    left_quote <- intToUtf8(0x201c)
    right_quote <- intToUtf8(0x201d)

    x <- gsub("^\\s*\\d+\\s*[\\.)\\:\\-]\\s+", "", x, perl = TRUE)
    x <- gsub(paste0("^\\s*[-*", bullet, "]\\s+"), "", x, perl = TRUE)
    x <- gsub(
      paste0("^\\s*[\\\"'", left_quote, right_quote, "]+|[\\\"'", left_quote, right_quote, "]+\\s*$"),
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
    "^(here|sure|certainly|below|these|the following|aqui|claro|por supuesto|lista|nota|note|sorry|i am sorry|i'm sorry|i cannot|i can't|unable to|as an ai)\\b",
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
.semantica_parse_generation_response <- function(raw_text, expected_n, factor_name,
                                                  minimum_n = expected_n,
                                                  output_mode = c("numbered", "json")) {
  output_mode <- match.arg(output_mode)
  raw_scalar <- .semantica_normalize_newlines(raw_text)
  nonempty_lines <- if (!nzchar(trimws(raw_scalar))) character(0L) else {
    z <- trimws(strsplit(raw_scalar, "\n", fixed = TRUE)[[1L]])
    z[nzchar(z)]
  }
  parsed_raw <- suppressWarnings(if (output_mode == "json") {
    .parse_items_json(raw_text, expected_n, factor_name, minimum_n = minimum_n)
  } else {
    .parse_items(raw_text, expected_n, factor_name, minimum_n = minimum_n)
  })
  retained <- .dedup_items(parsed_raw)
  reasons <- character(0L)
  if (!length(nonempty_lines)) reasons <- c(reasons, "empty_response")
  refusal <- grepl("\\b(sorry|cannot|can't|unable|refuse)\\b", tolower(raw_scalar), perl = TRUE)
  if (isTRUE(refusal) && !length(retained)) reasons <- c(reasons, "refusal_or_non_item_response")
  # "fewer_than_requested" is descriptive attempt metadata: compare with the
  # number requested from the backend, not the lower usability floor. A response
  # can therefore be usable for the current generation loop while still being
  # truthfully recorded as shorter than requested. This does not change which
  # items are retained or whether retries occur.
  if (length(parsed_raw) < expected_n) reasons <- c(reasons, "fewer_than_requested")
  if (length(parsed_raw) < minimum_n) reasons <- c(reasons, "below_minimum_usable")
  if (length(parsed_raw) > expected_n) reasons <- c(reasons, "more_than_requested_truncated")
  duplicate_n <- max(0L, length(parsed_raw) - length(retained))
  if (duplicate_n > 0L) reasons <- c(reasons, "duplicates_removed")
  list(
    items = retained,
    metadata = list(
      requested = as.integer(expected_n),
      received = as.integer(length(nonempty_lines)),
      parsed = as.integer(length(parsed_raw)),
      rejected = as.integer(max(0L, length(nonempty_lines) - length(parsed_raw))),
      duplicate = as.integer(duplicate_n),
      retained = as.integer(length(retained)),
      rejection_reasons = unique(reasons),
      output_mode = output_mode
    )
  )
}

#' @keywords internal
.semantica_normalize_item_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

#' @keywords internal
.semantica_char_bigram_set <- function(x) {
  x <- gsub("\\s+", " ", .semantica_normalize_item_text(x), perl = TRUE)
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  if (length(ch) < 2L) return(character(0L))
  unique(paste0(ch[-length(ch)], ch[-1L]))
}

#' @keywords internal
.semantica_lexical_jaccard <- function(a, b) {
  aa <- .semantica_char_bigram_set(a)
  bb <- .semantica_char_bigram_set(b)
  if (!length(aa) || !length(bb)) return(0)
  u <- union(aa, bb)
  if (!length(u)) return(0)
  length(intersect(aa, bb)) / length(u)
}

#' @keywords internal
.dedup_items <- function(items, threshold = 0.92) {
  if (length(items) <= 1L) return(items)
  items <- as.character(items)
  # First remove threshold-free lexical identity after case/punctuation/spacing
  # normalization. Near-duplicate screening remains a separate lexical rule and
  # does not reuse an embedding cosine threshold.
  norm <- .semantica_normalize_item_text(items)
  items <- items[!duplicated(norm)]
  if (length(items) <= 1L) return(items)
  keep <- rep(TRUE, length(items))
  for (i in seq_len(length(items) - 1L)) {
    if (!keep[i]) next
    for (j in (i + 1L):length(items)) {
      if (!keep[j]) next
      if (.semantica_lexical_jaccard(items[i], items[j]) >= threshold) keep[j] <- FALSE
    }
  }
  items[keep]
}

#' @keywords internal
.semantica_select_diverse_generated_items <- function(items, n_target) {
  items <- as.character(items)
  n_target <- .as_positive_int(n_target, "'n_target'")
  if (length(items) <= n_target) return(items)
  n <- length(items)
  sim <- matrix(0, n, n)
  diag(sim) <- 1
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      z <- .semantica_lexical_jaccard(items[i], items[j])
      sim[i, j] <- sim[j, i] <- z
    }
  }
  # Deterministic max-min diversity: seed with the item having the lowest
  # typical lexical similarity to the rest, then repeatedly add the candidate
  # whose closest selected neighbour is least similar. No new duplicate cutoff
  # or model-dependent semantic threshold is introduced.
  typical <- vapply(seq_len(n), function(i) stats::median(sim[i, -i], na.rm = TRUE), numeric(1L))
  selected <- which.min(typical)
  while (length(selected) < n_target) {
    remaining <- setdiff(seq_len(n), selected)
    max_to_selected <- vapply(remaining, function(i) max(sim[i, selected, drop = TRUE], na.rm = TRUE), numeric(1L))
    mean_to_selected <- vapply(remaining, function(i) mean(sim[i, selected, drop = TRUE], na.rm = TRUE), numeric(1L))
    ord <- order(max_to_selected, mean_to_selected, remaining, decreasing = FALSE)
    selected <- c(selected, remaining[ord[1L]])
  }
  out <- items[selected]
  attr(out, "generation_diversity") <- list(
    policy = "deterministic_lexical_maxmin_v1",
    candidate_n = n,
    retained_n = length(out),
    discarded_n = n - length(out)
  )
  out
}

#' @keywords internal
.semantica_generation_replenishment_plan <- function(deficit, successful_requested, successful_new_retained, initial_request) {
  deficit <- .as_positive_int(deficit, "'deficit'")
  initial_request <- .as_positive_int(initial_request, "'initial_request'")
  successful_requested <- suppressWarnings(as.integer(successful_requested))
  successful_new_retained <- suppressWarnings(as.integer(successful_new_retained))

  if (length(successful_requested) != 1L || is.na(successful_requested) || successful_requested < 0L) {
    stop("'successful_requested' must be a non-negative integer.")
  }
  if (length(successful_new_retained) != 1L || is.na(successful_new_retained) || successful_new_retained < 0L) {
    stop("'successful_new_retained' must be a non-negative integer.")
  }

  observed_yield <- if (successful_requested > 0L && successful_new_retained > 0L) {
    min(1, successful_new_retained / successful_requested)
  } else {
    NA_real_
  }

  estimated <- if (is.finite(observed_yield) && observed_yield > 0) {
    ceiling(deficit / observed_yield)
  } else {
    deficit
  }

  capped_estimate <- min(as.double(initial_request), as.double(estimated))
  request_n <- max(deficit, as.integer(capped_estimate))
  list(
    deficit = as.integer(deficit),
    request_n = as.integer(request_n),
    observed_yield = observed_yield,
    successful_requested = successful_requested,
    successful_new_retained = successful_new_retained,
    capped_at_initial_request = isTRUE(deficit <= initial_request && estimated > initial_request && request_n == initial_request),
    policy = "yield_adaptive_v1"
  )
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
#' @param max_retries      Maximum generation attempts. After a partial successful response, retries are deficit-aware: already usable items are preserved and the next request is sized from the observed new-item yield rather than regenerating the full target.
#' @param global_forbidden_max Maximum number of previously generated items to
#'   include as anti-duplicate examples in subsequent prompts.
#' @param temperature      LLM temperature.
#' @param structured_output Output contract: `"auto"`, `"numbered"`, or structured `"json"` when supported.
#' @param verbose          Print progress.
#' @param seed Optional nonnegative master seed for LLM generation. SEMANTICA
#'   currently forwards deterministically derived per-task seeds only to backends
#'   with an implemented seed-control contract (Ollama). Unsupported protocols
#'   are recorded as uncontrolled rather than silently treated as reproducible.
#'   A requested backend seed controls SEMANTICA's deterministic seed schedule
#'   but does not guarantee byte-identical backend output, even within one
#'   runtime. Exact downstream replay requires saving and reusing the realized
#'   item pool identified by its item-pool fingerprint.
#' @return Tibble with legacy columns (`item_id`, `factor`, `item_text`,
#'   `attempt`) and facet-aware columns (`ID`, `Dimension`, `Facet`, `item`).
#'   Attribute `semantica_generation_metadata` records the generation contract,
#'   backend seed-control status, a stable seed schedule separated from the
#'   dynamic execution ledger, generation-specification and replay-plan
#'   fingerprints, the exact retained-pool fingerprint, and whether downstream
#'   content screening has occurred.
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
                                     global_forbidden_max = 40L, temperature = 0.8,
                                     structured_output = c("auto", "numbered", "json"),
                                     verbose = TRUE, seed = NULL) {
  if (!inherits(session, "semantica_session")) stop("'session' must be created with semantica_connect().")
  if (!is.list(factors) || is.null(names(factors))) stop("'factors' must be a named list.")
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
  structured_output <- match.arg(structured_output)
  output_mode <- if (structured_output == "auto") {
    if (isTRUE(session$supports_structured_output)) "json" else "numbered"
  } else structured_output
  if (output_mode == "json" && !requireNamespace("jsonlite", quietly = TRUE)) {
    warning("Structured output requested but 'jsonlite' is unavailable; falling back to numbered text.", call. = FALSE)
    output_mode <- "numbered"
  }

  seed <- .semantica_normalize_generation_seed(seed)
  seed_capability <- .semantica_generation_seed_capability(session)
  seed_controlled <- !is.null(seed) && isTRUE(seed_capability$supported)

  generation_plan <- .expand_generation_plan(factors, n_per_factor, n_per_factor_override)
  factor_names <- unique(vapply(generation_plan, `[[`, character(1L), "dimension"))
  system_prompt <- .build_system_prompt(scale_name, scale_description, response_format, item_style, language, output_mode = output_mode)
  if (verbose) {
    cat("============================================================\nSTEP 2 -- ITEM GENERATION\n============================================================\n")
    cat(sprintf("  Scale         : %s\n  Dimensions    : %d\n  Generation units: %d\n", scale_name, length(factor_names), length(generation_plan)))
    if (isTRUE(n_per_factor_override) && !is.null(n_per_factor)) {
      has_declared_facets <- any(vapply(factors, function(spec) {
        if (!is.list(spec)) return(FALSE)
        facets <- spec$facets %||% spec$Facets %||% NULL
        !is.null(facets) && length(facets) > 0L
      }, logical(1L)))
      if (has_declared_facets) {
        cat(sprintf("  Count override: %d retained items per dimension, allocated across facets\n",
                    .as_positive_int(n_per_factor, "'n_per_factor'")))
      } else {
        cat(sprintf("  Count override: %d retained items per dimension\n",
                    .as_positive_int(n_per_factor, "'n_per_factor'")))
      }
    }
    if (!is.null(seed)) {
      if (seed_controlled) {
        cat(sprintf("  Generation seed: %d master seed -> deterministically derived per-call %s seeds\n", seed, seed_capability$protocol))
      } else {
        cat(sprintf("  Generation seed: %d requested, but protocol '%s' has no implemented seed-control contract in SEMANTICA; generation remains backend-uncontrolled\n", seed, seed_capability$protocol))
      }
    }
  }

  all_rows <- list(); item_counter <- 1L; global_forbidden <- character(0L)
  generation_attempts <- list()
  diversity_curation <- list()
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
        if (output_mode == "json") "Return only valid JSON with key 'items'." else "Return only a plain numbered list. Do not add headings, explanations, markdown tables, JSON, or bullet points."
      ),
      dimension_name = unit$dimension,
      dimension_description = unit$dimension_description,
      facet_name = unit$facet,
      output_mode = output_mode
    )
    collected <- character(0L); attempt <- 1L; request_n <- n_request; last_error <- NULL
    successful_requested <- 0L
    successful_new_retained <- 0L
    while (length(collected) < n_target && attempt <= max_retries) {
      needed_before <- n_target - length(collected)
      collected_before <- collected
      task_seed <- if (seed_controlled) {
        .semantica_derive_generation_task_seed(seed, unit$dimension, unit$facet, attempt, request_n)
      } else NA_integer_
      prompt_fingerprint <- .semantica_object_md5(list(
        schema = "semantica-generation-prompt-v1",
        system_prompt = enc2utf8(system_prompt),
        user_prompt = enc2utf8(user_prompt),
        output_mode = output_mode,
        request_n = as.integer(request_n)
      ))
      raw <- tryCatch(
        .call_chat(session, messages = list(list(role="user", content=user_prompt)),
                   max_tokens = max(256L, request_n * 90L), temperature = temperature,
                   system_prompt = system_prompt,
                   response_format = if (output_mode == "json") "json" else NULL,
                   seed = if (is.finite(task_seed)) task_seed else NULL),
        error = function(e) {
          last_error <<- conditionMessage(e)
          if (verbose) message(sprintf("    Attempt %d failed for %s/%s: %s", attempt, unit$dimension, unit$facet, last_error))
          NULL
        }
      )
      if (!is.null(raw)) {
        parsed_response <- .semantica_parse_generation_response(
          raw, request_n, unit$facet, minimum_n = min(needed_before, request_n),
          output_mode = output_mode
        )
        parsed <- parsed_response$items
        new_items <- setdiff(parsed, collected_before)
        collected <- unique(c(collected_before, new_items))
        successful_requested <- successful_requested + as.integer(request_n)
        successful_new_retained <- successful_new_retained + as.integer(length(new_items))
        attempt_meta <- parsed_response$metadata
        attempt_meta$dimension <- unit$dimension
        attempt_meta$facet <- unit$facet
        attempt_meta$attempt <- attempt
        attempt_meta$retried <- attempt > 1L
        attempt_meta$prompt_fingerprint <- prompt_fingerprint
        attempt_meta$generation_seed_master <- if (is.null(seed)) NA_integer_ else seed
        attempt_meta$generation_task_seed <- if (is.finite(task_seed)) as.integer(task_seed) else NA_integer_
        attempt_meta$generation_seed_supported <- isTRUE(seed_capability$supported)
        attempt_meta$generation_seed_control <- seed_capability$mechanism
        attempt_meta$needed_before <- as.integer(needed_before)
        attempt_meta$newly_retained <- as.integer(length(new_items))
        attempt_meta$new_item_yield <- if (request_n > 0L) length(new_items) / request_n else NA_real_
        attempt_meta$duplicate_existing <- as.integer(max(0L, length(parsed) - length(new_items)))
        attempt_meta$retained_total <- length(collected)
        attempt_meta$needed_after <- as.integer(max(0L, n_target - length(collected)))
        generation_attempts[[length(generation_attempts) + 1L]] <- attempt_meta
      }
      if (is.null(raw)) {
        generation_attempts[[length(generation_attempts) + 1L]] <- list(
          requested = as.integer(request_n), received = 0L, parsed = 0L, rejected = 0L,
          duplicate = 0L, retained = 0L, rejection_reasons = "backend_error",
          output_mode = output_mode, dimension = unit$dimension, facet = unit$facet,
          attempt = attempt, retried = attempt > 1L, prompt_fingerprint = prompt_fingerprint,
          generation_seed_master = if (is.null(seed)) NA_integer_ else seed,
          generation_task_seed = if (is.finite(task_seed)) as.integer(task_seed) else NA_integer_,
          generation_seed_supported = isTRUE(seed_capability$supported),
          generation_seed_control = seed_capability$mechanism,
          needed_before = as.integer(needed_before),
          newly_retained = 0L, new_item_yield = NA_real_, duplicate_existing = 0L, retained_total = length(collected),
          needed_after = as.integer(max(0L, n_target - length(collected))),
          backend_error = last_error
        )
      }
      attempt <- attempt + 1L
      if (length(collected) < n_target && attempt <= max_retries) {
        n_miss <- n_target - length(collected)
        replenishment <- .semantica_generation_replenishment_plan(
          deficit = n_miss,
          successful_requested = successful_requested,
          successful_new_retained = successful_new_retained,
          initial_request = n_request
        )
        request_n <- replenishment$request_n
        if (verbose) {
          yield_txt <- if (is.finite(replenishment$observed_yield)) {
            sprintf("%.1f%% observed new-item yield", 100 * replenishment$observed_yield)
          } else {
            "no successful yield estimate yet"
          }
          cat(sprintf("    Replenishment: %d item(s) still needed; requesting %d new candidate(s) (%s).\n",
                      n_miss, request_n, yield_txt))
        }
        prompt_forbidden <- unique(c(unit$forbidden, collected, tail(global_forbidden, global_forbidden_max)))
        user_prompt <- .build_factor_prompt(
          factor_name = unit$facet,
          factor_description = unit$facet_description,
          n_items = request_n,
          user_examples = unit$examples,
          forbidden_concepts = prompt_forbidden,
          extra_instructions = paste(
            "Generate NEW items only.",
            if (output_mode == "json") "Return valid JSON with key 'items' and exactly the requested number of strings." else "Return exactly the requested number of items as '1. item text', one item per line, with no preamble.",
            unit$extra_instructions
          ),
          dimension_name = unit$dimension,
          dimension_description = unit$dimension_description,
          facet_name = unit$facet,
          output_mode = output_mode
        )
      }
    }
    if (length(collected) > n_target) {
      n_before_diversity <- length(collected)
      collected <- .semantica_select_diverse_generated_items(collected, n_target)
      diversity_curation[[length(diversity_curation) + 1L]] <- list(
        dimension = unit$dimension, facet = unit$facet,
        candidate_n = n_before_diversity, retained_n = length(collected),
        discarded_n = n_before_diversity - length(collected),
        policy = "deterministic_lexical_maxmin_v1"
      )
    }
    if (length(collected) < n_target) {
      # Continuing with too few items breaks the downstream ACO/ESEM constraints.
      err_suffix <- if (!is.null(last_error)) paste0(" Last backend error: ", last_error) else ""
      stop(sprintf("Facet '%s' in dimension '%s': generated only %d/%d usable items after %d attempt(s).%s",
                   unit$facet, unit$dimension, length(collected), n_target, max_retries, err_suffix))
    }
    if (verbose) cat(sprintf("    --> Retained %d generated candidates (pre-alignment)\n", length(collected)))
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
  # Preserve the 0.5.3 contract fingerprint exactly for compatibility with
  # stored provenance. The new specification fingerprint below has clearer
  # semantics and may include additional model identity when available.
  generation_contract_fingerprint <- .semantica_object_md5(list(
    schema = "semantica-generation-contract-v1",
    scale_name = enc2utf8(scale_name),
    scale_description = enc2utf8(scale_description),
    factors = factors,
    response_format = response_format, item_style = item_style, language = language,
    n_per_factor = n_per_factor, n_per_factor_override = n_per_factor_override,
    overgenerate = overgenerate, max_retries = as.integer(max_retries),
    global_forbidden_max = global_forbidden_max, temperature = temperature,
    output_mode = output_mode, backend = session$backend %||% NA_character_,
    protocol = session$protocol %||% NA_character_, chat_model = session$chat_model %||% NA_character_
  ))
  generation_spec_fingerprint <- .semantica_object_md5(list(
    schema = "semantica-generation-spec-v1",
    scale_name = enc2utf8(scale_name),
    scale_description = enc2utf8(scale_description),
    factors = factors,
    response_format = response_format, item_style = item_style, language = language,
    n_per_factor = n_per_factor, n_per_factor_override = n_per_factor_override,
    overgenerate = overgenerate, max_retries = as.integer(max_retries),
    global_forbidden_max = global_forbidden_max, temperature = temperature,
    output_mode = output_mode, backend = session$backend %||% NA_character_,
    protocol = session$protocol %||% NA_character_, chat_model = session$chat_model %||% NA_character_,
    model_revision = session$model_revision %||% NA_character_,
    model_precision = session$model_precision %||% NA_character_
  ))
  item_pool_fingerprint <- .semantica_object_md5(data.frame(
    item_id = out$item_id, factor = out$factor, item_text = enc2utf8(out$item_text),
    stringsAsFactors = FALSE
  ))
  seed_ledger <- if (length(generation_attempts)) {
    do.call(rbind, lapply(generation_attempts, function(a) data.frame(
      dimension = as.character(a$dimension %||% NA_character_),
      facet = as.character(a$facet %||% NA_character_),
      attempt = as.integer(a$attempt %||% NA_integer_),
      requested_n = as.integer(a$requested %||% NA_integer_),
      prompt_fingerprint = as.character(a$prompt_fingerprint %||% NA_character_),
      generation_task_seed = as.integer(a$generation_task_seed %||% NA_integer_),
      seed_supported = isTRUE(a$generation_seed_supported),
      seed_control = as.character(a$generation_seed_control %||% "not_recorded"),
      stringsAsFactors = FALSE
    )))
  } else data.frame(
    dimension = character(0L), facet = character(0L), attempt = integer(0L),
    requested_n = integer(0L), prompt_fingerprint = character(0L), generation_task_seed = integer(0L),
    seed_supported = logical(0L), seed_control = character(0L), stringsAsFactors = FALSE
  )
  # The seed schedule intentionally excludes prompt fingerprints. Later prompts can
  # legitimately depend on earlier generated text through the anti-duplication
  # context, so prompt equality is an observed execution property rather than a
  # stable seed-contract invariant.
  generation_seed_schedule <- seed_ledger[, c(
    "dimension", "facet", "attempt", "requested_n",
    "generation_task_seed", "seed_supported", "seed_control"
  ), drop = FALSE]
  generation_replay_plan_fingerprint <- if (seed_controlled) {
    .semantica_object_md5(list(
      schema = "semantica-generation-replay-plan-v1",
      generation_spec_fingerprint = generation_spec_fingerprint,
      generation_seed_master = seed,
      generation_seed_schedule = generation_seed_schedule
    ))
  } else {
    NA_character_
  }
  attr(out, "semantica_generation_metadata") <- list(
    schema = "semantica-generation-provenance-v1",
    requested_total = sum(vapply(generation_plan, function(x) x$n_items, integer(1L))),
    retained_total = nrow(out),
    attempts = generation_attempts,
    retry_count = sum(vapply(generation_attempts, function(x) isTRUE(x$retried), logical(1L))),
    parser_contract = "semantica-generation-parser-1",
    replenishment_policy = "yield_adaptive_v1",
    lexical_deduplication = "normalized_identity_plus_char_bigram_jaccard",
    overgeneration_curation = "deterministic_lexical_maxmin_v1",
    diversity_curation = diversity_curation,
    generation_backend = as.character(session$backend %||% NA_character_),
    generation_protocol = as.character(session$protocol %||% NA_character_),
    generation_chat_model = as.character(session$chat_model %||% NA_character_),
    generation_temperature = as.numeric(temperature),
    generation_output_mode = output_mode,
    generation_seed_master = if (is.null(seed)) NA_integer_ else seed,
    generation_seed_supported = isTRUE(seed_capability$supported),
    generation_seed_controlled = seed_controlled,
    generation_seed_protocol = seed_capability$protocol,
    generation_seed_mechanism = seed_capability$mechanism,
    generation_seed_guarantee = if (seed_controlled) {
      "backend_seed_requested_not_runtime_guaranteed"
    } else if (is.null(seed)) {
      "not_requested"
    } else {
      "not_controlled"
    },
    task_seed_ledger = seed_ledger,
    task_seed_ledger_role = "observed_execution_trace_including_dynamic_prompt_fingerprints",
    generation_seed_schedule_schema = "semantica-generation-seed-schedule-v1",
    generation_seed_schedule = generation_seed_schedule,
    generation_seed_schedule_role = "stable_seed_control_fields_for_realized_generation_calls",
    generation_spec_fingerprint_schema = "semantica-generation-spec-md5-v1",
    generation_spec_fingerprint = generation_spec_fingerprint,
    # Exact 0.5.3 fingerprint retained for compatibility with stored runs.
    generation_contract_fingerprint = generation_contract_fingerprint,
    generation_replay_plan_fingerprint_schema = "semantica-generation-replay-plan-md5-v1",
    generation_replay_plan_fingerprint = generation_replay_plan_fingerprint,
    exact_text_replay_guaranteed = FALSE,
    generation_replay_note = paste(
      "A controlled backend seed fixes SEMANTICA's seed schedule but does not guarantee",
      "byte-identical LLM output. Exact downstream replay requires saving and reusing",
      "the realized item pool identified by item_pool_fingerprint."
    ),
    item_pool_fingerprint_schema = "semantica-item-pool-md5-v1",
    item_pool_fingerprint = item_pool_fingerprint,
    content_screening_status = "not_yet_performed",
    content_screening_note = paste(
      "Retained candidates passed generation parsing and deterministic lexical curation only;",
      "construct-definition alignment and other construct-alignment guards occur downstream."
    )
  )
  if (verbose) {
    cat(sprintf("\n  Total generated candidates retained: %d\n", nrow(out)))
    cat("  Generation-stage status: lexical curation complete; construct-alignment screening not yet performed.\n")
    if (seed_controlled) {
      cat("  Replay semantics: backend seed controlled; exact text replay is not guaranteed. Reuse the fingerprinted item pool for exact downstream replay.\n")
    }
    cat(sprintf("  Item-pool fingerprint: %s\n", item_pool_fingerprint))
  }
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
#' @param normalize L2-normalise vectors.
#' @param cache Use the persistent content-addressed embedding cache.
#' @param cache_dir Cache directory; `NULL` uses an OS-appropriate SEMANTICA cache.
#' @param cache_namespace Optional analyst-defined namespace included in cache keys.
#' @param verbose       Print progress.
#' @usage semantica_embed(
#'   items_tbl,
#'   session,
#'   embed_session = NULL,
#'   text_col = "item_text",
#'   id_col = "item_id",
#'   batch_size = 64L,
#'   normalize = TRUE,
#'   cache = TRUE,
#'   cache_dir = NULL,
#'   cache_namespace = NULL,
#'   verbose = TRUE
#' )
#' @section Side effects:
#' May call remote embedding APIs or local Python models and may read/write the
#' persistent content-addressed embedding cache. Cache writes are RNG-neutral.
#'
#' @section Reproducibility:
#' Embedding diagnostics record the resolved model/task/device information that is
#' available. A stable remote model name may still be a mutable provider alias;
#' cache reuse preserves the exact vectors previously stored for that cache key.
#'
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
semantica_embed <- function(items_tbl, session, embed_session = NULL, text_col = "item_text", id_col = "item_id", batch_size = 64L,
                             normalize = TRUE, cache = TRUE, cache_dir = NULL,
                             cache_namespace = NULL, verbose = TRUE) {
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

  cache <- isTRUE(cache)
  cache_dir <- cache_dir %||% .semantica_default_cache_dir()
  cache_hits <- 0L
  cache_misses <- 0L
  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  emb_matrix <- NULL
  embedding_device_runtime <- NULL
  for (bi in seq_along(batches)) {
    idx <- batches[[bi]]
    batch_texts <- texts[idx]
    keys <- if (cache) vapply(batch_texts, .semantica_text_cache_key, character(1L),
                              session = esess, normalize = normalize,
                              cache_namespace = cache_namespace) else rep(NA_character_, length(idx))
    cached_rows <- if (cache) lapply(keys, .semantica_embedding_cache_get, cache_dir = cache_dir) else vector("list", length(idx))
    hit <- vapply(cached_rows, function(x) is.numeric(x) && length(x) > 0L && all(is.finite(x)), logical(1L))
    cache_hits <- cache_hits + sum(hit)
    cache_misses <- cache_misses + sum(!hit)
    fresh <- NULL
    fresh_device <- NULL
    if (any(!hit)) {
      fresh <- tryCatch(
        .call_embed(esess, batch_texts[!hit]),
        error = function(e) stop(sprintf("Embedding batch %d failed: %s", bi, e$message))
      )
      fresh_device <- attr(fresh, "semantica_embedding_device", exact = TRUE)
      if (is.null(dim(fresh))) fresh <- matrix(fresh, nrow = 1L)
      fresh <- as.matrix(fresh)
      storage.mode(fresh) <- "double"
      if (nrow(fresh) != sum(!hit)) stop(sprintf("Embedding batch %d returned the wrong number of fresh rows.", bi))
      fresh_names <- colnames(fresh)
      fresh_i <- 1L
      for (k in which(!hit)) {
        row_value <- as.numeric(fresh[fresh_i, ])
        if (!is.null(fresh_names) && length(fresh_names) == length(row_value)) {
          names(row_value) <- fresh_names
        }
        cached_rows[[k]] <- row_value
        if (cache) .semantica_embedding_cache_set(keys[[k]], cached_rows[[k]], cache_dir)
        fresh_i <- fresh_i + 1L
      }
    }
    dims <- vapply(cached_rows, length, integer(1L))
    if (length(unique(dims)) != 1L) stop(sprintf("Embedding batch %d mixed incompatible cached dimensions.", bi))
    component_names <- NULL
    named_row <- which(vapply(cached_rows, function(x) !is.null(names(x)), logical(1L)))[1L]
    if (length(named_row) == 1L && !is.na(named_row)) component_names <- names(cached_rows[[named_row]])
    batch_matrix <- do.call(rbind, cached_rows)
    if (!is.null(component_names) && length(component_names) == ncol(batch_matrix)) {
      colnames(batch_matrix) <- component_names
    }
    # Device consistency is meaningful only for backend inference. Cached
    # vectors do not represent a runtime device and must not constrain later
    # cache-miss batches.
    batch_device <- fresh_device
    if (!is.null(batch_device)) {
      if (is.null(embedding_device_runtime)) {
        embedding_device_runtime <- batch_device
      } else if (!identical(
        embedding_device_runtime$resolved, batch_device$resolved
      )) {
        stop("Embedding backend changed devices between batches.")
      }
    }
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
  embedding_policy <- .semantica_embedding_policy(
    esess$embed_model, esess$embedding_task %||% "auto",
    esess$embedding_instruction %||% NULL,
    esess$embedding_spec %||% NULL
  )
  embedding_diagnostics <- list(
    model = esess$embed_model %||% "unknown",
    provider = esess$backend %||% "unknown",
    model_revision = esess$model_revision %||% NA_character_,
    analysis_intent = embedding_policy$analysis_intent %||% "psychometric_similarity",
    provider_task = embedding_policy$provider_task %||% NA_character_,
    embedding_task_requested = embedding_policy$requested_task,
    embedding_task_resolved = embedding_policy$resolved_task,
    instruction_applied = isTRUE(embedding_policy$instruction_applied),
    embedding_instruction_source = embedding_policy$source,
    embedding_instruction_fingerprint = embedding_policy$instruction_fingerprint %||% NA_character_,
    embedding_instruction_prefix = embedding_policy$prefix %||% NA_character_,
    embedding_capability_source = embedding_policy$capability_source %||% "unknown",
    embedding_capability_fingerprint = embedding_policy$capability_fingerprint %||% NA_character_,
    embedding_capability_note = embedding_policy$capability_note %||% NA_character_,
    requested_device = if (identical(esess$protocol, "python_llamacpp")) {
      paste0("gpu_layers=", esess$gpu_layers %||% 0L)
    } else {
      embedding_device_runtime$requested %||%
        esess$embedding_device %||% "backend_default"
    },
    resolved_device = if (!is.null(embedding_device_runtime$resolved)) {
      embedding_device_runtime$resolved
    } else if (cache && cache_hits == n) {
      "persistent_cache"
    } else if (identical(esess$protocol, "python_llamacpp")) {
      if (identical(as.integer(esess$gpu_layers %||% 0L), 0L)) {
        "cpu"
      } else {
        "configured_gpu_layers_unverified"
      }
    } else if ((esess$embedding_device %||% "auto") == "auto") {
      "backend_auto_unverified"
    } else {
      esess$embedding_device
    },
    device_status = if (cache && cache_hits == n && is.null(embedding_device_runtime)) {
      "cached_vectors"
    } else {
      embedding_device_runtime$status %||% esess$device_status %||% "not_reported"
    },
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
    cache_enabled = cache,
    cache_dir = if (cache) normalizePath(cache_dir, mustWork = FALSE) else NA_character_,
    cache_hits = cache_hits,
    cache_misses = cache_misses,
    cache_hit_rate = if ((cache_hits + cache_misses) > 0L) cache_hits / (cache_hits + cache_misses) else NA_real_,
    warnings = dim_warning
  )
  if (verbose) {
    cat(sprintf("  Embedding matrix: %d x %d\n", nrow(emb_matrix), ncol(emb_matrix)))
    if (!is.null(embedding_policy$prefix)) {
      cat(sprintf("  Embedding task  : %s (%s instruction)\n",
                  embedding_policy$resolved_task, embedding_policy$source))
    }
    cat(sprintf("  Vector norms    : median %.4f%s\n",
                embedding_diagnostics$final_norm_median,
                if (isTRUE(normalize)) " (L2-normalized)" else ""))
    if (cache) cat(sprintf("  Embedding cache : %d hit(s), %d miss(es)\n", cache_hits, cache_misses))
  }
  representation_provenance <- list(
    provider = esess$backend %||% "unknown",
    model = esess$embed_model %||% "unknown",
    model_revision = esess$model_revision %||% NA_character_,
    analysis_intent = embedding_policy$analysis_intent %||% "psychometric_similarity",
    provider_task = embedding_policy$provider_task %||% NULL,
    instruction_applied = isTRUE(embedding_policy$instruction_applied),
    instruction_source = embedding_policy$source %||% "none",
    instruction_fingerprint = embedding_policy$instruction_fingerprint %||% NA_character_,
    capability_source = embedding_policy$capability_source %||% "unknown",
    capability_fingerprint = embedding_policy$capability_fingerprint %||% NA_character_,
    embedding_dimension = ncol(emb_matrix),
    normalization = if (isTRUE(normalize)) "l2" else "none"
  )
  list(
    embeddings = emb_matrix, items_tbl = items_tbl,
    embed_model = esess$embed_model %||% "unknown",
    embed_dim = ncol(emb_matrix),
    embedding_diagnostics = embedding_diagnostics,
    representation_provenance = representation_provenance
  )
}

#' Import externally produced embeddings into SEMANTICA
#'
#' Creates the same validated embedding-result contract consumed by
#' [semantica_wrap()] without requiring a SEMANTICA provider session. This is
#' the preferred boundary for embeddings produced by Python, another R package,
#' a corporate inference service, or a future/custom model.
#'
#' @param embeddings Numeric matrix with one row per item and any positive
#'   embedding dimension.
#' @param items_tbl Data frame containing item identity/metadata.
#' @param id_col Item-ID column in `items_tbl`.
#' @param embedding_ids Explicit row IDs for `embeddings`. Defaults to matrix
#'   row names. Positional alignment is intentionally not guessed.
#' @param normalize Logical; explicitly L2-normalize imported rows.
#' @param provider,model,model_version,task,instruction,source Optional safe
#'   representation provenance. Unknown values may be `NULL`.
#' @param provenance Optional additional named provenance fields. Credential-like
#'   fields are removed by SEMANTICA's provenance sanitizer.
#' @return A `semantica_embedding_result` list compatible with [semantica_wrap()].
#' @export
semantica_import_embeddings <- function(
  embeddings, items_tbl, id_col = "item_id", embedding_ids = rownames(embeddings),
  normalize = FALSE, provider = "external", model = NULL, model_version = NULL,
  task = NULL, instruction = NULL, source = "external", provenance = NULL
) {
  if (!is.data.frame(items_tbl)) stop("'items_tbl' must be a data.frame or tibble.")
  if (!id_col %in% names(items_tbl) && "ID" %in% names(items_tbl)) id_col <- "ID"
  if (!id_col %in% names(items_tbl)) stop("Missing item-ID column: ", id_col)
  if (!is.matrix(embeddings)) {
    if (is.data.frame(embeddings) && all(vapply(embeddings, is.numeric, logical(1L)))) {
      embeddings <- as.matrix(embeddings)
    } else {
      stop("'embeddings' must be a numeric two-dimensional matrix.")
    }
  }
  if (!is.numeric(embeddings) || length(dim(embeddings)) != 2L) {
    stop("'embeddings' must be a numeric two-dimensional matrix.")
  }
  if (nrow(embeddings) < 1L || ncol(embeddings) < 1L) {
    stop("'embeddings' must contain at least one row and one dimension.")
  }
  if (any(!is.finite(embeddings))) stop("'embeddings' contains NA, NaN, or Inf values.")
  normalize <- .semantica_assert_flag(normalize, "normalize", condition_class = "semantica_error_input")

  item_ids <- as.character(items_tbl[[id_col]])
  if (anyNA(item_ids) || any(!nzchar(trimws(item_ids)))) stop("All item IDs must be non-empty.")
  if (anyDuplicated(item_ids)) stop("Item IDs in 'items_tbl' must be unique.")
  if (is.null(embedding_ids)) {
    stop("Embedding row identity is required. Supply matrix row names or 'embedding_ids'; positional alignment is not guessed.")
  }
  embedding_ids <- as.character(embedding_ids)
  if (length(embedding_ids) != nrow(embeddings)) stop("'embedding_ids' length must equal nrow(embeddings).")
  if (anyNA(embedding_ids) || any(!nzchar(trimws(embedding_ids)))) stop("Embedding IDs must be non-empty.")
  if (anyDuplicated(embedding_ids)) stop("Embedding IDs must be unique.")
  missing_embeddings <- setdiff(item_ids, embedding_ids)
  extra_embeddings <- setdiff(embedding_ids, item_ids)
  if (length(missing_embeddings) || length(extra_embeddings)) {
    stop(sprintf(
      "Embedding/item ID sets differ (missing embeddings: %d; extra embedding rows: %d).",
      length(missing_embeddings), length(extra_embeddings)
    ))
  }
  embeddings <- embeddings[match(item_ids, embedding_ids), , drop = FALSE]
  storage.mode(embeddings) <- "double"
  raw_norms <- sqrt(rowSums(embeddings^2))
  if (any(!is.finite(raw_norms) | raw_norms <= .Machine$double.eps)) {
    stop("'embeddings' contains at least one zero or invalid vector.")
  }
  input_normalized <- all(abs(raw_norms - 1) < 1e-4)
  normalization_applied <- isTRUE(normalize)
  if (normalization_applied) embeddings <- embeddings / raw_norms
  final_norms <- sqrt(rowSums(embeddings^2))
  rownames(embeddings) <- item_ids

  safe_extra <- .semantica_sanitize_config_provenance(provenance %||% list())
  representation_provenance <- c(list(
    provider = provider %||% "external",
    model = model %||% "unknown",
    model_version = model_version,
    task = task,
    instruction = instruction,
    dimension = ncol(embeddings),
    input_normalized = input_normalized,
    normalization_applied = normalization_applied,
    normalized = all(abs(final_norms - 1) < 1e-4),
    source = source %||% "external"
  ), safe_extra)
  representation_provenance <- .semantica_sanitize_config_provenance(representation_provenance)

  out <- list(
    embeddings = embeddings,
    items_tbl = items_tbl,
    embed_model = model %||% "unknown",
    embed_dim = ncol(embeddings),
    embedding_diagnostics = list(
      model = model %||% "unknown",
      provider = provider %||% "external",
      n_items = nrow(embeddings),
      embed_dim = ncol(embeddings),
      input_normalized = input_normalized,
      normalization_applied = normalization_applied,
      normalized = all(abs(final_norms - 1) < 1e-4),
      raw_norm_min = min(raw_norms),
      raw_norm_median = stats::median(raw_norms),
      raw_norm_max = max(raw_norms),
      final_norm_min = min(final_norms),
      final_norm_median = stats::median(final_norms),
      final_norm_max = max(final_norms),
      source = source %||% "external",
      representation_provenance = representation_provenance,
      warnings = character(0L)
    ),
    representation_provenance = representation_provenance
  )
  class(out) <- c("semantica_embedding_result", "list")
  out
}

# =================================================================
# P6 & P7  COSINE & WRAPPER
# =================================================================
#' @keywords internal
.compute_cosine_matrix <- function(emb_matrix, already_normalized = TRUE,
                                   adjustment = c("none", "mean_center"),
                                   compute_device = "cpu",
                                   gpu_fallback = NULL,
                                   gpu_precision = "double",
                                   memory_limit = NULL) {
  adjustment <- match.arg(adjustment)
  requested_device <- .semantica_normalize_device(compute_device)
  if (is.null(gpu_fallback)) {
    gpu_fallback <- if (requested_device == "auto") "cpu" else "error"
  }
  computed <- .semantica_compute_cosine(
    emb_matrix = emb_matrix,
    already_normalized = already_normalized,
    adjustment = adjustment,
    compute_device = requested_device,
    gpu_fallback = gpu_fallback,
    gpu_precision = gpu_precision,
    memory_limit = memory_limit
  )
  out <- computed$matrix
  attr(out, "semantica_compute_telemetry") <- computed$telemetry
  out
}

.cosine_diagnostics <- function(cos_mat, factor_assignment = NULL) {
  off <- cos_mat[upper.tri(cos_mat)]
  off <- off[is.finite(off)]
  n_items <- nrow(cos_mat)
  off_mean <- if (length(off)) mean(off) else NA_real_
  # For a cosine Gram matrix of unit vectors, this quantity equals the norm
  # of the mean embedding vector. It is a continuous description of common
  # directional concentration and does not require a model-invariant cutoff.
  common_direction_strength <- if (is.finite(off_mean) && n_items > 0L) {
    sqrt(max(0, (1 + (n_items - 1) * off_mean) / n_items))
  } else NA_real_

  # Spectral concentration is reported descriptively rather than thresholded.
  # Embedding models legitimately differ in anisotropy and effective rank; these
  # quantities are therefore representation-health context, not pass/fail rules.
  spectral <- tryCatch({
    sym <- (cos_mat + t(cos_mat)) / 2
    vals <- eigen(sym, symmetric = TRUE, only.values = TRUE)$values
    finite_vals <- vals[is.finite(vals)]
    positive <- pmax(finite_vals, 0)
    total_positive <- sum(positive)
    if (!length(finite_vals) || !is.finite(total_positive) || total_positive <= 0) {
      list(
        effective_rank = NA_real_, effective_rank_ratio = NA_real_,
        top_eigen_share = NA_real_, top3_eigen_share = NA_real_,
        negative_eigen_mass_ratio = NA_real_
      )
    } else {
      prob <- positive[positive > 0] / total_positive
      eff_rank <- exp(-sum(prob * log(prob)))
      sorted_positive <- sort(positive, decreasing = TRUE)
      negative_mass <- sum(abs(finite_vals[finite_vals < 0]))
      total_abs <- sum(abs(finite_vals))
      list(
        effective_rank = eff_rank,
        effective_rank_ratio = eff_rank / max(1, n_items),
        top_eigen_share = sorted_positive[[1L]] / total_positive,
        top3_eigen_share = sum(utils::head(sorted_positive, 3L)) / total_positive,
        negative_eigen_mass_ratio = if (total_abs > 0) negative_mass / total_abs else 0
      )
    }
  }, error = function(e) {
    list(
      effective_rank = NA_real_, effective_rank_ratio = NA_real_,
      top_eigen_share = NA_real_, top3_eigen_share = NA_real_,
      negative_eigen_mass_ratio = NA_real_
    )
  })

  out <- list(
    n_items = n_items,
    offdiag_mean = off_mean,
    offdiag_median = if (length(off)) stats::median(off) else NA_real_,
    offdiag_sd = if (length(off) > 1L) stats::sd(off) else NA_real_,
    offdiag_q05 = if (length(off)) as.numeric(stats::quantile(off, 0.05, names = FALSE, na.rm = TRUE)) else NA_real_,
    offdiag_q95 = if (length(off)) as.numeric(stats::quantile(off, 0.95, names = FALSE, na.rm = TRUE)) else NA_real_,
    offdiag_min = if (length(off)) min(off) else NA_real_,
    offdiag_max = if (length(off)) max(off) else NA_real_,
    common_direction_strength = common_direction_strength,
    effective_rank = spectral$effective_rank,
    effective_rank_ratio = spectral$effective_rank_ratio,
    top_eigen_share = spectral$top_eigen_share,
    top3_eigen_share = spectral$top3_eigen_share,
    negative_eigen_mass_ratio = spectral$negative_eigen_mass_ratio,
    spectral_note = paste(
      "Effective rank and eigenvalue concentration are descriptive representation-health metrics.",
      "No model-invariant cutoff is applied and SEMANTICA does not alter the representation from them."
    ),
    possible_anisotropy = NA,
    possible_anisotropy_note = paste(
      "No universal anisotropy cutoff is applied.",
      "Inspect common_direction_strength and none-vs-mean-center sensitivity for the configured embedding model."
    )
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
                                                   top_pair_fraction = 0.05,
                                                   standard_cosine = NULL,
                                                   centered_cosine = NULL,
                                                   compute_device = "cpu",
                                                   gpu_fallback = NULL,
                                                   gpu_precision = "double",
                                                   memory_limit = NULL) {
  fail <- list(
    available = FALSE,
    source = "none_vs_mean_center",
    note = "Cosine-adjustment sensitivity unavailable."
  )
  if (is.null(emb_matrix) || !is.matrix(emb_matrix) || nrow(emb_matrix) < 2L) return(fail)
  standard <- standard_cosine
  if (is.null(standard)) {
    standard <- tryCatch(
      .compute_cosine_matrix(
        emb_matrix, already_normalized = already_normalized, adjustment = "none",
        compute_device = compute_device, gpu_fallback = gpu_fallback,
        gpu_precision = gpu_precision, memory_limit = memory_limit
      ),
      error = function(e) NULL
    )
  }
  centered <- centered_cosine
  if (is.null(centered)) {
    centered <- tryCatch(
      .compute_cosine_matrix(
        emb_matrix, already_normalized = already_normalized, adjustment = "mean_center",
        compute_device = compute_device, gpu_fallback = gpu_fallback,
        gpu_precision = gpu_precision, memory_limit = memory_limit
      ),
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

  # Model-relative sensitivity: compare the identity of the strongest pairwise
  # relations by rank rather than relying only on an absolute cosine cutoff.
  top_pair_fraction <- suppressWarnings(as.numeric(top_pair_fraction[1L]))
  if (!is.finite(top_pair_fraction) || top_pair_fraction <= 0 || top_pair_fraction > 1) {
    top_pair_fraction <- 0.05
  }
  n_pair <- sum(finite)
  top_n <- max(1L, min(n_pair, as.integer(ceiling(n_pair * top_pair_fraction))))
  std_order <- order(off_standard[finite], decreasing = TRUE, na.last = NA)
  ctr_order <- order(off_centered[finite], decreasing = TRUE, na.last = NA)
  top_standard_idx <- std_order[seq_len(min(top_n, length(std_order)))]
  top_centered_idx <- ctr_order[seq_len(min(top_n, length(ctr_order)))]
  top_union <- union(top_standard_idx, top_centered_idx)
  top_pair_jaccard <- if (length(top_union)) {
    length(intersect(top_standard_idx, top_centered_idx)) / length(top_union)
  } else {
    NA_real_
  }
  # For two independently selected top-k sets among N pairs, the ratio of the
  # expected intersection size to expected union size is k / (2N - k). This
  # provides a finite-pool random-overlap reference without inventing a model-
  # specific stability cutoff. Falling at/below this baseline is a strong
  # warning that the identity of the strongest semantic relations is not more
  # reproducible across preprocessing choices than random top-pair membership.
  effective_top_fraction <- top_n / max(1L, n_pair)
  top_pair_random_baseline <- effective_top_fraction / (2 - effective_top_fraction)
  top_pair_excess_over_random <- top_pair_jaccard - top_pair_random_baseline
  top_pair_overlap_vs_random <- if (!is.finite(top_pair_jaccard)) {
    NA_character_
  } else if (top_pair_jaccard <= top_pair_random_baseline) {
    "at_or_below_random_reference"
  } else {
    "above_random_reference"
  }
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
    top_pair_fraction = top_pair_fraction,
    top_pair_fraction_effective = effective_top_fraction,
    top_pair_n = top_n,
    top_pair_jaccard = top_pair_jaccard,
    top_pair_jaccard_random_baseline = top_pair_random_baseline,
    top_pair_jaccard_excess_over_random = top_pair_excess_over_random,
    top_pair_overlap_vs_random = top_pair_overlap_vs_random,
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
#' @param cosine_sensitivity_max_items Maximum item count used directly in the optional sensitivity diagnostic before deterministic subsampling.
#' @param cosine_sensitivity_seed Seed used if the sensitivity diagnostic subsamples a large item pool.
#' @param compute_device Cosine compute backend: `"cpu"` (compatibility
#'   default), `"auto"` (currently CPU until crossover benchmarks are
#'   validated), `"cuda"`, `"cuda:N"`, or `"mps"`.
#' @param gpu_fallback Optional fallback policy. `NULL` means an explicit GPU
#'   request errors if unavailable, while `"auto"` may use CPU. Set to `"cpu"`
#'   to explicitly permit CPU fallback for a requested accelerator.
#' @param gpu_precision `"double"` (default) or explicitly requested
#'   `"single"`. MPS requires single precision for the torch path.
#' @param compute_memory_limit Optional conservative working-memory ceiling in
#'   bytes for the cosine calculation.
#' @param verbose              Print summary.
#' @section Side effects:
#' Computes local matrix diagnostics. If the optional cosine-sensitivity
#' diagnostic must subsample a large pool, it uses the documented seed inside a
#' caller-RNG-preserving scope; no provider or filesystem I/O is performed here.
#'
#' @section Reproducibility:
#' `cosine_sensitivity_seed` controls the deliberate large-pool subsample and the
#' sampled seed is recorded in the returned sensitivity diagnostics. The selected
#' cosine representation itself is deterministic for fixed embeddings/settings.
#'
#' @return Named list with `$cosine_sim_matrix`, `$df`, `$i.per.f`, cosine and
#'   adjustment diagnostics, and `$compute_telemetry` describing the requested
#'   and resolved backend, precision, fallback, memory estimate, and timing.
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
                           cosine_sensitivity_max_items = 1500L,
                           cosine_sensitivity_seed = 1L,
                           compute_device = "cpu", gpu_fallback = NULL,
                           gpu_precision = "double",
                           compute_memory_limit = NULL,
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
  cos_mat <- .compute_cosine_matrix(
    emb_a,
    already_normalized = already_normalized,
    adjustment = cosine_adjustment,
    compute_device = compute_device,
    gpu_fallback = gpu_fallback,
    gpu_precision = gpu_precision,
    memory_limit = compute_memory_limit
  )
  compute_telemetry <- attr(cos_mat, "semantica_compute_telemetry", exact = TRUE)
  attr(cos_mat, "semantica_compute_telemetry") <- NULL
  rownames(cos_mat) <- colnames(cos_mat) <- common_ids; storage.mode(cos_mat) <- "double"
  cosine_adjustment_sensitivity <- if (isTRUE(compute_cosine_sensitivity)) {
    max_sens <- suppressWarnings(as.integer(cosine_sensitivity_max_items[1L]))
    if (!is.finite(max_sens) || max_sens < 4L) max_sens <- 1500L
    sens_ids <- common_ids
    sampled <- FALSE
    if (length(sens_ids) > max_sens) {
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit({
        if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(as.integer(cosine_sensitivity_seed[1L]))
      sens_ids <- sample(sens_ids, max_sens, replace = FALSE)
      sampled <- TRUE
    }
    emb_sens <- emb_a[sens_ids, , drop = FALSE]
    fa_sens <- factor_lookup[sens_ids]
    standard_sens <- if (cosine_adjustment == "none") cos_mat[sens_ids, sens_ids, drop = FALSE] else NULL
    centered_sens <- if (cosine_adjustment == "mean_center") cos_mat[sens_ids, sens_ids, drop = FALSE] else NULL
    ans <- .compute_cosine_adjustment_sensitivity(
      emb_sens,
      already_normalized = already_normalized,
      factor_assignment = fa_sens,
      standard_cosine = standard_sens,
      centered_cosine = centered_sens,
      compute_device = compute_device,
      gpu_fallback = gpu_fallback,
      gpu_precision = gpu_precision,
      memory_limit = compute_memory_limit
    )
    ans$sampled <- sampled
    ans$n_items_total <- length(common_ids)
    ans$n_items_used <- length(sens_ids)
    ans$sample_seed <- if (sampled) as.integer(cosine_sensitivity_seed[1L]) else NA_integer_
    ans
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
  discrimination_diag <- tryCatch(
    semantica_semantic_discrimination(cos_mat, factor_lookup),
    error = function(e) list(estimate = NA_real_, status = "failed", reason = conditionMessage(e))
  )
  cosine_diagnostics$stochastic_superiority <- discrimination_diag$estimate %||% NA_real_
  cosine_diagnostics$stochastic_superiority_status <- discrimination_diag$status %||% "unavailable"
  cosine_diagnostics$stochastic_superiority_reason <- discrimination_diag$reason %||% NULL
  df_s <- data.frame(item = common_ids, type = factors_v, factor = factors_v, item_text = item_text,
                     ID = metadata$ID, Dimension = metadata$Dimension, Facet = metadata$Facet,
                     stringsAsFactors = FALSE)
  rownames(df_s) <- common_ids
  counts <- table(factors_v)
  i_per_f <- setNames(pmin(3L, as.integer(counts[factors_u])), factors_u)
  if (verbose) {
    cat("============================================================\nSTEP 4 -- SEMANTICA INPUT WRAPPER\n============================================================\n")
    cat(sprintf("  Cosine adjustment: %s\n", cosine_adjustment))
    if (!is.null(compute_telemetry)) {
      cat(sprintf(
        "  Compute device   : %s -> %s (%s, %s precision)\n",
        compute_telemetry$requested_device,
        compute_telemetry$resolved_device,
        compute_telemetry$backend,
        compute_telemetry$resolved_precision
      ))
      if (isTRUE(compute_telemetry$used_fallback)) {
        cat(sprintf("  Compute fallback : %s\n", compute_telemetry$fallback_reason))
      }
    }
    cat(sprintf("  Cosine offdiag   : mean %.4f | q95 %.4f | max %.4f\n",
                cosine_diagnostics$offdiag_mean,
                cosine_diagnostics$offdiag_q95,
                cosine_diagnostics$offdiag_max))
    if (is.finite(cosine_diagnostics$common_direction_strength %||% NA_real_)) {
      cat(sprintf("  Common direction : %.4f (descriptive; no universal cutoff applied)\n",
                  cosine_diagnostics$common_direction_strength))
    }
    if (is.finite(cosine_diagnostics$effective_rank %||% NA_real_)) {
      cat(sprintf(
        "  Spectral profile : effective rank %.2f/%d | top eigen share %.3f (descriptive)\n",
        cosine_diagnostics$effective_rank, nrow(cos_mat),
        cosine_diagnostics$top_eigen_share %||% NA_real_
      ))
    }
    if (isTRUE(cosine_adjustment_sensitivity$available)) {
      cat(sprintf("  Cosine sensitivity: none vs mean_center offdiag r=%.3f | q95 |delta|=%.3f | top %.1f%% pair J=%.3f (random ref=%.3f)\n",
                  cosine_adjustment_sensitivity$offdiag_correlation,
                  cosine_adjustment_sensitivity$q95_abs_delta,
                  100 * (cosine_adjustment_sensitivity$top_pair_fraction_effective %||% cosine_adjustment_sensitivity$top_pair_fraction %||% 0.05),
                  cosine_adjustment_sensitivity$top_pair_jaccard %||% NA_real_,
                  cosine_adjustment_sensitivity$top_pair_jaccard_random_baseline %||% NA_real_))
      if (identical(cosine_adjustment_sensitivity$top_pair_overlap_vs_random %||% "",
                    "at_or_below_random_reference")) {
        cat("  Representation  : WARNING -- top-pair agreement across cosine preprocessing is at/below its finite-pool random reference.\n")
      }
    }
    if (isTRUE(calibration$info$applied)) {
      cat(sprintf("  Calibration      : %s semantic proxy calibration applied before ACO/ESEM.\n",
                  calibration$info$method %||% "external"))
    }
  }
  representation_stability <- list(
    common_direction_strength = cosine_diagnostics$common_direction_strength %||% NA_real_,
    effective_rank = cosine_diagnostics$effective_rank %||% NA_real_,
    effective_rank_ratio = cosine_diagnostics$effective_rank_ratio %||% NA_real_,
    top_eigen_share = cosine_diagnostics$top_eigen_share %||% NA_real_,
    top3_eigen_share = cosine_diagnostics$top3_eigen_share %||% NA_real_,
    negative_eigen_mass_ratio = cosine_diagnostics$negative_eigen_mass_ratio %||% NA_real_,
    cosine_adjustment_sensitivity = cosine_adjustment_sensitivity,
    top_pair_overlap_warning = isTRUE(
      identical(
        cosine_adjustment_sensitivity$top_pair_overlap_vs_random %||% NA_character_,
        "at_or_below_random_reference"
      )
    ),
    automatic_adjustment = FALSE,
    note = paste(
      "SEMANTICA reports representation sensitivity but does not automatically switch cosine transformations.",
      "Representation concentration is referenced descriptively to an isotropic random-vector null and",
      "does not by itself invalidate a language embedding representation."
    )
  )
  embedding_diag_for_state <- embed_result$embedding_diagnostics %||% list(
    n_items = nrow(emb_a), embed_dim = ncol(emb_a)
  )
  if (is.null(embedding_diag_for_state$n_items)) embedding_diag_for_state$n_items <- nrow(emb_a)
  if (is.null(embedding_diag_for_state$embed_dim)) embedding_diag_for_state$embed_dim <- ncol(emb_a)
  representation_evidence_state <- .semantica_representation_evidence_state(
    representation_stability = representation_stability,
    cosine_diagnostics = cosine_diagnostics,
    embedding_diagnostics = embedding_diag_for_state
  )

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
       representation_stability = representation_stability,
       representation_evidence_state = representation_evidence_state,
       compute_telemetry = compute_telemetry,
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
#' @param backend_spec,embed_backend_spec Optional explicit custom backend
#'   contracts created by [semantica_backend_spec()] for generation and
#'   embeddings respectively.
#' @param base_url         Override host:port for generation.
#' @param embed_base_url   Override host:port for embedding.
#' @param api_key          Generation API key.
#' @param embed_api_key    Embedding API key.
#' @param hf_token,embed_hf_token Hugging Face tokens for local generation and
#'   an optional separate embedding session. Tokens are used only by live
#'   sessions and are removed from returned results.
#' @param chat_model       Override default chat model.
#' @param embed_model      Override default embed model.
#' @param embedding_device,chat_device,device_map,gpu_layers,model_precision
#'   Local-model device controls passed to [semantica_connect()].
#' @param embed_batch_size Items per embedding backend request. Smaller values
#'   reduce per-request memory pressure for local embedding models.
#' @param timeout_s,embed_timeout_s HTTP timeout in seconds for generation and
#'   optional separate embedding sessions. `embed_timeout_s = NULL` reuses
#'   `timeout_s`.
#' @param retry_max_tries,retry_on_failure HTTP resilience controls for retryable provider failures.
#' @param embedding_cache,embedding_cache_dir,embedding_cache_namespace Persistent embedding-cache controls.
#' @param embedding_spec Optional embedding capability contract created by
#'   [semantica_embedding_spec()]. It controls provider/task text preparation
#'   only; it does not change psychometric thresholds or objective weights.
#' @param cosine_adjustment Cosine preprocessing for embeddings. `"none"`
#'   preserves standard normalized cosine; `"mean_center"` subtracts the pool
#'   centroid before cosine as an anisotropy sensitivity check.
#' @param semantic_calibration Optional matrix or function passed to
#'   `semantica_wrap()` to calibrate the embedding cosine proxy.
#' @param compute_cosine_sensitivity Logical; compute the optional
#'   none-versus-mean-centered cosine sensitivity diagnostic.
#' @param cosine_sensitivity_max_items Maximum number of items used for the optional sensitivity diagnostic; the optimization matrix still uses all items.
#' @param cosine_sensitivity_seed Seed used when the sensitivity diagnostic samples a very large pool.
#' @param compute_device Cosine compute backend passed to [semantica_wrap()]:
#'   `"cpu"`, conservative `"auto"`, `"cuda"`, `"cuda:N"`, or `"mps"`.
#' @param gpu_fallback Optional explicit accelerator fallback policy. `NULL`
#'   errors for an unavailable requested GPU and permits CPU for `"auto"`;
#'   use `"cpu"` to permit an explicit GPU request to fall back.
#' @param gpu_precision Cosine precision policy: `"double"` or explicitly
#'   requested `"single"`; the MPS path requires single precision.
#' @param compute_memory_limit Optional conservative cosine working-memory
#'   ceiling in bytes.
#' @param release_local_models Logical; after embedding and wrapping, remove
#'   cached `python_llamacpp` model instances and request garbage collection.
#'   This reduces retained RAM for one-shot local runs at the cost of reloading
#'   models in a subsequent call.
#' @param retain_embeddings Logical; retain the dense embedding matrix in the
#'   returned `embed_result`. Set to `FALSE` after the cosine matrix has been
#'   built when later facet/unit embedding diagnostics are not needed.
#' @param preflight Logical; run provider/model preflight checks.
#' @param embedding_task Embedding-task policy. `"auto"` applies documented model-specific task instructions only when the configured embedding model requires them.
#' @param embedding_instruction Optional explicit embedding prefix/instruction overriding the automatic model policy.
#' @param content_alignment Logical; compute item-to-factor/facet definition
#'   alignment when usable definitions and embeddings are available.
#' @param content_exclusions Optional named list of factor-specific concepts
#'   that should not define each construct. The high-level full pipeline derives
#'   this automatically from a construct blueprint or factor `forbidden` fields.
#' @param generation_seed Optional nonnegative generation master seed forwarded
#'   to [semantica_generate_items()]. In the high-level pipeline this inherits
#'   the run master seed. Seed control is backend-specific and is recorded in
#'   generation provenance. A controlled seed fixes SEMANTICA's seed schedule;
#'   exact LLM text replay is not guaranteed, so exact downstream replay uses
#'   the saved item pool and its fingerprint.
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
#' @section Side effects:
#' May read credentials/environment settings, perform provider network I/O or
#' local Python inference, and read/write the persistent embedding cache. The
#' optional cosine-sensitivity subsample uses its documented seed in a
#' caller-RNG-preserving scope.
#'
#' @section Reproducibility:
#' Returned session/model/device diagnostics describe the resolved execution
#' environment available to SEMANTICA. Remote model aliases may remain mutable;
#' cached embeddings preserve the vectors associated with their recorded key.
#'
#' @return A `semantica_pipeline_result` ready for `ACO_with_ESEM()`, including
#'   the cosine matrix and item metadata, sanitized session metadata,
#'   embedding/cosine diagnostics, and stage `$performance` telemetry.
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
                               hf_token = NULL, embed_hf_token = NULL,
                               embedding_device = "auto", chat_device = "auto",
                               device_map = NULL, gpu_layers = "auto",
                               model_precision = "auto",
                               timeout_s = 120L, embed_timeout_s = NULL,
                               retry_max_tries = 4L, retry_on_failure = TRUE,
                               preflight = TRUE,
                               gguf_path = NULL, scale_name,
                               scale_description, factors, n_per_factor = 15L,
                               n_per_factor_override = !missing(n_per_factor),
                               cosine_adjustment = c("none", "mean_center"),
                               semantic_calibration = NULL,
                               compute_cosine_sensitivity = TRUE,
                               cosine_sensitivity_max_items = 1500L,
                               cosine_sensitivity_seed = 1L,
                               compute_device = "cpu", gpu_fallback = NULL,
                               gpu_precision = "double",
                               compute_memory_limit = NULL,
                               release_local_models = FALSE,
                               retain_embeddings = TRUE,
                               embedding_cache = TRUE,
                               embedding_cache_dir = NULL,
                               embedding_cache_namespace = NULL,
                               embedding_spec = NULL,
                               verbose = TRUE,
                               embedding_task = "auto", embedding_instruction = NULL,
                               content_alignment = TRUE, content_exclusions = NULL, ...,
                               backend_spec = NULL, embed_backend_spec = NULL,
                               generation_seed = NULL) {
  pipeline_started <- proc.time()[["elapsed"]]
  cosine_adjustment <- match.arg(cosine_adjustment)
  embed_backend_eff <- embed_backend %||% backend
  generation_spec <- if (!is.null(backend_spec)) backend_spec else SEMANTICA_BACKENDS[[backend]]
  generation_caps <- if (!is.null(generation_spec)) .semantica_backend_capabilities(generation_spec) else NULL
  if (is.null(generation_caps) && is.null(backend_spec)) {
    stop("Unknown generation backend '", backend, "'. Provide an explicit semantica_backend_spec().")
  }
  if (!isTRUE(generation_caps$can_embed) && is.null(embed_backend) && is.null(embed_backend_spec)) {
    stop("Backend '", backend, "' does not provide embeddings in SEMANTICA. Specify 'embed_backend'/'embed_backend_spec'.")
  }

  separate_embed <- !identical(embed_backend_eff, backend) || !is.null(embed_backend_spec) ||
    !is.null(embed_api_key) || !is.null(embed_base_url) ||
    !is.null(embed_hf_token)
  session <- semantica_connect(
    backend = backend, api_key = api_key, chat_model = chat_model,
    embed_model = if (separate_embed) NULL else embed_model,
    base_url = base_url, gguf_path = gguf_path, hf_token = hf_token,
    embedding_device = embedding_device, chat_device = chat_device,
    device_map = device_map, gpu_layers = gpu_layers,
    model_precision = model_precision, timeout_s = timeout_s,
    retry_max_tries = retry_max_tries,
    retry_on_failure = retry_on_failure,
    preflight = preflight, purpose = if (separate_embed) "chat" else "both",
    embedding_task = embedding_task, embedding_instruction = embedding_instruction,
    embedding_spec = if (separate_embed) NULL else embedding_spec,
    backend_spec = backend_spec, verbose = verbose
  )

  embed_key <- if (!is.null(embed_api_key)) embed_api_key else if (identical(embed_backend_eff, backend)) api_key else NULL
  embed_session <- if (separate_embed) {
    semantica_connect(
      backend = embed_backend_eff, api_key = embed_key, embed_model = embed_model,
      base_url = embed_base_url %||% base_url, gguf_path = gguf_path,
      hf_token = embed_hf_token %||% hf_token,
      embedding_device = embedding_device,
      chat_device = "auto", device_map = NULL, gpu_layers = gpu_layers,
      model_precision = model_precision,
      timeout_s = embed_timeout_s %||% timeout_s,
      retry_max_tries = retry_max_tries,
      retry_on_failure = retry_on_failure,
      preflight = preflight, purpose = "embed",
      embedding_task = embedding_task, embedding_instruction = embedding_instruction,
      embedding_spec = embedding_spec,
      backend_spec = embed_backend_spec, verbose = verbose
    )
  } else NULL
  generation_started <- proc.time()[["elapsed"]]
  items_tbl <- semantica_generate_items(
    session, scale_name, scale_description, factors,
    n_per_factor = n_per_factor,
    n_per_factor_override = n_per_factor_override,
    verbose = verbose, seed = generation_seed, ...
  )
  generation_seconds <- proc.time()[["elapsed"]] - generation_started
  generation_provenance <- attr(items_tbl, "semantica_generation_metadata") %||% list(
    schema = "semantica-generation-provenance-unavailable",
    content_screening_status = "unknown"
  )
  generated_item_metadata <- semantica_standardize_item_metadata(items_tbl)
  embedding_started <- proc.time()[["elapsed"]]
  embed_result <- semantica_embed(
    items_tbl, session, embed_session,
    batch_size = embed_batch_size,
    cache = embedding_cache,
    cache_dir = embedding_cache_dir,
    cache_namespace = embedding_cache_namespace,
    verbose = verbose
  )
  embedding_seconds <- proc.time()[["elapsed"]] - embedding_started
  content_alignment_result <- NULL
  if (isTRUE(content_alignment)) {
    content_alignment_result <- tryCatch(
      .semantica_definition_alignment(
        items_tbl, embed_result$embeddings, factors, embed_session %||% session,
        cache = embedding_cache, cache_dir = embedding_cache_dir,
        cache_namespace = embedding_cache_namespace, batch_size = embed_batch_size,
        exclusions = content_exclusions
      ), error = function(e) list(available = FALSE, note = conditionMessage(e), table = NULL)
    )
    if (isTRUE(content_alignment_result$available) && !is.null(content_alignment_result$table)) {
      at <- content_alignment_result$table
      idxa <- match(as.character(items_tbl$item_id %||% items_tbl$ID), at$item_id)
      addcols <- setdiff(names(at), "item_id")
      for (cc in addcols) items_tbl[[cc]] <- at[[cc]][idxa]
      generated_item_metadata <- semantica_standardize_item_metadata(items_tbl)
    }
  }
  generation_provenance$content_screening_status <- if (isTRUE(content_alignment_result$available) && !is.null(content_alignment_result$table)) {
    "definition_alignment_performed_downstream"
  } else if (isTRUE(content_alignment)) {
    "definition_alignment_requested_but_unavailable"
  } else {
    "definition_alignment_not_requested"
  }
  generation_provenance$content_screening_note <- if (identical(generation_provenance$content_screening_status, "definition_alignment_performed_downstream")) {
    "Generation/lexical curation was followed by embedding-derived construct-definition alignment; later ACO guards remain feasibility-aware."
  } else {
    "Generated candidates were not construct-qualified by a successful definition-alignment stage in this pipeline call."
  }
  cosine_started <- proc.time()[["elapsed"]]
  wrapped <- semantica_wrap(
    embed_result, items_tbl = items_tbl,
    cosine_adjustment = cosine_adjustment,
    semantic_calibration = semantic_calibration,
    compute_cosine_sensitivity = compute_cosine_sensitivity,
    cosine_sensitivity_max_items = cosine_sensitivity_max_items,
    cosine_sensitivity_seed = cosine_sensitivity_seed,
    compute_device = compute_device,
    gpu_fallback = gpu_fallback,
    gpu_precision = gpu_precision,
    compute_memory_limit = compute_memory_limit,
    verbose = verbose
  )
  cosine_seconds <- proc.time()[["elapsed"]] - cosine_started
  align_cols <- grep("^semantica_", names(items_tbl), value = TRUE)
  if (length(align_cols) && !is.null(wrapped$df)) {
    mid <- if ("item_id" %in% names(items_tbl)) as.character(items_tbl$item_id) else as.character(items_tbl$ID)
    wi <- match(as.character(wrapped$df$item), mid)
    for (cc in align_cols) wrapped$df[[cc]] <- items_tbl[[cc]][wi]
    wi2 <- match(wrapped$item_metadata$ID, mid)
    for (cc in align_cols) wrapped$item_metadata[[cc]] <- items_tbl[[cc]][wi2]
    wrapped$generated_item_metadata <- wrapped$item_metadata
  }
  result <- c(wrapped, list(session = sanitize_session_for_result(session),
                            embed_session = sanitize_session_for_result(embed_session), items_tbl_raw = items_tbl,
                            generation_provenance = generation_provenance,
                            generated_item_metadata = wrapped$generated_item_metadata %||% generated_item_metadata,
                            item_metadata = wrapped$item_metadata %||% generated_item_metadata,
                            embed_result = embed_result, content_alignment = content_alignment_result,
                            embedding_policy = .semantica_embedding_policy(
                              (embed_session %||% session)$embed_model,
                              (embed_session %||% session)$embedding_task %||% "auto",
                              (embed_session %||% session)$embedding_instruction %||% NULL,
                              (embed_session %||% session)$embedding_spec %||% NULL
                            ),
                            performance = list(
                              generation_seconds = unname(generation_seconds),
                              embedding_seconds = unname(embedding_seconds),
                              cosine_seconds = unname(cosine_seconds),
                              total_seconds = unname(proc.time()[["elapsed"]] - pipeline_started),
                              compute = wrapped$compute_telemetry
                            )))
  if (!isTRUE(retain_embeddings)) {
    result$embed_result$embeddings <- NULL
  }
  if (isTRUE(release_local_models) &&
      any(c(session$protocol, embed_session$protocol %||% NA_character_) == "python_llamacpp", na.rm = TRUE)) {
    .semantica_clear_python_model_cache(pattern = '^"llamacpp"\\|')
  }
  class(result) <- c("semantica_pipeline_result", "list")
  sanitize_result_for_serialization(result)
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

#' Export SEMANTICA results for human-readable/interchange use
#'
#' For a high-level [semantica_run()] or [semantica_full_pipeline()] result,
#' writes the selected final scale, evidence status, readable summary, and
#' sanitized resolved configuration. For legacy component results from
#' [semantica_pipeline()] or [semantica_wrap()], preserves the historical
#' item/metadata/cosine CSV export unchanged.
#'
#' @param pipeline_result A high-level SEMANTICA result, or a legacy component
#'   result from [semantica_pipeline()] or [semantica_wrap()].
#' @param prefix Character string to prepend to output filenames.
#' @param include_candidates Logical; for high-level results, also export the
#'   generated candidate-item table when it is available.
#' @param format Export contract: `"auto"` selects a report for high-level results and optimizer interchange for component results; `"report"` and `"optimizer"` require the matching result type.
#' @param quiet Logical; suppress completion messages while still returning written paths.
#' @section Side effects:
#' High-level results write `_selected_items.csv`, `_evidence_status.csv`,
#' `_summary.txt`, and `_config.json`, plus an optional `_candidate_items.csv`.
#' Legacy component results retain the existing `_items.csv`, `_df.csv`, and
#' `_cosine_matrix.csv` files consumed by [semantica_reload()].
#'
#' @section Reproducibility:
#' These exports are human-readable/interchange artifacts, not the canonical
#' exact-replay artifact. Use [semantica_save_bundle()] to preserve full
#' provenance and analysis state.
#'
#' @return For high-level results, invisibly returns a named list of written
#'   paths. For legacy component results, invisibly returns the prefix as before.
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
semantica_export <- function(pipeline_result, prefix = "SEMANTICA", include_candidates = FALSE, format = c("auto", "report", "optimizer"), quiet = FALSE) {
  include_candidates <- .semantica_assert_flag(include_candidates, "include_candidates")
  quiet <- .semantica_assert_flag(quiet, "quiet")
  format <- match.arg(format)
  is_high_level <- inherits(pipeline_result, "semantica_full_pipeline_result")
  if (identical(format, "report") && !is_high_level) stop("format = 'report' requires a semantica_run()/semantica_full_pipeline() result.", call. = FALSE)
  if (identical(format, "optimizer") && is_high_level) stop("format = 'optimizer' expects a semantica_wrap()/semantica_pipeline() component result. Use format = 'report' for a completed high-level analysis, or semantica_save_bundle() for exact replay.", call. = FALSE)

  # High-level results get a user-oriented export: the final selected scale,
  # evidence state, a readable summary, and sanitized resolved configuration.
  # This is presentation/interchange only; semantica_save_bundle() remains the
  # canonical provenance-preserving artifact.
  if (is_high_level) {
    out_dir <- dirname(prefix)
    if (!identical(out_dir, ".") && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    paths <- list(
      selected_items = paste0(prefix, "_selected_items.csv"),
      evidence_status = paste0(prefix, "_evidence_status.csv"),
      summary = paste0(prefix, "_summary.txt"),
      config = paste0(prefix, "_config.json")
    )
    utils::write.csv(semantica_items(pipeline_result, details = TRUE), paths$selected_items, row.names = FALSE)
    evidence <- tryCatch(semantica_evidence_status(pipeline_result), error = function(e) NULL)
    if (is.data.frame(evidence)) {
      utils::write.csv(evidence, paths$evidence_status, row.names = FALSE)
    } else {
      utils::write.csv(data.frame(status = "Evidence status unavailable.", stringsAsFactors = FALSE), paths$evidence_status, row.names = FALSE)
    }
    writeLines(utils::capture.output(print(summary(pipeline_result))), paths$summary, useBytes = TRUE)
    cfg <- tryCatch(semantica_config(pipeline_result), error = function(e) list(note = conditionMessage(e)))
    jsonlite::write_json(cfg, paths$config, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
    if (isTRUE(include_candidates)) {
      candidates <- pipeline_result$generated_item_metadata %||% pipeline_result$generation$items_tbl %||% NULL
      if (is.data.frame(candidates)) {
        paths$candidate_items <- paste0(prefix, "_candidate_items.csv")
        utils::write.csv(candidates, paths$candidate_items, row.names = FALSE)
      }
    }
    if (!isTRUE(quiet)) cat(sprintf(
      "Exported selected scale and user-facing diagnostics with prefix '%s'.\nFor exact reproducibility, also use semantica_save_bundle().\n",
      prefix
    ))
    return(invisible(paths))
  }

  # Backward-compatible component/interchange export.
  utils::write.csv(pipeline_result$items_tbl, paste0(prefix,"_items.csv"), row.names=FALSE)
  utils::write.csv(pipeline_result$df, paste0(prefix,"_df.csv"), row.names=FALSE)
  utils::write.csv(pipeline_result$cosine_sim_matrix, paste0(prefix,"_cosine_matrix.csv"))
  if (!isTRUE(quiet)) cat(sprintf("Exported optimizer-interchange files: %s_items.csv | %s_df.csv | %s_cosine_matrix.csv\n", prefix, prefix, prefix))
  invisible(prefix)
}

#' Reload an optimizer-interchange export
#'
#' Reads the legacy/component optimizer-interchange CSV files created by
#' [semantica_export()] with `format = "optimizer"` (or `format = "auto"` for a component result). Human-readable high-level report exports are not reconstructive; use [semantica_load_bundle()] to restore a complete analysis.
#'
#' @param prefix Character string matching the prefix used during export.
#' @param i.per.f Optional named integer vector overriding the number of items
#'   to select per factor after reload.
#' @param default_i_per_f Default number of items to select per factor when
#'   `i.per.f` is not supplied; capped at the number of available items.
#' @section Side effects:
#' Reads the three CSV files created by [semantica_export()] and performs
#' cross-file structural integrity checks before returning optimizer inputs.
#'
#' @section Reproducibility:
#' Reload validates structure and consistency but does not make CSV files
#' equivalent to a provenance-preserving SEMANTICA analysis bundle.
#'
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
  reload_abort <- function(message, invariant, ...) {
    .semantica_abort(
      message,
      subclass = "semantica_error_integrity",
      stage = "reload",
      invariant = invariant,
      ...
    )
  }
  read_reload_csv <- function(path, ...) {
    tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...),
      error = function(e) reload_abort(
        sprintf("Could not read SEMANTICA reload file '%s': %s", basename(path), conditionMessage(e)),
        "csv_parse",
        file = basename(path)
      )
    )
  }
  pick_id <- function(x, candidates, label) {
    hit <- intersect(candidates, names(x))
    if (length(hit) == 0L) {
      reload_abort(
        sprintf("Reloaded %s file has no supported item-ID column (%s).",
                label, paste(candidates, collapse = ", ")),
        paste0(label, "_item_id_column")
      )
    }
    ids <- as.character(x[[hit[[1L]]]])
    if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
      reload_abort(sprintf("Reloaded %s file contains missing/empty item IDs.", label),
                   paste0(label, "_item_ids_complete"))
    }
    if (anyDuplicated(ids)) {
      reload_abort(sprintf("Reloaded %s file contains duplicated item IDs.", label),
                   paste0(label, "_item_ids_unique"))
    }
    ids
  }

  items_tbl <- read_reload_csv(paste0(prefix, "_items.csv"))
  df <- read_reload_csv(paste0(prefix, "_df.csv"))
  cos_df <- read_reload_csv(
    paste0(prefix, "_cosine_matrix.csv"), row.names = 1
  )

  if (nrow(cos_df) != ncol(cos_df) || nrow(cos_df) < 1L) {
    reload_abort("Reloaded cosine matrix must be a non-empty square matrix.",
                 "cosine_square")
  }
  if (!all(vapply(cos_df, is.numeric, logical(1L)))) {
    reload_abort("Reloaded cosine matrix contains non-numeric columns.",
                 "cosine_numeric")
  }
  cos_mat <- as.matrix(cos_df)
  storage.mode(cos_mat) <- "double"
  if (any(!is.finite(cos_mat))) {
    reload_abort("Reloaded cosine matrix contains non-finite values.",
                 "cosine_finite")
  }
  rn <- rownames(cos_mat)
  cn <- colnames(cos_mat)
  if (is.null(rn) || is.null(cn) || anyNA(rn) || anyNA(cn) ||
      any(!nzchar(rn)) || any(!nzchar(cn)) || anyDuplicated(rn) || anyDuplicated(cn)) {
    reload_abort("Reloaded cosine matrix must have unique, non-empty row and column item IDs.",
                 "cosine_dimnames")
  }
  if (!identical(rn, cn)) {
    reload_abort("Reloaded cosine matrix row and column item IDs must match in the same order.",
                 "cosine_dimnames_match")
  }
  symmetry_tol <- sqrt(.Machine$double.eps)
  if (max(abs(cos_mat - t(cos_mat))) > symmetry_tol) {
    reload_abort(
      sprintf("Reloaded cosine matrix is not symmetric within tolerance %.3g.", symmetry_tol),
      "cosine_symmetric",
      tolerance = symmetry_tol
    )
  }

  df_ids <- pick_id(df, c("item", "ID", "item_id"), "df")
  item_ids <- pick_id(items_tbl, c("item_id", "ID", "item"), "items")
  if (!setequal(df_ids, rn)) {
    reload_abort("Reloaded df item IDs do not match cosine-matrix item IDs.",
                 "df_cosine_ids_match")
  }
  if (!setequal(item_ids, rn)) {
    reload_abort("Reloaded item-table IDs do not match cosine-matrix item IDs.",
                 "items_cosine_ids_match")
  }
  if (!"type" %in% names(df)) {
    reload_abort("Reloaded df file is missing the factor-assignment column 'type'.",
                 "factor_assignment_column")
  }
  factor_values <- as.character(df$type)
  if (anyNA(factor_values) || any(!nzchar(trimws(factor_values)))) {
    reload_abort("Reloaded df contains missing/empty factor assignments.",
                 "factor_assignments_complete")
  }
  counts <- table(factor_values)
  default_i_per_f <- .semantica_assert_positive_integer(
    default_i_per_f, "default_i_per_f", condition_class = "semantica_error_integrity"
  )
  if (is.null(i.per.f)) {
    # `i.per.f` is a selection target, not the number of available items.
    i_per_f <- setNames(pmin(default_i_per_f, as.integer(counts)), names(counts))
  } else {
    i_per_f <- .semantica_assert_positive_integer_vector(
      i.per.f, "i.per.f", condition_class = "semantica_error_integrity"
    )
    if (is.null(names(i_per_f)) || anyNA(names(i_per_f)) ||
        any(!nzchar(names(i_per_f))) || anyDuplicated(names(i_per_f))) {
      reload_abort("'i.per.f' must be a uniquely named vector of factor selection targets.",
                   "i_per_f_named")
    }
    unknown_factors <- setdiff(names(i_per_f), names(counts))
    if (length(unknown_factors) > 0L) {
      reload_abort(
        sprintf("'i.per.f' references unknown factor(s): %s.",
                paste(unknown_factors, collapse = ", ")),
        "i_per_f_factor_names",
        factors = unknown_factors
      )
    }
    infeasible <- names(i_per_f)[i_per_f > as.integer(counts[names(i_per_f)])]
    if (length(infeasible) > 0L) {
      reload_abort(
        sprintf("'i.per.f' exceeds available items for factor(s): %s.",
                paste(infeasible, collapse = ", ")),
        "i_per_f_feasible",
        factors = infeasible
      )
    }
  }
  list(cosine_sim_matrix = cos_mat, df = df,
       items_tbl = tibble::as_tibble(items_tbl), i.per.f = i_per_f)
}

#' List all available backends
#'
#' Prints a task-oriented backend table showing local/cloud type, credential
#' environment variable, generation and embedding capability, and default
#' models. It also highlights generation-only backends that require a separate
#' embedding provider.
#'
#' @return Invisibly returns `SEMANTICA_BACKENDS`; the printed table is for user guidance.
#' @export
#' @examples
#' backends <- semantica_list_backends()
#' names(backends)
semantica_list_backends <- function() {
  rows <- do.call(rbind, lapply(names(SEMANTICA_BACKENDS), .semantica_backend_row))
  cat("\nSEMANTICA -- user-facing backend guide\n")
  cat("========================================\n")
  print(rows, row.names = FALSE, right = FALSE)
  cat("\nNotes:\n")
  cat("  - 'auth' is the environment variable SEMANTICA checks when credentials are required.\n")
  cat("  - Anthropic and Groq are generation-only in the built-in registry; pair them with an embedding backend.\n")
  cat("  - Local/server backends require the corresponding local service or Python environment to be available.\n")
  cat("  - Run semantica_check_setup(...) before an expensive run; set probe = TRUE to verify reachable model registries.\n")
  cat("  - For custom OpenAI-compatible servers use backend = 'generic_openai' plus base_url.\n\n")
  invisible(SEMANTICA_BACKENDS)
}
