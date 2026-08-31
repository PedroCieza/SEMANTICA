# Compute-device helpers for SEMANTICA.

.semantica_compute_or <- function(x, default) {
  if (!is.null(x) && length(x) > 0L) x else default
}

.semantica_normalize_device <- function(device = "cpu", arg = "compute_device") {
  if (length(device) != 1L || is.na(device)) {
    stop(sprintf("'%s' must be one device string.", arg), call. = FALSE)
  }
  device <- tolower(trimws(as.character(device)))
  if (!nzchar(device) ||
      !grepl("^(auto|cpu|cuda|cuda:[0-9]+|mps)$", device)) {
    stop(
      sprintf(
        "'%s' must be one of 'cpu', 'auto', 'cuda', 'cuda:<index>', or 'mps'.",
        arg
      ),
      call. = FALSE
    )
  }
  device
}

.semantica_normalize_precision <- function(precision = "double") {
  if (length(precision) != 1L || is.na(precision)) {
    stop("'gpu_precision' must be one precision string.", call. = FALSE)
  }
  precision <- tolower(trimws(as.character(precision)))
  precision <- switch(
    precision,
    float64 = "double",
    double = "double",
    float32 = "single",
    single = "single",
    precision
  )
  if (!precision %in% c("double", "single")) {
    stop(
      "'gpu_precision' must be 'double'/'float64' or 'single'/'float32'.",
      call. = FALSE
    )
  }
  precision
}

.semantica_normalize_gpu_fallback <- function(gpu_fallback = "error") {
  if (length(gpu_fallback) != 1L || is.na(gpu_fallback)) {
    stop("'gpu_fallback' must be one policy string.", call. = FALSE)
  }
  gpu_fallback <- tolower(trimws(as.character(gpu_fallback)))
  if (!gpu_fallback %in% c("error", "cpu")) {
    stop("'gpu_fallback' must be either 'error' or 'cpu'.", call. = FALSE)
  }
  gpu_fallback
}

.semantica_torch_available <- function() {
  package_ok <- isTRUE(tryCatch(
    suppressWarnings(suppressMessages(requireNamespace("torch", quietly = TRUE))),
    error = function(e) FALSE
  ))
  if (!package_ok) return(FALSE)
  checker <- tryCatch(
    getExportedValue("torch", "torch_is_installed"),
    error = function(e) NULL
  )
  if (is.null(checker)) return(TRUE)
  isTRUE(tryCatch(
    suppressWarnings(suppressMessages(checker())),
    error = function(e) FALSE
  ))
}

.semantica_torch_capabilities_uncached <- function() {
  package_available <- isTRUE(tryCatch(
    suppressWarnings(suppressMessages(requireNamespace("torch", quietly = TRUE))),
    error = function(e) FALSE
  ))
  runtime_available <- if (package_available) {
    .semantica_torch_available()
  } else {
    FALSE
  }

  out <- list(
    package_installed = package_available,
    runtime_available = runtime_available,
    version = if (package_available) {
      as.character(utils::packageVersion("torch"))
    } else {
      NA_character_
    },
    cuda_available = FALSE,
    cuda_device_count = 0L,
    cuda_runtime = NA_character_,
    mps_available = FALSE
  )
  if (!runtime_available) return(out)

  out$cuda_available <- isTRUE(tryCatch(
    torch::cuda_is_available(),
    error = function(e) FALSE
  ))
  if (out$cuda_available) {
    out$cuda_device_count <- as.integer(tryCatch(
      torch::cuda_device_count(),
      error = function(e) 0L
    ))
    out$cuda_runtime <- as.character(tryCatch(
      torch::cuda_runtime_version(),
      error = function(e) NA_character_
    ))
  }
  mps_probe <- tryCatch(
    getExportedValue("torch", "backends_mps_is_available"),
    error = function(e) NULL
  )
  if (!is.null(mps_probe)) {
    out$mps_available <- isTRUE(tryCatch(
      mps_probe(),
      error = function(e) FALSE
    ))
  }
  out
}


.semantica_compute_capability_cache <- new.env(parent = emptyenv())

.semantica_torch_capabilities <- function(refresh = FALSE) {
  key <- "r_torch"
  if (!isTRUE(refresh) && exists(key, envir = .semantica_compute_capability_cache, inherits = FALSE)) {
    return(get(key, envir = .semantica_compute_capability_cache, inherits = FALSE))
  }
  out <- tryCatch(
    .semantica_torch_capabilities_uncached(),
    error = function(e) list(
      package_installed = requireNamespace("torch", quietly = TRUE),
      runtime_available = FALSE,
      version = NA_character_,
      cuda_available = FALSE,
      cuda_device_count = 0L,
      cuda_runtime = NA_character_,
      mps_available = FALSE,
      detection_error = conditionMessage(e)
    )
  )
  assign(key, out, envir = .semantica_compute_capability_cache)
  out
}

.semantica_python_capabilities <- function(deep_python = FALSE) {
  out <- list(
    checked = isTRUE(deep_python),
    reticulate_installed = requireNamespace("reticulate", quietly = TRUE),
    available = NA,
    executable = NA_character_,
    version = NA_character_,
    torch_version = NA_character_,
    package_versions = character(0L),
    cuda_available = NA,
    mps_available = NA
  )
  if (!isTRUE(deep_python) || !out$reticulate_installed) return(out)

  out$available <- isTRUE(tryCatch(
    reticulate::py_available(initialize = TRUE),
    error = function(e) FALSE
  ))
  if (!out$available) return(out)

  config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(config)) {
    out$executable <- as.character(
      .semantica_compute_or(config$python, NA_character_)
    )
    out$version <- as.character(
      .semantica_compute_or(config$version_string, config$version)
    )
  }

  py_torch <- tryCatch(
    reticulate::import("torch", delay_load = FALSE),
    error = function(e) NULL
  )
  if (!is.null(py_torch)) {
    out$torch_version <- tryCatch(
      as.character(py_torch$`__version__`),
      error = function(e) NA_character_
    )
    out$cuda_available <- isTRUE(tryCatch(
      py_torch$cuda$is_available(),
      error = function(e) FALSE
    ))
    out$mps_available <- isTRUE(tryCatch(
      py_torch$backends$mps$is_available(),
      error = function(e) FALSE
    ))
  }
  module_names <- c(
    "transformers", "sentence_transformers", "accelerate",
    "llama_cpp", "numpy", "scipy"
  )
  module_versions <- vapply(module_names, function(module_name) {
    module <- tryCatch(
      reticulate::import(module_name, delay_load = FALSE),
      error = function(e) NULL
    )
    if (is.null(module)) return(NA_character_)
    tryCatch(
      as.character(module$`__version__`),
      error = function(e) NA_character_
    )
  }, character(1L))
  out$package_versions <- module_versions[!is.na(module_versions)]
  out
}

#' Inspect SEMANTICA compute capabilities
#'
#' Reports CPU and optional R/Python accelerator availability without loading a
#' local language or embedding model. Python is not initialized unless
#' `deep_python = TRUE` is explicitly requested.
#'
#' @param deep_python Logical; initialize Python and inspect Python PyTorch.
#' @param refresh Logical; refresh the session-cached optional R Torch capability probe.
#' @return An object of class `semantica_capabilities`.
#' @export
semantica_capabilities <- function(deep_python = FALSE, refresh = FALSE) {
  if (length(deep_python) != 1L || is.na(deep_python)) {
    stop("'deep_python' must be TRUE or FALSE.", call. = FALSE)
  }
  deep_python <- isTRUE(deep_python)
  if (length(refresh) != 1L || is.na(refresh)) stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  refresh <- isTRUE(refresh)

  parallelly_installed <- requireNamespace("parallelly", quietly = TRUE)
  available_cores <- if (parallelly_installed) {
    tryCatch(
      as.integer(parallelly::availableCores(omit = 0L)),
      error = function(e) NA_integer_
    )
  } else {
    NA_integer_
  }
  if (!is.finite(available_cores) || available_cores < 1L) {
    available_cores <- 1L
  }
  resource_caps <- tryCatch(
    .semantica_resource_capabilities(),
    error = function(e) list(
      parallel_backend = "PSOCK",
      worker_blas_threads = 1L,
      blas = NA_character_
    )
  )
  torch_caps <- .semantica_torch_capabilities(refresh = refresh)

  out <- list(
    cpu = list(
      available = TRUE,
      available_cores = available_cores,
      default_auto_workers = resource_caps$default_auto_workers %||%
        .semantica_auto_worker_capacity(available_cores, reserve.cores = 1L, coordinator.cores = 1L),
      coordinator_cores = resource_caps$coordinator_cores %||% NA_integer_,
      physical_worker_cap = resource_caps$physical_worker_cap %||% NA_integer_,
      memory_aware_auto = resource_caps$memory_aware_auto %||% FALSE,
      memory_worker_cap = resource_caps$memory_worker_cap %||% NA_integer_,
      memory_source = resource_caps$memory_source %||% "unavailable",
      parallel_backend = resource_caps$parallel_backend %||% "PSOCK",
      worker_blas_threads = resource_caps$worker_blas_threads %||% 1L,
      blas = resource_caps$blas %||% NA_character_,
      source = if (parallelly_installed) {
        "parallelly::availableCores"
      } else {
        "parallelly unavailable; conservative fallback"
      }
    ),
    r_torch = torch_caps,
    python = .semantica_python_capabilities(deep_python),
    policy = list(
      default_compute_device = "cpu",
      auto_compute_device = "cpu",
      auto_reason = paste(
        "Automatic GPU selection remains disabled until benchmark-based",
        "crossover rules are validated."
      ),
      default_gpu_precision = "double",
      recommended_n_cores = "auto",
      recommended_compute_device = "cpu"
    )
  )
  class(out) <- c("semantica_capabilities", "list")
  out
}

#' Print SEMANTICA compute capabilities
#'
#' @param x A `semantica_capabilities` object.
#' @param ... Additional arguments ignored.
#' @return Invisibly returns `x`.
#' @method print semantica_capabilities
#' @export
print.semantica_capabilities <- function(x, ...) {
  yes_no <- function(value) {
    if (is.na(value)) "not checked" else if (isTRUE(value)) "yes" else "no"
  }
  cat("SEMANTICA compute capabilities\n\n")
  cat(sprintf("CPU cores available to R : %d\n", x$cpu$available_cores))
  cat(sprintf("Default auto workers     : %d\n", x$cpu$default_auto_workers))
  if (is.finite(x$cpu$physical_worker_cap %||% NA_real_)) {
    cat(sprintf("Physical-core worker cap : %d\n", x$cpu$physical_worker_cap))
  }
  if (isTRUE(x$cpu$memory_aware_auto)) {
    cap <- x$cpu$memory_worker_cap %||% NA_integer_
    if (is.finite(cap)) {
      cat(sprintf("Memory-aware worker cap  : %d (%s)\n", cap, x$cpu$memory_source %||% "measured"))
    }
  }
  cat(sprintf("Parallel backend         : %s\n", x$cpu$parallel_backend))
  cat(sprintf("Worker BLAS threads      : %d\n", x$cpu$worker_blas_threads))
  cat(sprintf("BLAS                     : %s\n", x$cpu$blas %||% "not reported"))
  cat(sprintf("R torch package          : %s\n", yes_no(x$r_torch$package_installed)))
  cat(sprintf("R torch runtime          : %s\n", yes_no(x$r_torch$runtime_available)))
  cat(sprintf("CUDA available           : %s\n", yes_no(x$r_torch$cuda_available)))
  cat(sprintf("CUDA devices             : %d\n", x$r_torch$cuda_device_count %||% 0L))
  cat(sprintf("MPS available            : %s\n", yes_no(x$r_torch$mps_available)))
  cat(sprintf("Python checked           : %s\n", yes_no(x$python$checked)))
  cat(sprintf("Default compute device   : %s\n", x$policy$default_compute_device))
  invisible(x)
}

.semantica_resolve_device <- function(
    compute_device = "cpu",
    gpu_fallback = "error",
    gpu_precision = "double",
    capabilities = NULL) {
  requested <- .semantica_normalize_device(compute_device)
  fallback <- .semantica_normalize_gpu_fallback(gpu_fallback)
  precision <- .semantica_normalize_precision(gpu_precision)

  if (requested == "cpu") {
    return(list(
      requested_device = requested,
      resolved_device = "cpu",
      requested_precision = precision,
      resolved_precision = "double",
      backend = "base_r",
      used_fallback = FALSE,
      fallback_reason = NULL,
      fallback_policy = fallback
    ))
  }

  if (requested == "auto") {
    return(list(
      requested_device = requested,
      resolved_device = "cpu",
      requested_precision = precision,
      resolved_precision = "double",
      backend = "base_r",
      used_fallback = FALSE,
      fallback_reason = paste(
        "Automatic GPU selection is disabled pending validated",
        "CPU/GPU crossover benchmarks."
      ),
      fallback_policy = fallback
    ))
  }

  capabilities <- .semantica_compute_or(
    capabilities,
    semantica_capabilities(deep_python = FALSE)
  )
  torch_caps <- .semantica_compute_or(capabilities$r_torch, list())
  torch_ready <- isTRUE(torch_caps$package_installed) &&
    isTRUE(torch_caps$runtime_available)

  unavailable_reason <- NULL
  if (!torch_ready) {
    unavailable_reason <- paste(
      "The optional R package 'torch' and its runtime are required for",
      sprintf("compute_device='%s'.", requested)
    )
  } else if (startsWith(requested, "cuda")) {
    if (!isTRUE(torch_caps$cuda_available)) {
      unavailable_reason <- paste(
        "CUDA was requested, but torch::cuda_is_available() is not TRUE."
      )
    } else if (grepl(":", requested, fixed = TRUE)) {
      index <- as.integer(sub("^cuda:", "", requested))
      count <- suppressWarnings(as.integer(torch_caps$cuda_device_count))
      if (!is.finite(count) || count < 1L || index >= count) {
        unavailable_reason <- sprintf(
          "CUDA device index %d was requested, but R torch reports %d device(s).",
          index,
          if (is.finite(count)) count else 0L
        )
      }
    }
  } else if (requested == "mps") {
    if (precision == "double") {
      unavailable_reason <- paste(
        "MPS does not support the float64 compatibility path.",
        "Request gpu_precision='single' explicitly or use CPU."
      )
    } else if (!isTRUE(torch_caps$mps_available)) {
      unavailable_reason <- paste(
        "MPS was requested, but torch::backends_mps_is_available() is not TRUE."
      )
    }
  }

  if (!is.null(unavailable_reason)) {
    if (fallback == "cpu") {
      return(list(
        requested_device = requested,
        resolved_device = "cpu",
        requested_precision = precision,
        resolved_precision = "double",
        backend = "base_r",
        used_fallback = TRUE,
        fallback_reason = unavailable_reason,
        fallback_policy = fallback
      ))
    }
    stop(
      paste0(
        unavailable_reason,
        "\nSEMANTICA will not silently fall back to CPU. ",
        "Use compute_device='cpu' or gpu_fallback='cpu'."
      ),
      call. = FALSE
    )
  }

  list(
    requested_device = requested,
    resolved_device = requested,
    requested_precision = precision,
    resolved_precision = precision,
    backend = "torch",
    used_fallback = FALSE,
    fallback_reason = NULL,
    fallback_policy = fallback
  )
}

.semantica_validate_embedding_matrix <- function(emb_matrix) {
  x <- as.matrix(emb_matrix)
  if (length(dim(x)) != 2L || nrow(x) < 1L || ncol(x) < 1L) {
    stop("'emb_matrix' must be a non-empty two-dimensional matrix.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("'emb_matrix' must contain numeric values.", call. = FALSE)
  }
  storage.mode(x) <- "double"
  if (any(!is.finite(x))) {
    stop("'emb_matrix' contains non-finite values.", call. = FALSE)
  }
  x
}

.semantica_validate_cosine_arguments <- function(
    already_normalized = TRUE,
    adjustment = c("none", "mean_center")) {
  if (length(already_normalized) != 1L || is.na(already_normalized)) {
    stop("'already_normalized' must be TRUE or FALSE.", call. = FALSE)
  }
  list(
    already_normalized = isTRUE(already_normalized),
    adjustment = match.arg(adjustment)
  )
}

.semantica_cosine_cpu <- function(
    emb_matrix,
    already_normalized = TRUE,
    adjustment = c("none", "mean_center")) {
  args <- .semantica_validate_cosine_arguments(
    already_normalized,
    adjustment
  )
  x <- .semantica_validate_embedding_matrix(emb_matrix)
  item_names <- rownames(x)

  if (args$adjustment == "mean_center") {
    x <- sweep(x, 2L, colMeans(x), FUN = "-")
    args$already_normalized <- FALSE
  }
  if (!args$already_normalized) {
    norms <- sqrt(rowSums(x^2))
    if (any(!is.finite(norms) | norms <= .Machine$double.eps)) {
      stop(
        "Cannot compute cosine matrix from zero or invalid embedding vectors.",
        call. = FALSE
      )
    }
    x <- x / norms
  }

  out <- tcrossprod(x)
  out <- (out + t(out)) / 2
  out[out > 1] <- 1
  out[out < -1] <- -1
  diag(out) <- 1
  dimnames(out) <- list(item_names, item_names)
  storage.mode(out) <- "double"
  out
}

.semantica_cosine_torch <- function(
    emb_matrix,
    already_normalized = TRUE,
    adjustment = c("none", "mean_center"),
    device = "cuda",
    precision = "double") {
  if (!.semantica_torch_available()) {
    stop(
      "The optional R package 'torch' and its runtime are required.",
      call. = FALSE
    )
  }
  args <- .semantica_validate_cosine_arguments(
    already_normalized,
    adjustment
  )
  device <- .semantica_normalize_device(device, arg = "device")
  if (device == "auto") {
    stop("Low-level torch cosine requires a resolved device.", call. = FALSE)
  }
  precision <- .semantica_normalize_precision(precision)
  if (device == "mps" && precision == "double") {
    stop(
      paste(
        "MPS does not support float64 tensors.",
        "Use precision='single' explicitly or select CPU."
      ),
      call. = FALSE
    )
  }

  x <- .semantica_validate_embedding_matrix(emb_matrix)
  item_names <- rownames(x)
  dtype <- if (precision == "double") {
    torch::torch_float64()
  } else {
    torch::torch_float32()
  }
  torch_device <- torch::torch_device(device)
  x_t <- torch::torch_tensor(x, dtype = dtype, device = torch_device)

  if (args$adjustment == "mean_center") {
    x_t <- x_t - x_t$mean(dim = 1L, keepdim = TRUE)
    args$already_normalized <- FALSE
  }
  if (!args$already_normalized) {
    norms_t <- x_t$pow(2)$sum(dim = 2L, keepdim = TRUE)$sqrt()
    norms <- as.numeric(torch::as_array(norms_t$cpu()))
    if (any(!is.finite(norms) | norms <= .Machine$double.eps)) {
      stop(
        "Cannot compute cosine matrix from zero or invalid embedding vectors.",
        call. = FALSE
      )
    }
    x_t <- x_t / norms_t
  }

  product_t <- x_t$matmul(x_t$t())
  cosine_t <- ((product_t + product_t$t()) / 2)$clamp(min = -1, max = 1)
  out <- torch::as_array(cosine_t$cpu())
  if (is.null(dim(out))) out <- matrix(out, nrow = nrow(x), ncol = nrow(x))
  out <- as.matrix(out)
  storage.mode(out) <- "double"
  # Enforce exact conventional-matrix invariants after transfer to R.
  out <- (out + t(out)) / 2
  out[out > 1] <- 1
  out[out < -1] <- -1
  diag(out) <- 1
  dimnames(out) <- list(item_names, item_names)
  out
}

.semantica_estimate_cosine_memory <- function(
    n_items,
    embedding_dimensions,
    precision = "double",
    adjustment = c("none", "mean_center")) {
  precision <- .semantica_normalize_precision(precision)
  adjustment <- match.arg(adjustment)
  n_items <- suppressWarnings(as.numeric(n_items))
  embedding_dimensions <- suppressWarnings(as.numeric(embedding_dimensions))
  if (length(n_items) != 1L || !is.finite(n_items) || n_items < 1 ||
      n_items != floor(n_items)) {
    stop("'n_items' must be a positive integer.", call. = FALSE)
  }
  if (length(embedding_dimensions) != 1L ||
      !is.finite(embedding_dimensions) || embedding_dimensions < 1 ||
      embedding_dimensions != floor(embedding_dimensions)) {
    stop("'embedding_dimensions' must be a positive integer.", call. = FALSE)
  }

  element_bytes <- if (precision == "double") 8 else 4
  # Conservative accelerator peak: input plus normalization/centering work,
  # product plus symmetrization/clamp temporaries, and row-wise work vectors.
  embedding_copies <- if (adjustment == "mean_center") 3 else 2
  device_bytes <- element_bytes * (
    embedding_copies * n_items * embedding_dimensions +
      3 * n_items * n_items +
      4 * n_items
  )
  # The returned object is always an ordinary double R matrix.
  host_output_bytes <- 8 * n_items * n_items
  list(
    device_bytes = as.numeric(device_bytes),
    host_output_bytes = as.numeric(host_output_bytes),
    conservative_total_bytes = as.numeric(device_bytes + host_output_bytes),
    n_items = as.integer(n_items),
    embedding_dimensions = as.integer(embedding_dimensions),
    precision = precision,
    adjustment = adjustment
  )
}

.semantica_validate_memory_limit <- function(memory_limit = NULL) {
  if (is.null(memory_limit)) return(NULL)
  memory_limit <- suppressWarnings(as.numeric(memory_limit))
  if (length(memory_limit) != 1L || is.na(memory_limit) ||
      memory_limit <= 0) {
    stop("'memory_limit' must be NULL or a positive number of bytes.", call. = FALSE)
  }
  memory_limit
}

.semantica_release_torch_memory <- function(device = NULL) {
  invisible(gc(FALSE))
  if (!is.null(device) && startsWith(device, "cuda") &&
      .semantica_torch_available()) {
    empty_cache <- tryCatch(
      getExportedValue("torch", "cuda_empty_cache"),
      error = function(e) NULL
    )
    if (!is.null(empty_cache)) {
      try(empty_cache(), silent = TRUE)
    }
  }
  invisible(NULL)
}

.semantica_compute_cosine <- function(
    emb_matrix,
    already_normalized = TRUE,
    adjustment = c("none", "mean_center"),
    compute_device = "cpu",
    gpu_fallback = "error",
    gpu_precision = "double",
    memory_limit = NULL,
    capabilities = NULL) {
  started <- proc.time()[["elapsed"]]
  adjustment <- match.arg(adjustment)
  x <- .semantica_validate_embedding_matrix(emb_matrix)
  memory_limit <- .semantica_validate_memory_limit(memory_limit)
  plan <- .semantica_resolve_device(
    compute_device = compute_device,
    gpu_fallback = gpu_fallback,
    gpu_precision = gpu_precision,
    capabilities = capabilities
  )
  estimate <- .semantica_estimate_cosine_memory(
    n_items = nrow(x),
    embedding_dimensions = ncol(x),
    precision = plan$resolved_precision,
    adjustment = adjustment
  )
  cpu_estimate <- .semantica_estimate_cosine_memory(
    n_items = nrow(x),
    embedding_dimensions = ncol(x),
    precision = "double",
    adjustment = adjustment
  )

  if (!is.null(memory_limit)) {
    requested_bytes <- if (plan$resolved_device == "cpu") {
      estimate$conservative_total_bytes
    } else {
      estimate$device_bytes
    }
    if (requested_bytes > memory_limit) {
      reason <- sprintf(
        paste0(
          "Estimated cosine working memory (%.0f bytes) exceeds ",
          "memory_limit (%.0f bytes)."
        ),
        requested_bytes,
        memory_limit
      )
      if (plan$resolved_device != "cpu" && plan$fallback_policy == "cpu") {
        if (cpu_estimate$conservative_total_bytes > memory_limit) {
          stop(
            paste(
              reason,
              sprintf(
                paste0(
                  "CPU fallback requires an estimated %.0f bytes and also ",
                  "exceeds memory_limit."
                ),
                cpu_estimate$conservative_total_bytes
              )
            ),
            call. = FALSE
          )
        }
        plan$resolved_device <- "cpu"
        plan$resolved_precision <- "double"
        plan$backend <- "base_r"
        plan$used_fallback <- TRUE
        plan$fallback_reason <- reason
        estimate <- cpu_estimate
      } else {
        stop(reason, call. = FALSE)
      }
    }
  }

  torch_error <- NULL
  if (plan$resolved_device == "cpu") {
    matrix_out <- .semantica_cosine_cpu(
      x,
      already_normalized = already_normalized,
      adjustment = adjustment
    )
  } else {
    accelerated <- tryCatch(
      .semantica_cosine_torch(
        x,
        already_normalized = already_normalized,
        adjustment = adjustment,
        device = plan$resolved_device,
        precision = plan$resolved_precision
      ),
      error = function(e) e
    )
    if (inherits(accelerated, "condition")) {
      torch_error <- conditionMessage(accelerated)
      .semantica_release_torch_memory(plan$resolved_device)
      if (plan$fallback_policy != "cpu") {
        stop(
          sprintf(
            paste0(
              "Torch cosine failed on '%s': %s\n",
              "SEMANTICA will not silently fall back to CPU."
            ),
            plan$resolved_device,
            torch_error
          ),
          call. = FALSE
        )
      }
      if (!is.null(memory_limit) &&
          cpu_estimate$conservative_total_bytes > memory_limit) {
        stop(
          sprintf(
            paste0(
              "Torch cosine failed on '%s': %s CPU fallback requires an ",
              "estimated %.0f bytes and exceeds memory_limit (%.0f bytes)."
            ),
            plan$resolved_device,
            torch_error,
            cpu_estimate$conservative_total_bytes,
            memory_limit
          ),
          call. = FALSE
        )
      }
      plan$used_fallback <- TRUE
      plan$fallback_reason <- paste(
        sprintf("Torch cosine failed on '%s':", plan$resolved_device),
        torch_error
      )
      plan$resolved_device <- "cpu"
      plan$resolved_precision <- "double"
      plan$backend <- "base_r"
      estimate <- cpu_estimate
      matrix_out <- .semantica_cosine_cpu(
        x,
        already_normalized = already_normalized,
        adjustment = adjustment
      )
    } else {
      matrix_out <- accelerated
    }
  }

  elapsed <- proc.time()[["elapsed"]] - started
  list(
    matrix = matrix_out,
    telemetry = list(
      requested_device = plan$requested_device,
      resolved_device = plan$resolved_device,
      backend = plan$backend,
      requested_precision = plan$requested_precision,
      resolved_precision = plan$resolved_precision,
      used_fallback = isTRUE(plan$used_fallback),
      fallback_reason = plan$fallback_reason,
      fallback_policy = plan$fallback_policy,
      torch_error = torch_error,
      memory_estimate = estimate,
      memory_limit = memory_limit,
      adjustment = adjustment,
      elapsed_seconds = unname(elapsed)
    )
  )
}
