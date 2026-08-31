# Hardware-oriented SEMANTICA benchmark helper.
#
# This script is intentionally kept under inst/benchmarks rather than exported
# as a stable package API. Source it explicitly, then call
# semantica_benchmark(). Timings are descriptive and are never used as package
# test thresholds.

semantica_benchmark <- function(
    cosine_sizes = c(250L, 1000L),
    embedding_dimensions = 384L,
    cosine_devices = "cpu",
    repeats = 3L,
    seed = 20260820L,
    gpu_precision = "double",
    gpu_fallback = "error",
    esem_workers = c(1L, 2L, 4L, 8L),
    esem_batch = NULL) {
  cosine_sizes <- unique(as.integer(cosine_sizes))
  embedding_dimensions <- as.integer(embedding_dimensions[1L])
  repeats <- as.integer(repeats[1L])
  seed <- as.integer(seed[1L])
  esem_workers <- unique(as.integer(esem_workers))
  if (any(!is.finite(cosine_sizes) | cosine_sizes < 2L)) {
    stop("'cosine_sizes' must contain integers of at least two.")
  }
  if (!is.finite(embedding_dimensions) || embedding_dimensions < 1L) {
    stop("'embedding_dimensions' must be positive.")
  }
  if (!is.finite(repeats) || repeats < 1L) stop("'repeats' must be positive.")
  if (!is.null(esem_batch) && !is.function(esem_batch)) {
    stop("'esem_batch' must be NULL or function(n.cores, seed).")
  }

  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  cosine_rows <- list()
  row_index <- 0L
  for (n_items in cosine_sizes) {
    set.seed(seed + n_items)
    x <- matrix(
      stats::rnorm(n_items * embedding_dimensions),
      nrow = n_items,
      ncol = embedding_dimensions
    )
    rownames(x) <- paste0("item_", seq_len(n_items))
    for (device in cosine_devices) {
      # Warm-up is deliberately separate from recorded repetitions.
      warm <- tryCatch(
        SEMANTICA:::.semantica_compute_cosine(
          x,
          already_normalized = FALSE,
          compute_device = device,
          gpu_fallback = gpu_fallback,
          gpu_precision = gpu_precision
        ),
        error = identity
      )
      if (inherits(warm, "error")) {
        row_index <- row_index + 1L
        cosine_rows[[row_index]] <- data.frame(
          n_items = n_items,
          embedding_dimensions = embedding_dimensions,
          requested_device = device,
          resolved_device = NA_character_,
          precision = gpu_precision,
          median_seconds = NA_real_,
          minimum_seconds = NA_real_,
          status = conditionMessage(warm),
          stringsAsFactors = FALSE
        )
        next
      }
      elapsed <- numeric(repeats)
      resolved <- warm$telemetry$resolved_device
      for (iteration in seq_len(repeats)) {
        started <- proc.time()[["elapsed"]]
        SEMANTICA:::.semantica_compute_cosine(
          x,
          already_normalized = FALSE,
          compute_device = device,
          gpu_fallback = gpu_fallback,
          gpu_precision = gpu_precision
        )
        elapsed[[iteration]] <- proc.time()[["elapsed"]] - started
      }
      row_index <- row_index + 1L
      cosine_rows[[row_index]] <- data.frame(
        n_items = n_items,
        embedding_dimensions = embedding_dimensions,
        requested_device = device,
        resolved_device = resolved,
        precision = warm$telemetry$resolved_precision,
        median_seconds = stats::median(elapsed),
        minimum_seconds = min(elapsed),
        status = "ok",
        stringsAsFactors = FALSE
      )
    }
  }

  esem_rows <- list()
  if (!is.null(esem_batch)) {
    for (workers in esem_workers) {
      plan <- tryCatch(
        SEMANTICA::semantica_resource_plan(
          n.cores = workers,
          use_parallel = workers > 1L,
          reserve.cores = 0L
        ),
        error = identity
      )
      if (inherits(plan, "error")) {
        esem_rows[[length(esem_rows) + 1L]] <- data.frame(
          requested_workers = workers,
          effective_workers = NA_integer_,
          elapsed_seconds = NA_real_,
          status = conditionMessage(plan),
          stringsAsFactors = FALSE
        )
        next
      }
      started <- proc.time()[["elapsed"]]
      status <- tryCatch({
        esem_batch(n.cores = plan$effective_workers, seed = seed)
        "ok"
      }, error = conditionMessage)
      esem_rows[[length(esem_rows) + 1L]] <- data.frame(
        requested_workers = workers,
        effective_workers = plan$effective_workers,
        elapsed_seconds = proc.time()[["elapsed"]] - started,
        status = status,
        stringsAsFactors = FALSE
      )
    }
  }

  structure(
    list(
      capabilities = SEMANTICA::semantica_capabilities(deep_python = FALSE),
      cosine = do.call(rbind, cosine_rows),
      esem = if (length(esem_rows)) do.call(rbind, esem_rows) else data.frame(),
      settings = list(
        repeats = repeats,
        seed = seed,
        warm_up_excluded = TRUE,
        cosine_includes_host_device_transfer = TRUE
      ),
      note = paste(
        "Use the optional esem_batch callback for an identical, representative",
        "ESEM workload at each worker count; no absolute timing threshold is implied."
      )
    ),
    class = c("semantica_benchmark_result", "list")
  )
}
