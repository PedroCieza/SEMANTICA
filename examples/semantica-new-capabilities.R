# SEMANTICA: resource, device, quality, security, and telemetry examples
#
# Sections 1-5 are credential-free and can run offline with SEMANTICA and its
# ordinary R dependencies installed. Hardware-specific blocks run only when a
# compatible accelerator is reported. Network, environment-installation, and
# full cloud-pipeline examples are intentionally guarded with if (FALSE).

library(SEMANTICA)

# -----------------------------------------------------------------------------
# 1. Inspect capabilities without initializing Python or loading a model
# -----------------------------------------------------------------------------

capabilities <- semantica_capabilities(deep_python = FALSE)
print(capabilities)

capabilities$cpu
capabilities$r_torch
capabilities$python
capabilities$policy

# compute_device = "auto" deliberately remains CPU until benchmark-based
# crossover rules have been validated.
stopifnot(identical(capabilities$policy$default_compute_device, "cpu"))
stopifnot(identical(capabilities$policy$auto_compute_device, "cpu"))

# This optional probe initializes Python but still does not load an embedding
# or language model.
if (FALSE) {
  deep_capabilities <- semantica_capabilities(deep_python = TRUE)
  print(deep_capabilities)
  deep_capabilities$python
}

# -----------------------------------------------------------------------------
# 2. Preview allocation-aware CPU plans without starting workers
# -----------------------------------------------------------------------------

default_plan <- semantica_resource_plan()
automatic_plan <- semantica_resource_plan(
  n.cores = "auto",
  use_parallel = TRUE,
  reserve.cores = 1L,
  max.cores = 4L
)
explicit_plan <- semantica_resource_plan(
  n.cores = 2L,
  use_parallel = TRUE,
  # reserve.cores applies only to automatic mode.
  reserve.cores = 1L,
  max.cores = 4L
)
serial_plan <- semantica_resource_plan(use_parallel = FALSE)

print(default_plan)
print(automatic_plan)
print(explicit_plan)
print(serial_plan)

automatic_plan[c(
  "requested_workers", "available_workers", "effective_workers",
  "reserve_cores_applied", "max_cores", "limited_by",
  "parallel_backend", "worker_blas_threads"
)]

# A persistent user ceiling can also be supplied through an R option.
old_options <- options(semantica.max.cores = 4L)
option_limited_plan <- semantica_resource_plan(n.cores = "auto")
options(old_options)
print(option_limited_plan)

# -----------------------------------------------------------------------------
# 3. Build a manual item pool and calculate cosine geometry on safe CPU defaults
# -----------------------------------------------------------------------------

items <- paste0("item_", seq_len(8L))
items_tbl <- data.frame(
  item_id = items,
  factor = rep(c("Clarity", "Flexibility"), each = 4L),
  item_text = c(
    "I organize my thoughts before explaining them.",
    "I express complex ideas in a clear sequence.",
    "I keep my reasoning focused under pressure.",
    "I notice when my thinking becomes scattered.",
    "I change strategies when evidence changes.",
    "I can shift perspective when an approach fails.",
    "I consider alternatives when I become stuck.",
    "I adjust plans when conditions change."
  ),
  stringsAsFactors = FALSE
)

embeddings <- rbind(
  c(1.00, 0.08, 0.04), c(0.98, 0.13, 0.05),
  c(0.96, 0.16, 0.07), c(0.94, 0.19, 0.09),
  c(0.08, 1.00, 0.06), c(0.12, 0.98, 0.05),
  c(0.15, 0.96, 0.08), c(0.18, 0.94, 0.10)
)
rownames(embeddings) <- items

embed_result <- list(
  embeddings = embeddings,
  items_tbl = items_tbl,
  embed_model = "manual-offline-example",
  embedding_diagnostics = list(normalized = FALSE)
)

wrapped_cpu <- semantica_wrap(
  embed_result = embed_result,
  items_tbl = items_tbl,
  id_col = "item_id",
  factor_col = "factor",
  min_items_per_factor = 3L,
  cosine_adjustment = "none",
  compute_cosine_sensitivity = TRUE,
  compute_device = "cpu",
  gpu_fallback = "error",
  gpu_precision = "double",
  compute_memory_limit = NULL,
  verbose = FALSE
)

wrapped_cpu$cosine_sim_matrix
wrapped_cpu$compute_telemetry
wrapped_cpu$cosine_diagnostics
wrapped_cpu$cosine_adjustment_sensitivity
stopifnot(identical(wrapped_cpu$compute_telemetry$resolved_device, "cpu"))

# Automatic compute selection currently resolves to CPU. This second call also
# demonstrates the conservative working-memory ceiling on a small matrix.
wrapped_auto <- semantica_wrap(
  embed_result = embed_result,
  items_tbl = items_tbl,
  compute_cosine_sensitivity = FALSE,
  compute_device = "auto",
  gpu_fallback = "error",
  gpu_precision = "double",
  compute_memory_limit = 1024^2,
  verbose = FALSE
)
wrapped_auto$compute_telemetry[c(
  "requested_device", "resolved_device", "fallback_reason",
  "memory_estimate", "memory_limit"
)]
stopifnot(identical(wrapped_auto$compute_telemetry$resolved_device, "cpu"))

# An explicit GPU request defaults to an error when unavailable. CPU fallback
# happens only when the user explicitly permits it.
if (!isTRUE(capabilities$r_torch$cuda_available)) {
  wrapped_cuda_fallback <- semantica_wrap(
    embed_result = embed_result,
    compute_device = "cuda",
    gpu_fallback = "cpu",
    gpu_precision = "double",
    verbose = FALSE
  )
  wrapped_cuda_fallback$compute_telemetry[c(
    "requested_device", "resolved_device", "used_fallback",
    "fallback_reason", "fallback_policy"
  )]
}

# Capability-guarded CUDA double-precision cosine calculation.
if (isTRUE(capabilities$r_torch$cuda_available)) {
  wrapped_cuda <- semantica_wrap(
    embed_result = embed_result,
    compute_device = "cuda",
    gpu_fallback = "error",
    gpu_precision = "double",
    verbose = FALSE
  )
  print(wrapped_cuda$compute_telemetry)
}

# R torch MPS does not use the default double-precision policy here; single
# precision must be explicitly requested and may differ within tested tolerance.
if (isTRUE(capabilities$r_torch$mps_available)) {
  wrapped_mps <- semantica_wrap(
    embed_result = embed_result,
    compute_device = "mps",
    gpu_fallback = "error",
    gpu_precision = "single",
    verbose = FALSE
  )
  print(wrapped_mps$compute_telemetry)
}

# This block demonstrates strict explicit-device behavior without triggering it.
if (FALSE) {
  semantica_wrap(
    embed_result,
    compute_device = "cuda",
    gpu_fallback = NULL,
    gpu_precision = "double"
  )
}

# -----------------------------------------------------------------------------
# 4. Run a small deterministic offline optimization and inspect new telemetry
# -----------------------------------------------------------------------------

result <- ACO_with_ESEM(
  cosine_sim_matrix = wrapped_cpu$cosine_sim_matrix,
  df = wrapped_cpu$df,
  i.per.f = c(Clarity = 3L, Flexibility = 3L),
  ants = 4L,
  max.iter = 2L,
  max_total_iter = 2L,
  run_esem_during_search = FALSE,
  dfi_mode = "heuristic_semantic",
  pfa_mode = "off",
  run_pfa_during_search = FALSE,
  semantic_n_sensitivity = FALSE,
  validation_n_diagnostic = FALSE,
  final_dddfi = FALSE,
  final_equivtest = FALSE,
  history_mode = "summary",
  use_parallel = FALSE,
  n.cores = "auto",
  reserve.cores = 1L,
  max.cores = 4L,
  seed = 20260820L,
  verbose = FALSE
)

result$best_items
result$resource_plan
result$performance$resource
result$performance$compute
result$performance$timing
result$performance$evaluations
result$evaluation_telemetry
result$reproducibility

# Evaluation accounting distinguishes requests, cache/coalescing behavior,
# unique candidates, admitted search jobs, solver attempts, admissible/failed
# outcomes, and separately observed DFI/archive/final fits.
evaluation_fields <- c(
  "esem_requests", "esem_cache_hits", "esem_coalesced_requests",
  "esem_unique_candidates", "esem_fits_started", "esem_fits_converged",
  "esem_fits_admissible", "esem_fits_failed",
  "esem_solver_attempts_observed", "dfi_fits_started",
  "archive_esem_fits_started", "final_esem_fits_started",
  "max_esem_fits_definition"
)
# A small structured correlation matrix makes the search-time ESEM, strict
# admissibility gate, deterministic factor/sign alignment, unique-job budget,
# and (when available) two-worker PSOCK path runnable without network access.
esem_items <- paste0("esem_item_", seq_len(10L))
esem_factor <- rep(c("F1", "F2"), each = 5L)
esem_lambda <- matrix(0.08, nrow = 10L, ncol = 2L)
esem_lambda[cbind(seq_len(10L), rep(seq_len(2L), each = 5L))] <- 0.68
esem_phi <- matrix(c(1, 0.25, 0.25, 1), nrow = 2L)
esem_similarity <- esem_lambda %*% esem_phi %*% t(esem_lambda)
diag(esem_similarity) <- 1
dimnames(esem_similarity) <- list(esem_items, esem_items)
example_workers <- if (capabilities$cpu$available_cores >= 2L) 2L else 1L

esem_guided <- ACO_with_ESEM(
  cosine_sim_matrix = esem_similarity,
  df = data.frame(item = esem_items, factor = esem_factor),
  i.per.f = c(F1 = 4L, F2 = 4L),
  ants = 4L,
  max.iter = 1L,
  max_total_iter = 1L,
  esem_every = 1L,
  run_esem_during_search = TRUE,
  max_esem_fits = 2L,
  esem_failure_policy = "stop",
  dfi_mode = "heuristic_semantic",
  pfa_mode = "off",
  run_pfa_during_search = FALSE,
  semantic_n_sensitivity = FALSE,
  validation_n_diagnostic = FALSE,
  final_dddfi = FALSE,
  final_equivtest = FALSE,
  use_parallel = example_workers > 1L,
  n.cores = example_workers,
  seed = 20260820L,
  verbose = FALSE
)

esem_guided$evaluation_telemetry[
  intersect(evaluation_fields, names(esem_guided$evaluation_telemetry))
]
stopifnot(esem_guided$evaluation_telemetry$esem_fits_started == 2L)

# ESEM axes are deterministically aligned and sign-anchored before intended-
# factor diagnostics. Only mathematically admissible fits may guide ESEM scores.
esem_guided$esem_admissible
esem_guided$esem_admissibility$reasons
esem_guided$esem_admissibility$warnings
esem_guided$esem_admissibility$details
esem_guided$esem_alignment$assignment_method
esem_guided$esem_alignment$globally_optimal_assignment
esem_guided$esem_alignment$score_matrix
stopifnot(
  isTRUE(esem_guided$esem_admissible),
  isTRUE(esem_guided$esem_alignment$globally_optimal_assignment)
)

# Preferred terminology. The compatibility alias remains available, but this
# diagnostic partitions semantic item-pair geometry; it is not respondent-data
# split-half reliability.
result$semantic_pair_perturbation_stability
result$split_half_stability

# PFA can guide only the configured search checkpoints. At other iterations,
# proposal scoring remains semantic; final PFA diagnostics are still returned.
pfa_guided <- ACO_with_ESEM(
  cosine_sim_matrix = wrapped_cpu$cosine_sim_matrix,
  df = wrapped_cpu$df,
  i.per.f = c(Clarity = 3L, Flexibility = 3L),
  ants = 3L,
  max.iter = 2L,
  max_total_iter = 2L,
  run_esem_during_search = FALSE,
  dfi_mode = "heuristic_semantic",
  pfa_mode = "objective",
  pfa_weight = 0.20,
  run_pfa_during_search = TRUE,
  pfa_every = 2L,
  semantic_n_sensitivity = FALSE,
  validation_n_diagnostic = FALSE,
  final_dddfi = FALSE,
  final_equivtest = FALSE,
  use_parallel = FALSE,
  seed = 20260820L,
  verbose = FALSE
)
pfa_guided[c(
  "search_guidance_status", "pfa_every", "pfa_search_iterations",
  "pfa_search_attempts", "pfa_search_successes", "pfa_diagnostics"
)]

# Multi-seed runs now pass each requested seed explicitly and return the seed
# ledger alongside selection agreement. Keep this example deliberately small.
multi_seed <- run_multi_seed_semantica(
  seeds = c(104729L, 130363L),
  cosine_sim_matrix = wrapped_cpu$cosine_sim_matrix,
  df = wrapped_cpu$df,
  i.per.f = c(Clarity = 3L, Flexibility = 3L),
  verbose_seeds = FALSE,
  ants = 2L,
  max.iter = 1L,
  max_total_iter = 1L,
  run_esem_during_search = FALSE,
  dfi_mode = "heuristic_semantic",
  pfa_mode = "off",
  run_pfa_during_search = FALSE,
  semantic_n_sensitivity = FALSE,
  validation_n_diagnostic = FALSE,
  final_dddfi = FALSE,
  final_equivtest = FALSE,
  use_parallel = FALSE,
  verbose = FALSE
)
multi_seed$requested_seeds
multi_seed$successful_seeds
multi_seed$selection_matrix
multi_seed$pairwise_jaccard
multi_seed$reproducibility

# This is how the same optimizer would request allocation-aware parallelism.
# It is guarded to keep the main example lightweight and serial everywhere.
if (FALSE) {
  parallel_result <- ACO_with_ESEM(
    cosine_sim_matrix = wrapped_cpu$cosine_sim_matrix,
    df = wrapped_cpu$df,
    i.per.f = c(Clarity = 3L, Flexibility = 3L),
    ants = 8L,
    max.iter = 4L,
    run_esem_during_search = TRUE,
    max_esem_fits = 8L,
    use_parallel = TRUE,
    n.cores = "auto",
    reserve.cores = 1L,
    max.cores = 4L,
    seed = 20260820L
  )
}

# -----------------------------------------------------------------------------
# 5. Returned pipeline sessions are sanitized before serialization
# -----------------------------------------------------------------------------

# Live sessions contain credentials needed for requests. Pipeline result objects
# replace them with allow-listed metadata and sanitize sensitive URL components.
# This cloud example uses only an environment-variable lookup and is not run.
if (FALSE) {
  prepared <- semantica_pipeline(
    backend = "openai",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive cognitive self-regulation.",
    factors = list(
      Clarity = list(description = "Clear thinking.", n_items = 8L),
      Flexibility = list(description = "Adaptive thinking.", n_items = 8L)
    ),
    compute_device = "cpu",
    gpu_precision = "double",
    verbose = FALSE
  )

  prepared$session
  prepared$embed_session
  prepared$performance
  stopifnot(!any(c(
    "api_key", "embed_api_key", "hf_token", "authorization"
  ) %in% names(prepared$session)))
  saveRDS(prepared, file.path(tempdir(), "semantica-prepared.rds"))
}

# -----------------------------------------------------------------------------
# 6. Explicit local-model devices and conservative Conda setup profiles
# -----------------------------------------------------------------------------

# These operations install packages, initialize Python, or load local models,
# so they require deliberate user action and are never run by this script.
if (FALSE) {
  # Compatibility setup. accelerator = "auto" currently resolves to CPU too.
  setup <- semantica_setup_conda(
    env_name = "semantica-cpu",
    python_ver = "3.11",
    accelerator = "cpu",
    install_llamacpp = TRUE,
    verify = TRUE,
    force = FALSE
  )
  attributes(setup)

  # CUDA requires a wheel index chosen for the user's platform/runtime.
  # Replace the placeholder only after consulting the official PyTorch matrix.
  semantica_setup_conda(
    env_name = "semantica-cuda",
    accelerator = "cuda",
    torch_index_url = "https://download.pytorch.org/whl/cuXXX",
    install_llamacpp = FALSE,
    verify = TRUE
  )

  semantica_activate_conda("semantica-cpu")

  # Sentence Transformers/Hugging Face with explicit devices. Do not combine
  # an explicit chat_device with an explicit device_map.
  hf_session <- semantica_connect(
    backend = "python_hf",
    hf_token = Sys.getenv("HF_CHAT_TOKEN"),
    chat_model = "local-chat-model",
    embed_model = "sentence-transformers/all-MiniLM-L6-v2",
    embedding_device = "cuda:0",
    chat_device = "cuda:0",
    device_map = NULL,
    model_precision = "float16"
  )

  # A distinct embedding token forces a separate embedding session. Batching
  # and retained-memory controls are explicit at the pipeline boundary.
  local_prepared <- semantica_pipeline(
    backend = "python_hf",
    hf_token = Sys.getenv("HF_CHAT_TOKEN"),
    embed_hf_token = Sys.getenv("HF_EMBED_TOKEN"),
    chat_model = "local-chat-model",
    embed_model = "sentence-transformers/all-MiniLM-L6-v2",
    embed_batch_size = 32L,
    embedding_device = "cuda:0",
    chat_device = "cuda:0",
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive cognitive self-regulation.",
    factors = list(
      Clarity = list(description = "Clear thinking.", n_items = 8L),
      Flexibility = list(description = "Adaptive thinking.", n_items = 8L)
    ),
    compute_device = "cpu",
    release_local_models = FALSE,
    retain_embeddings = FALSE,
    verbose = TRUE
  )

  # llama.cpp: "auto" resolves to -1L (request all layers). The printed status
  # remains configured-not-runtime-verified until the backend reports otherwise.
  llama_session <- semantica_connect(
    backend = "python_llamacpp",
    gguf_path = "path/to/model.gguf",
    embedding_device = "auto",
    chat_device = "auto",
    gpu_layers = "auto",
    model_precision = "auto"
  )
}

# -----------------------------------------------------------------------------
# 7. Expert one-call cloud workflow with every control (intentionally not run)
# -----------------------------------------------------------------------------

if (FALSE) {
  full <- semantica_full_pipeline_custom(
    backend = "openai",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    chat_model = NULL,
    embed_model = "text-embedding-3-small",
    embed_batch_size = 32L,
    embedding_device = "auto",
    chat_device = "auto",
    device_map = NULL,
    gpu_layers = "auto",
    model_precision = "auto",
    compute_device = "cpu",
    gpu_fallback = "error",
    gpu_precision = "double",
    compute_memory_limit = NULL,
    compute_cosine_sensitivity = TRUE,
    release_local_models = FALSE,
    retain_embeddings = FALSE,
    scale_name = "Cognitive Agility",
    scale_description = "Clear and adaptive cognitive self-regulation.",
    factors = list(
      Clarity = list(description = "Clear thinking.", n_items = 12L),
      Flexibility = list(description = "Adaptive thinking.", n_items = 12L)
    ),
    n_per_factor = 12L,
    n_per_factor_override = FALSE,
    i.per.f = c(Clarity = 4L, Flexibility = 4L),
    use_parallel = TRUE,
    n.cores = "auto",
    reserve.cores = 1L,
    max.cores = 4L,
    seed = 20260820L,
    generate_plots = FALSE,
    verbose = TRUE
  )

  full$resource_plan
  full$performance
  full$evaluation_telemetry
  full$reproducibility
  full$semantic_pair_perturbation_stability
  full$optimization$esem_alignment
  full$optimization$esem_admissibility
  full$generation$session
}

# Deliberate current boundaries:
# - automatic GPU crossover selection is not enabled;
# - lavaan and DFI fits remain CPU work;
# - GPU DFI simulation and batched semantic candidate evaluation are deferred;
# - evaluation telemetry is incremental accounting behind the existing ACO API,
#   not a new exported general-purpose evaluation broker.

# -----------------------------------------------------------------------------
# 8. Optional hardware benchmark (not an exported/stable API)
# -----------------------------------------------------------------------------

if (FALSE) {
  benchmark_file <- system.file(
    "benchmarks", "semantica-benchmark.R", package = "SEMANTICA"
  )
  source(benchmark_file)

  # Cosine timing includes warm-up separation and host/device transfer.
  bench <- semantica_benchmark(
    cosine_sizes = c(250L, 1000L),
    cosine_devices = c("cpu", "cuda"),
    repeats = 3L,
    gpu_fallback = "error"
  )
  bench$cosine

  # To compare ESEM worker counts, supply one representative callback that
  # performs exactly the same fixed workload for every n.cores value.
  # Do not change repetitions or statistical settings between worker counts.
  esem_bench <- semantica_benchmark(
    cosine_sizes = 250L,
    repeats = 2L,
    esem_workers = c(1L, 2L, 4L, 8L),
    esem_batch = function(n.cores, seed) {
      ACO_with_ESEM(
        cosine_sim_matrix = wrapped_cpu$cosine_sim_matrix,
        df = wrapped_cpu$df,
        i.per.f = c(Clarity = 3L, Flexibility = 3L),
        ants = 8L,
        max.iter = 2L,
        run_esem_during_search = TRUE,
        max_esem_fits = 4L,
        use_parallel = n.cores > 1L,
        n.cores = n.cores,
        seed = seed,
        verbose = FALSE
      )
    }
  )
  esem_bench$esem
}

# -----------------------------------------------------------------------------
# Method-safety and research-track examples
# -----------------------------------------------------------------------------

# Blueprint-first content representation.
factors_research <- list(
  Clarity = list(
    description = "Clear and organized thinking.",
    facets = list(focus = list(), organization = list()),
    forbidden = c("memory capacity")
  ),
  Flexibility = list(
    description = "Adapting thinking when demands change.",
    facets = list(adaptation = list(), perspective_shift = list())
  )
)
blueprint_research <- semantica_construct_blueprint(factors_research)

# Experimental pool-relative thresholds must be requested explicitly.
# quality_research <- semantica_quality_config(
#   threshold_mode = "adaptive_pool",
#   construct_blueprint = blueprint_research
# )

# Full, sanitized, integrity-checked result serialization.
# semantica_save_bundle(result, "semantica-study.rds")
# result2 <- semantica_load_bundle("semantica-study.rds")

# Cross-representation robustness should be inspected before treating one
# embedding model's conclusion as model-independent.
# robustness <- semantica_semantic_robustness(
#   list(model_a = cosine_a, model_b = cosine_b),
#   selected_sets = list(model_a = selected_a, model_b = selected_b)
# )

# Empirical calibration requires participant-derived item correlations and
# should be evaluated on independent/held-out instruments.
# cal <- semantica_fit_empirical_calibration(
#   semantic_matrix = semantic_matrix,
#   response_matrix = empirical_item_cor,
#   method = "fisher_linear"
# )
