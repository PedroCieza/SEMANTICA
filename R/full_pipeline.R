if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
}

.semantica_config_object <- function(values, class_name, schema_fields = names(values)) {
  schema_fields <- unique(as.character(schema_fields %||% character(0L)))
  schema_fields <- schema_fields[!is.na(schema_fields) & nzchar(schema_fields)]
  attr(values, "semantica_schema_fields") <- schema_fields
  class(values) <- c(class_name, "semantica_config", "list")
  values
}

.semantica_merge_config <- function(x, default, name) {
  if (is.null(x)) return(default)
  if (!is.list(x)) {
    .semantica_abort(
      sprintf("'%s' must be a SEMANTICA config object or a named list.", name),
      subclass = "semantica_error_config",
      argument = name
    )
  }
  if (length(x) > 0L) {
    x_names <- names(x)
    if (is.null(x_names) || anyNA(x_names) || any(!nzchar(x_names))) {
      .semantica_abort(
        sprintf("'%s' configuration overrides must be fully named.", name),
        subclass = "semantica_error_config",
        argument = name
      )
    }
    duplicated_names <- unique(x_names[duplicated(x_names)])
    if (length(duplicated_names) > 0L) {
      .semantica_abort(
        sprintf("'%s' contains duplicated configuration field(s): %s.",
                name, paste(duplicated_names, collapse = ", ")),
        subclass = "semantica_error_config",
        argument = name,
        duplicated_fields = duplicated_names
      )
    }
    allowed_fields <- attr(default, "semantica_schema_fields", exact = TRUE) %||% names(default)
    unknown <- setdiff(x_names, allowed_fields)
    if (length(unknown) > 0L) {
      .semantica_abort(
        sprintf(
          "Unknown '%s' configuration field(s): %s. SEMANTICA now rejects unknown analysis-configuration fields to prevent silent misspecification.",
          name, paste(unknown, collapse = ", ")
        ),
        subclass = "semantica_error_config",
        argument = name,
        unknown_fields = unknown,
        config_group = name
      )
    }
  }
  utils::modifyList(default, x, keep.null = TRUE)
}

.semantica_option_list <- function(options, arg_name, allowed = NULL,
                                   aliases = NULL, allow_unknown = FALSE) {
  if (is.null(options)) return(list())
  if (!is.list(options)) {
    stop(sprintf("`%s` must be a named list.", arg_name), call. = FALSE)
  }
  if (length(options) == 0L) return(list())

  option_names <- names(options)
  if (is.null(option_names) || any(!nzchar(option_names))) {
    stop(sprintf("All entries in `%s` must be named.", arg_name), call. = FALSE)
  }

  if (!is.null(aliases)) {
    for (alias in names(aliases)) {
      if (alias %in% names(options)) {
        target <- aliases[[alias]]
        if (!(target %in% names(options))) {
          options[target] <- options[alias]
        }
        options[alias] <- NULL
      }
    }
  }

  if (!allow_unknown) {
    unknown <- setdiff(names(options), allowed)
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "Unknown option(s) in `%s`: %s.",
          arg_name,
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  options
}

.semantica_merge_options <- function(...) {
  merged <- list()
  for (options in list(...)) {
    if (length(options) == 0L) next
    for (name in names(options)) {
      merged[name] <- options[name]
    }
  }
  merged
}

.semantica_full_pipeline_resolve_args <- function(
    scale_name, scale_description, factors, backend,
    candidate_items_per_factor, candidate_items_per_factor_supplied,
    items_per_factor, generation_options, optimization_options, dfi_options,
    pfa_options, validation_options, plot_options, verbose, legacy_args) {
  generation_options <- .semantica_option_list(
    generation_options,
    "generation_options",
    aliases = c(candidate_items_per_factor = "n_per_factor"),
    allow_unknown = TRUE
  )

  optimization_options <- .semantica_option_list(
    optimization_options,
    "optimization_options",
    allowed = c(
      "i.per.f", "ants", "max.iter", "search_patience", "evaporation",
      "esem_every", "run_esem_during_search", "max_total_iter",
      "max_esem_fits", "esem_weight", "esem_failure_policy",
      "esem_sample_size", "elite_k", "esem_eval_top_k", "fast_esem",
      "fast_esem_iter_max", "full_esem_iter_max", "rotation",
      "rotation_args", "data_type", "target_loadings", "target_factor_cors",
      "loading_pattern", "redundancy_threshold", "dup_threshold",
      "htmt_threshold", "htmt_objective_role", "cohesion_quantile",
      "cohesion_retention", "within_similarity_target",
      "within_similarity_band", "semantic_objective_mode",
      "semantic_threshold_mode", "adaptive_redundancy_quantile",
      "adaptive_duplicate_quantile", "construct_blueprint",
      "nomological_weight", "content_alignment_mode", "polarity_action",
      "within_target_method", "facet_coverage_weight",
      "psychometric_guard_weight", "psychometric_guard_min_ave",
      "psychometric_guard_min_loading",
      "psychometric_guard_min_primary_ge_50", "sigmoid_center",
      "sigmoid_steepness", "heuristic_beta", "elite_pareto_rerank",
      "elite_multicriteria_rerank", "archive_stable_window",
      "structural_archive_stable_window", "min_successful_pfa_checkpoints",
      "min_successful_esem_checkpoints", "pheromone_update",
      "fixed_evaporation", "debug_mode", "keep_solution_history",
      "history_mode", "use_parallel", "n.cores", "reserve.cores",
      "max.cores", "cfa_every", "cfa_weight", "cfa_sample_size"
    )
  )

  dfi_options <- .semantica_option_list(
    dfi_options,
    "dfi_options",
    allowed = c(
      "dfi_reps", "dfi_level", "dfi_criterion", "dfi_mode",
      "dfi_esem_reps", "dfi_search_reps", "final_dfi_recalibrate",
      "final_dfi_reps", "dfi_roc_misspec_strength", "dfi_esem_strategy",
      "dfi_adaptive_min_reps", "dfi_adaptive_batch_reps",
      "dfi_adaptive_tol", "dfi_adaptive_stable_batches",
      "dfi_fallback_policy", "final_dddfi", "final_dddfi_reps",
      "final_dddfi_mad_target", "final_equivtest", "embed_reliability",
      "residual_inflation", "dfi_warmup_iters", "reference_rmsea_close",
      "reference_rmsea_poor", "reference_power", "reference_alpha",
      "reference_max_n", "semantic_n_sensitivity", "semantic_n_grid",
      "semantic_n_multipliers", "semantic_n_iter_max",
      "semantic_esem_score_mode"
    )
  )

  pfa_options <- .semantica_option_list(
    pfa_options,
    "pfa_options",
    allowed = c(
      "pfa_mode", "pfa_weight", "pfa_failure_policy",
      "run_pfa_during_search", "pfa_every", "pfa_extraction",
      "pfa_final_extraction", "pfa_rotation", "pfa_min_loading",
      "pfa_min_margin", "pfa_unit_diagnostics"
    )
  )

  validation_options <- .semantica_option_list(
    validation_options,
    "validation_options",
    allowed = c(
      "validation_n_diagnostic", "validation_n_reps", "validation_n_grid",
      "validation_n_max", "validation_n_convergence",
      "validation_n_max_heywood", "validation_n_min_recovery",
      "validation_n_max_loading_error", "validation_n_min_dominance",
      "validation_n_max_cross_error", "validation_n_max_factor_cor_error",
      "validation_n_on_inadmissible", "validation_data", "validation_ordered"
    )
  )

  plot_options <- .semantica_option_list(
    plot_options,
    "plot_options",
    allowed = c(
      "generate_plots", "interactive_mode", "save_plots", "plot_out_dir",
      "plot_device", "plot_width", "plot_height", "plot_dpi",
      "plot_before_path_model", "plot_before_path_refit_max_items",
      "plot_network_max_items", "plot_mds_max_items",
      "plot_path_proxy_max_items", "include_interactive_plot",
      "plot_progress"
    ),
    aliases = c(
      generate = "generate_plots",
      save = "save_plots",
      out_dir = "plot_out_dir",
      device = "plot_device",
      width = "plot_width",
      height = "plot_height",
      dpi = "plot_dpi",
      before_path_model = "plot_before_path_model",
      before_path_refit_max_items = "plot_before_path_refit_max_items",
      network_max_items = "plot_network_max_items",
      mds_max_items = "plot_mds_max_items",
      path_proxy_max_items = "plot_path_proxy_max_items",
      include_interactive = "include_interactive_plot",
      progress = "plot_progress"
    )
  )

  legacy_args <- .semantica_option_list(
    legacy_args,
    "...",
    aliases = c(candidate_items_per_factor = "n_per_factor",
                items_per_factor = "i.per.f"),
    allow_unknown = TRUE
  )

  if (candidate_items_per_factor_supplied &&
      "n_per_factor" %in% names(legacy_args)) {
    stop(
      "Use either `candidate_items_per_factor` or legacy `n_per_factor`, not both.",
      call. = FALSE
    )
  }
  if (!is.null(items_per_factor) && "i.per.f" %in% names(legacy_args)) {
    stop(
      "Use either `items_per_factor` or legacy `i.per.f`, not both.",
      call. = FALSE
    )
  }

  n_override <- candidate_items_per_factor_supplied ||
    "n_per_factor" %in% names(generation_options) ||
    "n_per_factor" %in% names(legacy_args)

  core_args <- list(
    backend = backend,
    scale_name = scale_name,
    scale_description = scale_description,
    factors = factors,
    n_per_factor = candidate_items_per_factor,
    n_per_factor_override = n_override,
    i.per.f = items_per_factor,
    verbose = verbose
  )

  .semantica_merge_options(
    core_args,
    generation_options,
    optimization_options,
    dfi_options,
    pfa_options,
    validation_options,
    plot_options,
    legacy_args
  )
}

#' Configure LLM providers for the configurable pipeline
#'
#' Groups provider, credential, server, and local-model deployment settings.
#' Model names remain explicit arguments of [semantica_full_pipeline()] so users
#' can choose generation and embedding models directly.
#'
#' @param backend Generation backend. Accepted registered values are `"openai"`,
#'   `"anthropic"`, `"groq"`, `"ollama"`, `"llamacpp"`,
#'   `"generic_openai"`, `"python_hf"`, and `"python_llamacpp"`.
#' @param embed_backend Embedding backend using the same registered names as
#'   `backend`. `NULL` reuses `backend`; use a separate value when the chat
#'   provider does not supply embeddings.
#' @param api_key,embed_api_key Provider keys for generation and embedding.
#' @param hf_token,embed_hf_token Hugging Face tokens for local model sessions.
#' @param base_url Optional provider/server URL for generation, including
#'   OpenAI-compatible providers not registered in `SEMANTICA_BACKENDS`.
#' @param embed_base_url Optional provider/server URL for embeddings.
#' @param gguf_path Optional GGUF path for local llama.cpp workflows.
#' @param embedding_device,chat_device,device_map,gpu_layers,model_precision
#'   Local-model deployment controls.
#' @param embed_batch_size Items per embedding request.
#' @param embedding_cache Logical; use persistent content-addressed embedding caching.
#' @param embedding_cache_dir Optional directory for persistent embedding cache files.
#' @param embedding_cache_namespace Optional analyst-defined namespace included in cache keys.
#' @param retry_max_tries Maximum number of attempts for retryable HTTP requests.
#' @param retry_on_failure Logical; retry transient HTTP/provider failures when supported.
#' @param preflight Logical; validate configured provider/model capabilities before network work.
#' @param embedding_task Embedding-task policy. Accepted values are `"auto"`,
#'   `"psychometric_similarity"`, `"clustering"`, `"classification"`,
#'   `"search_document"`, `"search_query"`, and `"none"`. `"auto"` is
#'   recommended: it applies a documented model-specific instruction only when
#'   SEMANTICA knows that the configured embedding family requires one.
#' @param embedding_instruction Optional explicit embedding prefix/instruction; overrides automatic model-specific instructions.
#' @param embedding_spec Optional embedding capability contract from
#'   [semantica_embedding_spec()]. Use this for an unregistered model whose
#'   documented task-instruction behavior is known.
#' @param timeout_s,embed_timeout_s HTTP timeout in seconds for generation and
#'   optional separate embedding sessions.
#' @param release_local_models Release cached local Python models after use.
#' @param backend_spec,embed_backend_spec Optional explicit custom backend contracts created by [semantica_backend_spec()].
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_llm_config <- function(
  backend = "openai",
  embed_backend = NULL,
  api_key = NULL,
  embed_api_key = NULL,
  hf_token = NULL,
  embed_hf_token = NULL,
  base_url = NULL,
  embed_base_url = NULL,
  gguf_path = NULL,
  embedding_device = "auto",
  chat_device = "auto",
  device_map = NULL,
  gpu_layers = "auto",
  model_precision = "auto",
  embed_batch_size = 64L,
  embedding_cache = TRUE,
  embedding_cache_dir = NULL,
  embedding_cache_namespace = NULL,
  retry_max_tries = 4L,
  retry_on_failure = TRUE,
  preflight = TRUE,
  timeout_s = 120L,
  embed_timeout_s = NULL,
  release_local_models = FALSE,
  embedding_task = "auto",
  embedding_instruction = NULL,
  embedding_spec = NULL,
  backend_spec = NULL,
  embed_backend_spec = NULL
) {
  embed_batch_size <- .semantica_assert_positive_integer(embed_batch_size, "embed_batch_size")
  embedding_cache <- .semantica_assert_flag(embedding_cache, "embedding_cache")
  retry_max_tries <- .semantica_assert_positive_integer(retry_max_tries, "retry_max_tries")
  retry_on_failure <- .semantica_assert_flag(retry_on_failure, "retry_on_failure")
  preflight <- .semantica_assert_flag(preflight, "preflight")
  timeout_s <- .semantica_assert_positive_scalar(timeout_s, "timeout_s")
  embed_timeout_s <- .semantica_assert_optional_positive_scalar(embed_timeout_s, "embed_timeout_s")
  release_local_models <- .semantica_assert_flag(release_local_models, "release_local_models")
  .semantica_config_object(list(
    backend = backend,
    embed_backend = embed_backend,
    api_key = api_key,
    embed_api_key = embed_api_key,
    hf_token = hf_token,
    embed_hf_token = embed_hf_token,
    base_url = base_url,
    embed_base_url = embed_base_url,
    gguf_path = gguf_path,
    embedding_device = embedding_device,
    chat_device = chat_device,
    device_map = device_map,
    gpu_layers = gpu_layers,
    model_precision = model_precision,
    embed_batch_size = embed_batch_size,
    embedding_cache = embedding_cache,
    embedding_cache_dir = embedding_cache_dir,
    embedding_cache_namespace = embedding_cache_namespace,
    retry_max_tries = retry_max_tries,
    retry_on_failure = retry_on_failure,
    preflight = preflight,
    embedding_task = embedding_task,
    embedding_instruction = embedding_instruction,
    embedding_spec = embedding_spec,
    backend_spec = backend_spec,
    embed_backend_spec = embed_backend_spec,
    timeout_s = timeout_s,
    embed_timeout_s = embed_timeout_s,
    release_local_models = release_local_models
  ), "semantica_llm_config")
}

#' Configure generated-pool and selected-form item counts
#'
#' Defines candidate-pool and final-form sizes for the advanced high-level
#' pipeline. The common `pool_items` and `selected_items` controls can also be set
#' directly in [semantica_run()].
#'
#' @param pool Positive integer number of retained candidate items per factor.
#'   This is the post-generation target pool size, not necessarily the number
#'   of raw items requested from the LLM when `overgenerate > 1`.
#' @param selected Number of final selected items per factor. A scalar is
#'   recycled across factors; a named vector can set factor-specific targets.
#' @param override_pool_counts Logical. If `TRUE`, `pool` replaces item counts
#'   nested inside factor/facet specifications and is allocated across facets.
#'   If `FALSE`, nested counts are preserved.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_item_count_config <- function(
  pool = 15L,
  selected = 3L,
  override_pool_counts = TRUE
) {
  pool <- .semantica_assert_positive_integer(pool, "pool")
  selected <- .semantica_assert_positive_integer_vector(selected, "selected")
  override_pool_counts <- .semantica_assert_flag(override_pool_counts, "override_pool_counts")
  .semantica_config_object(list(
    pool = pool,
    selected = selected,
    override_pool_counts = override_pool_counts
  ), "semantica_item_count_config")
}

#' Configure LLM item-generation style
#'
#' Defines language, response format, wording style, overgeneration, and output
#' formatting for the existing item-generation contract. The most common fields
#' are also available directly in [semantica_run()].
#'
#' @param response_format Response scale format requested in prompts.
#' @param item_style Wording style requested in prompts.
#' @param language Generation language.
#' @param overgenerate Positive generation multiplier. For example, `1.5` asks
#'   the LLM for about 50 percent more raw items than the retained target so
#'   generation-stage filtering has alternatives.
#' @param max_retries Retries on short/failed generation attempts.
#' @param global_forbidden_max Number of prior items included as anti-duplicate
#'   examples.
#' @param temperature Non-negative LLM sampling temperature passed to the
#'   configured provider. Lower values generally reduce wording variability;
#'   provider-specific limits still apply.
#' @param structured_output Output contract: `"auto"`, `"numbered"`, or schema-oriented `"json"`.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_generation_config <- function(
  response_format = "5-point Likert",
  item_style = "first-person declarative sentence",
  language = "English",
  overgenerate = 2,
  max_retries = 3L,
  global_forbidden_max = 40L,
  temperature = 0.8,
  structured_output = c("auto", "numbered", "json")
) {
  structured_output <- match.arg(structured_output)
  overgenerate <- .semantica_assert_positive_scalar(overgenerate, "overgenerate")
  max_retries <- .semantica_assert_nonnegative_integer(max_retries, "max_retries")
  global_forbidden_max <- .semantica_assert_nonnegative_integer(global_forbidden_max, "global_forbidden_max")
  temperature <- .semantica_assert_nonnegative_scalar(temperature, "temperature")
  .semantica_config_object(list(
    response_format = response_format,
    item_style = item_style,
    language = language,
    overgenerate = overgenerate,
    max_retries = max_retries,
    global_forbidden_max = global_forbidden_max,
    temperature = temperature,
    structured_output = structured_output
  ), "semantica_generation_config")
}

#' Configure CPU use for SEMANTICA
#'
#' Controls the existing worker-allocation policy used by the advanced pipeline.
#' The `workers` argument of [semantica_run()] covers the common worker-control use case.
#'
#' @param cpu_cores How many CPU cores SEMANTICA may use. Use `"auto"` to let
#'   SEMANTICA resolve an allocation-, coordinator-, physical-core-, and
#'   memory-aware PSOCK worker count, or `1` for serial execution. Explicit
#'   numeric values remain user-authoritative. The
#'   compatibility aliases `"serial"`, `"none"`, and `"off"` also request
#'   serial execution.
#' @param reserve_cpu_cores CPU cores to leave unused when `cpu_cores = "auto"`. The main/coordinator R process is budgeted separately.
#' @param max_cpu_cores Optional hard ceiling on CPU workers.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_resource_config <- function(
  cpu_cores = "auto",
  reserve_cpu_cores = 1L,
  max_cpu_cores = NULL
) {
  if (is.character(cpu_cores) && length(cpu_cores) == 1L && !is.na(cpu_cores) &&
      tolower(trimws(cpu_cores)) %in% c("auto", "serial", "none", "off")) {
    # Preserve the compatibility aliases consumed by .semantica_resource_args().
    cpu_cores <- tolower(trimws(cpu_cores))
  } else {
    cpu_cores <- .semantica_assert_positive_integer(cpu_cores, "cpu_cores")
  }
  reserve_cpu_cores <- .semantica_assert_nonnegative_integer(reserve_cpu_cores, "reserve_cpu_cores")
  max_cpu_cores <- .semantica_assert_optional_positive_integer(max_cpu_cores, "max_cpu_cores")
  .semantica_config_object(list(
    cpu_cores = cpu_cores,
    reserve_cpu_cores = reserve_cpu_cores,
    max_cpu_cores = max_cpu_cores
  ), "semantica_resource_config")
}

#' Configure embedding cosine computation
#'
#' Advanced controls for cosine preprocessing, device selection, precision, and
#' representation-sensitivity diagnostics. These settings do not configure LLM
#' generation or structural-model computation.
#'
#' @param cosine_adjustment Cosine preprocessing, `"none"` or `"mean_center"`.
#' @param semantic_calibration Optional calibration matrix/function.
#' @param compute_cosine_sensitivity Compute none-versus-mean-centered
#'   sensitivity diagnostics.
#' @param cosine_sensitivity_max_items Maximum number of items used in the optional cosine-sensitivity diagnostic before deterministic subsampling.
#' @param cosine_sensitivity_seed Seed used when the cosine-sensitivity diagnostic subsamples a large item pool.
#' @param compute_device Cosine compute backend: `"cpu"`, `"auto"`,
#'   `"cuda"`, `"cuda:<index>"`, or `"mps"`. CPU is the portable default.
#' @param gpu_fallback Explicit GPU fallback policy; `NULL`/error preserves a
#'   failed explicit accelerator request, while `"cpu"` permits CPU fallback.
#' @param gpu_precision Cosine precision policy, normally `"double"` or
#'   `"single"`; MPS requires single precision when explicitly requested.
#' @param compute_memory_limit Optional working-memory ceiling.
#' @param retain_embeddings Retain dense embeddings in the returned object.
#'   `NULL` lets [semantica_full_pipeline()] infer this from requested
#'   diagnostics.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_compute_config <- function(
  cosine_adjustment = c("none", "mean_center"),
  semantic_calibration = NULL,
  compute_cosine_sensitivity = TRUE,
  cosine_sensitivity_max_items = 1500L,
  cosine_sensitivity_seed = 1L,
  compute_device = "cpu",
  gpu_fallback = NULL,
  gpu_precision = "double",
  compute_memory_limit = NULL,
  retain_embeddings = NULL
) {
  cosine_adjustment <- match.arg(cosine_adjustment)
  compute_cosine_sensitivity <- .semantica_assert_flag(compute_cosine_sensitivity, "compute_cosine_sensitivity")
  cosine_sensitivity_max_items <- .semantica_assert_positive_integer(cosine_sensitivity_max_items, "cosine_sensitivity_max_items")
  retain_embeddings <- .semantica_assert_optional_flag(retain_embeddings, "retain_embeddings")
  .semantica_config_object(list(
    cosine_adjustment = cosine_adjustment,
    semantic_calibration = semantic_calibration,
    compute_cosine_sensitivity = compute_cosine_sensitivity,
    cosine_sensitivity_max_items = cosine_sensitivity_max_items,
    cosine_sensitivity_seed = cosine_sensitivity_seed,
    compute_device = compute_device,
    gpu_fallback = gpu_fallback,
    gpu_precision = gpu_precision,
    compute_memory_limit = compute_memory_limit,
    retain_embeddings = retain_embeddings
  ), "semantica_compute_config")
}

#' Configure ESEM options for the configurable pipeline
#'
#' Collects advanced proxy-ESEM scheduling and scoring options for
#' [semantica_full_pipeline()]. Normal scale-construction workflows should prefer
#' [semantica_run()] and its coherent ACO presets.
#'
#' @param proxy_reference_n Semantic-proxy ESEM/DFI reference N, or `"auto"`.
#' @param rotation ESEM rotation string passed to `lavaan`; `"geomin"` is the
#'   SEMANTICA default. Other values must be supported by the installed
#'   `lavaan` version. SEMANTICA's solver fallback may try `"oblimin"` when a
#'   geomin fit is inadmissible (and vice versa).
#' @param rotation_args Rotation arguments passed to `lavaan`.
#' @param score_mode ESEM proxy score aggregation: `"current"` preserves the
#'   established fit/quality score; `"structure_weighted"` additionally
#'   emphasizes simple-structure and intended-factor dominance diagnostics.
#' @param cadence_mode ESEM checkpoint scheduler. `"adaptive"` preserves the
#'   established entropy-responsive advanced behavior; `"fixed"` evaluates at
#'   the declared `esem_every` interval exactly.
#' @param esem_every,run_esem_during_search,esem_weight,fast_esem,fast_esem_iter_max,full_esem_iter_max,esem_eval_top_k
#'   Optional convenience overrides for ESEM search controls. The canonical
#'   configurable interface keeps these as top-level [semantica_full_pipeline()]
#'   arguments; top-level values take precedence when both are supplied.
#' @param esem_failure_policy Optional ESEM failure policy: `"stop"` or
#'   `"semantic_fallback"`. Under fallback, an all-failed checkpoint is scored
#'   semantically/PFA for that checkpoint, but later ESEM checkpoints are still
#'   attempted. Final archived candidates are also refit before non-ESEM
#'   fallback selection is allowed.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_esem_config <- function(
  proxy_reference_n = "auto",
  rotation = "geomin",
  rotation_args = list(geomin.epsilon = 0.50),
  score_mode = c("current", "structure_weighted"),
  esem_every = NULL,
  run_esem_during_search = NULL,
  esem_weight = NULL,
  esem_failure_policy = NULL,
  fast_esem = NULL,
  fast_esem_iter_max = NULL,
  full_esem_iter_max = NULL,
  esem_eval_top_k = NULL,
  cadence_mode = c("adaptive", "fixed")
) {
  score_mode <- match.arg(score_mode)
  cadence_mode <- match.arg(cadence_mode)
  esem_every <- .semantica_assert_optional_positive_integer(esem_every, "esem_every")
  run_esem_during_search <- .semantica_assert_optional_flag(run_esem_during_search, "run_esem_during_search")
  esem_weight <- .semantica_assert_optional_probability(esem_weight, "esem_weight")
  fast_esem <- .semantica_assert_optional_flag(fast_esem, "fast_esem")
  fast_esem_iter_max <- .semantica_assert_optional_positive_integer(fast_esem_iter_max, "fast_esem_iter_max")
  full_esem_iter_max <- .semantica_assert_optional_positive_integer(full_esem_iter_max, "full_esem_iter_max")
  esem_eval_top_k <- .semantica_assert_optional_positive_integer_or_inf(esem_eval_top_k, "esem_eval_top_k")
  if (!is.null(esem_failure_policy)) {
    esem_failure_policy <- match.arg(
      esem_failure_policy,
      c("stop", "semantic_fallback")
    )
  }
  .semantica_config_object(list(
    proxy_reference_n = proxy_reference_n,
    rotation = rotation,
    rotation_args = rotation_args,
    score_mode = score_mode,
    cadence_mode = cadence_mode,
    esem_every = esem_every,
    run_esem_during_search = run_esem_during_search,
    esem_weight = esem_weight,
    esem_failure_policy = esem_failure_policy,
    fast_esem = fast_esem,
    fast_esem_iter_max = fast_esem_iter_max,
    full_esem_iter_max = full_esem_iter_max,
    esem_eval_top_k = esem_eval_top_k
  ), "semantica_esem_config")
}

#' Configure semantic and psychometric quality guardrails
#'
#' @param profile One of `"standard"`, `"lenient"`, or `"strict"`.
#' @param redundancy_threshold,dup_threshold,htmt_threshold Optional threshold
#'   overrides. `htmt_threshold` is a descriptive semantic-proxy reference by
#'   default; it affects the optimization utility only when
#'   `htmt_objective_role = "penalty"` is explicitly requested.
#' @param htmt_objective_role Role of the HTMT-like semantic proxy in ESEM
#'   scoring. `"diagnostic"` (default) reports overlap continuously without
#'   changing the objective; `"penalty"` restores threshold-based objective
#'   influence for compatibility/sensitivity analyses.
#' @param cohesion_retention Candidate-pool retention around the target
#'   within-factor similarity.
#' @param within_similarity_target,within_similarity_band Within-factor
#'   semantic cohesion target and tolerance. In the default multidimensional
#'   relative objective these act as cohesion/redundancy guards rather than as
#'   the primary definition of semantic quality.
#' @param semantic_objective_mode Multidimensional semantic objective.
#'   `"relative_conservative"` (default) combines threshold-free stochastic
#'   superiority with a robust within-between median-gap component.
#'   `"legacy_target_burden"` retains the 0.4.x score for reproducibility.
#' @param facet_coverage_weight Soft weight for facet coverage.
#' @param psychometric_guard_weight Soft penalty strength for weak proxy
#'   structure.
#' @param psychometric_guard_min_ave,psychometric_guard_min_loading,psychometric_guard_min_primary_ge_50
#'   Minimum proxy-structure guardrails.
#' @param threshold_mode `"fixed"` preserves declared thresholds; `"adaptive_pool"` enables an explicitly experimental pool-relative heuristic.
#' @param adaptive_redundancy_quantile,adaptive_duplicate_quantile Quantiles used by the experimental pool-relative threshold calibration.
#' @param construct_blueprint Optional object from [semantica_construct_blueprint()].
#' @param nomological_weight Optional theory-alignment weight for matching specified *semantic-domain* factor relations; defaults to zero.
#' @param polarity_screen Run conservative reverse/negation wording diagnostics.
#' @param content_alignment_mode `"diagnostic"` (default) reports item-to-definition
#'   alignment without excluding candidates; `"guard"` applies a conservative,
#'   pool-relative guard that removes only clear factor mismatches or explicit
#'   exclusion conflicts when enough alternatives remain; `"off"` disables it.
#' @param polarity_action `"guard"` excludes flagged wording when enough
#'   alternatives remain, `"diagnostic"` only reports it, and `"off"` disables
#'   selection guarding.
#' @param within_target_method Automatic within-factor target method when
#'   `within_similarity_target = NULL`. `"nonredundant_median"` uses the median
#'   within-factor similarity below the configured redundancy threshold in the
#'   current pool/model; `"legacy_q40"` reproduces the older 0.25--0.55-clamped
#'   40th-percentile heuristic for compatibility studies.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_quality_config <- function(
  profile = c("standard", "lenient", "strict"),
  redundancy_threshold = NULL,
  dup_threshold = NULL,
  htmt_threshold = NULL,
  htmt_objective_role = c("diagnostic", "penalty"),
  cohesion_retention = NULL,
  within_similarity_target = NULL,
  within_similarity_band = NULL,
  semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
  facet_coverage_weight = NULL,
  psychometric_guard_weight = NULL,
  psychometric_guard_min_ave = NULL,
  psychometric_guard_min_loading = NULL,
  psychometric_guard_min_primary_ge_50 = NULL,
  threshold_mode = c("fixed", "adaptive_pool"),
  adaptive_redundancy_quantile = 0.95,
  adaptive_duplicate_quantile = 0.99,
  construct_blueprint = NULL,
  nomological_weight = 0,
  polarity_screen = TRUE,
  content_alignment_mode = c("diagnostic", "guard", "off"),
  polarity_action = c("diagnostic", "guard", "off"),
  within_target_method = c("nonredundant_median", "legacy_q40")
) {
  profile <- match.arg(profile)
  threshold_mode <- match.arg(threshold_mode)
  htmt_objective_role <- match.arg(htmt_objective_role)
  content_alignment_mode <- match.arg(content_alignment_mode)
  polarity_action <- match.arg(polarity_action)
  within_target_method <- match.arg(within_target_method)
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  adaptive_redundancy_quantile <- .semantica_assert_probability(adaptive_redundancy_quantile, "adaptive_redundancy_quantile")
  adaptive_duplicate_quantile <- .semantica_assert_probability(adaptive_duplicate_quantile, "adaptive_duplicate_quantile")
  polarity_screen <- .semantica_assert_flag(polarity_screen, "polarity_screen")
  defaults <- .semantica_decision_policy()$quality_profiles[[profile]]
  if (is.null(defaults)) {
    stop(sprintf("No centralized decision-policy profile is registered for '%s'.", profile), call. = FALSE)
  }
  overrides <- list(
    redundancy_threshold = redundancy_threshold,
    dup_threshold = dup_threshold,
    htmt_threshold = htmt_threshold,
    cohesion_retention = cohesion_retention,
    within_similarity_band = within_similarity_band,
    facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50
  )
  overrides <- overrides[!vapply(overrides, is.null, logical(1L))]
  out <- utils::modifyList(defaults, overrides, keep.null = TRUE)
  out$profile <- profile
  out$htmt_objective_role <- htmt_objective_role
  out$within_similarity_target <- within_similarity_target
  out$semantic_objective_mode <- semantic_objective_mode
  out$threshold_mode <- threshold_mode
  out$adaptive_redundancy_quantile <- adaptive_redundancy_quantile
  out$adaptive_duplicate_quantile <- adaptive_duplicate_quantile
  out$construct_blueprint <- construct_blueprint
  nomological_weight <- suppressWarnings(as.numeric(nomological_weight[1L]))
  if (length(nomological_weight) != 1L || !is.finite(nomological_weight) || nomological_weight < 0 || nomological_weight > 1) {
    stop("'nomological_weight' must be a finite number in [0, 1].")
  }
  out$nomological_weight <- nomological_weight
  out$polarity_screen <- polarity_screen
  out$content_alignment_mode <- content_alignment_mode
  out$polarity_action <- polarity_action
  out$within_target_method <- within_target_method
  cfg <- .semantica_config_object(
    out, "semantica_quality_config",
    schema_fields = names(formals(semantica_quality_config))
  )
  attr(cfg, "decision_policy_schema") <- .semantica_decision_policy()$schema_version
  attr(cfg, "threshold_policy_origin") <- .semantica_decision_policy()$semantic_thresholds$provenance
  cfg
}

#' Configure sample-free PFA behavior
#'
#' Advanced controls for how embedding-derived PFA is used as a search objective
#' or final diagnostic. PFA here remains representation-derived proxy evidence,
#' not participant-response factor evidence.
#'
#' @param mode `"diagnostic"`, `"objective"`, or `"off"`.
#' @param weight Weight when PFA enters the objective.
#' @param failure_policy Objective-mode PFA failure behavior: `"semantic_fallback"`,
#'   `"penalize"`, or `"stop"`.
#' @param during_search Whether objective-mode PFA is allowed during search.
#' @param every Search interval for PFA objective scoring.
#' @param extraction,final_extraction PFA extraction methods; each accepts
#'   `"principal"` or `"ml"`.
#' @param rotation PFA rotation: `"promax"`, `"target_oblique"`, `"oblimin"`,
#'   `"varimax"`, or `"none"`.
#' @param min_loading,min_margin Simple-structure thresholds.
#' @param unit_diagnostics Compute facet/unit-level PFA when metadata and
#'   embeddings are available.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_pfa_config <- function(
  mode = c("diagnostic", "objective", "off"),
  weight = 0.20,
  failure_policy = c("semantic_fallback", "penalize", "stop"),
  during_search = NULL,
  every = 1L,
  extraction = c("principal", "ml"),
  final_extraction = c("ml", "principal"),
  rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
  min_loading = NULL,
  min_margin = NULL,
  unit_diagnostics = TRUE
) {
  mode <- match.arg(mode)
  failure_policy <- match.arg(failure_policy)
  extraction <- match.arg(extraction)
  final_extraction <- match.arg(final_extraction)
  rotation <- match.arg(rotation)
  if (is.null(during_search)) during_search <- identical(mode, "objective")
  during_search <- .semantica_assert_flag(during_search, "during_search")
  every <- .semantica_assert_positive_integer(every, "every")
  unit_diagnostics <- .semantica_assert_flag(unit_diagnostics, "unit_diagnostics")
  .semantica_config_object(list(
    mode = mode,
    weight = weight,
    failure_policy = failure_policy,
    during_search = during_search,
    every = every,
    extraction = extraction,
    final_extraction = final_extraction,
    rotation = rotation,
    min_loading = min_loading,
    min_margin = min_margin,
    unit_diagnostics = unit_diagnostics
  ), "semantica_pfa_config")
}

#' Configure DFI and fit-calibration behavior
#'
#' Advanced controls for SEMANTICA's established fit-calibration and DFI paths.
#' ACO presets provide coherent defaults for these calibration-level controls
#' when individual tuning is not required.
#'
#' @param mode Calibration mode. `"off"` and `"fast"` both map to
#'   `dfi_mode = "heuristic_semantic"`; `"strict"` maps to
#'   `dfi_mode = "esem_parametric_dfi"`.
#' @param reps,level,criterion Search-time DFI simulation controls.
#' @param esem_reps,search_reps Optional ESEM-DFI replication budgets.
#' @param final_recalibrate,final_reps Optional final DFI recalibration.
#' @param roc_misspec_strength Semantic-ROC misspecification strength.
#' @param strategy ESEM-DFI simulation strategy: `"fixed"` uses the requested
#'   replication budget; `"adaptive"` evaluates batches until the configured
#'   stability rule is reached or the available budget is exhausted.
#' @param adaptive_min_reps,adaptive_batch_reps,adaptive_tol,adaptive_stable_batches
#'   Adaptive ESEM DFI controls.
#' @param fallback_policy DFI fallback policy: `"conservative"` permits the
#'   documented safer fallback sequence when a requested calibration cannot be
#'   estimated; `"requested_only"` does not substitute another DFI method.
#' @param data_type Data-type label used by DFI/ESEM logic. The established
#'   workflow recognizes `"continuous"`, `"likert"`, `"categorical"`, and
#'   `"nonnormal"`; some DFI paths require `original_data` for non-continuous
#'   assumptions and otherwise fall back transparently.
#' @param target_loadings,target_factor_cors,loading_pattern,embed_reliability,residual_inflation
#'   Population assumptions for fallback DFI.
#' @param warmup_iters Warm-up iterations before DFI calibration.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_fit_calibration_config <- function(
  mode = c("auto", "semantic_roc_dfi", "semantic_approx_dfi",
           "esem_parametric_dfi", "strict_cfa_dfi", "heuristic_semantic",
           "off", "fast", "strict"),
  reps = 500L,
  level = 1,
  criterion = "Sensitivity",
  esem_reps = NULL,
  search_reps = NULL,
  final_recalibrate = FALSE,
  final_reps = NULL,
  roc_misspec_strength = 1.0,
  strategy = c("fixed", "adaptive"),
  adaptive_min_reps = NULL,
  adaptive_batch_reps = 50L,
  adaptive_tol = 0.002,
  adaptive_stable_batches = 2L,
  fallback_policy = c("conservative", "requested_only"),
  data_type = "continuous",
  target_loadings = 0.70,
  target_factor_cors = NULL,
  loading_pattern = "varied",
  embed_reliability = 1.0,
  residual_inflation = 0.0,
  warmup_iters = 5L
) {
  mode <- match.arg(mode)
  strategy <- match.arg(strategy)
  fallback_policy <- match.arg(fallback_policy)
  reps <- .semantica_assert_positive_integer(reps, "reps")
  esem_reps <- .semantica_assert_optional_positive_integer(esem_reps, "esem_reps")
  search_reps <- .semantica_assert_optional_positive_integer(search_reps, "search_reps")
  final_recalibrate <- .semantica_assert_flag(final_recalibrate, "final_recalibrate")
  final_reps <- .semantica_assert_optional_positive_integer(final_reps, "final_reps")
  adaptive_min_reps <- .semantica_assert_optional_positive_integer(adaptive_min_reps, "adaptive_min_reps")
  adaptive_batch_reps <- .semantica_assert_positive_integer(adaptive_batch_reps, "adaptive_batch_reps")
  adaptive_stable_batches <- .semantica_assert_positive_integer(adaptive_stable_batches, "adaptive_stable_batches")
  warmup_iters <- .semantica_assert_nonnegative_integer(warmup_iters, "warmup_iters")
  dfi_mode <- switch(
    mode,
    off = "heuristic_semantic",
    fast = "heuristic_semantic",
    strict = "esem_parametric_dfi",
    mode
  )
  .semantica_config_object(list(
    mode = mode,
    dfi_mode = dfi_mode,
    reps = reps,
    level = level,
    criterion = criterion,
    esem_reps = esem_reps,
    search_reps = search_reps,
    final_recalibrate = final_recalibrate,
    final_reps = final_reps,
    roc_misspec_strength = roc_misspec_strength,
    strategy = strategy,
    adaptive_min_reps = adaptive_min_reps,
    adaptive_batch_reps = adaptive_batch_reps,
    adaptive_tol = adaptive_tol,
    adaptive_stable_batches = adaptive_stable_batches,
    fallback_policy = fallback_policy,
    data_type = data_type,
    target_loadings = target_loadings,
    target_factor_cors = target_factor_cors,
    loading_pattern = loading_pattern,
    embed_reliability = embed_reliability,
    residual_inflation = residual_inflation,
    warmup_iters = warmup_iters
  ), "semantica_fit_calibration_config")
}

#' Configure optional diagnostics for the configurable pipeline
#'
#' @param final_fit Final fit companion diagnostics: `"off"`, `"dddfi"`, or
#'   `"extended"` (`DDDFI` plus `equivTest`).
#' @param final_dddfi_reps,final_dddfi_mad_target DDDFI controls when enabled.
#' @param semantic_stability Run semantic-proxy reference-N sensitivity.
#' @param reference_rmsea_close,reference_rmsea_poor,reference_power,reference_alpha,reference_max_n
#'   RMSEA-power reference-N controls. Defaults contrast close fit at .05 with
#'   misfit at .06; these settings define the proxy reference-N calculation, not
#'   the separate ESEM fit cutoffs.
#' @param semantic_n_grid,semantic_n_multipliers,semantic_n_iter_max
#'   Reference-N sensitivity controls.
#' @param validation_planning Estimate response-data planning N.
#' @param validation_n_reps,validation_n_grid,validation_n_max,validation_n_convergence,validation_n_max_heywood,validation_n_min_recovery,validation_n_max_loading_error,validation_n_min_dominance,validation_n_max_cross_error,validation_n_max_factor_cor_error
#'   Validation-N planning controls.
#' @param validation_planning_on_inadmissible What to do when response-data
#'   planning is requested but the selected semantic-proxy ESEM is inadmissible.
#'   `"skip"` (default) avoids a misleading Monte Carlo sample-size exercise;
#'   `"run"` forces the legacy PFA-informed planning calculation.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_diagnostics_config <- function(
  final_fit = c("off", "dddfi", "extended"),
  final_dddfi_reps = 250L,
  final_dddfi_mad_target = c("close", "fair", "mediocre"),
  semantic_stability = TRUE,
  reference_rmsea_close = 0.05,
  reference_rmsea_poor = 0.06,
  reference_power = 0.80,
  reference_alpha = 0.05,
  reference_max_n = Inf,
  semantic_n_grid = NULL,
  semantic_n_multipliers = c(0.5, 1, 1.5, 2),
  semantic_n_iter_max = 800L,
  validation_planning = FALSE,
  validation_n_reps = 20L,
  validation_n_grid = NULL,
  validation_n_max = 2000L,
  validation_n_convergence = 0.90,
  validation_n_max_heywood = 0.05,
  validation_n_min_recovery = 0.90,
  validation_n_max_loading_error = 0.10,
  validation_n_min_dominance = NULL,
  validation_n_max_cross_error = NULL,
  validation_n_max_factor_cor_error = NULL,
  validation_planning_on_inadmissible = c("skip", "run")
) {
  final_fit <- match.arg(final_fit)
  final_dddfi_mad_target <- match.arg(final_dddfi_mad_target)
  validation_planning_on_inadmissible <- match.arg(validation_planning_on_inadmissible)
  final_dddfi_reps <- .semantica_assert_positive_integer(final_dddfi_reps, "final_dddfi_reps")
  semantic_stability <- .semantica_assert_flag(semantic_stability, "semantic_stability")
  semantic_n_iter_max <- .semantica_assert_positive_integer(semantic_n_iter_max, "semantic_n_iter_max")
  validation_planning <- .semantica_assert_flag(validation_planning, "validation_planning")
  validation_n_reps <- .semantica_assert_positive_integer(validation_n_reps, "validation_n_reps")
  validation_n_max <- .semantica_assert_positive_integer(validation_n_max, "validation_n_max")
  .semantica_config_object(list(
    final_fit = final_fit,
    final_dddfi = final_fit %in% c("dddfi", "extended"),
    final_equivtest = identical(final_fit, "extended"),
    final_dddfi_reps = final_dddfi_reps,
    final_dddfi_mad_target = final_dddfi_mad_target,
    semantic_stability = semantic_stability,
    reference_rmsea_close = reference_rmsea_close,
    reference_rmsea_poor = reference_rmsea_poor,
    reference_power = reference_power,
    reference_alpha = reference_alpha,
    reference_max_n = reference_max_n,
    semantic_n_grid = semantic_n_grid,
    semantic_n_multipliers = semantic_n_multipliers,
    semantic_n_iter_max = semantic_n_iter_max,
    validation_planning = validation_planning,
    validation_n_reps = validation_n_reps,
    validation_n_grid = validation_n_grid,
    validation_n_max = validation_n_max,
    validation_n_convergence = validation_n_convergence,
    validation_n_max_heywood = validation_n_max_heywood,
    validation_n_min_recovery = validation_n_min_recovery,
    validation_n_max_loading_error = validation_n_max_loading_error,
    validation_n_min_dominance = validation_n_min_dominance,
    validation_n_max_cross_error = validation_n_max_cross_error,
    validation_n_max_factor_cor_error = validation_n_max_factor_cor_error,
    validation_planning_on_inadmissible = validation_planning_on_inadmissible
  ), "semantica_diagnostics_config")
}

#' Configure plotting for the configurable pipeline
#'
#' Controls how high-level diagnostic plots are produced or saved. The default
#' `"summary"` level retains the core report set in the result object: the
#' summary, ACO fitness evolution, BEFORE/AFTER ESEM path views, and PFA
#' solution plot. For an already-computed result, call `plot(result)` for the
#' summary or `plot(result, which = "all")` for the complete plot set.
#'
#' @param level `"none"`, `"summary"`, or `"full"`. `"summary"` stores the
#'   five core report plots; `"full"` requests the complete visualization set.
#' @param interactive_mode `"2d"` or `"3d"` for the interactive MDS plot.
#' @param save Save generated plots.
#' @param out_dir Output directory.
#' @param device,width,height,dpi Saved-plot device controls.
#' @param before_path_model Method used for the full-level BEFORE path panel.
#' @param before_path_refit_max_items Maximum item count allowed for an optional BEFORE full-pool refit.
#' @param network_max_items Maximum pool items rendered in semantic-network diagnostics.
#' @param mds_max_items Maximum pool items included in MDS diagnostics.
#' @param path_proxy_max_items Maximum pool items represented in the fast BEFORE path proxy.
#' @param include_interactive Logical; include interactive plotly diagnostics when requested.
#' @param progress Print per-plot progress; `NULL` follows `verbose`.
#' @return A configuration list for [semantica_full_pipeline()].
#' @export
semantica_plot_config <- function(
  level = c("summary", "full", "none"),
  interactive_mode = c("2d", "3d"),
  save = FALSE,
  out_dir = "semantica_plots/",
  device = "png",
  width = 12,
  height = 8,
  dpi = 180,
  before_path_model = c("proxy", "refit"),
  before_path_refit_max_items = 60L,
  network_max_items = 200L,
  mds_max_items = 250L,
  path_proxy_max_items = 150L,
  include_interactive = TRUE,
  progress = NULL
) {
  level <- match.arg(level)
  interactive_mode <- match.arg(interactive_mode)
  before_path_model <- match.arg(before_path_model)
  save <- .semantica_assert_flag(save, "save")
  width <- .semantica_assert_positive_scalar(width, "width")
  height <- .semantica_assert_positive_scalar(height, "height")
  dpi <- .semantica_assert_positive_scalar(dpi, "dpi")
  before_path_refit_max_items <- .semantica_assert_positive_integer_or_inf(before_path_refit_max_items, "before_path_refit_max_items")
  network_max_items <- .semantica_assert_positive_integer_or_inf(network_max_items, "network_max_items")
  mds_max_items <- .semantica_assert_positive_integer_or_inf(mds_max_items, "mds_max_items")
  path_proxy_max_items <- .semantica_assert_positive_integer_or_inf(path_proxy_max_items, "path_proxy_max_items")
  include_interactive <- .semantica_assert_flag(include_interactive, "include_interactive")
  progress <- .semantica_assert_optional_flag(progress, "progress")
  .semantica_config_object(list(
    level = level,
    interactive_mode = interactive_mode,
    save = save,
    out_dir = out_dir,
    device = device,
    width = width,
    height = height,
    dpi = dpi,
    before_path_model = before_path_model,
    before_path_refit_max_items = before_path_refit_max_items,
    network_max_items = network_max_items,
    mds_max_items = mds_max_items,
    path_proxy_max_items = path_proxy_max_items,
    include_interactive = include_interactive,
    progress = progress
  ), "semantica_plot_config")
}

.semantica_selected_counts <- function(selected, factors) {
  factor_names <- names(factors)
  if (is.null(factor_names) || any(!nzchar(factor_names))) {
    stop("'factors' must be a named list.", call. = FALSE)
  }
  selected <- suppressWarnings(as.integer(selected))
  if (length(selected) == 1L) {
    selected <- stats::setNames(rep(selected, length(factor_names)), factor_names)
  } else {
    if (length(selected) != length(factor_names)) {
      stop("'item_counts$selected' must be a scalar or have one value per factor.", call. = FALSE)
    }
    if (is.null(names(selected)) || any(!nzchar(names(selected)))) {
      names(selected) <- factor_names
    } else {
      missing_names <- setdiff(factor_names, names(selected))
      extra_names <- setdiff(names(selected), factor_names)
      if (length(missing_names) || length(extra_names)) {
        stop(
          "'item_counts$selected' names must match the names in 'factors'.",
          call. = FALSE
        )
      }
      selected <- selected[factor_names]
    }
  }
  if (anyNA(selected) || any(selected < 1L)) {
    stop("'item_counts$selected' values must be positive integers.", call. = FALSE)
  }
  selected
}

.semantica_normalize_plot_config <- function(plots) {
  if (is.logical(plots) && length(plots) == 1L) {
    return(semantica_plot_config(level = if (isTRUE(plots)) "full" else "none"))
  }
  if (is.character(plots) && length(plots) == 1L) {
    return(semantica_plot_config(level = plots))
  }
  .semantica_merge_config(plots, semantica_plot_config(), "plots")
}

.semantica_core_result_plots <- function(result, plots, progress = FALSE) {
  generated <- semantica_plot_all(
    result = result,
    interactive_mode = plots$interactive_mode,
    save = plots$save,
    out_dir = plots$out_dir,
    device = plots$device,
    width = plots$width,
    height = plots$height,
    dpi = plots$dpi,
    before_path_model = "proxy",
    before_path_refit_max_items = plots$before_path_refit_max_items,
    network_max_items = plots$network_max_items,
    mds_max_items = plots$mds_max_items,
    path_proxy_max_items = plots$path_proxy_max_items,
    include_interactive = FALSE,
    progress = progress,
    which = c("fitness", "paths", "pfa", "summary")
  )

  out <- list(
    plot_summary_of_results = generated$plot_summary_of_results %||% NULL,
    plot_fitness_evolution = generated$p02_fitness %||% NULL,
    plot_esem_before = generated$p10a_path_before %||% NULL,
    plot_esem_after = generated$p10b_path_after %||% NULL,
    plot_pfa_diagnostics = generated$p13_pfa %||% NULL
  )
  attr(out, "semantica_plot_failures") <- attr(generated, "semantica_plot_failures")
  attr(out, "semantica_plot_manifest") <- attr(generated, "semantica_plot_manifest")
  out
}

.semantica_resource_args <- function(resources) {
  cpu_cores <- resources$cpu_cores %||% resources$n.cores %||% "auto"
  reserve_cpu_cores <- resources$reserve_cpu_cores %||%
    resources$reserve.cores %||% 1L
  max_cpu_cores <- resources$max_cpu_cores %||% resources$max.cores
  serial_alias <- is.character(cpu_cores) &&
    length(cpu_cores) == 1L &&
    tolower(cpu_cores) %in% c("serial", "none", "off")
  if (serial_alias) cpu_cores <- 1L
  use_parallel <- !(is.numeric(cpu_cores) && length(cpu_cores) == 1L &&
                      is.finite(cpu_cores) && as.integer(cpu_cores) <= 1L)
  list(
    n.cores = cpu_cores,
    use_parallel = use_parallel,
    reserve.cores = reserve_cpu_cores,
    max.cores = max_cpu_cores
  )
}

#' Configurable SEMANTICA pipeline
#'
#' User-facing one-call workflow for construct definition, LLM generation,
#' semantic embedding, ACO-ESEM item selection, optional response-data
#' validation, and compact diagnostics. The previous all-arguments interface is
#' still available as [semantica_full_pipeline_custom()].
#'
#' @param scale_name Short scale name.
#' @param scale_description Overall construct description.
#' @param factors Named factor/facet specification list.
#' @param llm Provider and credential config from [semantica_llm_config()]. A
#'   single character is treated as the backend name.
#' @param chat_model,embed_model Explicit generation and embedding model names.
#' @param item_counts Pool/final item-count config from
#'   [semantica_item_count_config()].
#' @param generation Item-generation style config from
#'   [semantica_generation_config()].
#' @param ants Positive integer ACO population size (candidate solutions sampled
#'   per iteration). Larger values broaden search but increase semantic/PFA work
#'   and can increase ESEM requests at checkpoints.
#' @param max.iter Compatibility alias for `search_patience`. It is retained for
#'   existing scripts and no longer controls pheromone evaporation.
#' @param search_patience Optional positive integer non-improvement/stagnation
#'   patience. `NULL` uses `max.iter`.
#' @param max_total_iter Optional positive integer hard ceiling on ACO
#'   iterations. `NULL`/`Inf` leaves termination to patience/archive stability.
#' @param evaporation Pheromone-memory configuration from
#'   [semantica_evaporation_config()].
#' @param max_esem_fits Optional positive integer ceiling on unique search-time
#'   ESEM candidate jobs. Cache hits and coalesced duplicates do not consume the
#'   budget; archive/final diagnostic refits are counted separately.
#' @param elite_k Positive integer maximum number of unique elite solutions kept
#'   for final comparison.
#' @param archive_stable_window Stable semantic-archive updates required for
#'   heuristic stopping.
#' @param structural_archive_stable_window Stable successful PFA/ESEM archive
#'   updates required for active structural tracks.
#' @param min_successful_pfa_checkpoints,min_successful_esem_checkpoints Minimum
#'   successful evidence checkpoints before archive-based stopping is eligible.
#' @param esem_eval_top_k `NULL`, `Inf`, or a positive integer controlling how
#'   many top proposal candidates are submitted at each ESEM checkpoint.
#' @param esem_every Positive integer baseline checkpoint interval. SEMANTICA may
#'   adapt the effective interval to current search entropy.
#' @param run_esem_during_search Logical; if `TRUE`, attempt semantic-proxy ESEM
#'   checkpoints during ACO. Final archive refits are still separate.
#' @param esem_weight Number from 0 to 1 controlling ESEM contribution when an
#'   admissible ESEM-guided score is available.
#' @param esem_failure_policy `"stop"` or `"semantic_fallback"`. `"stop"`
#'   aborts when all candidates at a required checkpoint fail.
#'   `"semantic_fallback"` falls back only for that checkpoint, continues trying
#'   ESEM at later checkpoints, and allows final semantic/PFA fallback only after
#'   every archived finalist has received a full-ESEM attempt.
#' @param fast_esem Logical; use the cheaper search-time ESEM solver path.
#' @param fast_esem_iter_max,full_esem_iter_max Positive iteration ceilings for
#'   search-time and full archive/final ESEM fits, respectively.
#' @param esem Expert ESEM config from [semantica_esem_config()].
#' @param resources CPU-use config from [semantica_resource_config()].
#' @param compute Embedding/cosine config from [semantica_compute_config()].
#' @param quality Semantic/psychometric guardrail config from
#'   [semantica_quality_config()].
#' @param pfa Sample-free PFA config from [semantica_pfa_config()].
#' @param fit_calibration DFI/fallback calibration config from
#'   [semantica_fit_calibration_config()].
#' @param diagnostics Optional diagnostic config from
#'   [semantica_diagnostics_config()].
#' @param validation_data Optional participant response data with columns named
#'   for the selected item IDs. When supplied, SEMANTICA fits a separate
#'   response-data ESEM; this participant-based result is reported separately
#'   from and should take precedence over semantic-proxy ESEM interpretation.
#' @param validation_ordered Optional character vector of ordered item columns
#'   for response-data validation. `NULL` leaves estimator-specific defaults.
#' @param plots Plot config from [semantica_plot_config()], or `"none"`,
#'   `"summary"`, `"full"`, `TRUE`, or `FALSE`.
#' @param seed Optional master seed. It controls SEMANTICA's stochastic analysis
#'   and is also inherited by LLM generation when the configured generation
#'   backend exposes an implemented seed contract (currently Ollama).
#' @param verbose Print progress messages.
#' @section Side effects:
#' May perform provider network I/O, persistent embedding-cache I/O, local
#' Python/model initialization, stochastic ACO/DFI computation, parallel worker
#' creation, and optional plot-file writes according to the supplied configs.
#'
#' @section Reproducibility:
#' The returned `reproducibility` record includes stochastic seeds, resolved
#' model/resource metadata, and a sanitized canonical resolved configuration plus
#' consistency hash. Generation provenance records whether backend-level seed
#' control was actually available, the deterministic task-seed ledger when it
#' was, prompt/item-pool fingerprints, and downstream content-screening state.
#' Remote provider aliases remain mutable unless an actual model revision is
#' available and recorded.
#'
#' @return A `semantica_full_pipeline_result`.
#' @export
semantica_full_pipeline <- function(
  scale_name,
  scale_description,
  factors,
  llm = semantica_llm_config(),
  chat_model = NULL,
  embed_model = NULL,
  item_counts = semantica_item_count_config(),
  generation = semantica_generation_config(),
  ants = 90,
  max.iter = 50,
  search_patience = NULL,
  max_total_iter = NULL,
  evaporation = NULL,
  max_esem_fits = NULL,
  elite_k = 10,
  archive_stable_window = 8L,
  structural_archive_stable_window = 2L,
  min_successful_pfa_checkpoints = 2L,
  min_successful_esem_checkpoints = 2L,
  esem_eval_top_k = NULL,
  esem_every = 10,
  run_esem_during_search = TRUE,
  esem_weight = 0.50,
  esem_failure_policy = c("stop", "semantic_fallback"),
  fast_esem = TRUE,
  fast_esem_iter_max = 500L,
  full_esem_iter_max = 2000L,
  esem = semantica_esem_config(),
  resources = semantica_resource_config(),
  compute = semantica_compute_config(),
  quality = semantica_quality_config(),
  pfa = semantica_pfa_config(),
  fit_calibration = semantica_fit_calibration_config(),
  diagnostics = semantica_diagnostics_config(),
  validation_data = NULL,
  validation_ordered = NULL,
  plots = semantica_plot_config(),
  seed = NULL,
  verbose = TRUE
) {
  top_max_iter <- !missing(max.iter)
  top_esem_every <- !missing(esem_every)
  top_run_esem <- !missing(run_esem_during_search)
  top_esem_weight <- !missing(esem_weight)
  top_esem_failure_policy <- !missing(esem_failure_policy)
  top_fast_esem <- !missing(fast_esem)
  top_fast_esem_iter_max <- !missing(fast_esem_iter_max)
  top_full_esem_iter_max <- !missing(full_esem_iter_max)
  top_esem_eval_top_k <- !missing(esem_eval_top_k)

  # When the new patience argument is supplied by itself, keep the legacy alias
  # synchronized internally so downstream compatibility layers do not interpret
  # the wrapper default as an explicit conflicting user value.
  if (!top_max_iter && !is.null(search_patience)) max.iter <- search_patience

  if (is.character(llm) && length(llm) == 1L) {
    llm <- semantica_llm_config(backend = llm)
  }
  if (is.numeric(resources) || is.character(resources)) {
    resources <- semantica_resource_config(cpu_cores = resources)
  }
  if (is.numeric(item_counts)) {
    item_counts <- semantica_item_count_config(pool = item_counts)
  }
  if (is.character(pfa) && length(pfa) == 1L) {
    pfa <- semantica_pfa_config(mode = pfa)
  }
  if (is.character(fit_calibration) && length(fit_calibration) == 1L) {
    fit_calibration <- semantica_fit_calibration_config(mode = fit_calibration)
  }
  if (is.character(diagnostics) && length(diagnostics) == 1L) {
    diagnostics <- semantica_diagnostics_config(final_fit = diagnostics)
  }

  llm <- .semantica_merge_config(llm, semantica_llm_config(), "llm")
  item_counts <- .semantica_merge_config(item_counts, semantica_item_count_config(), "item_counts")
  generation <- .semantica_merge_config(
    generation, semantica_generation_config(), "generation"
  )
  resources <- .semantica_merge_config(resources, semantica_resource_config(), "resources")
  compute <- .semantica_merge_config(compute, semantica_compute_config(), "compute")
  quality <- .semantica_merge_config(quality, semantica_quality_config(), "quality")
  pfa <- .semantica_merge_config(pfa, semantica_pfa_config(), "pfa")
  fit_calibration <- .semantica_merge_config(
    fit_calibration, semantica_fit_calibration_config(), "fit_calibration"
  )
  diagnostics <- .semantica_merge_config(
    diagnostics, semantica_diagnostics_config(), "diagnostics"
  )
  esem <- .semantica_merge_config(esem, semantica_esem_config(), "esem")
  plots <- .semantica_normalize_plot_config(plots)

  if (!top_esem_every && !is.null(esem$esem_every)) {
    esem_every <- esem$esem_every
  }
  if (!top_run_esem && !is.null(esem$run_esem_during_search)) {
    run_esem_during_search <- esem$run_esem_during_search
  }
  if (!top_esem_weight && !is.null(esem$esem_weight)) {
    esem_weight <- esem$esem_weight
  }
  if (!top_esem_failure_policy && !is.null(esem$esem_failure_policy)) {
    esem_failure_policy <- esem$esem_failure_policy
  }
  if (!top_fast_esem && !is.null(esem$fast_esem)) {
    fast_esem <- esem$fast_esem
  }
  if (!top_fast_esem_iter_max && !is.null(esem$fast_esem_iter_max)) {
    fast_esem_iter_max <- esem$fast_esem_iter_max
  }
  if (!top_full_esem_iter_max && !is.null(esem$full_esem_iter_max)) {
    full_esem_iter_max <- esem$full_esem_iter_max
  }
  if (!top_esem_eval_top_k && !is.null(esem$esem_eval_top_k)) {
    esem_eval_top_k <- esem$esem_eval_top_k
  }
  esem_failure_policy <- match.arg(esem_failure_policy)
  selected_counts <- .semantica_selected_counts(item_counts$selected, factors)
  retain_embeddings <- compute$retain_embeddings
  if (is.null(retain_embeddings)) retain_embeddings <- isTRUE(pfa$unit_diagnostics)
  plot_progress <- plots$progress
  if (is.null(plot_progress)) plot_progress <- verbose
  resource_args <- .semantica_resource_args(resources)

  out <- semantica_full_pipeline_custom(
    backend = llm$backend,
    embed_backend = llm$embed_backend,
    backend_spec = llm$backend_spec,
    embed_backend_spec = llm$embed_backend_spec,
    api_key = llm$api_key,
    embed_api_key = llm$embed_api_key,
    chat_model = chat_model,
    embed_model = embed_model,
    embed_batch_size = llm$embed_batch_size,
    embedding_cache = llm$embedding_cache,
    embedding_cache_dir = llm$embedding_cache_dir,
    embedding_cache_namespace = llm$embedding_cache_namespace,
    retry_max_tries = llm$retry_max_tries,
    retry_on_failure = llm$retry_on_failure,
    preflight = llm$preflight,
    embedding_task = llm$embedding_task,
    embedding_instruction = llm$embedding_instruction,
    embedding_spec = llm$embedding_spec %||% NULL,
    timeout_s = llm$timeout_s,
    embed_timeout_s = llm$embed_timeout_s,
    hf_token = llm$hf_token,
    embed_hf_token = llm$embed_hf_token,
    embedding_device = llm$embedding_device,
    chat_device = llm$chat_device,
    device_map = llm$device_map,
    gpu_layers = llm$gpu_layers,
    model_precision = llm$model_precision,
    base_url = llm$base_url,
    embed_base_url = llm$embed_base_url,
    gguf_path = llm$gguf_path,
    cosine_adjustment = compute$cosine_adjustment,
    semantic_calibration = compute$semantic_calibration,
    compute_cosine_sensitivity = compute$compute_cosine_sensitivity,
    cosine_sensitivity_max_items = compute$cosine_sensitivity_max_items,
    cosine_sensitivity_seed = compute$cosine_sensitivity_seed,
    release_local_models = llm$release_local_models,
    compute_device = compute$compute_device,
    gpu_fallback = compute$gpu_fallback,
    gpu_precision = compute$gpu_precision,
    compute_memory_limit = compute$compute_memory_limit,
    retain_embeddings = retain_embeddings,
    scale_name = scale_name,
    scale_description = scale_description,
    factors = factors,
    n_per_factor = item_counts$pool,
    n_per_factor_override = item_counts$override_pool_counts,
    i.per.f = selected_counts,
    ants = ants,
    max.iter = max.iter,
    search_patience = search_patience,
    evaporation = evaporation,
    esem_every = esem_every,
    esem_cadence_mode = esem$cadence_mode,
    run_esem_during_search = run_esem_during_search,
    max_total_iter = max_total_iter,
    max_esem_fits = max_esem_fits,
    esem_weight = esem_weight,
    esem_failure_policy = esem_failure_policy,
    esem_sample_size = esem$proxy_reference_n,
    elite_k = elite_k,
    esem_eval_top_k = esem_eval_top_k,
    fast_esem = fast_esem,
    fast_esem_iter_max = fast_esem_iter_max,
    full_esem_iter_max = full_esem_iter_max,
    rotation = esem$rotation,
    rotation_args = esem$rotation_args,
    data_type = fit_calibration$data_type,
    target_loadings = fit_calibration$target_loadings,
    target_factor_cors = fit_calibration$target_factor_cors,
    dfi_reps = fit_calibration$reps,
    dfi_level = fit_calibration$level,
    dfi_criterion = fit_calibration$criterion,
    dfi_mode = fit_calibration$dfi_mode,
    dfi_esem_reps = fit_calibration$esem_reps,
    dfi_search_reps = fit_calibration$search_reps,
    final_dfi_recalibrate = fit_calibration$final_recalibrate,
    final_dfi_reps = fit_calibration$final_reps,
    dfi_roc_misspec_strength = fit_calibration$roc_misspec_strength,
    dfi_esem_strategy = fit_calibration$strategy,
    dfi_adaptive_min_reps = fit_calibration$adaptive_min_reps,
    dfi_adaptive_batch_reps = fit_calibration$adaptive_batch_reps,
    dfi_adaptive_tol = fit_calibration$adaptive_tol,
    dfi_adaptive_stable_batches = fit_calibration$adaptive_stable_batches,
    dfi_fallback_policy = fit_calibration$fallback_policy,
    final_dddfi = diagnostics$final_dddfi,
    final_dddfi_reps = diagnostics$final_dddfi_reps,
    final_dddfi_mad_target = diagnostics$final_dddfi_mad_target,
    final_equivtest = diagnostics$final_equivtest,
    loading_pattern = fit_calibration$loading_pattern,
    embed_reliability = fit_calibration$embed_reliability,
    residual_inflation = fit_calibration$residual_inflation,
    dfi_warmup_iters = fit_calibration$warmup_iters,
    redundancy_threshold = quality$redundancy_threshold,
    dup_threshold = quality$dup_threshold,
    htmt_threshold = quality$htmt_threshold,
    htmt_objective_role = quality$htmt_objective_role,
    cohesion_quantile = NULL,
    cohesion_retention = quality$cohesion_retention,
    within_similarity_target = quality$within_similarity_target,
    within_similarity_band = quality$within_similarity_band,
    semantic_objective_mode = quality$semantic_objective_mode,
    facet_coverage_weight = quality$facet_coverage_weight,
    psychometric_guard_weight = quality$psychometric_guard_weight,
    psychometric_guard_min_ave = quality$psychometric_guard_min_ave,
    psychometric_guard_min_loading = quality$psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = quality$psychometric_guard_min_primary_ge_50,
    semantic_threshold_mode = quality$threshold_mode,
    adaptive_redundancy_quantile = quality$adaptive_redundancy_quantile,
    adaptive_duplicate_quantile = quality$adaptive_duplicate_quantile,
    construct_blueprint = quality$construct_blueprint,
    nomological_weight = quality$nomological_weight,
    polarity_screen = quality$polarity_screen,
    content_alignment_mode = quality$content_alignment_mode,
    polarity_action = quality$polarity_action,
    within_target_method = quality$within_target_method,
    pfa_mode = pfa$mode,
    pfa_weight = pfa$weight,
    pfa_failure_policy = pfa$failure_policy %||% "semantic_fallback",
    run_pfa_during_search = pfa$during_search,
    pfa_every = pfa$every,
    pfa_extraction = pfa$extraction,
    pfa_final_extraction = pfa$final_extraction,
    pfa_rotation = pfa$rotation,
    pfa_min_loading = pfa$min_loading %||% quality$psychometric_guard_min_loading,
    pfa_min_margin = pfa$min_margin,
    pfa_unit_diagnostics = pfa$unit_diagnostics,
    reference_rmsea_close = diagnostics$reference_rmsea_close,
    reference_rmsea_poor = diagnostics$reference_rmsea_poor,
    reference_power = diagnostics$reference_power,
    reference_alpha = diagnostics$reference_alpha,
    reference_max_n = diagnostics$reference_max_n,
    semantic_n_sensitivity = diagnostics$semantic_stability,
    semantic_n_grid = diagnostics$semantic_n_grid,
    semantic_n_multipliers = diagnostics$semantic_n_multipliers,
    semantic_n_iter_max = diagnostics$semantic_n_iter_max,
    semantic_esem_score_mode = esem$score_mode,
    validation_n_diagnostic = diagnostics$validation_planning,
    validation_n_reps = diagnostics$validation_n_reps,
    validation_n_grid = diagnostics$validation_n_grid,
    validation_n_max = diagnostics$validation_n_max,
    validation_n_convergence = diagnostics$validation_n_convergence,
    validation_n_max_heywood = diagnostics$validation_n_max_heywood,
    validation_n_min_recovery = diagnostics$validation_n_min_recovery,
    validation_n_max_loading_error = diagnostics$validation_n_max_loading_error,
    validation_n_min_dominance = diagnostics$validation_n_min_dominance,
    validation_n_max_cross_error = diagnostics$validation_n_max_cross_error,
    validation_n_max_factor_cor_error = diagnostics$validation_n_max_factor_cor_error,
    validation_n_on_inadmissible = diagnostics$validation_planning_on_inadmissible,
    sigmoid_center = 0.15,
    elite_multicriteria_rerank = TRUE,
    validation_data = validation_data,
    validation_ordered = validation_ordered,
    sigmoid_steepness = 10,
    heuristic_beta = 0.50,
    archive_stable_window = archive_stable_window,
    structural_archive_stable_window = structural_archive_stable_window,
    min_successful_pfa_checkpoints = min_successful_pfa_checkpoints,
    min_successful_esem_checkpoints = min_successful_esem_checkpoints,
    pheromone_update = "top_elite",
    fixed_evaporation = NULL,
    debug_mode = FALSE,
    keep_solution_history = TRUE,
    history_mode = "summary",
    use_parallel = resource_args$use_parallel,
    n.cores = resource_args$n.cores,
    reserve.cores = resource_args$reserve.cores,
    max.cores = resource_args$max.cores,
    seed = seed,
    generation_seed = seed,
    generate_plots = identical(plots$level, "full"),
    interactive_mode = plots$interactive_mode,
    save_plots = plots$save,
    plot_out_dir = plots$out_dir,
    plot_device = plots$device,
    plot_width = plots$width,
    plot_height = plots$height,
    plot_dpi = plots$dpi,
    plot_before_path_model = plots$before_path_model,
    plot_before_path_refit_max_items = plots$before_path_refit_max_items,
    plot_network_max_items = plots$network_max_items,
    plot_mds_max_items = plots$mds_max_items,
    plot_path_proxy_max_items = plots$path_proxy_max_items,
    include_interactive_plot = plots$include_interactive,
    plot_progress = plot_progress,
    verbose = verbose,
    response_format = generation$response_format,
    item_style = generation$item_style,
    language = generation$language,
    overgenerate = generation$overgenerate,
    max_retries = generation$max_retries,
    global_forbidden_max = generation$global_forbidden_max,
    temperature = generation$temperature,
    structured_output = generation$structured_output
  )

  # Record the sanitized configuration that actually reached the execution
  # boundary after defaults, named-config merges, and top-level precedence.
  resolved_esem <- esem
  resolved_esem$esem_every <- esem_every
  resolved_esem$run_esem_during_search <- run_esem_during_search
  resolved_esem$esem_weight <- esem_weight
  resolved_esem$esem_failure_policy <- esem_failure_policy
  resolved_esem$fast_esem <- fast_esem
  resolved_esem$fast_esem_iter_max <- fast_esem_iter_max
  resolved_esem$full_esem_iter_max <- full_esem_iter_max
  resolved_esem$esem_eval_top_k <- esem_eval_top_k
  resolved_compute <- compute
  resolved_compute$retain_embeddings <- retain_embeddings
  resolved_pfa <- pfa
  resolved_pfa$min_loading <- pfa$min_loading %||% quality$psychometric_guard_min_loading
  resolved_plots <- plots
  resolved_plots$progress <- plot_progress
  resolved_config <- list(
    schema = "semantica-resolved-config-1",
    scale = list(
      scale_name = scale_name,
      scale_description = scale_description,
      factors = factors
    ),
    llm = .semantica_sanitize_config_provenance(llm),
    models = out$reproducibility$models %||% list(),
    item_counts = list(
      pool = item_counts$pool,
      selected = selected_counts,
      override_pool_counts = item_counts$override_pool_counts
    ),
    generation = generation,
    resources = list(
      requested = resources,
      effective = out$resource_plan %||% out$performance$resource %||% NULL
    ),
    compute = resolved_compute,
    quality = quality,
    pfa = resolved_pfa,
    fit_calibration = fit_calibration,
    diagnostics = diagnostics,
    esem = resolved_esem,
    optimizer = list(
      ants = ants,
      max.iter = max.iter,
      search_patience = search_patience %||% max.iter,
      max_total_iter = max_total_iter,
      evaporation = evaporation,
      max_esem_fits = max_esem_fits,
      elite_k = elite_k,
      archive_stable_window = archive_stable_window,
      structural_archive_stable_window = structural_archive_stable_window,
      min_successful_pfa_checkpoints = min_successful_pfa_checkpoints,
      min_successful_esem_checkpoints = min_successful_esem_checkpoints
    ),
    participant_validation = list(
      data_supplied = !is.null(validation_data),
      ordered = validation_ordered
    ),
    plots = resolved_plots,
    seed = seed
  )
  resolved_config <- .semantica_canonicalize_config(
    .semantica_sanitize_config_provenance(resolved_config)
  )
  out$reproducibility$resolved_config_schema <- "semantica-resolved-config-1"
  out$reproducibility$resolved_config <- resolved_config
  out$reproducibility$resolved_config_hash <- .semantica_object_md5(resolved_config)

  if (identical(plots$level, "summary")) {
    out$plots <- .semantica_core_result_plots(
      out,
      plots = plots,
      progress = plot_progress
    )
  }

  out
}

#' Flat compatibility interface for the complete SEMANTICA pipeline
#'
#' Retains the historical flattened argument surface for existing scripts and
#' method-development workflows. New configurable workflows can use
#' [semantica_full_pipeline()] with configuration objects.
#'
#' @param backend_spec,embed_backend_spec Optional explicit custom backend
#'   contracts created by [semantica_backend_spec()] for generation and
#'   embeddings respectively.
#' @param embed_base_url Optional provider/server URL for a separate embedding
#'   backend.
#'
#' Executes the complete psychometric scale construction workflow in a single
#' function call: LLM item generation, semantic embedding, ACO-ESEM optimization
#' with DFI calibration, and comprehensive diagnostic plotting.
#'
#' @inheritParams ACO_with_ESEM
#' @inheritParams semantica_plot_all
#' @param backend Generation backend (see `semantica_connect()`).
#' @param embed_backend Embedding backend. `NULL` = same as backend.
#' @param api_key,embed_api_key API keys for generation/embedding.
#' @param hf_token,embed_hf_token Hugging Face tokens for local model sessions.
#'   Live credentials are removed from returned objects.
#' @param chat_model,embed_model Override default model names.
#' @param embedding_device,chat_device,device_map,gpu_layers,model_precision
#'   Local Python model device configuration passed to `semantica_pipeline()`.
#' @param embed_batch_size Items per embedding backend request. Lower values
#'   reduce peak request memory for local embedding models.
#' @param embedding_cache Logical; use persistent content-addressed embedding caching.
#' @param embedding_cache_dir Optional persistent embedding-cache directory.
#' @param embedding_cache_namespace Optional namespace included in embedding-cache keys.
#' @param embedding_spec Optional embedding capability contract from
#'   [semantica_embedding_spec()]. It controls provider/task text
#'   preparation for unregistered models without changing psychometric
#'   thresholds or automatic cosine transformations.
#' @param retry_max_tries Maximum attempts for retryable provider requests.
#' @param retry_on_failure Logical; retry transient provider failures when supported.
#' @param preflight Logical; run provider/model capability checks before network work.
#' @param embedding_task Embedding-task policy; `"auto"` applies documented model-specific instructions where required.
#' @param embedding_instruction Optional explicit embedding prefix/instruction.
#' @param content_alignment_mode Content-definition alignment behavior:
#'   `"diagnostic"` (default), `"guard"`, or `"off"`. Guard mode is
#'   feasibility-aware and removes only clear factor mismatches or explicit
#'   exclusion conflicts; ordinary ambiguity remains diagnostic.
#' @param polarity_action Selection behavior for wording polarity flags:
#'   `"diagnostic"` (default), `"guard"`, or `"off"`.
#' @param base_url,gguf_path Server overrides or GGUF path.
#' @param cosine_adjustment Embedding cosine preprocessing passed to
#'   `semantica_pipeline()`.
#' @param semantic_calibration Optional matrix or function used to calibrate
#'   the semantic cosine proxy before ACO/ESEM.
#' @param semantic_threshold_mode Semantic threshold policy: fixed defaults or experimental pool-relative calibration.
#' @param adaptive_redundancy_quantile Quantile used for the experimental pool-relative redundancy threshold.
#' @param adaptive_duplicate_quantile Quantile used for the experimental pool-relative duplicate threshold.
#' @param construct_blueprint Optional structured construct/facet blueprint used for coverage diagnostics and constraints.
#' @param polarity_screen Logical; run conservative wording/polarity screening on generated items.
#' @param compute_cosine_sensitivity Logical; compute the optional
#'   none-versus-mean-centered embedding diagnostic.
#' @param cosine_sensitivity_max_items Maximum item count used directly in cosine-sensitivity diagnostics before deterministic subsampling.
#' @param cosine_sensitivity_seed Seed used for any cosine-sensitivity subsampling.
#' @param compute_device,gpu_fallback,gpu_precision,compute_memory_limit
#'   Full-pool cosine compute controls passed to `semantica_pipeline()`.
#' @param release_local_models Logical; release cached Python llama.cpp models
#'   after item generation and embedding to reduce retained RAM in one-shot
#'   local workflows.
#' @param retain_embeddings Logical; retain dense embeddings in the returned
#'   generation object. Keep this `TRUE` when `pfa_unit_diagnostics` is needed.
#' @param scale_name Short name of the scale.
#' @param scale_description One-paragraph construct description.
#' @param factors Named list of factor specs (`$description`, `$n_items`, etc.).
#' @param n_per_factor Retained items per factor. When explicitly supplied,
#'   this overrides nested dimension/facet item counts and is distributed
#'   across facets; when omitted, counts stored in `factors` keep precedence.
#' @param n_per_factor_override Logical; override nested factor/facet counts
#'   using `n_per_factor`. Defaults to `TRUE` only when `n_per_factor` is
#'   explicitly supplied.
#' @param i.per.f Named integer vector of items to select per factor for ACO.
#'   If `NULL`, defaults to 3 items per factor.
#' @param generate_plots Logical; generate diagnostic plots?
#' @param save_plots Logical; save plots to disk?
#' @param plot_device Image format for saved plots (e.g., `"png"`, `"pdf"`).
#' @param plot_width  Plot width in inches.
#' @param plot_height Plot height in inches.
#' @param plot_dpi    Resolution for saved images.
#' @param plot_out_dir Directory for saved plots.
#' @param plot_before_path_model How to build Plot 10's BEFORE panel:
#'   `"proxy"` (default) uses a bounded sample-free proxy; `"refit"` opts into
#'   a new guarded full-pool ESEM estimation during plotting.
#' @param plot_before_path_refit_max_items Maximum pool size for the optional
#'   Plot 10 BEFORE ESEM refit.
#' @param plot_network_max_items Maximum number of BEFORE-pool items rendered
#'   in the semantic network plot. Selected items are always retained.
#' @param plot_mds_max_items Maximum number of pool items included in the
#'   interactive MDS plot. Selected items are always retained.
#' @param plot_path_proxy_max_items Maximum number of pool items represented in
#'   the fast BEFORE path-diagram proxy. Selected items are always retained.
#' @param include_interactive_plot Logical; construct Plot 9's interactive MDS
#'   widget. Disable this when only static diagnostics are needed.
#' @param plot_progress Logical; print per-plot elapsed-time progress.
#' @param pfa_unit_diagnostics Logical; compute sample-free PFA on averaged
#'   facet/unit embeddings when facet metadata are available.
#' @param history_mode History retention policy passed to `ACO_with_ESEM()`.
#'   This high-level pipeline defaults to `"summary"` to avoid retaining every
#'   ant evaluation; use `"full"` for legacy candidate-level traces.
#' @param generation_seed Optional generation-specific seed. `NULL` inherits
#'   the ACO/master `seed`. Supported generation backends receive deterministic
#'   per-call seeds; unsupported protocols are recorded as uncontrolled. Backend
#'   seed control does not guarantee byte-identical text, so exact downstream
#'   replay requires reusing the realized fingerprinted item pool.
#' @param verbose Print progress messages.
#' @param ... Additional arguments passed to `semantica_pipeline()`.
#'
#' @return A named list containing:
#'   * `generation`: Output from `semantica_pipeline()`.
#'   * `optimization`: Output from `ACO_with_ESEM()`.
#'   * `plots`: List of `ggplot`/`plotly` objects (or `NULL`).
#'   * `best_items`: Character vector of selected item IDs.
#'   * `factor_assignment`: Named vector mapping items to factors.
#'   * `best_objective`: Final optimization utility; interpret it together with
#'     `objective_context`, not as a universal scale-quality score.
#'   * `objective_context`: Evidence regime and comparability metadata for the
#'     optimization utility.
#'   * `selection_semantic_context`: Candidate-pool versus selected semantic
#'     discrimination/gap context; selected values are post-selection descriptive.
#'   * `factor_semantic_diagnostics`: Factor-specific selected semantic separation.
#'   * `esem_state`: Consolidated technical and structural-quality ESEM state.
#'   * `pfa_esem_discrepancy`: Complementary PFA/ESEM discrepancy state.
#'   * `item_structure_diagnostics`: Item-level ESEM loading and cross-loading diagnostics.
#'   * `embedding_diagnostics`: Embedding dimensionality, norm, and model checks.
#'   * `cosine_diagnostics`: Cosine similarity distribution diagnostics.
#'   * `fit_indices`: List of CFI, RMSEA, SRMR, AVE, HTMT.
#'   * `semantic_score`: Sigmoid semantic score of the final solution.
#'   * `proposal_objective_score`: Semantic/PFA proposal objective for the final
#'     item set, prior to ESEM-guided scoring.
#'     The legacy `search_objective_score` field is retained as an alias.
#'   * `final_guided_objective_score`: Final objective after requested ESEM
#'     guidance and archive reranking.
#'   * `search_guidance_status`: Whether selection was ESEM-guided, explicitly
#'     semantic-only, or continued under an explicit semantic fallback.
#'   * `candidate_counts`: Generated, eligible, and selected-target counts by
#'     factor after content-pool screening.
#'   * `pfa_diagnostics`: Sample-free pseudo-factor-analysis diagnostics.
#'   * `pfa_unit_diagnostics`: Optional facet/unit-level PFA diagnostics from
#'     averaged unit embeddings.
#'   * `reference_sample_size`: RMSEA-power reference fit N for semantic ESEM/DFI.
#'   * `semantic_n_sensitivity`: Final selected semantic-proxy ESEM refits over
#'     nearby reference-N anchors.
#'   * `cosine_adjustment_sensitivity`: None-versus-mean-centered embedding
#'     cosine sensitivity diagnostic.
#'   * `recommended_validation_n`: PFA-informed Monte Carlo response-data
#'     sample-size planning diagnostic.
#'   * `semantic_similarity_reduction`: Metrics tracking within-factor redundancy change.
#'   * `optimization$esem_alignment` and
#'     `optimization$esem_admissibility`: Final factor-axis mapping and
#'     canonical fit-admissibility diagnostics in the nested optimizer result.
#'   * `semantic_resampling_stability`: Stratified within/between-pair bootstrap
#'     and item-jackknife sensitivity diagnostics. Pair similarities are dependent,
#'     so intervals are descriptive representation-sensitivity summaries rather
#'     than respondent-sampling confidence intervals.
#'   * `semantic_pair_perturbation_stability` and `split_half_stability`: legacy
#'     compatibility aliases; new analyses no longer use their uncalibrated 0.10
#'     stable/unstable rule.
#'   * `resource_plan`, `performance`, and `evaluation_telemetry`: Resolved
#'     workers/devices, stage timings, and cache-aware evaluation accounting.
#'   * `reproducibility`: Master/task seeds, RNG configuration, effective
#'     resources, package versions, and optimizer settings.
#'   * `summary`: Compact summary object from ACO.
#'
#' @section Side effects:
#' May perform provider network I/O, persistent embedding-cache I/O, local
#' Python/model initialization, stochastic ACO/DFI computation, parallel worker
#' creation, response-data fitting, and optional plot-file writes.
#'
#' @section Reproducibility:
#' The returned `reproducibility` record retains master/task seeds, resource and
#' model metadata, and optimizer settings. Provider aliases should be treated as
#' mutable unless an explicit model revision is available and recorded.
#'
#' @export
#' @examples
#' \dontrun{
#' # Requires valid API credentials or an available local backend.
#' result <- semantica_full_pipeline_custom(
#'   backend = "openai",
#'   scale_name = "Cognitive Agility",
#'   scale_description = "Clear and adaptive thinking.",
#'   factors = list(
#'     Clarity = list(description = "Clear thinking.", n_items = 8L),
#'     Flexibility = list(description = "Adaptive thinking.", n_items = 8L)
#'   ),
#'   i.per.f = c(Clarity = 3L, Flexibility = 3L),
#'   ants = 20L,
#'   max.iter = 10L,
#'   generate_plots = TRUE,
#'   verbose = FALSE
#' )
#' }
semantica_full_pipeline_custom <- function(
    # LLM & Embedding Setup
  backend = "openai", embed_backend = NULL, api_key = NULL, embed_api_key = NULL,
  chat_model = NULL, embed_model = NULL, embed_batch_size = 64L,
  embedding_cache = TRUE, embedding_cache_dir = NULL,
  embedding_cache_namespace = NULL, embedding_spec = NULL, retry_max_tries = 4L,
  retry_on_failure = TRUE, preflight = TRUE,
  hf_token = NULL, embed_hf_token = NULL,
  embedding_device = "auto", chat_device = "auto", device_map = NULL,
  gpu_layers = "auto", model_precision = "auto",
  base_url = NULL, gguf_path = NULL,
  cosine_adjustment = c("none", "mean_center"), semantic_calibration = NULL,
  compute_cosine_sensitivity = TRUE, cosine_sensitivity_max_items = 1500L,
  cosine_sensitivity_seed = 1L, release_local_models = FALSE,
  compute_device = "cpu", gpu_fallback = NULL,
  gpu_precision = "double", compute_memory_limit = NULL,
  retain_embeddings = TRUE,
  # Scale & Factor Specs
  scale_name, scale_description, factors, n_per_factor = 15L,
  n_per_factor_override = !missing(n_per_factor),
  # ACO Selection Target
  i.per.f = NULL,
  # ACO Optimization Parameters
  ants = 90, max.iter = 50, search_patience = NULL,
  esem_every = 10, run_esem_during_search = TRUE,
  max_total_iter = NULL, max_esem_fits = NULL, evaporation = NULL,
  esem_weight = 0.50, esem_failure_policy = c("stop", "semantic_fallback"),
  esem_sample_size = "auto", elite_k = 10,
  esem_eval_top_k = NULL, fast_esem = TRUE, fast_esem_iter_max = 500L,
  full_esem_iter_max = 2000L,
  rotation = "geomin", rotation_args = list(geomin.epsilon = 0.50),
  data_type = "continuous", target_loadings = 0.70, target_factor_cors = NULL,
  dfi_reps = 500, dfi_level = 1, dfi_criterion = "Sensitivity",
  dfi_mode = c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi", "strict_cfa_dfi", "heuristic_semantic"),
  dfi_esem_reps = NULL, dfi_search_reps = NULL,
  final_dfi_recalibrate = FALSE, final_dfi_reps = NULL,
  dfi_roc_misspec_strength = 1.0,
  dfi_esem_strategy = c("fixed", "adaptive"),
  dfi_adaptive_min_reps = NULL, dfi_adaptive_batch_reps = 50L,
  dfi_adaptive_tol = 0.002, dfi_adaptive_stable_batches = 2L,
  dfi_fallback_policy = c("conservative", "requested_only"),
  final_dddfi = TRUE, final_dddfi_reps = 250L,
  final_dddfi_mad_target = c("close", "fair", "mediocre"),
  final_equivtest = TRUE,
  loading_pattern = "varied", embed_reliability = 1.0, residual_inflation = 0.0,
  dfi_warmup_iters = 5L, redundancy_threshold = 0.85, dup_threshold = 0.90,
  htmt_threshold = 0.85, cohesion_quantile = NULL, cohesion_retention = 0.75,
  within_similarity_target = NULL, within_similarity_band = 0.08,
  semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
  facet_coverage_weight = 0.15, psychometric_guard_weight = 0.50,
  psychometric_guard_min_ave = 0.30,
  psychometric_guard_min_loading = 0.40,
  psychometric_guard_min_primary_ge_50 = 0.70,
  semantic_threshold_mode = c("fixed", "adaptive_pool"),
  adaptive_redundancy_quantile = 0.95,
  adaptive_duplicate_quantile = 0.99,
  construct_blueprint = NULL,
  nomological_weight = 0,
  polarity_screen = TRUE,
  pfa_mode = c("diagnostic", "objective", "off"),
  pfa_weight = 0.20,
  pfa_failure_policy = c("semantic_fallback", "penalize", "stop"),
  run_pfa_during_search = TRUE,
  pfa_every = 1L,
  pfa_extraction = c("principal", "ml"),
  pfa_final_extraction = c("ml", "principal"),
  pfa_rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
  pfa_min_loading = psychometric_guard_min_loading,
  pfa_min_margin = NULL,
  pfa_unit_diagnostics = TRUE,
  reference_rmsea_close = 0.05,
  reference_rmsea_poor = 0.06,
  reference_power = 0.80,
  reference_alpha = 0.05,
  reference_max_n = Inf,
  semantic_n_sensitivity = TRUE,
  semantic_n_grid = NULL,
  semantic_n_multipliers = c(0.5, 1, 1.5, 2),
  semantic_n_iter_max = 800L,
  semantic_esem_score_mode = c("current", "structure_weighted"),
  validation_n_diagnostic = FALSE,
  validation_n_reps = 20L,
  validation_n_grid = NULL,
  validation_n_max = 2000L,
  validation_n_convergence = 0.90,
  validation_n_max_heywood = 0.05,
  validation_n_min_recovery = 0.90,
  validation_n_max_loading_error = 0.10,
  validation_n_min_dominance = NULL,
  validation_n_max_cross_error = NULL,
  validation_n_max_factor_cor_error = NULL,
  sigmoid_center = 0.15,
  elite_pareto_rerank = NULL,
  validation_data = NULL, validation_ordered = NULL,
  sigmoid_steepness = 10, heuristic_beta = 0.50, archive_stable_window = 8L,
  structural_archive_stable_window = 2L,
  min_successful_pfa_checkpoints = 2L, min_successful_esem_checkpoints = 2L,
  pheromone_update = c("top_elite", "best_ant"), fixed_evaporation = NULL,
  debug_mode = FALSE, keep_solution_history = TRUE,
  history_mode = c("summary", "full", "none"), use_parallel = TRUE,
  n.cores = 2L, reserve.cores = 1L, max.cores = NULL, seed = NULL,
  # Visualization Parameters
  generate_plots = TRUE, interactive_mode = c("2d", "3d"),
  save_plots = FALSE, plot_out_dir = "semantica_plots/",
  plot_device = "png", plot_width = 12, plot_height = 8, plot_dpi = 180,
  plot_before_path_model = c("proxy", "refit"),
  plot_before_path_refit_max_items = 60L,
  plot_network_max_items = 200L, plot_mds_max_items = 250L,
  plot_path_proxy_max_items = 150L, include_interactive_plot = TRUE,
  plot_progress = verbose,
  # Misc
  verbose = TRUE,
  embedding_task = "auto", embedding_instruction = NULL,
  content_alignment_mode = c("diagnostic", "guard", "off"),
  polarity_action = c("diagnostic", "guard", "off"),
  within_target_method = c("nonredundant_median", "legacy_q40"),
  validation_n_on_inadmissible = c("skip", "run"), ...,
  backend_spec = NULL, embed_backend_spec = NULL, embed_base_url = NULL,
  esem_cadence_mode = c("adaptive", "fixed"), generation_seed = NULL,
  htmt_objective_role = c("diagnostic", "penalty"),
  elite_multicriteria_rerank = NULL
) {
  max_iter_explicit <- !missing(max.iter)
  if (!max_iter_explicit && !is.null(search_patience)) max.iter <- search_patience

  full_pipeline_started <- proc.time()[["elapsed"]]
  pheromone_update <- match.arg(pheromone_update)
  interactive_mode <- match.arg(interactive_mode)
  dfi_mode <- match.arg(dfi_mode)
  dfi_esem_strategy <- match.arg(dfi_esem_strategy)
  dfi_fallback_policy <- match.arg(dfi_fallback_policy)
  final_dddfi_mad_target <- match.arg(final_dddfi_mad_target)
  pfa_mode <- match.arg(pfa_mode)
  pfa_failure_policy <- match.arg(pfa_failure_policy)
  pfa_extraction <- match.arg(pfa_extraction)
  pfa_final_extraction <- match.arg(pfa_final_extraction)
  pfa_rotation <- match.arg(pfa_rotation)
  pfa_every <- suppressWarnings(as.integer(pfa_every[1L]))
  if (length(pfa_every) != 1L || !is.finite(pfa_every) || pfa_every < 1L) {
    stop("'pfa_every' must be a positive integer.")
  }
  cosine_adjustment <- match.arg(cosine_adjustment)
  esem_cadence_mode <- match.arg(esem_cadence_mode)
  semantic_esem_score_mode <- match.arg(semantic_esem_score_mode)
  semantic_threshold_mode <- match.arg(semantic_threshold_mode)
  content_alignment_mode <- match.arg(content_alignment_mode)
  polarity_action <- match.arg(polarity_action)
  within_target_method <- match.arg(within_target_method)
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  htmt_objective_role <- match.arg(htmt_objective_role)
  validation_n_on_inadmissible <- match.arg(validation_n_on_inadmissible)
  if (!is.null(elite_multicriteria_rerank) && !is.null(elite_pareto_rerank)) {
    stop("Supply only 'elite_multicriteria_rerank'; 'elite_pareto_rerank' is a deprecated compatibility alias.", call. = FALSE)
  }
  if (is.null(elite_multicriteria_rerank)) {
    elite_multicriteria_rerank <- if (is.null(elite_pareto_rerank)) TRUE else elite_pareto_rerank
  }
  elite_multicriteria_rerank <- .semantica_assert_flag(elite_multicriteria_rerank, "elite_multicriteria_rerank")
  if (!is.null(elite_pareto_rerank)) {
    warning("'elite_pareto_rerank' is deprecated because the procedure is scalar multicriteria reranking, not Pareto dominance; use 'elite_multicriteria_rerank'.", call. = FALSE)
  }
  nomological_weight <- suppressWarnings(as.numeric(nomological_weight[1L]))
  if (length(nomological_weight) != 1L || !is.finite(nomological_weight) || nomological_weight < 0 || nomological_weight > 1) {
    stop("'nomological_weight' must be a finite number in [0, 1].")
  }
  esem_failure_policy <- match.arg(esem_failure_policy)
  history_mode <- match.arg(history_mode)
  plot_before_path_model <- match.arg(plot_before_path_model)
  resource_plan_preview <- semantica_resource_plan(
    n.cores = n.cores,
    use_parallel = use_parallel,
    reserve.cores = reserve.cores,
    max.cores = max.cores
  )
  if (verbose) {
    cat("\n[SEMANTICA] Resolved resource plan before expensive execution:\n")
    print(resource_plan_preview)
    cat(sprintf(
      "  Cosine device       : %s (lavaan remains on CPU)\n",
      compute_device
    ))
  }
  dots <- list(...)
  if (is.null(generation_seed)) generation_seed <- seed
  generation_seed <- .semantica_normalize_generation_seed(generation_seed)
  polarity_language <- dots$language %||% "auto"
  if (isTRUE(pfa_unit_diagnostics) && !isTRUE(retain_embeddings)) {
    warning(
      "'pfa_unit_diagnostics = TRUE' requires retained embeddings; facet/unit PFA will be unavailable when 'retain_embeddings = FALSE'.",
      call. = FALSE
    )
  }

  # Backward-compatible aliases for earlier SEMANTICA versions.
  # They are consumed here so generation helpers do not receive ACO-only args.
  if (!is.null(dots$cfa_every)) {
    esem_every <- dots$cfa_every
    dots$cfa_every <- NULL
  }
  if (!is.null(dots$cfa_weight)) {
    esem_weight <- dots$cfa_weight
    dots$cfa_weight <- NULL
  }
  if (!is.null(dots$cfa_sample_size)) {
    esem_sample_size <- dots$cfa_sample_size
    dots$cfa_sample_size <- NULL
  }

  # Default i.per.f if not provided
  if (is.null(i.per.f)) {
    i.per.f <- setNames(rep(3L, length(factors)), names(factors))
    if (verbose) message("[FULL PIPELINE] i.per.f not specified. Defaulting to 3 items per factor.")
  }

  blueprint_eff <- if (is.null(construct_blueprint)) {
    semantica_construct_blueprint(factors)
  } else if (inherits(construct_blueprint, "semantica_construct_blueprint")) {
    construct_blueprint
  } else {
    stop("'construct_blueprint' must be NULL or created by semantica_construct_blueprint().")
  }

  # =================================================================
  # STEP 1: LLM Generation & Semantic Embedding
  # =================================================================
  if (verbose) cat("\n[SEMANTICA] Step 1/3: LLM Generation & Semantic Embedding...\n")
  gen_res <- do.call(
    semantica_pipeline,
    c(
      list(
        backend = backend, embed_backend = embed_backend, api_key = api_key,
        embed_api_key = embed_api_key, chat_model = chat_model, embed_model = embed_model,
        backend_spec = backend_spec, embed_backend_spec = embed_backend_spec,
        hf_token = hf_token, embed_hf_token = embed_hf_token,
        embedding_device = embedding_device, chat_device = chat_device,
        device_map = device_map, gpu_layers = gpu_layers,
        model_precision = model_precision,
        embed_batch_size = embed_batch_size,
        embedding_cache = embedding_cache,
        embedding_cache_dir = embedding_cache_dir,
        embedding_cache_namespace = embedding_cache_namespace,
        retry_max_tries = retry_max_tries,
        retry_on_failure = retry_on_failure,
        preflight = preflight, embedding_task = embedding_task,
        embedding_instruction = embedding_instruction,
        embedding_spec = embedding_spec, content_alignment = !identical(content_alignment_mode, "off"),
        content_exclusions = blueprint_eff$exclusions,
        base_url = base_url, embed_base_url = embed_base_url, gguf_path = gguf_path, scale_name = scale_name,
        scale_description = scale_description, factors = factors, n_per_factor = n_per_factor,
        n_per_factor_override = n_per_factor_override,
        cosine_adjustment = cosine_adjustment, semantic_calibration = semantic_calibration,
        compute_cosine_sensitivity = compute_cosine_sensitivity,
        cosine_sensitivity_max_items = cosine_sensitivity_max_items,
        cosine_sensitivity_seed = cosine_sensitivity_seed,
        compute_device = compute_device, gpu_fallback = gpu_fallback,
        gpu_precision = gpu_precision,
        compute_memory_limit = compute_memory_limit,
        release_local_models = release_local_models,
        retain_embeddings = retain_embeddings,
        generation_seed = generation_seed,
        verbose = verbose
      ),
      dots
    )
  )

  # -----------------------------------------------------------------
  # Method safeguards / research diagnostics before optimization
  # -----------------------------------------------------------------
  threshold_calibration <- NULL
  df_cal <- gen_res$df
  factor_col_cal <- if ("factor" %in% names(df_cal)) "factor" else if ("type" %in% names(df_cal)) "type" else NULL
  id_col_cal <- if ("item" %in% names(df_cal)) "item" else if ("item_id" %in% names(df_cal)) "item_id" else NULL
  fa_cal <- NULL
  if (!is.null(factor_col_cal) && !is.null(id_col_cal)) {
    fa_cal <- as.character(df_cal[[factor_col_cal]])
    names(fa_cal) <- as.character(df_cal[[id_col_cal]])
  }
  if (identical(semantic_threshold_mode, "adaptive_pool")) {
    threshold_calibration <- semantica_calibrate_similarity_thresholds(
      gen_res$cosine_sim_matrix,
      factor_assignment = fa_cal,
      redundancy_quantile = adaptive_redundancy_quantile,
      duplicate_quantile = adaptive_duplicate_quantile
    )
    redundancy_threshold <- threshold_calibration$redundancy_threshold
    dup_threshold <- threshold_calibration$duplicate_threshold
    if (is.null(within_similarity_target) && !is.null(threshold_calibration$within_similarity_target)) {
      within_similarity_target <- threshold_calibration$within_similarity_target
    }
    if (verbose) {
      cat(sprintf("[SEMANTICA] Experimental pool-relative thresholds: redundancy %.3f | duplicate %.3f\n",
                  redundancy_threshold, dup_threshold))
    }
  }

  construct_coverage_pool <- tryCatch(
    semantica_assess_construct_coverage(
      gen_res$items_tbl_raw %||% gen_res$items_tbl,
      blueprint_eff,
      cosine_sim_matrix = gen_res$cosine_sim_matrix
    ),
    error = function(e) list(available = FALSE, note = conditionMessage(e))
  )
  polarity_pool_eval <- .semantica_optional_diagnostic(
    function() semantica_polarity_diagnostics(
      gen_res$items_tbl_raw %||% gen_res$items_tbl, language = polarity_language
    ),
    requested = polarity_screen,
    applicable = TRUE
  )
  polarity_diagnostics_pool <- polarity_pool_eval$value
  polarity_diagnostics_pool_status <- polarity_pool_eval$status
  if (is.data.frame(polarity_diagnostics_pool)) {
    pd <- polarity_diagnostics_pool
    flag <- if ("explicit_negation" %in% names(pd)) as.logical(pd$explicit_negation) else
      if ("flagged" %in% names(pd)) as.logical(pd$flagged) else rep(FALSE, nrow(pd))
    if ("item_index" %in% names(pd) && length(flag) == nrow(gen_res$df)) {
      gen_res$df$semantica_polarity_flag <- flag[order(as.integer(pd$item_index))]
    } else if (nrow(pd) == nrow(gen_res$df)) {
      gen_res$df$semantica_polarity_flag <- flag
    }
  }

  # =================================================================
  # STEP 2: ACO-ESEM Optimization with DFI Calibration
  # =================================================================
  if (verbose) cat("\n[SEMANTICA] Step 2/3: ACO-ESEM Optimization...\n")
  aco_res <- ACO_with_ESEM(
    cosine_sim_matrix = gen_res$cosine_sim_matrix, df = gen_res$df,
    i.per.f = i.per.f, ants = ants, max.iter = max.iter,
    search_patience = search_patience, evaporation = evaporation, esem_every = esem_every,
    esem_cadence_mode = esem_cadence_mode,
    run_esem_during_search = run_esem_during_search, esem_weight = esem_weight,
    max_total_iter = max_total_iter, max_esem_fits = max_esem_fits,
    esem_failure_policy = esem_failure_policy,
    esem_sample_size = esem_sample_size, elite_k = elite_k, rotation = rotation,
    esem_eval_top_k = esem_eval_top_k, fast_esem = fast_esem,
    fast_esem_iter_max = fast_esem_iter_max, full_esem_iter_max = full_esem_iter_max,
    rotation_args = rotation_args, data_type = data_type, target_loadings = target_loadings,
    target_factor_cors = target_factor_cors, dfi_reps = dfi_reps, dfi_level = dfi_level,
    dfi_criterion = dfi_criterion, dfi_mode = dfi_mode,
    dfi_esem_reps = dfi_esem_reps,
    dfi_search_reps = dfi_search_reps,
    final_dfi_recalibrate = final_dfi_recalibrate,
    final_dfi_reps = final_dfi_reps,
    dfi_roc_misspec_strength = dfi_roc_misspec_strength,
    dfi_esem_strategy = dfi_esem_strategy,
    dfi_adaptive_min_reps = dfi_adaptive_min_reps,
    dfi_adaptive_batch_reps = dfi_adaptive_batch_reps,
    dfi_adaptive_tol = dfi_adaptive_tol,
    dfi_adaptive_stable_batches = dfi_adaptive_stable_batches,
    dfi_fallback_policy = dfi_fallback_policy,
    final_dddfi = final_dddfi,
    final_dddfi_reps = final_dddfi_reps,
    final_dddfi_mad_target = final_dddfi_mad_target,
    final_equivtest = final_equivtest,
    loading_pattern = loading_pattern,
    embed_reliability = embed_reliability, residual_inflation = residual_inflation,
    dfi_warmup_iters = dfi_warmup_iters, redundancy_threshold = redundancy_threshold,
    dup_threshold = dup_threshold, htmt_threshold = htmt_threshold,
    htmt_objective_role = htmt_objective_role,
    cohesion_quantile = cohesion_quantile,
    cohesion_retention = cohesion_retention,
    within_similarity_target = within_similarity_target,
    within_similarity_band = within_similarity_band,
    semantic_objective_mode = semantic_objective_mode,
    expected_factor_relations = blueprint_eff$expected_relations %||% NULL,
    nomological_weight = nomological_weight,
    content_alignment_mode = content_alignment_mode,
    polarity_action = polarity_action,
    within_target_method = within_target_method,
    validation_n_on_inadmissible = validation_n_on_inadmissible,
    facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode,
    pfa_weight = pfa_weight,
    pfa_failure_policy = pfa_failure_policy,
    run_pfa_during_search = run_pfa_during_search,
    pfa_every = pfa_every,
    pfa_extraction = pfa_extraction,
    pfa_final_extraction = pfa_final_extraction,
    pfa_rotation = pfa_rotation,
    pfa_min_loading = pfa_min_loading,
    pfa_min_margin = pfa_min_margin,
    reference_rmsea_close = reference_rmsea_close,
    reference_rmsea_poor = reference_rmsea_poor,
    reference_power = reference_power,
    reference_alpha = reference_alpha,
    reference_max_n = reference_max_n,
    semantic_n_sensitivity = semantic_n_sensitivity,
    semantic_n_grid = semantic_n_grid,
    semantic_n_multipliers = semantic_n_multipliers,
    semantic_n_iter_max = semantic_n_iter_max,
    semantic_esem_score_mode = semantic_esem_score_mode,
    validation_n_diagnostic = validation_n_diagnostic,
    validation_n_reps = validation_n_reps,
    validation_n_grid = validation_n_grid,
    validation_n_max = validation_n_max,
    validation_n_convergence = validation_n_convergence,
    validation_n_max_heywood = validation_n_max_heywood,
    validation_n_min_recovery = validation_n_min_recovery,
    validation_n_max_loading_error = validation_n_max_loading_error,
    validation_n_min_dominance = validation_n_min_dominance,
    validation_n_max_cross_error = validation_n_max_cross_error,
    validation_n_max_factor_cor_error = validation_n_max_factor_cor_error,
    sigmoid_center = sigmoid_center,
    elite_multicriteria_rerank = elite_multicriteria_rerank,
    validation_data = validation_data,
    validation_ordered = validation_ordered,
    sigmoid_steepness = sigmoid_steepness, heuristic_beta = heuristic_beta,
    archive_stable_window = archive_stable_window,
    structural_archive_stable_window = structural_archive_stable_window,
    min_successful_pfa_checkpoints = min_successful_pfa_checkpoints,
    min_successful_esem_checkpoints = min_successful_esem_checkpoints,
    pheromone_update = pheromone_update,
    fixed_evaporation = fixed_evaporation, debug_mode = debug_mode,
    keep_solution_history = keep_solution_history, history_mode = history_mode,
    use_parallel = use_parallel,
    n.cores = n.cores, reserve.cores = reserve.cores,
    max.cores = max.cores, seed = seed, verbose = verbose
  )

  # ---- Compute semantic similarity reduction metric ----
  # The baseline is the full generated pool, not the ACO eligible subset.
  gen_df <- gen_res$df
  factor_col <- if (!is.null(gen_df) && "factor" %in% names(gen_df)) "factor" else if (!is.null(gen_df) && "type" %in% names(gen_df)) "type" else NULL
  id_col <- if (!is.null(gen_df) && "item" %in% names(gen_df)) "item" else if (!is.null(gen_df) && "item_id" %in% names(gen_df)) "item_id" else NULL
  pool_ids <- if (!is.null(gen_df) && !is.null(id_col)) as.character(gen_df[[id_col]]) else rownames(gen_res$cosine_sim_matrix)
  pool_items <- intersect(pool_ids, rownames(gen_res$cosine_sim_matrix))
  fa_pool <- character(length(pool_items))
  names(fa_pool) <- pool_items
  if (!is.null(gen_df) && !is.null(factor_col)) {
    factor_values <- as.character(gen_df[[factor_col]])
    names(factor_values) <- pool_ids
    fa_pool <- factor_values[pool_items]
  }
  fa_pool[aco_res$best_items] <- aco_res$factor_assignment

  # Local helper to compute mean cosine similarity within or between factors.
  .mean_pairwise_sim <- function(cos_mat, items, fa, f_names, type = c("within", "between")) {
    type <- match.arg(type)
    sim_blocks <- vector("list", length(f_names))
    n_blocks <- 0L
    if (type == "within") {
      for (f in f_names) {
        f_items <- names(fa[fa == f])
        f_items <- intersect(f_items, items)
        if (length(f_items) < 2L) next
        sub <- cos_mat[f_items, f_items, drop = FALSE]
        n_blocks <- n_blocks + 1L
        sim_blocks[[n_blocks]] <- sub[lower.tri(sub)]
      }
    } else if (length(f_names) >= 2L) {
      for (i in seq_len(length(f_names) - 1L)) {
        for (j in (i + 1L):length(f_names)) {
          i_items <- intersect(names(fa[fa == f_names[i]]), items)
          j_items <- intersect(names(fa[fa == f_names[j]]), items)
          if (length(i_items) == 0L || length(j_items) == 0L) next
          n_blocks <- n_blocks + 1L
          sim_blocks[[n_blocks]] <- as.vector(cos_mat[i_items, j_items, drop = FALSE])
        }
      }
    }
    if (n_blocks == 0L) return(NA_real_)
    sims <- unlist(sim_blocks[seq_len(n_blocks)], use.names = FALSE)
    mean(sims, na.rm = TRUE)
  }

  within_before <- .mean_pairwise_sim(
    cos_mat = gen_res$cosine_sim_matrix,
    items   = pool_items,
    fa      = fa_pool,
    f_names = unique(fa_pool[fa_pool != ""]),
    type    = "within"
  )

  within_after <- .mean_pairwise_sim(
    cos_mat = gen_res$cosine_sim_matrix,
    items   = aco_res$best_items,
    fa      = aco_res$factor_assignment,
    f_names = unique(aco_res$factor_assignment),
    type    = "within"
  )

  between_before <- .mean_pairwise_sim(
    cos_mat = gen_res$cosine_sim_matrix,
    items   = pool_items,
    fa      = fa_pool,
    f_names = unique(fa_pool[fa_pool != ""]),
    type    = "between"
  )

  between_after <- .mean_pairwise_sim(
    cos_mat = gen_res$cosine_sim_matrix,
    items   = aco_res$best_items,
    fa      = aco_res$factor_assignment,
    f_names = unique(aco_res$factor_assignment),
    type    = "between"
  )

  reduction <- if (!is.na(within_before) && !is.na(within_after)) within_before - within_after else NA_real_
  percent_reduction <- if (!is.na(reduction) && !is.na(within_before) && within_before > 0) {
    100 * reduction / within_before
  } else NA_real_
  between_reduction <- if (!is.na(between_before) && !is.na(between_after)) between_before - between_after else NA_real_
  sem_index_before <- mean(c(within_before, between_before), na.rm = TRUE)
  sem_index_after <- mean(c(within_after, between_after), na.rm = TRUE)
  sem_index_reduction <- sem_index_before - sem_index_after
  target_eff <- aco_res$within_similarity_target %||% within_similarity_target
  target_vals <- if (!is.null(target_eff)) as.numeric(target_eff) else numeric(0L)
  target_vals <- target_vals[is.finite(target_vals)]
  target_value <- if (length(target_vals) > 0L) mean(target_vals) else NA_real_
  target_dev_before <- if (is.finite(target_value) && is.finite(within_before)) abs(within_before - target_value) else NA_real_
  target_dev_after <- if (is.finite(target_value) && is.finite(within_after)) abs(within_after - target_value) else NA_real_
  target_improvement <- if (is.finite(target_dev_before) && is.finite(target_dev_after)) target_dev_before - target_dev_after else NA_real_
  target_band_status <- if (is.finite(target_value) && is.finite(within_after)) {
    if (abs(within_after - target_value) <= within_similarity_band) "inside target band" else "outside target band"
  } else NA_character_
  separation_gap_before <- if (is.finite(within_before) && is.finite(between_before)) within_before - between_before else NA_real_
  separation_gap_after <- if (is.finite(within_after) && is.finite(between_after)) within_after - between_after else NA_real_
  separation_gap_change <- if (is.finite(separation_gap_before) && is.finite(separation_gap_after)) separation_gap_after - separation_gap_before else NA_real_
  sem_reduction_interpretation <- if (is.finite(separation_gap_change)) {
    direction <- if (separation_gap_change > 0) "improved" else if (separation_gap_change < 0) "weakened" else "was unchanged"
    guard_txt <- if (!is.na(target_band_status)) paste0("; within-factor cohesion remained ", target_band_status) else ""
    paste0("Relative within-versus-between semantic separation ", direction, guard_txt, ".")
  } else if (!is.na(sem_index_reduction)) {
    "Relative separation could not be computed; inspect the available cohesion diagnostics."
  } else {
    "Could not compute (insufficient items)"
  }

  if (verbose && is.null(aco_res$semantic_similarity_reduction)) {
    cat("\n[SEMANTICA] Semantic Similarity Reduction Summary:\n")
    cat(sprintf("  Within-factor : %.4f -> %.4f | reduction = %.4f",
                within_before, within_after, reduction))
    if (!is.na(percent_reduction)) cat(sprintf(" (%.2f%%)", percent_reduction))
    cat("\n")
    if (is.finite(target_value)) {
      cat(sprintf("  Within target : %.4f +/- %.4f | deviation %.4f -> %.4f | %s\n",
                  target_value, within_similarity_band, target_dev_before, target_dev_after,
                  target_band_status))
    }
    cat(sprintf("  Between-factor: %.4f -> %.4f | reduction = %.4f\n",
                between_before, between_after, between_reduction))
    if (is.finite(separation_gap_before) && is.finite(separation_gap_after)) {
      cat(sprintf("  Separation gap: %.4f -> %.4f | change = %+.4f\n",
                  separation_gap_before, separation_gap_after, separation_gap_change))
    }
    cat(sprintf("  Composite index: %.4f -> %.4f | reduction = %.4f\n",
                sem_index_before, sem_index_after, sem_index_reduction))
    cat(sprintf("  Interpretation: %s\n", sem_reduction_interpretation))
  }

  # ---- Build standardized selected item metadata ----
  selected_item_metadata <- aco_res$selected_item_metadata
  generated_item_metadata <- gen_res$generated_item_metadata %||% gen_res$item_metadata
  if (is.null(selected_item_metadata) && exists("semantica_standardize_item_metadata", mode = "function")) {
    selected_item_metadata <- tryCatch({
      meta_all <- semantica_standardize_item_metadata(gen_df)
      idx <- match(aco_res$best_items, meta_all$ID)
      if (anyNA(idx)) stop("Selected item IDs are missing from generated metadata.")
      meta_all[idx, c("ID", "Dimension", "Facet", "item"), drop = FALSE]
    }, error = function(e) NULL)
  }
  if (!is.null(selected_item_metadata) && !is.null(gen_res$items_tbl_raw)) {
    raw_meta <- as.data.frame(gen_res$items_tbl_raw, stringsAsFactors = FALSE)
    raw_id <- if ("item_id" %in% names(raw_meta)) as.character(raw_meta$item_id) else if ("ID" %in% names(raw_meta)) as.character(raw_meta$ID) else NULL
    sel_id <- if ("ID" %in% names(selected_item_metadata)) as.character(selected_item_metadata$ID) else NULL
    extra_cols <- grep("^semantica_", names(raw_meta), value = TRUE)
    if (!is.null(raw_id) && !is.null(sel_id) && length(extra_cols)) {
      mi <- match(sel_id, raw_id)
      for (cc in extra_cols) selected_item_metadata[[cc]] <- raw_meta[[cc]][mi]
    }
    if ("semantica_polarity_flag" %in% names(gen_res$df)) {
      dfi <- match(sel_id, as.character(gen_res$df$item))
      selected_item_metadata$semantica_polarity_flag <- gen_res$df$semantica_polarity_flag[dfi]
    }
  }

  semantic_cluster_consensus <- list(pool = NULL, selected = NULL)
  if (!is.null(generated_item_metadata) && all(c("ID", "Dimension") %in% names(generated_item_metadata))) {
    pool_fa <- stats::setNames(as.character(generated_item_metadata$Dimension), as.character(generated_item_metadata$ID))
    semantic_cluster_consensus$pool <- tryCatch(
      semantica_cluster_consensus(gen_res$cosine_sim_matrix, pool_fa),
      error = function(e) list(available = FALSE, status = "unavailable", note = conditionMessage(e), evidence_family = "embedding_semantic")
    )
  }
  if (!is.null(aco_res$best_items) && !is.null(aco_res$factor_assignment)) {
    sel_ids <- intersect(as.character(aco_res$best_items), rownames(gen_res$cosine_sim_matrix))
    sel_fa <- aco_res$factor_assignment[sel_ids]
    if (length(sel_ids) >= 2L && length(sel_fa) == length(sel_ids)) {
      semantic_cluster_consensus$selected <- tryCatch(
        semantica_cluster_consensus(gen_res$cosine_sim_matrix[sel_ids, sel_ids, drop = FALSE], sel_fa),
        error = function(e) list(available = FALSE, status = "unavailable", note = conditionMessage(e), evidence_family = "embedding_semantic")
      )
    }
  }

  construct_coverage_selected <- tryCatch({
    if (is.null(selected_item_metadata)) stop("Selected item metadata unavailable.")
    semantica_assess_construct_coverage(
      selected_item_metadata,
      blueprint_eff,
      cosine_sim_matrix = gen_res$cosine_sim_matrix
    )
  }, error = function(e) list(available = FALSE, note = conditionMessage(e)))
  polarity_selected_eval <- .semantica_optional_diagnostic(
    function() semantica_polarity_diagnostics(selected_item_metadata, text_col = "item"),
    requested = polarity_screen,
    applicable = !is.null(selected_item_metadata)
  )
  polarity_diagnostics_selected <- polarity_selected_eval$value
  polarity_diagnostics_selected_status <- polarity_selected_eval$status
  matrix_repair_diagnostics <- tryCatch({
    selected_cos <- gen_res$cosine_sim_matrix[aco_res$best_items, aco_res$best_items, drop = FALSE]
    semantica_matrix_repair_diagnostics(
      selected_cos,
      factor_assignment = aco_res$factor_assignment,
      factors = unique(as.character(aco_res$factor_assignment))
    )
  }, error = function(e) list(available = FALSE, note = conditionMessage(e)))

  pfa_unit_result <- NULL
  if (isTRUE(pfa_unit_diagnostics) && pfa_mode != "off" &&
      exists("compute_pfa_unit_diagnostics", mode = "function")) {
    embeddings <- NULL
    if (!is.null(gen_res$embed_result) && !is.null(gen_res$embed_result$embeddings)) {
      embeddings <- gen_res$embed_result$embeddings
    } else if (!is.null(gen_res$embeddings)) {
      embeddings <- gen_res$embeddings
    }
    pfa_unit_result <- tryCatch({
      compute_pfa_unit_diagnostics(
        embeddings = embeddings,
        item_metadata = generated_item_metadata,
        factors = names(i.per.f),
        extraction = pfa_final_extraction,
        rotation = pfa_rotation,
        min_loading = pfa_min_loading,
        min_margin = pfa_min_margin
      )
    }, error = function(e) list(
      available = FALSE, score = 0,
      note = paste("Facet/unit-level PFA failed:", conditionMessage(e))
    ))
    if (verbose && !is.null(pfa_unit_result)) {
      if (isTRUE(pfa_unit_result$available)) {
        cat(sprintf("\n[SEMANTICA] Facet/unit PFA: score=%.4f | recovery=%.4f | clarity=%.4f\n",
                    pfa_unit_result$score,
                    pfa_unit_result$recovery_score,
                    pfa_unit_result$clarity_score))
      } else {
        unit_status <- if (isTRUE(pfa_unit_result$skipped)) "skipped" else "unavailable"
        cat(sprintf("\n[SEMANTICA] Facet/unit PFA %s: %s\n",
                    unit_status,
                    pfa_unit_result$note %||% "insufficient facet/unit structure"))
      }
    }
  }

  if (verbose && !is.null(gen_res$generation_provenance)) {
    gp <- gen_res$generation_provenance
    cat("\n[SEMANTICA] Generation reproducibility/provenance:\n")
    seed_txt <- gp$generation_seed_master %||% NA_integer_
    if (is.finite(seed_txt)) {
      cat(sprintf("  Generation master seed : %d\n", as.integer(seed_txt)))
      cat(sprintf("  Backend seed control   : %s (%s)\n",
                  if (isTRUE(gp$generation_seed_controlled)) "requested" else "not controlled",
                  gp$generation_seed_mechanism %||% "unavailable"))
    } else {
      cat("  Generation master seed : not requested\n")
    }
    if (!is.null(gp$generation_seed_schedule)) {
      cat(sprintf("  Seed schedule          : %d realized call(s)\n", nrow(gp$generation_seed_schedule)))
    } else if (!is.null(gp$task_seed_ledger)) {
      cat(sprintf("  Generation call ledger : %d call(s)\n", nrow(gp$task_seed_ledger)))
    }
    if (isTRUE(gp$generation_seed_controlled)) {
      cat("  Exact text replay      : not guaranteed by backend/runtime; reuse the realized item pool for exact downstream replay\n")
    }
    replay_fp <- gp$generation_replay_plan_fingerprint %||% NA_character_
    if (is.character(replay_fp) && length(replay_fp) == 1L &&
        !is.na(replay_fp) && nzchar(replay_fp)) {
      cat(sprintf("  Replay-plan fingerprint: %s\n", replay_fp))
    }
    if (!is.null(gp$item_pool_fingerprint) && nzchar(gp$item_pool_fingerprint)) {
      cat(sprintf("  Item-pool fingerprint  : %s\n", gp$item_pool_fingerprint))
    }
    cat(sprintf("  Content-screening state: %s\n",
                gp$content_screening_status %||% "unknown"))
  }

  if (verbose && !is.null(gen_res$representation_stability)) {
    rs <- gen_res$representation_stability
    sens <- rs$cosine_adjustment_sensitivity %||% list()
    cat("\n[SEMANTICA] Representation robustness summary:\n")
    rep_state <- gen_res$representation_evidence_state %||% NULL
    if (!is.null(rep_state)) {
      cat(sprintf("  Evidence state: %s\n", rep_state$status %||% "insufficient_representation_diagnostics"))
      cat("  Evidence dependency: semantic/PFA/ESEM/clustering conclusions share this embedding representation; agreement is dependent corroboration, not independent validation.\n")
    }
    if (is.finite(rs$common_direction_strength %||% NA_real_)) {
      cat(sprintf("  Common-direction concentration: %.4f (descriptive; no universal cutoff)\n",
                  rs$common_direction_strength))
    }
    if (isTRUE(sens$available)) {
      cat(sprintf("  Raw vs mean-centered cosine: offdiag r=%.3f | top %.1f%% pair J=%.3f | random ref=%.3f\n",
                  sens$offdiag_correlation %||% NA_real_,
                  100 * (sens$top_pair_fraction_effective %||% sens$top_pair_fraction %||% 0.05),
                  sens$top_pair_jaccard %||% NA_real_,
                  sens$top_pair_jaccard_random_baseline %||% NA_real_))
      if (isTRUE(rs$top_pair_overlap_warning)) {
        cat("  Interpretation: WARNING -- strongest-pair membership is not more stable across preprocessing than its finite-pool random reference; replicate proxy conclusions under a justified alternative representation.\n")
      } else {
        cat("  Interpretation: sensitivity diagnostic only; SEMANTICA does not automatically choose a more favorable cosine preprocessing.\n")
      }
    }
  }

  # =================================================================
  # STEP 3: Diagnostic Visualization
  # =================================================================
  plots <- NULL
  if (generate_plots) {
    if (verbose) cat("\n[SEMANTICA] Step 3/3: Generating Diagnostic Plots...\n")
    plots <- semantica_plot_all(
      result = aco_res, cosine_sim_matrix = gen_res$cosine_sim_matrix,
      df = gen_res$df, interactive_mode = interactive_mode, save = save_plots,
      out_dir = plot_out_dir, device = plot_device, width = plot_width,
      height = plot_height, dpi = plot_dpi,
      before_path_model = plot_before_path_model,
      before_path_refit_max_items = plot_before_path_refit_max_items,
      network_max_items = plot_network_max_items,
      mds_max_items = plot_mds_max_items,
      path_proxy_max_items = plot_path_proxy_max_items,
      include_interactive = include_interactive_plot,
      progress = plot_progress
    )
  }

  # =================================================================
  # RETURN UNIFIED OBJECT
  # =================================================================
  if (verbose) cat("\n[SEMANTICA] Pipeline complete.\n")
  .metadata_value <- function(x, name) {
    if (is.null(x) || !is.list(x) || !name %in% names(x)) return(NULL)
    x[[name]]
  }
  combined_reproducibility <- aco_res$reproducibility %||% list()
  embedding_session_obj <- gen_res$embed_session %||% gen_res$session
  resolved_chat_model <- .metadata_value(gen_res$session, "chat_model") %||%
    chat_model %||% NA_character_
  resolved_embedding_model <- .metadata_value(embedding_session_obj, "embed_model") %||%
    .metadata_value(gen_res$embedding_diagnostics, "model") %||%
    embed_model %||% NA_character_
  chat_model_revision <- .metadata_value(gen_res$session, "model_revision") %||% NA_character_
  embedding_model_revision <- .metadata_value(embedding_session_obj, "model_revision") %||% NA_character_
  combined_reproducibility$models <- list(
    generation_backend = backend,
    embedding_backend = embed_backend %||% backend,
    chat_model = chat_model %||% resolved_chat_model,
    embedding_model = embed_model %||% resolved_embedding_model,
    requested_chat_model = chat_model %||% NA_character_,
    resolved_chat_model = resolved_chat_model,
    chat_model_revision = chat_model_revision,
    chat_model_identity_status = .semantica_model_identity_status(
      resolved_chat_model, chat_model_revision
    ),
    requested_embedding_model = embed_model %||% NA_character_,
    resolved_embedding_model = resolved_embedding_model,
    embedding_model_revision = embedding_model_revision,
    embedding_model_identity_status = .semantica_model_identity_status(
      resolved_embedding_model, embedding_model_revision
    ),
    embedding_device = .metadata_value(
      gen_res$embedding_diagnostics, "resolved_device"
    ) %||% embedding_device,
    compute_device = .metadata_value(
      gen_res$compute_telemetry, "resolved_device"
    ) %||% compute_device
  )
  combined_reproducibility$generation <- gen_res$generation_provenance %||% list(
    schema = "semantica-generation-provenance-unavailable"
  )
  combined_reproducibility$semantic_calibration <- gen_res$semantic_calibration
  combined_reproducibility$semantic_threshold_mode <- semantic_threshold_mode
  combined_reproducibility$threshold_calibration <- threshold_calibration
  combined_reproducibility$construct_blueprint <- blueprint_eff
  combined_reproducibility$cosine_adjustment <- cosine_adjustment
  combined_reproducibility$local_model_precision <- model_precision
  session_protocols <- c(
    .metadata_value(gen_res$session, "protocol"),
    .metadata_value(gen_res$embed_session, "protocol")
  )
  if (any(session_protocols %in% c("python_hf", "python_llamacpp"))) {
    python_caps <- .semantica_python_capabilities(deep_python = TRUE)
    combined_reproducibility$python <- list(
      checked = python_caps$checked,
      available = python_caps$available,
      version = python_caps$version,
      torch_version = python_caps$torch_version,
      cuda_available = python_caps$cuda_available,
      mps_available = python_caps$mps_available
    )
  }
  full_evidence_records <- aco_res$evidence_records %||% list()
  full_evidence_records$representation <- if (!is.null(gen_res$cosine_diagnostics)) {
    .semantica_evidence_record(
      "computed",
      value = list(
        cosine_diagnostics = gen_res$cosine_diagnostics,
        representation_stability = gen_res$representation_stability,
        representation_evidence_state = gen_res$representation_evidence_state,
        embedding_policy = gen_res$embedding_policy
      ),
      participant_based = FALSE, selection_conditioned = FALSE,
      evidence_scope = "generated candidate pool"
    )
  } else {
    .semantica_evidence_record(
      "unavailable", reason = "representation diagnostics unavailable",
      participant_based = FALSE, selection_conditioned = FALSE,
      evidence_scope = "generated candidate pool"
    )
  }
  full_evidence_records$participant_validation <- if (is.null(aco_res$response_validation)) {
    .semantica_evidence_record(
      "not_requested", participant_based = TRUE, selection_conditioned = TRUE,
      evidence_scope = "selected items"
    )
  } else if (isTRUE(aco_res$response_validation$result$converged)) {
    .semantica_evidence_record(
      "computed", value = aco_res$response_validation, participant_based = TRUE,
      selection_conditioned = TRUE, evidence_scope = "selected items"
    )
  } else {
    .semantica_evidence_record(
      "unavailable", value = aco_res$response_validation,
      reason = "participant validation was attempted but did not converge",
      participant_based = TRUE, selection_conditioned = TRUE,
      evidence_scope = "selected items"
    )
  }

  combined_performance <- list(
    generation = gen_res$performance,
    optimization = aco_res$performance,
    resource = aco_res$performance$resource %||% resource_plan_preview,
    compute = gen_res$compute_telemetry,
    total_seconds = unname(proc.time()[["elapsed"]] - full_pipeline_started)
  )
  out <- list(
    generation        = gen_res,
    optimization      = aco_res,
    plots             = plots,
    best_items        = aco_res$best_items,
    factor_assignment = aco_res$factor_assignment,
    best_objective    = aco_res$best_objective,
    objective_context = aco_res$objective_context,
    objective_schema = aco_res$objective_schema,
    evidence_records = full_evidence_records,
    dimensionality_mode = aco_res$dimensionality_mode %||% if (length(unique(as.character(aco_res$factor_assignment))) == 1L) "unidimensional" else "multidimensional",
    unidimensional_diagnostics = aco_res$unidimensional_diagnostics,
    selection_semantic_context = aco_res$selection_semantic_context,
    factor_semantic_diagnostics = aco_res$factor_semantic_diagnostics,
    esem_state = aco_res$esem_state,
    pfa_esem_discrepancy = aco_res$pfa_esem_discrepancy,
    generated_item_metadata = generated_item_metadata,
    selected_item_metadata = selected_item_metadata,
    item_structure_diagnostics = aco_res$item_structure_diagnostics,
    embedding_diagnostics = gen_res$embedding_diagnostics,
    embedding_policy = gen_res$embedding_policy,
    generation_provenance = gen_res$generation_provenance,
    content_alignment = gen_res$content_alignment,
    content_alignment_mode = content_alignment_mode,
    polarity_action = polarity_action,
    cosine_diagnostics = gen_res$cosine_diagnostics,
    cosine_adjustment_sensitivity = gen_res$cosine_adjustment_sensitivity,
    representation_stability = gen_res$representation_stability,
    representation_evidence_state = gen_res$representation_evidence_state,
    semantic_cluster_consensus = semantic_cluster_consensus,
    selection_guard_audit = aco_res$selection_guard_audit,
    pool_health = aco_res$pool_health,
    duplicate_feasibility = aco_res$duplicate_feasibility,
    evidence_archives = aco_res$evidence_archives,
    evidence_archive_states = aco_res$evidence_archive_states,
    semantic_calibration = gen_res$semantic_calibration,
    semantic_threshold_mode = semantic_threshold_mode,
    threshold_calibration = threshold_calibration,
    nomological_weight = nomological_weight,
    construct_blueprint = blueprint_eff,
    construct_coverage = construct_coverage_selected,
    construct_coverage_pool = construct_coverage_pool,
    polarity_diagnostics = polarity_diagnostics_selected,
    polarity_diagnostics_status = polarity_diagnostics_selected_status,
    polarity_diagnostics_pool = polarity_diagnostics_pool,
    polarity_diagnostics_pool_status = polarity_diagnostics_pool_status,
    matrix_repair_diagnostics = matrix_repair_diagnostics,
    participant_validation_performed = !is.null(aco_res$response_validation),
    participant_validation_converged = isTRUE(aco_res$response_validation$result$converged),
    interpretation_notice = paste(
      "Sample-free PFA/ESEM/DFI and semantic scores are proxy diagnostics for pre-data screening.",
      "They do not establish construct validity, reliability, measurement invariance, DIF, or criterion validity."
    ),
    fit_indices       = c(
      aco_res$esem_result[c("cfi", "rmsea", "srmr", "ave", "factor_ave", "ave_method", "ave_warnings", "htmt_max", "structure_diagnostics")],
      list(dfi_mode = aco_res$dfi_mode, cutoff_source = aco_res$cutoff_source,
           active_cutoffs = aco_res$active_cutoffs,
           search_cutoff_source = aco_res$search_cutoff_source,
           search_active_cutoffs = aco_res$search_active_cutoffs,
           reference_sample_size = aco_res$reference_sample_size,
           semantic_n_sensitivity = aco_res$semantic_n_sensitivity,
           recommended_validation_n = aco_res$recommended_validation_n,
           pfa_score = aco_res$pfa_score,
           pfa_diagnostics = aco_res$pfa_diagnostics,
           pfa_unit_diagnostics = pfa_unit_result,
           unidimensional_diagnostics = aco_res$unidimensional_diagnostics,
           final_dfi_cutoffs = aco_res$final_dfi_cutoffs,
           final_dddfi_cutoffs = aco_res$final_dddfi_cutoffs,
           final_equivtest_diagnostic = aco_res$final_equivtest_diagnostic)
    ),
    semantic_score    = aco_res$semantic_score,
    semantic_objective_score = aco_res$semantic_objective_score,
    search_objective_score = aco_res$search_objective_score,
    proposal_objective_score = aco_res$proposal_objective_score,
    final_guided_objective_score = aco_res$final_guided_objective_score,
    search_guidance_status = aco_res$search_guidance_status,
    candidate_counts = aco_res$candidate_counts,
    cohesion_retention = aco_res$cohesion_retention,
    esem_attempts = aco_res$esem_attempts,
    esem_successes = aco_res$esem_successes,
    esem_failures = aco_res$esem_failures,
    pfa_score        = aco_res$pfa_score,
    pfa_diagnostics  = aco_res$pfa_diagnostics,
    pfa_objective_score = aco_res$pfa_objective_score,
    pfa_objective_diagnostics = aco_res$pfa_objective_diagnostics,
    pfa_unit_diagnostics = pfa_unit_result,
    reference_sample_size = aco_res$reference_sample_size,
    semantic_reference_n = aco_res$semantic_reference_n,
    semantic_n_sensitivity = aco_res$semantic_n_sensitivity,
    recommended_validation_n = aco_res$recommended_validation_n,
    evaluation_telemetry = aco_res$evaluation_telemetry,
    resource_plan = aco_res$resource_plan %||% resource_plan_preview,
    performance = combined_performance,
    reproducibility = combined_reproducibility,
    semantic_pair_perturbation_stability = aco_res$semantic_pair_perturbation_stability,
    semantic_resampling_stability = aco_res$semantic_resampling_stability,
    split_half_stability = aco_res$split_half_stability,
    summary           = aco_res$summary,
    semantic_similarity_reduction = list(
      within_factor_before = within_before,
      within_factor_after  = within_after,
      between_factor_before = between_before,
      between_factor_after  = between_after,
      absolute_reduction   = reduction,
      percent_reduction    = percent_reduction,
      between_absolute_reduction = between_reduction,
      semantic_similarity_index_before = sem_index_before,
      semantic_similarity_index_after = sem_index_after,
      semantic_similarity_index_reduction = sem_index_reduction,
      within_similarity_target = target_value,
      within_similarity_band = within_similarity_band,
      within_target_deviation_before = target_dev_before,
      within_target_deviation_after = target_dev_after,
      within_target_improvement = target_improvement,
      target_band_status = target_band_status,
      interpretation = sem_reduction_interpretation
  )
  )
  class(out) <- c("semantica_full_pipeline_result", "list")
  sanitize_result_for_serialization(out)
}
