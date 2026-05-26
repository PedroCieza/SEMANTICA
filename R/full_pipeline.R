if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
}

#' One-Call SEMANTICA Pipeline: Generation to ACO-ESEM to Visualization
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
#' @param chat_model,embed_model Override default model names.
#' @param embed_batch_size Items per embedding backend request. Lower values
#'   reduce peak request memory for local embedding models.
#' @param base_url,gguf_path Server overrides or GGUF path.
#' @param cosine_adjustment Embedding cosine preprocessing passed to
#'   `semantica_pipeline()`.
#' @param semantic_calibration Optional matrix or function used to calibrate
#'   the semantic cosine proxy before ACO/ESEM.
#' @param compute_cosine_sensitivity Logical; compute the optional
#'   none-versus-mean-centered embedding diagnostic.
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
#' @param verbose Print progress messages.
#' @param ... Additional arguments passed to `semantica_pipeline()`.
#'
#' @return A named list containing:
#'   * `generation`: Output from `semantica_pipeline()`.
#'   * `optimization`: Output from `ACO_with_ESEM()`.
#'   * `plots`: List of `ggplot`/`plotly` objects (or `NULL`).
#'   * `best_items`: Character vector of selected item IDs.
#'   * `factor_assignment`: Named vector mapping items to factors.
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
#'   * `summary`: Compact summary object from ACO.
#'
#' @export
#' @examples
#' \dontrun{
#' # Requires valid API credentials or an available local backend.
#' result <- semantica_full_pipeline(
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
semantica_full_pipeline <- function(
    # LLM & Embedding Setup
  backend = "openai", embed_backend = NULL, api_key = NULL, embed_api_key = NULL,
  chat_model = NULL, embed_model = NULL, embed_batch_size = 64L,
  base_url = NULL, gguf_path = NULL,
  cosine_adjustment = c("none", "mean_center"), semantic_calibration = NULL,
  compute_cosine_sensitivity = TRUE, release_local_models = FALSE,
  retain_embeddings = TRUE,
  # Scale & Factor Specs
  scale_name, scale_description, factors, n_per_factor = 15L,
  n_per_factor_override = !missing(n_per_factor),
  # ACO Selection Target
  i.per.f = NULL,
  # ACO Optimization Parameters
  ants = 90, max.iter = 50, esem_every = 10, run_esem_during_search = TRUE,
  max_total_iter = NULL, max_esem_fits = NULL,
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
  facet_coverage_weight = 0.15, psychometric_guard_weight = 0.50,
  psychometric_guard_min_ave = 0.30,
  psychometric_guard_min_loading = 0.40,
  psychometric_guard_min_primary_ge_50 = 0.70,
  pfa_mode = c("diagnostic", "objective", "off"),
  pfa_weight = 0.20,
  pfa_extraction = c("principal", "ml"),
  pfa_final_extraction = c("ml", "principal"),
  pfa_rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
  pfa_min_loading = psychometric_guard_min_loading,
  pfa_min_margin = NULL,
  pfa_unit_diagnostics = TRUE,
  reference_rmsea_close = 0.05,
  reference_rmsea_poor = 0.08,
  reference_power = 0.80,
  reference_alpha = 0.05,
  reference_max_n = 5000L,
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
  elite_pareto_rerank = TRUE,
  validation_data = NULL, validation_ordered = NULL,
  sigmoid_steepness = 10, heuristic_beta = 0.50, archive_stable_window = 8L,
  pheromone_update = c("top_elite", "best_ant"), fixed_evaporation = NULL,
  debug_mode = FALSE, keep_solution_history = TRUE,
  history_mode = c("summary", "full", "none"), use_parallel = TRUE,
  n.cores = 2L,
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
  verbose = TRUE, ...
) {
  pheromone_update <- match.arg(pheromone_update)
  interactive_mode <- match.arg(interactive_mode)
  dfi_mode <- match.arg(dfi_mode)
  dfi_esem_strategy <- match.arg(dfi_esem_strategy)
  dfi_fallback_policy <- match.arg(dfi_fallback_policy)
  final_dddfi_mad_target <- match.arg(final_dddfi_mad_target)
  pfa_mode <- match.arg(pfa_mode)
  pfa_extraction <- match.arg(pfa_extraction)
  pfa_final_extraction <- match.arg(pfa_final_extraction)
  pfa_rotation <- match.arg(pfa_rotation)
  cosine_adjustment <- match.arg(cosine_adjustment)
  semantic_esem_score_mode <- match.arg(semantic_esem_score_mode)
  esem_failure_policy <- match.arg(esem_failure_policy)
  history_mode <- match.arg(history_mode)
  plot_before_path_model <- match.arg(plot_before_path_model)
  dots <- list(...)
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
        embed_batch_size = embed_batch_size,
        base_url = base_url, gguf_path = gguf_path, scale_name = scale_name,
        scale_description = scale_description, factors = factors, n_per_factor = n_per_factor,
        n_per_factor_override = n_per_factor_override,
        cosine_adjustment = cosine_adjustment, semantic_calibration = semantic_calibration,
        compute_cosine_sensitivity = compute_cosine_sensitivity,
        release_local_models = release_local_models,
        retain_embeddings = retain_embeddings,
        verbose = verbose
      ),
      dots
    )
  )

  # =================================================================
  # STEP 2: ACO-ESEM Optimization with DFI Calibration
  # =================================================================
  if (verbose) cat("\n[SEMANTICA] Step 2/3: ACO-ESEM Optimization...\n")
  aco_res <- ACO_with_ESEM(
    cosine_sim_matrix = gen_res$cosine_sim_matrix, df = gen_res$df,
    i.per.f = i.per.f, ants = ants, max.iter = max.iter, esem_every = esem_every,
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
    cohesion_quantile = cohesion_quantile,
    cohesion_retention = cohesion_retention,
    within_similarity_target = within_similarity_target,
    within_similarity_band = within_similarity_band,
    facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode,
    pfa_weight = pfa_weight,
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
    elite_pareto_rerank = elite_pareto_rerank,
    validation_data = validation_data,
    validation_ordered = validation_ordered,
    sigmoid_steepness = sigmoid_steepness, heuristic_beta = heuristic_beta,
    archive_stable_window = archive_stable_window, pheromone_update = pheromone_update,
    fixed_evaporation = fixed_evaporation, debug_mode = debug_mode,
    keep_solution_history = keep_solution_history, history_mode = history_mode,
    use_parallel = use_parallel,
    n.cores = n.cores, verbose = verbose
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
  sem_reduction_interpretation <- if (is.finite(target_value) && is.finite(target_improvement)) {
    between_txt <- if (is.finite(between_reduction) && between_reduction > 0.02) {
      "between-factor similarity decreased"
    } else if (is.finite(between_reduction) && between_reduction >= 0) {
      "between-factor similarity was stable/slightly lower"
    } else {
      "between-factor similarity increased"
    }
    if (target_improvement > 0.02) paste("Within-factor similarity moved meaningfully toward the target;", between_txt)
    else if (target_improvement >= -0.01) paste("Within-factor similarity stayed near the target;", between_txt)
    else paste("Within-factor similarity moved away from the target;", between_txt)
  } else if (!is.na(sem_index_reduction)) {
    if (sem_index_reduction > 0.05) "Strong reduction in semantic similarity"
    else if (sem_index_reduction > 0.02) "Moderate reduction"
    else if (sem_index_reduction >= 0) "Slight reduction or stable"
    else "Warning: semantic similarity increased"
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
        cat(sprintf("\n[SEMANTICA] Facet/unit PFA unavailable: %s\n",
                    pfa_unit_result$note %||% "insufficient facet/unit structure"))
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
  list(
    generation        = gen_res,
    optimization      = aco_res,
    plots             = plots,
    best_items        = aco_res$best_items,
    factor_assignment = aco_res$factor_assignment,
    generated_item_metadata = generated_item_metadata,
    selected_item_metadata = selected_item_metadata,
    item_structure_diagnostics = aco_res$item_structure_diagnostics,
    embedding_diagnostics = gen_res$embedding_diagnostics,
    cosine_diagnostics = gen_res$cosine_diagnostics,
    cosine_adjustment_sensitivity = gen_res$cosine_adjustment_sensitivity,
    semantic_calibration = gen_res$semantic_calibration,
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
}
