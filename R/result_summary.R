# Human-oriented result summaries that make semantic-proxy status explicit.

# Read an optional nested fit-index component without assuming that fit_indices
# uses the rich list representation. Older results, lightweight fixtures, and
# reloaded objects may legitimately store fit_indices as a named atomic vector.
# Reporting must remain backward-compatible with both shapes.
.semantica_fit_indices_component <- function(fit_indices, name, default = NULL) {
  if (!is.list(fit_indices)) return(default)
  fit_indices[[name]] %||% default
}

# Build a compact, context-aware diagnostic hierarchy without changing
# underlying calculations. This helper is deliberately reporting-only.
.semantica_diagnostic_sections <- function(object) {
  opt <- object$optimization %||% list()
  coverage <- object$construct_coverage %||% NULL
  representation <- object$representation_stability %||% list()
  sensitivity <- representation$cosine_adjustment_sensitivity %||% list()
  cosine_diag <- object$cosine_diagnostics %||% object$generation$cosine_diagnostics %||% list()
  rep_state <- object$representation_evidence_state %||% object$generation$representation_evidence_state %||% NULL
  pfa <- object$pfa_diagnostics %||% opt$pfa_diagnostics %||% NULL
  esem_state <- object$esem_state %||% opt$esem_state %||% NULL
  discrepancy <- object$pfa_esem_discrepancy %||% opt$pfa_esem_discrepancy %||% NULL
  resampling <- object$semantic_resampling_stability %||% opt$semantic_resampling_stability %||%
    object$semantic_pair_perturbation_stability$resampling %||% opt$semantic_pair_perturbation_stability$resampling %||% NULL
  stab <- object$semantic_pair_perturbation_stability %||% opt$semantic_pair_perturbation_stability %||% NULL
  evidence <- semantica_evidence_status(object)
  selection_ctx <- object$selection_semantic_context %||% opt$selection_semantic_context %||% NULL
  factor_semantic <- object$factor_semantic_diagnostics %||% opt$factor_semantic_diagnostics %||%
    selection_ctx$selected_factor_diagnostics %||% NULL
  objective_ctx <- object$objective_context %||% opt$objective_context %||% NULL
  fit_indices_obj <- object$fit_indices %||% list()
  structure_diag <- .semantica_fit_indices_component(fit_indices_obj, "structure_diagnostics", NULL)
  htmt_from_fit <- .semantica_fit_indices_component(fit_indices_obj, "htmt_max", NA_real_)
  structure_diag <- structure_diag %||% opt$structure_diagnostics %||% NULL
  dimensionality_mode <- object$dimensionality_mode %||% opt$dimensionality_mode %||%
    object$run_config$dimensionality %||% "multidimensional"
  is_unidimensional <- identical(dimensionality_mode, "unidimensional")
  unidim <- object$unidimensional_diagnostics %||% opt$unidimensional_diagnostics %||%
    .semantica_fit_indices_component(fit_indices_obj, "unidimensional_diagnostics", NULL)

  sections <- list()

  # 1. Content/blueprint: omit entirely when no blueprint/coverage evidence exists.
  if (!is.null(coverage) || !is.null(object$content_alignment) || !is.null(object$selected_item_metadata)) {
    sections$content_blueprint <- list(
      status = "computed",
      required_facet_coverage = coverage$overall_required_facet_coverage %||% NA_real_,
      metadata_coverage = coverage$metadata_overall_coverage %||% NA_real_,
      semantic_coverage = coverage$semantic_overall_coverage %||% NA_real_
    )
  }

  pool_health <- object$pool_health %||% opt$pool_health %||% NULL
  duplicate_feasibility <- object$duplicate_feasibility %||% opt$duplicate_feasibility %||% NULL
  if (is.data.frame(pool_health) || is.data.frame(duplicate_feasibility)) {
    sections$pool_operational_health <- list(
      status = "computed",
      scope = "candidate-pool selection capacity; not construct validity",
      factors = pool_health,
      duplicate_feasibility = duplicate_feasibility
    )
  }

  # 2. Representation/provenance. Keep stable secondary details available but
  # expose a warning only when preprocessing sensitivity crosses its own reference.
  policy <- object$embedding_policy %||% object$generation$embedding_policy %||% NULL
  rep_warning <- identical(sensitivity$top_pair_overlap_vs_random %||% "",
                           "at_or_below_random_reference")
  if (!is.null(policy) || length(representation) || length(cosine_diag)) {
    sections$representation <- list(
      status = if (rep_warning) "warning" else "computed",
      provider = object$embedding_provider %||% object$generation$embedding_provider %||% NA_character_,
      model = object$embedding_model %||% object$generation$embedding_model %||% NA_character_,
      analysis_intent = policy$analysis_intent %||% NA_character_,
      provider_task = policy$provider_task %||% policy$resolved_task %||% NA_character_,
      common_direction_strength = representation$common_direction_strength %||% NA_real_,
      effective_rank = representation$effective_rank %||% cosine_diag$effective_rank %||% NA_real_,
      effective_rank_ratio = representation$effective_rank_ratio %||% cosine_diag$effective_rank_ratio %||% NA_real_,
      top_eigen_share = representation$top_eigen_share %||% cosine_diag$top_eigen_share %||% NA_real_,
      top3_eigen_share = representation$top3_eigen_share %||% cosine_diag$top3_eigen_share %||% NA_real_,
      top_pair_jaccard = sensitivity$top_pair_jaccard %||% NA_real_,
      evidence_state = rep_state$status %||% NA_character_,
      evidence_schema = rep_state$state_schema %||% NA_character_,
      concentration_axis = rep_state$concentration_axis %||% NULL,
      preprocessing_sensitivity_axis = rep_state$preprocessing_sensitivity_axis %||% NULL,
      evidence_qualifiers = rep_state$qualifiers %||% NULL,
      downstream_dependency = if (!is.null(rep_state)) "qualifies semantic, PFA, ESEM, and clustering evidence derived from this representation" else NULL,
      warning = if (rep_warning) "strongest-pair membership is unusually sensitive to preprocessing" else NULL
    )
  }

  # 3. Semantic discrimination. Keep the pool and selected values separate:
  # selected values are descriptive after optimization and may be selection-inflated.
  pool_discr <- selection_ctx$pool$estimate %||% cosine_diag$stochastic_superiority %||% NA_real_
  selected_discr <- selection_ctx$selected$estimate %||% NA_real_
  discr_status <- selection_ctx$status %||% cosine_diag$stochastic_superiority_status %||%
    if (is.finite(pool_discr)) "computed" else "unavailable"
  weakest_factor <- NULL
  if (is.data.frame(factor_semantic) && nrow(factor_semantic) > 0L) {
    ok <- is.finite(factor_semantic$gap)
    if (any(ok)) {
      z <- factor_semantic[ok, , drop = FALSE]
      weakest_factor <- z[which.min(z$gap), , drop = FALSE]
    }
  }
  sections$semantic_discrimination <- list(
    status = discr_status,
    dimensionality_mode = dimensionality_mode,
    reason = selection_ctx$reason %||% cosine_diag$stochastic_superiority_reason %||% NULL,
    # Backward-compatible alias: the pre-selection/pool diagnostic.
    stochastic_superiority = pool_discr,
    pool_stochastic_superiority = pool_discr,
    selected_stochastic_superiority = selected_discr,
    stochastic_superiority_gain = selection_ctx$stochastic_superiority_gain %||% NA_real_,
    pool_gap = selection_ctx$pool_gap %||% NA_real_,
    selected_gap = selection_ctx$selected_gap %||% NA_real_,
    gap_gain = selection_ctx$gap_gain %||% NA_real_,
    selection_conditioned = isTRUE(selection_ctx$selection_conditioned),
    weakest_factor = weakest_factor,
    factor_diagnostics = factor_semantic,
    reason = selection_ctx$reason %||% cosine_diag$stochastic_superiority_reason %||% NULL,
    semantic_proxy_score = object$semantic_score %||% opt$semantic_score %||% NA_real_,
    pool_within_mean = selection_ctx$pool$within_mean %||% NA_real_,
    selected_within_mean = selection_ctx$selected$within_mean %||% NA_real_,
    within_cohesion_change = selection_ctx$within_cohesion_change %||% NA_real_,
    note = selection_ctx$note %||% NULL
  )

  # 4. Structural proxies. An ESEM technical failure/inadmissibility is surfaced
  # once through its consolidated state rather than as a cascade of derivative NAs.
  structural_status <- if (is.null(esem_state) && is.null(pfa)) "unavailable" else "computed"
  if (!is.null(esem_state) && esem_state$technical_state %in% c("attempted_failed", "converged_inadmissible")) {
    structural_status <- "warning"
  }
  if (!is.null(esem_state) && identical(esem_state$structural_quality, "admissible_but_structurally_mixed")) {
    structural_status <- "warning"
  }
  discrepancy_state <- if (is.null(discrepancy)) NA_character_ else
    discrepancy$state %||% discrepancy$status %||% NA_character_
  discrepancy_warning_states <- c(
    "grouping_recoverable__separation_weak",
    "grouping_weak__esem_weak",
    # Accept the short-lived development spellings for resilient reload/reporting.
    "grouping_recoverable_separation_weak",
    "grouping_weak_esem_weak"
  )
  if (isTRUE(discrepancy_state %in% discrepancy_warning_states)) {
    structural_status <- "warning"
  }
  sections$structural_proxies <- list(
    status = structural_status,
    dimensionality_mode = dimensionality_mode,
    unidimensional_diagnostics = unidim,
    pfa_factor_presence_recovery = pfa$factor_presence_recovery %||% pfa$recovery_score %||% object$pfa_score %||% NA_real_,
    pfa_partition_agreement_ari = pfa$partition_agreement_ari %||% NA_real_,
    pfa_partition_agreement_role = pfa$partition_agreement_role %||% NA_character_,
    pfa_factor_presence_role = pfa$factor_presence_role %||% NA_character_,
    esem_state = esem_state,
    esem_htmt_max = htmt_from_fit %||% opt$htmt_max %||% NA_real_,
    esem_htmt_objective_role = object$esem_result$htmt_objective_role %||%
      opt$esem_result$htmt_objective_role %||% opt$model_info$htmt_objective_role %||% "diagnostic",
    esem_factor_diagnostics = structure_diag$factor_diagnostics %||% NULL,
    discrepancy = discrepancy
  )

  # 5. ACO optimization. Hard ceilings are warnings and are shown before winner
  # details by the print method.
  termination <- object$termination_reason %||% opt$termination_reason %||% NA_character_
  hard_ceiling <- termination %in% c("max_total_iter_reached", "max_esem_fits_reached")
  sections$aco_optimization <- list(
    status = if (isTRUE(hard_ceiling)) "warning" else if (!is.na(termination)) "computed" else "unavailable",
    termination_reason = termination,
    hard_ceiling = isTRUE(hard_ceiling),
    selected_items = length(object$best_items %||% opt$best_items %||% character()),
    best_found_objective = object$best_objective %||% object$final_guided_objective_score %||%
      opt$best_objective %||% opt$final_guided_objective_score %||% object$best_score %||% object$objective %||% opt$best_score %||% NA_real_,
    objective_context = objective_ctx,
    search_space = object$search_space %||% opt$search_space %||% NULL
  )

  # 6. Robustness. New analyses use descriptive stratified-pair bootstrap and
  # item-jackknife sensitivity without a universal stable/unstable threshold.
  # The legacy pair-perturbation object remains a compatibility field only.
  robustness <- list(status = "not_requested")
  if (is.list(resampling) && identical(resampling$status %||% "", "computed")) {
    robustness <- list(
      status = "computed",
      semantic_resampling = resampling,
      stochastic_superiority_interval = resampling$pair_bootstrap$stochastic_superiority_interval %||% NULL,
      median_gap_interval = resampling$pair_bootstrap$median_gap_interval %||% NULL,
      item_jackknife_stochastic_superiority_range = resampling$item_jackknife$stochastic_superiority_range %||% NULL,
      binary_stability_classification = NA_character_,
      note = resampling$note %||% NULL
    )
  } else if (!is.null(stab) && !identical(stab$classification %||% "", "superseded_by_resampling_sensitivity")) {
    # Backward-compatible reporting for old serialized results only.
    robustness <- list(
      status = if (identical(stab$stable, FALSE)) "warning" else "computed",
      pair_perturbation_difference = stab$difference %||% NA_real_,
      pair_perturbation_classification = stab$classification %||% if (isTRUE(stab$stable)) "heuristically_stable" else "heuristically_unstable",
      pair_perturbation_threshold_status = stab$threshold_status %||% NULL,
      legacy = TRUE
    )
  }
  multi_seed <- object$multi_seed_stability %||% object$robustness$multi_seed %||% NULL
  cross_model <- object$semantic_robustness %||% object$cross_embedding_robustness %||% NULL
  if (!is.null(multi_seed)) robustness$multi_seed <- multi_seed
  if (!is.null(cross_model)) robustness$cross_embedding <- cross_model
  sections$robustness <- robustness

  # 7. Participant evidence: one explicit state when supplied; otherwise one
  # proxy-evidence boundary rather than repeating the same disclaimer per metric.
  participant_row <- evidence[evidence$evidence == "participant_internal_structure", , drop = FALSE]
  participant_status <- if (nrow(participant_row)) participant_row$status[[1L]] else "not_established"
  sections$participant_evidence <- list(
    status = if (participant_status == "not_established") "not_requested" else participant_status,
    participant_based = if (nrow(participant_row)) isTRUE(participant_row$participant_based[[1L]]) else FALSE,
    boundary = if (participant_status == "not_established")
      "sample-free semantic/structural proxies require participant validation" else NULL
  )

  sections
}

#' @export
summary.semantica_full_pipeline_result <- function(object, sections = "all", ...) {
  allowed_sections <- c("content", "representation", "semantic", "structural", "aco", "robustness", "participant")
  if (length(sections) == 1L && identical(sections, "all")) {
    sections <- "all"
  } else {
    sections <- unique(as.character(sections))
    bad <- setdiff(sections, allowed_sections)
    if (!length(sections) || length(bad)) stop(sprintf("Unknown summary section(s): %s. Available: %s.", paste(bad, collapse = ", "), paste(allowed_sections, collapse = ", ")), call. = FALSE)
  }
  opt <- object$optimization %||% list()
  response_present <- !is.null(opt$response_validation) || !is.null(object$response_validation)
  repair <- object$matrix_repair_diagnostics %||% NULL
  coverage <- object$construct_coverage %||% NULL
  polarity <- object$polarity_diagnostics %||% NULL
  align_meta <- object$selected_item_metadata %||% NULL
  aligned_prop <- if (is.data.frame(align_meta) && "semantica_factor_aligned" %in% names(align_meta)) mean(align_meta$semantica_factor_aligned, na.rm=TRUE) else NA_real_
  ambiguous_prop <- if (is.data.frame(align_meta) && "semantica_factor_alignment_status" %in% names(align_meta)) mean(align_meta$semantica_factor_alignment_status == "ambiguous", na.rm=TRUE) else NA_real_
  clear_mismatch_prop <- if (is.data.frame(align_meta) && "semantica_factor_clear_mismatch" %in% names(align_meta)) mean(align_meta$semantica_factor_clear_mismatch, na.rm=TRUE) else NA_real_
  median_factor_margin <- if (is.data.frame(align_meta) && "semantica_factor_margin" %in% names(align_meta) && any(is.finite(align_meta$semantica_factor_margin))) stats::median(align_meta$semantica_factor_margin, na.rm=TRUE) else NA_real_
  median_factor_score <- if (is.data.frame(align_meta) && "semantica_factor_score" %in% names(align_meta) && any(is.finite(align_meta$semantica_factor_score))) stats::median(align_meta$semantica_factor_score, na.rm=TRUE) else NA_real_
  exclusion_conflicts <- if (is.data.frame(align_meta) && "semantica_exclusion_conflict" %in% names(align_meta)) sum(align_meta$semantica_exclusion_conflict, na.rm=TRUE) else NA_integer_
  representation <- object$representation_stability %||% list()
  sensitivity <- representation$cosine_adjustment_sensitivity %||% list()
  metadata_cov <- coverage$metadata_overall_coverage %||% NA_real_
  semantic_cov <- coverage$semantic_overall_coverage %||% NA_real_
  embedding_policy <- object$embedding_policy %||% object$generation$embedding_policy %||% NULL
  flagged_polarity <- if (is.data.frame(polarity)) sum(polarity$direction != "not_flagged", na.rm = TRUE) else NA_integer_
  dimensionality_mode <- object$dimensionality_mode %||% object$optimization$dimensionality_mode %||%
    object$run_config$dimensionality %||% "multidimensional"
  fit_indices_obj <- object$fit_indices %||% list()
  if (identical(dimensionality_mode, "unidimensional")) {
    # Top-factor match is tautological with one intended factor and must not be
    # presented as relative content-alignment evidence. Retain the direct
    # item-definition similarity only as a descriptive representation metric.
    aligned_prop <- NA_real_
    ambiguous_prop <- NA_real_
    clear_mismatch_prop <- NA_real_
    median_factor_margin <- NA_real_
  }
  out <- list(
    dimensionality_mode = dimensionality_mode,
    unidimensional_diagnostics = object$unidimensional_diagnostics %||% object$optimization$unidimensional_diagnostics %||%
      .semantica_fit_indices_component(fit_indices_obj, "unidimensional_diagnostics", NULL),
    selected_items = length(object$best_items %||% character(0L)),
    semantic_proxy_score = object$semantic_score %||% NA_real_,
    semantic_proxy_esem = fit_indices_obj[c("cfi", "rmsea", "srmr")],
    semantic_intended_structure_alignment = object$pfa_score %||% NA_real_,
    construct_facet_coverage = coverage$overall_required_facet_coverage %||% NA_real_,
    metadata_facet_coverage = metadata_cov,
    semantic_facet_coverage = semantic_cov,
    content_factor_alignment = aligned_prop,
    content_ambiguous_rate = ambiguous_prop,
    content_clear_mismatch_rate = clear_mismatch_prop,
    content_alignment_mode = opt$model_info$content_alignment_mode %||% NA_character_,
    content_median_factor_margin = median_factor_margin,
    content_median_definition_similarity = median_factor_score,
    content_exclusion_conflicts = exclusion_conflicts,
    within_target_method = object$optimization$within_target_method %||% object$within_target_method %||% NA_character_,
    representation_common_direction = representation$common_direction_strength %||% NA_real_,
    representation_offdiag_correlation = sensitivity$offdiag_correlation %||% NA_real_,
    representation_top_pair_fraction = sensitivity$top_pair_fraction_effective %||% sensitivity$top_pair_fraction %||% NA_real_,
    representation_top_pair_jaccard = sensitivity$top_pair_jaccard %||% NA_real_,
    representation_top_pair_random_reference = sensitivity$top_pair_jaccard_random_baseline %||% NA_real_,
    representation_top_pair_overlap_vs_random = sensitivity$top_pair_overlap_vs_random %||% NA_character_,
    embedding_task = embedding_policy$resolved_task %||% NA_character_,
    embedding_instruction_source = embedding_policy$source %||% NA_character_,
    polarity_flags = flagged_polarity,
    matrix_repair_required = isTRUE(repair$repair_required),
    matrix_repair_source = repair$matrix_source %||% NA_character_,
    matrix_repair_max_abs_change = repair$max_abs_change %||% NA_real_,
    matrix_repair_relative_frobenius_change = repair$relative_frobenius_change %||% NA_real_,
    matrix_repair_offdiag_pearson = repair$offdiag_pearson %||% NA_real_,
    matrix_repair_offdiag_spearman = repair$offdiag_spearman %||% NA_real_,
    matrix_repair_interpretation = repair$interpretation %||% NULL,
    participant_validation = if (response_present) "participant data supplied" else "NOT PERFORMED",
    print_sections = sections,
    diagnostic_sections = .semantica_diagnostic_sections(object),
    evidence_status = semantica_evidence_status(object),
    evidence_profile = object$evidence_profile %||% opt$evidence_profile %||% NULL,
    decision_policy = object$decision_policy %||% opt$decision_policy %||% NULL,
    notice = paste(
      "Semantic-proxy diagnostics are pre-data screening evidence.",
      "They do not establish construct validity, reliability, invariance, DIF, or criterion validity."
    )
  )
  class(out) <- c("summary.semantica_full_pipeline_result", "list")
  out
}

#' @export
print.summary.semantica_full_pipeline_result <- function(x, ...) {
  if (!is.null(x$print_sections) && !identical(x$print_sections, "all")) {
    return(.semantica_print_summary_subset(x, ...))
  }
  cat("\nSEMANTICA summary\n")
  cat("============================\n")
  sec <- x$diagnostic_sections %||% list()
  ep <- x$evidence_profile %||% NULL
  if (!is.null(ep)) {
    cat(sprintf("Analysis source families        : %s (theory + embedding%s)\n",
                ep$analysis_source_family_count %||% "unknown",
                if (isTRUE(ep$participant_response_family_available)) " + participant response" else ""))
    cat(sprintf("Independent participant family  : %s\n",
                if (isTRUE(ep$participant_response_family_available)) "available" else "not supplied"))
    cat("Evidence dependency             : semantic/PFA/ESEM/HTMT/DFI proxies share one embedding source family\n")
  }

  if (!is.null(sec$content_blueprint)) {
    cat("\n[1] Content / blueprint\n")
    cov <- sec$content_blueprint$required_facet_coverage %||% NA_real_
    cat(sprintf("Required-facet coverage          : %s\n", if (is.finite(cov)) sprintf("%.1f%%", 100 * cov) else "not assessed"))
    if (identical(x$dimensionality_mode %||% "", "unidimensional")) {
      cat("Relative factor alignment        : not applicable (single intended factor)\n")
      if (is.finite(x$content_median_definition_similarity %||% NA_real_)) {
        cat(sprintf("Median item-definition similarity: %.3f (descriptive)\n", x$content_median_definition_similarity))
      }
    } else if (is.finite(x$content_factor_alignment)) {
      cat(sprintf("Content-definition alignment     : %.1f%% assigned-factor top match\n", 100 * x$content_factor_alignment))
    }
    if (is.finite(x$content_clear_mismatch_rate) && x$content_clear_mismatch_rate > 0) {
      cat(sprintf("Clear factor mismatch rate       : %.1f%%\n", 100 * x$content_clear_mismatch_rate))
    }
    ph <- sec$pool_operational_health$factors %||% NULL
    if (is.data.frame(ph) && nrow(ph) > 0L &&
        all(c("guard_retention", "selection_pressure_after_guard") %in% names(ph))) {
      gr <- ph$guard_retention[is.finite(ph$guard_retention)]
      sp <- ph$selection_pressure_after_guard[is.finite(ph$selection_pressure_after_guard)]
      if (length(gr)) cat(sprintf("Guard retention range            : %.1f%% to %.1f%% (descriptive capacity)\n", 100 * min(gr), 100 * max(gr)))
      if (length(sp)) cat(sprintf("Post-guard selection pressure    : %.1f%% to %.1f%% of factor pools\n", 100 * min(sp), 100 * max(sp)))
      if ("raw_exclusion_retained_by_sensitivity" %in% names(ph)) {
        n_retained <- sum(ph$raw_exclusion_retained_by_sensitivity, na.rm = TRUE)
        if (n_retained > 0L) cat(sprintf("Sensitivity-retained exclusions : %d raw exclusion(s) kept for ACO after preprocessing disagreement\n", n_retained))
      }
    }
  }

  if (!is.null(sec$representation)) {
    cat("\n[2] Representation / provenance\n")
    rp <- sec$representation
    if (!is.na(rp$model %||% NA_character_)) cat(sprintf("Embedding model                  : %s\n", rp$model))
    if (!is.na(rp$analysis_intent %||% NA_character_)) cat(sprintf("Analysis intent                  : %s\n", rp$analysis_intent))
    if (!is.na(rp$evidence_state %||% NA_character_)) cat(sprintf("Representation evidence state   : %s\n", rp$evidence_state))
    psa <- rp$preprocessing_sensitivity_axis %||% NULL
    if (!is.null(psa) && isTRUE(psa$available)) {
      if (is.finite(psa$offdiag_correlation %||% NA_real_)) {
        cat(sprintf("Raw/centered geometry correlation: %.3f (continuous sensitivity evidence)\n", psa$offdiag_correlation))
      }
      if (is.finite(psa$top_pair_jaccard %||% NA_real_)) {
        cat(sprintf("Raw/centered top-pair Jaccard    : %.3f (random ref %.3f)\n",
                    psa$top_pair_jaccard, psa$top_pair_random_reference %||% NA_real_))
      }
    }
    if (!is.null(rp$downstream_dependency)) cat(sprintf("Downstream qualification        : %s\n", rp$downstream_dependency))
    if (!is.null(rp$warning)) cat(sprintf("Representation warning          : %s\n", rp$warning))
  }

  sd <- sec$semantic_discrimination %||% list()
  is_unidimensional <- identical(x$dimensionality_mode %||% sd$dimensionality_mode %||% "", "unidimensional")
  if (is_unidimensional) {
    cat("\n[3] Semantic cohesion\n")
    cat(sprintf("Semantic-proxy score            : %s\n", if (is.finite(x$semantic_proxy_score)) sprintf("%.3f", x$semantic_proxy_score) else "NA"))
    if (is.finite(sd$pool_within_mean %||% NA_real_) && is.finite(sd$selected_within_mean %||% NA_real_)) {
      cat(sprintf("Within similarity, pool->set    : %.3f -> %.3f\n", sd$pool_within_mean, sd$selected_within_mean))
      if (is.finite(sd$within_cohesion_change %||% NA_real_)) {
        cat(sprintf("Selection change in cohesion    : %+.3f (post-selection descriptive)\n", sd$within_cohesion_change))
      }
    }
    cat("Between-factor A / gap          : not applicable (single intended factor)\n")
  } else {
    cat("\n[3] Semantic discrimination\n")
    cat(sprintf("Semantic-proxy score            : %s\n", if (is.finite(x$semantic_proxy_score)) sprintf("%.3f", x$semantic_proxy_score) else "NA"))
    if (is.finite(sd$pool_stochastic_superiority %||% NA_real_)) {
      cat(sprintf("Candidate-pool superiority A    : %.3f (sample-free proxy)\n", sd$pool_stochastic_superiority))
    }
    if (is.finite(sd$selected_stochastic_superiority %||% NA_real_)) {
      cat(sprintf("Selected-set superiority A      : %.3f (post-selection descriptive)\n", sd$selected_stochastic_superiority))
      if (is.finite(sd$stochastic_superiority_gain %||% NA_real_)) {
        cat(sprintf("Selection change in A           : %+.3f (not selection-adjusted inference)\n", sd$stochastic_superiority_gain))
      }
    } else if (is.finite(sd$stochastic_superiority %||% NA_real_)) {
      cat(sprintf("Within > between superiority A  : %.3f (sample-free proxy)\n", sd$stochastic_superiority))
    } else if (identical(sd$status, "failed") || identical(sd$status, "warning")) {
      cat(sprintf("Semantic discrimination         : %s\n", sd$reason %||% sd$status))
    }
    if (is.finite(sd$pool_gap %||% NA_real_) && is.finite(sd$selected_gap %||% NA_real_)) {
      cat(sprintf("Within-between gap, pool->set   : %+.3f -> %+.3f\n", sd$pool_gap, sd$selected_gap))
    }
    wf <- sd$weakest_factor %||% NULL
    if (is.data.frame(wf) && nrow(wf) == 1L) {
      cat(sprintf("Weakest selected factor         : %s | gap %+.3f | A %s\n",
                  wf$factor[[1L]], wf$gap[[1L]],
                  if (is.finite(wf$stochastic_superiority[[1L]])) sprintf("%.3f", wf$stochastic_superiority[[1L]]) else "NA"))
    }
  }

  cat("\n[4] Structural proxies\n")
  sp <- sec$structural_proxies %||% list()
  es <- sp$esem_state %||% NULL
  unidim <- sp$unidimensional_diagnostics %||% x$unidimensional_diagnostics %||% NULL
  if (!is.null(es)) {
    cat(sprintf("ESEM technical state            : %s\n", es$technical_state %||% "unavailable"))
    if (!is.null(es$structural_quality)) {
      cat(sprintf("ESEM structural quality         : %s\n", es$structural_quality))
    }
    if (identical(es$structural_quality %||% "", "admissible_but_structurally_mixed")) {
      cat("ESEM STRUCTURAL WARNING          : technically admissible does NOT mean clean intended structure\n")
      flags <- names(which(es$quality_flags %||% logical(0L)))
      if (length(flags)) cat(sprintf("Mixed-structure flags           : %s\n", paste(flags, collapse = ", ")))
      if (is.finite(sp$esem_htmt_max %||% NA_real_)) {
        cat(sprintf("HTMT-like semantic proxy max    : %.3f (sample-free; not participant HTMT; role=%s)\n",
                    sp$esem_htmt_max, sp$esem_htmt_objective_role %||% "diagnostic"))
      }
    }
    if ((es$technical_state %||% "") %in% c("attempted_failed", "converged_inadmissible")) {
      cat(sprintf("ESEM reason                     : %s\n", es$reason %||% "see detailed result telemetry"))
    } else {
      fit <- x$semantic_proxy_esem %||% list()
      if (any(vapply(fit, function(z) is.finite(z %||% NA_real_), logical(1)))) {
        cat(sprintf("ESEM fit proxies                : CFI %s | RMSEA %s | SRMR %s\n",
                    if (is.finite(fit$cfi %||% NA_real_)) sprintf("%.3f", fit$cfi) else "NA",
                    if (is.finite(fit$rmsea %||% NA_real_)) sprintf("%.3f", fit$rmsea) else "NA",
                    if (is.finite(fit$srmr %||% NA_real_)) sprintf("%.3f", fit$srmr) else "NA"))
      }
    }
  } else {
    cat("ESEM                            : not requested or unavailable\n")
  }
  if (!is_unidimensional && is.finite(sp$esem_htmt_max %||% NA_real_) &&
      !identical(es$structural_quality %||% "", "admissible_but_structurally_mixed")) {
    cat(sprintf("HTMT-like semantic proxy max    : %.3f (descriptive; role=%s)\n",
                sp$esem_htmt_max, sp$esem_htmt_objective_role %||% "diagnostic"))
  }
  if (is_unidimensional && !is.null(unidim)) {
    if (is.finite(unidim$ave %||% NA_real_)) cat(sprintf("One-factor AVE proxy            : %.3f\n", unidim$ave))
    if (is.finite(unidim$mean_primary_loading %||% NA_real_)) {
      cat(sprintf("Primary loadings mean / min     : %.3f / %s\n",
                  unidim$mean_primary_loading,
                  if (is.finite(unidim$min_primary_loading %||% NA_real_)) sprintf("%.3f", unidim$min_primary_loading) else "NA"))
    }
    if (is.finite(unidim$primary_ge_40 %||% NA_real_) || is.finite(unidim$primary_ge_50 %||% NA_real_)) {
      cat(sprintf("Primary >= .40/.50              : %s / %s\n",
                  if (is.finite(unidim$primary_ge_40 %||% NA_real_)) sprintf("%.1f%%", 100 * unidim$primary_ge_40) else "NA",
                  if (is.finite(unidim$primary_ge_50 %||% NA_real_)) sprintf("%.1f%%", 100 * unidim$primary_ge_50) else "NA"))
    }
    if (is.finite(unidim$mean_abs_residual %||% NA_real_)) {
      cat(sprintf("Residual |r| mean / q95 / max  : %.3f / %s / %s\n",
                  unidim$mean_abs_residual,
                  if (is.finite(unidim$q95_abs_residual %||% NA_real_)) sprintf("%.3f", unidim$q95_abs_residual) else "NA",
                  if (is.finite(unidim$max_abs_residual %||% NA_real_)) sprintf("%.3f", unidim$max_abs_residual) else "NA"))
    }
    if (is.finite(unidim$max_abs_centered_residual %||% NA_real_)) {
      cat(sprintf("Centered residual max |r|       : %.3f (descriptive; no universal cutoff)\n", unidim$max_abs_centered_residual))
    }
    if (is.finite(unidim$eigenvalue_ratio_1_to_2 %||% NA_real_) || is.finite(unidim$first_eigenvalue_share %||% NA_real_)) {
      cat(sprintf("Eigen dominance 1/2 / share     : %s / %s (descriptive)\n",
                  if (is.finite(unidim$eigenvalue_ratio_1_to_2 %||% NA_real_)) sprintf("%.3f", unidim$eigenvalue_ratio_1_to_2) else "NA",
                  if (is.finite(unidim$first_eigenvalue_share %||% NA_real_)) sprintf("%.3f", unidim$first_eigenvalue_share) else "NA"))
    }
    cat("PFA partition / HTMT            : not applicable (single intended factor)\n")
  }
  efd <- sp$esem_factor_diagnostics %||% NULL
  if (!is_unidimensional && is.data.frame(efd) && nrow(efd) > 0L && any(is.finite(efd$simple_structure))) {
    z <- efd[is.finite(efd$simple_structure), , drop = FALSE]
    worst <- z[which.min(z$simple_structure), , drop = FALSE]
    cat(sprintf("Weakest ESEM factor             : %s | dominance %.1f%% | simple structure %.1f%%\n",
                worst$factor[[1L]], 100 * worst$correct_dominance[[1L]], 100 * worst$simple_structure[[1L]]))
  }
  if (!is_unidimensional && is.finite(sp$pfa_partition_agreement_ari %||% NA_real_)) {
    cat(sprintf("PFA partition agreement (ARI)   : %.3f (chance-adjusted partition descriptor)\n", sp$pfa_partition_agreement_ari))
  }
  if (!is_unidimensional && is.finite(sp$pfa_factor_presence_recovery %||% NA_real_)) {
    cat(sprintf("PFA factor-presence recovery    : %.3f (factor presence only)\n", sp$pfa_factor_presence_recovery))
  }
  d <- sp$discrepancy %||% NULL
  d_state <- if (is.null(d)) NULL else d$state %||% d$status %||% NULL
  if (!is_unidimensional && !is.null(d_state) && !identical(d_state, "grouping_recoverable__esem_favorable")) {
    cat(sprintf("PFA/ESEM discrepancy            : %s\n", d_state))
  }
  if (!is_unidimensional && !is.null(d$pfa_partition_state) && !identical(d$pfa_partition_state, "exact")) {
    cat(sprintf("PFA partition state             : %s\n", d$pfa_partition_state))
  }

  cat("\n[5] ACO optimization\n")
  ao <- sec$aco_optimization %||% list()
  if (isTRUE(ao$hard_ceiling)) cat(sprintf("ACO WARNING                     : hard ceiling reached (%s)\n", ao$termination_reason))
  else if (!is.na(ao$termination_reason %||% NA_character_)) cat(sprintf("Termination reason              : %s\n", ao$termination_reason))
  cat(sprintf("Selected items                  : %d\n", x$selected_items %||% 0L))
  ss <- ao$search_space$eligible %||% NULL
  if (!is.null(ss) && is.finite(ss$log10_total %||% NA_real_)) {
    cat(sprintf("Eligible constrained search     : 10^%.2f candidate forms\n", ss$log10_total))
    ratio <- ss$aco_effort$nominal_proposal_to_space_ratio %||% NA_real_
    if (is.finite(ratio)) {
      cat(sprintf("Nominal proposals / space       : %.4g (descriptive; revisits possible)\n", ratio))
    }
  }
  if (is.finite(ao$best_found_objective %||% NA_real_)) {
    cat(sprintf("Optimization utility            : %.4f\n", ao$best_found_objective))
  }
  oc <- ao$objective_context %||% NULL
  if (!is.null(oc)) {
    cat(sprintf("Objective evidence regime       : %s\n", oc$evidence_regime %||% "unknown"))
    if (grepl("fallback", oc$evidence_regime %||% "", fixed = TRUE)) {
      cat("OBJECTIVE CONTEXT WARNING        : fallback objective is not comparable to an ESEM-guided quality score\n")
    }
    cat("Objective interpretation        : optimization utility; not a universal scale-quality score\n")
  }

  rb <- sec$robustness %||% list(status = "not_requested")
  if (!identical(rb$status, "not_requested") || !is.null(rb$multi_seed) || !is.null(rb$cross_embedding)) {
    cat("\n[6] Robustness\n")
    if (!is.null(rb$semantic_resampling)) {
      ai <- rb$stochastic_superiority_interval %||% c(lower = NA_real_, upper = NA_real_)
      if (is.finite(ai[["lower"]] %||% NA_real_) && is.finite(ai[["upper"]] %||% NA_real_)) {
        cat(sprintf("Semantic A sensitivity interval : [%.3f, %.3f] (descriptive bootstrap)\n", ai[["lower"]], ai[["upper"]]))
      } else {
        cat("Semantic resampling sensitivity : available in detailed result\n")
      }
      cat("Binary stability verdict         : not applied\n")
    } else if (isTRUE(rb$legacy) && identical(rb$status, "warning")) {
      cat(sprintf("Legacy pair-perturbation warning: %s (uncalibrated historical heuristic)\n",
                  rb$pair_perturbation_classification %||% "heuristically_unstable"))
    }
    if (!is.null(rb$multi_seed)) cat("Multi-seed stability            : available in detailed result\n")
    if (!is.null(rb$cross_embedding)) cat("Cross-embedding robustness      : available in detailed result\n")
  }

  if (isTRUE(x$matrix_repair_required)) {
    cat("\nMatrix repair dependence\n")
    cat(sprintf("Matrix source                    : %s\n", x$matrix_repair_source %||% "repaired_semantic_proxy"))
    if (is.finite(x$matrix_repair_relative_frobenius_change %||% NA_real_)) {
      cat(sprintf("Relative Frobenius perturbation  : %.4f\n", x$matrix_repair_relative_frobenius_change))
    }
    if (is.finite(x$matrix_repair_offdiag_spearman %||% NA_real_)) {
      cat(sprintf("Off-diagonal rank preservation   : %.4f (Spearman)\n", x$matrix_repair_offdiag_spearman))
    }
    cat("Repair interpretation            : continuous sensitivity evidence; no universal pass/fail repair cutoff is applied\n")
  }

  pe <- sec$participant_evidence %||% list(status = "not_requested")
  cat("\n[7] Participant evidence\n")
  if (identical(pe$status, "not_requested")) {
    # Keep the long-standing explicit state token for backward-compatible text
    # consumers while adding the more informative context-aware explanation.
    cat("Participant validation          : NOT PERFORMED \u2014 participant data not supplied; sample-free proxy evidence only\n")
  } else {
    cat(sprintf("Participant evidence state      : %s\n", pe$status))
  }
  cat("\nIMPORTANT: ", x$notice, "\n", sep = "")
  invisible(x)
}

#' @export
print.semantica_full_pipeline_result <- function(x, ...) {
  print(semantica_view(x, view = "auto"))
  invisible(x)
}

.semantica_print_summary_subset <- function(x, ...) {
  y <- x
  y$print_sections <- "all"
  lines <- utils::capture.output(print.summary.semantica_full_pipeline_result(y, ...))
  requested <- x$print_sections
  section_map <- c(`1` = "content", `2` = "representation", `3` = "semantic", `4` = "structural", `5` = "aco", `6` = "robustness", `7` = "participant")
  current <- NA_character_
  keep <- logical(length(lines))
  for (i in seq_along(lines)) {
    m <- regexec("^\\[([1-7])\\]", lines[[i]])
    hit <- regmatches(lines[[i]], m)[[1L]]
    if (length(hit) >= 2L) current <- unname(section_map[[hit[[2L]]]])
    if (grepl("^IMPORTANT:", lines[[i]]) || grepl("^SEMANTICA summary", lines[[i]]) || grepl("^=+$", lines[[i]]) ||
        grepl("^(Analysis source families|Independent participant family|Evidence dependency)", lines[[i]])) {
      keep[[i]] <- TRUE
    } else if (is.na(current)) {
      keep[[i]] <- FALSE
    } else {
      keep[[i]] <- current %in% requested
    }
  }
  cat(paste(lines[keep], collapse = "\n"), "\n", sep = "")
  invisible(x)
}
