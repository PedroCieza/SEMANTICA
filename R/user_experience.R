# User-facing workflow, inspection, and export helpers.

.semantica_pick_column <- function(x, candidates) {
  hit <- intersect(candidates, names(x))
  if (length(hit)) hit[[1L]] else NULL
}

#' Extract the selected scale in a stable user-facing table
#'
#' `semantica_items()` is the recommended way to retrieve the final selected
#' item wording from a SEMANTICA result.  It avoids requiring users to inspect
#' nested result-list fields and normalizes historical metadata names to the
#' stable columns `item_id`, `factor`, `facet`, and `item_text`.
#'
#' @param result A `semantica_full_pipeline_result` or compatible SEMANTICA
#'   result containing selected-item metadata.
#' @param details Logical; append the original selected-item diagnostic columns
#'   after the stable user-facing columns.
#' @return A data frame ordered to match the selected item order when that order
#'   is available.
#' @export
semantica_items <- function(result, details = FALSE) {
  details <- .semantica_assert_flag(details, "details")
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)

  meta <- result$selected_item_metadata %||%
    result$optimization$selected_item_metadata %||% NULL
  best <- as.character(result$best_items %||% result$optimization$best_items %||% character(0L))

  if (is.null(meta)) {
    pool_meta <- result$generated_item_metadata %||%
      result$generation$generated_item_metadata %||%
      result$generation$item_metadata %||% NULL
    if (!is.data.frame(pool_meta) || !length(best)) {
      stop(
        "Selected item metadata are unavailable in this result. Use a result returned by semantica_run()/semantica_full_pipeline(), or inspect the component result directly.",
        call. = FALSE
      )
    }
    id_pool <- .semantica_pick_column(pool_meta, c("item_id", "ID", "id", "item"))
    if (is.null(id_pool)) stop("Selected item IDs cannot be matched to metadata.", call. = FALSE)
    meta <- pool_meta[match(best, as.character(pool_meta[[id_pool]])), , drop = FALSE]
  }
  if (!is.data.frame(meta)) meta <- as.data.frame(meta, stringsAsFactors = FALSE)

  id_col <- .semantica_pick_column(meta, c("item_id", "ID", "id"))
  factor_col <- .semantica_pick_column(meta, c("factor", "Dimension", "dimension", "type"))
  facet_col <- .semantica_pick_column(meta, c("facet", "Facet"))
  text_col <- .semantica_pick_column(meta, c("item_text", "item", "text"))

  n <- nrow(meta)
  item_id <- if (!is.null(id_col)) as.character(meta[[id_col]]) else rep(NA_character_, n)
  if (length(best) && !is.null(id_col)) {
    ord <- match(best, item_id)
    ord <- ord[!is.na(ord)]
    if (length(ord)) {
      remaining <- setdiff(seq_len(n), ord)
      meta <- meta[c(ord, remaining), , drop = FALSE]
      item_id <- as.character(meta[[id_col]])
    }
  }

  out <- data.frame(
    item_id = item_id,
    factor = if (!is.null(factor_col)) as.character(meta[[factor_col]]) else NA_character_,
    facet = if (!is.null(facet_col)) as.character(meta[[facet_col]]) else NA_character_,
    item_text = if (!is.null(text_col)) as.character(meta[[text_col]]) else NA_character_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (isTRUE(details)) {
    extra <- setdiff(names(meta), unique(c(id_col, factor_col, facet_col, text_col)))
    if (length(extra)) out <- cbind(out, meta[extra])
  }
  rownames(out) <- NULL
  out
}

#' Inspect the resolved configuration used by a SEMANTICA run
#'
#' Returns the sanitized resolved configuration recorded by the high-level
#' pipeline.  This is the easiest way to answer "what settings did this run
#' actually use?" after presets, defaults, and automatic resource resolution.
#'
#' @param result A SEMANTICA full-pipeline result.
#' @param section Optional top-level section name such as `"generation"`,
#'   `"models"`, `"resources"`, `"quality"`, or `"optimizer"`.
#' @param changes_only Logical; return only fields that differ from SEMANTICA's
#'   corresponding constructor defaults where a stable default is available.
#'   Runtime-resolved fields without a constructor baseline are retained.
#' @return The complete sanitized resolved configuration, one requested section,
#'   or a recursive difference view when `changes_only = TRUE`. Older results
#'   fall back to their stored `run_config`.
#' @export
semantica_config <- function(result, section = NULL, changes_only = FALSE) {
  changes_only <- .semantica_assert_flag(changes_only, "changes_only")
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)
  cfg <- result$reproducibility$resolved_config %||% result$run_config %||% NULL
  if (is.null(cfg)) stop("This result does not contain a resolved configuration record.", call. = FALSE)
  if (!is.null(section)) {
    if (!is.character(section) || length(section) != 1L || is.na(section) || !nzchar(section)) {
      stop("'section' must be one non-empty character string.", call. = FALSE)
    }
    if (!section %in% names(cfg)) {
      stop(sprintf("Unknown configuration section '%s'. Available: %s.", section, paste(names(cfg), collapse = ", ")), call. = FALSE)
    }
    cfg <- cfg[[section]]
  }
  if (!isTRUE(changes_only)) return(cfg)

  defaults <- .semantica_user_config_defaults()
  baseline <- if (is.null(section)) defaults else defaults[[section]] %||% NULL
  .semantica_recursive_diff(cfg, baseline)
}

#' Show generation and embedding models used by a run
#'
#' @param result A SEMANTICA full-pipeline result.
#' @return A two-row data frame for generation and embedding model identity.
#' @export
semantica_models <- function(result) {
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)
  m <- result$reproducibility$models %||%
    result$reproducibility$resolved_config$models %||% list()
  data.frame(
    role = c("generation", "embedding"),
    backend = c(m$generation_backend %||% NA_character_, m$embedding_backend %||% NA_character_),
    requested_model = c(m$requested_chat_model %||% NA_character_, m$requested_embedding_model %||% NA_character_),
    resolved_model = c(m$resolved_chat_model %||% m$chat_model %||% NA_character_,
                       m$resolved_embedding_model %||% m$embedding_model %||% NA_character_),
    revision = c(m$chat_model_revision %||% NA_character_, m$embedding_model_revision %||% NA_character_),
    identity_status = c(m$chat_model_identity_status %||% NA_character_,
                        m$embedding_model_identity_status %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Plain-language overview of a SEMANTICA result
#'
#' Prints the immediate run overview while leaving
#' `summary(result)` for the detailed diagnostic report.
#'
#' @param result A SEMANTICA full-pipeline result.
#' @param print_items Logical; print the selected item wording grouped by factor.
#' @return Invisibly returns a named overview list.
#' @export
semantica_overview <- function(result, print_items = TRUE) {
  print_items <- .semantica_assert_flag(print_items, "print_items")
  items <- tryCatch(semantica_items(result), error = function(e) NULL)
  evidence <- tryCatch(semantica_evidence_status(result), error = function(e) NULL)
  n_items <- if (is.data.frame(items)) nrow(items) else length(result$best_items %||% character(0L))
  factors <- if (is.data.frame(items)) unique(items$factor[!is.na(items$factor) & nzchar(items$factor)]) else character(0L)
  participant <- isTRUE(result$participant_validation_performed) ||
    !is.null(result$response_validation) || !is.null(result$optimization$response_validation)
  weakest <- result$factor_semantic_diagnostics %||% result$optimization$factor_semantic_diagnostics %||% NULL
  weakest_name <- NA_character_
  if (is.data.frame(weakest) && "gap" %in% names(weakest) && any(is.finite(weakest$gap))) {
    factor_name_col <- .semantica_pick_column(weakest, c("factor", "Dimension", "dimension"))
    if (!is.null(factor_name_col)) weakest_name <- as.character(weakest[[factor_name_col]][which.min(ifelse(is.finite(weakest$gap), weakest$gap, Inf))])
  }
  out <- list(
    selected_items = n_items,
    factors = factors,
    participant_validation = participant,
    weakest_relative_factor = weakest_name,
    evidence_status = evidence
  )

  cat("\nSEMANTICA overview\n")
  cat("==================\n")
  cat(sprintf("Selected final items : %d\n", n_items))
  if (length(factors)) cat(sprintf("Intended factors     : %d (%s)\n", length(factors), paste(factors, collapse = ", ")))
  if (!is.na(weakest_name) && nzchar(weakest_name)) {
    cat(sprintf("Relative review cue  : %s has the weakest selected semantic separation; review its wording in context.\n", weakest_name))
  }
  cat(sprintf("Participant data     : %s\n", if (participant) "supplied; inspect participant evidence separately" else "not supplied; current structural results are pre-data proxy evidence"))
  cat("Next steps           : review selected wording, inspect summary(result), plot(result), then preserve the run with semantica_save_bundle().\n")

  if (isTRUE(print_items) && is.data.frame(items) && nrow(items)) {
    cat("\nSelected scale\n")
    cat("--------------\n")
    groups <- if (all(is.na(items$factor))) list(Items = seq_len(nrow(items))) else split(seq_len(nrow(items)), items$factor, drop = TRUE)
    for (nm in names(groups)) {
      cat(sprintf("\n%s\n", nm))
      for (i in groups[[nm]]) {
        txt <- items$item_text[[i]]
        if (is.na(txt) || !nzchar(txt)) txt <- items$item_id[[i]]
        cat(sprintf("  - %s\n", txt))
      }
    }
  }
  invisible(out)
}


# Internal result-surface helpers. These reorganize retained output only; they do
# not mutate the canonical result object or recompute diagnostics.
#
# semantica_run() uses a compact six-part facade so the object itself is
# navigable in RStudio.  The complete canonical full-pipeline result is retained
# unchanged in $advanced.  semantica_full_pipeline() continues to return that
# canonical object directly.
.semantica_is_run_facade <- function(result) {
  inherits(result, "semantica_run_result") &&
    is.list(result) &&
    "advanced" %in% names(unclass(result)) &&
    is.list(unclass(result)[["advanced"]])
}

.semantica_raw_result <- function(result) {
  if (.semantica_is_run_facade(result)) unclass(result)[["advanced"]] else result
}

.semantica_regular_plot_surface <- function(result) {
  raw <- .semantica_raw_result(result)
  stored <- if (is.list(raw$plots)) raw$plots else list()
  list(
    plot_summary_of_results = stored$plot_summary_of_results %||% NULL,
    plot_fitness_evolution = stored$plot_fitness_evolution %||% stored$p02_fitness %||% NULL,
    plot_esem_before = stored$plot_esem_before %||% stored$p10a_path_before %||% NULL,
    plot_esem_after = stored$plot_esem_after %||% stored$p10b_path_after %||% NULL,
    plot_pfa_diagnostics = stored$plot_pfa_diagnostics %||% stored$p13_pfa %||% NULL
  )
}

.semantica_wrap_run_result <- function(result) {
  raw <- .semantica_raw_result(result)
  if (!is.list(raw)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)

  compact <- .semantica_compact_result_view(raw)
  scale_cfg <- raw$reproducibility$resolved_config$scale %||% list()
  scale <- c(
    list(
      name = scale_cfg$scale_name %||% NA_character_,
      description = scale_cfg$scale_description %||% NA_character_
    ),
    compact$scale
  )

  out <- list(
    scale = scale,
    items = compact$selected_scale,
    diagnostics = list(
      review_flags = compact$review_flags,
      fit_indices = raw$fit_indices %||% NULL,
      factor_review = compact$factor_review,
      evidence_status = compact$evidence_status
    ),
    plots = .semantica_regular_plot_surface(raw),
    provenance = tryCatch(semantica_provenance(raw), error = function(e) list()),
    advanced = raw
  )
  class(out) <- c("semantica_run_result", "semantica_full_pipeline_result", "list")
  out
}

# Compatibility accessor: fields that were historically exposed at the top
# level of semantica_run() continue to work (for example fit$optimization), but
# the Environment pane only has to display the five regular-user groups above.
#' @export
#' @noRd
`$.semantica_run_result` <- function(x, name) {
  surface <- unclass(x)
  if (name %in% names(surface)) return(surface[[name]])
  raw <- surface[["advanced"]]
  if (is.list(raw) && name %in% names(raw)) return(raw[[name]])
  NULL
}

#' @export
#' @noRd
`[[.semantica_run_result` <- function(x, i, ..., exact = TRUE) {
  surface <- unclass(x)
  raw <- surface[["advanced"]]
  if (is.character(i) && length(i) >= 1L) {
    first <- i[[1L]]
    if (!first %in% names(surface) && is.list(raw) && first %in% names(raw)) {
      return(base::`[[`(raw, i, ..., exact = exact))
    }
  }
  base::`[[`(surface, i, ..., exact = exact)
}

#' @export
#' @noRd
`[.semantica_run_result` <- function(x, i, ...) {
  surface <- unclass(x)
  if (missing(i)) return(base::`[`(surface, ...))
  raw <- surface[["advanced"]]
  if (is.character(i)) {
    vals <- lapply(i, function(nm) {
      if (nm %in% names(surface)) surface[[nm]]
      else if (is.list(raw) && nm %in% names(raw)) raw[[nm]]
      else NULL
    })
    names(vals) <- i
    return(vals)
  }
  base::`[`(surface, i, ...)
}

# Keep legacy field assignment coherent: assigning a canonical result field
# updates the preserved raw result and rebuilds the derived facade. Assigning a
# facade field changes only that facade field.
#' @export
#' @noRd
`$<-.semantica_run_result` <- function(x, name, value) {
  surface <- unclass(x)
  raw <- surface[["advanced"]]
  if (identical(name, "advanced")) return(.semantica_wrap_run_result(value))
  if (name %in% names(surface)) {
    surface[[name]] <- value
    class(surface) <- c("semantica_run_result", "semantica_full_pipeline_result", "list")
    return(surface)
  }
  raw[[name]] <- value
  .semantica_wrap_run_result(raw)
}

#' @export
#' @noRd
`[[<-.semantica_run_result` <- function(x, i, ..., value) {
  surface <- unclass(x)
  raw <- surface[["advanced"]]
  if (is.character(i) && length(i) == 1L) {
    name <- i[[1L]]
    if (identical(name, "advanced")) return(.semantica_wrap_run_result(value))
    if (name %in% names(surface)) {
      surface[[name]] <- value
      class(surface) <- c("semantica_run_result", "semantica_full_pipeline_result", "list")
      return(surface)
    }
    raw[[name]] <- value
    return(.semantica_wrap_run_result(raw))
  }
  base::`[[<-`(surface, i, ..., value = value)
}

.semantica_result_interface <- function(result) {
  run_cfg <- if (is.list(result$run_config)) result$run_config else list()
  rep <- if (is.list(result$reproducibility)) result$reproducibility else list()
  run_rep <- if (is.list(rep$run_interface)) rep$run_interface else list()
  iface <- run_cfg$interface %||% run_rep$interface %||% NA_character_
  if (identical(iface, "semantica_run")) "regular" else "advanced"
}

.semantica_result_component_groups <- function(result) {
  result <- .semantica_raw_result(result)
  nms <- names(result) %||% character(0L)
  catalog <- list(
    scale = c(
      "best_items", "factor_assignment", "selected_item_metadata",
      "dimensionality_mode", "unidimensional_diagnostics", "best_objective",
      "objective_context", "objective_schema", "summary"
    ),
    generation = c(
      "generation", "generated_item_metadata", "generation_provenance",
      "candidate_counts"
    ),
    content = c(
      "content_alignment", "content_alignment_mode", "construct_blueprint",
      "construct_coverage", "construct_coverage_pool", "polarity_action",
      "polarity_diagnostics", "polarity_diagnostics_status",
      "polarity_diagnostics_pool", "polarity_diagnostics_pool_status"
    ),
    semantic = c(
      "selection_semantic_context", "factor_semantic_diagnostics",
      "embedding_diagnostics", "embedding_policy", "cosine_diagnostics",
      "cosine_adjustment_sensitivity", "representation_stability",
      "representation_evidence_state", "semantic_cluster_consensus",
      "semantic_calibration", "semantic_threshold_mode", "threshold_calibration",
      "nomological_weight", "semantic_score", "semantic_objective_score",
      "cohesion_retention", "semantic_reference_n", "semantic_n_sensitivity",
      "semantic_pair_perturbation_stability", "semantic_resampling_stability",
      "split_half_stability", "semantic_similarity_reduction"
    ),
    structural = c(
      "esem_state", "pfa_esem_discrepancy", "item_structure_diagnostics",
      "matrix_repair_diagnostics", "fit_indices", "esem_attempts",
      "esem_successes", "esem_failures", "pfa_score", "pfa_diagnostics",
      "pfa_objective_score", "pfa_objective_diagnostics",
      "pfa_unit_diagnostics", "reference_sample_size",
      "recommended_validation_n"
    ),
    optimization = c(
      "optimization", "search_objective_score", "proposal_objective_score",
      "final_guided_objective_score", "search_guidance_status",
      "selection_guard_audit", "pool_health", "duplicate_feasibility",
      "evidence_archives", "evidence_archive_states", "evaluation_telemetry"
    ),
    evidence = c(
      "evidence_records", "participant_validation_performed",
      "participant_validation_converged", "interpretation_notice"
    ),
    outputs = "plots",
    provenance = c("resource_plan", "performance", "reproducibility")
  )

  # Preserve future compatibility: newly added top-level fields are never
  # hidden merely because this reporting catalog predates them.
  catalog <- lapply(catalog, function(x) intersect(x, nms))
  assigned <- unique(unlist(catalog, use.names = FALSE))
  extra <- setdiff(nms, assigned)
  if (length(extra)) catalog$other <- extra
  catalog[vapply(catalog, length, integer(1L)) > 0L]
}

.semantica_result_component_index <- function(result) {
  result <- .semantica_raw_result(result)
  groups <- .semantica_result_component_groups(result)
  rows <- lapply(names(groups), function(section) {
    fields <- groups[[section]]
    data.frame(
      section = rep(section, length(fields)),
      component = fields,
      object_class = vapply(fields, function(nm) {
        cls <- class(result[[nm]])
        if (!length(cls)) typeof(result[[nm]]) else paste(cls, collapse = "/")
      }, character(1L)),
      object_length = vapply(fields, function(nm) {
        tryCatch(as.integer(length(result[[nm]]))[1L], error = function(e) NA_integer_)
      }, integer(1L)),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      section = character(), component = character(),
      object_class = character(), object_length = integer(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

.semantica_compact_result_view <- function(result) {
  opt <- if (is.list(result$optimization)) result$optimization else list()
  items <- tryCatch(semantica_items(result), error = function(e) NULL)
  factor_review <- tryCatch(semantica_factor_review(result), error = function(e) NULL)
  evidence <- tryCatch(semantica_evidence_status(result, labels = "both"), error = function(e) NULL)
  models <- tryCatch(semantica_models(result), error = function(e) NULL)
  diag <- tryCatch(.semantica_diagnostic_sections(result), error = function(e) list())
  flags <- names(diag)[vapply(diag, function(x) {
    is.list(x) && identical(x$status %||% NA_character_, "warning")
  }, logical(1L))]

  factors <- character(0L)
  if (is.data.frame(items) && "factor" %in% names(items)) {
    factors <- unique(items$factor[!is.na(items$factor) & nzchar(items$factor)])
  }
  if (!length(factors)) {
    fa <- result$factor_assignment %||% opt$factor_assignment %||% NULL
    if (!is.null(fa)) factors <- unique(as.character(fa[!is.na(fa)]))
  }

  participant <- isTRUE(result$participant_validation_performed) ||
    !is.null(result$response_validation) ||
    !is.null(opt$response_validation)
  selected_n <- if (is.data.frame(items)) nrow(items) else
    length(result$best_items %||% opt$best_items %||% character(0L))
  run_cfg <- if (is.list(result$run_config)) result$run_config else list()
  dimensionality <- result$dimensionality_mode %||%
    opt$dimensionality_mode %||%
    run_cfg$dimensionality %||% NA_character_
  best_objective <- result$best_objective %||% opt$best_objective %||% NA_real_

  out <- list(
    view = "compact",
    interface = .semantica_result_interface(result),
    scale = list(
      selected_items = selected_n,
      factors = factors,
      dimensionality = dimensionality,
      best_observed_objective = best_objective,
      participant_data = participant
    ),
    selected_scale = items,
    review_flags = flags,
    factor_review = factor_review,
    evidence_status = evidence,
    models = models,
    next_steps = c(
      "semantica_items(result) for the final wording",
      "summary(result) for the detailed diagnostic report",
      "semantica_view(result, view = 'advanced') for the complete component map",
      "semantica_diagnostics(result, section = ...) for focused diagnostics",
      "semantica_provenance(result) for reproducibility metadata"
    )
  )
  class(out) <- c("semantica_result_view", "list")
  out
}

#' Present a completed SEMANTICA result without expanding the full raw list
#'
#' `semantica_view()` is a read-only presentation layer for completed SEMANTICA
#' results. Direct `semantica_full_pipeline()` output remains unchanged. For
#' `semantica_run()`, the canonical full result is retained unchanged under the
#' facade's `advanced` field, while the top level is intentionally compact.
#'
#' With `view = "auto"`, results created by [semantica_run()] use the compact
#' scale-development view, while direct [semantica_full_pipeline()] results use
#' the advanced component map. `view = "advanced"` groups all retained
#' top-level fields into a small set of task-oriented sections. Supplying
#' `section` returns the original stored components in that section, without
#' recomputing or simplifying their values. `view = "raw"` returns the
#' canonical full result: it is identical to a direct full-pipeline input and to
#' `result$advanced` for a compact `semantica_run()` result.
#'
#' For `semantica_run()` results, the Environment pane shows a compact six-part
#' facade (`scale`, `items`, `diagnostics`, `plots`, `provenance`, `advanced`). The
#' complete canonical result is retained unchanged under `advanced`. Direct
#' `semantica_full_pipeline()` results remain the full advanced object.
#'
#' @param result A `semantica_full_pipeline_result` or compatible SEMANTICA
#'   high-level result.
#' @param view One of `"auto"`, `"compact"`, `"advanced"`, or `"raw"`.
#' @param section Optional advanced section: `"scale"`, `"generation"`,
#'   `"content"`, `"semantic"`, `"structural"`, `"optimization"`,
#'   `"evidence"`, `"outputs"`, or `"provenance"`. A future-version
#'   `"other"` section is exposed automatically if new top-level fields have
#'   not yet been assigned to a named presentation section.
#' @param x A result-view or result-section object returned by
#'   `semantica_view()`.
#' @param ... Additional print arguments; currently ignored.
#' @return A compact `semantica_result_view`, an advanced component-map view,
#'   a read-only subset of original stored components for one section, or the
#'   canonical full result when `view = "raw"`.
#' @examples
#' \dontrun{
#' result <- semantica_run(...)
#' semantica_view(result)
#' semantica_view(result, view = "advanced")
#' structural <- semantica_view(result, view = "advanced", section = "structural")
#' structural$fit_indices
#' }
#' @export
semantica_view <- function(result, view = c("auto", "compact", "advanced", "raw"), section = NULL) {
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)
  result <- .semantica_raw_result(result)
  view <- match.arg(view)
  if (identical(view, "auto")) {
    view <- if (identical(.semantica_result_interface(result), "regular")) "compact" else "advanced"
  }
  if (identical(view, "raw")) {
    if (!is.null(section)) stop("'section' is only used with view = 'advanced'.", call. = FALSE)
    return(result)
  }
  if (identical(view, "compact")) {
    if (!is.null(section)) stop("'section' is only used with view = 'advanced'.", call. = FALSE)
    return(.semantica_compact_result_view(result))
  }

  groups <- .semantica_result_component_groups(result)
  if (!is.null(section)) {
    if (!is.character(section) || length(section) != 1L || is.na(section) || !nzchar(section)) {
      stop("'section' must be one non-empty character string.", call. = FALSE)
    }
    if (!section %in% names(groups)) {
      stop(sprintf(
        "Unknown result section '%s'. Available: %s.",
        section, paste(names(groups), collapse = ", ")
      ), call. = FALSE)
    }
    out <- result[groups[[section]]]
    attr(out, "semantica_section") <- section
    class(out) <- c("semantica_result_section", "list")
    return(out)
  }

  index <- .semantica_result_component_index(result)
  out <- list(
    view = "advanced",
    interface = .semantica_result_interface(result),
    retained_top_level_components = length(result),
    sections = groups,
    index = index,
    note = paste(
      "This map reorganizes names only. The canonical result is unchanged;",
      "request a section to access its original stored components."
    )
  )
  class(out) <- c("semantica_result_view", "list")
  out
}

#' @rdname semantica_view
#' @export
print.semantica_result_view <- function(x, ...) {
  if (identical(x$view, "advanced")) {
    cat("\nSEMANTICA advanced result map\n")
    cat("=============================\n")
    cat(sprintf("Retained top-level components : %d\n", x$retained_top_level_components %||% 0L))
    cat(sprintf("Presentation sections         : %d\n", length(x$sections %||% list())))
    cat("Canonical result              : unchanged\n\n")
    for (nm in names(x$sections %||% list())) {
      fields <- x$sections[[nm]]
      preview <- paste(utils::head(fields, 5L), collapse = ", ")
      if (length(fields) > 5L) preview <- paste0(preview, ", ...")
      cat(sprintf("  %-13s %2d component%s  %s\n",
                  paste0(nm, ":"), length(fields), if (length(fields) == 1L) " " else "s", preview))
    }
    cat("\nInspect one section with semantica_view(result, view = 'advanced', section = 'structural').\n")
    cat("Use semantica_view(result, view = 'compact') for the scale-development view.\n")
    return(invisible(x))
  }

  scale <- x$scale %||% list()
  cat("\nSEMANTICA overview\n")
  cat("==================\n")
  cat(sprintf("Selected final items : %d\n", scale$selected_items %||% 0L))
  factors <- scale$factors %||% character(0L)
  if (length(factors)) cat(sprintf("Intended factors     : %d (%s)\n", length(factors), paste(factors, collapse = ", ")))
  if (!is.na(scale$dimensionality %||% NA_character_)) {
    cat(sprintf("Dimensionality       : %s\n", scale$dimensionality))
  }
  objective <- tryCatch(
    suppressWarnings(as.numeric(scale$best_observed_objective %||% NA_real_))[1L],
    error = function(e) NA_real_
  )
  if (is.finite(objective)) cat(sprintf("Best observed objective: %.4f\n", objective))
  cat(sprintf("Participant data     : %s\n", if (isTRUE(scale$participant_data)) "supplied" else "not supplied; pre-data proxy evidence only"))
  flags <- x$review_flags %||% character(0L)
  if (length(flags)) {
    cat(sprintf("Review flags         : %s\n", paste(gsub("_", " ", flags, fixed = TRUE), collapse = "; ")))
  } else {
    cat("Review flags         : none surfaced by stored reporting states\n")
  }

  items <- x$selected_scale
  if (is.data.frame(items) && nrow(items)) {
    cat("\nSelected scale\n")
    cat("--------------\n")
    max_print <- min(nrow(items), 12L)
    for (i in seq_len(max_print)) {
      factor <- items$factor[[i]] %||% NA_character_
      text <- items$item_text[[i]] %||% NA_character_
      if (is.na(text) || !nzchar(text)) text <- items$item_id[[i]] %||% ""
      label <- if (!is.na(factor) && nzchar(factor)) paste0("[", factor, "] ") else ""
      cat(sprintf("  %2d. %s%s\n", i, label, text))
    }
    if (nrow(items) > max_print) {
      cat(sprintf("  ... %d more item(s); use semantica_items(result) for the complete table.\n", nrow(items) - max_print))
    }
  }
  cat("\nNext: summary(result) for diagnostics; semantica_view(result, view = 'advanced') for the complete component map.\n")
  invisible(x)
}

#' @rdname semantica_view
#' @export
print.semantica_result_section <- function(x, ...) {
  section <- attr(x, "semantica_section") %||% "result"
  cat(sprintf("\nSEMANTICA %s section\n", section))
  cat(paste(rep("=", nchar(section) + 18L), collapse = ""), "\n", sep = "")
  cat(sprintf("Stored components: %d\n", length(x)))
  if (length(x)) cat(paste0("  - ", names(x), collapse = "\n"), "\n", sep = "")
  cat("Values are the original retained components; use $<component> to inspect one.\n")
  invisible(x)
}

#' Preview a SEMANTICA run before model/API work
#'
#' `semantica_run_plan()` resolves the same factor, ACO, item-count, LLM,
#' generation-style, and worker requests used by [semantica_run()] but performs
#' no provider calls and no analysis.  It is intended to make factor/facet item
#' counts and approximate generation workload visible before paid or lengthy
#' model work starts.
#'
#' @inheritParams semantica_run
#' @return A `semantica_run_plan` list.
#' @export
semantica_run_plan <- function(
    scale_name,
    scale_description,
    factors,
    pool_items = 15L,
    selected_items = NULL,
    overgenerate = 2,
    aco = c("standard", "fast", "full"),
    llm = "openai",
    chat_model = NULL,
    embed_model = NULL,
    workers = "auto",
    language = "English",
    response_format = "5-point Likert",
    item_style = "first-person declarative sentence",
    temperature = 0.8,
    structured_output = c("auto", "numbered", "json"),
    prompts = NULL,
    seed = 1234L,
    progress = c("normal", "detailed", "quiet")) {

  if (!is.character(scale_name) || length(scale_name) != 1L || is.na(scale_name) || !nzchar(trimws(scale_name))) stop("'scale_name' must be one non-empty character string.", call. = FALSE)
  if (!is.character(scale_description) || length(scale_description) != 1L || is.na(scale_description) || !nzchar(trimws(scale_description))) stop("'scale_description' must be one non-empty character string.", call. = FALSE)
  pool_items <- .semantica_assert_positive_integer(pool_items, "pool_items")
  overgenerate <- .semantica_assert_positive_scalar(overgenerate, "overgenerate")
  temperature <- .semantica_assert_nonnegative_scalar(temperature, "temperature")
  structured_output <- match.arg(structured_output)
  progress <- match.arg(progress)
  seed <- .semantica_assert_nonnegative_integer(seed, "seed")
  for (nm in c("language", "response_format", "item_style")) {
    value <- get(nm, inherits = FALSE)
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop(sprintf("'%s' must be one non-empty character string.", nm), call. = FALSE)
    }
  }
  resource_cfg <- semantica_resource_config(cpu_cores = workers)
  factors <- .semantica_run_normalize_factors(factors)
  dimensionality <- .semantica_run_dimensionality(factors)
  if (is.null(selected_items)) selected_items <- if (identical(dimensionality, "unidimensional")) 4L else 3L
  selected_items <- .semantica_assert_positive_integer_vector(selected_items, "selected_items")
  selected_counts <- if (length(selected_items) == 1L) {
    stats::setNames(rep(selected_items, length(factors)), names(factors))
  } else {
    if (length(selected_items) != length(factors)) stop("'selected_items' must be scalar or one value per factor.", call. = FALSE)
    if (!is.null(names(selected_items)) && all(nzchar(names(selected_items)))) selected_items[names(factors)] else stats::setNames(selected_items, names(factors))
  }
  if (anyNA(selected_counts)) stop("Named 'selected_items' must match the factor names.", call. = FALSE)
  if (any(selected_counts > pool_items)) stop("'selected_items' cannot exceed 'pool_items'.", call. = FALSE)
  if (identical(dimensionality, "unidimensional") && any(selected_counts < 4L)) stop("A unidimensional run requires at least 4 selected items.", call. = FALSE)

  if (length(aco) > 1L && is.character(aco)) aco <- aco[[1L]]
  aco_cfg <- .semantica_run_resolve_aco(aco)
  if (identical(dimensionality, "unidimensional")) aco_cfg <- .semantica_run_adapt_unidimensional_aco(aco_cfg)
  llm_resolved <- .semantica_run_resolve_llm(llm, chat_model, embed_model)
  llm_cfg <- llm_resolved$config
  generation_plan <- .expand_generation_plan(factors, n_per_factor = pool_items, n_per_factor_override = TRUE)
  rows <- lapply(generation_plan, function(x) data.frame(
    factor = x$dimension,
    facet = x$facet,
    retained_target = as.integer(x$n_items),
    approximate_raw_target = as.integer(ceiling(x$n_items * overgenerate)),
    stringsAsFactors = FALSE
  ))
  allocation <- do.call(rbind, rows)
  generation_backend <- llm_cfg$backend
  embed_backend <- llm_cfg$embed_backend %||% generation_backend
  gen_spec <- SEMANTICA_BACKENDS[[generation_backend]] %||% llm_cfg$backend_spec %||% list()
  embed_spec <- SEMANTICA_BACKENDS[[embed_backend]] %||% llm_cfg$embed_backend_spec %||%
    (if (identical(embed_backend, generation_backend)) llm_cfg$backend_spec else NULL) %||% list()

  out <- list(
    schema = "semantica-run-plan-1",
    scale_name = trimws(scale_name),
    dimensionality = dimensionality,
    factors = names(factors),
    allocation = allocation,
    retained_candidates = sum(allocation$retained_target),
    approximate_raw_generation_target = sum(allocation$approximate_raw_target),
    selected_counts = selected_counts,
    selected_total = sum(selected_counts),
    generation = list(language = language, response_format = response_format, item_style = item_style,
                      overgenerate = overgenerate, temperature = temperature, structured_output = structured_output),
    aco = list(mode = aco_cfg$mode, description = aco_cfg$description, ants = aco_cfg$ants,
               search_patience = aco_cfg$search_patience, max_total_iter = aco_cfg$max_total_iter),
    backends = list(
      generation = list(name = generation_backend, label = gen_spec$label %||% generation_backend,
                        auth_env = gen_spec$auth_env %||% NULL, model = llm_resolved$chat_model %||% gen_spec$default_chat_model %||% NULL),
      embedding = list(name = embed_backend, label = embed_spec$label %||% embed_backend,
                       auth_env = embed_spec$auth_env %||% NULL, model = llm_resolved$embed_model %||% embed_spec$default_embed_model %||% NULL,
                       embedding_capable = isTRUE(embed_spec$has_embed))
    ),
    workers_requested = resource_cfg$cpu_cores,
    execution_spec = list(
      scale_name = trimws(scale_name),
      scale_description = scale_description,
      factors = factors,
      pool_items = pool_items,
      selected_items = selected_counts,
      overgenerate = overgenerate,
      prompts = prompts,
      aco = aco_cfg,
      llm = .semantica_sanitize_config_provenance(llm_cfg),
      chat_model = llm_resolved$chat_model,
      embed_model = llm_resolved$embed_model,
      seed = seed,
      workers = workers,
      language = language,
      response_format = response_format,
      item_style = item_style,
      temperature = temperature,
      structured_output = structured_output,
      progress = progress
    ),
    note = "Raw generation target is an approximate planned workload; retries, deduplication, provider behavior, and deficit-aware generation can change actual requests."
  )
  class(out) <- c("semantica_run_plan", "list")
  out
}

#' @export
print.semantica_run_plan <- function(x, ...) {
  cat("\nSEMANTICA run plan (no model calls performed)\n")
  cat("=============================================\n")
  cat(sprintf("Scale               : %s\n", x$scale_name))
  cat(sprintf("Factors             : %d (%s)\n", length(x$factors), paste(x$factors, collapse = ", ")))
  cat(sprintf("Retained candidates : %d total\n", x$retained_candidates))
  cat(sprintf("Approx. raw target  : %d generated candidates\n", x$approximate_raw_generation_target))
  cat(sprintf("Final form          : %d items total (%s)\n", x$selected_total,
              paste(sprintf("%s=%d", names(x$selected_counts), x$selected_counts), collapse = ", ")))
  cat(sprintf("Generation          : %s | %s | %s\n", x$generation$language, x$generation$response_format, x$generation$item_style))
  cat(sprintf("Generation backend  : %s | model %s\n", x$backends$generation$name, x$backends$generation$model %||% "provider default / user supplied"))
  cat(sprintf("Embedding backend   : %s | model %s\n", x$backends$embedding$name, x$backends$embedding$model %||% "provider default / user supplied"))
  if (!isTRUE(x$backends$embedding$embedding_capable)) cat("Embedding readiness : NOT READY -- selected embedding backend is not embedding-capable.\n")
  cat(sprintf("ACO preset          : %s -- %s\n", x$aco$mode, x$aco$description))
  cat(sprintf("Workers requested   : %s\n", paste(x$workers_requested, collapse = ",")))
  if (nrow(x$allocation) > length(x$factors)) {
    cat("\nFacet allocation\n")
    print(x$allocation, row.names = FALSE)
  }
  cat("\n", x$note, "\n", sep = "")
  invisible(x)
}

.semantica_backend_row <- function(name) {
  spec <- SEMANTICA_BACKENDS[[name]]
  if (is.null(spec)) return(NULL)
  remote <- grepl("^https://", spec$chat_url %||% spec$embed_url %||% "")
  data.frame(
    backend = name,
    type = if (spec$protocol %in% c("python_hf", "python_llamacpp")) "local Python" else if (remote) "cloud/API" else "local/server",
    auth = spec$auth_env %||% "none",
    chat = !is.null(spec$chat_url) || spec$protocol %in% c("python_hf", "python_llamacpp"),
    embeddings = isTRUE(spec$has_embed),
    default_chat_model = spec$default_chat_model %||% "user supplied",
    default_embed_model = spec$default_embed_model %||% "not available / user supplied",
    stringsAsFactors = FALSE
  )
}

#' Check whether a backend configuration is ready to run
#'
#' Performs a user-oriented configuration check before an expensive SEMANTICA
#' run.  With `probe = FALSE` it only checks registry capabilities and credential
#' presence.  With `probe = TRUE` it additionally delegates to the existing
#' backend preflight machinery; it does not generate or embed any items.
#'
#' @param llm,chat_model,embed_model Same backend/model specification as
#'   [semantica_run()].
#' @param probe Logical; contact configured provider/local model registries.
#' @param verify_models Logical; when probing, verify model IDs where the
#'   provider exposes a registry.
#' @param timeout_s Positive preflight timeout in seconds.
#' @return A `semantica_setup_check` list.
#' @export
semantica_check_setup <- function(llm = "openai", chat_model = NULL, embed_model = NULL,
                                  probe = FALSE, verify_models = TRUE, timeout_s = 10L) {
  probe <- .semantica_assert_flag(probe, "probe")
  verify_models <- .semantica_assert_flag(verify_models, "verify_models")
  timeout_s <- .semantica_assert_positive_scalar(timeout_s, "timeout_s")
  rr <- .semantica_run_resolve_llm(llm, chat_model, embed_model)
  cfg <- rr$config
  gen_backend <- cfg$backend
  embed_backend <- cfg$embed_backend %||% gen_backend
  gen_spec <- SEMANTICA_BACKENDS[[gen_backend]] %||% cfg$backend_spec %||% NULL
  embed_spec <- SEMANTICA_BACKENDS[[embed_backend]] %||% cfg$embed_backend_spec %||%
    (if (identical(embed_backend, gen_backend)) cfg$backend_spec else NULL)

  issues <- character(0L)
  actions <- character(0L)
  if (is.null(gen_spec) && is.null(cfg$backend_spec)) issues <- c(issues, sprintf("Generation backend '%s' is not registered.", gen_backend))
  if (is.null(embed_spec) && is.null(cfg$embed_backend_spec)) issues <- c(issues, sprintf("Embedding backend '%s' is not registered.", embed_backend))
  if (!is.null(embed_spec) && !isTRUE(embed_spec$has_embed)) {
    issues <- c(issues, sprintf("Backend '%s' does not provide embeddings in SEMANTICA.", embed_backend))
    actions <- c(actions, "Set embed_backend to an embedding-capable backend such as openai, ollama, llamacpp, generic_openai, python_hf, or python_llamacpp.")
  }

  credential_state <- function(spec, explicit) {
    if (is.null(spec) || is.null(spec$auth_env) || !nzchar(spec$auth_env)) {
      return(list(required = FALSE, env = NA_character_, present = TRUE))
    }
    explicit_value <- if (is.null(explicit) || !length(explicit)) NA_character_ else as.character(explicit)[1L]
    explicit_present <- !is.na(explicit_value) && nzchar(explicit_value)
    env_present <- nzchar(Sys.getenv(spec$auth_env, unset = ""))
    list(required = TRUE, env = spec$auth_env, present = explicit_present || env_present)
  }
  gen_auth <- credential_state(gen_spec, cfg$api_key)
  emb_explicit <- cfg$embed_api_key %||% if (identical(embed_backend, gen_backend)) cfg$api_key else NULL
  embed_auth <- credential_state(embed_spec, emb_explicit)
  if (isTRUE(gen_auth$required) && !isTRUE(gen_auth$present)) {
    issues <- c(issues, sprintf("Generation credential is missing (%s).", gen_auth$env))
    actions <- c(actions, sprintf("Set %s in .Renviron or pass the credential through semantica_llm_config().", gen_auth$env))
  }
  if (isTRUE(embed_auth$required) && !isTRUE(embed_auth$present)) {
    issues <- c(issues, sprintf("Embedding credential is missing (%s).", embed_auth$env))
    actions <- c(actions, sprintf("Set %s in .Renviron or configure a different embedding backend.", embed_auth$env))
  }

  chat_preflight <- NULL
  embed_preflight <- NULL
  probe_error <- character(0L)
  if (isTRUE(probe) && !length(issues)) {
    common_chat <- list(
      gguf_path = cfg$gguf_path, hf_token = cfg$hf_token,
      embedding_device = cfg$embedding_device, chat_device = cfg$chat_device,
      device_map = cfg$device_map, gpu_layers = cfg$gpu_layers,
      model_precision = cfg$model_precision,
      retry_max_tries = cfg$retry_max_tries, retry_on_failure = cfg$retry_on_failure,
      preflight = FALSE, verbose = FALSE
    )
    chat_session <- tryCatch(do.call(semantica_connect, c(list(
      backend = gen_backend, api_key = cfg$api_key, chat_model = rr$chat_model,
      base_url = cfg$base_url, timeout_s = timeout_s, purpose = "chat",
      backend_spec = cfg$backend_spec
    ), common_chat)), error = function(e) e)
    if (inherits(chat_session, "error")) {
      probe_error <- c(probe_error, paste("Generation preflight:", conditionMessage(chat_session)))
    } else {
      chat_preflight <- tryCatch(semantica_backend_preflight(chat_session, verify_models = verify_models, strict = FALSE, timeout_s = timeout_s), error = function(e) e)
      if (inherits(chat_preflight, "error")) probe_error <- c(probe_error, paste("Generation preflight:", conditionMessage(chat_preflight)))
    }

    embed_key <- cfg$embed_api_key %||% if (identical(embed_backend, gen_backend)) cfg$api_key else NULL
    embed_hf <- cfg$embed_hf_token %||% cfg$hf_token
    common_embed <- list(
      gguf_path = cfg$gguf_path, hf_token = embed_hf,
      embedding_device = cfg$embedding_device, chat_device = "auto",
      device_map = NULL, gpu_layers = cfg$gpu_layers,
      model_precision = cfg$model_precision,
      retry_max_tries = cfg$retry_max_tries, retry_on_failure = cfg$retry_on_failure,
      preflight = FALSE, verbose = FALSE
    )
    embed_session <- tryCatch(do.call(semantica_connect, c(list(
      backend = embed_backend, api_key = embed_key, embed_model = rr$embed_model,
      base_url = cfg$embed_base_url %||% cfg$base_url, timeout_s = timeout_s, purpose = "embed",
      backend_spec = cfg$embed_backend_spec
    ), common_embed)), error = function(e) e)
    if (inherits(embed_session, "error")) {
      probe_error <- c(probe_error, paste("Embedding preflight:", conditionMessage(embed_session)))
    } else {
      embed_preflight <- tryCatch(semantica_backend_preflight(embed_session, verify_models = verify_models, strict = FALSE, timeout_s = timeout_s), error = function(e) e)
      if (inherits(embed_preflight, "error")) probe_error <- c(probe_error, paste("Embedding preflight:", conditionMessage(embed_preflight)))
    }
  }

  preflight_warnings <- c(
    if (is.list(chat_preflight)) chat_preflight$warnings %||% character(0L) else character(0L),
    if (is.list(embed_preflight)) embed_preflight$warnings %||% character(0L) else character(0L),
    probe_error
  )
  static_ready <- !length(issues)
  probe_ok <- if (!isTRUE(probe)) {
    NA
  } else {
    chat_ok <- is.list(chat_preflight) && isTRUE(chat_preflight$ok)
    embed_ok <- is.list(embed_preflight) && isTRUE(embed_preflight$ok)
    isTRUE(chat_ok) && isTRUE(embed_ok) && !length(probe_error)
  }
  ready <- static_ready && (!isTRUE(probe) || isTRUE(probe_ok))
  out <- list(
    ready = ready,
    static_ready = static_ready,
    probe_ok = probe_ok,
    probe_performed = probe,
    generation = list(backend = gen_backend, model = rr$chat_model %||% gen_spec$default_chat_model %||% NULL,
                      credential = gen_auth, preflight = if (is.list(chat_preflight)) chat_preflight else NULL),
    embedding = list(backend = embed_backend, model = rr$embed_model %||% embed_spec$default_embed_model %||% NULL,
                     credential = embed_auth, capable = isTRUE(embed_spec$has_embed), preflight = if (is.list(embed_preflight)) embed_preflight else NULL),
    issues = unique(issues),
    warnings = unique(preflight_warnings),
    actions = unique(actions)
  )
  class(out) <- c("semantica_setup_check", "list")
  out
}

#' @export
print.semantica_setup_check <- function(x, ...) {
  cat("\nSEMANTICA setup check\n")
  cat("=====================\n")
  cat(sprintf("Generation : %s | %s\n", x$generation$backend, x$generation$model %||% "provider default / user supplied"))
  if (isTRUE(x$generation$credential$required)) cat(sprintf("  credential %s : %s\n", x$generation$credential$env, if (isTRUE(x$generation$credential$present)) "present" else "MISSING"))
  cat(sprintf("Embeddings : %s | %s | %s\n", x$embedding$backend, x$embedding$model %||% "provider default / user supplied", if (isTRUE(x$embedding$capable)) "supported" else "NOT SUPPORTED"))
  if (isTRUE(x$embedding$credential$required)) cat(sprintf("  credential %s : %s\n", x$embedding$credential$env, if (isTRUE(x$embedding$credential$present)) "present" else "MISSING"))
  cat(sprintf("Static configuration : %s\n", if (isTRUE(x$static_ready %||% !length(x$issues))) "READY" else "NEEDS CHANGES"))
  if (isTRUE(x$probe_performed)) {
    cat(sprintf("Registry/model probe : %s\n", if (isTRUE(x$probe_ok)) "PASSED" else "ATTENTION -- see warnings below"))
  } else {
    cat("Registry/model probe : not performed (set probe = TRUE to contact the configured services)\n")
  }
  if (length(x$issues)) {
    cat("\nBlocking issues\n")
    for (z in x$issues) cat("  - ", z, "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("\nPreflight warnings\n")
    for (z in x$warnings) cat("  - ", z, "\n", sep = "")
  }
  if (length(x$actions)) {
    cat("\nSuggested actions\n")
    for (z in x$actions) cat("  - ", z, "\n", sep = "")
  }
  cat(sprintf("\nSetup status: %s\n", if (isTRUE(x$ready)) "READY" else if (length(x$issues)) "NOT READY -- fix blocking issues" else "ATTENTION -- static configuration is valid, but the live probe did not fully pass"))
  invisible(x)
}

#' Inspect SEMANTICA's persistent embedding cache
#'
#' @param result Optional SEMANTICA result. When supplied, cache hit/miss
#'   telemetry and the exact cache directory recorded for that run are included.
#' @param cache_dir Optional cache directory; `NULL` uses the run's recorded
#'   directory when available, otherwise SEMANTICA's OS-appropriate default.
#' @return A named list containing cache path, entry count, size, and optional
#'   run telemetry.
#' @export
semantica_cache_info <- function(result = NULL, cache_dir = NULL) {
  diag <- if (is.list(result)) result$generation$embedding_diagnostics %||% result$embedding_diagnostics %||% list() else list()
  path <- cache_dir %||% diag$cache_dir %||% .semantica_default_cache_dir()
  path <- path.expand(path)
  files <- if (dir.exists(path)) list.files(path, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE) else character(0L)
  size <- if (length(files)) sum(file.info(files)$size, na.rm = TRUE) else 0
  list(
    cache_dir = normalizePath(path, winslash = "/", mustWork = FALSE),
    exists = dir.exists(path),
    entries = length(files),
    bytes = unname(size),
    megabytes = unname(size / 1024^2),
    run_cache_enabled = diag$cache_enabled %||% NA,
    run_hits = diag$cache_hits %||% NA_integer_,
    run_misses = diag$cache_misses %||% NA_integer_,
    run_hit_rate = diag$cache_hit_rate %||% NA_real_
  )
}

#' Clear SEMANTICA's persistent embedding cache
#'
#' Deletes only hashed `.rds` embedding-cache entries under SEMANTICA's
#' configured cache directory. Other files in a user-supplied directory are
#' left untouched. The explicit `confirm = TRUE` requirement prevents accidental
#' deletion.
#'
#' @param cache_dir Optional cache directory; `NULL` uses SEMANTICA's default.
#' @param confirm Must be `TRUE` to delete cache contents.
#' @return Invisibly returns the cache directory.
#' @export
semantica_clear_cache <- function(cache_dir = NULL, confirm = FALSE) {
  confirm <- .semantica_assert_flag(confirm, "confirm")
  path <- normalizePath(path.expand(cache_dir %||% .semantica_default_cache_dir()), winslash = "/", mustWork = FALSE)
  if (!isTRUE(confirm)) stop("Cache deletion was not performed. Re-run with confirm = TRUE after checking semantica_cache_info().", call. = FALSE)
  if (dir.exists(path)) {
    # SEMANTICA's cache writer stores hashed .rds entries. Delete only those
    # entries, never arbitrary neighboring files in a user-supplied directory.
    targets <- list.files(path, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
    if (length(targets)) unlink(targets, force = TRUE)
    subdirs <- setdiff(list.dirs(path, recursive = TRUE, full.names = TRUE), path)
    if (length(subdirs)) {
      subdirs <- subdirs[order(nchar(subdirs), decreasing = TRUE)]
      for (d in subdirs) {
        if (dir.exists(d) && length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0L) unlink(d, recursive = FALSE, force = TRUE)
      }
    }
  }
  message("SEMANTICA embedding cache entries cleared: ", path)
  invisible(path)
}

#' Plot a high-level SEMANTICA result
#'
#' `plot(result)` defaults to the summary plot already stored by the high-level
#' pipeline.  `which = "all"` delegates to [semantica_plot_all()] using the
#' optimizer result, cosine matrix, and metadata already contained in `result`.
#'
#' @param x A `semantica_full_pipeline_result`.
#' @param which Plot selection. `"summary"` returns the high-level summary plot;
#'   `"all"` or one or more names accepted by [semantica_plot_all()] return a
#'   named plot list.
#' @param ... Additional arguments passed to [semantica_plot_all()] for non-summary selections.
#' @return A plot object for `"summary"`, or a named plot list otherwise.
#' @export
plot.semantica_full_pipeline_result <- function(x, which = "summary", ...) {
  which <- unique(as.character(which))
  if (length(which) == 1L && identical(which, "summary")) {
    p <- x$plots$plot_summary_of_results %||% NULL
    if (is.null(p)) {
      p <- plot_summary_of_results(
        x$optimization %||% x,
        x$generation$cosine_sim_matrix %||% NULL
      )
    }
    return(p)
  }
  semantica_plot_all(x, which = which, ...)
}


# Internal defaults used only for display-level configuration comparison.
.semantica_user_config_defaults <- function() {
  list(
    llm = semantica_llm_config(),
    item_counts = semantica_item_count_config(),
    generation = semantica_generation_config(),
    resources = list(requested = semantica_resource_config(), effective = NULL),
    compute = semantica_compute_config(),
    quality = semantica_quality_config(),
    pfa = semantica_pfa_config(),
    fit_calibration = semantica_fit_calibration_config(),
    diagnostics = semantica_diagnostics_config(),
    esem = semantica_esem_config(),
    plots = semantica_plot_config(),
    seed = 1234L
  )
}

.semantica_recursive_diff <- function(x, baseline = NULL) {
  if (is.null(baseline)) return(x)
  if (is.list(x) && is.list(baseline)) {
    out <- list()
    for (nm in names(x)) {
      if (!nm %in% names(baseline)) {
        out[[nm]] <- x[[nm]]
      } else {
        d <- .semantica_recursive_diff(x[[nm]], baseline[[nm]])
        if (!is.null(d) && !(is.list(d) && length(d) == 0L)) out[[nm]] <- d
      }
    }
    return(out)
  }
  if (identical(x, baseline)) return(NULL)
  x
}

.semantica_safe_display_value <- function(name, value) {
  sensitive <- grepl("key|token|secret|password|credential", name, ignore.case = TRUE)
  if (sensitive) return(if (is.null(value) || !length(value) || !nzchar(as.character(value)[1L])) "<not set>" else "<configured>")
  if (is.null(value)) return("NULL")
  if (is.function(value)) return("<function>")
  if (is.environment(value)) return("<environment>")
  if (is.list(value)) return(sprintf("<list: %d field%s>", length(value), if (length(value) == 1L) "" else "s"))
  if (length(value) > 4L) return(paste0(paste(utils::head(as.character(value), 4L), collapse = ", "), ", ..."))
  paste(as.character(value), collapse = ", ")
}

#' Print SEMANTICA configuration objects safely
#'
#' Configuration objects are summarized without printing API keys, access
#' tokens, passwords, or credential-like fields. The underlying object is not
#' modified.
#'
#' @param x A SEMANTICA configuration object.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
print.semantica_config <- function(x, ...) {
  cls <- class(x)[grepl("^semantica_.*_config$", class(x))][1L] %||% "semantica_config"
  label <- gsub("^semantica_|_config$", "", cls)
  label <- gsub("_", " ", label, fixed = TRUE)
  cat(sprintf("\n<SEMANTICA %s configuration>\n", label))
  for (nm in names(x)) {
    cat(sprintf("  %-28s %s\n", paste0(nm, ":"), .semantica_safe_display_value(nm, x[[nm]])))
  }
  invisible(x)
}

#' Execute a previously inspected SEMANTICA run plan
#'
#' Delegates directly to [semantica_run()] using the execution specification
#' stored by [semantica_run_plan()]. Credential values are never stored in a
#' plan; pass `llm` to supply a credential-bearing configuration when it is not
#' available through environment variables.
#'
#' @param plan A `semantica_run_plan`.
#' @param llm Optional backend/configuration override, commonly used to provide
#'   credentials or a custom endpoint at execution time.
#' @param ... Named overrides passed to [semantica_run()].
#' @return A compact `semantica_run_result`; its `advanced` field retains the
#'   complete canonical `semantica_full_pipeline_result`.
#' @export
semantica_execute <- function(plan, llm = NULL, ...) {
  if (!inherits(plan, "semantica_run_plan") || !is.list(plan$execution_spec)) {
    stop("'plan' must be a run plan created by semantica_run_plan().", call. = FALSE)
  }
  args <- plan$execution_spec
  contains_omitted_component <- function(x) {
    if (is.character(x) && any(x == "<non-serializable config component omitted>", na.rm = TRUE)) return(TRUE)
    if (!is.list(x)) return(FALSE)
    any(vapply(x, contains_omitted_component, logical(1L)))
  }
  if (is.null(llm) && contains_omitted_component(args$llm)) {
    stop(
      "This run plan used custom non-serializable backend components. Pass the original backend/configuration through 'llm =' when executing the plan.",
      call. = FALSE
    )
  }
  if (!is.null(llm)) args$llm <- llm
  dots <- list(...)
  if (length(dots)) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) stop("All execution overrides in '...' must be named.", call. = FALSE)
    args[names(dots)] <- dots
  }
  do.call(semantica_run, args)
}

#' Inspect model and execution provenance for a SEMANTICA result
#'
#' Collects provenance already recorded by the pipeline into one stable object;
#' no diagnostics are recomputed.
#'
#' @param result A SEMANTICA result.
#' @param x A `semantica_provenance` object passed to its print method.
#' @param ... Additional arguments to the print method; currently ignored.
#' @return A `semantica_provenance` list.
#' @export
semantica_provenance <- function(result) {
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)
  rep <- result$reproducibility %||% result$optimization$reproducibility %||% list()
  gen <- result$generation_provenance %||% result$generation$generation_provenance %||% list()
  emb <- result$embedding_diagnostics %||% result$generation$embedding_diagnostics %||% list()
  out <- list(
    package_version = rep$semantica_version %||% rep$package_version %||% result$package_version %||% NA_character_,
    result_schema = result$result_schema %||% rep$result_schema %||% NA_character_,
    resolved_config_schema = rep$resolved_config_schema %||% NA_character_,
    resolved_config_hash = rep$resolved_config_hash %||% NA_character_,
    models = tryCatch(semantica_models(result), error = function(e) NULL),
    analysis_seed = rep$master_seed %||% rep$seed %||% rep$analysis_seed %||% result$run_config$seed %||% NA_integer_,
    generation_seed_status = gen$seed_control_status %||% gen$generation_seed_status %||% NA_character_,
    generation_spec_fingerprint = gen$generation_spec_fingerprint %||% gen$generation_contract_fingerprint %||% NA_character_,
    generation_replay_plan_fingerprint = gen$generation_replay_plan_fingerprint %||% NA_character_,
    item_pool_fingerprint = gen$item_pool_fingerprint %||% NA_character_,
    embedding_cache = list(enabled = emb$cache_enabled %||% NA, hits = emb$cache_hits %||% NA_integer_, misses = emb$cache_misses %||% NA_integer_, cache_dir = emb$cache_dir %||% NA_character_),
    resources = result$resource_plan %||% result$performance$resource %||% result$optimization$resource_plan %||% NULL,
    posthoc_participant_validation = rep$posthoc_participant_validation %||% NULL
  )
  class(out) <- c("semantica_provenance", "list")
  out
}

#' @rdname semantica_provenance
#' @export
print.semantica_provenance <- function(x, ...) {
  cat("\nSEMANTICA provenance\n===================\n")
  cat(sprintf("Package version       : %s\n", x$package_version %||% "unknown"))
  cat(sprintf("Result schema         : %s\n", x$result_schema %||% "not recorded"))
  cat(sprintf("Config hash           : %s\n", x$resolved_config_hash %||% "not recorded"))
  if (is.data.frame(x$models)) print(x$models, row.names = FALSE)
  cat(sprintf("Analysis seed         : %s\n", x$analysis_seed %||% "not recorded"))
  cat(sprintf("Item-pool fingerprint : %s\n", x$item_pool_fingerprint %||% "not recorded"))
  invisible(x)
}

#' Describe a SEMANTICA result object
#'
#' @param result A SEMANTICA result.
#' @param x A `semantica_result_info` object passed to its print method.
#' @param ... Additional arguments to the print method; currently ignored.
#' @return A `semantica_result_info` list describing class, version/schema,
#'   originating interface, visible facade size, canonical component count,
#'   presentation-section counts, and retained evidence/components.
#' @export
semantica_result_info <- function(result) {
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.", call. = FALSE)
  raw <- .semantica_raw_result(result)
  p <- tryCatch(semantica_provenance(raw), error = function(e) list())
  component_groups <- .semantica_result_component_groups(raw)
  is_facade <- .semantica_is_run_facade(result)
  out <- list(
    class = class(result),
    package_version = p$package_version %||% NA_character_,
    result_schema = p$result_schema %||% NA_character_,
    resolved_config_schema = p$resolved_config_schema %||% NA_character_,
    interface = .semantica_result_interface(raw),
    top_level_components = length(result),
    canonical_top_level_components = length(raw),
    compact_facade = is_facade,
    presentation_sections = vapply(component_groups, length, integer(1L)),
    participant_data = isTRUE(raw$participant_validation_performed) || !is.null(raw$response_validation) || !is.null(raw$optimization$response_validation),
    embeddings_retained = !is.null(raw$generation$embeddings) || !is.null(raw$embeddings),
    cosine_matrix_retained = !is.null(raw$generation$cosine_sim_matrix) || !is.null(raw$cosine_sim_matrix),
    bundle_ready = inherits(result, "semantica_full_pipeline_result") || inherits(result, "semantica_result")
  )
  class(out) <- c("semantica_result_info", "list")
  out
}

#' @rdname semantica_result_info
#' @export
print.semantica_result_info <- function(x, ...) {
  cat("\nSEMANTICA result information\n===========================\n")
  cat(sprintf("Class               : %s\n", paste(x$class, collapse = ", ")))
  cat(sprintf("Package version     : %s\n", x$package_version %||% "not recorded"))
  cat(sprintf("Result schema       : %s\n", x$result_schema %||% "not recorded"))
  cat(sprintf("Interface           : %s\n", x$interface %||% "advanced"))
  if (isTRUE(x$compact_facade)) {
    cat(sprintf("Visible top level   : %d regular-user groups\n", x$top_level_components %||% 0L))
    cat(sprintf("Canonical result    : %d components retained under $advanced\n", x$canonical_top_level_components %||% 0L))
  } else {
    cat(sprintf("Top-level components: %d retained\n", x$top_level_components %||% 0L))
  }
  if (length(x$presentation_sections %||% integer(0L))) {
    cat(sprintf("Presentation groups : %d (use semantica_view())\n", length(x$presentation_sections)))
  }
  cat(sprintf("Participant data    : %s\n", if (isTRUE(x$participant_data)) "present" else "not attached"))
  cat(sprintf("Cosine matrix       : %s\n", if (isTRUE(x$cosine_matrix_retained)) "retained" else "not retained"))
  invisible(x)
}

.semantica_prefix_columns <- function(x, prefix, keys = c("item", "item_id", "factor")) {
  if (!is.data.frame(x)) return(NULL)
  names(x) <- ifelse(names(x) %in% keys, names(x), paste0(prefix, names(x)))
  x
}

#' Consolidate item-level diagnostics for review
#'
#' Rearranges already-computed item metadata, semantic alignment, ESEM and PFA
#' diagnostics into one table. It introduces no new score or threshold.
#'
#' @param result A SEMANTICA result.
#' @param scope `"selected"` or `"pool"`.
#' @return A data frame with one row per reviewed item.
#' @export
semantica_item_review <- function(result, scope = c("selected", "pool")) {
  scope <- match.arg(scope)
  selected_ids <- as.character(result$best_items %||% result$optimization$best_items %||% character())
  if (identical(scope, "selected")) {
    base <- semantica_items(result, details = TRUE)
  } else {
    meta <- result$generated_item_metadata %||% result$generation$generated_item_metadata %||% result$generation$item_metadata %||% NULL
    if (!is.data.frame(meta)) stop("Candidate-pool metadata are not retained in this result.", call. = FALSE)
    id_col <- .semantica_pick_column(meta, c("item_id", "ID", "id")); factor_col <- .semantica_pick_column(meta, c("factor", "Dimension", "dimension", "type")); facet_col <- .semantica_pick_column(meta, c("facet", "Facet")); text_col <- .semantica_pick_column(meta, c("item_text", "item", "text"))
    base <- data.frame(item_id = if (!is.null(id_col)) as.character(meta[[id_col]]) else NA_character_, factor = if (!is.null(factor_col)) as.character(meta[[factor_col]]) else NA_character_, facet = if (!is.null(facet_col)) as.character(meta[[facet_col]]) else NA_character_, item_text = if (!is.null(text_col)) as.character(meta[[text_col]]) else NA_character_, stringsAsFactors = FALSE)
    extra <- setdiff(names(meta), unique(c(id_col, factor_col, facet_col, text_col))); if (length(extra)) base <- cbind(base, meta[extra])
  }
  base$selected <- base$item_id %in% selected_ids
  opt <- result$optimization %||% result
  merge_diag <- function(base, diag, prefix) {
    if (!is.data.frame(diag) || !nrow(diag)) return(base)
    key <- .semantica_pick_column(diag, c("item_id", "item", "ID", "id")); if (is.null(key)) return(base)
    d <- diag
    if (!identical(key, "item_id")) {
      d$item_id <- as.character(d[[key]])
      d[[key]] <- NULL
    } else {
      d$item_id <- as.character(d$item_id)
    }
    if ("factor" %in% names(d)) d$factor <- NULL
    names(d)[names(d) != "item_id"] <- paste0(prefix, names(d)[names(d) != "item_id"])
    merge(base, d, by = "item_id", all.x = TRUE, sort = FALSE)
  }
  base <- merge_diag(base, opt$item_structure_diagnostics %||% NULL, "esem_")
  base <- merge_diag(base, opt$pfa_diagnostics$item_diagnostics %||% NULL, "pfa_")
  if (length(selected_ids)) base <- base[order(match(base$item_id, selected_ids), na.last = TRUE), , drop = FALSE]
  rownames(base) <- NULL
  base
}

#' Consolidate factor-level diagnostics for review
#'
#' @param result A SEMANTICA result.
#' @return A data frame combining already-computed semantic separation, content
#'   coverage and structural factor summaries when available.
#' @export
semantica_factor_review <- function(result) {
  opt <- result$optimization %||% result
  sem <- result$factor_semantic_diagnostics %||% opt$factor_semantic_diagnostics %||% data.frame()
  if (!is.data.frame(sem) || !nrow(sem)) {
    items <- semantica_items(result); sem <- data.frame(factor = unique(items$factor[!is.na(items$factor)]), stringsAsFactors = FALSE)
  }
  cov <- result$construct_coverage$coverage_table %||% result$construct_coverage$table %||% NULL
  struc <- opt$structure_diagnostics$factor_diagnostics %||% opt$esem_result$structure_diagnostics$factor_diagnostics %||% NULL
  pfa <- opt$pfa_diagnostics$factor_diagnostics %||% NULL
  merge_factor <- function(base, x, prefix) {
    if (!is.data.frame(x) || !nrow(x) || !"factor" %in% names(x)) return(base)
    y <- x; names(y)[names(y) != "factor"] <- paste0(prefix, names(y)[names(y) != "factor"])
    merge(base, y, by = "factor", all = TRUE, sort = FALSE)
  }
  out <- sem
  out <- merge_factor(out, cov, "coverage_")
  out <- merge_factor(out, struc, "esem_")
  out <- merge_factor(out, pfa, "pfa_")
  rownames(out) <- NULL
  out
}

#' Extract stable diagnostic sections from a SEMANTICA result
#'
#' @param result A SEMANTICA result.
#' @param section One of `"all"`, `"items"`, `"factors"`, `"representation"`,
#'   `"selection"`, `"esem"`, `"pfa"`, `"aco"`, `"participant"`, or
#'   `"performance"`.
#' @return The requested already-computed diagnostic object.
#' @export
semantica_diagnostics <- function(result, section = "all") {
  section <- match.arg(section, choices = c("all", "items", "factors", "representation", "selection", "esem", "pfa", "aco", "participant", "performance"))
  opt <- result$optimization %||% result
  getter <- function(name) switch(name,
    items = semantica_item_review(result, "selected"),
    factors = semantica_factor_review(result),
    representation = list(stability = result$representation_stability %||% result$generation$representation_stability %||% NULL, evidence_state = result$representation_evidence_state %||% result$generation$representation_evidence_state %||% NULL, embedding = result$embedding_diagnostics %||% result$generation$embedding_diagnostics %||% NULL),
    selection = list(context = result$selection_semantic_context %||% opt$selection_semantic_context %||% NULL, guard_audit = result$selection_guard_audit %||% opt$selection_guard_audit %||% NULL, pool_health = result$pool_health %||% opt$pool_health %||% NULL),
    esem = list(state = result$esem_state %||% opt$esem_state %||% NULL, result = opt$esem_result %||% NULL, structure = opt$structure_diagnostics %||% NULL, telemetry = opt$evaluation_telemetry %||% NULL),
    pfa = list(score = result$pfa_score %||% opt$pfa_score %||% NA_real_, diagnostics = result$pfa_diagnostics %||% opt$pfa_diagnostics %||% NULL, discrepancy = result$pfa_esem_discrepancy %||% opt$pfa_esem_discrepancy %||% NULL),
    aco = list(best_items = opt$best_items %||% result$best_items %||% NULL, objective_context = opt$objective_context %||% NULL, termination_reason = opt$termination_reason %||% NULL, total_iterations = opt$total_iterations %||% NULL, archive = opt$elite_archive %||% NULL, history = opt$solution_history %||% NULL),
    participant = opt$response_validation %||% result$response_validation %||% NULL,
    performance = result$performance %||% opt$performance %||% NULL
  )
  if (!identical(section, "all")) return(getter(section))
  names_all <- c("items", "factors", "representation", "selection", "esem", "pfa", "aco", "participant", "performance")
  stats::setNames(lapply(names_all, function(nm) tryCatch(getter(nm), error = function(e) structure(list(message = conditionMessage(e), condition_class = class(e)[1L]), class = "semantica_diagnostic_unavailable"))), names_all)
}

.semantica_evidence_human_label <- function(x) {
  map <- c(
    not_established = "Not established",
    participant_model_attempted_failed = "Participant model attempted but did not converge successfully",
    participant_model_converged_inadmissible = "Participant model converged but was not admissible",
    participant_model_converged_admissible = "Participant model converged and was admissible",
    participant_model_converged = "Participant model converged",
    participant_data_supplied = "Participant data supplied",
    not_assessed = "Not assessed",
    not_applicable_unidimensional = "Not applicable for a single intended factor",
    available_as_proxy = "Available as an embedding-derived proxy",
    available_as_content_screen = "Available as a content screen",
    semantic_definition_screen = "Semantic definition coverage available",
    metadata_coverage_only = "Metadata coverage only",
    unavailable = "Unavailable",
    not_established_by_semantic_proxy = "Not established by semantic-proxy evidence"
  )
  out <- unname(map[as.character(x)])
  out[is.na(out)] <- gsub("_", " ", as.character(x)[is.na(out)], fixed = TRUE)
  out
}

#' Attach participant-response validation to an existing selected scale
#'
#' Uses the same response-data ESEM helpers and stored selected-item structure as
#' the full pipeline. It does not regenerate items, recompute embeddings, or rerun
#' ACO, and it does not change the selected scale.
#'
#' @param result A completed high-level SEMANTICA result.
#' @param responses Data frame or matrix with one column for every selected item ID.
#' @param ordered Optional ordered-item specification passed to lavaan.
#' @param verbose Logical; print a concise validation message.
#' @return A copy of `result` with participant-response validation attached.
#' @export
semantica_validate <- function(result, responses, ordered = NULL, verbose = TRUE) {
  verbose <- .semantica_assert_flag(verbose, "verbose")
  if (!inherits(result, "semantica_full_pipeline_result")) stop("'result' must be a completed semantica_run()/semantica_full_pipeline() result.", call. = FALSE)
  was_run_facade <- .semantica_is_run_facade(result)
  result <- .semantica_raw_result(result)
  if (!(is.data.frame(responses) || is.matrix(responses))) stop("'responses' must be a data frame or matrix.", call. = FALSE)
  opt <- result$optimization %||% NULL
  if (!is.list(opt)) stop("The optimizer result required for participant validation is unavailable.", call. = FALSE)
  best_items <- as.character(opt$best_items %||% result$best_items %||% character())
  if (!length(best_items) || is.null(colnames(responses)) || !all(best_items %in% colnames(responses))) {
    miss <- setdiff(best_items, colnames(responses) %||% character())
    stop(sprintf("Participant data must contain columns for every selected item ID.%s", if (length(miss)) paste0(" Missing: ", paste(miss, collapse = ", "), ".") else ""), call. = FALSE)
  }
  factor_assignment <- opt$factor_assignment %||% NULL
  if (is.null(factor_assignment) || is.null(names(factor_assignment))) stop("Stored factor assignments are unavailable.", call. = FALSE)
  factor_assignment <- factor_assignment[best_items]
  factors_cfg <- result$reproducibility$resolved_config$scale$factors %||% NULL
  factors <- if (is.list(factors_cfg) && length(names(factors_cfg))) names(factors_cfg) else unique(as.character(factor_assignment))
  model_info <- opt$model_info %||% list()
  rotation <- model_info$rotation %||% "geomin"
  rotation_args <- prepare_esem_rotation_args(rotation, model_info$rotation_args %||% list(geomin.epsilon = 0.50), best_items, factor_assignment, factors)
  syntax <- opt$esem_syntax %||% build_esem_syntax_safe(best_items, factor_assignment, factors)
  response_estimator <- if ((model_info$data_type %||% "continuous") %in% c("categorical", "likert")) "WLSMV" else model_info$estimator %||% "ML"
  fit <- run_esem_on_response_data(syntax, responses, best_items, estimator = response_estimator, rotation = rotation, rotation_args = rotation_args, ordered = ordered, iter_max = model_info$full_esem_iter_max %||% 2000L, fallback = TRUE)
  response_cor <- compute_response_cor(responses, best_items)
  if (is.null(opt$active_cutoffs)) stop("The stored fit-cutoff configuration is unavailable; post-hoc validation cannot reproduce the established response-validation path safely.", call. = FALSE)
  response_result <- extract_and_score_esem(fit, response_cor, factor_assignment, factors, opt$active_cutoffs, model_info$htmt_threshold %||% 0.85, verbose_decomp = FALSE, score_mode = model_info$semantic_esem_score_mode %||% "current")
  validation <- list(fit = fit, result = response_result, estimator = response_estimator, ordered = ordered, n_obs = nrow(responses), note = "Response-data validation is based on observed item responses and should take priority over semantic-proxy ESEM for final scale validation.")
  out <- result
  out$optimization$response_validation <- validation
  out$participant_validation_performed <- TRUE
  out$participant_validation_converged <- isTRUE(response_result$converged)

  # Refresh only the source-family metadata that the original pipeline derives
  # from the presence of response validation. The diagnostic values themselves
  # remain those returned by the established response-data helpers above.
  refresh_evidence_profile <- function(profile) {
    if (!is.list(profile)) return(profile)
    profile$participant_response_family_available <- TRUE
    profile$analysis_source_family_count <- 3L
    profile$independent_empirical_evidence_family_count <- 1L
    if (is.data.frame(profile$source_families) && "family" %in% names(profile$source_families)) {
      idx <- which(profile$source_families$family == "participant_response")
      if (length(idx)) {
        if ("status" %in% names(profile$source_families)) profile$source_families$status[idx] <- "available"
        if ("independent_of_embedding" %in% names(profile$source_families)) profile$source_families$independent_of_embedding[idx] <- TRUE
        if ("selection_conditioned" %in% names(profile$source_families)) profile$source_families$selection_conditioned[idx] <- TRUE
      }
    }
    profile$interpretation <- paste(
      "Independent participant-response evidence was supplied in addition to",
      "the embedding-semantic family; keep their inferential roles separate."
    )
    profile
  }
  out$evidence_profile <- refresh_evidence_profile(out$evidence_profile %||% out$optimization$evidence_profile %||% NULL)
  out$optimization$evidence_profile <- refresh_evidence_profile(out$optimization$evidence_profile %||% out$evidence_profile %||% NULL)

  out$reproducibility$posthoc_participant_validation <- list(schema = "semantica-posthoc-validation-1", n_obs = nrow(responses), ordered = ordered, estimator = response_estimator, selected_item_ids = best_items)
  if (isTRUE(verbose)) message("SEMANTICA participant validation attached to the existing selected scale; generation and ACO were not rerun.")
  if (isTRUE(was_run_facade)) .semantica_wrap_run_result(out) else out
}


#' Reload a SEMANTICA optimizer-interchange export
#'
#' Descriptive alias for [semantica_reload()]. It reads the component-level CSV
#' interchange format and does not reconstruct high-level report exports.
#'
#' @inheritParams semantica_reload
#' @return The same optimizer-input list as [semantica_reload()].
#' @export
semantica_reload_optimizer <- function(prefix = "SEMANTICA", i.per.f = NULL, default_i_per_f = 3L) {
  semantica_reload(prefix = prefix, i.per.f = i.per.f, default_i_per_f = default_i_per_f)
}


#' @rdname run_multi_seed_semantica
#' @export
print.semantica_multi_seed_result <- function(x, ...) {
  cat("\nSEMANTICA multi-seed result\n==========================\n")
  cat(sprintf("Successful seeds        : %d / %d\n", x$n_successful %||% 0L, length(x$requested_seeds %||% integer())))
  cat(sprintf("Unique selected forms   : %d\n", x$n_unique_solutions %||% NA_integer_))
  if (is.finite(x$mean_pairwise_jaccard %||% NA_real_)) cat(sprintf("Mean pairwise Jaccard   : %.3f\n", x$mean_pairwise_jaccard))
  cmp <- x$objective_comparability$comparable_across_seeds %||% NA
  cat(sprintf("Utility comparability   : %s\n", if (isTRUE(cmp)) "same recorded evidence regime" else if (identical(cmp, FALSE)) "mixed evidence regimes" else "not recorded"))
  cat(sprintf("Consensus items         : %d\n", length(x$consensus_items %||% character())))
  cat("Use summary(x) for stability details and plot(x) for item-selection frequency.\n")
  invisible(x)
}

#' @rdname run_multi_seed_semantica
#' @export
summary.semantica_multi_seed_result <- function(object, ...) {
  out <- list(
    n_successful = object$n_successful %||% 0L,
    requested_seeds = object$requested_seeds %||% integer(),
    successful_seeds = object$successful_seeds %||% integer(),
    n_unique_solutions = object$n_unique_solutions %||% NA_integer_,
    consensus_items = object$consensus_items %||% character(),
    item_frequencies = object$item_frequencies %||% integer(),
    pairwise_jaccard = object$pairwise_jaccard %||% numeric(),
    objective_dispersion = object$objective_dispersion %||% NULL,
    objective_comparability = object$objective_comparability %||% NULL,
    esem_telemetry = object$esem_telemetry %||% NULL,
    stability_scope = object$stability_scope %||% NULL,
    stability_note = object$stability_note %||% NULL
  )
  class(out) <- c("summary.semantica_multi_seed_result", "list")
  out
}

#' @rdname run_multi_seed_semantica
#' @export
print.summary.semantica_multi_seed_result <- function(x, ...) {
  cat("\nSEMANTICA multi-seed summary\n==========================\n")
  cat(sprintf("Successful seeds      : %d / %d\n", x$n_successful, length(x$requested_seeds)))
  cat(sprintf("Unique selected forms : %s\n", x$n_unique_solutions %||% "not recorded"))
  if (length(x$pairwise_jaccard)) cat(sprintf("Pairwise Jaccard       : median %.3f | min %.3f | max %.3f\n", stats::median(x$pairwise_jaccard), min(x$pairwise_jaccard), max(x$pairwise_jaccard)))
  if (!is.null(x$objective_dispersion)) cat(sprintf("Utility dispersion     : SD %.4f | IQR %.4f | range %.4f\n", x$objective_dispersion$sd %||% NA_real_, x$objective_dispersion$iqr %||% NA_real_, x$objective_dispersion$range %||% NA_real_))
  if (!is.null(x$objective_comparability$note)) cat("Utility context        : ", x$objective_comparability$note, "\n", sep = "")
  cat(sprintf("Consensus items       : %s\n", if (length(x$consensus_items)) paste(x$consensus_items, collapse = ", ") else "none"))
  if (!is.null(x$stability_note)) cat("Scope                 : ", x$stability_note, "\n", sep = "")
  invisible(x)
}

#' @rdname run_multi_seed_semantica
#' @export
plot.semantica_multi_seed_result <- function(x, ...) {
  plot_item_selection_frequency(multi_result = x)
}


#' Inspect a SEMANTICA condition without parsing its message
#'
#' Returns common structured fields already attached to SEMANTICA errors and
#' warnings. It does not change condition signaling or error behavior.
#'
#' @param condition A condition object captured with `tryCatch()` or
#'   `withCallingHandlers()`.
#' @return A named list with class, message, and any available stage/argument/
#'   invariant/backend metadata.
#' @export
semantica_condition_info <- function(condition) {
  if (!inherits(condition, "condition")) stop("'condition' must be an R condition object.", call. = FALSE)
  fields <- c("stage", "argument", "invariant", "backend", "model", "status", "reason")
  out <- list(class = class(condition), message = conditionMessage(condition))
  for (nm in fields) if (!is.null(condition[[nm]])) out[[nm]] <- condition[[nm]]
  out
}
