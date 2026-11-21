# Research policy: freeze geometry from admitted candidates, never from truth.
.semantica_semantic_reference <- function(matrix, assignment, factors, ids,
    comparison = c("candidate", "fixed")) {
  comparison <- match.arg(comparison)
  if (length(factors) < 2L) stop("The admitted-pool semantic policy requires at least two domains.")
  matrix <- matrix[ids, ids, drop = FALSE]
  assignment <- assignment[ids]
  out <- .semantica_relative_semantic_components(matrix, assignment, factors)
  out$schema <- "semantica-admitted-pool-lower-guard-v1"
  out$source <- "native_admitted_full_pool_fixed_before_subset_evaluation"
  out$admitted_ids <- ids
  out$pool_fingerprint <- .semantica_object_md5(list(matrix = matrix, assignment = assignment))
  out$available <- is.finite(out$global$robust_scale) &&
    all(is.finite(out$factor_table$robust_scale))
  if (comparison == "fixed") {
    finite_sorted <- function(x) sort(as.numeric(x[is.finite(x)]))
    out$schema <- "semantica-admitted-pool-fixed-comparison-v1"
    out$gap_reference <- "admitted_pool_fixed_comparison"
    out$admitted_assignment <- assignment
    out$between_reference <- list(
      global = finite_sorted(.semantica_semantic_pair_vectors(matrix, assignment)$between),
      by_factor = stats::setNames(lapply(factors, function(f)
        finite_sorted(matrix[ids[assignment == f], ids[assignment != f], drop = FALSE])), factors))
  }
  out$id <- .semantica_object_md5(out)
  out
}

.semantica_apply_semantic_reference <- function(out, reference, matrix = NULL, assignment = NULL) {
  if (identical(reference$gap_reference, "admitted_pool_fixed_comparison")) {
    ids <- rownames(matrix)
    if (is.null(matrix) || is.null(ids) || !all(ids %in% reference$admitted_ids) ||
        !identical(as.character(assignment[ids]), as.character(reference$admitted_assignment[ids])) ||
        !setequal(out$factor_table$factor, reference$factor_table$factor))
      stop("Fixed comparison requires a candidate matrix and assignments from the frozen admitted pool.")
    candidate <- out
    out <- .semantica_relative_semantic_components(matrix, assignment, out$factor_table$factor,
      comparison_reference = reference)
    out$candidate_comparison <- candidate
  }
  gap_score <- function(gap, rank, scale) {
    z <- ifelse(is.finite(scale) & scale > 0, gap / scale, NA_real_)
    positive <- pmax(0, z)
    list(z = z, gap = positive / (1 + positive), score = pmin(rank, positive / (1 + positive)))
  }
  global <- gap_score(out$global$median_gap, out$global$rank_component, reference$global$robust_scale)
  out$global$robust_scale <- reference$global$robust_scale
  out$global$standardized_gap <- global$z
  out$global$gap_component <- global$gap
  out$global$conservative_score <- global$score
  tab <- out$factor_table
  scale <- reference$factor_table$robust_scale[match(tab$factor, reference$factor_table$factor)]
  fixed <- gap_score(tab$median_gap, tab$rank_component, scale)
  tab$robust_scale <- scale; tab$standardized_gap <- fixed$z
  tab$gap_component <- fixed$gap; tab$relative_score <- fixed$score
  out$factor_table <- tab
  out$weakest_factor_score <- if (all(is.finite(fixed$score))) min(fixed$score) else NA_real_
  out$aggregate_score <- min(global$score, out$weakest_factor_score)
  out$reference_source <- reference$source
  out$reference_id <- reference$id
  out
}

# Sorted, admitted-pool comparisons make rank and median-gap evidence independent
# of other domains' selected items. Midranks give exact ties half credit; no
# candidate-specific spread or new representation-specific threshold is used.
.semantica_fixed_comparison_gap <- function(within, between, reference) {
  within <- as.numeric(within); within <- within[is.finite(within)]
  if (!length(within) || !length(between))
    return(.semantica_robust_relative_gap(numeric(), numeric()))
  A <- mean((findInterval(within, between) + findInterval(within, between, left.open = TRUE)) / 2) / length(between)
  median_within <- stats::median(within)
  gap <- median_within - reference$median_between
  scale <- reference$robust_scale
  z <- if (is.finite(scale) && scale > 0) gap / scale else NA_real_
  rank <- max(0, min(1, 2 * A - 1))
  positive <- max(0, z); component <- positive / (1 + positive)
  list(stochastic_superiority = A, rank_component = rank,
    median_within = median_within, median_between = reference$median_between, median_gap = gap,
    robust_scale = scale, standardized_gap = z, gap_component = component,
    conservative_score = min(rank, component))
}
