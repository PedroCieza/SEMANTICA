# Research policy: freeze geometry from admitted candidates, never from truth.
.semantica_semantic_reference <- function(matrix, assignment, factors, ids) {
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
  out$id <- .semantica_object_md5(out)
  out
}

.semantica_apply_semantic_reference <- function(out, reference) {
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
