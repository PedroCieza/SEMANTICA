#' Compute mean within-factor cosine similarity for a set of items
#'
#' @param cosine_sim_matrix Square symmetric cosine similarity matrix.
#' @param items Character vector of item IDs to evaluate.
#' @param factor_assignment Named vector: item -> factor.
#' @param factors Character vector of factor names.
#' @return Numeric: mean within-factor cosine similarity (NA if <2 items per factor).
#' @keywords internal
.compute_within_factor_similarity <- function(cosine_sim_matrix, items,
                                               factor_assignment, factors) {
  within_blocks <- vector("list", length(factors))
  n_blocks <- 0L
  for (f in factors) {
    f_items <- names(factor_assignment[factor_assignment == f])
    f_items <- intersect(f_items, items)
    if (length(f_items) >= 2L) {
      sub <- cosine_sim_matrix[f_items, f_items, drop = FALSE]
      lt <- sub[lower.tri(sub)]
      n_blocks <- n_blocks + 1L
      within_blocks[[n_blocks]] <- lt
    }
  }
  if (n_blocks == 0L) return(NA_real_)
  within_sims <- unlist(within_blocks[seq_len(n_blocks)], use.names = FALSE)
  mean(within_sims, na.rm = TRUE)
}
