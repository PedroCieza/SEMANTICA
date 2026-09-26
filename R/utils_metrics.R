# Shared argument-validation helpers used across the high-level and optimizer APIs.
# Kept in this established core source file so R CMD build/install always
# includes the validator with the package namespace.

.semantica_validate_i_per_f <- function(i.per.f, arg = "i.per.f") {
  if (is.null(i.per.f)) {
    stop(sprintf("'%s' must be a named integer vector.", arg), call. = FALSE)
  }
  nms <- names(i.per.f)
  if (is.null(nms) || length(nms) != length(i.per.f) || anyNA(nms) ||
      any(!nzchar(trimws(nms)))) {
    stop(sprintf("'%s' must have one non-empty factor name per value.", arg), call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    duplicates <- unique(nms[duplicated(nms)])
    stop(sprintf(
      "'%s' factor names must be unique; duplicated factor(s): %s.",
      arg, paste(duplicates, collapse = ", ")
    ), call. = FALSE)
  }

  numeric_values <- suppressWarnings(as.numeric(i.per.f))
  if (length(numeric_values) != length(i.per.f) || anyNA(numeric_values) ||
      any(!is.finite(numeric_values)) || any(numeric_values < 1) ||
      any(abs(numeric_values - round(numeric_values)) > sqrt(.Machine$double.eps))) {
    stop(sprintf("'%s' values must be positive integers.", arg), call. = FALSE)
  }

  out <- as.integer(round(numeric_values))
  names(out) <- nms
  out
}

# Normalize a pre-ACO relaxation policy. A scalar keeps the historical API; a
# named vector allows recovery to relax only the factors that actually needed it.
.semantica_normalize_relaxation_policy <- function(policy, factors, arg = "relaxation_level") {
  factors <- as.character(factors)
  allowed <- c("strict", "wording_forbidden_relaxed")
  if (!length(factors) || anyNA(factors) || any(!nzchar(trimws(factors))) || anyDuplicated(factors)) {
    stop("'factors' must contain unique non-empty names.", call. = FALSE)
  }
  policy_names <- names(policy)
  policy <- as.character(policy)
  if (!is.null(policy_names)) names(policy) <- policy_names
  if (length(policy) == 1L && (is.null(names(policy)) || !nzchar(names(policy)[1L]))) {
    out <- stats::setNames(rep(policy, length(factors)), factors)
  } else {
    nms <- names(policy)
    if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms))) || anyDuplicated(nms) ||
        !setequal(nms, factors)) {
      stop(sprintf("'%s' must be one policy value or a named value for every factor.", arg), call. = FALSE)
    }
    out <- policy[factors]
    names(out) <- factors
  }
  bad <- unique(out[!out %in% allowed])
  if (length(bad)) {
    stop(sprintf("'%s' contains unsupported value(s): %s.", arg, paste(bad, collapse = ", ")), call. = FALSE)
  }
  out
}

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

# Semantic-proxy reference comparisons are descriptive screening anchors, not
# participant-based hypothesis tests or universal psychometric validity rules.
.semantica_proxy_reference_status <- function(value, cutoff, direction = c("higher", "lower")) {
  direction <- match.arg(direction)
  value <- suppressWarnings(as.numeric(value[1L]))
  cutoff <- suppressWarnings(as.numeric(cutoff[1L]))
  if (!is.finite(value) || !is.finite(cutoff)) return("N/A")
  met <- if (identical(direction, "higher")) value >= cutoff else value <= cutoff
  if (isTRUE(met)) "REFERENCE MET" else "REFERENCE NOT MET"
}
