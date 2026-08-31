# Internal helpers for ESEM factor-axis identification and fit admissibility.

.semantica_lexicographically_less <- function(x, y) {
  if (is.null(y)) return(TRUE)
  differing <- which(x != y)
  if (length(differing) == 0L) return(FALSE)
  x[differing[1L]] < y[differing[1L]]
}

.semantica_large_factor_assignment <- function(score_matrix, tolerance) {
  n_factors <- nrow(score_matrix)
  pairs <- expand.grid(
    factor = seq_len(n_factors),
    axis = seq_len(n_factors),
    KEEP.OUT.ATTRS = FALSE
  )
  pairs$score <- score_matrix[cbind(pairs$factor, pairs$axis)]
  pairs <- pairs[order(-pairs$score, pairs$factor, pairs$axis), , drop = FALSE]

  assignment <- rep.int(NA_integer_, n_factors)
  used_axes <- rep.int(FALSE, n_factors)
  for (i in seq_len(nrow(pairs))) {
    factor <- pairs$factor[i]
    axis <- pairs$axis[i]
    if (is.na(assignment[factor]) && !used_axes[axis]) {
      assignment[factor] <- axis
      used_axes[axis] <- TRUE
    }
  }

  # Deterministic pair-exchange improvement. This remains bounded for unusually
  # large factor models while making the fallback materially safer than a
  # factor-order greedy assignment.
  max_passes <- max(1L, n_factors^2L)
  passes <- 0L
  repeat {
    passes <- passes + 1L
    improved <- FALSE
    if (n_factors >= 2L) {
      for (left in seq_len(n_factors - 1L)) {
        for (right in (left + 1L):n_factors) {
          current <- score_matrix[left, assignment[left]] +
            score_matrix[right, assignment[right]]
          swapped <- score_matrix[left, assignment[right]] +
            score_matrix[right, assignment[left]]
          scale <- max(1, abs(current), abs(swapped))
          if (swapped > current + tolerance * scale) {
            tmp <- assignment[left]
            assignment[left] <- assignment[right]
            assignment[right] <- tmp
            improved <- TRUE
          }
        }
      }
    }
    if (!improved || passes >= max_passes) break
  }

  list(
    assignment = assignment,
    score = sum(score_matrix[cbind(seq_len(n_factors), assignment)]),
    method = "deterministic_greedy_pair_exchange",
    globally_optimal = FALSE,
    passes = passes,
    note = paste(
      "The factor count exceeded the exact-assignment limit; a deterministic",
      "greedy assignment with pair-exchange improvement was used."
    )
  )
}

.semantica_solve_factor_assignment <- function(score_matrix,
                                                max_exact_factors = 16L,
                                                large_strategy = c("greedy_2opt", "error"),
                                                tolerance = sqrt(.Machine$double.eps)) {
  large_strategy <- match.arg(large_strategy)
  score_matrix <- as.matrix(score_matrix)
  storage.mode(score_matrix) <- "double"
  if (length(dim(score_matrix)) != 2L || nrow(score_matrix) != ncol(score_matrix)) {
    stop("'score_matrix' must be a square numeric matrix.")
  }
  if (any(!is.finite(score_matrix))) {
    stop("'score_matrix' must contain only finite values.")
  }
  n_factors <- nrow(score_matrix)
  if (n_factors == 0L) {
    return(list(
      assignment = integer(0L), score = 0,
      method = "exact_dynamic_programming", globally_optimal = TRUE,
      note = NULL
    ))
  }

  max_exact_factors <- suppressWarnings(as.integer(max_exact_factors[1L]))
  if (!is.finite(max_exact_factors) || max_exact_factors < 1L) {
    stop("'max_exact_factors' must be a positive integer.")
  }
  # The exact solver uses 32-bit bit masks. Keeping the public ceiling below
  # that representation limit also bounds memory use.
  max_exact_factors <- min(max_exact_factors, 30L)
  if (n_factors > max_exact_factors) {
    if (identical(large_strategy, "error")) {
      stop(sprintf(
        "Exact ESEM factor alignment is limited to %d factors; got %d.",
        max_exact_factors, n_factors
      ))
    }
    return(.semantica_large_factor_assignment(score_matrix, tolerance))
  }

  score_memo <- new.env(parent = emptyenv(), hash = TRUE)
  assignment_memo <- new.env(parent = emptyenv(), hash = TRUE)
  solve_from <- function(row, used_mask) {
    if (row > n_factors) {
      return(list(score = 0, assignment = integer(0L)))
    }
    key <- paste0(row, ":", used_mask)
    if (exists(key, envir = score_memo, inherits = FALSE)) {
      return(list(
        score = get(key, envir = score_memo, inherits = FALSE),
        assignment = get(key, envir = assignment_memo, inherits = FALSE)
      ))
    }

    best_score <- -Inf
    best_assignment <- NULL
    for (axis in seq_len(n_factors)) {
      flag <- bitwShiftL(1L, axis - 1L)
      if (bitwAnd(used_mask, flag) != 0L) next
      remainder <- solve_from(row + 1L, bitwOr(used_mask, flag))
      candidate_score <- score_matrix[row, axis] + remainder$score
      candidate_assignment <- c(axis, remainder$assignment)
      scale <- max(1, abs(candidate_score), abs(best_score[is.finite(best_score)]))
      better <- candidate_score > best_score + tolerance * scale
      tied <- is.finite(best_score) &&
        abs(candidate_score - best_score) <= tolerance * scale
      if (better || (tied && .semantica_lexicographically_less(
        candidate_assignment, best_assignment
      ))) {
        best_score <- candidate_score
        best_assignment <- candidate_assignment
      }
    }
    assign(key, best_score, envir = score_memo)
    assign(key, best_assignment, envir = assignment_memo)
    list(score = best_score, assignment = best_assignment)
  }

  solved <- solve_from(1L, 0L)
  list(
    assignment = as.integer(solved$assignment),
    score = as.numeric(solved$score),
    method = "exact_dynamic_programming",
    globally_optimal = TRUE,
    note = NULL
  )
}

#' Align ESEM axes with the intended factor structure
#'
#' @param lambda Numeric item-by-axis loading matrix with item row names.
#' @param factor_assignment Named intended factor assignment for each item.
#' @param factors Optional intended factor order; defaults to the observed assignments.
#' @param psi Optional factor covariance/correlation matrix aligned to the ESEM axes.
#' @param max_exact_factors Maximum number of factors for exact assignment optimization.
#' @param large_strategy Strategy used beyond `max_exact_factors`.
#' @param tolerance Numerical tolerance used for assignment tie handling.
#' @return Alignment metadata plus aligned loading/covariance matrices.
#' @keywords internal
align_esem_to_intended_structure <- function(
    lambda,
    factor_assignment,
    factors = NULL,
    psi = NULL,
    max_exact_factors = 16L,
    large_strategy = c("greedy_2opt", "error"),
    tolerance = sqrt(.Machine$double.eps)) {
  large_strategy <- match.arg(large_strategy)
  lambda <- as.matrix(lambda)
  storage.mode(lambda) <- "double"
  if (length(dim(lambda)) != 2L || nrow(lambda) == 0L || ncol(lambda) == 0L) {
    stop("'lambda' must be a non-empty numeric matrix.")
  }
  if (any(!is.finite(lambda))) {
    stop("'lambda' must contain only finite values.")
  }
  if (is.null(rownames(lambda)) || any(!nzchar(rownames(lambda)))) {
    stop("'lambda' must have non-empty item row names.")
  }
  if (is.null(names(factor_assignment))) {
    stop("'factor_assignment' must be named by item ID.")
  }
  factor_assignment_names <- names(factor_assignment)
  factor_assignment <- as.character(factor_assignment)
  names(factor_assignment) <- factor_assignment_names
  if (is.null(factors)) {
    factors <- unique(factor_assignment[!is.na(factor_assignment) & nzchar(factor_assignment)])
  }
  factors <- as.character(factors)
  if (length(factors) == 0L || anyNA(factors) || any(!nzchar(factors)) || anyDuplicated(factors)) {
    stop("'factors' must contain unique, non-empty factor names.")
  }
  if (length(factors) != ncol(lambda)) {
    stop("The number of intended factors must equal the number of loading columns.")
  }

  original_axis_names <- colnames(lambda)
  if (is.null(original_axis_names) || any(!nzchar(original_axis_names)) ||
      anyDuplicated(original_axis_names)) {
    original_axis_names <- paste0("axis_", seq_len(ncol(lambda)))
    colnames(lambda) <- original_axis_names
  }

  anchor_items <- lapply(factors, function(factor) {
    intended <- names(factor_assignment)[
      !is.na(factor_assignment) & factor_assignment == factor
    ]
    intersect(rownames(lambda), intended)
  })
  names(anchor_items) <- factors
  missing_anchors <- factors[lengths(anchor_items) == 0L]
  if (length(missing_anchors) > 0L) {
    stop(sprintf(
      "No intended items are available for factor(s): %s.",
      paste(missing_anchors, collapse = ", ")
    ))
  }

  score_matrix <- vapply(seq_len(ncol(lambda)), function(axis) {
    vapply(anchor_items, function(items) {
      mean(abs(lambda[items, axis, drop = TRUE]))
    }, numeric(1L))
  }, numeric(length(factors)))
  if (!is.matrix(score_matrix)) {
    score_matrix <- matrix(score_matrix, nrow = length(factors))
  }
  dimnames(score_matrix) <- list(factors, original_axis_names)
  assignment_result <- .semantica_solve_factor_assignment(
    score_matrix,
    max_exact_factors = max_exact_factors,
    large_strategy = large_strategy,
    tolerance = tolerance
  )
  permutation <- assignment_result$assignment

  aligned_lambda <- lambda[, permutation, drop = FALSE]
  colnames(aligned_lambda) <- factors
  sign_multipliers <- rep.int(1, length(factors))
  anchor_statistics <- rep.int(NA_real_, length(factors))
  anchor_methods <- rep.int("intended_item_mean", length(factors))
  for (factor_index in seq_along(factors)) {
    values <- as.numeric(aligned_lambda[
      anchor_items[[factor_index]], factor_index, drop = TRUE
    ])
    anchor_statistics[factor_index] <- mean(values)
    if (abs(anchor_statistics[factor_index]) > tolerance) {
      sign_multipliers[factor_index] <- if (anchor_statistics[factor_index] < 0) -1 else 1
    } else {
      anchor_methods[factor_index] <- "largest_intended_loading"
      largest <- order(-abs(values), seq_along(values))[1L]
      if (is.finite(values[largest]) && abs(values[largest]) > tolerance) {
        sign_multipliers[factor_index] <- if (values[largest] < 0) -1 else 1
      }
    }
  }
  aligned_lambda <- sweep(aligned_lambda, 2L, sign_multipliers, `*`)

  aligned_psi <- NULL
  psi_axis_alignment <- NULL
  if (!is.null(psi)) {
    psi <- as.matrix(psi)
    storage.mode(psi) <- "double"
    if (!identical(dim(psi), c(ncol(lambda), ncol(lambda)))) {
      stop("'psi' must be square with one row and column per loading axis.")
    }
    if (any(!is.finite(psi))) stop("'psi' must contain only finite values.")
    psi_has_rows <- !is.null(rownames(psi))
    psi_has_cols <- !is.null(colnames(psi))
    if (xor(psi_has_rows, psi_has_cols)) {
      stop("'psi' must have both row and column names, or neither.")
    }
    if (psi_has_rows && psi_has_cols) {
      if (!all(original_axis_names %in% rownames(psi)) ||
          !all(original_axis_names %in% colnames(psi))) {
        stop("Named 'psi' axes must match the loading-matrix column names.")
      }
      psi <- psi[original_axis_names, original_axis_names, drop = FALSE]
      psi_axis_alignment <- "matched_by_dimnames"
    } else {
      psi_axis_alignment <- "positional"
    }
    aligned_psi <- psi[permutation, permutation, drop = FALSE]
    aligned_psi <- aligned_psi * tcrossprod(sign_multipliers)
    dimnames(aligned_psi) <- list(factors, factors)
  }

  mapping <- data.frame(
    intended_factor = factors,
    original_axis = original_axis_names[permutation],
    original_column = permutation,
    aligned_column = seq_along(factors),
    match_score = score_matrix[cbind(seq_along(factors), permutation)],
    sign_multiplier = sign_multipliers,
    anchor_statistic = anchor_statistics,
    anchor_method = anchor_methods,
    n_anchor_items = lengths(anchor_items),
    stringsAsFactors = FALSE
  )

  list(
    lambda = aligned_lambda,
    psi = aligned_psi,
    mapping = mapping,
    permutation = stats::setNames(permutation, factors),
    sign_multipliers = stats::setNames(sign_multipliers, factors),
    score_matrix = score_matrix,
    total_match_score = assignment_result$score,
    assignment_method = assignment_result$method,
    globally_optimal_assignment = assignment_result$globally_optimal,
    assignment_note = assignment_result$note,
    psi_axis_alignment = psi_axis_alignment,
    diagnostics = list(
      score_matrix = score_matrix,
      total_match_score = assignment_result$score,
      assignment_method = assignment_result$method,
      globally_optimal_assignment = assignment_result$globally_optimal,
      assignment_note = assignment_result$note,
      psi_axis_alignment = psi_axis_alignment,
      max_exact_factors = max_exact_factors,
      tolerance = tolerance
    ),
    factors = factors,
    anchor_items = anchor_items
  )
}

.semantica_collect_numeric <- function(x) {
  if (is.null(x)) return(numeric(0L))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  if (is.list(x)) {
    return(unlist(lapply(x, .semantica_collect_numeric), use.names = FALSE))
  }
  numeric(0L)
}

.semantica_numeric_matrix <- function(x) {
  is.matrix(x) && (is.numeric(x) || is.integer(x))
}

#' Assess whether an ESEM solution is mathematically admissible
#'
#' @param converged Logical convergence indicator.
#' @param post_check Optional post-estimation check indicator.
#' @param lambda Loading matrix.
#' @param theta Residual covariance/variance matrix.
#' @param psi Latent-factor covariance/correlation matrix.
#' @param estimates Optional additional estimated quantities inspected for non-finite values.
#' @param residual_tolerance Numerical tolerance for residual-variance admissibility.
#' @param correlation_tolerance Numerical tolerance for correlations outside their admissible bounds.
#' @param symmetry_tolerance Numerical tolerance for covariance-matrix symmetry.
#' @param pd_tolerance Numerical tolerance for positive-definiteness checks.
#' @return Structured admissibility assessment with reasons and diagnostics.
#' @keywords internal
assess_esem_admissibility <- function(
    converged,
    post_check = NA,
    lambda,
    theta,
    psi,
    estimates = NULL,
    residual_tolerance = sqrt(.Machine$double.eps),
    correlation_tolerance = sqrt(.Machine$double.eps),
    symmetry_tolerance = sqrt(.Machine$double.eps),
    pd_tolerance = 100 * .Machine$double.eps) {
  reasons <- character(0L)
  warnings <- character(0L)
  details <- list()

  converged_known <- length(converged) == 1L && !is.na(converged)
  details$convergence_available <- converged_known
  details$converged <- if (converged_known) isTRUE(converged) else NA
  if (!converged_known) {
    reasons <- c(reasons, "convergence_unavailable")
  } else if (!isTRUE(converged)) {
    reasons <- c(reasons, "not_converged")
  }

  post_known <- length(post_check) == 1L && !is.na(post_check)
  details$post_check_status <- if (!post_known) {
    "unavailable"
  } else if (isTRUE(post_check)) {
    "passed"
  } else {
    "failed"
  }
  if (!post_known) {
    warnings <- c(warnings, "post_check_unavailable")
  } else if (!isTRUE(post_check)) {
    reasons <- c(reasons, "post_check_failed")
  }

  matrices <- list(lambda = lambda, theta = theta, psi = psi)
  for (matrix_name in names(matrices)) {
    value <- matrices[[matrix_name]]
    if (is.null(value)) {
      reasons <- c(reasons, paste0(matrix_name, "_unavailable"))
      next
    }
    if (!.semantica_numeric_matrix(value) || length(dim(value)) != 2L) {
      reasons <- c(reasons, paste0(matrix_name, "_not_numeric_matrix"))
      next
    }
    if (nrow(value) == 0L || ncol(value) == 0L) {
      reasons <- c(reasons, paste0(matrix_name, "_empty"))
      next
    }
    if (any(!is.finite(value))) {
      reasons <- c(reasons, paste0("nonfinite_", matrix_name))
    }
  }

  estimate_values <- .semantica_collect_numeric(estimates)
  details$n_checked_estimates <- length(estimate_values)
  if (length(estimate_values) > 0L && any(!is.finite(estimate_values))) {
    reasons <- c(reasons, "nonfinite_estimates")
  }

  theta_valid <- .semantica_numeric_matrix(theta) && length(dim(theta)) == 2L &&
    nrow(theta) > 0L && nrow(theta) == ncol(theta) && all(is.finite(theta))
  if (.semantica_numeric_matrix(theta) && nrow(theta) != ncol(theta)) {
    reasons <- c(reasons, "theta_not_square")
  }
  if (theta_valid) {
    theta_scale <- max(1, max(abs(theta)))
    theta_asymmetry <- max(abs(theta - t(theta)))
    details$theta_max_asymmetry <- theta_asymmetry
    if (theta_asymmetry > symmetry_tolerance * theta_scale) {
      reasons <- c(reasons, "theta_not_symmetric")
    } else {
      theta_sym <- (theta + t(theta)) / 2
      residual_variances <- diag(theta_sym)
      details$min_residual_variance <- min(residual_variances)
      if (any(residual_variances < -residual_tolerance * theta_scale)) {
        reasons <- c(reasons, "negative_residual_variance")
      }
      theta_eigen <- tryCatch(
        eigen(theta_sym, symmetric = TRUE, only.values = TRUE)$values,
        error = function(e) NA_real_
      )
      details$min_theta_eigenvalue <- suppressWarnings(min(theta_eigen, na.rm = TRUE))
      if (any(!is.finite(theta_eigen))) {
        reasons <- c(reasons, "theta_eigendecomposition_failed")
      } else if (min(theta_eigen) < -residual_tolerance * theta_scale) {
        reasons <- c(reasons, "residual_covariance_not_positive_semidefinite")
      }
    }
  }

  psi_valid <- .semantica_numeric_matrix(psi) && length(dim(psi)) == 2L &&
    nrow(psi) > 0L && nrow(psi) == ncol(psi) && all(is.finite(psi))
  if (.semantica_numeric_matrix(psi) && nrow(psi) != ncol(psi)) {
    reasons <- c(reasons, "psi_not_square")
  }
  if (psi_valid) {
    psi_scale <- max(1, max(abs(psi)))
    psi_asymmetry <- max(abs(psi - t(psi)))
    details$psi_max_asymmetry <- psi_asymmetry
    if (psi_asymmetry > symmetry_tolerance * psi_scale) {
      reasons <- c(reasons, "psi_not_symmetric")
    } else {
      psi_sym <- (psi + t(psi)) / 2
      latent_variances <- diag(psi_sym)
      details$min_latent_variance <- min(latent_variances)
      variance_floor <- pd_tolerance * psi_scale
      if (any(latent_variances <= variance_floor)) {
        reasons <- c(reasons, "nonpositive_latent_variance")
      }

      psi_eigen <- tryCatch(
        eigen(psi_sym, symmetric = TRUE, only.values = TRUE)$values,
        error = function(e) NA_real_
      )
      details$min_psi_eigenvalue <- suppressWarnings(min(psi_eigen, na.rm = TRUE))
      if (any(!is.finite(psi_eigen))) {
        reasons <- c(reasons, "psi_eigendecomposition_failed")
      } else if (min(psi_eigen) <= variance_floor) {
        reasons <- c(reasons, "latent_covariance_not_positive_definite")
      }

      if (all(latent_variances > 0)) {
        latent_cor <- psi_sym / sqrt(tcrossprod(latent_variances))
        diag(latent_cor) <- 1
        off_diagonal <- latent_cor[upper.tri(latent_cor)]
        details$max_abs_latent_correlation <- if (length(off_diagonal) > 0L) {
          max(abs(off_diagonal))
        } else {
          0
        }
        if (any(!is.finite(latent_cor))) {
          reasons <- c(reasons, "nonfinite_latent_correlation")
        } else if (any(abs(off_diagonal) > 1 + correlation_tolerance)) {
          reasons <- c(reasons, "invalid_latent_correlation")
        }
      }
    }
  }

  reasons <- unique(reasons)
  warnings <- unique(warnings)
  structure(
    list(
      admissible = length(reasons) == 0L,
      reasons = reasons,
      warnings = warnings,
      details = details
    ),
    class = c("semantica_esem_admissibility", "list")
  )
}

.semantica_safe_lav_inspect <- function(esem_fit, what) {
  tryCatch(
    list(
      available = TRUE,
      value = suppressWarnings(lavaan::lavInspect(esem_fit, what)),
      error = NULL
    ),
    error = function(e) list(
      available = FALSE, value = NULL, error = conditionMessage(e)
    )
  )
}

.semantica_assess_esem_fit <- function(
    esem_fit,
    residual_tolerance = sqrt(.Machine$double.eps),
    correlation_tolerance = sqrt(.Machine$double.eps),
    symmetry_tolerance = sqrt(.Machine$double.eps),
    pd_tolerance = 100 * .Machine$double.eps) {
  if (is.null(esem_fit) || !requireNamespace("lavaan", quietly = TRUE)) {
    out <- assess_esem_admissibility(
      converged = FALSE, post_check = NA,
      lambda = NULL, theta = NULL, psi = NULL
    )
    out$details$fit_error <- if (is.null(esem_fit)) {
      "missing ESEM fit"
    } else {
      "package 'lavaan' is unavailable"
    }
    return(out)
  }

  convergence <- .semantica_safe_lav_inspect(esem_fit, "converged")
  post_check <- .semantica_safe_lav_inspect(esem_fit, "post.check")
  standardized <- .semantica_safe_lav_inspect(esem_fit, "std")
  estimated <- .semantica_safe_lav_inspect(esem_fit, "est")
  std_value <- standardized$value

  out <- assess_esem_admissibility(
    converged = if (convergence$available) convergence$value else NA,
    post_check = if (post_check$available) post_check$value else NA,
    lambda = if (standardized$available) std_value$lambda else NULL,
    theta = if (standardized$available) std_value$theta else NULL,
    psi = if (standardized$available) std_value$psi else NULL,
    estimates = list(
      standardized = if (standardized$available) standardized$value else NULL,
      estimated = if (estimated$available) estimated$value else NULL
    ),
    residual_tolerance = residual_tolerance,
    correlation_tolerance = correlation_tolerance,
    symmetry_tolerance = symmetry_tolerance,
    pd_tolerance = pd_tolerance
  )
  out$details$query_errors <- Filter(
    Negate(is.null),
    list(
      convergence = convergence$error,
      post_check = post_check$error,
      standardized = standardized$error,
      estimated = estimated$error
    )
  )
  out
}

#' Test whether a fitted ESEM model is admissible
#'
#' @param esem_fit Fitted lavaan ESEM object.
#' @param residual_tolerance Numerical tolerance for residual-variance admissibility.
#' @param correlation_tolerance Numerical tolerance for correlation bounds.
#' @param symmetry_tolerance Numerical tolerance for covariance-matrix symmetry.
#' @param pd_tolerance Numerical tolerance for positive-definiteness checks.
#' @param return_assessment Logical; return the full assessment instead of a single flag.
#' @return Logical admissibility flag or the full assessment object.
#' @keywords internal
is_admissible_esem_fit <- function(
    esem_fit,
    residual_tolerance = sqrt(.Machine$double.eps),
    correlation_tolerance = sqrt(.Machine$double.eps),
    symmetry_tolerance = sqrt(.Machine$double.eps),
    pd_tolerance = 100 * .Machine$double.eps,
    return_assessment = FALSE) {
  assessment <- .semantica_assess_esem_fit(
    esem_fit,
    residual_tolerance = residual_tolerance,
    correlation_tolerance = correlation_tolerance,
    symmetry_tolerance = symmetry_tolerance,
    pd_tolerance = pd_tolerance
  )
  if (isTRUE(return_assessment)) assessment else isTRUE(assessment$admissible)
}

#' Extract and align loading/covariance matrices from an ESEM fit
#'
#' @param esem_fit Fitted lavaan ESEM object.
#' @param factor_assignment Named intended factor assignment for each item.
#' @param factors Optional intended factor order.
#' @param standardized Logical; extract standardized rather than unstandardized matrices.
#' @param max_exact_factors Maximum number of factors for exact axis assignment.
#' @param large_strategy Assignment strategy used beyond `max_exact_factors`.
#' @param tolerance Numerical tolerance used in factor-axis alignment.
#' @return Aligned ESEM loading/covariance matrices and alignment diagnostics.
#' @keywords internal
extract_aligned_esem_solution <- function(
    esem_fit,
    factor_assignment,
    factors = NULL,
    standardized = TRUE,
    max_exact_factors = 16L,
    large_strategy = c("greedy_2opt", "error"),
    tolerance = sqrt(.Machine$double.eps)) {
  if (is.null(esem_fit)) stop("'esem_fit' cannot be NULL.")
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required to inspect an ESEM fit.")
  }
  large_strategy <- match.arg(large_strategy)
  inspected <- .semantica_safe_lav_inspect(
    esem_fit, if (isTRUE(standardized)) "std" else "est"
  )
  if (!inspected$available || is.null(inspected$value$lambda)) {
    stop(sprintf(
      "Unable to extract %s ESEM loading matrices%s.",
      if (isTRUE(standardized)) "standardized" else "estimated",
      if (!is.null(inspected$error)) paste0(": ", inspected$error) else ""
    ))
  }
  alignment <- align_esem_to_intended_structure(
    lambda = inspected$value$lambda,
    psi = inspected$value$psi,
    factor_assignment = factor_assignment,
    factors = factors,
    max_exact_factors = max_exact_factors,
    large_strategy = large_strategy,
    tolerance = tolerance
  )
  alignment$theta <- inspected$value$theta
  alignment$standardized <- isTRUE(standardized)
  alignment$admissibility <- .semantica_assess_esem_fit(esem_fit)
  alignment
}

#' Consolidate technical and structural ESEM proxy states
#'
#' Keeps convergence/admissibility separate from structural quality. An
#' admissible sample-free ESEM fit is not automatically interpreted as good
#' structure or participant-based validity evidence.
#'
#' @param requested Whether the ESEM evaluation was requested.
#' @param attempted Whether an evaluation was actually attempted.
#' @param esem_result SEMANTICA ESEM scoring result, if available.
#' @param failure_reason Optional explicit execution failure reason.
#' @param fallback_policy Optional recorded fallback policy.
#' @param stage Stage label such as `"search"`, `"archive"`, or `"final"`.
#' @return A transparent state object with separate technical state, structural
#'   quality, and a human-readable `quality_message` that prevents technical
#'   admissibility from being mistaken for favorable structure.
#' @export
semantica_esem_state <- function(requested = TRUE, attempted = requested,
                                 esem_result = NULL, failure_reason = NULL,
                                 fallback_policy = NULL, stage = "final") {
  requested <- isTRUE(requested); attempted <- isTRUE(attempted)
  if (!requested) {
    return(list(
      technical_state = "not_requested", structural_quality = "not_assessed",
      stage = stage, reason = NULL, fallback_policy = fallback_policy,
      quality_flags = logical(0L),
      quality_message = "ESEM was not requested; structural quality is not assessed.",
      participant_based = FALSE,
      note = "Admissibility is a technical state; structural quality remains a separate sample-free proxy interpretation."
    ))
  }
  if (!attempted || is.null(esem_result)) {
    return(list(
      technical_state = if (attempted) "attempted_failed" else "not_attempted",
      structural_quality = "not_assessed", stage = stage,
      reason = failure_reason %||% if (attempted) "no ESEM result available" else "evaluation was not attempted",
      fallback_policy = fallback_policy,
      quality_flags = logical(0L),
      quality_message = "ESEM structural quality is not assessed because no usable technical fit is available.",
      participant_based = FALSE,
      note = "Admissibility is a technical state; structural quality remains a separate sample-free proxy interpretation."
    ))
  }
  admissibility <- esem_result$admissibility %||% list()
  converged <- isTRUE(esem_result$converged) || isTRUE(admissibility$details$converged)
  admissible <- isTRUE(esem_result$admissible)
  technical <- if (!converged) {
    "attempted_failed"
  } else if (!admissible) {
    "converged_inadmissible"
  } else {
    "admissible"
  }
  sd <- esem_result$structure_diagnostics %||% list()
  is_unidimensional <- identical(
    esem_result$dimensionality_mode %||% sd$dimensionality_mode %||% "",
    "unidimensional"
  )
  quality_flags <- if (is_unidimensional) {
    # HTMT, dominance, and cross-loading flags are comparative multi-factor
    # concepts. Do not manufacture favorable or unfavorable flags from their
    # vacuous one-factor values.
    logical(0L)
  } else {
    c(
      htmt_overlap = is.finite(esem_result$htmt_violations %||% NA_real_) && (esem_result$htmt_violations %||% 0) > 0,
      dominance_mismatch = is.finite(sd$correct_dominance %||% NA_real_) && (sd$correct_dominance %||% 1) < 1,
      cross_loading = is.finite(sd$no_large_cross_loading %||% NA_real_) && (sd$no_large_cross_loading %||% 1) < 1
    )
  }
  structural_quality <- if (!admissible) {
    "not_assessed"
  } else if (is_unidimensional) {
    "admissible_unidimensional_proxy"
  } else if (any(quality_flags, na.rm = TRUE)) {
    "admissible_but_structurally_mixed"
  } else {
    "admissible_with_comparatively_favorable_proxy_structure"
  }
  reasons <- unique(c(
    failure_reason,
    admissibility$reasons %||% character(0L)
  ))
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
  quality_message <- if (identical(structural_quality, "admissible_but_structurally_mixed")) {
    "Technically admissible ESEM, but the intended structure remains mixed; do not interpret admissibility as favorable structural evidence."
  } else if (identical(structural_quality, "admissible_unidimensional_proxy")) {
    paste(
      "Technically admissible one-factor semantic-proxy model.",
      "Interpret loading strength, residual structure, eigenvalue dominance, and later participant-based dimensionality evidence together."
    )
  } else if (identical(structural_quality, "admissible_with_comparatively_favorable_proxy_structure")) {
    "Technically admissible ESEM with comparatively favorable sample-free structural-proxy diagnostics."
  } else {
    "Structural quality is not assessed because the ESEM result is not technically admissible."
  }
  list(
    technical_state = technical,
    structural_quality = structural_quality,
    stage = stage,
    reason = if (length(reasons)) paste(reasons, collapse = "; ") else NULL,
    fallback_policy = fallback_policy,
    quality_flags = quality_flags,
    quality_message = quality_message,
    participant_based = FALSE,
    dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
    note = "Admissibility is a technical state; structural quality remains a separate sample-free proxy interpretation."
  )
}
