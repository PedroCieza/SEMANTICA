# Visit complete forms in deterministic factor-stratified order.
.semantica_visit_factor_forms <- function(choices, visit, depth = 1L, selected = character()) {
  if (depth > length(choices)) return(visit(selected))
  for (pick in choices[[depth]])
    .semantica_visit_factor_forms(choices, visit, depth + 1L, c(selected, pick))
  invisible(NULL)
}

# Corrected item-to-composite correlations on trait-keyed responses.
.semantica_response_form <- function(correlation, assignment, min_alpha, max_htmt) {
  ids <- names(assignment)
  groups <- split(ids, assignment)
  x <- correlation[ids, ids, drop = FALSE]
  variances <- vapply(groups, function(g) sum(x[g, g, drop = FALSE]), numeric(1L))
  n <- lengths(groups)
  alpha <- rep(NA_real_, length(n))
  positive <- variances > 0
  alpha[positive] <- n[positive] / (n[positive] - 1) *
    (1 - n[positive] / variances[positive])
  names(alpha) <- names(groups)
  within <- vapply(groups, function(g) {
    s <- x[g, g, drop = FALSE]
    mean(abs(s[upper.tri(s)]))
  }, numeric(1L))
  htmt <- if (all(within > 0)) max(utils::combn(names(groups), 2L, FUN = function(pair)
    mean(abs(x[groups[[pair[1L]]], groups[[pair[2L]]], drop = FALSE])) /
      sqrt(prod(within[pair])))) else NA_real_
  contrast <- if (all(positive)) min(vapply(ids, function(i) {
    f <- assignment[[i]]
    rest <- setdiff(groups[[f]], i)
    v <- sum(x[rest, rest, drop = FALSE])
    if (v <= 0) return(NA_real_)
    rivals <- groups[names(groups) != f]
    competing <- vapply(rivals, function(g) abs(sum(x[i, g])) /
      sqrt(variances[[assignment[[g[1L]]]]]), numeric(1L))
    sum(x[i, rest]) / sqrt(v) - max(competing)
  }, numeric(1L))) else NA_real_
  supported <- if (any(alpha < min_alpha, na.rm = TRUE) ||
      isTRUE(htmt >= max_htmt) || isTRUE(contrast <= 0)) FALSE else
    if (all(is.finite(c(alpha, htmt, contrast)))) TRUE else NA
  list(alpha = alpha, min_alpha = min(alpha), htmt = htmt,
    min_item_contrast = contrast, supported = supported)
}

.semantica_adequacy_thresholds <- function(min_alpha, max_htmt) {
  if (!is.numeric(min_alpha) || length(min_alpha) != 1L || !is.finite(min_alpha) ||
      min_alpha < 0 || min_alpha > 1 || !is.numeric(max_htmt) || length(max_htmt) != 1L ||
      !is.finite(max_htmt) || max_htmt <= 0)
    stop("Supply min_alpha in [0, 1] and a positive finite max_htmt.", call. = FALSE)
}

.semantica_adequacy_matrix <- function(correlation) {
  m <- as.matrix(correlation)
  ids <- .semantica_initial_item_ids(rownames(m))
  if (!is.numeric(m) || is.null(ids) || length(ids) < 2L || ncol(m) != length(ids) ||
      anyDuplicated(colnames(m)) || !setequal(ids, colnames(m)) || any(!is.finite(m)))
    stop("Supply a finite response correlation matrix with unique matching item IDs.", call. = FALSE)
  m <- m[ids, ids, drop = FALSE]
  if (!isSymmetric(m, tol = 1e-8) || any(abs(diag(m) - 1) > 1e-8) ||
      any(abs(m) > 1 + 1e-8) ||
      min(eigen(m, symmetric = TRUE, only.values = TRUE)$values) < -1e-8)
    stop("The response correlation matrix must be positive semidefinite with unit diagonal.", call. = FALSE)
  m
}

.semantica_adequacy_assignment <- function(assignment, ids) {
  .semantica_initial_item_ids(names(assignment))
  if (!is.character(assignment) || !all(ids %in% names(assignment)) ||
      anyNA(assignment[ids]) || any(!nzchar(trimws(assignment[ids]))))
    stop("Supply a factor assignment named by each item ID.", call. = FALSE)
}

#' Assess scale adequacy for a fixed form
#'
#' Evaluate the proposed form against trait-keyed participant-response
#' correlations. The response criteria are separate from the PFA disposition.
#' For a calibrated prediction from [semantica_apply_empirical_calibration()],
#' the same criteria are reported as exploratory predictions. A prediction
#' alone cannot yield a joint advance.
#' @param correlation Named, trait-keyed response correlation matrix covering
#'   the selected items. May include additional items. Use `NULL` when
#'   response evidence is unavailable.
#' @param factor_assignment Character vector of intended factors, named by
#'   selected item ID. Supply at least two items in each of at least two factors.
#' @param structure_disposition Recorded PFA decision for the same form,
#'   either `"advance"`, `"defer"`, or `"unavailable"`.
#' @param min_alpha Minimum standardized alpha in each domain.
#' @param max_htmt Exclusive upper limit for absolute HTMT.
#' @return A list with the response criterion status, the structure and joint
#'   dispositions, per-factor alpha, maximum HTMT, and the minimum signed
#'   intended-versus-rival item contrast. The contrast must be positive.
#' @export
semantica_assess_form_adequacy <- function(correlation = NULL, factor_assignment,
    structure_disposition = c("unavailable", "advance", "defer"),
    min_alpha = .70, max_htmt = .85) {
  structure_disposition <- match.arg(structure_disposition)
  .semantica_adequacy_thresholds(min_alpha, max_htmt)
  ids <- names(factor_assignment)
  .semantica_adequacy_assignment(factor_assignment, ids)
  counts <- table(factor_assignment)
  if (length(counts) < 2L || any(counts < 2L) || is.null(ids))
    stop("Supply at least two named items in each of at least two factors.", call. = FALSE)
  fit <- list(alpha = stats::setNames(rep(NA_real_, length(counts)), names(counts)),
    min_alpha = NA_real_, htmt = NA_real_, min_item_contrast = NA_real_, supported = NA)
  source <- "unavailable"
  if (!is.null(correlation)) {
    predicted <- !is.null(attr(correlation, "semantica_calibration", exact = TRUE))
    m <- .semantica_adequacy_matrix(correlation)
    if (!all(ids %in% rownames(m))) stop("The response matrix must cover every selected item ID.", call. = FALSE)
    fit <- .semantica_response_form(m, factor_assignment, min_alpha, max_htmt)
    source <- if (predicted) "calibrated_prediction" else "declared_response_correlations"
  }
  status <- if (isTRUE(fit$supported)) "supported" else
    if (identical(fit$supported, FALSE)) "unsupported" else "unavailable"
  joint <- if (structure_disposition == "defer" ||
      (source == "declared_response_correlations" && status == "unsupported")) "defer" else
    if (structure_disposition == "advance" && source == "declared_response_correlations" &&
      status == "supported") "advance" else "unavailable"
  c(list(response_status = status, evidence_source = source,
    structure_disposition = structure_disposition, joint_disposition = joint,
    criteria = c(min_alpha = min_alpha, max_htmt = max_htmt, min_item_contrast_exclusive = 0)), fit)
}

#' Audit response-supported forms in a small candidate pool
#'
#' Enumerates fixed-length forms against a caller-supplied, independently
#' obtained response correlation matrix. Compare support before and after
#' declared feasibility constraints. Use held-out responses for evaluation
#' only and keep them separate from proposal selection.
#' @param correlation Trait-keyed participant response correlation matrix
#'   with matching item IDs on both axes and unit diagonal.
#' @param factor_assignment Character vector of factor labels named by item ID.
#' @param select_per_factor Named integer counts of at least two items for
#'   each of at least two declared factors.
#' @param max_combinations Positive whole-number cap on complete forms. The
#'   audit returns a budget status before enumerating if the cap is exceeded.
#' @param eligible_items Optional item IDs to consider before enumeration.
#' @param feasible Optional function of selected IDs returning TRUE or FALSE.
#'   Use this to apply the same duplicate, facet, or other constraints as selection.
#' @param min_alpha Minimum standardized alpha in every domain.
#' @param max_htmt Exclusive upper limit for the maximum absolute-correlation HTMT.
#' @return A list with the audit status, form counts, per-factor alpha ceilings
#'   before the feasibility callback, and metrics and item IDs for each form.
#' @export
semantica_pool_adequacy <- function(correlation, factor_assignment, select_per_factor,
    max_combinations, eligible_items = NULL, feasible = NULL,
    min_alpha = .70, max_htmt = .85) {
  cap <- .semantica_assert_positive_integer(max_combinations, "max_combinations")
  .semantica_adequacy_thresholds(min_alpha, max_htmt)
  if (!is.null(feasible) && !is.function(feasible))
    stop("'feasible' must be a function of item IDs.", call. = FALSE)
  m <- .semantica_adequacy_matrix(correlation)
  ids <- rownames(m)
  .semantica_adequacy_assignment(factor_assignment, ids)
  factors <- .semantica_initial_item_ids(names(select_per_factor))
  counts <- .semantica_assert_positive_integer_vector(select_per_factor, "select_per_factor")
  if (length(factors) < 2L || any(counts < 2L))
    stop("Supply named counts of at least two items for each of at least two factors.", call. = FALSE)
  eligible <- .semantica_initial_item_ids(eligible_items) %||% ids
  if (!all(eligible %in% ids)) stop("Eligible item IDs must occur in the matrix.", call. = FALSE)
  groups <- lapply(factors, function(f) eligible[factor_assignment[eligible] == f])
  sizes <- lengths(groups)
  total <- if (any(sizes < counts)) 0 else prod(choose(sizes, counts))
  per_factor <- data.frame(factor = factors, eligible_items = sizes, selected_items = counts,
    max_alpha_before_constraints = NA_real_, row.names = NULL)
  out <- list(status = "no_feasible_forms", combinations = total, evaluated_forms = 0L,
    feasible_forms = 0L, supported_before_constraints = 0L, supported_forms = 0L,
    unresolved_feasible_forms = 0L, factors = per_factor, forms = NULL)
  if (total == 0) return(out)
  if (total > cap) {
    out$status <- "budget_exceeded"
    out[c("feasible_forms", "supported_before_constraints", "supported_forms",
      "unresolved_feasible_forms")] <- NA_integer_
    return(out)
  }
  choices <- Map(function(g, n) utils::combn(g, n, simplify = FALSE), groups, counts)
  rows <- vector("list", total)
  evaluated <- 0L
  max_alpha <- stats::setNames(rep(-Inf, length(factors)), factors)
  .semantica_visit_factor_forms(choices, function(selected) {
    allowed <- if (is.null(feasible)) TRUE else feasible(selected)
    if (!is.logical(allowed) || length(allowed) != 1L || is.na(allowed))
      stop("'feasible' must return a single TRUE or FALSE.", call. = FALSE)
    fit <- .semantica_response_form(m, factor_assignment[selected], min_alpha, max_htmt)
    available <- names(fit$alpha)[is.finite(fit$alpha)]
    max_alpha[available] <<- pmax(max_alpha[available], fit$alpha[available])
    evaluated <<- evaluated + 1L
    rows[[evaluated]] <<- data.frame(feasible = allowed, supported = fit$supported,
      min_alpha = fit$min_alpha, htmt = fit$htmt, min_item_contrast = fit$min_item_contrast,
      items = I(list(selected)))
  })
  audit <- do.call(rbind, rows)
  max_alpha[!is.finite(max_alpha)] <- NA_real_
  out$factors$max_alpha_before_constraints <- unname(max_alpha)
  out$evaluated_forms <- evaluated
  out$feasible_forms <- sum(audit$feasible)
  out$supported_before_constraints <- sum(audit$supported %in% TRUE)
  out$supported_forms <- sum(audit$feasible & audit$supported %in% TRUE)
  out$unresolved_feasible_forms <- sum(audit$feasible & is.na(audit$supported))
  if (out$supported_forms > 0L) out$status <- "supported_forms_available" else
    if (out$unresolved_feasible_forms > 0L) out$status <- "response_evidence_unavailable" else
      if (out$feasible_forms > 0L) out$status <- "no_supported_forms"
  out$forms <- audit
  out
}
