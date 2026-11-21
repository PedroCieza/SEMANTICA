#' Declare development reference evidence for candidate selection
#'
#' Supplies response covariances for candidate and external reference items.
#' Primary panels govern selection; sensitivity panels are reported separately.
#' References, scoring directions and their evidential justification must be
#' fixed before search. Different IDs or methods do not establish independence.
#' Comparisons use point estimates, not calibrated confidence bounds or final
#' validation. Reverse-code variables before supplying them. Unit composite
#' weights are used within each declared domain.
#'
#' @param references Named list of reference item IDs, one entry per domain.
#' @param data Complete numeric development responses, including candidate and
#'   reference columns. Supply exactly one of `data` and `covariance`.
#' @param covariance Named symmetric positive-semidefinite covariance matrix.
#' @param source,method,justification Nonempty descriptions identifying the
#'   evidence source, measurement method and justification for its intended role.
#'   These declarations are preserved, not independently verified by SEMANTICA.
#' @param role `"primary"` governs comparisons; `"sensitivity"` reports an
#'   alternative panel without letting it override primary evidence.
#' @param source_family Declared origin: observed `"response_data"`,
#'   `"response_prediction"`, or `"synthetic"` experimental controls.
#' @return A frozen `semantica_reference_evidence` object. Pass one object or a
#'   list through `reference_evidence` in [semantica_quality_config()] or
#'   [ACO_with_ESEM()]. At least one primary panel is required. Selection is
#'   exploratory; keep `validation_data` independent of these development data.
#'   Use one optimizer seed; scalar restart aggregation cannot combine reference
#'   frontiers and is unavailable in this mode.
#' @export
semantica_reference_evidence <- function(references, data = NULL, covariance = NULL,
    source, method, justification, role = c("primary", "sensitivity"),
    source_family = c("response_data", "response_prediction", "synthetic")) {
  role <- match.arg(role); source_family <- match.arg(source_family)
  text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  if (!all(vapply(list(source, method, justification), text, logical(1))))
    stop("Reference source, method and justification must be nonempty strings.")
  if (!is.list(references) || !length(references) || is.null(names(references)) ||
      anyNA(names(references)) || any(!nzchar(names(references))) || anyDuplicated(names(references)) ||
      !all(vapply(references, function(x) is.character(x) && length(x) > 0L && !anyNA(x) && all(nzchar(x)), logical(1))))
    stop("references must be a named list of nonempty item-ID vectors.")
  refs <- unlist(references, use.names = FALSE)
  if (anyDuplicated(refs)) stop("Reference items cannot repeat within or across domains.")
  if (is.null(data) == is.null(covariance)) stop("Supply exactly one of data and covariance.")
  n <- NULL
  if (!is.null(data)) {
    if (!(is.matrix(data) || is.data.frame(data)) || nrow(data) < 2L ||
        !all(vapply(as.data.frame(data), is.numeric, logical(1))))
      stop("Reference data require at least two rows of numeric, reverse-coded responses.")
    data <- as.matrix(data)
    if (any(!is.finite(data))) stop("Reference data must be complete; declare missing-data handling before selection.")
    n <- nrow(data); covariance <- stats::cov(data)
  }
  S <- as.matrix(covariance); ids <- rownames(S)
  if (!is.numeric(S) || nrow(S) < 2L || nrow(S) != ncol(S) || any(!is.finite(S)) ||
      is.null(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      !identical(ids, colnames(S)) || !all(refs %in% ids) || !isSymmetric(S) || any(diag(S) <= 0))
    stop("Reference covariance must be finite, symmetric, have positive variances and unique matching item IDs.")
  ev <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < -sqrt(.Machine$double.eps)*max(1, max(abs(ev))))
    stop("Reference covariance is indefinite; no automatic matrix repair is applied.")
  ids <- sort(ids); S <- S[ids, ids, drop = FALSE]
  references <- lapply(references[sort(names(references))], sort)
  x <- list(covariance = S, references = references, source = source, method = method,
    justification = justification, role = role, source_family = source_family, sample_size = n)
  x$id <- .semantica_object_md5(x)
  structure(x, class = "semantica_reference_evidence")
}

.semantica_reference_context <- function(evidence, assignment, factors, counts, constraints) {
  if (is.null(evidence)) return(NULL)
  if (inherits(evidence, "semantica_reference_evidence")) evidence <- list(evidence)
  if (!is.list(evidence) || !length(evidence) ||
      !all(vapply(evidence, inherits, logical(1), "semantica_reference_evidence")))
    stop("reference_evidence requires declared semantica_reference_evidence objects.")
  ids <- names(assignment)
  panels <- lapply(evidence, function(p) {
    if (!identical(p$id, .semantica_object_md5(unclass(p)[setdiff(names(p), "id")])))
      stop("Reference evidence was modified; construct a new declaration before selection.")
    refs <- unlist(p$references, use.names = FALSE)
    if (!setequal(names(p$references), factors) || length(intersect(ids, refs)) ||
        !all(ids %in% rownames(p$covariance)))
      stop("Reference evidence must cover all admitted candidates and domains, with disjoint reference IDs.")
    H <- vapply(factors, function(f) as.numeric(refs %in% p$references[[f]]), numeric(length(refs)))
    p$xx <- p$covariance[ids, ids, drop = FALSE]
    p$xr <- p$covariance[ids, refs, drop = FALSE] %*% H
    p$vr <- diag(t(H) %*% p$covariance[refs, refs, drop = FALSE] %*% H)
    if (any(p$vr <= 0)) stop("A reference composite has zero variance.")
    p$covariance <- NULL; p
  })
  if (!any(vapply(panels, function(p) p$role == "primary", logical(1))))
    stop("Reference selection requires at least one explicitly justified primary panel.")
  list(id = .semantica_object_md5(list("semantica-reference-policy-v1", vapply(panels, `[[`, character(1), "id"),
    assignment, factors, counts, constraints)), panels = panels, assignment = assignment,
    factors = factors, counts = counts, constraints = constraints)
}

.semantica_reference_evaluate <- function(context, ids) {
  if (is.null(context)) return(NULL)
  fa <- context$assignment[ids]; fs <- context$factors; rules <- context$constraints
  if (anyNA(fa) || anyDuplicated(ids) ||
      !all(table(factor(fa, levels = fs)) == context$counts) ||
      .semantica_duplicate_conflict(ids, rules$duplicate_cluster_id) ||
      !isTRUE(.semantica_facet_constraint_diagnostics(ids, fa, rules$item.facet.lookup,
        rules$facets.by.factor, context$counts, rules$facet_constraint_mode, rules$facet_min_per_facet,
        rules$facet_max_imbalance, rules$duplicate_cluster_id)$passed)) return(NULL)
  rows <- values <- list()
  for (j in seq_along(context$panels)) {
    p <- context$panels[[j]]
    C <- t(vapply(fs, function(f) {
      a <- ids[fa == f]; variance <- sum(p$xx[a, a, drop = FALSE])
      if (variance <= 0) stop("A candidate composite has zero variance in the reference data.")
      colSums(p$xr[a, , drop = FALSE])/sqrt(variance*p$vr)
    }, numeric(length(fs))))
    own <- diag(C)
    margin <- if (length(fs) > 1L) own - vapply(seq_along(fs), function(k) max(abs(C[k, -k])), numeric(1)) else rep(NA_real_, length(fs))
    if (any(!is.finite(C))) stop("Nonfinite reference comparison; selection stopped.")
    rows[[j]] <- data.frame(panel = j, source = p$source, role = p$role, factor = fs,
      convergence = own, discrimination = margin)
    if (p$role == "primary") values[[as.character(j)]] <- stats::setNames(c(own, if (length(fs) > 1L) margin),
      c(paste(j, "convergence", fs, sep = "::"), if (length(fs) > 1L) paste(j, "discrimination", fs, sep = "::")))
  }
  list(criteria = do.call(c, unname(values)), panels = do.call(rbind, rows), reference_id = context$id)
}

.semantica_reference_dominates <- function(a, b) {
  identical(names(a), names(b)) && all(is.finite(c(a, b))) &&
    all(a >= b - 1e-12) && any(a > b + 1e-12)
}

.semantica_reference_front <- function(entries) {
  which(!vapply(seq_along(entries), function(i) any(vapply(seq_along(entries), function(j)
    .semantica_reference_dominates(entries[[j]]$reference$criteria, entries[[i]]$reference$criteria), logical(1))), logical(1)))
}

.semantica_reference_observe <- function(state, context, vectors, capacity) {
  if (is.null(context)) return(invisible(NULL))
  before <- vapply(state$entries, function(e) make_solution_key(e$vec), character(1))
  for (v in vectors) {
    key <- make_solution_key(v)
    if (exists(key, state$seen, inherits = FALSE)) next
    r <- .semantica_reference_evaluate(context, names(context$assignment)[v == 1L])
    if (is.null(r)) next
    assign(key, TRUE, state$seen); state$observed <- state$observed + 1L
    state$entries[[length(state$entries) + 1L]] <- list(vec = v, reference = r,
      score_type = "reference", score_schema = context$id)
  }
  if (length(state$entries)) {
    state$entries <- state$entries[.semantica_reference_front(state$entries)]
    keys <- vapply(state$entries, function(e) make_solution_key(e$vec), character(1))
    state$entries <- state$entries[order(keys)]
    if (length(state$entries) > capacity) state$truncated <- TRUE
    state$entries <- utils::head(state$entries, capacity)
  }
  after <- vapply(state$entries, function(e) make_solution_key(e$vec), character(1))
  state$changed <- !identical(before, after)
  invisible(NULL)
}

.semantica_reference_pheromone <- function(pheromone, entries, rho) {
  pheromone <- pheromone*(1-rho)
  for (e in entries) {
    ix <- cbind(seq_along(e$vec), ifelse(e$vec == 1L, 2L, 1L))
    pheromone[ix] <- pheromone[ix] + 1/length(entries)
  }
  pmax(pmin(pheromone, 50), .01)
}

.semantica_reference_refine <- function(state, context, items, heuristics, max_evals, max_seconds, capacity) {
  evaluate <- function(v) {
    r <- .semantica_reference_evaluate(context, names(context$assignment)[v == 1L])
    if (!is.null(r)) .semantica_reference_observe(state, context, list(v), capacity)
    # The generic walk needs a finite placeholder; only the reference vector
    # comparator governs acceptance. No scalar utility is inferred from zero.
    list(score = if (is.null(r)) -Inf else 0, reference = r)
  }
  v <- state$entries[[1L]]$vec
  .semantica_objective_polish(v, evaluate(v), items, heuristics, evaluate,
    function(v) !is.null(.semantica_reference_evaluate(context, names(context$assignment)[v == 1L])),
    max_evals, max_seconds, better = function(a, b)
      .semantica_reference_dominates(a$reference$criteria, b$reference$criteria))$diagnostics
}

.semantica_reference_result <- function(context, state) {
  if (!length(state$entries)) stop("No feasible candidates have reference evidence.")
  resolved <- length(state$entries) == 1L && !state$truncated
  items <- lapply(state$entries, function(e) names(context$assignment)[e$vec == 1L])
  list(schema = "semantica-reference-selection-v1", status = if (state$truncated) "unresolved_reference_archive_limit" else
      if (resolved) "unique_observed_reference_front" else "unresolved_reference_tradeoff",
    reference_id = context$id, recommended_items = if (resolved) items[[1L]] else NULL,
    provisional_items = items[[1L]], shortlist = items, observed_forms = state$observed,
    archive_truncated = state$truncated, criteria = lapply(state$entries, function(e) e$reference$criteria),
    panels = lapply(state$entries, function(e) e$reference$panels),
    provenance = lapply(context$panels, function(p) p[c("source", "method", "justification", "role", "source_family", "sample_size", "references", "id")]),
    uncertainty = "point_estimates_no_calibrated_confidence_claim",
    evidence_role = "development_selection_not_confirmatory_validation",
    scope = "observed_feasible_forms_within_search_budget_not_global_optimality")
}
