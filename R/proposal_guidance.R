# Secondary search information, never a replacement for scalar quality.
.semantica_initial_item_ids <- function(ids) {
  if (is.null(ids)) return(NULL)
  if (!is.character(ids) || !length(ids) || anyNA(ids) ||
      any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    stop("'initial_items' must contain unique, nonempty item IDs.", call. = FALSE)
  }
  unname(ids)
}

#' Bind an initial nomination to recorded wording and its source
#'
#' Pass the nomination as `initial_items` in [semantica_quality_config()] or
#' [ACO_with_ESEM()]. SEMANTICA checks exact wording against the active pool.
#' Source declarations are recorded as provenance.
#'
#' @param items Unique IDs of the nominated items.
#' @param item_texts Recorded wording from the nomination source, named by ID.
#'   It may include other items in the same pool.
#' @param source Nonempty identifier for the nomination evidence.
#' @param source_family Declared kind of evidence.
#' @param fallback_items Optional primary-source form to try if the nominee
#'   fails native eligibility, facet, or duplicate checks.
#' @return A source-bound initial nomination for one existing ant slot.
#' @export
semantica_source_nomination <- function(items, item_texts, source,
    source_family = c("alternate_encoder", "response_data", "synthetic"), fallback_items = NULL) {
  items <- .semantica_initial_item_ids(items)
  fallback_items <- .semantica_initial_item_ids(fallback_items)
  source_family <- match.arg(source_family)
  if (is.null(items) || !is.character(item_texts) || anyDuplicated(names(item_texts)) ||
      !all(items %in% names(item_texts)) || anyNA(item_texts[items]) ||
      any(!nzchar(trimws(item_texts[items]))))
    stop("Provide recorded wording named by the nominated item IDs.", call. = FALSE)
  if (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(trimws(source)))
    stop("Provide a nonempty nomination source identifier.", call. = FALSE)
  value <- list(items = items, item_texts = item_texts[items],
    source = source, source_family = source_family, fallback_items = fallback_items)
  value$id <- .semantica_object_md5(value)
  structure(value, class = "semantica_source_nomination")
}

.semantica_initial_input <- function(x) {
  if (!inherits(x, "semantica_source_nomination")) return(.semantica_initial_item_ids(x))
  value <- unclass(x)
  if (!identical(value$id, .semantica_object_md5(value[setdiff(names(value), "id")])))
    stop("Source nomination changed after construction.", call. = FALSE)
  x
}

.semantica_source_nomination_check <- function(x, item_ids, df) {
  if (!inherits(x, "semantica_source_nomination")) return(invisible(NULL))
  field <- .semantica_pick_column(df, c("item_text", "text", "wording", "statement", "item"))
  text <- if (!is.null(field)) as.character(df[[field]][match(x$items, item_ids)])
  if (!identical(enc2utf8(text %||% character()), enc2utf8(unname(x$item_texts))))
    stop("Source nomination wording or item IDs differ from the active pool.", call. = FALSE)
  invisible(NULL)
}

.semantica_resolve_initial_proposal <- function(initial, ...) {
  if (!inherits(initial, "semantica_source_nomination"))
    return(.semantica_initial_proposal(initial, ...))
  proposal <- .semantica_initial_proposal(initial$items, ...)
  source_status <- proposal$status
  used <- "source"
  if (identical(source_status, "infeasible") && length(initial$fallback_items)) {
    proposal <- .semantica_initial_proposal(initial$fallback_items, ...)
    used <- "fallback"
  }
  proposal$nomination <- list(id = initial$id, source = initial$source,
    source_family = initial$source_family, source_status = source_status,
    source_items = initial$items, attempted = used)
  proposal
}

.semantica_initial_proposal <- function(ids, slot, eligible, assignment, counts,
                                        clusters, facets, by_factor, mode, minimum, imbalance) {
  if (is.null(ids)) return(NULL)
  eligible_ids <- unlist(eligible, use.names = FALSE)
  if (any(!ids %in% eligible_ids) ||
      !isTRUE(.semantica_facet_constraint_diagnostics(ids, assignment[ids],
        facets, by_factor, counts, mode, minimum, imbalance, clusters)$passed)) {
    return(list(status = "infeasible", items = ids))
  }
  if (slot < 1L) return(list(status = "slot_unavailable", items = ids))
  list(status = "ready", items = ids, slot = slot)
}

.semantica_signed_guidance <- function(relative) {
  tab <- relative$factor_table
  required <- c("factor", "stochastic_superiority", "standardized_gap")
  if (!is.data.frame(tab) || !nrow(tab) || !all(required %in% names(tab)) ||
      anyNA(tab$factor) || anyDuplicated(tab$factor)) return(numeric())
  z <- tab$standardized_gap
  q <- pmin(2 * tab$stochastic_superiority - 1, z / (1 + abs(z)))
  q[!is.finite(z) | !is.finite(tab$stochastic_superiority) |
    tab$stochastic_superiority < 0 | tab$stochastic_superiority > 1] <- NA_real_
  stats::setNames(q, tab$factor)
}

.semantica_guidance_available <- function(x) is.numeric(x) && length(x) > 0L && all(is.finite(x))

.semantica_proposal_better <- function(candidate, incumbent) {
  a <- candidate$score; b <- incumbent$score
  if (length(a) != 1L || length(b) != 1L || !is.finite(a) || !is.finite(b)) return(FALSE)
  if (a != b) return(a > b)
  x <- candidate$proposal_guidance; y <- incumbent$proposal_guidance
  if (!.semantica_guidance_available(x) || !.semantica_guidance_available(y) ||
      length(x) != length(y) || !setequal(names(x), names(y))) return(FALSE)
  difference <- sort(x) - sort(y)
  first <- which(difference != 0)[1L]
  !is.na(first) && difference[first] > 0
}

.semantica_proposal_scalar_better <- function(candidate, incumbent) {
  candidate$proposal_guidance <- incumbent$proposal_guidance <- NULL
  .semantica_proposal_better(candidate, incumbent)
}

.semantica_proposal_summary <- function(scores, guidance) {
  finite <- scores[is.finite(scores)]
  list(schema = "semantica-proposal-guidance-v1", evaluated = length(scores),
    finite = length(finite), zero = sum(finite == 0), distinct = length(unique(finite)),
    best_ties = if (length(finite)) sum(finite == max(finite)) else 0L,
    signed_guidance_available = sum(vapply(guidance, .semantica_guidance_available, logical(1L))),
    status = if (!length(finite)) "no_finite_proposals" else if (length(finite) < 2L)
      "insufficient_comparisons" else if (length(unique(finite)) == 1L)
      "no_scalar_discrimination_among_evaluated_proposals" else "scalar_discrimination_observed",
    scope = "evaluated_proposals_only_not_full_pool_or_participant_validation")
}

# Preserve the full-budget scalar walk before spending any remaining budget
# crossing signed ties. Screens cannot fit; only the terminal proposal may
# request one explicitly authorized native final fit.
.semantica_run_proposal_rescue <- function(vector, baseline, items_by_factor, heuristics,
    screen, evaluate_final, feasible, max_evals, max_seconds, allow_fit = FALSE) {
  initial <- screen(vector)
  diagnostics <- list(schema = "proposal-rescue-v2", enabled = TRUE, reason = "no_finite_proposal", selected = FALSE,
    baseline_final_score = baseline$score, candidate_final_score = NA_real_,
    esem_fits = 0L, certificate_scope = "none", allow_fit = allow_fit,
    max_evals = max_evals, max_seconds = max_seconds)
  if (!is.finite(initial$score))
    return(list(vector = vector, evaluation = NULL, diagnostics = diagnostics))
  started <- proc.time()[["elapsed"]]
  screens <- new.env(parent = emptyenv())
  assign(make_solution_key(vector), initial, screens)
  cache_hits <- computations <- 0L
  cached_screen <- function(v) {
    key <- make_solution_key(v)
    if (exists(key, screens, inherits = FALSE)) {
      cache_hits <<- cache_hits + 1L
      return(get(key, screens, inherits = FALSE))
    }
    value <- screen(v); computations <<- computations + 1L
    assign(key, value, screens); value
  }
  walk <- .semantica_objective_polish(vector, initial, items_by_factor, heuristics,
    cached_screen, feasible, max_evals, max_seconds, better = .semantica_proposal_scalar_better)
  primary <- walk$diagnostics
  scalar_score <- walk$evaluation$score
  remaining_evals <- max_evals - primary$evaluated
  remaining_seconds <- max_seconds - (proc.time()[["elapsed"]] - started)
  secondary <- list(enabled = FALSE, reason = if (!primary$neighborhood_complete)
    "primary_neighborhood_unfinished" else if (primary$unavailable > 0L)
      "primary_evaluations_unavailable" else if (!.semantica_guidance_available(walk$evaluation$proposal_guidance))
        "signed_guidance_unavailable" else if (remaining_evals <= 0L || remaining_seconds <= 0)
          "no_remaining_budget" else "ready")
  if (secondary$reason == "ready") {
    walk <- .semantica_objective_polish(walk$vector, walk$evaluation, items_by_factor, heuristics,
      cached_screen, feasible, remaining_evals, remaining_seconds, better = .semantica_proposal_better)
    secondary <- walk$diagnostics
  }
  diagnostics <- utils::modifyList(diagnostics, walk$diagnostics)
  phases <- list(scalar = primary, signed = if (isTRUE(secondary$enabled)) secondary else NULL)
  for (field in c("evaluated", "proposed", "accepted", "screened", "infeasible", "bound_excluded",
      "unavailable", "unvisited_screened", "budget_used"))
    diagnostics[[field]] <- sum(vapply(phases, function(x) x[[field]] %||% 0L, numeric(1L)))
  histories <- lapply(names(phases), function(name) {
    h <- phases[[name]]$history
    if (!is.null(h) && nrow(h)) { h$phase <- name; h } else NULL
  })
  diagnostics$history <- do.call(rbind, histories) %||% data.frame()
  diagnostics$primary <- primary[setdiff(names(primary), "history")]
  diagnostics$secondary <- secondary[setdiff(names(secondary), "history")]
  diagnostics$scalar_only_score <- scalar_score
  diagnostics$scalar_only_score_preserved <- walk$evaluation$score >= scalar_score
  diagnostics$screen_cache_hits <- cache_hits
  diagnostics$screen_computations <- computations
  diagnostics$max_evals <- max_evals; diagnostics$max_seconds <- max_seconds
  diagnostics$elapsed_seconds <- proc.time()[["elapsed"]] - started
  diagnostics$score_before <- initial$score
  diagnostics$certificate_scope <- "none"
  diagnostics$comparison <- "scalar_first_then_signed_with_remaining_budget"
  diagnostics$proposal_before <- initial$score
  diagnostics$proposal_after <- walk$evaluation$score
  diagnostics$guidance_before <- initial$proposal_guidance
  diagnostics$guidance_after <- walk$evaluation$proposal_guidance
  diagnostics$candidate_items <- names(walk$vector)[walk$vector == 1L]
  final <- if (diagnostics$accepted > 0L)
    .semantica_preserve_caller_rng(evaluate_final(walk$vector, allow_fit)) else NULL
  if (!is.null(final)) {
    diagnostics$candidate_final_score <- final$score
    diagnostics$esem_fits <- as.integer(isTRUE(final$archive_esem_fit_started))
    diagnostics$final_status <- if (isTRUE(final$budget_exhausted)) "fit_budget_unavailable" else
      if (isTRUE(final$bound_dominated)) "bound_excluded" else
        if (is.finite(final$score)) "evaluated" else "unavailable"
    tolerance <- sqrt(.Machine$double.eps) * max(1, abs(baseline$score))
    diagnostics$selected <- is.finite(final$score) && final$score > baseline$score + tolerance
  } else diagnostics$final_status <- "not_attempted"
  list(vector = walk$vector, evaluation = final, diagnostics = diagnostics)
}
