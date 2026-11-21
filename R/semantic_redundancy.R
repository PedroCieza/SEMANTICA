# Pair evidence is separate from the semantic matrix and from selection policy.
.semantica_redundancy_version <- "semantic-pairs-v1"

#' Configure experimental semantic pair constraints
#'
#' Opt in through `semantica_quality_config(redundancy = ...)`. NULL retains
#' the historical duplicate policy. Encoder weights and semantic scores are
#' unchanged. In this mode the historical cosine duplicate penalty is disabled.
#' @param judgments Optional table with item_a, item_b, text_a, text_b, decision
#'   (conflict/compatible/unresolved), source (human/model/other), reviewer and
#'   rationale. Bind judgments to exact IDs and wording; provenance is declared.
#' @param calibration Optional result of [semantica_fit_redundancy()].
#' @param automatic Explicitly enable experimental calibrated conflicts. Merely
#'   supplying calibration keeps its predictions diagnostic.
#' @param representation Nonempty identifier covering encoder revision, task,
#'   pooling, normalization, language/domain and reference-panel design. Required
#'   for calibration; applicability and identity are declared, not attested.
#' @param max_nodes Positive branch budget for joint pair/blueprint feasibility.
#'   Exhaustion raises an unknown-feasibility error, never an infeasibility claim.
#' @return A policy object. No semantic decision thresholds are supplied by default.
#' @export
semantica_redundancy_policy <- function(judgments = NULL, calibration = NULL,
    automatic = FALSE, representation = NULL, max_nodes = 10000L) {
  automatic <- .semantica_assert_flag(automatic, "automatic")
  max_nodes <- .semantica_assert_positive_integer(max_nodes, "max_nodes")
  if (!is.null(representation) && (!is.character(representation) ||
      length(representation) != 1L || is.na(representation) || !nzchar(trimws(representation))))
    stop("representation must be NULL or one nonempty configuration identifier.")
  if (!is.null(calibration) && (!inherits(calibration, "semantica_redundancy_calibration") ||
      !identical(calibration$version, .semantica_redundancy_version) ||
      !identical(calibration$representation, representation) ||
      !identical(calibration$integrity, .semantica_object_md5(
        unclass(calibration)[setdiff(names(calibration), "integrity")]))))
    stop("Calibration version/representation does not match this policy.")
  if (automatic && (is.null(calibration) || !isTRUE(calibration$usable)))
    stop("Automatic exclusions require an explicitly supplied usable experimental calibration.")
  structure(list(judgments = judgments, calibration = calibration, automatic = automatic,
    representation = representation, max_nodes = max_nodes), class = "semantica_redundancy_policy")
}

.semantica_redundancy_policy <- function(x) {
  if (is.null(x)) return(NULL)
  if (!inherits(x, "semantica_redundancy_policy")) stop("Use semantica_redundancy_policy().")
  do.call(semantica_redundancy_policy, unclass(x))
}

#' Audit semantic redundancy without response data or encoder training
#'
#' Relative ranks and leave-pair-out Spearman profiles are descriptive evidence,
#' not duplicate probabilities. Constant profiles are unresolved. The original
#' matrix is never modified. Exact copies normalize whitespace only.
#' @param similarity Finite symmetric named cosine/similarity matrix, diagonal one.
#' @param items Item table with IDs and wording (same conventions as content review).
#' @param representation Configuration identifier used to bind later calibration.
#'   Include encoder/task settings and the intended reference-panel context;
#'   rank normalization does not establish transfer across different pool designs.
#' @param judgments Optional pair judgments as in [semantica_redundancy_policy()].
#' @param panel_id Optional semantic template-family/panel identifier. Required
#'   when fitting calibration; related paraphrase panels must share this ID.
#' @return An audit with item-bound pair evidence and no inferred hard exclusions.
#' @export
semantica_redundancy_audit <- function(similarity, items, representation = NULL,
    judgments = NULL, panel_id = NULL) {
  semantica_redundancy_policy(representation = representation)
  if (!is.null(panel_id) && (!is.character(panel_id) || length(panel_id) != 1L ||
      is.na(panel_id) || !nzchar(trimws(panel_id)))) stop("panel_id must be NULL or one nonempty family identifier.")
  x <- .semantica_review_metadata(items)
  ids <- x$item_id; n <- length(ids)
  S <- as.matrix(similarity)
  if (!is.numeric(S) || nrow(S) != ncol(S) || anyNA(rownames(S)) ||
      anyDuplicated(rownames(S)) || !identical(rownames(S), colnames(S)) ||
      !all(ids %in% rownames(S)) || any(!is.finite(S)) ||
      any(abs(S) > 1 + 1e-8) || max(abs(S - t(S)), 0) > 1e-8 ||
      any(abs(diag(S) - 1) > 1e-8) || !n || anyNA(x$item_text) ||
      any(!nzchar(trimws(x$item_text)))) stop("Supply a valid named matrix and nonempty item wording.")
  S <- S[ids, ids, drop = FALSE]
  diag(S) <- NA_real_
  ranks <- t(vapply(seq_len(n), function(i) rank(-S[i, ], na.last = "keep"), numeric(n)))
  profiles <- if (n >= 4L) suppressWarnings(stats::cor(t(S),
    use = "pairwise.complete.obs", method = "spearman")) else matrix(NA_real_, n, n)
  ij <- which(upper.tri(S), arr.ind = TRUE)
  a <- ij[, 1L]; b <- ij[, 2L]
  wording <- trimws(gsub("[[:space:]]+", " ", enc2utf8(x$item_text)))
  pairs <- data.frame(item_a = ids[a], item_b = ids[b], text_a = x$item_text[a],
    text_b = x$item_text[b], cosine = S[ij],
    mutual_rank = pmax(ranks[ij], t(ranks)[ij]) / max(1L, n - 1L),
    profile = profiles[ij], exact_copy = wording[a] == wording[b],
    decision = rep("unresolved", length(a)), source = rep("", length(a)),
    reviewer = rep("", length(a)), rationale = rep("", length(a)),
    stringsAsFactors = FALSE)
  if (!is.null(judgments)) {
    fields <- c("item_a", "item_b", "text_a", "text_b", "decision", "source", "reviewer", "rationale")
    if (!is.data.frame(judgments) || anyDuplicated(names(judgments)) ||
        !all(fields %in% names(judgments)) || anyNA(judgments[fields]))
      stop("Pair judgments require IDs, exact wording, decision and provenance.")
    j <- as.data.frame(lapply(judgments[fields], as.character), stringsAsFactors = FALSE)
    ia <- match(j$item_a, ids); ib <- match(j$item_b, ids)
    if (anyNA(c(ia, ib)) || any(ia == ib) || any(j$text_a != x$item_text[ia]) ||
        any(j$text_b != x$item_text[ib])) stop("Pair judgment IDs or wording are stale/unknown.")
    # Match by integer endpoints, independent of user-supplied orientation or ID punctuation.
    key <- function(i, k) paste(pmin(i, k), pmax(i, k), sep = ":")
    jk <- key(ia, ib)
    if (anyDuplicated(jk) || any(!j$decision %in% c("conflict", "compatible", "unresolved")))
      stop("Pair judgments must be unique with valid decisions.")
    reviewed <- j$decision != "unresolved"
    if (any(reviewed & (!j$source %in% c("human", "model", "other") |
        !nzchar(trimws(j$reviewer)) | !nzchar(trimws(j$rationale)))))
      stop("Reviewed pairs require source, reviewer and rationale.")
    index <- match(jk, key(a, b))
    pairs[index, fields[5:8]] <- j[fields[5:8]]
    if (any(pairs$exact_copy & pairs$decision == "compatible"))
      stop("An exact wording copy cannot be declared compatible under this policy.")
  }
  structure(list(version = .semantica_redundancy_version, representation = representation,
    panel_id = panel_id, items = x, pairs = pairs,
    fingerprint = .semantica_object_md5(list(.semantica_redundancy_version, representation, x, similarity, pairs))),
    class = "semantica_redundancy_audit")
}

.semantica_redundancy_metrics <- function(pairs, q, p) {
  known <- pairs$decision %in% c("conflict", "compatible")
  positive <- pairs$decision == "conflict"
  resolved <- is.finite(pairs$mutual_rank) & is.finite(pairs$profile)
  predicted <- resolved & pairs$mutual_rank <= q & pairs$profile >= p
  c(positives = sum(known & positive), negatives = sum(known & !positive),
    true_positive = sum(predicted & positive & known), false_positive = sum(predicted & !positive & known),
    false_merge_rate = sum(predicted & !positive & known)/sum(known & !positive),
    recall = sum(predicted & positive & known)/sum(known & positive),
    unresolved = sum(known & !resolved))
}

#' Fit an experimental semantic exclusion rule on reviewed control panels
#'
#' Encoder weights are unchanged. Fits q <= rank_max AND profile >= profile_min
#' by maximum development recall subject to a declared empirical false-merge
#' limit. Ties prefer fewer false merges, then narrower boundaries. Validation
#' is evaluated once and never tunes boundaries. This reports empirical control
#' performance, not a population error guarantee or independent authentication.
#' @param development,validation Lists of [semantica_redundancy_audit()] objects
#'   with reviewed positive/negative pairs. Exact-copy pairs are omitted from
#'   fitting. Panel/template-family IDs and exact wording must be disjoint across
#'   the two splits. Caller must also prevent semantic/paraphrase leakage.
#' @param max_false_merge Explicit maximum empirical false-positive fraction
#'   among reviewed nonduplicates; no scientific default is chosen.
#' @param max_rules Computational limit on candidate boundaries; no subsampling
#'   or threshold approximation is performed when the limit is exceeded.
#' @return Experimental calibration, split metrics and frozen representation ID.
#'   Automatic activation remains an explicit separate policy choice.
#' @export
semantica_fit_redundancy <- function(development, validation, max_false_merge, max_rules = 10000L) {
  max_false_merge <- .semantica_assert_probability(max_false_merge, "max_false_merge")
  max_rules <- .semantica_assert_positive_integer(max_rules, "max_rules")
  panels <- c(development, validation)
  if (!length(development) || !length(validation) ||
      !all(vapply(panels, inherits, logical(1L), "semantica_redundancy_audit")))
    stop("Supply nonempty development and held-out validation lists of redundancy audits.")
  rep_id <- panels[[1L]]$representation
  if (is.null(rep_id) || !all(vapply(panels, function(a)
      identical(a$version, .semantica_redundancy_version) && identical(a$representation, rep_id) &&
        is.character(a$panel_id) && length(a$panel_id) == 1L && !is.na(a$panel_id) &&
        nzchar(trimws(a$panel_id)), logical(1L)))) stop("All panels need the same representation/version and a panel_id.")
  families <- function(x) vapply(x, `[[`, character(1L), "panel_id")
  words <- function(x) unique(unlist(lapply(x, function(a)
    trimws(gsub("[[:space:]]+", " ", enc2utf8(a$items$item_text))))))
  if (length(intersect(families(development), families(validation))) ||
      length(intersect(words(development), words(validation)))) stop("Development and validation panels/wording overlap.")
  collect <- function(x) {
    p <- do.call(rbind, lapply(x, `[[`, "pairs"))
    p <- p[!p$exact_copy & p$decision %in% c("conflict", "compatible"), , drop = FALSE]
    if (!all(c("conflict", "compatible") %in% p$decision))
      stop("Each split requires reviewed nonexact duplicate and nonduplicate controls.")
    p
  }
  d <- collect(development); v <- collect(validation)
  qs <- sort(unique(c(0, d$mutual_rank[is.finite(d$profile)])))
  ps <- sort(unique(c(1, d$profile[is.finite(d$profile)])))
  if (length(qs) * length(ps) > max_rules) stop("Calibration rule budget exceeded; increase max_rules explicitly.")
  grid <- expand.grid(rank_max = qs, profile_min = ps)
  scores <- t(vapply(seq_len(nrow(grid)), function(i)
    .semantica_redundancy_metrics(d, grid$rank_max[i], grid$profile_min[i]), numeric(7L)))
  valid <- which(scores[, "false_merge_rate"] <= max_false_merge)
  chosen <- valid[order(-scores[valid, "true_positive"], scores[valid, "false_positive"],
    grid$rank_max[valid], -grid$profile_min[valid])][1L]
  rule <- grid[chosen, , drop = FALSE]
  metrics <- .semantica_redundancy_metrics(v, rule$rank_max, rule$profile_min)
  out <- structure(list(version = .semantica_redundancy_version, representation = rep_id,
    rule = rule, development = scores[chosen, ], validation = metrics,
    max_false_merge = max_false_merge,
    usable = scores[chosen, "true_positive"] > 0 && metrics["true_positive"] > 0 &&
      metrics["false_merge_rate"] <= max_false_merge,
    status = "experimental_empirical_control_evaluation",
    panel_ids = list(development = families(development), validation = families(validation)),
    panel_sizes = vapply(panels, function(a) nrow(a$items), integer(1L)),
    panel_metrics = lapply(list(development = development, validation = validation),
      function(split) do.call(rbind, lapply(split, function(a) data.frame(panel_id = a$panel_id,
        as.list(.semantica_redundancy_metrics(a$pairs[!a$pairs$exact_copy, , drop = FALSE],
          rule$rank_max, rule$profile_min)), row.names = NULL)))),
    fingerprints = vapply(panels, function(a) .semantica_object_md5(
      unclass(a)[setdiff(names(a), "fingerprint")]), character(1L))),
    class = "semantica_redundancy_calibration")
  out$integrity <- .semantica_object_md5(unclass(out))
  out
}

.semantica_pair_constraints <- function(audit, policy) {
  p <- audit$pairs
  predicted <- rep(FALSE, nrow(p))
  if (!is.null(policy$calibration)) {
    r <- policy$calibration$rule
    predicted <- is.finite(p$profile) & p$mutual_rank <= r$rank_max & p$profile >= r$profile_min
  }
  p$calibrated_prediction <- predicted
  p$hard_conflict <- p$exact_copy | p$decision == "conflict" |
    (policy$automatic & predicted & p$decision != "compatible")
  neighbors <- stats::setNames(rep(list(character()), nrow(audit$items)), audit$items$item_id)
  for (i in which(p$hard_conflict)) {
    a <- p$item_a[i]; b <- p$item_b[i]
    neighbors[[a]] <- c(neighbors[[a]], b); neighbors[[b]] <- c(neighbors[[b]], a)
  }
  audit$pairs <- p
  structure(list(neighbors = neighbors, audit = audit, max_nodes = policy$max_nodes,
    automatic = policy$automatic), class = "semantica_pair_constraints")
}

.semantica_duplicate_evidence <- function(S, ids, threshold, assignment, redundancy = NULL, data = NULL) {
  policy <- .semantica_redundancy_policy(redundancy)
  if (is.null(policy)) return(identify_duplicate_clusters(S, ids, threshold,
    max(0.9995, threshold), assignment))
  audit <- semantica_redundancy_audit(S, data, policy$representation, policy$judgments)
  constraints <- .semantica_pair_constraints(audit, policy)
  list(item_cluster = constraints, clusters = list(), n_clusters = 0L,
    n_items_clustered = sum(lengths(constraints$neighbors) > 0L),
    n_exact_pairs = sum(audit$pairs$exact_copy), threshold = NA_real_,
    method = "semantic_pair_constraints", pair_evidence = constraints$audit)
}

.semantica_duplicate_conflict <- function(ids, constraints) {
  if (inherits(constraints, "semantica_pair_constraints"))
    return(any(vapply(constraints$neighbors[ids], function(x) any(x %in% ids), logical(1L))))
  cid <- constraints[ids]
  anyDuplicated(cid[!is.na(cid) & nzchar(cid)]) > 0L
}

.semantica_duplicate_units <- function(ids, constraints) {
  if (inherits(constraints, "semantica_pair_constraints")) return(NA_integer_)
  if (is.null(constraints)) return(length(ids))
  cid <- constraints[ids]; grouped <- !is.na(cid) & nzchar(cid)
  sum(!grouped) + length(unique(cid[grouped]))
}

# Reuse the exact quota flow solver; branch only on a violated direct edge.
.semantica_joint_pair_selection <- function(eligible, counts, constraints, ...) {
  args <- list(...)
  if (is.null(args$priority) && !is.null(args$probs)) {
    ids <- unlist(eligible[names(counts)], use.names = FALSE)
    args$priority <- ids[.semantica_constraint_priority_order(args$probs[ids])]
  }
  nodes <- 0L
  search <- function(pool) {
    nodes <<- nodes + 1L
    if (nodes > constraints$max_nodes) stop(structure(list(
      message = "Pair-constraint feasibility budget exhausted; feasibility is unknown.",
      call = NULL, status = "unknown", nodes = nodes),
      class = c("semantica_error_constraint_budget", "semantica_error", "error", "condition")))
    fit <- do.call(.semantica_joint_pool_selection,
      c(list(eligible_items = pool, i_per_f = counts, duplicate_cluster_id = NULL), args))
    if (!isTRUE(fit$feasible) || !.semantica_duplicate_conflict(fit$items, constraints)) return(fit)
    a <- fit$items[vapply(constraints$neighbors[fit$items], function(x)
      any(x %in% fit$items), logical(1L))][1L]
    b <- intersect(constraints$neighbors[[a]], fit$items)[1L]
    drops <- c(a, b)
    if (!is.null(args$priority)) drops <- drops[order(match(drops, args$priority), decreasing = TRUE)]
    for (drop in drops) {
      branch <- search(lapply(pool, setdiff, y = drop))
      if (isTRUE(branch$feasible)) return(branch)
    }
    list(feasible = FALSE, items = character(), unfilled_domain_slots = counts,
      deficit_allocation = "conservative_recovery_targets_not_minimum")
  }
  answer <- search(eligible)
  if (!isTRUE(answer$feasible) && is.null(answer$unfilled_domain_slots)) {
    answer$unfilled_domain_slots <- counts
    answer$deficit_allocation <- "conservative_recovery_targets_not_minimum"
  }
  answer$method <- "quota_flow_with_pair_branching"; answer$nodes <- nodes
  answer
}
