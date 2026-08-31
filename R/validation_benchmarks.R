# Validation benchmarking utilities for SEMANTICA.
#
# These functions are deliberately external-validation oriented. They do not
# convert sample-free semantic diagnostics into participant-based evidence;
# instead, they quantify how well semantic quantities predict independent human
# response structure.

.semantica_align_square_matrices <- function(matrices) {
  if (!is.list(matrices) || length(matrices) < 1L) stop("'matrices' must be a non-empty list.")
  mats <- lapply(matrices, as.matrix)
  ids <- Reduce(intersect, lapply(mats, rownames))
  if (is.null(ids) || length(ids) < 2L) {
    dims <- unique(vapply(mats, nrow, integer(1L)))
    if (length(dims) != 1L || any(vapply(mats, ncol, integer(1L)) != dims[[1L]])) {
      stop("Matrices need common row names or identical dimensions.")
    }
    ids <- rownames(mats[[1L]]) %||% paste0("item", seq_len(dims[[1L]]))
    for (i in seq_along(mats)) rownames(mats[[i]]) <- colnames(mats[[i]]) <- ids
  }
  mats <- lapply(mats, function(m) {
    if (nrow(m) != ncol(m)) stop("All matrices must be square.")
    if (!all(ids %in% rownames(m)) || !all(ids %in% colnames(m))) stop("Matrices do not share the same item IDs.")
    m[ids, ids, drop = FALSE]
  })
  list(matrices = mats, ids = ids)
}

#' Combine multiple semantic representations
#'
#' Experimental ensemble utility for robustness research. `rank_mean` averages
#' pairwise ranks and therefore reduces dependence on each model's raw cosine
#' scale. `mean` averages raw similarities and should only be used when the
#' component matrices are meaningfully calibrated to a common scale.
#'
#' @param matrices Named list of square similarity matrices.
#' @param method `"rank_mean"` or `"mean"`.
#' @param weights Optional non-negative representation weights.
#' @return Symmetric ensemble similarity matrix with unit diagonal.
#' @export
semantica_ensemble_similarity <- function(matrices,
                                          method = c("rank_mean", "mean"),
                                          weights = NULL) {
  method <- match.arg(method)
  aligned <- .semantica_align_square_matrices(matrices)
  mats <- aligned$matrices
  n <- length(mats)
  if (is.null(weights)) weights <- rep(1 / n, n)
  weights <- as.numeric(weights)
  if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    stop("'weights' must contain one non-negative finite weight per matrix.")
  }
  weights <- weights / sum(weights)
  idx <- lower.tri(mats[[1L]])
  vals <- vapply(mats, function(m) as.numeric(m[idx]), numeric(sum(idx)))
  if (method == "rank_mean") {
    vals <- apply(vals, 2L, function(x) rank(x, ties.method = "average", na.last = "keep"))
    if (is.null(dim(vals))) vals <- matrix(vals, ncol = n)
    denom <- pmax(1, colSums(is.finite(vals)) - 1)
    vals <- sweep(vals - 1, 2L, denom, "/")
  }
  combined <- apply(vals, 1L, function(x) {
    ok <- is.finite(x) & weights > 0
    if (!any(ok)) return(NA_real_)
    sum(x[ok] * weights[ok]) / sum(weights[ok])
  })
  out <- diag(1, nrow(mats[[1L]]))
  out[idx] <- combined
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  dimnames(out) <- list(aligned$ids, aligned$ids)
  attr(out, "semantica_ensemble") <- list(
    method = method,
    weights = weights,
    status = "experimental_representation_ensemble",
    source_family = "embedding_semantic_ensemble",
    participant_based = FALSE,
    dependency_note = "Combines multiple embedding representations; this is robustness evidence, not participant-response validation.",
    warning = if (method == "rank_mean") {
      "Rank ensemble preserves pair ordering, not the raw cosine scale."
    } else {
      "Raw means assume component similarities are sufficiently comparable."
    }
  )
  out
}

#' Build an ensemble clustering co-assignment matrix
#'
#' Each representation is clustered independently and the output reports the
#' proportion of representations in which each item pair belongs to the same
#' cluster. This is an experimental representation-stability diagnostic, not a
#' replacement for empirical factor analysis.
#'
#' @param matrices List of semantic similarity matrices.
#' @param k Number of clusters.
#' @param linkage Hierarchical clustering linkage method.
#' @return Co-assignment matrix with values between 0 and 1.
#' @export
semantica_ensemble_cluster_similarity <- function(matrices, k,
                                                  linkage = "average") {
  aligned <- .semantica_align_square_matrices(matrices)
  mats <- aligned$matrices
  k <- as.integer(k[1L])
  if (!is.finite(k) || k < 2L || k >= length(aligned$ids)) stop("'k' must be between 2 and n_items - 1.")
  memberships <- vapply(mats, function(m) {
    sim <- pmin(pmax((m + t(m)) / 2, -1), 1)
    diag(sim) <- 1
    dmat <- 1 - sim
    dmat[dmat < 0] <- 0
    diag(dmat) <- 0
    d <- stats::as.dist(dmat)
    stats::cutree(stats::hclust(d, method = linkage), k = k)
  }, integer(length(aligned$ids)))
  if (is.null(dim(memberships))) memberships <- matrix(memberships, ncol = 1L)
  out <- matrix(0, length(aligned$ids), length(aligned$ids), dimnames = list(aligned$ids, aligned$ids))
  for (j in seq_len(ncol(memberships))) {
    out <- out + outer(memberships[, j], memberships[, j], "==")
  }
  out <- out / ncol(memberships)
  diag(out) <- 1
  attr(out, "semantica_ensemble") <- list(
    method = "cluster_coassignment",
    k = k,
    linkage = linkage,
    n_representations = length(mats),
    status = "experimental_representation_ensemble",
    source_family = "embedding_semantic_ensemble",
    participant_based = FALSE,
    dependency_note = "Co-assignment agreement across embedding representations is not independent participant-response evidence."
  )
  out
}

#' Create an explicitly signed semantic relation matrix
#'
#' This polarity-aware transformation combines unsigned topic similarity with
#' analyst-supplied item polarity. It does not infer polarity automatically and
#' must not be interpreted as a validated replacement for human correlations.
#'
#' @param similarity_matrix Square semantic-similarity matrix.
#' @param polarity Named numeric/logical/character vector. Positive direction can
#'   be encoded as `1`, `"positive"`, or `"forward"`; negative direction as
#'   `-1`, `"negative"`, or `"reverse"`.
#' @return Signed symmetric matrix.
#' @export
semantica_signed_semantic_matrix <- function(similarity_matrix, polarity) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m)) stop("'similarity_matrix' must be square.")
  ids <- rownames(m) %||% paste0("item", seq_len(nrow(m)))
  if (is.null(colnames(m))) colnames(m) <- ids
  if (!is.null(names(polarity))) polarity <- polarity[ids]
  if (length(polarity) != length(ids)) stop("'polarity' must provide one value per item.")
  if (is.character(polarity)) {
    z <- tolower(trimws(polarity))
    p <- ifelse(z %in% c("negative", "reverse", "reversed", "-", "-1"), -1,
                ifelse(z %in% c("positive", "forward", "+", "+1", "1"), 1, NA_real_))
  } else if (is.logical(polarity)) {
    p <- ifelse(is.na(polarity), NA_real_, ifelse(polarity, 1, -1))
  } else {
    p <- ifelse(as.numeric(polarity) < 0, -1, ifelse(as.numeric(polarity) > 0, 1, NA_real_))
  }
  if (anyNA(p)) stop("Every polarity value must be classifiable as positive or negative.")
  out <- m * outer(p, p)
  out <- (out + t(out)) / 2
  diag(out) <- 1
  dimnames(out) <- list(ids, ids)
  attr(out, "semantica_signed_relation") <- list(
    status = "experimental_analyst_supplied_polarity",
    warning = paste(
      "Signs were supplied by the analyst rather than learned from respondent data.",
      "Validate this representation against independent human responses before inference."
    )
  )
  out
}

#' Benchmark a semantic matrix against an empirical item correlation matrix
#'
#' Benchmark that compares pairwise semantic similarities with an
#' empirical participant-response correlation structure.
#'
#' @param semantic_matrix Semantic similarity matrix.
#' @param response_matrix Empirical item correlation matrix.
#' @param response_data Optional participant-by-item data used when
#'   `response_matrix` is omitted.
#' @param cor_method Correlation method for `response_data`.
#' @return Pair-level data and prediction metrics.
#' @export
semantica_benchmark_matrix_prediction <- function(semantic_matrix,
                                                  response_matrix = NULL,
                                                  response_data = NULL,
                                                  cor_method = "pearson") {
  if (is.null(response_matrix)) {
    if (is.null(response_data)) stop("Provide 'response_matrix' or 'response_data'.")
    response_matrix <- stats::cor(as.data.frame(response_data), use = "pairwise.complete.obs", method = cor_method)
  }
  pairs <- .semantica_extract_pair_vectors(semantic_matrix, response_matrix)
  pairs <- pairs[is.finite(pairs$semantic) & is.finite(pairs$empirical), , drop = FALSE]
  if (nrow(pairs) < 3L) stop("Too few finite common item pairs for benchmarking.")
  list(
    n_pairs = nrow(pairs),
    pearson = suppressWarnings(stats::cor(pairs$semantic, pairs$empirical, method = "pearson")),
    spearman = suppressWarnings(stats::cor(pairs$semantic, pairs$empirical, method = "spearman")),
    rmse_identity = sqrt(mean((pairs$semantic - pairs$empirical)^2)),
    mae_identity = mean(abs(pairs$semantic - pairs$empirical)),
    pairs = pairs,
    evidence_role = "external_semantic_to_response_benchmark",
    source_families = c("embedding_semantic", "response_data"),
    participant_based = TRUE,
    validation_status = "external_response_benchmark_not_semantic_proxy_validation"
  )
}

.semantica_fit_xy_calibration <- function(x, y, method) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 6L) stop("Too few training item pairs.")
  if (method == "isotonic") {
    ord <- order(x)
    iso <- stats::isoreg(x[ord], y[ord])
    return(list(method = method, model = list(x = iso$x, y = iso$yf)))
  }
  if (method == "fisher_linear") {
    sx <- atanh(pmin(pmax(x, -0.999), 0.999))
    ry <- atanh(pmin(pmax(y, -0.999), 0.999))
    return(list(method = method, model = stats::lm(ry ~ sx)))
  }
  list(method = method, model = stats::lm(y ~ x))
}

.semantica_predict_xy_calibration <- function(fit, x) {
  if (fit$method == "isotonic") {
    return(pmin(pmax(stats::approx(fit$model$x, fit$model$y, xout = x, ties = mean, rule = 2)$y, -0.999), 0.999))
  }
  if (fit$method == "fisher_linear") {
    sx <- atanh(pmin(pmax(x, -0.999), 0.999))
    return(pmin(pmax(tanh(stats::predict(fit$model, newdata = data.frame(sx = sx))), -0.999), 0.999))
  }
  pmin(pmax(stats::predict(fit$model, newdata = data.frame(x = x)), -0.999), 0.999)
}

#' Leave-one-scale-out empirical calibration benchmark
#'
#' This benchmark prevents item-pair leakage within the same instrument by
#' training the semantic-to-response mapping on entire other scales and testing
#' on the held-out scale.
#'
#' @param datasets Named list. Each element must contain `semantic_matrix` and
#'   either `response_matrix` or `response_data`.
#' @param method Calibration method.
#' @param cor_method Correlation method for response data.
#' @return Per-scale holdout metrics and pooled summary.
#' @export
semantica_leave_one_scale_out_calibration <- function(
  datasets,
  method = c("isotonic", "linear", "fisher_linear"),
  cor_method = "pearson"
) {
  method <- match.arg(method)
  if (!is.list(datasets) || length(datasets) < 2L) stop("Provide at least two scale datasets.")
  if (is.null(names(datasets))) names(datasets) <- paste0("scale", seq_along(datasets))
  if (any(!nzchar(names(datasets))) || anyDuplicated(names(datasets))) {
    stop("'datasets' must have unique, non-empty scale names so holdout provenance is unambiguous.")
  }
  pairsets <- lapply(datasets, function(d) {
    rm <- d$response_matrix %||% NULL
    if (is.null(rm)) {
      if (is.null(d$response_data)) stop("Each dataset needs response_matrix or response_data.")
      rm <- stats::cor(as.data.frame(d$response_data), use = "pairwise.complete.obs", method = cor_method)
    }
    .semantica_extract_pair_vectors(d$semantic_matrix, rm)
  })
  rows <- vector("list", length(pairsets))
  all_predictions <- vector("list", length(pairsets))
  for (i in seq_along(pairsets)) {
    train <- do.call(rbind, pairsets[-i])
    test <- pairsets[[i]]
    train <- train[is.finite(train$semantic) & is.finite(train$empirical), , drop = FALSE]
    test <- test[is.finite(test$semantic) & is.finite(test$empirical), , drop = FALSE]
    fit <- .semantica_fit_xy_calibration(train$semantic, train$empirical, method)
    pred <- .semantica_predict_xy_calibration(fit, test$semantic)
    rows[[i]] <- data.frame(
      scale = names(pairsets)[i],
      held_out_scale = names(pairsets)[i],
      training_scales = paste(names(pairsets)[-i], collapse = " | "),
      n_train_scales = length(pairsets) - 1L,
      n_train_pairs = nrow(train),
      n_test_pairs = nrow(test),
      pearson = suppressWarnings(stats::cor(pred, test$empirical, method = "pearson")),
      spearman = suppressWarnings(stats::cor(pred, test$empirical, method = "spearman")),
      rmse = sqrt(mean((pred - test$empirical)^2)),
      mae = mean(abs(pred - test$empirical)),
      stringsAsFactors = FALSE
    )
    all_predictions[[i]] <- data.frame(scale = names(pairsets)[i], empirical = test$empirical, predicted = pred)
  }
  metrics <- do.call(rbind, rows)
  pred_all <- do.call(rbind, all_predictions)
  list(
    method = method,
    per_scale = metrics,
    pooled = list(
      pearson = suppressWarnings(stats::cor(pred_all$predicted, pred_all$empirical, method = "pearson")),
      spearman = suppressWarnings(stats::cor(pred_all$predicted, pred_all$empirical, method = "spearman")),
      rmse = sqrt(mean((pred_all$predicted - pred_all$empirical)^2)),
      mae = mean(abs(pred_all$predicted - pred_all$empirical))
    ),
    predictions = pred_all,
    validation_status = "out_of_scale_external_cross_validation",
    leakage_control = "entire_scale_holdout_no_within_instrument_pair_split",
    source_families = c("embedding_semantic", "response_data"),
    participant_based = TRUE,
    evidence_role = "external_semantic_to_response_calibration_benchmark"
  )
}

#' Compare item-selection methods under an explicit evaluation budget
#'
#' The function is optimizer-agnostic: each candidate method receives the same
#' problem object and declared budget, while the analyst supplies an independent
#' evaluator (ideally held-out participant data). This makes ACO-vs-simple-search
#' ablations reproducible without hard-coding a favorable baseline.
#'
#' @param methods Named list of functions `(problem, budget, seed) -> result`.
#' @param problem Arbitrary selection problem object passed to each method.
#' @param budget Common computational/evaluation budget.
#' @param evaluator Function mapping a method result to a named numeric vector or
#'   one-row data frame of independent evaluation metrics.
#' @param seeds Integer seeds for repeated comparisons.
#' @return Long-form benchmark table plus raw method results.
#' @export
semantica_compare_selection_methods <- function(methods, problem, budget,
                                                evaluator,
                                                seeds = c(1L, 2L, 3L)) {
  if (!is.list(methods) || !length(methods) || is.null(names(methods))) stop("'methods' must be a named list of functions.")
  if (!all(vapply(methods, is.function, logical(1L)))) stop("Every selection method must be a function.")
  if (!is.function(evaluator)) stop("'evaluator' must be a function.")
  records <- list(); raw <- list(); k <- 0L
  for (seed in as.integer(seeds)) {
    for (nm in names(methods)) {
      started <- proc.time()[["elapsed"]]
      res <- methods[[nm]](problem = problem, budget = budget, seed = seed)
      elapsed <- proc.time()[["elapsed"]] - started
      ev <- evaluator(res)
      if (is.data.frame(ev)) {
        if (nrow(ev) != 1L) stop("'evaluator' data frame must contain one row per run.")
        ev <- as.list(ev[1L, , drop = FALSE])
      } else if (is.atomic(ev)) ev <- as.list(ev)
      k <- k + 1L
      records[[k]] <- data.frame(method = nm, seed = seed, elapsed_seconds = elapsed,
                                 as.data.frame(ev, stringsAsFactors = FALSE), check.names = FALSE)
      raw[[paste(nm, seed, sep = "::")]] <- res
    }
  }
  list(
    results = do.call(rbind, records),
    raw = raw,
    budget = budget,
    evidence_role = "selection_method_external_benchmark",
    benchmark_requirements = c("same_declared_budget", "same_seed_set", "independent_evaluator"),
    validation_status = "depends_on_user_supplied_independent_evaluator",
    weighting_policy = "Do not change default semantic/PFA/ESEM weights from this benchmark unless evaluation uses held-out participant or other genuinely external validation evidence."
  )
}
