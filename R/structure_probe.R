.semantica_pfa_residuals <- function(matrix, loadings, phi) {
  implied <- tcrossprod(loadings %*% phi, loadings)
  residual <- (matrix - implied)[upper.tri(matrix)]
  list(uniquenesses = 1 - diag(implied), rms_offdiag = sqrt(mean(residual^2)))
}

#' Probe item structure without a target assignment
#'
#' Fits sample-free factors using a supplied count or the descriptive
#' eigenvalue-greater-than-one heuristic.
#' @param cosine_matrix Finite, symmetric item cosine matrix with unit diagonal.
#' @param n_factors Positive factor count, or NULL for the heuristic count.
#' @param extraction Extraction method. Minimum residual requires package psych.
#' @param rotation Exploratory rotation. One-factor solutions are unrotated.
#' @return Counts, fit status, loadings, factor correlations, item assignments,
#'   residual RMS, uniquenesses and matrix repair diagnostics.
#' @export
semantica_structure_probe <- function(
    cosine_matrix, n_factors = NULL, extraction = c("ml", "minres", "principal"),
    rotation = c("promax", "oblimin", "varimax", "none")) {
  extraction <- match.arg(extraction)
  rotation <- match.arg(rotation)
  x <- as.matrix(cosine_matrix)
  if (!is.numeric(x) || nrow(x) != ncol(x) || nrow(x) < 3L ||
      any(!is.finite(x)) || !isSymmetric(unname(x), tol = 1e-8) ||
      any(abs(diag(x) - 1) > 1e-8) || any(abs(x) > 1 + 1e-8)) {
    stop("Provide a finite, symmetric item cosine matrix with unit diagonal.", call. = FALSE)
  }
  ids <- rownames(x)
  if (is.null(ids) && is.null(colnames(x))) ids <- as.character(seq_len(nrow(x)))
  if (is.null(ids) || (!is.null(colnames(x)) && !identical(ids, colnames(x))) ||
      anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Provide unique item IDs in the same order on both axes.", call. = FALSE)
  }
  dimnames(x) <- list(ids, ids)
  discovery <- is.null(n_factors)
  x <- transform_cosine_for_esem(x)
  if (discovery) {
    n_factors <- sum(eigen(x, symmetric = TRUE, only.values = TRUE)$values > 1 + 1e-8)
  } else {
    n_factors <- .semantica_assert_positive_integer(n_factors, "n_factors")
  }
  result <- list(
    status = "unsupported_count", count_method = if (discovery) "eigen_gt_one" else "fixed",
    n_factors = n_factors, matrix_repair = attr(x, "semantica_matrix_repair"),
    requested_extraction = extraction, requested_rotation = rotation
  )
  if (n_factors < 1L || n_factors >= nrow(x)) return(result)
  fit <- extract_pfa_loadings(x, n_factors, extraction, rotation, already_transformed = TRUE)
  result$status <- "fit_unavailable"
  if (is.null(fit)) return(result)
  components <- .semantica_unique_top_prototype(abs(fit$loadings), colnames(fit$loadings))
  residuals <- .semantica_pfa_residuals(x, fit$loadings, fit$phi)
  result$status <- if (fit$extraction != extraction ||
      (n_factors > 1L && fit$rotation != rotation)) "fallback" else if (
        length(fit$fit_warnings) || any(!is.finite(residuals$uniquenesses)) ||
        any(residuals$uniquenesses <= 0)
      ) "fit_warning" else "observed"
  c(result, list(extraction = fit$extraction, rotation = fit$rotation,
    fit_warnings = fit$fit_warnings, loadings = fit$loadings, phi = fit$phi,
    components = components, occupied_factors = length(unique(stats::na.omit(components)))),
    residuals)
}

.semantica_pfa_disposition <- function(pfa, diagnostics, residuals, extraction, rotation,
                                       min_loading, min_margin) {
  if (!identical(pfa$extraction, extraction) || !identical(pfa$rotation, rotation) ||
      rotation == "target_oblique" || length(pfa$fit_warnings) ||
      any(!is.finite(residuals$uniquenesses))) return("unavailable")
  clear <- !is.na(diagnostics$dominant_pfa_component) &
    diagnostics$primary_loading >= min_loading & diagnostics$loading_margin >= min_margin
  if (!isTRUE(all(clear)) || any(residuals$uniquenesses <= 0)) return("defer")
  "advance"
}

.semantica_structure_geometry_id <- function(matrix) {
  ids <- sort(rownames(matrix), method = "radix")
  .semantica_object_md5(list(ids, unname(round(matrix[ids, ids, drop = FALSE], 12L))))
}

.semantica_corroboration_status <- function(primary, secondary = NULL) {
  if (identical(primary, "defer") || identical(secondary, "defer")) return("defer")
  if (!identical(primary, "advance")) return("primary_unavailable")
  if (identical(secondary, "advance")) return("supported_by_both")
  "uncorroborated"
}

#' Bind an alternate embedding view to its item wording and model
#'
#' @param embeddings Numeric item-by-feature matrix with unique item IDs as row names.
#' @param item_texts Exact encoded wording, as a character vector named by item ID.
#' @param model Nonempty identifier for the encoder that produced the vectors.
#' @param revision Optional recorded revision of the encoder.
#' @return A `semantica_embedding_view` with a cosine matrix, wording, model,
#'   revision and artifact fingerprint for use in [semantica_corroborate_structure()].
#' @export
semantica_embedding_view <- function(embeddings, item_texts, model, revision = NULL) {
  ids <- names(item_texts)
  if (!is.character(item_texts) || !length(ids) || anyNA(item_texts) ||
      any(!nzchar(trimws(item_texts))))
    stop("Provide exact encoded wording named by item ID.", call. = FALSE)
  provenance <- if (is.null(revision)) list(model = model) else list(model = model, revision = revision)
  for (field in names(provenance)) {
    x <- provenance[[field]]
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop("Provide a nonempty ", field, " identifier.", call. = FALSE)
  }
  imported <- semantica_import_embeddings(embeddings, data.frame(item_id = ids), normalize = TRUE)
  value <- list(matrix = tcrossprod(imported$embeddings), item_texts = enc2utf8(item_texts),
    model = model, revision = revision)
  value$id <- .semantica_object_md5(value)
  structure(value, class = "semantica_embedding_view")
}

.semantica_check_embedding_view <- function(view, result, ids) {
  value <- unclass(view)
  if (!identical(value$id, .semantica_object_md5(value[names(value) != "id"])))
    stop("Embedding view changed after construction.", call. = FALSE)
  items <- semantica_items(result)
  active <- items$item_text[match(ids, items$item_id)]
  if (anyDuplicated(items$item_id) || anyNA(active) ||
      !identical(enc2utf8(active), unname(view$item_texts[ids])))
    stop("Embedding view wording or item IDs differ from the selected form.", call. = FALSE)
}

#' Apply a structural gate to recorded PFA diagnostics
#'
#' The experimental borderline rule allows one item up to .05 below the
#' configured primary loading threshold. The affected factor must retain
#' at least three indicators meeting the threshold, and every item must
#' meet the existing loading-margin rule.
#' Request `"one_borderline"` explicitly to use the exception. A favorable
#' disposition describes the fitted structure on its input matrix. Response
#' reliability and scale adequacy require their own evidence.
#'
#' @param pfa The final PFA diagnostics from a SEMANTICA run.
#' @param policy Use `"all_items"` for the recorded disposition or
#'   `"one_borderline"` to evaluate the experimental rule.
#' @param details Return threshold diagnostics for the evaluated rule.
#' @return The structural disposition by default. With `details = TRUE`,
#'   returns a list with the disposition, failed and unresolved item IDs, and
#'   whether the borderline exception changed the recorded decision.
#' @export
semantica_pfa_gate <- function(pfa, policy = c("all_items", "one_borderline"), details = FALSE) {
  policy <- match.arg(policy)
  details <- .semantica_assert_flag(details, "details")
  disposition <- .semantica_pfa_gate_disposition(pfa, policy)
  if (!details) return(disposition)
  d <- pfa$item_diagnostics
  current <- pfa$structure_disposition %||% "unavailable"
  ids <- if (is.data.frame(d) && "item" %in% names(d)) as.character(d$item) else
    if (is.data.frame(d)) as.character(seq_len(nrow(d))) else character()
  fails <- function(field, threshold) {
    value <- if (is.data.frame(d)) d[[field]] else NULL
    if (length(threshold) != 1L || !is.finite(threshold) || length(value) != length(ids))
      return(list(failed = character(), unresolved = ids))
    list(failed = ids[is.finite(value) & value < threshold], unresolved = ids[!is.finite(value)])
  }
  loading <- fails("primary_loading", pfa$min_loading)
  margin <- fails("loading_margin", pfa$min_margin)
  list(disposition = disposition, recorded_disposition = current, policy = policy,
    borderline_used = policy == "one_borderline" && current == "defer" && disposition == "advance",
    below_loading = loading$failed, below_margin = margin$failed,
    unresolved_items = union(loading$unresolved, margin$unresolved),
    item_thresholds_available = length(ids) > 0L &&
      !length(loading$unresolved) && !length(margin$unresolved))
}

.semantica_pfa_gate_disposition <- function(pfa, policy) {
  current <- pfa$structure_disposition %||% "unavailable"
  if (policy == "all_items" || !identical(current, "defer")) return(current)
  d <- pfa$item_diagnostics
  if (!isTRUE(pfa$available) || !isTRUE(pfa$min_uniqueness > 0) ||
      !isTRUE(is.finite(pfa$min_loading) && pfa$min_loading > 0) ||
      !isTRUE(is.finite(pfa$min_margin) && pfa$min_margin > 0) ||
      !is.data.frame(d) || !nrow(d) ||
      !all(c("factor", "primary_loading", "loading_margin", "dominant_pfa_component") %in% names(d)) ||
      !isTRUE(all(is.finite(d$primary_loading) & is.finite(d$loading_margin) &
        !is.na(d$factor) & !is.na(d$dominant_pfa_component) & d$loading_margin >= pfa$min_margin)))
    return(current)
  below <- which(d$primary_loading < pfa$min_loading)
  floor <- pfa$min_loading - .05 - sqrt(.Machine$double.eps)
  if (length(below) == 1L && d$primary_loading[below] >= floor &&
      sum(d$factor[-below] == d$factor[below]) >= 3L) "advance" else current
}

#' Challenge the selected form with separately sourced structural evidence
#'
#' Refits the saved exploratory PFA specification on the same selected item
#' IDs using a supplied alternate encoder cosine matrix or response
#' correlation matrix. A conflicting view defers. Agreement describes the two
#' supplied sources. Provenance is declared by the caller and does not establish
#' source independence, participant validity or a probability of success.
#'
#' @param result SEMANTICA pipeline or ACO result with saved final PFA.
#' @param matrix A [semantica_embedding_view()] or a named cosine or correlation
#'   matrix covering the selected items. An embedding view checks the selected
#'   wording and uses its recorded model and revision as provenance.
#' @param source Description identifying the separate source for a bare matrix.
#' @param source_family One of `"alternate_encoder"`, `"response_data"` or
#'   `"synthetic"`. Required for a bare matrix.
#' @param gate_policy Structural gate applied to both views. See [semantica_pfa_gate()].
#' @return A list with the primary and corroborating dispositions, overall
#'   status, declared provenance and optional companion PFA diagnostics.
#' @export
semantica_corroborate_structure <- function(result, matrix = NULL, source = NULL,
                                            source_family = NULL,
                                            gate_policy = c("all_items", "one_borderline")) {
  gate_policy <- match.arg(gate_policy)
  result <- result$advanced %||% result
  opt <- result$optimization %||% result
  primary <- opt$pfa_diagnostics
  disposition <- semantica_pfa_gate(primary, gate_policy)
  out <- list(status = .semantica_corroboration_status(disposition),
    primary = disposition,
    corroboration = "unavailable", reason = "no_separately_sourced_matrix")
  if (is.null(matrix)) return(out)
  ids <- opt$best_items
  assignment <- opt$factor_assignment[ids]
  if (!length(ids) || anyDuplicated(ids) || anyNA(assignment))
    stop("The result must contain a selected form with intended item assignments.", call. = FALSE)
  if (inherits(matrix, "semantica_embedding_view")) {
    if (!is.null(source) || !is.null(source_family))
      stop("The embedding view already supplies source and source_family.", call. = FALSE)
    .semantica_check_embedding_view(matrix, result, ids)
    out$source <- matrix$model
    out$source_family <- "alternate_encoder"
    out$view_id <- matrix$id
    out$revision <- matrix$revision
    models <- result$reproducibility$models
    primary_models <- c(models$resolved_embedding_model, models$embedding_model,
      result$embedding_diagnostics$model, result$generation$embedding_diagnostics$model,
      result$embedding_model)
    if (tolower(trimws(matrix$model)) %in% tolower(trimws(primary_models))) {
      out$reason <- "same_encoder"
      return(out)
    }
    matrix <- matrix$matrix
  } else {
    if (!is.character(source) || length(source) != 1L || is.na(source) ||
        !nzchar(trimws(source)) || is.null(source_family)) {
      stop("Supply the source and source_family of the corroborating matrix.", call. = FALSE)
    }
    out$source <- source
    out$source_family <- match.arg(source_family, c("alternate_encoder", "response_data", "synthetic"))
  }
  matrix <- as.matrix(matrix)
  if (!is.numeric(matrix) || anyDuplicated(rownames(matrix)) || anyDuplicated(colnames(matrix)) ||
      !all(ids %in% rownames(matrix)) || !all(ids %in% colnames(matrix))) {
    stop("The corroborating matrix must cover the same unique selected item IDs.", call. = FALSE)
  }
  matrix <- matrix[ids, ids, drop = FALSE]
  if (any(!is.finite(matrix)) || !isSymmetric(matrix, tol = 1e-8) ||
      any(abs(diag(matrix) - 1) > 1e-8) ||
      min(eigen(matrix, symmetric = TRUE, only.values = TRUE)$values) < -1e-8) {
    stop("Supply a finite positive-semidefinite correlation or cosine matrix.", call. = FALSE)
  }
  geometry <- primary$input_geometry_id
  saved <- result$generation$cosine_sim_matrix
  if (is.null(geometry) && all(ids %in% rownames(saved)) && all(ids %in% colnames(saved))) {
    geometry <- .semantica_structure_geometry_id(saved[ids, ids, drop = FALSE])
  }
  if (is.null(geometry) || identical(geometry, .semantica_structure_geometry_id(matrix))) {
    out$reason <- if (is.null(geometry)) "primary_geometry_unavailable" else "same_geometry"
    return(out)
  }
  if (is.null(primary$requested_extraction) || is.null(primary$requested_rotation)) {
    out$reason <- "primary_specification_unavailable"
    return(out)
  }
  secondary <- .semantica_preserve_caller_rng(
    compute_pfa_diagnostics(matrix, assignment, unique(assignment),
      extraction = primary$requested_extraction, rotation = primary$requested_rotation,
      min_loading = primary$min_loading, min_margin = primary$min_margin)
  )
  out$corroboration <- semantica_pfa_gate(secondary, gate_policy)
  out$status <- .semantica_corroboration_status(out$primary, out$corroboration)
  out$reason <- if (out$corroboration == "unavailable") "companion_fit_unavailable" else "companion_fit_observed"
  out$diagnostics <- secondary
  out
}
