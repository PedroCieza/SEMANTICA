# Experimental research utilities -----------------------------------------
#
# These functions provide optional methodological utilities exposed by the SEMANTICA
# method-development roadmap. They deliberately return explicit experimental status
# metadata. None of them turns sample-free semantic evidence into construct
# validity, measurement invariance, or other participant-based evidence.

.sem_norm_rows <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  nr <- sqrt(rowSums(x^2))
  if (any(!is.finite(nr) | nr <= .Machine$double.eps)) {
    stop("All embedding rows must be finite, non-zero vectors.")
  }
  x / nr
}

#' Contrast items with intended and neighboring construct prototypes
#'
#' This experimental diagnostic asks whether an item's embedding is closer to
#' its intended construct prototype than to competing construct prototypes. It
#' is a semantic discrimination diagnostic, not empirical discriminant validity.
#'
#' @param item_embeddings Numeric matrix with one item per row. Row names are
#'   strongly recommended and become item identifiers.
#' @param factor_assignment Intended factor for each item. A named vector is
#'   aligned by item identifier; otherwise its length must equal the number of
#'   item rows.
#' @param construct_embeddings Numeric matrix with one construct prototype per
#'   row; row names must identify the constructs.
#' @param exclusion_embeddings Optional numeric matrix of exclusion/adjacent
#'   construct prototypes in the same embedding space.
#' @return A list containing the item-by-construct cosine matrix and a table of
#'   intended similarity, strongest competitor, and contrastive margin.
#' @export
semantica_contrastive_construct_alignment <- function(
    item_embeddings, factor_assignment, construct_embeddings,
    exclusion_embeddings = NULL) {
  item_embeddings <- .sem_norm_rows(item_embeddings)
  construct_embeddings <- .sem_norm_rows(construct_embeddings)
  if (ncol(item_embeddings) != ncol(construct_embeddings)) {
    stop("Item and construct embeddings must use the same dimensionality.")
  }
  if (is.null(rownames(construct_embeddings)) || any(!nzchar(rownames(construct_embeddings)))) {
    stop("'construct_embeddings' must have unique construct row names.")
  }
  if (anyDuplicated(rownames(construct_embeddings))) stop("Construct prototype names must be unique.")
  ids <- rownames(item_embeddings) %||% paste0("item_", seq_len(nrow(item_embeddings)))
  rownames(item_embeddings) <- ids
  fa <- factor_assignment
  if (!is.null(names(fa))) {
    if (!all(ids %in% names(fa))) stop("Named 'factor_assignment' is missing one or more item IDs.")
    fa <- fa[ids]
  } else if (length(fa) != nrow(item_embeddings)) {
    stop("Unnamed 'factor_assignment' must have one value per item.")
  }
  fa <- as.character(fa)
  unknown <- setdiff(unique(fa), rownames(construct_embeddings))
  if (length(unknown)) stop("Missing construct prototype(s): ", paste(unknown, collapse = ", "))

  sims <- item_embeddings %*% t(construct_embeddings)
  rownames(sims) <- ids
  colnames(sims) <- rownames(construct_embeddings)
  intended <- vapply(seq_along(ids), function(i) sims[i, fa[[i]]], numeric(1L))
  competitor_name <- rep(NA_character_, length(ids))
  competitor_sim <- rep(NA_real_, length(ids))
  if (ncol(sims) > 1L) {
    for (i in seq_along(ids)) {
      others <- setdiff(colnames(sims), fa[[i]])
      j <- which.max(sims[i, others])
      competitor_name[[i]] <- others[[j]]
      competitor_sim[[i]] <- sims[i, others[[j]]]
    }
  }
  exclusion_max <- rep(NA_real_, length(ids))
  exclusion_name <- rep(NA_character_, length(ids))
  if (!is.null(exclusion_embeddings)) {
    ex <- .sem_norm_rows(exclusion_embeddings)
    if (ncol(ex) != ncol(item_embeddings)) stop("Exclusion embeddings must use the same dimensionality.")
    if (is.null(rownames(ex))) rownames(ex) <- paste0("exclusion_", seq_len(nrow(ex)))
    exs <- item_embeddings %*% t(ex)
    for (i in seq_along(ids)) {
      j <- which.max(exs[i, ])
      exclusion_max[[i]] <- exs[i, j]
      exclusion_name[[i]] <- colnames(exs)[[j]]
    }
  }
  tab <- data.frame(
    item_id = ids,
    intended_factor = fa,
    intended_similarity = intended,
    best_competing_factor = competitor_name,
    best_competing_similarity = competitor_sim,
    contrastive_margin = intended - competitor_sim,
    nearest_exclusion = exclusion_name,
    exclusion_similarity = exclusion_max,
    stringsAsFactors = FALSE
  )
  structure(list(
    item_diagnostics = tab,
    item_construct_similarity = sims,
    status = "experimental_semantic_prototype_diagnostic",
    interpretation = paste(
      "Positive contrastive margins mean the text representation is closer to",
      "its intended prototype than to the strongest supplied competitor.",
      "This is not empirical discriminant validity."
    )
  ), class = "semantica_construct_alignment")
}

#' Create a compact semantic fingerprint for an instrument
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Optional factor assignment for each item.
#' @param redundancy_threshold Similarity used to summarize high-overlap pairs.
#' @return A `semantica_scale_fingerprint` object containing descriptive,
#'   reproducible features of the supplied semantic representation.
#' @export
semantica_scale_fingerprint <- function(similarity_matrix, factor_assignment = NULL,
                                         redundancy_threshold = 0.85) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L) stop("Provide a square matrix with at least two items.")
  if (!isTRUE(all.equal(m, t(m), tolerance = 1e-8))) stop("Similarity matrix must be symmetric.")
  off <- m[lower.tri(m)]
  eig <- tryCatch(eigen(m, symmetric = TRUE, only.values = TRUE)$values,
                  error = function(e) rep(NA_real_, nrow(m)))
  features <- c(
    n_items = nrow(m),
    offdiag_mean = mean(off, na.rm = TRUE),
    offdiag_sd = stats::sd(off, na.rm = TRUE),
    offdiag_q10 = as.numeric(stats::quantile(off, .10, na.rm = TRUE, names = FALSE)),
    offdiag_median = stats::median(off, na.rm = TRUE),
    offdiag_q90 = as.numeric(stats::quantile(off, .90, na.rm = TRUE, names = FALSE)),
    redundancy_pair_rate = mean(off >= redundancy_threshold, na.rm = TRUE),
    first_eigen_share = if (all(is.finite(eig)) && sum(abs(eig)) > 0) max(eig) / sum(abs(eig)) else NA_real_
  )
  factor_features <- NULL
  if (!is.null(factor_assignment)) {
    ids <- rownames(m)
    fa <- factor_assignment
    if (!is.null(names(fa)) && !is.null(ids)) fa <- fa[ids]
    if (length(fa) != nrow(m) || anyNA(fa)) stop("'factor_assignment' must align to matrix rows.")
    fa <- as.character(fa)
    within <- between <- numeric(0L)
    for (i in seq_len(nrow(m) - 1L)) for (j in (i + 1L):nrow(m)) {
      if (fa[[i]] == fa[[j]]) within <- c(within, m[i, j]) else between <- c(between, m[i, j])
    }
    factor_features <- c(
      within_mean = if (length(within)) mean(within) else NA_real_,
      between_mean = if (length(between)) mean(between) else NA_real_,
      semantic_separation = if (length(within) && length(between)) mean(within) - mean(between) else NA_real_,
      n_factors = length(unique(fa))
    )
    features <- c(features, factor_features)
  }
  structure(list(
    features = features,
    redundancy_threshold = redundancy_threshold,
    status = "descriptive_semantic_fingerprint",
    note = "Fingerprints describe a semantic representation; they are not psychometric validity coefficients."
  ), class = "semantica_scale_fingerprint")
}

#' Compare two versions of a scale in semantic space
#'
#' @param similarity_a First named square similarity matrix.
#' @param similarity_b Second named square similarity matrix.
#' @param item_map Optional two-column data frame mapping IDs in A to IDs in B.
#'   If omitted, common row names are used.
#' @return Diagnostics describing preservation of pairwise semantic geometry.
#' @export
semantica_compare_scale_versions <- function(similarity_a, similarity_b, item_map = NULL) {
  a <- as.matrix(similarity_a); b <- as.matrix(similarity_b)
  if (is.null(item_map)) {
    common <- intersect(rownames(a), rownames(b))
    if (length(common) < 3L) stop("At least three common named items are required, or supply 'item_map'.")
    ia <- ib <- common
  } else {
    if (!is.data.frame(item_map) || ncol(item_map) < 2L) stop("'item_map' must have at least two columns.")
    ia <- as.character(item_map[[1L]]); ib <- as.character(item_map[[2L]])
    if (length(ia) < 3L || any(!ia %in% rownames(a)) || any(!ib %in% rownames(b))) {
      stop("'item_map' must map at least three valid item IDs.")
    }
  }
  aa <- a[ia, ia, drop = FALSE]
  bb <- b[ib, ib, drop = FALSE]
  va <- aa[lower.tri(aa)]; vb <- bb[lower.tri(bb)]
  structure(list(
    n_mapped_items = length(ia),
    pairwise_spearman = suppressWarnings(stats::cor(va, vb, method = "spearman", use = "complete.obs")),
    pairwise_pearson = suppressWarnings(stats::cor(va, vb, method = "pearson", use = "complete.obs")),
    mean_absolute_change = mean(abs(va - vb), na.rm = TRUE),
    max_absolute_change = max(abs(va - vb), na.rm = TRUE),
    mapping = data.frame(item_a = ia, item_b = ib, stringsAsFactors = FALSE),
    status = "semantic_version_comparison",
    note = "Semantic equivalence does not establish longitudinal or measurement invariance."
  ), class = "semantica_scale_version_comparison")
}

#' Screen paired cross-language item representations
#'
#' @param reference_embeddings Embeddings for source-language items.
#' @param translated_embeddings Embeddings for translated items in a shared
#'   multilingual embedding space, with corresponding row order or names.
#' @return Paired cosine and neighborhood-preservation diagnostics.
#' @export
semantica_cross_language_equivalence <- function(reference_embeddings,
                                                  translated_embeddings) {
  a <- .sem_norm_rows(reference_embeddings)
  b <- .sem_norm_rows(translated_embeddings)
  if (nrow(a) != nrow(b) || ncol(a) != ncol(b)) {
    stop("Reference and translated embeddings must have matching rows and dimensions.")
  }
  if (!is.null(rownames(a)) && !is.null(rownames(b)) && setequal(rownames(a), rownames(b))) {
    b <- b[rownames(a), , drop = FALSE]
  }
  paired <- rowSums(a * b)
  sa <- tcrossprod(a); sb <- tcrossprod(b)
  structure(list(
    paired_item_cosine = paired,
    mean_paired_cosine = mean(paired),
    minimum_paired_cosine = min(paired),
    neighborhood_spearman = if (nrow(a) >= 3L) suppressWarnings(stats::cor(sa[lower.tri(sa)], sb[lower.tri(sb)], method = "spearman")) else NA_real_,
    status = "experimental_cross_language_semantic_screen",
    note = paste(
      "This tests semantic representation consistency in a shared multilingual embedding space.",
      "It does not establish translation validity, DIF, or measurement invariance."
    )
  ), class = "semantica_cross_language_equivalence")
}

#' Build an expert-review queue from model disagreement and polarity flags
#'
#' @param matrices Named list of semantic similarity matrices for the same items.
#' @param polarity Optional output from [semantica_polarity_diagnostics()].
#' @param top_n Maximum number of pairs/items to return.
#' @return A prioritized review queue; high disagreement is intended for human
#'   adjudication rather than automatic rejection.
#' @export
semantica_expert_review_queue <- function(matrices, polarity = NULL, top_n = 20L) {
  if (!is.list(matrices) || length(matrices) < 2L) stop("Provide at least two representations.")
  mats <- lapply(matrices, as.matrix)
  common <- Reduce(intersect, lapply(mats, rownames))
  if (length(common) < 2L) stop("Matrices need common row names.")
  mats <- lapply(mats, function(m) m[common, common, drop = FALSE])
  pairs <- which(lower.tri(mats[[1L]]), arr.ind = TRUE)
  vals <- vapply(mats, function(m) m[pairs], numeric(nrow(pairs)))
  if (is.null(dim(vals))) vals <- matrix(vals, ncol = length(mats))
  disagreement <- apply(vals, 1L, stats::sd, na.rm = TRUE)
  q <- data.frame(
    review_type = "representation_disagreement",
    item_a = common[pairs[, 1L]],
    item_b = common[pairs[, 2L]],
    priority_score = disagreement,
    reason = "Semantic representations disagree on pair similarity; expert adjudication recommended.",
    stringsAsFactors = FALSE
  )
  if (!is.null(polarity) && is.data.frame(polarity) && "explicit_negation" %in% names(polarity)) {
    flagged <- polarity[isTRUE(polarity$explicit_negation) | polarity$explicit_negation %in% TRUE, , drop = FALSE]
    if (nrow(flagged)) {
      pq <- data.frame(
        review_type = "polarity_flag",
        item_a = as.character(flagged$item_text),
        item_b = NA_character_,
        priority_score = as.numeric(flagged$confidence %||% rep(.7, nrow(flagged))),
        reason = "Potential negation/reverse wording requires directional human review.",
        stringsAsFactors = FALSE
      )
      q <- rbind(q, pq)
    }
  }
  q <- q[order(q$priority_score, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  head(q, max(1L, as.integer(top_n)))
}

#' Generate prompts for construct regions that lack item coverage
#'
#' @param coverage Output from [semantica_assess_construct_coverage()].
#' @param blueprint A construct blueprint.
#' @param n_items_per_missing Number of candidate items requested per missing
#'   facet. This function creates prompts only; it does not claim generated items
#'   are valid or call an LLM automatically.
#' @return Character vector of expert/LLM drafting prompts.
#' @export
semantica_suggest_coverage_prompts <- function(coverage, blueprint,
                                                n_items_per_missing = 3L) {
  n_items_per_missing <- as.integer(n_items_per_missing)
  if (!is.finite(n_items_per_missing) || n_items_per_missing < 1L) stop("'n_items_per_missing' must be positive.")
  missing <- coverage$missing_facets %||% list()
  prompts <- character(0L)
  for (f in names(missing)) {
    for (facet in missing[[f]]) {
      fdef <- blueprint$factors[[f]]$definition %||% f
      prompts <- c(prompts, sprintf(
        "Draft %d candidate item(s) for the missing facet '%s' of construct '%s' (%s). Ensure the new items add content coverage rather than paraphrasing existing items; submit them to expert and empirical review before use.",
        n_items_per_missing, facet, f, fdef
      ))
    }
  }
  structure(prompts,
            status = "item_drafting_prompts_only",
            note = "Generated or drafted items require expert content review and participant-based validation.")
}

.semantica_build_preregistration_manifest <- function(safe_config, config_hash,
                                                       inputs_hash, package_version,
                                                       r_version, created_utc) {
  list(
    schema = "semantica-preregistration-manifest-1",
    package_version = package_version,
    r_version = r_version,
    created_utc = created_utc,
    config_hash = config_hash,
    inputs_hash = inputs_hash,
    config = safe_config,
    interpretation_boundary = paste(
      "This manifest freezes a semantic screening analysis plan.",
      "It does not preregister or establish participant-based construct validity unless such analyses are separately specified."
    )
  )
}

#' Create a preregistration-ready SEMANTICA analysis manifest
#'
#' @param config Configuration object/list to freeze.
#' @param inputs Optional item table or other analysis inputs whose serialized
#'   hash should be recorded.
#' @param path Optional JSON path. If supplied, the manifest is written there.
#' @section Side effects:
#' Reads the current package/R version and current UTC time. If `path` is supplied,
#' writes the manifest to the filesystem. No RNG state is consumed.
#'
#' @section Reproducibility:
#' Configuration and optional inputs are sanitized before hashing. The timestamp
#' and runtime-version effects are gathered by the wrapper and passed explicitly
#' into an internal deterministic manifest builder.
#'
#' @return Manifest list with package/R versions and content hashes.
#' @export
semantica_preregistration_manifest <- function(config, inputs = NULL, path = NULL) {
  safe_config <- sanitize_result_for_serialization(config)
  safe_inputs <- if (is.null(inputs)) NULL else sanitize_result_for_serialization(inputs)
  manifest <- .semantica_build_preregistration_manifest(
    safe_config = safe_config,
    config_hash = .semantica_object_md5(safe_config),
    inputs_hash = if (is.null(safe_inputs)) NA_character_ else .semantica_object_md5(safe_inputs),
    package_version = tryCatch(
      as.character(utils::packageVersion("SEMANTICA")),
      error = function(e) "development"
    ),
    r_version = R.version.string,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  if (!is.null(path)) {
    dir.create(dirname(normalizePath(path, mustWork = FALSE)), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  manifest
}
