#' Build an experimental anchor-profile embedding representation
#'
#' For each candidate item, compute its cosine similarities to a common,
#' ordered set of distinct anchor items. Center and normalize each profile,
#' so candidate-item cosine similarity is Pearson correlation between their
#' anchor-similarity profiles. All inputs come from one encoder result. The
#' representation shares that encoder's provenance and can be compared with
#' raw cosine under a fixed analysis budget.
#' Apply it explicitly to opt in. Cleaner grouping in this representation
#' does not establish response reliability or independent corroboration.
#'
#' @param embed_result A result of [semantica_embed()] or
#'   [semantica_import_embeddings()] containing candidates and anchors encoded
#'   together with a recorded model and revision.
#' @param anchor_ids Ordered IDs of the anchor items. Supply at least three
#'   unique anchors with wording distinct from the candidate pool.
#' @param id_col,text_col Columns containing item IDs and exact encoded wording.
#' @return A `semantica_embedding_result` for candidate items only, with
#'   normalized profiles and anchor provenance. Pass it to [semantica_wrap()]
#'   to supply geometry to [ACO_with_ESEM()].
#' @export
semantica_anchor_profiles <- function(embed_result, anchor_ids,
    id_col = "item_id", text_col = "item_text") {
  if (!is.list(embed_result))
    stop("Supply a SEMANTICA embedding result containing candidates and anchors.", call. = FALSE)
  encoder <- embed_result$representation_provenance %||%
    embed_result$embedding_diagnostics$representation_provenance %||% list()
  if (!is.null(encoder$anchor_profile))
    stop("Anchor profiles require original encoder vectors.", call. = FALSE)
  model <- encoder$model %||% embed_result$embed_model
  revision <- encoder$model_revision %||% encoder$model_version %||%
    embed_result$embedding_diagnostics$model_revision
  known <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
    nzchar(trimws(x)) && tolower(trimws(x)) != "unknown"
  if (!known(model) || !known(revision))
    stop("Anchor profiles require a recorded encoder model and revision.", call. = FALSE)

  imported <- semantica_import_embeddings(embed_result$embeddings, embed_result$items_tbl,
    id_col = id_col, normalize = TRUE)
  ids <- rownames(imported$embeddings)
  wording <- imported$items_tbl[[text_col]]
  if (!is.character(wording) || anyNA(wording) || any(!nzchar(trimws(wording))))
    stop("Supply the exact encoded wording for candidates and anchors.", call. = FALSE)
  wording <- stats::setNames(enc2utf8(wording), ids)
  if (!is.character(anchor_ids) || length(anchor_ids) < 3L || anyNA(anchor_ids) ||
      anyDuplicated(anchor_ids) || !all(anchor_ids %in% ids))
    stop("Supply at least three unique anchor IDs from the embedding result.", call. = FALSE)
  anchor_ids <- unname(anchor_ids)
  candidates <- !ids %in% anchor_ids
  if (!any(candidates)) stop("At least one candidate item must remain.", call. = FALSE)
  anchor_wording <- wording[anchor_ids]
  if (anyDuplicated(trimws(anchor_wording)) ||
      any(trimws(anchor_wording) %in% trimws(wording[candidates])))
    stop("Anchors must have distinct wording from one another and the candidates.", call. = FALSE)
  anchors <- imported$embeddings[anchor_ids, , drop = FALSE]
  profiles <- tcrossprod(imported$embeddings[candidates, , drop = FALSE], anchors)
  profiles <- profiles - rowMeans(profiles)
  if (any(sqrt(rowSums(profiles^2)) <= sqrt(.Machine$double.eps)))
    stop("At least one candidate has a constant or numerically flat anchor profile.", call. = FALSE)

  anchor_profile <- list(method = "centered_cosine_profiles", version = 1L,
    anchor_ids = anchor_ids, anchor_texts = anchor_wording,
    anchor_geometry_id = .semantica_object_md5(unname(anchors), version = 2L),
    base_dimension = ncol(anchors))
  semantica_import_embeddings(profiles, imported$items_tbl[candidates, , drop = FALSE],
    id_col = id_col, normalize = TRUE, provider = encoder$provider,
    model = model, model_version = revision, task = encoder$task %||% encoder$analysis_intent,
    instruction = encoder$instruction, source = encoder$source,
    provenance = list(representation = "anchor_profiles", anchor_profile = anchor_profile,
      encoder_provenance = encoder))
}
