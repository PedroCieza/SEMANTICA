# Content judgments remain observed inputs, separate from embedding diagnostics.
.semantica_review_metadata <- function(items) {
  if (!is.data.frame(items) || anyDuplicated(names(items)))
    stop("Content review requires an item table with unique column names.", call. = FALSE)
  pick <- function(fields, default = NA_character_) {
    field <- .semantica_pick_column(items, fields)
    if (is.null(field)) return(rep(default, nrow(items)))
    as.character(items[[field]])
  }
  x <- data.frame(item_id = pick(c("ID", "item_id", "id", "item_name", "Item", "item")),
    item_text = pick(c("item_text", "text", "wording", "statement", "item")),
    factor = pick(c("type", "Type", "factor", "Factor", "dimension", "Dimension", "scale", "Scale")),
    facet = pick(c("Facet", "facet", "subfacet", "domain", "Domain"), ""), stringsAsFactors = FALSE)
  x$facet[is.na(x$facet)] <- ""
  if (!nrow(x) || ncol(x) != 4L || anyNA(x) || anyDuplicated(x$item_id) ||
      any(!nzchar(trimws(x$item_id))) || any(!nzchar(trimws(x$item_text))) ||
      any(!nzchar(trimws(x$factor))) || identical(x$item_id, x$item_text))
    stop("Content review needs unique item IDs, actual wording and assigned factors; supply item_text explicitly.", call. = FALSE)
  x
}

#' Create a reusable item-content review sheet
#'
#' Create a sheet, review the wording, then pass it as `content_review` to
#' [semantica_run_custom()] or [ACO_with_ESEM()]. A callback receiving the current
#' item table is also accepted, including by [semantica_run()]. Reviewed conflicts
#' are excluded; ambiguity and missing review remain visible. No judgment is
#' inferred from embeddings, and content review does not validate responses.
#' @param items Item table with IDs, wording, factors and optional facets; or a
#'   result with retained pool metadata, obtained with [semantica_item_review()].
#' @param reviewer,rubric,source Review provenance, recycled from one string or
#'   supplied per item. Source is `"human"`, `"model"` or `"other"` for reviewed rows.
#' @return Editable data frame. Set status to `"supported"`, `"conflict"`,
#'   `"ambiguous"` or `"unreviewed"`; reviewed rows require provenance and rationale.
#'   Optional behavior/context/scoring_key fields document meaning; they do not
#'   alter scores. A nonempty scoring_key is `"1"` or `"-1"`.
#' @details Partial review is allowed. Records bind to exact IDs, wording, factors
#'   and facets; changed or unknown items are refused. Preserve the rubric version
#'   and relevant population/language in rubric. Declarations are not authenticated
#'   expert judgments. Do not supply hidden simulation outcomes as content evidence.
#' @examples
#' items <- data.frame(item_id = c("a", "b"), factor = "Persistence",
#'   item_text = c("I finish tedious tasks", "I discuss ways to finish tasks"))
#' sheet <- semantica_content_review(items, reviewer = "panel",
#'   rubric = "behavior-v1", source = "human")
#' sheet$status <- c("supported", "ambiguous")
#' sheet$rationale <- c("Direct task-completion behavior", "Topic may differ from behavior")
#' sheet
#' @export
semantica_content_review <- function(items, reviewer = "", rubric = "", source = "") {
  if (is.list(items) && !is.data.frame(items)) items <- semantica_item_review(items, "pool")
  x <- .semantica_review_metadata(items)
  for (value in list(reviewer, rubric, source)) if (!is.character(value) || anyNA(value) ||
      !length(value) || !length(value) %in% c(1L, nrow(x)))
    stop("Review provenance must be one string or one per item.", call. = FALSE)
  x$status <- "unreviewed"; x$source <- source; x$reviewer <- reviewer; x$rubric <- rubric
  x$rationale <- x$behavior <- x$context <- x$scoring_key <- ""
  x
}

.semantica_resolve_content_review <- function(review, items) {
  if (is.null(review)) return(NULL)
  current <- semantica_content_review(items)
  if (is.function(review)) review <- .semantica_preserve_caller_rng(review(current[, 1:4]))
  required <- c("item_id", "item_text", "factor", "facet", "status", "source", "reviewer", "rubric", "rationale")
  if (!is.data.frame(review) || anyDuplicated(names(review)) || !all(required %in% names(review)))
    stop("Use semantica_content_review() to create a content-review sheet.", call. = FALSE)
  review <- as.data.frame(lapply(review, as.character), stringsAsFactors = FALSE)
  for (field in setdiff(names(review), c("item_id", "item_text", "factor", "status")))
    review[[field]][is.na(review[[field]])] <- ""
  if (anyNA(review[required]) || anyDuplicated(review$item_id) ||
      any(!review$item_id %in% current$item_id) ||
      any(!review$status %in% c("unreviewed", "supported", "conflict", "ambiguous")))
    stop("Review IDs must be unique/current and statuses must be supported, conflict, ambiguous or unreviewed.", call. = FALSE)
  index <- match(review$item_id, current$item_id)
  if (!identical(unname(as.matrix(review[, names(current)[1:4]])), unname(as.matrix(current[index, 1:4]))))
    stop("Content-review wording, factor or facet changed; review the current items again.", call. = FALSE)
  judged <- review$status != "unreviewed"
  if (any(judged & (!review$source %in% c("human", "model", "other") |
      !nzchar(trimws(review$reviewer)) | !nzchar(trimws(review$rubric)) |
      !nzchar(trimws(review$rationale)))))
    stop("Reviewed rows require source (human/model/other), reviewer, rubric and rationale.", call. = FALSE)
  if ("scoring_key" %in% names(review) && anyNA(review$scoring_key) ||
      ("scoring_key" %in% names(review) && any(!review$scoring_key %in% c("", "1", "-1"))))
    stop("Reviewed scoring_key must be empty, 1 or -1.", call. = FALSE)
  fields <- intersect(names(current), names(review))
  current[index, fields] <- review[, fields, drop = FALSE]
  current
}

.semantica_content_witness <- function(review, eligible, counts, clusters, facets, by_factor,
    mode, minimum, imbalance) {
  if (is.null(review)) return(NULL)
  supported <- review$item_id[review$status == "supported"]
  pool <- lapply(eligible, intersect, y = supported)
  if (any(lengths(pool) < counts)) return(list(status = "insufficient_supported_items", items = character()))
  # Supported records tie; IDs provide only a reproducible feasibility order.
  fit <- .semantica_joint_pool_selection(pool, counts, clusters, facets, by_factor,
    mode, minimum, imbalance, priority = sort(supported, method = "radix"))
  list(status = if (fit$feasible) "retained_for_final_evaluation" else "supported_constraints_infeasible",
    items = fit$items, method = "deterministic_feasible_supported_content")
}

.semantica_content_review_result <- function(review, eligible, selected = character(), witness = NULL) {
  if (is.null(review)) return(NULL)
  review$admitted <- review$item_id %in% unlist(eligible, use.names = FALSE)
  review$selected <- review$item_id %in% selected
  review$admission_reason <- ifelse(review$status == "conflict", "reviewed_content_conflict",
    ifelse(review$admitted, "admitted", "native_constraint_or_prefilter"))
  chosen <- review$status[review$selected]
  list(status = if (!length(chosen)) "selection_not_evaluated" else
      if (all(chosen == "supported")) "selected_content_supported" else "content_review_required",
    records = review, fingerprint = .semantica_object_md5(review[, !names(review) %in%
      c("admitted", "selected", "admission_reason")]), witness = witness,
    interpretation = "Declared content judgments; not participant validation or a calibrated success probability.")
}
