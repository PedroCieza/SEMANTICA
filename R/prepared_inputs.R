# Native, provider-free preparation from recorded item and definition vectors.
.semantica_preparation_source_id <- function() {
  .semantica_object_md5(list(schema = "semantica-prepared-1",
    alignment = body(.semantica_definition_alignment), wrap = body(semantica_wrap),
    preparation = body(.semantica_prepared_generation)))
}

.semantica_prepared_items <- function(items, factors) {
  x <- as.data.frame(items, stringsAsFactors = FALSE)
  pick <- function(options) {
    field <- intersect(options, names(x))
    if (!length(field)) stop("Missing prepared item field: ", options[1L])
    as.character(x[[field[1L]]])
  }
  out <- data.frame(item_id = pick(c("item_id", "ID")),
    factor = pick(c("factor", "Dimension")),
    Facet = if (any(c("Facet", "facet") %in% names(x))) pick(c("Facet", "facet")) else "",
    item_text = pick(c("item_text", "text")), stringsAsFactors = FALSE)
  if (nrow(out) < 4L || anyNA(out) || any(!nzchar(out$item_id)) ||
      anyDuplicated(out$item_id) || any(!nzchar(trimws(out$item_text))) ||
      any(!out$factor %in% names(factors))) stop("Invalid prepared item identities, texts or factors.")
  for (f in names(factors)) {
    facets <- names(factors[[f]]$facets)
    if (length(facets) && any(!out$Facet[out$factor == f] %in% facets))
      stop("Prepared facet is outside the declared blueprint: ", f)
    if (!any(out$factor == f)) stop("Prepared pool omits factor: ", f)
  }
  # Deliberately discard every supplied diagnostic, rating, response and pass flag.
  out$ID <- out$item_id; out$Dimension <- out$factor; out$item <- out$item_text
  out
}

.semantica_validate_prepared <- function(x, factors = x$factors, compute = x$compute) {
  expected <- c("schema", "items", "embeddings", "reference_embeddings", "factors",
    "compute", "active_matrix", "preparation_diagnostics", "provenance", "source_id", "pool_id", "representation_id",
    "preparation_spec_id", "fingerprint")
  if (!inherits(x, "semantica_prepared_inputs") || !identical(names(x), expected) ||
      !identical(x$schema, "semantica-prepared-1")) stop("Invalid prepared-input schema.")
  value <- unclass(x); value$fingerprint <- NULL
  if (!identical(x$fingerprint, .semantica_object_md5(value))) stop("Prepared inputs changed after construction.")
  if (!identical(x$source_id, .semantica_preparation_source_id())) stop("Prepared-input sources changed; prepare a new version.")
  if (!identical(factors, x$factors)) stop("Prepared blueprint changed.")
  fields <- c("cosine_adjustment", "semantic_calibration")
  if (!identical(as.list(compute)[fields], as.list(x$compute)[fields])) stop("Prepared representation policy changed.")
  invisible(TRUE)
}

#' Prepare immutable selection inputs from recorded vectors
#'
#' Uses native definition alignment and cosine construction without provider calls.
#' Evaluation-only fields are discarded from item metadata. Definition vectors
#' must include exact reference texts as recorded by native alignment.
#' @param items Item metadata with item_id, factor, Facet and item_text.
#' @param embeddings Finite item-by-dimension matrix with item row names.
#' @param factors Named substantive blueprint.
#' @param reference_embeddings List with vectors and named texts, as retained by
#'   native definition alignment. No responses or external judgments belong here.
#' @param compute A semantica_compute_config object.
#' @param provenance Named character metadata: model, revision, content_policy,
#'   origin, or data_version. Never supply credentials or sessions.
#' @return A fingerprinted semantica_prepared_inputs object.
#' @details
#' Supply exact item IDs and the factor/facet/exclusion reference texts and
#' vectors retained by native definition alignment. Item and reference vectors
#' must share a compatible encoder and preprocessing policy; declared model
#' identity is provenance, not independently authenticated evidence.
#'
#' Pass the object as `prepared_inputs` to [semantica_run_custom()] with the
#' same blueprint and representation settings. Native diagnostics are recomputed;
#' generation, embedding requests and pool top-up are skipped. Insufficient
#' pools fail. Changed objects, blueprint, representation policy or preparation
#' source definitions require a newly prepared version. Fingerprints detect
#' mutation; they do not authenticate an untrusted serialized object.
#' @seealso [semantica_run_custom()], [semantica_import_embeddings()],
#'   [semantica_compute_config()]
#' @export
semantica_prepare_inputs <- function(items, embeddings, factors, reference_embeddings,
    compute = semantica_compute_config(retain_embeddings = TRUE), provenance = list()) {
  if (!is.list(factors) || is.null(names(factors)) || anyDuplicated(names(factors))) stop("A named blueprint is required.")
  items <- .semantica_prepared_items(items, factors)
  if (!is.matrix(embeddings) || !is.numeric(embeddings) || any(!is.finite(embeddings)) ||
      is.null(rownames(embeddings)) || anyDuplicated(rownames(embeddings)) ||
      !setequal(rownames(embeddings), items$item_id) || any(rowSums(embeddings^2) <= 0))
    stop("Prepared vectors must match every item ID exactly and be finite/nonzero.")
  embeddings <- embeddings[items$item_id, , drop = FALSE]
  if (!is.list(reference_embeddings) || !setequal(names(reference_embeddings), c("vectors", "texts")) ||
      length(reference_embeddings) != 2L || !is.character(reference_embeddings$texts))
    stop("Reference evidence permits only vectors and exact named texts.")
  allowed <- c("model", "revision", "content_policy", "origin", "data_version")
  if (!is.list(provenance) || anyDuplicated(names(provenance)) || any(!names(provenance) %in% allowed) ||
      (length(provenance) && is.null(names(provenance))) ||
      any(!vapply(provenance, function(z) is.character(z) && length(z) == 1L && !is.na(z), logical(1))))
    stop("Only declared non-secret character preparation provenance is permitted.")
  x <- list(schema = "semantica-prepared-1", items = items, embeddings = embeddings,
    reference_embeddings = reference_embeddings, factors = factors, compute = compute,
    active_matrix = NULL, preparation_diagnostics = NULL,
    provenance = provenance, source_id = .semantica_preparation_source_id(),
    pool_id = .semantica_object_md5(items),
    representation_id = .semantica_object_md5(list(embeddings, reference_embeddings, compute)),
    preparation_spec_id = .semantica_object_md5(list(factors, compute, provenance)))
  x$fingerprint <- .semantica_object_md5(x)
  class(x) <- c("semantica_prepared_inputs", "list")
  # Validate reference identities/texts now, not halfway through optimization.
  generated <- .semantica_prepared_generation(x, factors, compute)
  x$active_matrix <- generated$cosine_sim_matrix
  x$preparation_diagnostics <- generated[c("embedding_diagnostics", "cosine_diagnostics", "content_alignment")]
  value <- unclass(x); value$fingerprint <- NULL
  x$fingerprint <- .semantica_object_md5(value)
  x
}

.semantica_prepared_generation <- function(prepared, factors, compute,
    exclusions = NULL, content_alignment = TRUE) {
  .semantica_validate_prepared(prepared, factors, compute)
  items <- prepared$items
  alignment <- if (content_alignment) .semantica_definition_alignment(items,
    prepared$embeddings, factors, embed_session = NULL, exclusions = exclusions,
    reference_embeddings = prepared$reference_embeddings) else NULL
  if (content_alignment && !isTRUE(alignment$available)) stop("Prepared definition-alignment evidence unavailable.")
  if (!is.null(alignment$table)) {
    a <- alignment$table
    for (field in setdiff(names(a), "item_id")) items[[field]] <- a[[field]][match(items$item_id, a$item_id)]
  }
  emb <- list(embeddings = prepared$embeddings, items_tbl = items,
    embed_model = prepared$provenance$model %||% "recorded-vectors")
  args <- as.list(compute)
  args <- args[intersect(names(args), names(formals(semantica_wrap)))]
  wrapped <- do.call(semantica_wrap, c(list(embed_result = emb, items_tbl = items, verbose = FALSE), args))
  # semantica_wrap standardizes its table; mirror the live route's evidence join.
  for (field in grep("^semantica_", names(items), value = TRUE)) {
    if (!is.null(wrapped$df)) wrapped$df[[field]] <- items[[field]][match(wrapped$df$item, items$item_id)]
    if (!is.null(wrapped$item_metadata)) wrapped$item_metadata[[field]] <- items[[field]][match(wrapped$item_metadata$ID, items$item_id)]
  }
  wrapped$generated_item_metadata <- wrapped$item_metadata
  wrapped$items_tbl_raw <- items; wrapped$embed_result <- emb
  wrapped$content_alignment <- alignment
  wrapped$generation_provenance <- list(schema = "semantica-prepared-origin-1",
    origin = "immutable_prepared_inputs", item_pool_fingerprint = prepared$pool_id,
    representation_id = prepared$representation_id,
    preparation_spec_id = prepared$preparation_spec_id,
    generation_seed_controlled = NA, retained_total = nrow(items))
  wrapped$pool_topup <- list(enabled = FALSE, reason = "immutable_prepared_pool",
    effective_constraint_level = stats::setNames(rep("strict", length(factors)), names(factors)))
  wrapped
}
