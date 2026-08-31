# Versioned result bundles for reproducible SEMANTICA analyses.


.semantica_bundle_stable_value <- function(x, depth = 0L) {
  if (depth > 100L) return("<bundle depth omitted>")
  if (is.null(x)) return(x)
  if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") {
    return(paste0("<runtime-only:", typeof(x), ">"))
  }
  if (isS4(x)) {
    sn <- methods::slotNames(x)
    slots <- lapply(sn, function(nm)
      .semantica_bundle_stable_value(methods::slot(x, nm), depth + 1L))
    names(slots) <- sn
    return(list(`__class__` = class(x), `__slots__` = slots))
  }
  if (is.atomic(x) || is.data.frame(x)) return(x)
  if (is.language(x) || is.pairlist(x)) {
    return(paste(deparse(x, width.cutoff = 500L), collapse = "\n"))
  }
  if (!is.list(x)) return(list(`__class__` = class(x), `__type__` = typeof(x)))
  out <- lapply(x, .semantica_bundle_stable_value, depth = depth + 1L)
  names(out) <- names(x)
  if (!identical(class(x), "list")) out <- list(`__class__` = class(x), `__data__` = out)
  out
}

.semantica_bundle_canonical_object <- function(x) {
  if (is.list(x)) x$bundle_manifest <- NULL
  x <- .semantica_bundle_stable_value(x)
  if (exists(".semantica_canonicalize_config", mode = "function")) {
    x <- .semantica_canonicalize_config(x)
  }
  x
}

.semantica_bundle_canonical_md5 <- function(x) {
  .semantica_object_md5(.semantica_bundle_canonical_object(x))
}

#' Save a complete, sanitized SEMANTICA analysis bundle
#'
#' Unlike `semantica_export()`, which creates convenient CSV files, this function
#' retains the analysis provenance needed to reproduce an analysis: models,
#' configurations, seeds, matrices, diagnostics, and (optionally) embeddings.
#' Live credentials are removed before serialization. Schema-4 bundles verify a
#' stable canonical checksum that ignores runtime-only representation state while
#' preserving analysis content; the exact serialized-object MD5 is retained as
#' provenance. These checksums detect accidental corruption, not authenticity.
#'
#' @param result A SEMANTICA result object.
#' @param path Destination `.rds` path.
#' @param include_embeddings Retain dense embeddings when present.
#' @param write_manifest Write a human-readable JSON sidecar manifest.
#' @param compress RDS compression method.
#' @section Side effects:
#' Writes an RDS bundle and, when requested, a JSON (or fallback R) manifest
#' sidecar. No RNG state is consumed.
#'
#' @section Reproducibility:
#' Sessions/results are sanitized before serialization. Schema-4 verification
#' uses the stable canonical checksum; schema-3 and earlier bundles retain the
#' legacy exact-MD5/canonical-fallback behavior.
#'
#' @return Invisibly returns the saved path.
#' @export
semantica_save_bundle <- function(result, path = "SEMANTICA_bundle.rds",
                                  include_embeddings = TRUE,
                                  write_manifest = TRUE,
                                  compress = "xz") {
  if (!is.list(result)) stop("'result' must be a SEMANTICA result list.")
  # semantica_run() presents a compact facade, but bundles intentionally store
  # the canonical analytical result so no retained evidence is duplicated or
  # omitted by the presentation layer.
  result_to_save <- if (exists(".semantica_raw_result", mode = "function")) {
    .semantica_raw_result(result)
  } else result
  safe <- if (exists("sanitize_result_for_serialization", mode = "function")) {
    sanitize_result_for_serialization(result_to_save)
  } else result_to_save
  if (!isTRUE(include_embeddings)) {
    if (!is.null(safe$embed_result$embeddings)) safe$embed_result$embeddings <- NULL
    if (!is.null(safe$generation$embed_result$embeddings)) safe$generation$embed_result$embeddings <- NULL
  }
  manifest <- list(
    format = "SEMANTICA analysis bundle",
    bundle_schema_version = 4L,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = tryCatch(as.character(utils::packageVersion("SEMANTICA")), error = function(e) NA_character_),
    object_class = class(safe),
    include_embeddings = isTRUE(include_embeddings),
    checksum_algorithm = "md5",
    checksum_purpose = "accidental_corruption_detection",
    canonical_checksum_purpose = "stable_analysis_projection_accidental_corruption_detection",
    primary_checksum = "object_canonical_md5",
    required_components = intersect(c("generation", "optimization", "reproducibility"), names(safe)),
    participant_validation_status = if (isTRUE(!is.null(safe$response_validation) || !is.null(safe$optimization$response_validation))) "participant_data_present" else "semantic_proxy_only_or_not_recorded",
    evidence_notice = paste(
      "Sample-free semantic PFA/ESEM/DFI outputs are proxy diagnostics.",
      "They do not establish construct validity without independent participant-based evidence."
    )
  )
  manifest$object_md5 <- .semantica_object_md5(safe[names(safe) != "bundle_manifest"])
  manifest$object_canonical_md5 <- .semantica_bundle_canonical_md5(safe)
  safe$bundle_manifest <- manifest
  dir.create(dirname(normalizePath(path, mustWork = FALSE)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(safe, path, version = 3L, compress = compress)
  if (isTRUE(write_manifest)) {
    sidecar <- sub("\\.rds$", "", path, ignore.case = TRUE)
    sidecar <- paste0(sidecar, ".manifest.json")
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(manifest, sidecar, pretty = TRUE, auto_unbox = TRUE, null = "null")
    } else {
      dput(manifest, file = paste0(sidecar, ".R"))
      warning("Package 'jsonlite' unavailable; wrote an R manifest instead of JSON.", call. = FALSE)
    }
  }
  invisible(path)
}

#' Load and verify a SEMANTICA analysis bundle
#'
#' @param path Saved bundle path.
#' @param verify Verify the stored bundle consistency checksum when present.
#' @section Side effects:
#' Reads the requested RDS bundle from the filesystem. Verification computes a
#' local consistency checksum and performs no network I/O.
#'
#' @section Reproducibility:
#' Verification detects accidental analysis-content drift relative to the
#' stored manifest; it is not a signature/authenticity check. Runtime-only state
#' inside fitted S4 objects is excluded from schema-4 checksum construction.
#' Legacy bundles remain supported.
#'
#' @return Restored SEMANTICA result.
#' @export
semantica_load_bundle <- function(path, verify = TRUE) {
  x <- readRDS(path)
  if (!is.list(x)) stop("The bundle does not contain a SEMANTICA result list.")
  manifest <- x$bundle_manifest
  if (isTRUE(verify) && !is.null(manifest$required_components) && length(manifest$required_components)) {
    missing_components <- setdiff(as.character(manifest$required_components), names(x))
    if (length(missing_components)) {
      .semantica_abort(
        sprintf("SEMANTICA bundle integrity check failed: missing required component(s): %s.", paste(missing_components, collapse = ", ")),
        subclass = "semantica_error_integrity", stage = "bundle_load", invariant = "required_components"
      )
    }
  }
  schema <- suppressWarnings(as.integer(manifest$bundle_schema_version %||% 0L))
  if (isTRUE(verify) && schema >= 4L && !is.null(manifest$object_canonical_md5)) {
    current <- .semantica_bundle_canonical_md5(x)
    if (!identical(unname(current), unname(manifest$object_canonical_md5))) {
      .semantica_abort(
        "SEMANTICA bundle integrity check failed: stable content checksum does not match manifest.",
        subclass = "semantica_error_integrity", stage = "bundle_load",
        invariant = "object_canonical_md5"
      )
    }
  } else if (isTRUE(verify) && !is.null(manifest$object_md5)) {
    current <- .semantica_object_md5(x[names(x) != "bundle_manifest"])
    if (!identical(unname(current), unname(manifest$object_md5))) {
      canonical_ok <- !is.null(manifest$object_canonical_md5) &&
        identical(unname(.semantica_bundle_canonical_md5(x)), unname(manifest$object_canonical_md5))
      if (isTRUE(canonical_ok)) {
        warning(
          "SEMANTICA bundle exact MD5 differs, but the canonical content checksum matches; treating this as a benign representation-order difference.",
          call. = FALSE
        )
      } else {
        .semantica_abort(
          "SEMANTICA bundle integrity check failed: object MD5 does not match manifest.",
          subclass = "semantica_error_integrity", stage = "bundle_load",
          invariant = "object_md5"
        )
      }
    }
  }
  if (exists(".semantica_wrap_run_result", mode = "function")) {
    run_cfg <- if (is.list(x$run_config)) x$run_config else list()
    rep_cfg <- if (is.list(x$reproducibility$run_interface)) x$reproducibility$run_interface else list()
    if (identical(run_cfg$interface %||% rep_cfg$interface %||% NA_character_, "semantica_run")) {
      return(.semantica_wrap_run_result(x))
    }
  }
  x
}
