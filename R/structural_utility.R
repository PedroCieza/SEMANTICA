#' Experimental continuous structural utilities for ACO
#'
#' Opt-in ESEM decision utilities for semantic item selection, configured through
#' [semantica_esem_config()] or [ACO_with_ESEM()].
#' @name continuous_structural_utility
#' @details
#' `continuous_structure` harmonically combines equal-factor means of assigned
#' absolute standardized loading strength and positive assigned-minus-cross-loading
#' margins, each bounded at one. For one factor it uses strength alone.
#' `continuous_reconstruction` uses off-diagonal squared-error improvement over
#' the identity correlation matrix, clipped to zero through one. Negative raw
#' improvements are retained; a zero-association input earns zero reconstruction
#' utility. `continuous_balanced` harmonically combines structure and reconstruction.
#' Unavailable required components are not replaced by neutral rewards.
#'
#' These utilities do not use loading bands or conventional fit, AVE, or HTMT
#' cutoffs. Search DFI is skipped; mathematical admissibility and configured
#' content constraints still apply. They describe embedding geometry, not
#' participant-based psychometric validity or a discovered factor count.
#'
#' For continuous modes, omitted quality options resolve to report-only guards
#' and scalar final selection. Explicit quality options take precedence. The
#' separate `report_only` action retains reference diagnostics while disabling
#' guard penalties, hard rejection, structural repair, and diagnostic-bonus
#' reranking. Legacy score modes keep their reference-based utility definitions.
#' Post-selection fit diagnostics remain separately configurable and may be expensive.
#'
#' See `vignette("continuous-aco", package = "SEMANTICA")` for formulas,
#' configuration, offline replay, and interpretation limits.
#' @seealso [semantica_esem_config()], [semantica_quality_config()],
#'   [semantica_run_custom()], [ACO_with_ESEM()]
NULL

# Experimental, sample-free decision utilities. These describe the configured
# semantic matrix and rotation, not participant-level psychometric validity.
.semantica_continuous_score_mode <- function(mode) {
  is.character(mode) && length(mode) == 1L && !is.na(mode) &&
    mode %in% c("continuous_structure", "continuous_reconstruction", "continuous_balanced")
}

.semantica_utility_fit_seed <- function(seed, items) {
  path <- tempfile(); on.exit(unlink(path), add = TRUE)
  # Serialization v3 includes the process native encoding. V2 plus explicit
  # UTF-8 and radix ordering gives master and workers the same seed bytes.
  saveRDS(list(stage = "continuous_archive", seed = as.integer(seed),
    items = sort(enc2utf8(items), method = "radix")), path, ascii = TRUE, compress = FALSE, version = 2L)
  1L + strtoi(substr(unname(tools::md5sum(path)), 1L, 7L), 16L)
}

.semantica_continuous_utility <- function(lambda, factor_assignment, factors,
                                         observed_cor = NULL, implied_cor = NULL,
                                         mode = "continuous_balanced") {
  stopifnot(.semantica_continuous_score_mode(mode), length(mode) == 1L)
  components <- c(strength = NA_real_, separation = NA_real_,
                  structure = NA_real_, reconstruction = NA_real_)
  out <- list(available = FALSE, score = 0, mode = mode, components = components,
    schema = "semantica-continuous-utility-v1", reason = "invalid_loading_metadata",
    aggregation = "equal_factor_means_then_harmonic_mean",
    reconstruction_r2 = NA_real_, residual_ss = NA_real_, baseline_ss = NA_real_)
  factors <- as.character(factors)
  ids <- rownames(lambda)
  if (!is.matrix(lambda) || !is.numeric(lambda) || any(!is.finite(lambda)) ||
      !length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) || is.null(names(factor_assignment)) ||
      anyDuplicated(names(factor_assignment)) || !length(factors) || anyNA(factors) ||
      anyDuplicated(factors) || any(!nzchar(factors)) ||
      ncol(lambda) != length(factors) || !setequal(colnames(lambda), factors) ||
      !all(ids %in% names(factor_assignment))) return(out)
  assignment <- as.character(factor_assignment[ids])
  if (anyNA(assignment) || !all(assignment %in% factors) || !all(factors %in% assignment)) return(out)
  lambda <- abs(lambda[, factors, drop = FALSE])
  primary <- lambda[cbind(seq_along(ids), match(assignment, factors))]
  macro_mean <- function(x) mean(vapply(factors, function(f) mean(x[assignment == f]), numeric(1L)))
  harmonic <- function(x) if (any(x <= 0)) 0 else length(x) * min(x) / sum(min(x) / x)
  out$components["strength"] <- macro_mean(pmin(1, primary))
  structure_values <- out$components["strength"]
  if (length(factors) > 1L) {
    cross <- lambda
    cross[cbind(seq_along(ids), match(assignment, factors))] <- 0
    out$components["separation"] <- macro_mean(pmin(1, pmax(0, primary - apply(cross, 1L, max))))
    structure_values <- c(structure_values, out$components["separation"])
  }
  out$components["structure"] <- harmonic(structure_values)
  if (mode != "continuous_structure") {
    valid_cor <- function(x) is.matrix(x) && is.numeric(x) &&
      nrow(x) == ncol(x) && !is.null(rownames(x)) &&
      !anyDuplicated(rownames(x)) && !anyDuplicated(colnames(x)) &&
      all(ids %in% rownames(x)) && all(ids %in% colnames(x)) && all(is.finite(x))
    if (!valid_cor(observed_cor) || !valid_cor(implied_cor) || length(ids) < 2L) {
      out$reason <- "reconstruction_matrix_unavailable"
      return(out)
    }
    observed <- observed_cor[ids, ids, drop = FALSE]
    implied <- implied_cor[ids, ids, drop = FALSE]
    if (max(abs(observed - t(observed))) > sqrt(.Machine$double.eps) ||
        max(abs(implied - t(implied))) > sqrt(.Machine$double.eps) ||
        max(abs(diag(observed) - 1)) > sqrt(.Machine$double.eps) ||
        max(abs(diag(implied) - 1)) > sqrt(.Machine$double.eps) ||
        max(abs(observed)) > 1 + sqrt(.Machine$double.eps) ||
        max(abs(implied)) > 1 + sqrt(.Machine$double.eps)) {
      out$reason <- "invalid_reconstruction_correlation"
      return(out)
    }
    off <- lower.tri(observed)
    out$residual_ss <- sum((observed[off] - implied[off])^2)
    out$baseline_ss <- sum(observed[off]^2)
    # Identity is the explicit zero-off-diagonal baseline. With no signal,
    # exact reconstruction earns no structural reward. Keep negative raw R2.
    out$reconstruction_r2 <- if (out$baseline_ss > 0) 1 - out$residual_ss / out$baseline_ss else NA_real_
    out$components["reconstruction"] <- if (out$baseline_ss > 0) max(0, min(1, out$reconstruction_r2)) else 0
  }
  out$score <- switch(mode,
    continuous_structure = out$components[["structure"]],
    continuous_reconstruction = out$components[["reconstruction"]],
    continuous_balanced = harmonic(out$components[c("structure", "reconstruction")]))
  out$available <- TRUE
  out$reason <- if (identical(out$baseline_ss, 0)) "no_offdiagonal_signal" else "computed"
  out
}
