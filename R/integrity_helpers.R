# Internal integrity helpers for explicit contracts and reproducibility metadata.

.semantica_condition <- function(message, subclass = NULL, fields = list(),
                                 warning = FALSE, call = NULL) {
  base_class <- if (isTRUE(warning)) "semantica_warning" else "semantica_error"
  classes <- unique(c(subclass, base_class))
  args <- c(list(message = as.character(message)[1L]), fields,
            list(class = classes, call = call))
  if (isTRUE(warning)) {
    do.call(base::warningCondition, args)
  } else {
    do.call(base::errorCondition, args)
  }
}

.semantica_abort <- function(message, subclass = NULL, ..., call = NULL) {
  cond <- .semantica_condition(
    message,
    subclass = subclass,
    fields = list(...),
    warning = FALSE,
    call = call
  )
  stop(cond)
}

.semantica_warn <- function(message, subclass = NULL, ..., call = NULL) {
  cond <- .semantica_condition(
    message,
    subclass = subclass,
    fields = list(...),
    warning = TRUE,
    call = call
  )
  warning(cond)
  invisible(cond)
}

.semantica_validation_error <- function(arg, expected, value = NULL,
                                        condition_class = "semantica_error_config") {
  .semantica_abort(
    sprintf("'%s' must be %s.", arg, expected),
    subclass = condition_class,
    argument = arg,
    expected = expected,
    supplied_type = typeof(value)
  )
}

.semantica_assert_flag <- function(x, arg,
                                   condition_class = "semantica_error_config") {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .semantica_validation_error(
      arg, "a single non-missing logical value (TRUE or FALSE)", x,
      condition_class = condition_class
    )
  }
  x
}

.semantica_assert_positive_integer <- function(x, arg,
                                               condition_class = "semantica_error_config") {
  if (!is.numeric(x) || is.logical(x) || length(x) != 1L ||
      is.na(x) || !is.finite(x) || x < 1 || x != floor(x) ||
      x > .Machine$integer.max) {
    .semantica_validation_error(
      arg, "a single finite whole-number value >= 1", x,
      condition_class = condition_class
    )
  }
  as.integer(x)
}

.semantica_assert_nonnegative_integer <- function(x, arg,
                                                  condition_class = "semantica_error_config") {
  if (!is.numeric(x) || is.logical(x) || length(x) != 1L ||
      is.na(x) || !is.finite(x) || x < 0 || x != floor(x) ||
      x > .Machine$integer.max) {
    .semantica_validation_error(
      arg, "a single finite whole-number value >= 0", x,
      condition_class = condition_class
    )
  }
  as.integer(x)
}

.semantica_assert_positive_integer_vector <- function(x, arg,
                                                      condition_class = "semantica_error_config") {
  if (!is.numeric(x) || is.logical(x) || length(x) < 1L || anyNA(x) ||
      any(!is.finite(x)) || any(x < 1) || any(x != floor(x)) ||
      any(x > .Machine$integer.max)) {
    .semantica_validation_error(
      arg, "one or more finite whole-number values >= 1", x,
      condition_class = condition_class
    )
  }
  out <- as.integer(x)
  names(out) <- names(x)
  out
}

.semantica_assert_finite_scalar <- function(x, arg,
                                            condition_class = "semantica_error_config") {
  if (!is.numeric(x) || is.logical(x) || length(x) != 1L ||
      is.na(x) || !is.finite(x)) {
    .semantica_validation_error(
      arg, "a single finite numeric value", x,
      condition_class = condition_class
    )
  }
  as.numeric(x)
}

.semantica_assert_positive_scalar <- function(x, arg,
                                              condition_class = "semantica_error_config") {
  out <- .semantica_assert_finite_scalar(x, arg, condition_class)
  if (out <= 0) {
    .semantica_validation_error(
      arg, "a single finite numeric value > 0", x,
      condition_class = condition_class
    )
  }
  out
}

.semantica_assert_nonnegative_scalar <- function(x, arg,
                                                 condition_class = "semantica_error_config") {
  out <- .semantica_assert_finite_scalar(x, arg, condition_class)
  if (out < 0) {
    .semantica_validation_error(
      arg, "a single finite numeric value >= 0", x,
      condition_class = condition_class
    )
  }
  out
}

.semantica_assert_probability <- function(x, arg, inclusive = c(TRUE, TRUE),
                                          condition_class = "semantica_error_config") {
  out <- .semantica_assert_finite_scalar(x, arg, condition_class)
  lower_ok <- if (isTRUE(inclusive[1L])) out >= 0 else out > 0
  upper_ok <- if (isTRUE(inclusive[2L])) out <= 1 else out < 1
  if (!lower_ok || !upper_ok) {
    bounds <- paste0(
      if (isTRUE(inclusive[1L])) "[" else "(",
      "0, 1",
      if (isTRUE(inclusive[2L])) "]" else ")"
    )
    .semantica_validation_error(
      arg, paste0("a single finite numeric value in ", bounds), x,
      condition_class = condition_class
    )
  }
  out
}

.semantica_assert_optional_flag <- function(x, arg,
                                            condition_class = "semantica_error_config") {
  if (is.null(x)) return(NULL)
  .semantica_assert_flag(x, arg, condition_class)
}

.semantica_assert_optional_positive_integer <- function(x, arg,
                                                        condition_class = "semantica_error_config") {
  if (is.null(x)) return(NULL)
  .semantica_assert_positive_integer(x, arg, condition_class)
}

.semantica_assert_optional_positive_integer_or_inf <- function(
    x, arg, condition_class = "semantica_error_config") {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && !is.logical(x) && length(x) == 1L &&
      !is.na(x) && is.infinite(x) && x > 0) {
    return(Inf)
  }
  .semantica_assert_positive_integer(x, arg, condition_class)
}

.semantica_assert_positive_integer_or_inf <- function(
    x, arg, condition_class = "semantica_error_config") {
  if (is.numeric(x) && !is.logical(x) && length(x) == 1L &&
      !is.na(x) && is.infinite(x) && x > 0) {
    return(Inf)
  }
  .semantica_assert_positive_integer(x, arg, condition_class)
}

.semantica_assert_optional_positive_scalar <- function(x, arg,
                                                       condition_class = "semantica_error_config") {
  if (is.null(x)) return(NULL)
  .semantica_assert_positive_scalar(x, arg, condition_class)
}

.semantica_assert_optional_probability <- function(x, arg,
                                                   condition_class = "semantica_error_config") {
  if (is.null(x)) return(NULL)
  .semantica_assert_probability(x, arg, condition_class = condition_class)
}

.semantica_canonicalize_config <- function(x, preserve_order = FALSE) {
  if (is.null(x) || is.atomic(x) || is.data.frame(x) || is.matrix(x)) return(x)
  if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") {
    return("<non-serializable config component omitted>")
  }
  if (!is.list(x)) return(x)
  nms <- names(x)
  ordered_subtrees <- c("factors", "construct_blueprint", "rotation_args")
  out <- lapply(seq_along(x), function(i) {
    nm <- if (is.null(nms)) "" else nms[[i]]
    .semantica_canonicalize_config(
      x[[i]], preserve_order = preserve_order || nm %in% ordered_subtrees
    )
  })
  if (!is.null(nms)) names(out) <- nms
  if (!preserve_order && !is.null(nms) && length(nms) > 1L &&
      all(!is.na(nms)) && all(nzchar(nms))) {
    out <- out[order(nms, method = "radix")]
  }
  out
}

.semantica_model_identity_status <- function(model, revision = NULL) {
  revision <- if (is.null(revision)) NA_character_ else as.character(revision[1L])
  model <- if (is.null(model)) NA_character_ else as.character(model[1L])
  if (!is.na(revision) && nzchar(trimws(revision))) return("revision_pinned")
  if (!is.na(model) && nzchar(trimws(model))) return("mutable_alias")
  "unknown"
}

.semantica_sanitize_config_provenance <- function(x, depth = 0L) {
  if (depth > 50L) return("<config depth omitted>")
  if (is.null(x) || is.atomic(x) || is.data.frame(x) || is.matrix(x)) return(x)
  if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") {
    return("<non-serializable config component omitted>")
  }
  if (!is.list(x)) return(x)
  nms <- names(x)
  out <- vector("list", 0L)
  out_names <- character(0L)
  for (i in seq_along(x)) {
    nm <- if (is.null(nms)) "" else nms[[i]]
    sensitive <- nzchar(nm) && (
      nm %in% c("api_key", "embed_api_key", "hf_token", "embed_hf_token", "token", "password", "secret") ||
        (exists(".semantica_sensitive_field_name", mode = "function") &&
           isTRUE(.semantica_sensitive_field_name(nm)))
    )
    if (sensitive) next
    value <- x[[i]]
    if (nzchar(nm) && grepl("url$|_url$|endpoint$", nm, ignore.case = TRUE) &&
        exists(".semantica_sanitize_url", mode = "function")) {
      value <- .semantica_sanitize_url(value)
    } else {
      value <- .semantica_sanitize_config_provenance(value, depth + 1L)
    }
    out[length(out) + 1L] <- list(value)
    out_names[[length(out_names) + 1L]] <- nm
  }
  if (!is.null(nms)) names(out) <- out_names
  out
}

.semantica_optional_diagnostic <- function(thunk, requested = TRUE, applicable = TRUE) {
  if (!isTRUE(requested)) {
    return(list(
      value = NULL,
      status = list(status = "not_requested", condition_class = NULL, message = NULL)
    ))
  }
  if (!isTRUE(applicable)) {
    return(list(
      value = NULL,
      status = list(status = "not_applicable", condition_class = NULL, message = NULL)
    ))
  }
  tryCatch(
    list(
      value = thunk(),
      status = list(status = "ok", condition_class = NULL, message = NULL)
    ),
    error = function(e) list(
      value = NULL,
      status = list(
        status = "failed",
        condition_class = class(e)[1L],
        message = conditionMessage(e)
      )
    )
  )
}
