# Incremental evaluation broker helpers.
#
# These functions intentionally sit behind the existing ACO compatibility
# wrappers.  They centralize canonical candidate keys, cache-aware batch
# planning, budget reservations, and search-time ESEM accounting without
# changing the public scoring method.

.semantica_cache_has_esem_outcome <- function(entry) {
  if (is.null(entry)) return(FALSE)
  isTRUE(entry$esem_evaluated) ||
    !is.null(entry$fit_result) ||
    !is.null(entry$esem_score)
}

.semantica_with_task_seed <- function(seed, expr) {
  seed <- suppressWarnings(as.integer(seed[1L]))
  if (length(seed) != 1L || !is.finite(seed) || seed < 1L) {
    stop("Task seed must be a positive integer.")
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

.semantica_new_evaluation_broker <- function(max_esem_fits = Inf) {
  max_esem_fits <- suppressWarnings(as.numeric(max_esem_fits[1L]))
  if (!is.finite(max_esem_fits)) max_esem_fits <- Inf
  if (max_esem_fits < 1L) stop("'max_esem_fits' must be positive or infinite.")

  broker <- new.env(parent = emptyenv())
  broker$max_esem_fits <- max_esem_fits
  broker$seen_keys <- new.env(parent = emptyenv(), hash = TRUE)
  broker$esem_requests <- 0L
  broker$esem_cache_hits <- 0L
  broker$esem_coalesced_requests <- 0L
  broker$esem_unique_candidates <- 0L
  broker$esem_fits_started <- 0L
  broker$esem_fits_converged <- 0L
  broker$esem_fits_admissible <- 0L
  broker$esem_fits_failed <- 0L
  broker$esem_solver_attempts_observed <- 0L
  broker$dfi_fits_started <- 0L
  broker$archive_esem_fits_started <- 0L
  broker$final_esem_fits_started <- 0L
  broker$esem_events <- list()
  class(broker) <- c("semantica_evaluation_broker", "environment")
  broker
}

.semantica_plan_esem_batch <- function(broker, candidate_indices, vectors,
                                       cache, key_fun = make_solution_key) {
  if (!inherits(broker, "semantica_evaluation_broker")) {
    stop("'broker' must be a SEMANTICA evaluation broker.")
  }
  candidate_indices <- as.integer(candidate_indices)
  if (length(candidate_indices) == 0L) {
    return(list(
      indices = integer(0L), keys = character(0L), cached = logical(0L),
      jobs_started = logical(0L), deferred = integer(0L),
      request_indices = integer(0L), request_keys = character(0L),
      request_to_evaluation = integer(0L),
      budget_exhausted = broker$esem_fits_started >= broker$max_esem_fits
    ))
  }

  keys_all <- vapply(vectors[candidate_indices], key_fun, character(1L))
  broker$esem_requests <- broker$esem_requests + length(candidate_indices)

  first <- !duplicated(keys_all)
  broker$esem_coalesced_requests <- broker$esem_coalesced_requests + sum(!first)
  representative_indices <- candidate_indices[first]
  representative_keys <- keys_all[first]
  representative_for_request <- match(keys_all, representative_keys)

  for (key in unique(keys_all)) {
    if (!exists(key, envir = broker$seen_keys, inherits = FALSE)) {
      assign(key, TRUE, envir = broker$seen_keys)
      broker$esem_unique_candidates <- broker$esem_unique_candidates + 1L
    }
  }

  cached <- vapply(representative_keys, function(key) {
    .semantica_cache_has_esem_outcome(cache_get(cache, key))
  }, logical(1L))
  broker$esem_cache_hits <- broker$esem_cache_hits + sum(cached)

  remaining <- if (is.infinite(broker$max_esem_fits)) {
    sum(!cached)
  } else {
    max(0L, as.integer(broker$max_esem_fits - broker$esem_fits_started))
  }
  miss_positions <- which(!cached)
  admitted_miss_positions <- utils::head(miss_positions, remaining)
  keep_positions <- sort(c(which(cached), admitted_miss_positions))
  deferred_positions <- setdiff(seq_along(representative_indices), keep_positions)
  jobs_started <- seq_along(representative_indices) %in% admitted_miss_positions
  broker$esem_fits_started <- broker$esem_fits_started + sum(jobs_started)
  request_admitted <- representative_for_request %in% keep_positions

  list(
    indices = representative_indices[keep_positions],
    keys = representative_keys[keep_positions],
    cached = cached[keep_positions],
    jobs_started = jobs_started[keep_positions],
    request_indices = candidate_indices[request_admitted],
    request_keys = keys_all[request_admitted],
    request_to_evaluation = match(
      representative_for_request[request_admitted], keep_positions
    ),
    deferred = candidate_indices[!request_admitted],
    budget_exhausted = length(deferred_positions) > 0L ||
      broker$esem_fits_started >= broker$max_esem_fits
  )
}


.semantica_esem_checkpoint_telemetry <- function(batch_plan, admissible_requests) {
  if (!is.list(batch_plan)) stop("'batch_plan' must be a list.", call. = FALSE)
  admissible_requests <- as.logical(admissible_requests)
  requests <- length(batch_plan$request_indices %||% integer(0L))
  unique_candidates <- length(batch_plan$keys %||% character(0L))
  list(
    requests = as.integer(requests),
    unique_candidates = as.integer(unique_candidates),
    cache_hits = as.integer(sum(batch_plan$cached %||% logical(0L))),
    coalesced_requests = as.integer(max(0L, requests - unique_candidates)),
    new_fits = as.integer(sum(batch_plan$jobs_started %||% logical(0L))),
    admissible_requests = as.integer(sum(admissible_requests, na.rm = TRUE))
  )
}


.semantica_append_esem_event <- function(broker, stage, candidate_key = NA_character_,
                                         cache_hit = FALSE, coalesced_requests = 0L,
                                         elapsed_seconds = NA_real_, fit_result = NULL,
                                         error = NA_character_, fallback_used = NA) {
  if (!inherits(broker, "semantica_evaluation_broker")) return(invisible(broker))
  fit_result <- fit_result %||% list()
  admissibility <- fit_result$admissibility %||% list()
  reason <- error
  if (is.null(reason) || length(reason) == 0L || is.na(reason) || !nzchar(reason)) {
    reasons <- admissibility$reasons %||% fit_result$rejection_reasons %||% character()
    reason <- if (length(reasons)) paste(unique(as.character(reasons)), collapse = "; ") else NA_character_
  }
  method <- fit_result$accepted_method %||% admissibility$accepted_method %||%
    fit_result$fit_method %||% fit_result$method %||% NA_character_
  event <- data.frame(
    stage = as.character(stage),
    candidate_key = as.character(candidate_key %||% NA_character_),
    cache_hit = isTRUE(cache_hit),
    coalesced_requests = as.integer(coalesced_requests %||% 0L),
    elapsed_seconds = as.numeric(elapsed_seconds %||% NA_real_),
    solver_method = as.character(method %||% NA_character_),
    converged = isTRUE(fit_result$converged),
    admissible = isTRUE(fit_result$converged) && isTRUE(fit_result$admissible),
    reason = as.character(reason %||% NA_character_),
    fallback_used = if (length(fallback_used) && !is.na(fallback_used)) isTRUE(fallback_used) else NA,
    stringsAsFactors = FALSE
  )
  broker$esem_events[[length(broker$esem_events) + 1L]] <- event
  invisible(broker)
}

.semantica_record_esem_payloads <- function(broker, payloads, jobs_started,
                                               keys = NULL, cached = NULL,
                                               coalesced_requests = 0L,
                                               stage = "search") {
  if (!inherits(broker, "semantica_evaluation_broker")) {
    stop("'broker' must be a SEMANTICA evaluation broker.")
  }
  if (length(payloads) != length(jobs_started)) {
    stop("Payload and job-start flags must have equal length.")
  }
  jobs_started <- as.logical(jobs_started)
  if (is.null(keys)) keys <- vapply(payloads, function(x) x$key %||% NA_character_, character(1L))
  if (is.null(cached)) cached <- !jobs_started
  for (i in seq_along(payloads)) {
    payload <- payloads[[i]] %||% list()
    fit_result <- payload$cache_entry$fit_result %||% list()
    if (isTRUE(jobs_started[[i]])) {
      converged <- isTRUE(fit_result$converged)
      admissible <- converged && isTRUE(fit_result$admissible)
      broker$esem_fits_converged <- broker$esem_fits_converged + as.integer(converged)
      broker$esem_fits_admissible <- broker$esem_fits_admissible + as.integer(admissible)
      broker$esem_fits_failed <- broker$esem_fits_failed + as.integer(!admissible)
      attempts <- suppressWarnings(as.integer(
        payload$cache_entry$solver_attempts_started %||%
          payload$cache_entry$fit_attempt %||%
          fit_result$fit_attempt %||%
          NA_integer_
      ))
      if (length(attempts) == 1L && is.finite(attempts) && attempts > 0L) {
        broker$esem_solver_attempts_observed <- broker$esem_solver_attempts_observed + attempts
      }
    }
    .semantica_append_esem_event(
      broker, stage = stage, candidate_key = keys[[i]],
      cache_hit = isTRUE(cached[[i]]), coalesced_requests = if (i == 1L) coalesced_requests else 0L,
      elapsed_seconds = payload$elapsed_seconds %||% NA_real_,
      fit_result = fit_result, error = payload$error %||% NA_character_,
      fallback_used = payload$cache_entry$fallback_used %||% fit_result$fallback_used %||% NA
    )
  }
  invisible(broker)
}

.semantica_record_dfi_fits <- function(broker, n) {
  n <- suppressWarnings(as.integer(n[1L]))
  if (inherits(broker, "semantica_evaluation_broker") &&
      length(n) == 1L && is.finite(n) && n > 0L) {
    broker$dfi_fits_started <- broker$dfi_fits_started + n
  }
  invisible(broker)
}

.semantica_record_final_esem_fit <- function(broker, n = 1L) {
  n <- suppressWarnings(as.integer(n[1L]))
  if (inherits(broker, "semantica_evaluation_broker") &&
      length(n) == 1L && is.finite(n) && n > 0L) {
    broker$final_esem_fits_started <- broker$final_esem_fits_started + n
  }
  invisible(broker)
}

.semantica_record_archive_esem_fit <- function(broker, n = 1L) {
  n <- suppressWarnings(as.integer(n[1L]))
  if (inherits(broker, "semantica_evaluation_broker") &&
      length(n) == 1L && is.finite(n) && n > 0L) {
    broker$archive_esem_fits_started <- broker$archive_esem_fits_started + n
  }
  invisible(broker)
}

.semantica_evaluation_snapshot <- function(broker) {
  if (!inherits(broker, "semantica_evaluation_broker")) return(NULL)
  events <- if (length(broker$esem_events)) do.call(rbind, broker$esem_events) else data.frame(
    stage = character(), candidate_key = character(), cache_hit = logical(),
    coalesced_requests = integer(), elapsed_seconds = numeric(), solver_method = character(),
    converged = logical(), admissible = logical(), reason = character(), fallback_used = logical(),
    stringsAsFactors = FALSE
  )
  list(
    esem_requests = broker$esem_requests,
    esem_cache_hits = broker$esem_cache_hits,
    esem_coalesced_requests = broker$esem_coalesced_requests,
    esem_unique_candidates = broker$esem_unique_candidates,
    esem_fits_started = broker$esem_fits_started,
    esem_fits_converged = broker$esem_fits_converged,
    esem_fits_admissible = broker$esem_fits_admissible,
    esem_fits_failed = broker$esem_fits_failed,
    esem_solver_attempts_observed = broker$esem_solver_attempts_observed,
    dfi_fits_started = broker$dfi_fits_started,
    archive_esem_fits_started = broker$archive_esem_fits_started,
    final_esem_fits_started = broker$final_esem_fits_started,
    esem_events = events,
    max_esem_fits = broker$max_esem_fits,
    max_esem_fits_definition =
      "Maximum unique search-time ESEM candidate jobs admitted to execution; cache hits and coalesced duplicate requests do not consume the budget."
  )
}
