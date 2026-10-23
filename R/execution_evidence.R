# Small, shared adapters for execution evidence. These do not change scoring.
.semantica_warning_events <- function(messages = character(), stage = "unknown",
                                      candidate_id = NA_character_, attempt = NA_integer_,
                                      calls = NA_character_) {
  data.frame(stage = rep(stage, length(messages)),
    candidate_id = rep(candidate_id, length.out = length(messages)),
    attempt = rep(as.integer(attempt), length.out = length(messages)),
    severity = rep("warning", length(messages)), message = as.character(messages),
    call = rep(calls, length.out = length(messages)), stringsAsFactors = FALSE)
}

.semantica_solver_warning_events <- function(records, stage, candidate_id = NA_character_) {
  rows <- lapply(records, function(x) .semantica_warning_events(x$warnings,
    stage, candidate_id, x$attempt %||% NA_integer_, x$warning_calls %||% NA_character_))
  if (length(rows)) do.call(rbind, rows) else .semantica_warning_events()
}

.semantica_calibration_telemetry <- function(cutoffs, method) {
  telemetry <- cutoffs$telemetry
  count <- function(...) {
    for (x in list(...)) if (is.numeric(x) && length(x) == 1L && is.finite(x)) return(as.integer(x))
    NA_integer_
  }
  requested <- count(telemetry$requested_reps, cutoffs$requested_reps, cutoffs$dfi_requested_reps)
  completed <- count(telemetry$completed_reps, cutoffs$simulation_provenance$completed,
    cutoffs$dfi_simulation_reps)
  successful <- count(telemetry$successful_fits, cutoffs$successful_fits)
  list(method = method,
    status = if (identical(method, "heuristic")) "not_requested" else .semantica_dfi_stage_status(cutoffs),
    requested_reps = requested, completed_reps = completed, successful_fits = successful,
    failed_fits = count(telemetry$failed_fits, completed - successful),
    execution_backend = telemetry$execution$completion_backend %||% NA_character_,
    recovery_count = if (is.null(telemetry$execution)) NA_integer_ else
      length(telemetry$execution$recovery_events),
    stop_reason = telemetry$adaptive$stop_reason %||%
      if (is.finite(requested) && is.finite(completed) && completed >= requested) "budget_completed" else NA_character_)
}

.semantica_solver_state <- function(fit, records, rejected) {
  if (!is.null(fit)) return("accepted")
  if (any(vapply(rejected, function(x) isTRUE(x$converged), logical(1L)))) return("converged_inadmissible")
  if (!length(records)) return("input_rejected")
  if (all(vapply(records, function(x) !is.null(x$error) &&
      !identical(x$failure_reason, "optimizer_nonconvergence_before_rotation"), logical(1L)))) return("execution_error")
  "nonconverged"
}
