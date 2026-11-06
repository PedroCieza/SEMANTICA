# Descriptive helpers: these do not rank candidates or alter search controls.
# Opt-in controller trace for development audits. File I/O uses no R RNG.
# Existing per-fit progress files cover worker fits; this trace covers decisions.
.semantica_trace_candidates <- function(event, vectors, details = list()) {
  directory <- getOption("SEMANTICA.candidate_trace_dir")
  if (is.null(directory)) return(invisible(NULL))
  tryCatch({
    if (!is.character(directory) || length(directory) != 1L || is.na(directory) || !nzchar(directory))
      stop("SEMANTICA.candidate_trace_dir must be one directory.")
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE))
      stop("Could not create the candidate trace directory.")
    rows <- lapply(seq_along(vectors), function(i) {
      v <- vectors[[i]]
      ids <- names(v)[v == 1L]
      c(list(schema = "semantica-candidate-trace-v1", event = event, slot = i,
        position_key = make_solution_key(v),
        form_key = if (length(ids)) paste(sort(ids), collapse = "|") else NA_character_), details)
    })
    if (length(rows)) cat(paste(vapply(rows, function(row)
      jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
      character(1L)), collapse = "\n"), "\n",
      file = file.path(directory, paste0("candidates-", Sys.getpid(), ".jsonl")),
      append = TRUE, sep = "")
  }, error = function(e) warning("Candidate trace unavailable: ", conditionMessage(e), call. = FALSE))
  invisible(NULL)
}

.semantica_search_evidence <- function(pfa_active, esem_requested, esem_active,
    pfa_successes, pfa_attempts, esem_successes, esem_attempts,
    pfa_minimum, esem_minimum, termination) {
  tracks <- data.frame(track = c("PFA", "ESEM"),
    requested = c(pfa_active, esem_requested), active = c(pfa_active, esem_active),
    successful_checkpoints = c(pfa_successes, esem_successes),
    attempted_checkpoints = c(pfa_attempts, esem_attempts),
    minimum_for_archive_stop = c(pfa_minimum, esem_minimum))
  tracks$minimum_met <- ifelse(tracks$active,
    tracks$successful_checkpoints >= tracks$minimum_for_archive_stop, NA)
  tracks$state <- ifelse(tracks$active, ifelse(tracks$minimum_met, "minimum_met", "insufficient_checkpoints"),
    ifelse(tracks$requested, "requested_not_active", "not_requested"))
  list(termination_reason = termination, tracks = tracks,
    active_checkpoint_minima_met = all(tracks$minimum_met[tracks$active]),
    note = "Checkpoint minima govern archive-stability stopping; patience and resource stops remain independent. Final model admissibility is reported separately.")
}

.semantica_form_pressure <- function(vectors, scores) {
  keys <- vapply(vectors, make_solution_key, character(1L))
  top <- utils::head(order(scores, decreasing = TRUE), max(1L, floor(.2 * length(keys))))
  data.frame(ants = length(keys), forms = length(unique(keys)),
    largest_clone_share = if (length(keys)) max(table(keys))/length(keys) else NA_real_,
    ranked_elite_slots = length(top), ranked_elite_forms = length(unique(keys[top])))
}

.semantica_solver_metadata <- function(result) {
  records <- Filter(is.list, result$solver_records %||% list())
  accepted <- if (isTRUE(result$admissible)) result$fit_attempt %||%
    utils::tail(which(vapply(records, function(x) isTRUE(x$admissible), logical(1L))), 1L) else NA_integer_
  if (!length(accepted) || !is.finite(accepted)) accepted <- NA_integer_
  index <- if (is.finite(accepted) && accepted >= 1 && accepted <= length(records)) accepted else length(records)
  method <- result$accepted_method %||% result$fit_method %||% result$method %||%
    if (index > 0L) records[[index]]$optimizer else NA_character_
  list(method = method %||% NA_character_, accepted_attempt = as.integer(accepted),
    attempts_recorded = if (length(records)) length(records) else NA_integer_,
    fallback_used = if (length(records)) length(records) > 1L else NA)
}

# Each process appends to its own JSONL file. A killed writer may leave one
# incomplete final line; earlier complete records and unmatched starts survive.
.semantica_esem_progress <- function(context, settings) {
  if (is.null(context$directory)) return(function(...) invisible(NULL))
  active <- TRUE; started <- proc.time()[["elapsed"]]
  job <- basename(tempfile(pattern = paste0("fit-", Sys.getpid(), "-")))
  path <- file.path(context$directory, paste0("events-", Sys.getpid(), ".jsonl"))
  record <- function(event, details) {
    if (!active) return(invisible(NULL))
    tryCatch({
      if (!dir.exists(context$directory) && !dir.create(context$directory, recursive = TRUE, showWarnings = FALSE))
        stop("Could not create the progress directory.")
      value <- c(list(schema = "semantica-esem-progress-v1", job = job,
        stage = context$stage, candidate_key = context$key, event = event,
        time_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
        elapsed_seconds = proc.time()[["elapsed"]] - started), details)
      cat(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
        "\n", file = path, append = TRUE, sep = "")
    }, error = function(e) {
      active <<- FALSE
      warning("ESEM progress logging unavailable: ", conditionMessage(e), call. = FALSE)
    })
    invisible(NULL)
  }
  record("started", list(settings = settings))
  function(result) record("finished", result[c("status", "solver_attempts_started", "accepted_attempt",
    "rejection_assessment", "rejected_attempts", "solver_records")])
}
