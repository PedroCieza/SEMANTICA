# Immutable finalization records. The checksum covers serialized bytes, so
# environments inside lavaan objects need not retain pointer identity on reload.
.semantica_bytes_md5 <- function(bytes) {
  path <- tempfile(); on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path); unname(tools::md5sum(path))
}
.semantica_checkpoint_save <- function(value, path) {
  if (file.exists(path)) stop("Refusing to overwrite an existing finalization record: ", path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- serialize(value, NULL, version = 3L)
  record <- list(schema = "semantica-checkpoint-v1", checksum = .semantica_bytes_md5(payload), payload = payload)
  temporary <- tempfile("checkpoint-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(record, temporary, compress = TRUE)
  if (!file.rename(temporary, path)) stop("Could not atomically commit finalization record: ", path)
  invisible(path)
}

#' Read a sealed SEMANTICA finalization record
#'
#' Verifies the stored bytes before decoding a selected-model, diagnostic request,
#' anchor or outcome record. Does not run analyses or modify files.
#' @param path Path to a native finalization record.
#' @return The saved R object, including fitted models when present.
#' @export
semantica_read_checkpoint <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) stop("Supply one checkpoint path.")
  record <- readRDS(path)
  if (!is.list(record) || !identical(record$schema, "semantica-checkpoint-v1") ||
      !is.raw(record$payload) || !identical(record$checksum, .semantica_bytes_md5(record$payload)))
    stop("Finalization record format or checksum differs: ", path)
  unserialize(record$payload)
}
.semantica_rng_snapshot <- function() list(kind = RNGkind(),
  seed = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL)
.semantica_rng_restore <- function(state) {
  do.call(RNGkind, as.list(state$kind))
  if (is.null(state$seed)) {
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", state$seed, envir = .GlobalEnv)
  invisible(NULL)
}
.semantica_finalization_source <- function() {
  functions <- c("evaluate_semantic_n_sensitivity", "run_esem_on_matrix", "extract_and_score_esem",
    "is_admissible_esem_fit", ".semantica_continuous_utility", ".semantica_run_semantic_n")
  .semantica_object_md5(lapply(functions, function(name) body(utils::removeSource(get(name, asNamespace("SEMANTICA"))))))
}
.semantica_finalization_environment <- function() list(R = R.version.string, platform = R.version$platform,
  BLAS = extSoftVersion()["BLAS"], locale = Sys.getlocale("LC_CTYPE"),
  packages = vapply(c("lavaan", "Matrix", "GPArotation"), function(x) as.character(utils::packageVersion(x)), character(1L)))
.semantica_finalization_options <- function(execution, seconds, directory) {
  execution <- match.arg(execution, c("inline", "isolated", "deferred"))
  if (!is.numeric(seconds) || length(seconds) != 1L || is.na(seconds) || seconds <= 0) stop("'semantic_n_max_seconds' must be positive or Inf.")
  if (!is.null(directory) && (!is.character(directory) || length(directory) != 1L || is.na(directory) || !nzchar(trimws(directory)))) stop("Supply one finalization directory or NULL.")
  if (is.finite(seconds) && execution == "inline") execution <- "isolated"
  if (execution != "inline") {
    if (is.null(directory)) stop("Isolated/deferred semantic-N diagnostics require checkpoint_dir (or finalization_dir for ACO_with_ESEM).")
    if (execution == "isolated" && !requireNamespace("callr", quietly = TRUE)) stop("Install 'callr' for isolated semantic-N diagnostics.")
  }
  list(execution = execution, seconds = seconds)
}
.semantica_optional_checkpoint <- function(value, path, required = FALSE) tryCatch({
  .semantica_checkpoint_save(value, path); TRUE
}, error = function(e) {
  if (required) stop(e)
  warning("Finalization record unavailable: ", conditionMessage(e), call. = FALSE); FALSE
})
.semantica_begin_finalization <- function(directory, core, required = FALSE) {
  if (is.null(directory)) return(NULL)
  tryCatch({
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    path <- tempfile("selection-", tmpdir = directory)
    if (!dir.create(path)) stop("Could not create a unique finalization directory.")
    .semantica_checkpoint_save(core, file.path(path, "selected-core.rds"))
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }, error = function(e) {
    if (required) stop(e)
    warning("Selected-model checkpoint unavailable: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}
.semantica_anchor_sources <- function(directory) {
  paths <- list.files(directory, "^anchor-[0-9]+\\.rds$", recursive = TRUE, full.names = TRUE)
  # Existing attempts are immutable. Reuse a compatible row; the reader checks
  # its diagnostic identity, incoming RNG and checksum before accepting it.
  keys <- sub("^anchor-([0-9]+)\\.rds$", "\\1", basename(paths))
  stats::setNames(paths[!duplicated(keys)], keys[!duplicated(keys)])
}
.semantica_diagnostic_child <- function(request, store, sources, package_path, source_checkout) {
  if (source_checkout) {
    if (!requireNamespace("pkgload", quietly = TRUE)) stop("Source-checkout diagnostics require 'pkgload'.")
    pkgload::load_all(package_path, quiet = TRUE, export_all = FALSE)
  } else loadNamespace("SEMANTICA", lib.loc = dirname(package_path))
  request <- getExportedValue("SEMANTICA", "semantica_read_checkpoint")(request)
  invisible(Sys.setlocale("LC_CTYPE", request$environment$locale))
  namespace <- asNamespace("SEMANTICA")
  if (!identical(request$source, get(".semantica_finalization_source", namespace)()) ||
      !identical(request$environment, get(".semantica_finalization_environment", namespace)())) stop("Diagnostic source/numerical environment changed.")
  get(".semantica_rng_restore", namespace)(request$rng)
  arguments <- request$arguments
  arguments$anchor_store <- store; arguments$anchor_sources <- sources
  arguments$diagnostic_id <- request$id; arguments$log_dir <- file.path(dirname(store), "progress")
  arguments$anchor_required <- TRUE
  value <- do.call(get("evaluate_semantic_n_sensitivity", namespace), arguments)
  list(value = value, rng = get(".semantica_rng_snapshot", namespace)())
}
.semantica_run_semantic_n <- function(directory, max_seconds = Inf, sources = character()) {
  caller_rng <- .semantica_rng_snapshot(); on.exit(.semantica_rng_restore(caller_rng), add = TRUE)
  request_path <- file.path(directory, "semantic-n-request.rds")
  request <- semantica_read_checkpoint(request_path)
  options <- .semantica_finalization_options("isolated", max_seconds, directory)
  if (!identical(request$source, .semantica_finalization_source()) ||
      !identical(request$environment, .semantica_finalization_environment())) stop("Diagnostic source/numerical environment changed; original records are retained.")
  if (!identical(request$rng_scope, "serial")) stop("Inline parallel diagnostics cannot be resumed from coordinator RNG. Use isolated/deferred execution for resumable diagnostics.")
  lock <- file.path(directory, ".diagnostics-running")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Diagnostics are locked; inspect the previous process before continuation.")
  process <- NULL
  on.exit({
    if (!is.null(process) && process$is_alive()) { process$kill_tree(); process$wait(5000L) }
    if (!is.null(process) && process$is_alive()) stop("Diagnostic child termination unconfirmed; lock retained.")
    target <- normalizePath(lock, winslash = "/", mustWork = TRUE)
    if (dirname(target) != normalizePath(directory, winslash = "/", mustWork = TRUE) || basename(target) != ".diagnostics-running") stop("Unexpected diagnostic lock path; retained.")
    unlink(target, recursive = TRUE)
  }, add = TRUE)
  attempt <- tempfile("attempt-", tmpdir = directory); dir.create(attempt)
  .semantica_checkpoint_save(list(state = "started", max_seconds = max_seconds, request_id = request$id,
    time = format(Sys.time(), tz = "UTC", usetz = TRUE)), file.path(attempt, "started.rds"))
  package_path <- getNamespaceInfo(asNamespace("SEMANTICA"), "path")
  source_checkout <- file.exists(file.path(package_path, "R", "pipeline_core.R"))
  if (source_checkout && !requireNamespace("pkgload", quietly = TRUE)) stop("Source-checkout diagnostics require 'pkgload'.")
  started <- proc.time()[["elapsed"]]
  child <- .semantica_diagnostic_child; environment(child) <- baseenv()
  process <- callr::r_bg(child,
    args = list(request = request_path, store = file.path(attempt, "anchors"), sources = sources,
      package_path = package_path, source_checkout = source_checkout),
    libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE, supervise = TRUE,
    windows_hide_window = TRUE, stdout = file.path(attempt, "stdout.log"), stderr = file.path(attempt, "stderr.log"),
    env = c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"))
  timed_out <- FALSE
  while (process$is_alive()) {
    if (proc.time()[["elapsed"]] - started >= max_seconds) {
      process$kill_tree(); process$wait(5000L); timed_out <- TRUE
      if (process$is_alive()) stop("Diagnostic child termination unconfirmed; original records retained.")
      break
    }
    process$wait(200L)
  }
  answer <- if (!timed_out) tryCatch(process$get_result(), error = function(e) e) else NULL
  state <- if (timed_out) "timed_out" else if (inherits(answer, "error")) "error" else "completed"
  metadata <- list(status = state, attempt_dir = normalizePath(attempt, winslash = "/"),
    elapsed_seconds = proc.time()[["elapsed"]] - started, max_seconds = max_seconds,
    error = if (inherits(answer, "error")) conditionMessage(answer) else NULL)
  logs <- list.files(file.path(attempt, "progress"), "\\.jsonl$", full.names = TRUE)
  metadata$recorded_fits_started <- sum(vapply(unlist(lapply(logs, readLines, warn = FALSE)), function(line) {
    record <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
    identical(record$event, "started")
  }, logical(1L)))
  .semantica_checkpoint_save(metadata, file.path(attempt, "outcome.rds"))
  if (state != "completed") return(list(available = FALSE, status = state, n_grid = request$arguments$n_grid,
    summary = NULL, execution = metadata, note = "Secondary diagnostics incomplete; selected core and completed anchors are retained."))
  answer$value$status <- "completed"
  answer$value$execution <- c(answer$value$execution, metadata)
  .semantica_checkpoint_save(list(value = answer$value, rng = answer$rng), file.path(attempt, "result.rds"))
  attr(answer$value, "semantica_rng_after") <- answer$rng
  answer$value
}

#' Continue saved reference-N diagnostics without repeating selection
#'
#' Creates a new isolated serial attempt, reuses compatible sealed anchor rows,
#' and restores their recorded RNG transitions. Original attempts and selected
#' models are never overwritten. The caller's RNG is preserved. A finite budget
#' includes child startup; timeout retains partial anchors without a quality score.
#' Inline parallel diagnostics have no replayable coordinator RNG stream; choose
#' isolated or deferred execution in the original call to enable continuation.
#' @param checkpoint_dir The selection-specific directory returned in
#'   `result$advanced$optimization$finalization_checkpoint$directory`, or in
#'   `result$finalization_checkpoint$directory` for direct ACO calls.
#' @param max_seconds Positive wall allowance, or `Inf`.
#' @return Reference-N diagnostic results with explicit completion/execution status.
#' @export
semantica_resume_diagnostics <- function(checkpoint_dir, max_seconds = Inf) {
  if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L || is.na(checkpoint_dir) || !nzchar(checkpoint_dir)) stop("Supply one selection checkpoint directory.")
  directory <- normalizePath(checkpoint_dir, winslash = "/", mustWork = TRUE)
  core <- semantica_read_checkpoint(file.path(directory, "selected-core.rds"))
  request <- semantica_read_checkpoint(file.path(directory, "semantic-n-request.rds"))
  if (!identical(core$id, request$core_id)) stop("Selected core and diagnostic request identities differ.")
  .semantica_preserve_caller_rng(.semantica_run_semantic_n(directory, max_seconds, .semantica_anchor_sources(directory)))
}
