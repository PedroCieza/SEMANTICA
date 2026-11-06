# Raw fitting inputs and RNG are independent of semantic score composition.
.semantica_fit_cache_context <- function(directory) {
  if (is.null(directory)) return(NULL)
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) || !nzchar(directory))
    stop("fit_cache_dir must be NULL or one directory path.", call. = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE))
    stop("Could not create fit_cache_dir: ", directory, call. = FALSE)
  ns <- asNamespace("SEMANTICA")
  code <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  identity <- .semantica_object_md5(list(schema = "semantica-fit-cache-v1",
    environment = .semantica_finalization_environment(),
    code = lapply(code, function(f) list(formals(f), body(utils::removeSource(f))))))
  list(directory = normalizePath(directory, winslash = "/"), identity = identity)
}

.semantica_cached_fit <- function(cache, specification, evaluate, seed = NULL, allow_compute = TRUE) {
  before <- .semantica_rng_snapshot()
  reusable <- !is.null(cache) && (!is.null(seed) || !is.null(before$seed))
  key <- path <- NULL
  if (reusable) {
    key <- .semantica_object_md5(list(cache$identity, specification,
      threads = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")),
      rng = if (is.null(seed)) before else list(seed = seed, kind = before$kind)))
    path <- file.path(cache$directory, paste0(key, ".rds"))
    if (file.exists(path)) {
      record <- tryCatch(semantica_read_checkpoint(path), error = function(e) NULL)
      if (is.list(record) && identical(record$key, key)) {
        if (is.null(seed)) .semantica_rng_restore(record$rng_after)
        return(list(value = record$value, cache_hit = TRUE, available = TRUE))
      }
      warning("Fit cache record is unreadable or incompatible; recomputing without overwriting it: ", path, call. = FALSE)
    }
  }
  if (!allow_compute) return(list(value = NULL, cache_hit = FALSE, available = FALSE))
  value <- if (is.null(seed)) evaluate() else .semantica_with_task_seed(seed, evaluate())
  if (reusable && !file.exists(path)) tryCatch(
    .semantica_checkpoint_save(list(key = key, value = value, rng_after = .semantica_rng_snapshot()), path),
    error = function(e) { if (!file.exists(path)) warning("Fit cache write failed: ", conditionMessage(e), call. = FALSE) })
  list(value = value, cache_hit = FALSE, available = TRUE)
}
