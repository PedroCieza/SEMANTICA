# Central CPU resource planning and PSOCK lifecycle helpers.

.semantica_pool_registry <- local({
  registry <- new.env(parent = emptyenv())
  registry$next_id <- 0L
  registry$active <- new.env(parent = emptyenv(), hash = TRUE)
  registry
})

.semantica_parallelly_available <- function() {
  requireNamespace("parallelly", quietly = TRUE)
}

.semantica_require_parallelly <- function() {
  if (!.semantica_parallelly_available()) {
    stop(
      "Package 'parallelly' is required for SEMANTICA CPU resource planning ",
      "and PSOCK worker creation. Install it with install.packages('parallelly').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.semantica_as_core_count <- function(x, name, minimum = 1L, allow_null = FALSE) {
  if (is.null(x)) {
    if (isTRUE(allow_null)) return(NULL)
    stop(sprintf("'%s' must be an integer greater than or equal to %d.", name, minimum),
         call. = FALSE)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < minimum || x > .Machine$integer.max ||
      abs(x - round(x)) > sqrt(.Machine$double.eps)) {
    stop(sprintf("'%s' must be an integer greater than or equal to %d.", name, minimum),
         call. = FALSE)
  }
  as.integer(round(x))
}

.semantica_normalize_worker_request <- function(n.cores) {
  if (is.character(n.cores) && length(n.cores) == 1L && !is.na(n.cores)) {
    request <- tolower(trimws(n.cores))
    if (identical(request, "auto")) {
      return(list(mode = "auto", requested = "auto"))
    }
  }
  list(
    mode = "explicit",
    requested = .semantica_as_core_count(n.cores, "n.cores")
  )
}

.semantica_available_cores <- function(omit = 0L) {
  omit <- .semantica_as_core_count(omit, "omit", minimum = 0L)
  .semantica_require_parallelly()
  available <- suppressWarnings(parallelly::availableCores(omit = omit))
  available <- suppressWarnings(as.numeric(available[1L]))
  if (length(available) != 1L || !is.finite(available) || available < 1L) {
    stop(
      "parallelly::availableCores() did not return a positive finite CPU count.",
      call. = FALSE
    )
  }
  as.integer(floor(available))
}

.semantica_effective_max_cores <- function(max.cores = NULL) {
  if (is.null(max.cores)) {
    max.cores <- getOption("semantica.max.cores", NULL)
  }
  .semantica_as_core_count(max.cores, "max.cores", allow_null = TRUE)
}


.semantica_available_physical_cores <- function() {
  .semantica_require_parallelly()
  value <- tryCatch(
    suppressWarnings(parallelly::availableCores(omit = 0L, logical = FALSE)),
    error = function(e) NA_integer_
  )
  value <- suppressWarnings(as.numeric(value[1L]))
  if (length(value) != 1L || !is.finite(value) || value < 1L) return(NA_integer_)
  as.integer(floor(value))
}

.semantica_parse_kib_value <- function(lines, key) {
  line <- grep(paste0("^", key, ":"), lines, value = TRUE)
  if (length(line) == 0L) return(NA_real_)
  value <- suppressWarnings(as.numeric(sub("^.*?:[[:space:]]*([0-9.]+).*$", "\\1", line[1L])))
  if (!is.finite(value) || value <= 0) NA_real_ else value * 1024
}

.semantica_process_rss_bytes <- function(pid = Sys.getpid()) {
  pid <- suppressWarnings(as.integer(pid[1L]))
  if (length(pid) != 1L || !is.finite(pid) || pid < 1L) return(NA_real_)

  if (identical(.Platform$OS.type, "unix") && file.exists(sprintf("/proc/%d/status", pid))) {
    lines <- tryCatch(readLines(sprintf("/proc/%d/status", pid), warn = FALSE),
                      error = function(e) character(0L))
    rss <- .semantica_parse_kib_value(lines, "VmRSS")
    if (is.finite(rss)) return(rss)
  }

  if (identical(.Platform$OS.type, "windows")) {
    shell <- unname(Sys.which(c("powershell", "pwsh")))
    shell <- shell[!is.na(shell) & nzchar(shell)]
    if (length(shell) > 0L) {
      shell <- shell[1L]
      command <- sprintf("(Get-Process -Id %d).WorkingSet64", pid)
      out <- tryCatch(
        suppressWarnings(system2(
          shell,
          c("-NoProfile", "-NonInteractive", "-Command", shQuote(command)),
          stdout = TRUE, stderr = FALSE
        )),
        error = function(e) character(0L)
      )
      value <- suppressWarnings(as.numeric(trimws(out[1L])))
      if (length(value) == 1L && is.finite(value) && value > 0) return(value)
    }
  }

  ps <- Sys.which("ps")
  if (nzchar(ps)) {
    out <- tryCatch(
      suppressWarnings(system2(ps, c("-o", "rss=", "-p", as.character(pid)),
                               stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0L)
    )
    value <- suppressWarnings(as.numeric(trimws(out[1L])))
    if (length(value) == 1L && is.finite(value) && value > 0) return(value * 1024)
  }
  NA_real_
}

.semantica_system_memory_bytes <- function() {
  if (identical(.Platform$OS.type, "unix") && file.exists("/proc/meminfo")) {
    lines <- tryCatch(readLines("/proc/meminfo", warn = FALSE),
                      error = function(e) character(0L))
    available <- .semantica_parse_kib_value(lines, "MemAvailable")
    total <- .semantica_parse_kib_value(lines, "MemTotal")
    if (is.finite(available) && is.finite(total)) {
      return(list(available_bytes = available, total_bytes = total, source = "/proc/meminfo"))
    }
  }

  if (identical(.Platform$OS.type, "windows")) {
    shell <- unname(Sys.which(c("powershell", "pwsh")))
    shell <- shell[!is.na(shell) & nzchar(shell)]
    if (length(shell) > 0L) {
      shell <- shell[1L]
      command <- paste0(
        "$os=Get-CimInstance Win32_OperatingSystem; ",
        "[Console]::WriteLine(('{0},{1}' -f $os.FreePhysicalMemory,$os.TotalVisibleMemorySize))"
      )
      out <- tryCatch(
        suppressWarnings(system2(
          shell,
          c("-NoProfile", "-NonInteractive", "-Command", shQuote(command)),
          stdout = TRUE, stderr = FALSE
        )),
        error = function(e) character(0L)
      )
      bits <- strsplit(trimws(out[1L]), ",", fixed = TRUE)[[1L]]
      values <- suppressWarnings(as.numeric(bits))
      if (length(values) >= 2L && all(is.finite(values[1:2])) && all(values[1:2] > 0)) {
        return(list(
          available_bytes = values[1L] * 1024,
          total_bytes = values[2L] * 1024,
          source = "Win32_OperatingSystem"
        ))
      }
    }
  }

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    sysctl <- Sys.which("sysctl")
    vm_stat <- Sys.which("vm_stat")
    if (nzchar(sysctl) && nzchar(vm_stat)) {
      total_out <- tryCatch(system2(sysctl, c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE),
                            error = function(e) character(0L))
      vm_out <- tryCatch(system2(vm_stat, stdout = TRUE, stderr = FALSE),
                         error = function(e) character(0L))
      total <- suppressWarnings(as.numeric(trimws(total_out[1L])))
      page_size <- suppressWarnings(as.numeric(sub(".*page size of ([0-9]+) bytes.*", "\\1", vm_out[1L])))
      page_keys <- c("Pages free", "Pages inactive", "Pages speculative")
      pages <- vapply(page_keys, function(key) {
        line <- grep(paste0("^", key, ":"), vm_out, value = TRUE)
        if (!length(line)) return(0)
        suppressWarnings(as.numeric(gsub("[^0-9]", "", line[1L])))
      }, numeric(1L))
      if (is.finite(total) && total > 0 && is.finite(page_size) && page_size > 0 && all(is.finite(pages))) {
        return(list(
          available_bytes = sum(pages) * page_size,
          total_bytes = total,
          source = "vm_stat/sysctl"
        ))
      }
    }
  }

  list(available_bytes = NA_real_, total_bytes = NA_real_, source = "unavailable")
}

.semantica_memory_snapshot <- function() {
  sys <- .semantica_system_memory_bytes()
  rss <- .semantica_process_rss_bytes()
  list(
    available_bytes = suppressWarnings(as.numeric(sys$available_bytes %||% NA_real_)),
    total_bytes = suppressWarnings(as.numeric(sys$total_bytes %||% NA_real_)),
    process_rss_bytes = suppressWarnings(as.numeric(rss)),
    source = sys$source %||% "unavailable"
  )
}

# PSOCK workers are independent R processes. During transmission, serialized
# globals can coexist transiently with their deserialized worker copies. Using
# two current-main-process RSS equivalents per worker is therefore a
# deliberately conservative, scale-free allowance rather than a fixed-RAM
# threshold. Explicit worker requests remain user-authoritative.
.semantica_auto_worker_capacity <- function(total.cores, reserve.cores = 1L, coordinator.cores = 1L) {
  total.cores <- .semantica_as_core_count(total.cores, "total.cores")
  reserve.cores <- .semantica_as_core_count(reserve.cores, "reserve.cores", minimum = 0L)
  coordinator.cores <- .semantica_as_core_count(coordinator.cores, "coordinator.cores", minimum = 0L)

  coordinator_applied <- min(coordinator.cores, max(0L, total.cores - 1L))
  reserve_applied <- min(
    reserve.cores,
    max(0L, total.cores - coordinator_applied - 1L)
  )
  max(1L, as.integer(total.cores - coordinator_applied - reserve_applied))
}

.semantica_psock_memory_worker_cap <- function(snapshot, transient_multiplier = 2) {
  if (!is.list(snapshot)) return(NA_integer_)
  available <- suppressWarnings(as.numeric(snapshot$available_bytes %||% NA_real_))
  rss <- suppressWarnings(as.numeric(snapshot$process_rss_bytes %||% NA_real_))
  transient_multiplier <- suppressWarnings(as.numeric(transient_multiplier[1L]))
  if (length(available) != 1L || length(rss) != 1L ||
      !is.finite(available) || !is.finite(rss) || available <= 0 || rss <= 0 ||
      length(transient_multiplier) != 1L || !is.finite(transient_multiplier) || transient_multiplier < 1) {
    return(NA_integer_)
  }
  max(1L, as.integer(floor(available / (transient_multiplier * rss))))
}

.semantica_worker_resolution <- function(n.cores = 2L,
                                         use_parallel = TRUE,
                                         reserve.cores = 1L,
                                         coordinator.cores = 1L,
                                         max.cores = NULL,
                                         available.cores = NULL,
                                         auto.cap = NULL,
                                         auto.cap.reason = "auto_safety_cap",
                                         warn = TRUE) {
  if (!is.logical(use_parallel) || length(use_parallel) != 1L || is.na(use_parallel)) {
    stop("'use_parallel' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn)) {
    stop("'warn' must be TRUE or FALSE.", call. = FALSE)
  }

  request <- .semantica_normalize_worker_request(n.cores)
  reserve.cores <- .semantica_as_core_count(
    reserve.cores, "reserve.cores", minimum = 0L
  )
  coordinator.cores <- .semantica_as_core_count(
    coordinator.cores, "coordinator.cores", minimum = 0L
  )
  max.cores <- .semantica_effective_max_cores(max.cores)
  auto.cap <- .semantica_as_core_count(auto.cap, "auto.cap", allow_null = TRUE)
  if (!is.character(auto.cap.reason) || length(auto.cap.reason) != 1L ||
      is.na(auto.cap.reason) || !nzchar(auto.cap.reason)) {
    stop("'auto.cap.reason' must be one non-empty character string.", call. = FALSE)
  }

  if (is.null(available.cores)) {
    if (!isTRUE(use_parallel)) {
      available.cores <- NA_integer_
    } else {
      available.cores <- .semantica_available_cores(omit = 0L)
    }
  } else {
    available.cores <- .semantica_as_core_count(
      available.cores, "available.cores"
    )
  }

  if (!isTRUE(use_parallel)) {
    return(list(
      request_mode = "serial",
      requested_workers = request$requested,
      available_workers = available.cores,
      available_after_reserve = available.cores,
      effective_workers = 1L,
      reserve_cores_requested = reserve.cores,
      reserve_cores_applied = 0L,
      coordinator_cores_requested = coordinator.cores,
      coordinator_cores_applied = 0L,
      max_cores = max.cores,
      auto_safety_cap = NULL,
      auto_safety_reason = NULL,
      use_parallel = FALSE,
      limited_by = "use_parallel_false"
    ))
  }

  available_for_request <- available.cores
  reserve_applied <- 0L
  coordinator_applied <- 0L
  if (identical(request$mode, "auto")) {
    # A PSOCK plan comprises the main/coordinator R process plus worker R
    # processes.  Reserve the coordinator separately so `reserve.cores = 1`
    # actually leaves one CPU slot outside SEMANTICA instead of being consumed
    # by the main R session itself.  On very small allocations we always keep
    # at least one execution slot, which naturally resolves to serial work.
    coordinator_applied <- min(
      coordinator.cores,
      max(0L, available.cores - 1L)
    )
    reserve_applied <- min(
      reserve.cores,
      max(0L, available.cores - coordinator_applied - 1L)
    )
    available_for_request <- max(
      1L,
      available.cores - coordinator_applied - reserve_applied
    )
    desired <- available_for_request
  } else {
    # An explicit request is a ceiling in its own right. Reserving coordinator
    # or user headroom here would make `n.cores = 8` mean something other than
    # the value the user requested, so automatic safety accounting applies only
    # to `n.cores = "auto"`.
    desired <- request$requested
  }

  limits <- c(available = available_for_request)
  if (!is.null(max.cores)) limits <- c(limits, max_cores = max.cores)
  if (identical(request$mode, "auto") && !is.null(auto.cap)) {
    limits <- c(limits, auto_safety = auto.cap)
  }
  effective <- max(1L, min(c(desired, limits)))
  effective <- as.integer(effective)

  limited_by <- character(0L)
  if (desired > available_for_request) limited_by <- c(limited_by, "available_workers")
  if (!is.null(max.cores) && desired > max.cores) limited_by <- c(limited_by, "max_cores")
  if (identical(request$mode, "auto") && !is.null(auto.cap) && desired > auto.cap) {
    limited_by <- c(limited_by, auto.cap.reason)
  }
  if (length(limited_by) == 0L) limited_by <- "none"

  if (isTRUE(warn) && identical(request$mode, "explicit") &&
      effective < request$requested) {
    warning(
      sprintf(
        "Requested %d parallel workers, but SEMANTICA will use %d (%s).",
        request$requested,
        effective,
        paste(limited_by, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    request_mode = request$mode,
    requested_workers = request$requested,
    available_workers = available.cores,
    available_after_reserve = available_for_request,
    effective_workers = effective,
    reserve_cores_requested = reserve.cores,
    reserve_cores_applied = reserve_applied,
    coordinator_cores_requested = coordinator.cores,
    coordinator_cores_applied = coordinator_applied,
    max_cores = max.cores,
    auto_safety_cap = if (identical(request$mode, "auto")) auto.cap else NULL,
    auto_safety_reason = if (identical(request$mode, "auto") && !is.null(auto.cap)) auto.cap.reason else NULL,
    use_parallel = TRUE,
    limited_by = limited_by
  )
}

.semantica_resolve_workers <- function(n.cores = 2L,
                                       use_parallel = TRUE,
                                       reserve.cores = 1L,
                                       coordinator.cores = 1L,
                                       max.cores = NULL,
                                       available.cores = NULL,
                                       warn = TRUE) {
  .semantica_worker_resolution(
    n.cores = n.cores,
    use_parallel = use_parallel,
    reserve.cores = reserve.cores,
    coordinator.cores = coordinator.cores,
    max.cores = max.cores,
    available.cores = available.cores,
    warn = warn
  )$effective_workers
}

# Backward-compatible internal helper. Unlike the historical implementation,
# this resolves against the CPU allocation visible to R and has no artificial
# two-worker ceiling.
.semantica_max_workers <- function(n, available.cores = NULL, max.cores = NULL) {
  .semantica_resolve_workers(
    n.cores = n,
    use_parallel = TRUE,
    reserve.cores = 0L,
    max.cores = max.cores,
    available.cores = available.cores,
    warn = FALSE
  )
}

.semantica_worker_env <- function(threads = 1L) {
  threads <- .semantica_as_core_count(threads, "threads")
  value <- as.character(threads)
  c(
    OMP_NUM_THREADS = value,
    OPENBLAS_NUM_THREADS = value,
    MKL_NUM_THREADS = value,
    VECLIB_MAXIMUM_THREADS = value,
    NUMEXPR_NUM_THREADS = value
  )
}

.semantica_resource_capabilities <- function() {
  installed <- .semantica_parallelly_available()
  available <- if (installed) {
    tryCatch(.semantica_available_cores(omit = 0L), error = function(e) NA_integer_)
  } else {
    NA_integer_
  }
  auto_plan <- if (installed) {
    tryCatch(semantica_resource_plan(n.cores = "auto", memory_aware = TRUE),
             error = function(e) NULL)
  } else NULL
  auto_workers <- if (!is.null(auto_plan)) {
    auto_plan$effective_workers
  } else if (is.finite(available)) {
    .semantica_auto_worker_capacity(available, reserve.cores = 1L, coordinator.cores = 1L)
  } else NA_integer_
  ext <- tryCatch(extSoftVersion(), error = function(e) character(0L))
  blas <- unname(ext["BLAS"])
  if (length(blas) == 0L || is.na(blas) || !nzchar(blas)) blas <- NA_character_
  list(
    parallelly_installed = installed,
    parallelly_version = if (installed) as.character(utils::packageVersion("parallelly")) else NA_character_,
    available_workers = available,
    default_auto_workers = auto_workers,
    memory_aware_auto = !is.null(auto_plan) && isTRUE(auto_plan$memory_aware),
    memory_worker_cap = if (!is.null(auto_plan)) auto_plan$memory_worker_cap else NA_integer_,
    memory_source = if (!is.null(auto_plan)) auto_plan$memory_source else "unavailable",
    physical_worker_cap = if (!is.null(auto_plan)) auto_plan$physical_worker_cap else NA_integer_,
    coordinator_cores = if (!is.null(auto_plan)) auto_plan$coordinator_cores_applied else NA_integer_,
    parallel_backend = "PSOCK",
    worker_blas_threads = 1L,
    blas = blas
  )
}

#' Inspect SEMANTICA's CPU resource plan
#'
#' Resolve a requested worker count against the CPU allocation visible to the
#' current R process. CPU detection uses [parallelly::availableCores()], so job
#' scheduler, container, option, and package-check limits are respected. For
#' `n.cores = "auto"`, SEMANTICA separately budgets the main/coordinator R
#' process and the requested reserve, avoids exceeding the detected physical-core
#' worker budget when that information is available, and applies a memory-aware
#' PSOCK safety cap when host memory and process RSS can be measured. Explicit
#' numeric requests remain user-authoritative.
#'
#' @param n.cores A positive integer worker request or `"auto"`. Explicit
#'   numeric requests are not reduced by `reserve.cores`; they are bounded only
#'   by the available allocation and `max.cores`.
#' @param use_parallel Logical. `FALSE` always selects serial execution.
#' @param reserve.cores Nonnegative number of cores retained outside SEMANTICA
#'   when `n.cores = "auto"`. The main/coordinator R process is budgeted
#'   separately, so the reserve is genuine user/OS headroom. It does not alter
#'   an explicit numeric request.
#' @param max.cores Optional positive upper bound. When `NULL`, the option
#'   `semantica.max.cores` is used when set.
#' @param memory_aware Logical. When `TRUE` (default), automatic PSOCK worker
#'   resolution is capped by measured available memory using the current R
#'   process resident set as a worker-footprint proxy. Explicit numeric worker
#'   requests are never reduced by this policy.
#'
#' @section Side effects:
#' Inspects local CPU/resource availability and process-environment information;
#' it does not start worker processes.
#'
#' @section Reproducibility:
#' Automatic worker resolution is host-dependent. The returned plan records the
#' requested and effective worker counts so the execution environment can be
#' reconstructed or deliberately overridden.
#'
#' @return A `semantica_resource_plan` list describing requested, available,
#'   and effective workers plus the PSOCK and worker-threading policy. Creating
#'   a plan does not start worker processes or change main-session BLAS settings.
#'
#' @references
#' Bengtsson, H. (2021). A unifying framework for parallel and distributed
#' processing in R using futures. *The R Journal, 13*(2), 208-227.
#'
#' Thrun, M. C., & Märte, J. (2026). Memory sharing for multicore computation
#' in R with an application to feature selection by mutual information using
#' PDE. *The R Journal*.
#' @export
#'
#' @examples
#' \dontrun{
#' semantica_resource_plan(n.cores = "auto")
#' semantica_resource_plan(n.cores = 8L, max.cores = 4L)
#' semantica_resource_plan(use_parallel = FALSE)
#' }
semantica_resource_plan <- function(n.cores = 2L,
                                    use_parallel = TRUE,
                                    reserve.cores = 1L,
                                    max.cores = NULL,
                                    memory_aware = TRUE) {
  if (!is.logical(memory_aware) || length(memory_aware) != 1L || is.na(memory_aware)) {
    stop("'memory_aware' must be TRUE or FALSE.", call. = FALSE)
  }
  # Detect even for a serial plan so the returned capability information is
  # complete and a missing runtime dependency produces one actionable error.
  available <- .semantica_available_cores(omit = 0L)
  request <- .semantica_normalize_worker_request(n.cores)
  memory_snapshot <- if (isTRUE(memory_aware) && identical(request$mode, "auto") && isTRUE(use_parallel)) {
    .semantica_memory_snapshot()
  } else {
    list(available_bytes = NA_real_, total_bytes = NA_real_, process_rss_bytes = NA_real_, source = "not_applied")
  }
  memory_cap <- if (isTRUE(memory_aware) && identical(request$mode, "auto") && isTRUE(use_parallel)) {
    .semantica_psock_memory_worker_cap(memory_snapshot)
  } else NA_integer_

  # CPU auto-planning is independent of the optional memory heuristic.  Prefer
  # one PSOCK worker per physical core at most, and charge both the coordinating
  # R process and the requested reserve against that budget.  Clamp the physical
  # count to the allocation visible to R so scheduler/cgroup limits remain
  # authoritative.
  physical_cores <- if (identical(request$mode, "auto") && isTRUE(use_parallel)) {
    .semantica_available_physical_cores()
  } else NA_integer_
  physical_cores_visible <- if (is.finite(physical_cores)) {
    as.integer(min(physical_cores, available))
  } else NA_integer_
  physical_worker_cap <- if (is.finite(physical_cores_visible)) {
    .semantica_auto_worker_capacity(
      physical_cores_visible,
      reserve.cores = reserve.cores,
      coordinator.cores = 1L
    )
  } else NA_integer_

  auto_caps <- c(
    memory_budget = if (is.finite(memory_cap)) as.integer(memory_cap) else NA_integer_,
    physical_core_budget = if (is.finite(physical_worker_cap)) as.integer(physical_worker_cap) else NA_integer_
  )
  finite_caps <- auto_caps[is.finite(auto_caps)]
  auto_cap <- if (length(finite_caps)) as.integer(min(finite_caps)) else NULL
  if (is.null(auto_cap)) {
    auto_reason <- "auto_safety_cap"
  } else {
    active <- names(finite_caps)[finite_caps == auto_cap]
    auto_reason <- if (length(active) > 1L) {
      "memory_and_physical_core_budget"
    } else {
      active[[1L]]
    }
  }

  resolution <- .semantica_worker_resolution(
    n.cores = n.cores,
    use_parallel = use_parallel,
    reserve.cores = reserve.cores,
    coordinator.cores = 1L,
    max.cores = max.cores,
    available.cores = available,
    auto.cap = auto_cap,
    auto.cap.reason = auto_reason,
    warn = TRUE
  )
  worker_env <- .semantica_worker_env(1L)
  plan <- c(
    resolution,
    list(
      parallel_backend = if (resolution$effective_workers > 1L) "PSOCK" else "serial",
      worker_blas_threads = if (resolution$effective_workers > 1L) 1L else NA_integer_,
      worker_environment = if (resolution$effective_workers > 1L) worker_env else character(0L),
      nested_parallelism = "one_semantica_pool_per_process",
      memory_aware = isTRUE(memory_aware),
      memory_available_bytes = memory_snapshot$available_bytes %||% NA_real_,
      memory_total_bytes = memory_snapshot$total_bytes %||% NA_real_,
      process_rss_bytes = memory_snapshot$process_rss_bytes %||% NA_real_,
      memory_source = memory_snapshot$source %||% "unavailable",
      memory_worker_cap = if (is.finite(memory_cap)) as.integer(memory_cap) else NA_integer_,
      physical_cores_detected = if (is.finite(physical_cores)) as.integer(physical_cores) else NA_integer_,
      physical_cores_visible = if (is.finite(physical_cores_visible)) as.integer(physical_cores_visible) else NA_integer_,
      physical_worker_cap = if (is.finite(physical_worker_cap)) as.integer(physical_worker_cap) else NA_integer_,
      # Backward-compatible field retained for consumers of 0.4.6 telemetry.
      # It is populated only when memory measurement is unavailable.
      physical_core_fallback = if (!is.finite(memory_cap) && is.finite(physical_worker_cap)) as.integer(physical_worker_cap) else NA_integer_,
      memory_transient_multiplier = 2,
      parallelly_version = as.character(utils::packageVersion("parallelly"))
    )
  )
  class(plan) <- c("semantica_resource_plan", "list")
  plan
}

.semantica_resource_plan <- function(...) {
  semantica_resource_plan(...)
}

#' Print a SEMANTICA CPU resource plan
#'
#' @param x A resource plan returned by [semantica_resource_plan()].
#' @param ... Additional arguments, currently ignored.
#'
#' @return Invisibly returns `x`.
#' @method print semantica_resource_plan
#' @export
print.semantica_resource_plan <- function(x, ...) {
  requested <- if (identical(x$request_mode, "serial")) {
    paste0(x$requested_workers, " (parallel disabled)")
  } else {
    as.character(x$requested_workers)
  }
  cat("<semantica_resource_plan>\n")
  cat(sprintf("  Requested workers   : %s\n", requested))
  cat(sprintf("  Available to R      : %s\n", as.character(x$available_workers)))
  if (identical(x$request_mode, "auto")) {
    cat(sprintf("  Coordinator budget  : %d\n", x$coordinator_cores_applied %||% 0L))
    cat(sprintf("  Reserved (auto)     : %d\n", x$reserve_cores_applied))
  }
  cat(sprintf("  Effective workers   : %d\n", x$effective_workers))
  if (identical(x$request_mode, "auto")) {
    if (is.finite(x$physical_worker_cap %||% NA_real_)) {
      cat(sprintf("  Physical-core cap   : %d worker(s)\n", x$physical_worker_cap))
    }
    if (isTRUE(x$memory_aware) && is.finite(x$memory_worker_cap %||% NA_real_)) {
      cat(sprintf("  Memory-aware cap    : %d (%s)\n", x$memory_worker_cap, x$memory_source %||% "measured"))
    }
    if (!is.finite(x$physical_worker_cap %||% NA_real_) &&
        (!isTRUE(x$memory_aware) || !is.finite(x$memory_worker_cap %||% NA_real_))) {
      cat("  Auto safety cap     : CPU allocation with coordinator/reserve accounting\n")
    }
  }
  cat(sprintf("  Parallel backend    : %s\n", x$parallel_backend))
  cat(sprintf(
    "  Worker BLAS threads : %s\n",
    if (is.na(x$worker_blas_threads)) "not applicable" else as.character(x$worker_blas_threads)
  ))
  invisible(x)
}

.semantica_validate_resource_plan <- function(resource_plan) {
  if (!inherits(resource_plan, "semantica_resource_plan") ||
      is.null(resource_plan$effective_workers)) {
    stop(
      "'resource_plan' must be created by semantica_resource_plan().",
      call. = FALSE
    )
  }
  effective <- .semantica_as_core_count(
    resource_plan$effective_workers, "resource_plan$effective_workers"
  )
  invisible(effective)
}


.semantica_pool_entry_alive <- function(entry, timeout = 0.25) {
  if (!is.list(entry) || is.null(entry$cluster)) return(NA)
  cl <- entry$cluster
  alive <- tryCatch(
    suppressWarnings(parallelly::isNodeAlive(cl, timeout = timeout)),
    error = function(e) rep(NA, length(cl))
  )
  if (!length(alive)) return(FALSE)
  if (any(alive %in% TRUE, na.rm = TRUE)) return(TRUE)
  if (all(alive %in% FALSE, na.rm = TRUE)) return(FALSE)
  NA
}

.semantica_reap_stale_pools <- function(timeout = 0.25) {
  tokens <- ls(.semantica_pool_registry$active, all.names = TRUE)
  if (!length(tokens)) return(invisible(character(0L)))
  .semantica_require_parallelly()
  reaped <- character(0L)
  for (token in tokens) {
    entry <- get(token, envir = .semantica_pool_registry$active, inherits = FALSE)
    # Registry entries from pre-0.3.0 sessions cannot be interrogated safely;
    # preserve them rather than deleting a potentially live pool.
    if (!is.list(entry) || is.null(entry$cluster)) next
    if (!is.null(entry$owner_pid) && !identical(as.integer(entry$owner_pid), as.integer(Sys.getpid()))) {
      rm(list = token, envir = .semantica_pool_registry$active)
      reaped <- c(reaped, token)
      next
    }
    alive <- .semantica_pool_entry_alive(entry, timeout = timeout)
    if (identical(alive, FALSE)) {
      rm(list = token, envir = .semantica_pool_registry$active)
      reaped <- c(reaped, token)
    }
  }
  invisible(reaped)
}

#' Reset SEMANTICA-owned CPU worker resources
#'
#' Attempts to stop only PSOCK clusters created and registered by SEMANTICA in
#' the current R process. This is intended as an advanced recovery tool after an
#' interrupted computation; it does not alter optimizer state, scores, or model
#' configuration.
#'
#' @param force If `FALSE` (default), clusters that cannot be stopped cleanly
#'   remain registered. If `TRUE`, SEMANTICA may use `parallelly::killNode()` on
#'   its own still-live RichSOCK workers after graceful shutdown fails.
#' @return Invisibly, a list containing stopped, reaped, and failed pool tokens.
#' @export
semantica_reset_resources <- function(force = FALSE) {
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("'force' must be TRUE or FALSE.", call. = FALSE)
  }
  .semantica_require_parallelly()
  reaped <- .semantica_reap_stale_pools()
  tokens <- ls(.semantica_pool_registry$active, all.names = TRUE)
  stopped <- failed <- character(0L)
  for (token in tokens) {
    entry <- get(token, envir = .semantica_pool_registry$active, inherits = FALSE)
    if (!is.list(entry) || is.null(entry$cluster)) {
      failed <- c(failed, token)
      next
    }
    cl <- entry$cluster
    ok <- tryCatch({
      parallel::stopCluster(cl)
      TRUE
    }, error = function(e) FALSE)
    if (!ok && isTRUE(force)) {
      ok <- tryCatch({
        parallelly::killNode(cl)
        TRUE
      }, error = function(e) FALSE)
    }
    alive <- .semantica_pool_entry_alive(entry)
    if (ok || identical(alive, FALSE)) {
      if (exists(token, envir = .semantica_pool_registry$active, inherits = FALSE)) {
        rm(list = token, envir = .semantica_pool_registry$active)
      }
      stopped <- c(stopped, token)
    } else {
      failed <- c(failed, token)
    }
  }
  invisible(list(stopped = unique(stopped), reaped = unique(reaped), failed = unique(failed)))
}

.semantica_make_cluster <- function(resource_plan, ...) {
  workers <- .semantica_validate_resource_plan(resource_plan)
  if (workers <= 1L) return(NULL)
  .semantica_require_parallelly()
  .semantica_reap_stale_pools()
  if (length(ls(.semantica_pool_registry$active, all.names = TRUE)) > 0L) {
    stop(
      paste(
        "SEMANTICA already owns an active CPU worker pool in this process.",
        "Nested SEMANTICA pools are disabled to prevent oversubscription."
      ),
      call. = FALSE
    )
  }

  dots <- list(...)
  protected <- intersect(names(dots), c("workers", "rscript_envs", "autoStop"))
  if (length(protected) > 0L) {
    stop(
      sprintf(
        "Do not pass %s through '...'; SEMANTICA derives these from the resource plan.",
        paste(sprintf("'%s'", protected), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  args <- c(
    list(
      workers = workers,
      rscript_envs = .semantica_worker_env(1L),
      autoStop = TRUE
    ),
    dots
  )
  cluster <- tryCatch(
    do.call(parallelly::makeClusterPSOCK, args),
    error = function(e) {
      stop(
        sprintf(
          "SEMANTICA could not create a %d-worker PSOCK cluster: %s",
          workers,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  .semantica_pool_registry$next_id <- .semantica_pool_registry$next_id + 1L
  token <- paste0("pool_", .semantica_pool_registry$next_id)
  attr(cluster, "semantica_pool_token") <- token
  assign(
    token,
    list(cluster = cluster, owner_pid = Sys.getpid(), created_at = Sys.time()),
    envir = .semantica_pool_registry$active
  )
  attr(cluster, "semantica_resource_plan") <- resource_plan
  cluster
}

.semantica_stop_cluster <- function(cluster) {
  if (is.null(cluster)) return(invisible(FALSE))
  token <- attr(cluster, "semantica_pool_token", exact = TRUE)
  stopped <- tryCatch(
    {
      .semantica_stop_psock_cluster(cluster)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!is.null(token) &&
      exists(token, envir = .semantica_pool_registry$active, inherits = FALSE)) {
    entry <- get(token, envir = .semantica_pool_registry$active, inherits = FALSE)
    alive <- if (isTRUE(stopped)) FALSE else .semantica_pool_entry_alive(entry)
    if (isTRUE(stopped) || identical(alive, FALSE)) {
      rm(list = token, envir = .semantica_pool_registry$active)
    }
  }
  invisible(stopped)
}

.semantica_stop_psock_cluster <- function(cluster) {
  parallel::stopCluster(cluster)
}

.semantica_cluster_export_environment <- function(cluster, envir) {
  if (is.null(cluster)) return(invisible(character(0L)))
  if (!is.environment(envir)) {
    stop("'envir' must be an environment.", call. = FALSE)
  }
  object_names <- ls(envir, all.names = TRUE)
  if (length(object_names) > 0L) {
    parallel::clusterExport(cluster, varlist = object_names, envir = envir)
  }
  invisible(object_names)
}

.semantica_with_cluster <- function(resource_plan, fun, ...) {
  if (!is.function(fun)) stop("'fun' must be a function.", call. = FALSE)
  cluster <- .semantica_make_cluster(resource_plan)
  on.exit(.semantica_stop_cluster(cluster), add = TRUE)
  fun(cluster, ...)
}

.semantica_resource_telemetry <- function(resource_plan, cluster = NULL,
                                          elapsed_seconds = NULL) {
  .semantica_validate_resource_plan(resource_plan)
  if (!is.null(elapsed_seconds)) {
    if (!is.numeric(elapsed_seconds) || length(elapsed_seconds) != 1L ||
        is.na(elapsed_seconds) || !is.finite(elapsed_seconds) || elapsed_seconds < 0) {
      stop("'elapsed_seconds' must be NULL or one nonnegative finite number.",
           call. = FALSE)
    }
    elapsed_seconds <- as.numeric(elapsed_seconds)
  }
  list(
    requested_workers = resource_plan$requested_workers,
    request_mode = resource_plan$request_mode,
    available_workers = resource_plan$available_workers,
    effective_workers = resource_plan$effective_workers,
    reserve_cores_applied = resource_plan$reserve_cores_applied,
    coordinator_cores_applied = resource_plan$coordinator_cores_applied %||% 0L,
    max_cores = resource_plan$max_cores,
    auto_safety_cap = resource_plan$auto_safety_cap %||% NULL,
    auto_safety_reason = resource_plan$auto_safety_reason %||% NULL,
    memory_aware = resource_plan$memory_aware %||% FALSE,
    memory_available_bytes = resource_plan$memory_available_bytes %||% NA_real_,
    process_rss_bytes = resource_plan$process_rss_bytes %||% NA_real_,
    memory_worker_cap = resource_plan$memory_worker_cap %||% NA_integer_,
    memory_source = resource_plan$memory_source %||% "unavailable",
    physical_cores_detected = resource_plan$physical_cores_detected %||% NA_integer_,
    physical_cores_visible = resource_plan$physical_cores_visible %||% NA_integer_,
    physical_worker_cap = resource_plan$physical_worker_cap %||% NA_integer_,
    physical_core_fallback = resource_plan$physical_core_fallback %||% NA_integer_,
    limited_by = resource_plan$limited_by %||% "none",
    parallel_backend = resource_plan$parallel_backend,
    worker_blas_threads = resource_plan$worker_blas_threads,
    workers_created = if (is.null(cluster)) 0L else as.integer(length(cluster)),
    elapsed_seconds = elapsed_seconds,
    parallelly_version = resource_plan$parallelly_version
  )
}
