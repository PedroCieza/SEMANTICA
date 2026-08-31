# SEMANTICA core ACO-ESEM optimization helpers.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
}

# Avoid R CMD check notes for dynamically referenced names
utils::globalVariables(c(
  "Item", "Archive_Entry", "Selected", "Factor", "type",
  "cfi", "rmsea", "srmr", "tli", "ave", "htmt_max",
  "eval", "value", "Metric", "best_so_far", "label",
  "observed", "cutoff", "direction", "pass", "x", "y",
  "item.vector", "factors", "list.items", "cosine_sim_matrix", "model_info", "s"
))
# =================================================================
# 0-A  SAFE FISHER-Z HELPERS
# =================================================================
fisherz <- function(r) {
  r <- pmin(pmax(r, -0.9999), 0.9999)
  atanh(r)
}

fisherz_inv <- function(z) {
  tanh(z)
}

# =================================================================
# 0-B  EXTRACT SIMILARITY SUBMATRIX
# =================================================================
extract_similarity_submatrix <- function(sim_matrix, items) {
  if (!is.matrix(sim_matrix)) {
    if (is.data.frame(sim_matrix)) {
      sim_matrix <- as.matrix(sim_matrix)
    } else {
      stop("extract_similarity_submatrix: input is not a matrix or data.frame.")
    }
  }
  rn <- rownames(sim_matrix)
  cn <- colnames(sim_matrix)
  if (is.null(rn) || is.null(cn))
    stop("extract_similarity_submatrix: matrix has no row/col names.")

  row_idx <- match(items, rn)
  col_idx <- match(items, cn)
  items_valid <- items[!is.na(row_idx) & !is.na(col_idx)]
  if (length(items_valid) < 2L)
    stop("extract_similarity_submatrix: fewer than 2 valid items found.")
  if (length(items_valid) < length(items)) {
    missing <- setdiff(items, items_valid)
    warning(sprintf("extract_similarity_submatrix: %d items not found in matrix.", length(missing)))
  }
  sim_matrix[items_valid, items_valid, drop = FALSE]
}

# =================================================================
# 0-B2  SOLUTION CACHE HELPERS
# =================================================================
make_solution_key <- function(vec) {
  paste(which(vec == 1L), collapse = "-")
}

cache_get <- function(cache, key) {
  if (exists(key, envir = cache, inherits = FALSE))
    get(key, envir = cache, inherits = FALSE)
  else
    NULL
}

cache_set <- function(cache, key, value) {
  assign(key, value, envir = cache, inherits = FALSE)
  invisible(NULL)
}

.semantica_history_append <- function(history_env, entry) {
  if (is.null(history_env)) return(invisible(NULL))
  n_hist <- history_env$n + 1L
  history_env$n <- n_hist
  history_env$history[[n_hist]] <- entry
  invisible(NULL)
}

# =================================================================
# 0-B3  LONG-CALCULATION PROGRESS HELPERS
# =================================================================
.semantica_progress_start <- function(total, label = NULL, enabled = TRUE) {
  total <- suppressWarnings(as.integer(total[1L]))
  if (!isTRUE(enabled) || length(total) == 0L || !is.finite(total) || total < 2L) {
    return(NULL)
  }
  if (!is.null(label) && length(label) > 0L && nzchar(as.character(label[1L]))) {
    cat(sprintf("  %s\n", as.character(label[1L])))
  }
  progress_bar <- utils::txtProgressBar(min = 0L, max = total, initial = 0L, style = 3)
  try(utils::flush.console(), silent = TRUE)
  progress_bar
}

.semantica_progress_update <- function(progress_bar, value) {
  if (!is.null(progress_bar)) {
    try(utils::setTxtProgressBar(progress_bar, value), silent = TRUE)
    try(utils::flush.console(), silent = TRUE)
  }
  invisible(NULL)
}

.semantica_progress_close <- function(progress_bar) {
  if (!is.null(progress_bar)) {
    try(close(progress_bar), silent = TRUE)
  }
  invisible(NULL)
}

.semantica_progress_lapply <- function(x, fun, progress = FALSE, label = NULL) {
  out <- vector("list", length(x))
  names(out) <- names(x)
  progress_bar <- .semantica_progress_start(length(x), label, progress)
  on.exit(.semantica_progress_close(progress_bar), add = TRUE)
  for (i in seq_along(x)) {
    out[i] <- list(fun(x[[i]]))
    .semantica_progress_update(progress_bar, i)
  }
  out
}

.semantica_progress_par_lapply <- function(cl, x, fun, progress = FALSE, label = NULL) {
  if (!isTRUE(progress) || length(x) < 2L) {
    return(parallel::parLapplyLB(cl, x, fun))
  }
  out <- vector("list", length(x))
  names(out) <- names(x)
  n_workers <- max(1L, length(cl))
  chunk_size <- max(n_workers, ceiling(length(x) / 8L))
  chunk_ids <- split(seq_along(x), ceiling(seq_along(x) / chunk_size))
  progress_bar <- .semantica_progress_start(length(x), label, TRUE)
  on.exit(.semantica_progress_close(progress_bar), add = TRUE)
  done <- 0L
  for (ids in chunk_ids) {
    out[ids] <- parallel::parLapplyLB(cl, x[ids], fun)
    done <- done + length(ids)
    .semantica_progress_update(progress_bar, done)
  }
  out
}

.semantica_new_dfi_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$entries <- list()
  cache$hits <- 0L
  cache
}

.semantica_dfi_cache_get <- function(cache, target) {
  if (is.null(cache) || is.null(cache$entries) || length(cache$entries) == 0L) return(NULL)
  for (entry in cache$entries) {
    if (identical(entry$target, target)) {
      cache$hits <- as.integer(cache$hits %||% 0L) + 1L
      out <- entry$value
      out$telemetry <- modifyList(
        out$telemetry %||% list(),
        list(cache_hit = TRUE, cache_hits = cache$hits)
      )
      return(out)
    }
  }
  NULL
}

.semantica_dfi_cache_set <- function(cache, target, value) {
  if (!is.null(cache)) {
    cache$entries[[length(cache$entries) + 1L]] <- list(target = target, value = value)
  }
  invisible(value)
}

.semantica_dfi_elapsed <- function(start_time) {
  as.numeric(proc.time()[["elapsed"]] - start_time)
}

.semantica_dfi_fit_telemetry <- function(results) {
  if (length(results) == 0L) {
    return(list(successful_first_attempt = 0L, successful_fallback_attempt = 0L,
                successful_fit_attempts = integer(0L)))
  }
  attempts <- vapply(results, function(x) {
    attempt <- suppressWarnings(as.integer(x$fit_attempt %||% NA_integer_))
    if (length(attempt) == 0L || !is.finite(attempt)) NA_integer_ else attempt[1L]
  }, integer(1L))
  attempt_table <- table(attempts[is.finite(attempts)])
  list(
    successful_first_attempt = sum(attempts == 1L, na.rm = TRUE),
    successful_fallback_attempt = sum(attempts > 1L, na.rm = TRUE),
    successful_fit_attempts = attempt_table
  )
}

.semantica_fast_lavaan_se <- function(estimator) {
  est <- toupper(as.character(estimator %||% "ML")[1L])
  if (identical(est, "ML")) "none" else NULL
}

.semantica_dfi_cutoff_delta <- function(previous, current) {
  metrics <- c("cfi", "tli", "rmsea", "srmr")
  old <- suppressWarnings(as.numeric(unlist(previous[metrics], use.names = FALSE)))
  new <- suppressWarnings(as.numeric(unlist(current[metrics], use.names = FALSE)))
  if (length(old) != length(metrics) || length(new) != length(metrics) ||
      any(!is.finite(old)) || any(!is.finite(new))) {
    return(Inf)
  }
  max(abs(new - old))
}

.semantica_dfi_adaptive_batches <- function(jobs, run_batch, estimate_cutoffs,
                                            enabled = FALSE, min_reps = NULL,
                                            batch_reps = 50L, cutoff_tol = 0.002,
                                            stable_batches = 2L) {
  total <- length(jobs)
  if (!isTRUE(enabled) || total == 0L) {
    return(list(
      results = run_batch(jobs, first_batch = TRUE),
      telemetry = list(
        enabled = FALSE, stopped_early = FALSE,
        requested_reps = total, completed_reps = total
      )
    ))
  }
  min_reps <- suppressWarnings(as.integer(min_reps[1L]))
  if (!is.finite(min_reps) || min_reps < 1L) min_reps <- min(total, max(200L, ceiling(total / 2L)))
  min_reps <- min(total, max(1L, min_reps))
  batch_reps <- suppressWarnings(as.integer(batch_reps[1L]))
  if (!is.finite(batch_reps) || batch_reps < 1L) batch_reps <- 50L
  cutoff_tol <- suppressWarnings(as.numeric(cutoff_tol[1L]))
  if (!is.finite(cutoff_tol) || cutoff_tol <= 0) cutoff_tol <- 0.002
  stable_batches <- suppressWarnings(as.integer(stable_batches[1L]))
  if (!is.finite(stable_batches) || stable_batches < 1L) stable_batches <- 2L

  results <- list()
  previous <- NULL
  stability_runs <- 0L
  checkpoints <- list()
  next_end <- min_reps
  first_batch <- TRUE
  repeat {
    start <- length(results) + 1L
    end <- min(total, next_end)
    if (start <= end) {
      results <- c(results, run_batch(jobs[start:end], first_batch = first_batch))
      first_batch <- FALSE
    }
    estimate <- estimate_cutoffs(results)
    delta <- if (!is.null(previous) && !is.null(estimate)) {
      .semantica_dfi_cutoff_delta(previous, estimate)
    } else {
      Inf
    }
    checkpoints[[length(checkpoints) + 1L]] <- list(
      completed_reps = length(results),
      cutoff_delta = delta,
      cutoffs = estimate
    )
    if (!is.null(estimate)) {
      stability_runs <- if (is.finite(delta) && delta <= cutoff_tol) stability_runs + 1L else 0L
      previous <- estimate
    }
    if (stability_runs >= stable_batches || length(results) >= total) break
    next_end <- min(total, length(results) + batch_reps)
  }
  list(
    results = results,
    telemetry = list(
      enabled = TRUE,
      stopped_early = length(results) < total,
      requested_reps = total,
      completed_reps = length(results),
      min_reps = min_reps,
      batch_reps = batch_reps,
      cutoff_tol = cutoff_tol,
      stable_batches = stable_batches,
      checkpoints = checkpoints
    )
  )
}

.semantica_make_dfi_cluster <- function(n_cores) {
  plan <- semantica_resource_plan(
    n.cores = n_cores,
    use_parallel = TRUE,
    reserve.cores = 0L
  )
  if (plan$effective_workers <= 1L) return(NULL)
  cl <- .semantica_make_cluster(plan)
  initialized <- FALSE
  on.exit({
    if (!initialized) .semantica_stop_cluster(cl)
  }, add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(lavaan)
      library(Matrix)
    })
  })
  initialized <- TRUE
  cl
}

# =================================================================
# 0-C  DFI POPULATION SYNTAX & SIMULATION FALLBACK
# =================================================================
build_population_syntax_modelbased <- function(items_per_factor,
                                               fitted_loadings    = NULL,
                                               fitted_factor_cors = NULL,
                                               loading_pattern    = "varied",
                                               mean_loading       = 0.70,
                                               target_factor_cors = NULL,
                                               embed_reliability  = 1.0,
                                               residual_inflation = 0.0,
                                               syntax_mode        = c("dfi_package", "simulation")) {
  syntax_mode <- match.arg(syntax_mode)
  counts    <- as.integer(items_per_factor)
  n_factors <- length(counts)
  fnames      <- paste0("F", seq_len(n_factors))
  lookup_keys <- if (!is.null(names(items_per_factor))) names(items_per_factor) else fnames
  lines        <- character(0)
  item_counter <- 1L

  for (j in seq_len(n_factors)) {
    n_items    <- counts[j]
    items      <- paste0("x", item_counter:(item_counter + n_items - 1L))
    item_counter <- item_counter + n_items
    f_name     <- fnames[j]
    f_key      <- lookup_keys[j]

    if (!is.null(fitted_loadings) && !is.null(fitted_loadings[[f_key]])) {
      raw_loads  <- as.numeric(fitted_loadings[[f_key]])
      if (length(raw_loads)  < n_items) raw_loads  <- rep_len(raw_loads, n_items)
      else if (length(raw_loads)  > n_items) raw_loads  <- raw_loads[seq_len(n_items)]
    } else {
      raw_loads  <- switch(loading_pattern,
                           "uniform" = rep(mean_loading, n_items),
                           "strong_anchor" = if (n_items == 1L) mean_loading else c(mean_loading + 0.10, rep(mean_loading - 0.05, n_items - 1L)),
                           "varied" = if (n_items == 1L) mean_loading else if (n_items == 2L) c(mean_loading + 0.08, mean_loading - 0.05) else c(mean_loading + 0.12, rep(mean_loading, n_items - 2L), mean_loading - 0.08)
      )
    }

    if (embed_reliability < 1.0) {
      rho_tt_safe <- max(0.50, embed_reliability)
      if (rho_tt_safe != embed_reliability)
        message(sprintf("[Fix B] embed_reliability=%.2f below floor 0.50 -- clamped to 0.50.", embed_reliability))
      raw_loads <- raw_loads * sqrt(rho_tt_safe)
    }

    raw_loads <- pmax(0.35, pmin(0.95, raw_loads))
    terms  <- paste(sprintf("%.3f*%s", raw_loads, items), collapse = " + ")
    lines  <- c(lines, paste0(f_name, " =~ ", terms))

    if (syntax_mode == "simulation") {
      for (k in seq_along(items)) {
        theta <- max(0.01, (1 - raw_loads[k]^2) + residual_inflation)
        lines <- c(lines, sprintf("%s ~~ %.4f*%s", items[k], theta, items[k]))
      }
      lines <- c(lines, sprintf("%s ~~ 1*%s", f_name, f_name))
    }
  }

  if (n_factors >= 2L) {
    fc_mat <- if (!is.null(fitted_factor_cors) && is.matrix(fitted_factor_cors) && nrow(fitted_factor_cors) == n_factors) {
      m <- fitted_factor_cors; m[m > 0.90] <- 0.90; m[m < -0.90] <- -0.90; diag(m) <- 1.0; m
    } else if (!is.null(target_factor_cors)) {
      if (is.numeric(target_factor_cors) && length(target_factor_cors) == 1L) {
        mc <- matrix(target_factor_cors, n_factors, n_factors); diag(mc) <- 1.0; mc
      } else as.matrix(target_factor_cors)
    } else {
      mc <- matrix(0.30, n_factors, n_factors); diag(mc) <- 1.0; mc
    }
    for (a in seq_len(n_factors - 1L)) {
      for (b in (a + 1L):n_factors) {
        r <- fc_mat[a, b]
        lines <- c(lines, sprintf("%s ~~ %.3f*%s", fnames[a], r, fnames[b]))
      }
    }
  }
  paste(lines, collapse = "\n")
}

compute_dfi_by_simulation <- function(factors, items_per_factor, n_obs = 1000,
                                      fitted_loadings = NULL, fitted_factor_cors = NULL,
                                      loading_pattern = "varied", mean_loading = 0.70,
                                      target_factor_cors = NULL, embed_reliability = 1.0,
                                      residual_inflation = 0.0, reps = 500,
                                      estimator = "ML", n_cores = 1, verbose = TRUE,
                                      progress = verbose) {
  start_time <- proc.time()[["elapsed"]]
  n_cores <- .semantica_max_workers(n_cores)
  pop_model <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors,
                                                  loading_pattern, mean_loading, target_factor_cors,
                                                  embed_reliability, residual_inflation, "simulation")
  fit_syntax <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors,
                                                   loading_pattern, mean_loading, target_factor_cors,
                                                   embed_reliability, 0.0, "dfi_package")
  if (verbose) cat("\n[DFI-SIM] Population model for simulation:\n", pop_model, "\n\n")
  with_task_seed <- .semantica_with_task_seed
  lavaan_se <- .semantica_fast_lavaan_se(estimator)

  single_rep <- function(seed) {
    with_task_seed(seed, {
      dat <- tryCatch(lavaan::simulateData(pop_model, sample.nobs = n_obs), error = function(e) NULL)
      if (is.null(dat)) return(NULL)
      fit_args <- list(model = fit_syntax, data = dat, std.lv = TRUE, estimator = estimator)
      if (!is.null(lavaan_se)) fit_args$se <- lavaan_se
      fit <- tryCatch(
        suppressWarnings(do.call(lavaan::cfa, fit_args)),
        error = function(e) NULL
      )
      if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) return(NULL)
      fm <- tryCatch(lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")), error = function(e) NULL)
      if (is.null(fm)) return(NULL)
      list(cfi = as.numeric(fm["cfi"]), tli = as.numeric(fm["tli"]),
           rmsea = as.numeric(fm["rmsea"]), srmr = as.numeric(fm["srmr"]))
    })
  }

  seeds <- sample.int(.Machine$integer.max, reps)
  results <- vector("list", reps)

  if (n_cores > 1L) {
    cl <- .semantica_make_dfi_cluster(n_cores)
    on.exit(.semantica_stop_cluster(cl), add = TRUE)
    results <- .semantica_progress_par_lapply(
      cl, seeds, single_rep,
      progress = progress,
      label = "[DFI-SIM] Fallback CFA simulation refits"
    )
  } else {
    results <- .semantica_progress_lapply(
      seeds, single_rep,
      progress = progress,
      label = "[DFI-SIM] Fallback CFA simulation refits"
    )
  }

  good <- Filter(Negate(is.null), results)
  if (length(good) == 0L) {
    if (verbose) message("[DFI-SIM] no successful fits in simulation.")
    return(NULL)
  }

  cfi_v <- vapply(good, `[[`, numeric(1), "cfi")
  tli_v <- vapply(good, `[[`, numeric(1), "tli")
  rmsea_v <- vapply(good, `[[`, numeric(1), "rmsea")
  srmr_v <- vapply(good, `[[`, numeric(1), "srmr")

  # Higher-is-better fit indices need a lower-tail cutoff so most data
  # generated from the target model pass; RMSEA/SRMR use the upper tail.
  cfi_cut <- as.numeric(quantile(cfi_v, probs = 0.05, na.rm = TRUE))
  tli_cut <- as.numeric(quantile(tli_v, probs = 0.05, na.rm = TRUE))
  rmsea_cut <- as.numeric(quantile(rmsea_v, probs = 0.95, na.rm = TRUE))
  srmr_cut <- as.numeric(quantile(srmr_v, probs = 0.95, na.rm = TRUE))

  if (verbose) {
    cat(sprintf("[DFI-SIM] successful fits = %d / %d\n", length(good), reps))
    cat(sprintf("[DFI-SIM] cutoffs: CFI >= %.4f, TLI >= %.4f, RMSEA <= %.4f, SRMR <= %.4f\n",
                cfi_cut, tli_cut, rmsea_cut, srmr_cut))
  }
  list(
    cfi = cfi_cut, tli = tli_cut, rmsea = rmsea_cut, srmr = srmr_cut,
    was_degenerate = FALSE,
    telemetry = list(
      elapsed_seconds = unname(proc.time()[["elapsed"]] - start_time),
      cache_hit = FALSE,
      requested_reps = reps,
      completed_reps = length(results),
      successful_fits = length(good),
      failed_fits = max(0L, length(results) - length(good)),
      parallel_workers = n_cores,
      task_seeds = seeds
    )
  )
}

# =================================================================
# 0-C-ALT & 0-C-MAIN  SAFE DFI & MAIN ENTRY
# =================================================================
select_dfi_function <- function(data_type, is_one_factor) {
  switch(data_type,
         "continuous"  = if (is_one_factor) "cfaOne" else "cfaHB",
         "categorical" = if (is_one_factor) "catOne" else "catHB",
         "likert"      = if (is_one_factor) "catOne" else "catHB",
         "nonnormal"   = if (is_one_factor) "cfaOne" else "cfaHB",
         if (is_one_factor) "cfaOne" else "cfaHB")
}

extract_cutoffs_from_dfi_result <- function(dfi, level, criterion, verbose, n_factors = 1L, items_per_factor = 3L) {
  tab <- dfi$cutoffs
  if (is.null(tab)) { if (verbose) message("DFI: cutoffs table is NULL."); return(NULL) }
  cn <- colnames(tab); rn <- rownames(tab)
  if (!all(c("SRMR", "RMSEA", "CFI") %in% cn)) { if (verbose) message("DFI: unexpected columns."); return(NULL) }

  is_decimal_cell <- function(x) {
    if (is.null(x) || length(x) == 0L) return(FALSE)
    s <- trimws(as.character(x))
    if (nchar(s) == 0L || s == "NA") return(FALSE)
    if (grepl("%", s, fixed = TRUE)) return(FALSE)
    v <- suppressWarnings(as.numeric(s))
    if (is.na(v) || !is.finite(v)) return(FALSE)
    v > 0 && v < 2
  }

  row_has_decimal_cutoffs <- function(ri) {
    is_decimal_cell(tab[ri, "CFI"]) && is_decimal_cell(tab[ri, "RMSEA"]) && is_decimal_cell(tab[ri, "SRMR"])
  }
  decimal_rows <- which(vapply(seq_len(nrow(tab)), row_has_decimal_cutoffs, logical(1L)))

  if (length(decimal_rows) == 0L) return(NULL)

  lvl_name <- paste0("Level-", level)
  target_rows <- which(tolower(trimws(rn)) == tolower(lvl_name))
  if (length(target_rows) > 0L) {
    idx <- target_rows[1L]
    if (!row_has_decimal_cutoffs(idx)) {
      # If dynamic reports NONE for the requested level, falling back to a
      # different row would silently change the user's decision rule.
      if (verbose) message("DFI: requested ", lvl_name, " cutoffs are unavailable; using fallback calibration.")
      return(NULL)
    }
  } else {
    idx <- decimal_rows[1L]
    if (verbose) message("DFI: requested ", lvl_name, " row not found; using first available decimal cutoff row.")
  }

  parse_decimal <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
  cfi <- parse_decimal(tab[idx, "CFI"])
  rmsea <- parse_decimal(tab[idx, "RMSEA"])
  srmr <- parse_decimal(tab[idx, "SRMR"])
  tli <- if ("TLI" %in% cn) parse_decimal(tab[idx, "TLI"]) else NA_real_

  rescale_if_needed <- function(v, is_cfi = FALSE) { if (is.na(v) || !is.finite(v)) return(v); if (v > 2) v / 100 else v }
  cfi <- rescale_if_needed(cfi, TRUE); rmsea <- rescale_if_needed(rmsea); srmr <- rescale_if_needed(srmr); tli <- rescale_if_needed(tli, TRUE)

  if (any(!is.finite(c(srmr, rmsea, cfi)))) return(NULL)

  degenerate_flags <- character(0)
  if (!is.na(rmsea) && rmsea > 0.25) degenerate_flags <- c(degenerate_flags, sprintf("RMSEA = %.4f", rmsea))
  if (!is.na(srmr)  && srmr  > 0.20) degenerate_flags <- c(degenerate_flags, sprintf("SRMR = %.4f", srmr))
  if (!is.na(cfi)   && cfi   < 0.80) degenerate_flags <- c(degenerate_flags, sprintf("CFI = %.4f", cfi))

  if (length(degenerate_flags) > 0L) {
    ipf_min <- min(as.integer(items_per_factor))
    if (!is.na(rmsea) && rmsea > 0.25) rmsea <- if (ipf_min <= 3L) 0.080 else if (n_factors >= 5L) 0.065 else 0.070
    if (!is.na(srmr)  && srmr  > 0.20) srmr  <- if (ipf_min <= 3L) 0.080 else 0.065
    if (!is.na(cfi)   && cfi   < 0.80) cfi   <- if (n_factors >= 5L) 0.940 else 0.950
    if (!is.na(tli)) tli <- cfi - 0.010
  }

  list(cfi = cfi, tli = if (!is.na(tli)) tli else cfi - 0.01, rmsea = rmsea, srmr = srmr,
       chosen_row = rn[idx], level = level, criterion = criterion, was_degenerate = length(degenerate_flags) > 0L)
}

safe_compute_dfi <- function(model_syntax, factors, items_per_factor, n_obs = 1000, fitted_loadings = NULL,
                             fitted_factor_cors = NULL, loading_pattern = "varied", mean_loading = 0.70,
                             target_factor_cors = NULL, embed_reliability = 1.0, residual_inflation = 0.0,
                             data_type = "continuous", estimator = NULL, reps = 500, level = 1,
                             criterion = "Sensitivity", sim_reps = 500, sim_cores = 2L,
                             verbose = TRUE) {
  sim_cores <- .semantica_max_workers(sim_cores)
  if (is.null(estimator)) estimator <- switch(data_type, "continuous" = "ML", "categorical" = "WLSMV", "likert" = "ML", "nonnormal" = "MLR")
  dfi_fn <- select_dfi_function(data_type, length(factors) == 1L)
  if (verbose) {
    message(sprintf(
      "[DFI] Strict-CFA fallback uses dynamic::%s; that engine does not expose per-rep progress, so a progress bar is only available if SEMANTICA switches to its simulation fallback.",
      dfi_fn
    ))
  }

  dynamic_optional <- function(name, ...) {
    fn <- tryCatch(getExportedValue("dynamic", name), error = function(e) NULL)
    if (is.null(fn)) {
      if (verbose) {
        message(sprintf(
          "[dynamic] %s is unavailable in the installed 'dynamic' package; using SEMANTICA's existing simulation fallback.",
          name
        ))
      }
      return(NULL)
    }
    fn(...)
  }

  dyn_out <- tryCatch({
    withCallingHandlers({
      switch(dfi_fn,
             "cfaHB"  = dynamic::cfaHB(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = estimator),
             "cfaOne" = dynamic::cfaOne(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = estimator),
             "catHB"  = dynamic_optional("catHB", model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = "WLSMV"),
             "catOne" = dynamic_optional("catOne", model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = "WLSMV"),
             stop("Unsupported dfi function: ", dfi_fn))
    }, warning = function(w) invokeRestart("muffleWarning"))
  }, error = function(e) { if (verbose) message("[dynamic] error: ", conditionMessage(e)); NULL })

  if (!is.null(dyn_out) && !is.null(dyn_out$cutoffs)) {
    cut <- tryCatch(extract_cutoffs_from_dfi_result(dyn_out, level, criterion, verbose, n_factors = length(factors), items_per_factor = items_per_factor), error = function(e) NULL)
    if (!is.null(cut)) { cut$dfi_function <- dfi_fn; cut$data_type <- data_type; cut$model_syntax <- model_syntax; cut$n_obs <- n_obs; return(cut) }
  }

  if (verbose) message("[dynamic] failed or returned NULL; running sim-based fallback...")
  sim_cut <- compute_dfi_by_simulation(factors, items_per_factor, n_obs, fitted_loadings, fitted_factor_cors,
                                       loading_pattern, mean_loading, target_factor_cors, embed_reliability,
                                       residual_inflation, sim_reps, estimator, sim_cores, verbose)
  if (!is.null(sim_cut)) { sim_cut$dfi_function <- "simulateData+lavaan::cfa"; sim_cut$data_type <- data_type; sim_cut$model_syntax <- model_syntax; sim_cut$n_obs <- n_obs; return(sim_cut) }

  if (verbose) message("[DFI-SAFE] Simulation fallback failed -- using heuristic cutoffs.")
  compute_heuristic_cutoffs(length(factors), items_per_factor, n_obs)
}

compute_dfi_cutoffs_from_model_spec <- function(factors, items_per_factor, n_obs = 1000, fitted_loadings = NULL,
                                                fitted_factor_cors = NULL, loading_pattern = "varied", mean_loading = 0.70,
                                                target_factor_cors = NULL, embed_reliability = 1.0, residual_inflation = 0.0,
                                                data_type = c("continuous", "categorical", "likert", "nonnormal"),
                                                original_data = NULL, estimator = NULL, reps = 500, level = 1,
                                                criterion = c("Sensitivity", "Specificity"), verbose = TRUE,
                                                loading_source_label = NULL, n_cores = 2L) {
  criterion <- match.arg(criterion); data_type <- match.arg(data_type)
  if (data_type %in% c("likert", "nonnormal") && is.null(original_data)) { if (verbose) message("DFI: falling back to 'continuous'."); data_type <- "continuous" }
  if (is.null(estimator)) estimator <- switch(data_type, "continuous" = "ML", "categorical" = "WLSMV", "likert" = "ML", "nonnormal" = "MLR")

  n_factors <- length(factors)
  using_fitted <- !is.null(fitted_loadings)
  source_label <- loading_source_label %||% if (using_fitted) "ESEM-fitted" else "prior-based"
  fix_b_active <- embed_reliability < 1.0 || residual_inflation > 0.0

  if (verbose) {
    cat("\n============================================================\n COMPUTING DFI CUTOFFS -- SEMANTICA\n")
    cat(sprintf("  Factors          : %d\n  Items per factor : %s\n  Sample size (N)  : %d\n", n_factors, paste(names(items_per_factor), items_per_factor, sep="=", collapse=", "), n_obs))
    cat(sprintf("  Loading source   : %s\n", source_label))
    cat(sprintf("  Data type        : %s | Estimator: %s | Reps: %d\n\n", data_type, estimator, reps))
  }

  model_syntax <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors, loading_pattern, mean_loading, target_factor_cors, embed_reliability, 0.0, "dfi_package")

  cutoffs <- safe_compute_dfi(model_syntax, factors, items_per_factor, n_obs, fitted_loadings, fitted_factor_cors, loading_pattern, mean_loading, target_factor_cors, embed_reliability, residual_inflation, data_type, estimator, reps, level, criterion, max(200L, min(1000L, reps * 2L)), n_cores, verbose)

  if (!is.null(cutoffs)) {
    if (is.null(cutoffs$dfi_function))  cutoffs$dfi_function <- "safe_compute_dfi"
    if (is.null(cutoffs$data_type))     cutoffs$data_type    <- data_type
    cutoffs$loading_source <- source_label
    cutoffs$loading_pattern <- loading_pattern; cutoffs$mean_loading <- mean_loading
    cutoffs$embed_reliability <- embed_reliability; cutoffs$residual_inflation <- residual_inflation
    cutoffs$fix_b_active <- fix_b_active
  }
  cutoffs
}

# =================================================================
# 0-C-ESEM  ESEM-PARAMETRIC DFI CALIBRATION
# =================================================================
stabilize_correlation_matrix <- function(x, min_eigen = 1e-5) {
  if (!is.matrix(x)) x <- as.matrix(x)
  nms <- dimnames(x)
  x <- (x + t(x)) / 2
  d <- sqrt(pmax(diag(x), .Machine$double.eps))
  x <- x / tcrossprod(d)
  x[row(x) != col(x)] <- pmin(pmax(x[row(x) != col(x)], -0.999), 0.999)
  diag(x) <- 1

  ev <- eigen(x, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev, na.rm = TRUE) < min_eigen) {
    x <- tryCatch(
      as.matrix(Matrix::nearPD(x, corr = TRUE, keepDiag = TRUE, maxit = 1000)$mat),
      error = function(e) {
        eig <- eigen(x, symmetric = TRUE)
        eig$values <- pmax(eig$values, min_eigen)
        y <- eig$vectors %*% diag(eig$values, length(eig$values)) %*% t(eig$vectors)
        y / tcrossprod(sqrt(pmax(diag(y), .Machine$double.eps)))
      }
    )
    diag(x) <- 1
  }
  dimnames(x) <- nms
  x
}

sample_correlation_from_population <- function(pop_cor, n_obs) {
  p <- nrow(pop_cor)
  if (p < 2L || n_obs <= p + 2L) return(NULL)
  w <- tryCatch(stats::rWishart(1L, df = n_obs - 1L, Sigma = pop_cor)[, , 1L] / (n_obs - 1L),
                error = function(e) NULL)
  if (is.null(w)) return(NULL)
  stabilize_correlation_matrix(w)
}

dfi_tail_probability <- function(level = 1, criterion = "Sensitivity") {
  # Dynamic-fit logic is percentile based: retain most samples generated from
  # the target population model. Level 1 uses the conventional 95% retention;
  # higher levels trade some sensitivity for a less severe target-model screen.
  lvl <- suppressWarnings(as.integer(level))
  if (!is.finite(lvl) || lvl < 1L) lvl <- 1L
  base <- switch(as.character(min(lvl, 3L)), "1" = 0.05, "2" = 0.10, "3" = 0.15, 0.05)
  if (identical(tolower(criterion), "specificity")) base <- max(base, 0.10)
  base
}

make_semantic_approx_population <- function(esem_fit, observed_cor,
                                            residual_cap_quantile = 0.95,
                                            embed_reliability = 1.0,
                                            verbose = TRUE) {
  if (is.null(esem_fit) || is.null(observed_cor)) return(NULL)

  pop_cov <- tryCatch(lavaan::fitted(esem_fit)$cov, error = function(e) NULL)
  if (is.null(pop_cov) || !is.matrix(pop_cov) || any(!is.finite(pop_cov))) {
    if (verbose) message("[SEMANTIC-DFI] Could not extract model-implied covariance.")
    return(NULL)
  }
  pop_cor <- stabilize_correlation_matrix(pop_cov)
  item_names <- rownames(pop_cor)
  if (is.null(item_names) || length(item_names) < 2L) return(NULL)

  observed_cor <- tryCatch(stabilize_correlation_matrix(observed_cor), error = function(e) NULL)
  if (is.null(observed_cor)) return(NULL)
  common <- intersect(item_names, rownames(observed_cor))
  common <- intersect(common, colnames(observed_cor))
  if (length(common) != length(item_names)) {
    if (verbose) message("[SEMANTIC-DFI] Observed and implied matrices do not align.")
    return(NULL)
  }
  pop_cor <- pop_cor[item_names, item_names, drop = FALSE]
  observed_cor <- observed_cor[item_names, item_names, drop = FALSE]

  residual <- observed_cor - pop_cor
  diag(residual) <- 0
  off <- lower.tri(residual) | upper.tri(residual)
  residual_vals <- abs(residual[off])
  residual_vals <- residual_vals[is.finite(residual_vals)]
  if (length(residual_vals) == 0L) return(NULL)

  residual_cap_quantile <- as.numeric(residual_cap_quantile)
  if (!is.finite(residual_cap_quantile) || residual_cap_quantile <= 0 || residual_cap_quantile >= 1) {
    residual_cap_quantile <- 0.95
  }
  resid_cap <- as.numeric(stats::quantile(residual_vals, residual_cap_quantile, na.rm = TRUE, names = FALSE))
  if (is.finite(resid_cap) && resid_cap > 0) {
    residual[off] <- sign(residual[off]) * pmin(abs(residual[off]), resid_cap)
  }

  reliability <- suppressWarnings(as.numeric(embed_reliability))
  if (!is.finite(reliability)) reliability <- 1.0
  reliability <- min(1.0, max(0.50, reliability))

  approx_cor <- pop_cor + residual
  if (reliability < 1.0) {
    # Optional attenuation follows the existing SEMANTICA proxy-reliability
    # logic: a less reliable embedding proxy should not imply stronger
    # population correlations than its semantic source can support.
    approx_cor[off] <- approx_cor[off] * sqrt(reliability)
  }
  approx_cor <- stabilize_correlation_matrix(approx_cor)
  dimnames(approx_cor) <- list(item_names, item_names)

  list(
    pop_cor = pop_cor,
    observed_cor = observed_cor,
    approx_cor = approx_cor,
    residual = residual,
    residual_vals = residual_vals,
    residual_cap_quantile = residual_cap_quantile,
    residual_q95 = as.numeric(stats::quantile(residual_vals, 0.95, na.rm = TRUE, names = FALSE)),
    residual_max = max(residual_vals, na.rm = TRUE),
    embed_reliability = reliability,
    item_names = item_names
  )
}

make_semantic_misspecified_population <- function(acceptable_cor, observed_cor = NULL,
                                                  factor_assignment = NULL, factors = NULL,
                                                  strength = 1.0) {
  if (is.null(acceptable_cor)) return(NULL)
  m <- as.matrix(acceptable_cor)
  item_names <- rownames(m)
  if (is.null(item_names) || length(item_names) < 2L) return(NULL)
  off <- row(m) != col(m)

  strength <- suppressWarnings(as.numeric(strength))
  if (!is.finite(strength) || strength <= 0) strength <- 1.0
  strength <- min(2.0, strength)

  if (!is.null(factor_assignment) && !is.null(factors)) {
    fa <- as.character(factor_assignment[item_names])
    same <- outer(fa, fa, "==") & off & !is.na(outer(fa, fa, "=="))
    between <- (!same) & off
    # Misspecification alternative: slightly weaker monotrait covariance and
    # stronger heterotrait covariance mimic weak indicators and factor blurring.
    m[same] <- m[same] * (1 - 0.10 * strength)
    m[between] <- m[between] + 0.04 * strength * (1 - abs(m[between]))
  }

  source_cor <- if (!is.null(observed_cor)) as.matrix(observed_cor) else m
  if (!is.null(rownames(source_cor)) && all(item_names %in% rownames(source_cor))) {
    source_cor <- source_cor[item_names, item_names, drop = FALSE]
  }
  pair_strength <- abs(source_cor)
  pair_vals <- pair_strength[lower.tri(pair_strength)]
  pair_vals <- pair_vals[is.finite(pair_vals)]
  if (length(pair_vals) > 0L) {
    top_cut <- as.numeric(stats::quantile(pair_vals, 0.95, na.rm = TRUE, names = FALSE))
    local_pairs <- off & pair_strength >= top_cut
    # Pair-specific residual covariance approximates wording/local-dependence
    # misspecification, which global ESEM loadings should not fully absorb.
    m[local_pairs] <- m[local_pairs] + 0.07 * strength * (1 - abs(m[local_pairs]))
  }

  m[off] <- pmin(pmax(m[off], -0.95), 0.95)
  diag(m) <- 1
  stabilize_correlation_matrix(m)
}

select_roc_cutoff <- function(good, bad, higher_is_better = TRUE,
                              tail_prob = 0.05,
                              criterion = "Sensitivity") {
  good <- good[is.finite(good)]
  bad <- bad[is.finite(bad)]
  if (length(good) == 0L) return(list(cutoff = NA_real_, sensitivity = NA_real_, specificity = NA_real_))
  fallback <- if (higher_is_better) {
    as.numeric(stats::quantile(good, probs = tail_prob, na.rm = TRUE, names = FALSE))
  } else {
    as.numeric(stats::quantile(good, probs = 1 - tail_prob, na.rm = TRUE, names = FALSE))
  }
  if (length(bad) == 0L) return(list(cutoff = fallback, sensitivity = 1 - tail_prob, specificity = NA_real_))

  candidates <- sort(unique(c(good, bad, fallback)))
  sensitivity <- if (higher_is_better) {
    vapply(candidates, function(cut) mean(good >= cut, na.rm = TRUE), numeric(1L))
  } else {
    vapply(candidates, function(cut) mean(good <= cut, na.rm = TRUE), numeric(1L))
  }
  specificity <- if (higher_is_better) {
    vapply(candidates, function(cut) mean(bad < cut, na.rm = TRUE), numeric(1L))
  } else {
    vapply(candidates, function(cut) mean(bad > cut, na.rm = TRUE), numeric(1L))
  }

  target <- 1 - tail_prob
  crit <- tolower(criterion)
  if (identical(crit, "specificity")) {
    ok <- specificity >= target
    score <- sensitivity
  } else if (identical(crit, "sensitivity")) {
    ok <- sensitivity >= target
    score <- specificity
  } else {
    ok <- rep(TRUE, length(candidates))
    score <- sensitivity + specificity - 1
  }
  if (!any(ok, na.rm = TRUE)) {
    ok <- rep(TRUE, length(candidates))
    score <- sensitivity + specificity - 1
  }
  idx_pool <- which(ok)
  score_pool <- score[idx_pool]
  best_score <- max(score_pool, na.rm = TRUE)
  idx_pool <- idx_pool[score_pool >= best_score - 1e-12]
  idx <- if (higher_is_better) idx_pool[which.max(candidates[idx_pool])] else idx_pool[which.min(candidates[idx_pool])]
  list(cutoff = candidates[idx], sensitivity = sensitivity[idx], specificity = specificity[idx])
}

compute_esem_parametric_dfi_cutoffs <- function(esem_fit, esem_syntax, factors, items_per_factor,
                                                n_obs = 1000, estimator = "ML",
                                                rotation = "geomin",
                                                rotation_args = list(geomin.epsilon = 0.50),
                                                reps = 100L, level = 1,
                                                criterion = "Sensitivity",
                                                n_cores = 1L, iter_max = 1000L,
                                                verbose = TRUE,
                                                progress = verbose,
                                                cache = NULL, cluster = NULL,
                                                adaptive = FALSE, adaptive_min_reps = NULL,
                                                adaptive_batch_reps = 50L,
                                                adaptive_tol = 0.002,
                                                adaptive_stable_batches = 2L) {
  if (is.null(esem_fit) || is.null(esem_syntax)) return(NULL)
  start_time <- proc.time()[["elapsed"]]
  reps <- max(20L, as.integer(reps))
  n_cores <- .semantica_max_workers(n_cores)

  pop_cov <- tryCatch(lavaan::fitted(esem_fit)$cov, error = function(e) NULL)
  if (is.null(pop_cov) || !is.matrix(pop_cov) || any(!is.finite(pop_cov))) {
    if (verbose) message("[ESEM-DFI] Could not extract model-implied covariance.")
    return(NULL)
  }
  pop_cor <- stabilize_correlation_matrix(pop_cov)
  item_names <- rownames(pop_cor)
  if (is.null(item_names) || length(item_names) < 2L) return(NULL)

  tail_prob <- dfi_tail_probability(level, criterion)
  cache_target <- list(
    kind = "esem_parametric",
    pop_cor = pop_cor,
    syntax = esem_syntax,
    factors = factors,
    items_per_factor = items_per_factor,
    n_obs = n_obs,
    estimator = estimator,
    rotation = rotation,
    rotation_args = rotation_args,
    reps = reps,
    level = level,
    criterion = criterion,
    iter_max = iter_max,
    adaptive = isTRUE(adaptive),
    adaptive_min_reps = adaptive_min_reps,
    adaptive_batch_reps = adaptive_batch_reps,
    adaptive_tol = adaptive_tol,
    adaptive_stable_batches = adaptive_stable_batches
  )
  cached <- .semantica_dfi_cache_get(cache, cache_target)
  if (!is.null(cached)) {
    if (verbose) cat("[ESEM-DFI] Reusing identical cached DFI calibration.\n")
    return(cached)
  }
  seeds <- sample.int(.Machine$integer.max, reps)
  with_task_seed <- .semantica_with_task_seed

  single_rep <- function(seed) {
    with_task_seed(seed, {
      sim_cor <- sample_correlation_from_population(pop_cor, n_obs)
      if (is.null(sim_cor)) return(NULL)
      dimnames(sim_cor) <- list(item_names, item_names)
      fit <- run_esem_on_matrix(
        esem_syntax, sim_cor, n_obs = n_obs, estimator = estimator,
        rotation = rotation, rotation_args = rotation_args,
        iter_max = iter_max, fallback = TRUE,
        sample_cov_rescale = TRUE
      )
      if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) return(NULL)
      fm <- tryCatch(lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")),
                     error = function(e) NULL)
      if (is.null(fm) || any(!is.finite(fm))) return(NULL)
      list(cfi = as.numeric(fm["cfi"]), tli = as.numeric(fm["tli"]),
           rmsea = as.numeric(fm["rmsea"]), srmr = as.numeric(fm["srmr"]),
           fit_attempt = attr(fit, "semantica_fit_attempt") %||% NA_integer_)
    })
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat(" COMPUTING DFI CUTOFFS -- ESEM-PARAMETRIC SEMANTIC PROXY\n")
    cat(sprintf("  Factors          : %d\n", length(factors)))
    cat(sprintf("  Items per factor : %s\n", paste(names(items_per_factor), items_per_factor, sep = "=", collapse = ", ")))
    cat(sprintf("  Sample size (N)  : %d | Reps: %d | Tail: %.2f\n", n_obs, reps, tail_prob))
    cat(sprintf("  Rotation         : %s | Estimator: %s\n\n", rotation, estimator))
  }

  cl <- if (is.function(cluster)) cluster() else cluster
  owns_cluster <- FALSE
  if (is.null(cl) && n_cores > 1L) {
    cl <- .semantica_make_dfi_cluster(n_cores)
    owns_cluster <- !is.null(cl)
    if (owns_cluster) on.exit(.semantica_stop_cluster(cl), add = TRUE)
  }
  if (!is.null(cl)) {
    n_cores <- max(1L, length(cl))
    export_env <- new.env(parent = emptyenv())
    export_env$pop_cor <- pop_cor
    export_env$item_names <- item_names
    export_env$n_obs <- n_obs
    export_env$estimator <- estimator
    export_env$rotation <- rotation
    export_env$rotation_args <- rotation_args
    export_env$iter_max <- iter_max
    export_env$esem_syntax <- esem_syntax
    export_env$sample_correlation_from_population <- sample_correlation_from_population
    export_env$stabilize_correlation_matrix <- stabilize_correlation_matrix
    export_env$run_esem_on_matrix <- run_esem_on_matrix
    for (fn in c(
      "%||%", ".semantica_fast_lavaan_se",
      "is_admissible_esem_fit", ".semantica_assess_esem_fit",
      "assess_esem_admissibility", ".semantica_safe_lav_inspect",
      ".semantica_collect_numeric", ".semantica_numeric_matrix"
    )) {
      if (exists(fn, mode = "function")) export_env[[fn]] <- get(fn)
    }
    .semantica_cluster_export_environment(cl, export_env)
  }
  run_batch <- function(batch, first_batch = FALSE) {
    label <- if (isTRUE(first_batch)) "[ESEM-DFI] Parametric semantic-proxy refits" else NULL
    if (!is.null(cl)) {
      .semantica_progress_par_lapply(cl, batch, single_rep, progress = progress, label = label)
    } else {
      .semantica_progress_lapply(batch, single_rep, progress = progress, label = label)
    }
  }
  estimate_cutoffs <- function(batch_results) {
    batch_good <- Filter(Negate(is.null), batch_results)
    if (length(batch_good) < 20L) return(NULL)
    list(
      cfi = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "cfi"),
                                       probs = tail_prob, na.rm = TRUE, names = FALSE)),
      tli = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "tli"),
                                       probs = tail_prob, na.rm = TRUE, names = FALSE)),
      rmsea = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "rmsea"),
                                         probs = 1 - tail_prob, na.rm = TRUE, names = FALSE)),
      srmr = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "srmr"),
                                        probs = 1 - tail_prob, na.rm = TRUE, names = FALSE))
    )
  }
  run <- .semantica_dfi_adaptive_batches(
    seeds, run_batch, estimate_cutoffs,
    enabled = adaptive,
    min_reps = adaptive_min_reps,
    batch_reps = adaptive_batch_reps,
    cutoff_tol = adaptive_tol,
    stable_batches = adaptive_stable_batches
  )
  results <- run$results

  good <- Filter(Negate(is.null), results)
  min_success <- max(20L, ceiling(length(results) * 0.40))
  if (length(good) < min_success) {
    if (verbose) {
      message(sprintf("[ESEM-DFI] Only %d/%d successful fits; falling back.", length(good), reps))
    }
    return(NULL)
  }

  cfi_v <- vapply(good, `[[`, numeric(1L), "cfi")
  tli_v <- vapply(good, `[[`, numeric(1L), "tli")
  rmsea_v <- vapply(good, `[[`, numeric(1L), "rmsea")
  srmr_v <- vapply(good, `[[`, numeric(1L), "srmr")

  cut <- list(
    cfi = as.numeric(stats::quantile(cfi_v, probs = tail_prob, na.rm = TRUE, names = FALSE)),
    tli = as.numeric(stats::quantile(tli_v, probs = tail_prob, na.rm = TRUE, names = FALSE)),
    rmsea = as.numeric(stats::quantile(rmsea_v, probs = 1 - tail_prob, na.rm = TRUE, names = FALSE)),
    srmr = as.numeric(stats::quantile(srmr_v, probs = 1 - tail_prob, na.rm = TRUE, names = FALSE)),
    was_degenerate = FALSE,
    dfi_function = "ESEM-parametric semantic-proxy simulation",
    data_type = "semantic_proxy_continuous",
    loading_source = "ESEM-implied covariance",
    cutoff_calibration = "ESEM-parametric",
    successful_fits = length(good),
    requested_reps = reps,
    tail_probability = tail_prob,
    model_syntax = esem_syntax,
    n_obs = n_obs,
    telemetry = c(
      list(
        elapsed_seconds = .semantica_dfi_elapsed(start_time),
        cache_hit = FALSE,
        requested_reps = reps,
        completed_reps = length(results),
        successful_fits = length(good),
        failed_fits = max(0L, length(results) - length(good)),
        parallel_workers = n_cores,
        task_seeds = seeds[seq_along(results)]
      ),
      .semantica_dfi_fit_telemetry(good),
      list(adaptive = run$telemetry)
    )
  )

  if (verbose) {
    cat(sprintf("[ESEM-DFI] successful fits = %d / %d\n", length(good), reps))
    cat(sprintf("[ESEM-DFI] cutoffs: CFI >= %.4f, TLI >= %.4f, RMSEA <= %.4f, SRMR <= %.4f\n",
                cut$cfi, cut$tli, cut$rmsea, cut$srmr))
    cat(sprintf("[ESEM-DFI] elapsed: %.1fs | workers: %d | successful fallback refits: %d\n",
                cut$telemetry$elapsed_seconds,
                cut$telemetry$parallel_workers,
                cut$telemetry$successful_fallback_attempt))
  }
  .semantica_dfi_cache_set(cache, cache_target, cut)
  cut
}

compute_semantic_approx_dfi_cutoffs <- function(esem_fit, esem_syntax, factors, items_per_factor,
                                                observed_cor = NULL,
                                                n_obs = 1000, estimator = "ML",
                                                rotation = "geomin",
                                                rotation_args = list(geomin.epsilon = 0.50),
                                                reps = 100L, level = 1,
                                                criterion = "Sensitivity",
                                                n_cores = 1L, iter_max = 1000L,
                                                residual_cap_quantile = 0.95,
                                                embed_reliability = 1.0,
                                                verbose = TRUE,
                                                progress = verbose,
                                                cache = NULL, cluster = NULL,
                                                adaptive = FALSE, adaptive_min_reps = NULL,
                                                adaptive_batch_reps = 50L,
                                                adaptive_tol = 0.002,
                                                adaptive_stable_batches = 2L) {
  if (is.null(esem_fit) || is.null(esem_syntax) || is.null(observed_cor)) return(NULL)
  start_time <- proc.time()[["elapsed"]]
  reps <- max(20L, as.integer(reps))
  n_cores <- .semantica_max_workers(n_cores)

  pop_cov <- tryCatch(lavaan::fitted(esem_fit)$cov, error = function(e) NULL)
  if (is.null(pop_cov) || !is.matrix(pop_cov) || any(!is.finite(pop_cov))) {
    if (verbose) message("[SEMANTIC-DFI] Could not extract model-implied covariance.")
    return(NULL)
  }
  pop_cor <- stabilize_correlation_matrix(pop_cov)
  item_names <- rownames(pop_cor)
  if (is.null(item_names) || length(item_names) < 2L) return(NULL)

  observed_cor <- tryCatch(stabilize_correlation_matrix(observed_cor), error = function(e) NULL)
  if (is.null(observed_cor)) return(NULL)
  common <- intersect(item_names, rownames(observed_cor))
  common <- intersect(common, colnames(observed_cor))
  if (length(common) != length(item_names)) {
    if (verbose) message("[SEMANTIC-DFI] Observed and implied matrices do not align.")
    return(NULL)
  }
  pop_cor <- pop_cor[item_names, item_names, drop = FALSE]
  observed_cor <- observed_cor[item_names, item_names, drop = FALSE]

  # The exact ESEM-parametric DFI simulates from the fitted model alone. Here
  # we retain the warm-up ESEM residual pattern because embedding-derived item
  # correlations are semantic proxies, not a direct sample from an exact SEM
  # population. Robust capping prevents a few local-dependence pairs from
  # defining the whole approximate-fit reference distribution.
  residual <- observed_cor - pop_cor
  diag(residual) <- 0
  off <- lower.tri(residual) | upper.tri(residual)
  residual_vals <- abs(residual[off])
  residual_vals <- residual_vals[is.finite(residual_vals)]
  if (length(residual_vals) == 0L) return(NULL)
  residual_cap_quantile <- as.numeric(residual_cap_quantile)
  if (!is.finite(residual_cap_quantile) || residual_cap_quantile <= 0 || residual_cap_quantile >= 1) {
    residual_cap_quantile <- 0.95
  }
  resid_cap <- as.numeric(stats::quantile(residual_vals, residual_cap_quantile, na.rm = TRUE, names = FALSE))
  if (is.finite(resid_cap) && resid_cap > 0) {
    residual[off] <- sign(residual[off]) * pmin(abs(residual[off]), resid_cap)
  }

  reliability <- suppressWarnings(as.numeric(embed_reliability))
  if (!is.finite(reliability)) reliability <- 1.0
  reliability <- min(1.0, max(0.50, reliability))
  approx_cor <- pop_cor + residual
  if (reliability < 1.0) {
    # Reliability attenuation is optional and mirrors the existing CFA-style
    # DFI logic: lower semantic proxy reliability should not imply stronger
    # population correlations than the embedding source can support.
    approx_cor[off] <- approx_cor[off] * sqrt(reliability)
  }
  approx_cor <- stabilize_correlation_matrix(approx_cor)
  dimnames(approx_cor) <- list(item_names, item_names)

  tail_prob <- dfi_tail_probability(level, criterion)
  cache_target <- list(
    kind = "semantic_approx",
    approx_cor = approx_cor,
    syntax = esem_syntax,
    factors = factors,
    items_per_factor = items_per_factor,
    n_obs = n_obs,
    estimator = estimator,
    rotation = rotation,
    rotation_args = rotation_args,
    reps = reps,
    level = level,
    criterion = criterion,
    iter_max = iter_max,
    residual_cap_quantile = residual_cap_quantile,
    embed_reliability = reliability,
    adaptive = isTRUE(adaptive),
    adaptive_min_reps = adaptive_min_reps,
    adaptive_batch_reps = adaptive_batch_reps,
    adaptive_tol = adaptive_tol,
    adaptive_stable_batches = adaptive_stable_batches
  )
  cached <- .semantica_dfi_cache_get(cache, cache_target)
  if (!is.null(cached)) {
    if (verbose) cat("[SEMANTIC-DFI] Reusing identical cached DFI calibration.\n")
    return(cached)
  }
  seeds <- sample.int(.Machine$integer.max, reps)
  with_task_seed <- .semantica_with_task_seed

  single_rep <- function(seed) {
    with_task_seed(seed, {
      sim_cor <- sample_correlation_from_population(approx_cor, n_obs)
      if (is.null(sim_cor)) return(NULL)
      dimnames(sim_cor) <- list(item_names, item_names)
      fit <- run_esem_on_matrix(
        esem_syntax, sim_cor, n_obs = n_obs, estimator = estimator,
        rotation = rotation, rotation_args = rotation_args,
        iter_max = iter_max, fallback = TRUE,
        sample_cov_rescale = TRUE
      )
      if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) return(NULL)
      fm <- tryCatch(lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")),
                     error = function(e) NULL)
      if (is.null(fm) || any(!is.finite(fm))) return(NULL)
      list(cfi = as.numeric(fm["cfi"]), tli = as.numeric(fm["tli"]),
           rmsea = as.numeric(fm["rmsea"]), srmr = as.numeric(fm["srmr"]),
           fit_attempt = attr(fit, "semantica_fit_attempt") %||% NA_integer_)
    })
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat(" COMPUTING DFI CUTOFFS -- ESEM SEMANTIC-APPROXIMATE PROXY\n")
    cat(sprintf("  Factors          : %d\n", length(factors)))
    cat(sprintf("  Items per factor : %s\n", paste(names(items_per_factor), items_per_factor, sep = "=", collapse = ", ")))
    cat(sprintf("  Sample size (N)  : %d | Reps: %d | Tail: %.2f\n", n_obs, reps, tail_prob))
    cat(sprintf("  Rotation         : %s | Estimator: %s\n", rotation, estimator))
    cat(sprintf("  Residual cap q   : %.2f | q95 |r| = %.4f | max |r| = %.4f\n\n",
                residual_cap_quantile,
                as.numeric(stats::quantile(residual_vals, 0.95, na.rm = TRUE, names = FALSE)),
                max(residual_vals, na.rm = TRUE)))
  }

  cl <- if (is.function(cluster)) cluster() else cluster
  if (is.null(cl) && n_cores > 1L) {
    cl <- .semantica_make_dfi_cluster(n_cores)
    if (!is.null(cl)) on.exit(.semantica_stop_cluster(cl), add = TRUE)
  }
  if (!is.null(cl)) {
    n_cores <- max(1L, length(cl))
    export_env <- new.env(parent = emptyenv())
    export_env$approx_cor <- approx_cor
    export_env$item_names <- item_names
    export_env$n_obs <- n_obs
    export_env$estimator <- estimator
    export_env$rotation <- rotation
    export_env$rotation_args <- rotation_args
    export_env$iter_max <- iter_max
    export_env$esem_syntax <- esem_syntax
    export_env$sample_correlation_from_population <- sample_correlation_from_population
    export_env$stabilize_correlation_matrix <- stabilize_correlation_matrix
    export_env$run_esem_on_matrix <- run_esem_on_matrix
    for (fn in c(
      "%||%", ".semantica_fast_lavaan_se",
      "is_admissible_esem_fit", ".semantica_assess_esem_fit",
      "assess_esem_admissibility", ".semantica_safe_lav_inspect",
      ".semantica_collect_numeric", ".semantica_numeric_matrix"
    )) {
      if (exists(fn, mode = "function")) export_env[[fn]] <- get(fn)
    }
    .semantica_cluster_export_environment(cl, export_env)
  }
  run_batch <- function(batch, first_batch = FALSE) {
    label <- if (isTRUE(first_batch)) "[SEMANTIC-DFI] Approximate-proxy refits" else NULL
    if (!is.null(cl)) {
      .semantica_progress_par_lapply(cl, batch, single_rep, progress = progress, label = label)
    } else {
      .semantica_progress_lapply(batch, single_rep, progress = progress, label = label)
    }
  }
  estimate_cutoffs <- function(batch_results) {
    batch_good <- Filter(Negate(is.null), batch_results)
    if (length(batch_good) < 20L) return(NULL)
    list(
      cfi = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "cfi"),
                                       probs = tail_prob, na.rm = TRUE, names = FALSE)),
      tli = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "tli"),
                                       probs = tail_prob, na.rm = TRUE, names = FALSE)),
      rmsea = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "rmsea"),
                                         probs = 1 - tail_prob, na.rm = TRUE, names = FALSE)),
      srmr = as.numeric(stats::quantile(vapply(batch_good, `[[`, numeric(1L), "srmr"),
                                        probs = 1 - tail_prob, na.rm = TRUE, names = FALSE))
    )
  }
  run <- .semantica_dfi_adaptive_batches(
    seeds, run_batch, estimate_cutoffs,
    enabled = adaptive,
    min_reps = adaptive_min_reps,
    batch_reps = adaptive_batch_reps,
    cutoff_tol = adaptive_tol,
    stable_batches = adaptive_stable_batches
  )
  results <- run$results

  good <- Filter(Negate(is.null), results)
  min_success <- max(20L, ceiling(length(results) * 0.40))
  if (length(good) < min_success) {
    if (verbose) {
      message(sprintf("[SEMANTIC-DFI] Only %d/%d successful fits; falling back.", length(good), reps))
    }
    return(NULL)
  }

  cfi_v <- vapply(good, `[[`, numeric(1L), "cfi")
  tli_v <- vapply(good, `[[`, numeric(1L), "tli")
  rmsea_v <- vapply(good, `[[`, numeric(1L), "rmsea")
  srmr_v <- vapply(good, `[[`, numeric(1L), "srmr")

  cfi_cut <- as.numeric(stats::quantile(cfi_v, probs = tail_prob, na.rm = TRUE, names = FALSE))
  tli_cut <- as.numeric(stats::quantile(tli_v, probs = tail_prob, na.rm = TRUE, names = FALSE))
  rmsea_cut <- as.numeric(stats::quantile(rmsea_v, probs = 1 - tail_prob, na.rm = TRUE, names = FALSE))
  srmr_cut <- as.numeric(stats::quantile(srmr_v, probs = 1 - tail_prob, na.rm = TRUE, names = FALSE))
  unusually_permissive <- (is.finite(cfi_cut) && cfi_cut < 0.80) ||
    (is.finite(rmsea_cut) && rmsea_cut > 0.15) ||
    (is.finite(srmr_cut) && srmr_cut > 0.12)
  unusually_strict <- (is.finite(cfi_cut) && cfi_cut > 0.985) ||
    (is.finite(rmsea_cut) && rmsea_cut < 0.020) ||
    (is.finite(srmr_cut) && srmr_cut < 0.020)

  cut <- list(
    cfi = cfi_cut,
    tli = tli_cut,
    rmsea = rmsea_cut,
    srmr = srmr_cut,
    was_degenerate = unusually_permissive,
    unusually_permissive = unusually_permissive,
    unusually_strict = unusually_strict,
    dfi_function = "ESEM semantic-approximate residual simulation",
    data_type = "semantic_proxy_continuous",
    loading_source = "ESEM-implied covariance plus semantic residual structure",
    cutoff_calibration = "ESEM-semantic-approximate",
    successful_fits = length(good),
    requested_reps = reps,
    tail_probability = tail_prob,
    residual_cap_quantile = residual_cap_quantile,
    residual_q95 = as.numeric(stats::quantile(residual_vals, 0.95, na.rm = TRUE, names = FALSE)),
    residual_max = max(residual_vals, na.rm = TRUE),
    embed_reliability = reliability,
    model_syntax = esem_syntax,
    n_obs = n_obs,
    note = "Approximate-fit DFI uses the warm-up semantic residual pattern as part of the population reference; it should be triangulated with loadings, AVE, HTMT, and final response-data validation.",
    telemetry = c(
      list(
        elapsed_seconds = .semantica_dfi_elapsed(start_time),
        cache_hit = FALSE,
        requested_reps = reps,
        completed_reps = length(results),
        successful_fits = length(good),
        failed_fits = max(0L, length(results) - length(good)),
        parallel_workers = n_cores,
        task_seeds = seeds[seq_along(results)]
      ),
      .semantica_dfi_fit_telemetry(good),
      list(adaptive = run$telemetry)
    )
  )

  if (verbose) {
    cat(sprintf("[SEMANTIC-DFI] successful fits = %d / %d\n", length(good), reps))
    cat(sprintf("[SEMANTIC-DFI] cutoffs: CFI >= %.4f, TLI >= %.4f, RMSEA <= %.4f, SRMR <= %.4f\n",
                cut$cfi, cut$tli, cut$rmsea, cut$srmr))
    cat(sprintf("[SEMANTIC-DFI] elapsed: %.1fs | workers: %d | successful fallback refits: %d\n",
                cut$telemetry$elapsed_seconds,
                cut$telemetry$parallel_workers,
                cut$telemetry$successful_fallback_attempt))
    if (isTRUE(cut$was_degenerate)) {
      cat("[SEMANTIC-DFI] cutoffs are unusually permissive; SEMANTICA will use fallback cutoffs for the search.\n")
    }
  }
  .semantica_dfi_cache_set(cache, cache_target, cut)
  cut
}

compute_semantic_roc_dfi_cutoffs <- function(esem_fit, esem_syntax, factors, items_per_factor,
                                             observed_cor = NULL, factor_assignment = NULL,
                                             n_obs = 1000, estimator = "ML",
                                             rotation = "geomin",
                                             rotation_args = list(geomin.epsilon = 0.50),
                                             reps = 100L, level = 1,
                                             criterion = "Sensitivity",
                                             n_cores = 1L, iter_max = 1000L,
                                             residual_cap_quantile = 0.95,
                                             embed_reliability = 1.0,
                                             misspec_strength = 1.0,
                                             verbose = TRUE,
                                             progress = verbose,
                                             cache = NULL, cluster = NULL,
                                             adaptive = FALSE, adaptive_min_reps = NULL,
                                             adaptive_batch_reps = 50L,
                                             adaptive_tol = 0.002,
                                             adaptive_stable_batches = 2L) {
  if (is.null(esem_fit) || is.null(esem_syntax) || is.null(observed_cor)) return(NULL)
  start_time <- proc.time()[["elapsed"]]
  reps <- max(40L, as.integer(reps))
  reps_per_dist <- max(20L, ceiling(reps / 2L))
  n_cores <- .semantica_max_workers(n_cores)

  pop <- make_semantic_approx_population(
    esem_fit = esem_fit,
    observed_cor = observed_cor,
    residual_cap_quantile = residual_cap_quantile,
    embed_reliability = embed_reliability,
    verbose = verbose
  )
  if (is.null(pop)) return(NULL)

  bad_cor <- make_semantic_misspecified_population(
    acceptable_cor = pop$approx_cor,
    observed_cor = pop$observed_cor,
    factor_assignment = factor_assignment,
    factors = factors,
    strength = misspec_strength
  )
  if (is.null(bad_cor)) return(NULL)

  item_names <- pop$item_names
  tail_prob <- dfi_tail_probability(level, criterion)
  cache_target <- list(
    kind = "semantic_roc",
    acceptable_cor = pop$approx_cor,
    misspecified_cor = bad_cor,
    syntax = esem_syntax,
    factors = factors,
    items_per_factor = items_per_factor,
    factor_assignment = factor_assignment,
    n_obs = n_obs,
    estimator = estimator,
    rotation = rotation,
    rotation_args = rotation_args,
    reps = reps,
    level = level,
    criterion = criterion,
    iter_max = iter_max,
    residual_cap_quantile = pop$residual_cap_quantile,
    embed_reliability = pop$embed_reliability,
    misspec_strength = misspec_strength,
    adaptive = isTRUE(adaptive),
    adaptive_min_reps = adaptive_min_reps,
    adaptive_batch_reps = adaptive_batch_reps,
    adaptive_tol = adaptive_tol,
    adaptive_stable_batches = adaptive_stable_batches
  )
  cached <- .semantica_dfi_cache_get(cache, cache_target)
  if (!is.null(cached)) {
    if (verbose) cat("[SEMANTIC-ROC-DFI] Reusing identical cached DFI calibration.\n")
    return(cached)
  }
  good_seeds <- sample.int(.Machine$integer.max, reps_per_dist)
  bad_seeds <- sample.int(.Machine$integer.max, reps_per_dist)
  good_jobs <- lapply(good_seeds, function(s) list(seed = s, kind = "acceptable"))
  bad_jobs <- lapply(bad_seeds, function(s) list(seed = s, kind = "misspecified"))
  jobs <- unlist(Map(function(good_job, bad_job) list(good_job, bad_job),
                     good_jobs, bad_jobs), recursive = FALSE)
  with_task_seed <- .semantica_with_task_seed

  single_job <- function(job) {
    with_task_seed(job$seed, {
      base_cor <- if (identical(job$kind, "acceptable")) acceptable_cor else misspecified_cor
      sim_cor <- sample_correlation_from_population(base_cor, n_obs)
      if (is.null(sim_cor)) return(NULL)
      dimnames(sim_cor) <- list(item_names, item_names)
      fit <- run_esem_on_matrix(
        esem_syntax, sim_cor, n_obs = n_obs, estimator = estimator,
        rotation = rotation, rotation_args = rotation_args,
        iter_max = iter_max, fallback = TRUE,
        sample_cov_rescale = TRUE
      )
      if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) return(NULL)
      fm <- tryCatch(lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")),
                     error = function(e) NULL)
      if (is.null(fm) || any(!is.finite(fm))) return(NULL)
      list(
        kind = job$kind,
        cfi = as.numeric(fm["cfi"]),
        tli = as.numeric(fm["tli"]),
        rmsea = as.numeric(fm["rmsea"]),
        srmr = as.numeric(fm["srmr"]),
        fit_attempt = attr(fit, "semantica_fit_attempt") %||% NA_integer_
      )
    })
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat(" COMPUTING DFI CUTOFFS -- ESEM SEMANTIC-ROC PROXY\n")
    cat(sprintf("  Factors          : %d\n", length(factors)))
    cat(sprintf("  Items per factor : %s\n", paste(names(items_per_factor), items_per_factor, sep = "=", collapse = ", ")))
    cat(sprintf("  Sample size (N)  : %d | Reps/dist: %d | Tail: %.2f\n", n_obs, reps_per_dist, tail_prob))
    cat(sprintf("  Rotation         : %s | Estimator: %s | Misspec strength: %.2f\n", rotation, estimator, misspec_strength))
    cat(sprintf("  Residual cap q   : %.2f | q95 |r| = %.4f | max |r| = %.4f\n\n",
                pop$residual_cap_quantile, pop$residual_q95, pop$residual_max))
  }

  acceptable_cor <- pop$approx_cor
  misspecified_cor <- bad_cor
  cl <- if (is.function(cluster)) cluster() else cluster
  if (is.null(cl) && n_cores > 1L) {
    cl <- .semantica_make_dfi_cluster(n_cores)
    if (!is.null(cl)) on.exit(.semantica_stop_cluster(cl), add = TRUE)
  }
  if (!is.null(cl)) {
    n_cores <- max(1L, length(cl))
    export_env <- new.env(parent = emptyenv())
    export_env$acceptable_cor <- acceptable_cor
    export_env$misspecified_cor <- misspecified_cor
    export_env$item_names <- item_names
    export_env$n_obs <- n_obs
    export_env$estimator <- estimator
    export_env$rotation <- rotation
    export_env$rotation_args <- rotation_args
    export_env$iter_max <- iter_max
    export_env$esem_syntax <- esem_syntax
    export_env$sample_correlation_from_population <- sample_correlation_from_population
    export_env$stabilize_correlation_matrix <- stabilize_correlation_matrix
    export_env$run_esem_on_matrix <- run_esem_on_matrix
    for (fn in c(
      "%||%", ".semantica_fast_lavaan_se",
      "is_admissible_esem_fit", ".semantica_assess_esem_fit",
      "assess_esem_admissibility", ".semantica_safe_lav_inspect",
      ".semantica_collect_numeric", ".semantica_numeric_matrix"
    )) {
      if (exists(fn, mode = "function")) export_env[[fn]] <- get(fn)
    }
    .semantica_cluster_export_environment(cl, export_env)
  }
  run_batch <- function(batch, first_batch = FALSE) {
    label <- if (isTRUE(first_batch)) "[SEMANTIC-ROC-DFI] Acceptable and misspecified proxy refits" else NULL
    if (!is.null(cl)) {
      .semantica_progress_par_lapply(cl, batch, single_job, progress = progress, label = label)
    } else {
      .semantica_progress_lapply(batch, single_job, progress = progress, label = label)
    }
  }
  estimate_cutoffs <- function(batch_results) {
    batch_good <- Filter(function(x) !is.null(x) && identical(x$kind, "acceptable"), batch_results)
    batch_bad <- Filter(function(x) !is.null(x) && identical(x$kind, "misspecified"), batch_results)
    if (length(batch_good) < 20L || length(batch_bad) < 20L) return(NULL)
    gv <- function(nm) vapply(batch_good, `[[`, numeric(1L), nm)
    bv <- function(nm) vapply(batch_bad, `[[`, numeric(1L), nm)
    list(
      cfi = select_roc_cutoff(gv("cfi"), bv("cfi"), TRUE, tail_prob, criterion)$cutoff,
      tli = select_roc_cutoff(gv("tli"), bv("tli"), TRUE, tail_prob, criterion)$cutoff,
      rmsea = select_roc_cutoff(gv("rmsea"), bv("rmsea"), FALSE, tail_prob, criterion)$cutoff,
      srmr = select_roc_cutoff(gv("srmr"), bv("srmr"), FALSE, tail_prob, criterion)$cutoff
    )
  }
  run <- .semantica_dfi_adaptive_batches(
    jobs, run_batch, estimate_cutoffs,
    enabled = adaptive,
    min_reps = adaptive_min_reps,
    batch_reps = adaptive_batch_reps,
    cutoff_tol = adaptive_tol,
    stable_batches = adaptive_stable_batches
  )
  results <- run$results

  good <- Filter(function(x) !is.null(x) && identical(x$kind, "acceptable"), results)
  bad <- Filter(function(x) !is.null(x) && identical(x$kind, "misspecified"), results)
  attempted_jobs <- jobs[seq_along(results)]
  attempted_good <- sum(vapply(attempted_jobs, function(x) identical(x$kind, "acceptable"), logical(1L)))
  attempted_bad <- sum(vapply(attempted_jobs, function(x) identical(x$kind, "misspecified"), logical(1L)))
  min_good_success <- max(20L, ceiling(attempted_good * 0.40))
  min_bad_success <- max(20L, ceiling(attempted_bad * 0.40))
  if (length(good) < min_good_success || length(bad) < min_bad_success) {
    if (verbose) {
      message(sprintf(
        "[SEMANTIC-ROC-DFI] Only %d acceptable and %d misspecified successful fits; falling back.",
        length(good), length(bad)
      ))
    }
    return(NULL)
  }

  gv <- function(nm) vapply(good, `[[`, numeric(1L), nm)
  bv <- function(nm) vapply(bad, `[[`, numeric(1L), nm)
  cfi_sel <- select_roc_cutoff(gv("cfi"), bv("cfi"), TRUE, tail_prob, criterion)
  tli_sel <- select_roc_cutoff(gv("tli"), bv("tli"), TRUE, tail_prob, criterion)
  rmsea_sel <- select_roc_cutoff(gv("rmsea"), bv("rmsea"), FALSE, tail_prob, criterion)
  srmr_sel <- select_roc_cutoff(gv("srmr"), bv("srmr"), FALSE, tail_prob, criterion)

  cfi_cut <- cfi_sel$cutoff
  tli_cut <- tli_sel$cutoff
  rmsea_cut <- rmsea_sel$cutoff
  srmr_cut <- srmr_sel$cutoff
  unusually_permissive <- (is.finite(cfi_cut) && cfi_cut < 0.80) ||
    (is.finite(rmsea_cut) && rmsea_cut > 0.15) ||
    (is.finite(srmr_cut) && srmr_cut > 0.12)
  unusually_strict <- (is.finite(cfi_cut) && cfi_cut > 0.990) ||
    (is.finite(rmsea_cut) && rmsea_cut < 0.015) ||
    (is.finite(srmr_cut) && srmr_cut < 0.015)

  cut <- list(
    cfi = cfi_cut,
    tli = tli_cut,
    rmsea = rmsea_cut,
    srmr = srmr_cut,
    was_degenerate = unusually_permissive || any(!is.finite(c(cfi_cut, tli_cut, rmsea_cut, srmr_cut))),
    unusually_permissive = unusually_permissive,
    unusually_strict = unusually_strict,
    dfi_function = "ESEM semantic-ROC approximate/misspecified simulation",
    data_type = "semantic_proxy_continuous",
    loading_source = "ESEM-implied covariance plus semantic residual and misspecification alternatives",
    cutoff_calibration = "ESEM-semantic-ROC",
    successful_fits = length(good),
    successful_misspecified_fits = length(bad),
    requested_reps = reps,
    reps_per_distribution = reps_per_dist,
    tail_probability = tail_prob,
    criterion = criterion,
    roc = list(cfi = cfi_sel, tli = tli_sel, rmsea = rmsea_sel, srmr = srmr_sel),
    residual_cap_quantile = pop$residual_cap_quantile,
    residual_q95 = pop$residual_q95,
    residual_max = pop$residual_max,
    misspec_strength = misspec_strength,
    embed_reliability = pop$embed_reliability,
    model_syntax = esem_syntax,
    n_obs = n_obs,
    note = "Semantic-ROC DFI chooses thresholds that retain acceptable semantic-ESEM populations while separating structured misspecification alternatives.",
    telemetry = c(
      list(
        elapsed_seconds = .semantica_dfi_elapsed(start_time),
        cache_hit = FALSE,
        requested_reps = length(jobs),
        completed_reps = length(results),
        successful_fits = length(good) + length(bad),
        failed_fits = max(0L, length(results) - length(good) - length(bad)),
        completed_acceptable_reps = attempted_good,
        completed_misspecified_reps = attempted_bad,
        parallel_workers = n_cores,
        task_seeds = vapply(
          attempted_jobs, function(job) job$seed, integer(1L)
        )
      ),
      .semantica_dfi_fit_telemetry(c(good, bad)),
      list(adaptive = run$telemetry)
    )
  )

  if (verbose) {
    cat(sprintf("[SEMANTIC-ROC-DFI] successful fits = acceptable %d/%d | misspecified %d/%d\n",
                length(good), reps_per_dist, length(bad), reps_per_dist))
    cat(sprintf("[SEMANTIC-ROC-DFI] cutoffs: CFI >= %.4f, TLI >= %.4f, RMSEA <= %.4f, SRMR <= %.4f\n",
                cut$cfi, cut$tli, cut$rmsea, cut$srmr))
    cat(sprintf("[SEMANTIC-ROC-DFI] sensitivity/specificity: CFI %.2f/%.2f | RMSEA %.2f/%.2f | SRMR %.2f/%.2f\n",
                cfi_sel$sensitivity, cfi_sel$specificity,
                rmsea_sel$sensitivity, rmsea_sel$specificity,
                srmr_sel$sensitivity, srmr_sel$specificity))
    cat(sprintf("[SEMANTIC-ROC-DFI] elapsed: %.1fs | workers: %d | successful fallback refits: %d\n",
                cut$telemetry$elapsed_seconds,
                cut$telemetry$parallel_workers,
                cut$telemetry$successful_fallback_attempt))
    if (isTRUE(cut$was_degenerate)) {
      cat("[SEMANTIC-ROC-DFI] cutoffs are degenerate; SEMANTICA will use fallback cutoffs for the search.\n")
    }
  }
  .semantica_dfi_cache_set(cache, cache_target, cut)
  cut
}

parse_dddfi_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  x <- trimws(as.character(x[1L]))
  if (!nzchar(x) || toupper(x) %in% c("NONE", "NA", "N/A")) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

compute_dddfi_final_cutoffs <- function(esem_fit, reps = 250L,
                                        mad_target = c("close", "fair", "mediocre"),
                                        mad_values = c(0.038, 0.050, 0.060),
                                        estimator = NULL, scale = "normal",
                                        verbose = TRUE) {
  mad_target <- match.arg(mad_target)
  if (is.null(esem_fit) || !requireNamespace("dynamic", quietly = TRUE)) return(NULL)
  reps <- max(20L, as.integer(reps))
  if (!is.numeric(mad_values) || length(mad_values) < 1L || any(!is.finite(mad_values))) {
    mad_values <- c(0.038, 0.050, 0.060)
  }

  out <- tryCatch(
    dynamic::DDDFI(
      model = esem_fit, scale = scale, reps = reps,
      estimator = estimator, MAD = mad_values,
      plot.dfi = FALSE, plot.dist = FALSE, plot.discrepancy = FALSE
    ),
    error = function(e) {
      if (verbose) message("[DDDFI] Final DDDFI failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out) || is.null(out$cutoffs)) return(NULL)

  cut_mat <- as.matrix(out$cutoffs)
  row_lookup <- c(close = "Close", fair = "Fair", mediocre = "Mediocre")
  target_row <- row_lookup[[mad_target]]
  row_idx <- which(tolower(rownames(cut_mat)) == tolower(target_row))
  if (length(row_idx) == 0L) {
    row_idx <- which(tolower(rownames(cut_mat)) == "consistent")
    target_row <- "Consistent"
  }
  if (length(row_idx) == 0L) return(NULL)
  row_idx <- row_idx[1L]

  get_col <- function(pattern) {
    idx <- grep(pattern, colnames(cut_mat), ignore.case = TRUE)
    if (length(idx) == 0L) NA_integer_ else idx[1L]
  }
  cfi_col <- get_col("^CFI$")
  rmsea_col <- get_col("^RMSEA$")
  rmsea_ci_col <- get_col("90")
  mad_col <- get_col("^MAD$")
  sim_mad_col <- get_col("Sim")

  fit_df <- out$fit
  fit_value <- function(pattern) {
    if (is.null(fit_df) || !is.data.frame(fit_df)) return(NA_real_)
    idx <- grep(pattern, trimws(colnames(fit_df)), ignore.case = TRUE)
    if (length(idx) == 0L) return(NA_real_)
    parse_dddfi_numeric(fit_df[1L, idx[1L]])
  }
  exact_fit <- tryCatch(
    lavaan::fitMeasures(esem_fit, c("cfi", "rmsea", "rmsea.ci.upper", "srmr")),
    error = function(e) NULL
  )
  exact_value <- function(name, fallback = NA_real_) {
    if (is.null(exact_fit) || !name %in% names(exact_fit)) return(fallback)
    val <- suppressWarnings(as.numeric(exact_fit[[name]]))
    if (is.finite(val)) val else fallback
  }

  cfi_cut <- if (!is.na(cfi_col)) parse_dddfi_numeric(cut_mat[row_idx, cfi_col]) else NA_real_
  rmsea_cut <- if (!is.na(rmsea_col)) parse_dddfi_numeric(cut_mat[row_idx, rmsea_col]) else NA_real_
  rmsea_ci_cut <- if (!is.na(rmsea_ci_col)) parse_dddfi_numeric(cut_mat[row_idx, rmsea_ci_col]) else NA_real_
  observed <- list(
    cfi = exact_value("cfi", fit_value("^CFI$")),
    rmsea = exact_value("rmsea", fit_value("^RMSEA$")),
    rmsea_ci = exact_value("rmsea.ci.upper", fit_value("90")),
    srmr = exact_value("srmr", fit_value("SRMR"))
  )

  pass <- list(
    cfi = if (is.finite(cfi_cut) && is.finite(observed$cfi)) observed$cfi >= cfi_cut else NA,
    rmsea = if (is.finite(rmsea_cut) && is.finite(observed$rmsea)) observed$rmsea <= rmsea_cut else NA,
    rmsea_ci = if (is.finite(rmsea_ci_cut) && is.finite(observed$rmsea_ci)) observed$rmsea_ci <= rmsea_ci_cut else NA
  )
  permissive <- (is.finite(cfi_cut) && cfi_cut < 0.90) ||
    (is.finite(rmsea_cut) && rmsea_cut > 0.10)

  list(
    target = mad_target,
    target_label = target_row,
    mad = if (!is.na(mad_col)) parse_dddfi_numeric(cut_mat[row_idx, mad_col]) else NA_real_,
    simulated_mad = if (!is.na(sim_mad_col)) parse_dddfi_numeric(cut_mat[row_idx, sim_mad_col]) else NA_real_,
    cfi = cfi_cut,
    rmsea = rmsea_cut,
    rmsea_ci = rmsea_ci_cut,
    observed = observed,
    pass = pass,
    raw_cutoffs = out$cutoffs,
    fit_table = out$fit,
    indices = out$indices,
    n = out$n,
    estimator = out$estimator,
    reps = reps,
    unusually_permissive = permissive,
    cutoff_source = "DDDFI direct-discrepancy approximate-fit cutoffs",
    note = if (permissive) {
      "DDDFI cutoffs are unusually permissive for a scale-development decision rule; interpret them as an approximate-discrepancy diagnostic and triangulate with ESEM-parametric DFI, equivalence-test diagnostics, AVE, HTMT, and loading quality."
    } else {
      "DDDFI may suppress cutoffs as NONE when sensitivity is below 50%; SRMR is descriptive because DDDFI does not return an SRMR cutoff."
    }
  )
}

compute_equivtest_final_diagnostic <- function(esem_fit, verbose = TRUE) {
  if (is.null(esem_fit) || !requireNamespace("dynamic", quietly = TRUE)) return(NULL)
  out <- tryCatch(
    dynamic::equivTest(esem_fit, plot = FALSE),
    error = function(e) {
      if (verbose) message("[equivTest] Final equivalence-test diagnostic failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out)) return(NULL)
  list(
    cfi_t_size = parse_dddfi_numeric(out$cfi),
    rmsea_t_size = parse_dddfi_numeric(out$rmsea),
    fit_table = out$fit,
    cfi_bins = c(
      poor = out$eo_c, mediocre = out$ct_c, fair = out$ft_c,
      close = out$mf_c, excellent = out$pf_c
    ),
    rmsea_bins = c(
      excellent = out$eo_r, close = out$ct_r, fair = out$ft_r,
      mediocre = out$mf_r, poor = out$pf_r
    ),
    cutoff_source = "dynamic::equivTest adjusted fit-index equivalence diagnostic",
    note = "equivTest follows Yuan, Chan, Marcoulides, and Bentler's equivalence-testing logic; it is reported as a diagnostic companion rather than as the ACO objective."
  )
}

diagnose_esem_solution_propriety <- function(esem_fit,
                                             loading_ceiling = 1.00,
                                             residual_floor = -1e-6,
                                             boundary_loading = 0.97,
                                             boundary_residual = 0.01) {
  # Backward-compatible diagnostic wrapper around the canonical admissibility
  # assessment. `loading_ceiling` is retained in the signature, but oblique
  # standardized loadings are not rejected by a blanket magnitude threshold.
  out <- list(
    improper = FALSE,
    near_boundary = FALSE,
    max_std_loading = NA_real_,
    min_std_residual_variance = NA_real_,
    reason = character(0)
  )
  if (is.null(esem_fit)) {
    out$improper <- TRUE
    out$reason <- "missing ESEM fit"
    return(out)
  }
  assessment <- is_admissible_esem_fit(esem_fit, return_assessment = TRUE)
  std <- tryCatch(lavaan::lavInspect(esem_fit, "std"), error = function(e) NULL)
  if (is.null(std)) {
    out$improper <- TRUE
    out$reason <- "standardized solution unavailable"
    return(out)
  }
  lambda <- std$lambda
  theta <- std$theta
  if (!is.null(lambda) && is.matrix(lambda)) {
    abs_lambda <- abs(lambda)
    out$max_std_loading <- suppressWarnings(max(abs_lambda, na.rm = TRUE))
    if (is.finite(out$max_std_loading) && out$max_std_loading >= boundary_loading) {
      out$near_boundary <- TRUE
    }
  }
  if (!is.null(theta) && is.matrix(theta) && nrow(theta) == ncol(theta)) {
    resid_var <- diag(theta)
    out$min_std_residual_variance <- suppressWarnings(min(resid_var, na.rm = TRUE))
    if (is.finite(out$min_std_residual_variance) && out$min_std_residual_variance < residual_floor) {
      out$reason <- c(out$reason, sprintf("standardized residual variance < %.4g", residual_floor))
    }
    if (is.finite(out$min_std_residual_variance) && out$min_std_residual_variance <= boundary_residual) {
      out$near_boundary <- TRUE
    }
  }
  out$improper <- !isTRUE(assessment$admissible)
  if (out$improper) out$reason <- unique(c(out$reason, assessment$reasons))
  out$admissibility <- assessment
  if (length(out$reason) == 0L) out$reason <- if (out$near_boundary) "near-boundary standardized solution" else "proper standardized solution"
  out
}

# =================================================================
# 0-C-BOOTSTRAP  EXTRACT FITTED LOADINGS FROM BOOTSTRAP ESEM
# =================================================================
extract_fitted_dfi_params_esem <- function(candidate_items, factor_assignment, factors, cosine_sim_matrix,
                                           n_obs = 1000, estimator = "ML", rotation = "geomin",
                                           rotation_args = list(geomin.epsilon = 0.50), heywood_ceiling = 0.97,
                                           max_fcor = 0.90, verbose = TRUE) {
  cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, candidate_items), error = function(e) NULL)
  if (is.null(cos_sub)) { if (verbose) message("[Bootstrap ESEM] Could not extract submatrix."); return(NULL) }

  esem_cor <- tryCatch(transform_cosine_for_esem(cos_sub, factor_assignment, factors), error = function(e) NULL)
  if (is.null(esem_cor)) { if (verbose) message("[Bootstrap ESEM] Correlation matrix transformation failed."); return(NULL) }

  esem_syntax <- build_esem_syntax_safe(candidate_items, factor_assignment, factors)
  esem_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, candidate_items, factor_assignment, factors)
  esem_fit    <- run_esem_on_matrix(esem_syntax, esem_cor, n_obs, estimator, rotation, esem_rotation_args)
  if (is.null(esem_fit) || !lavaan::lavInspect(esem_fit, "converged")) { if (verbose) message("[Bootstrap ESEM] ESEM did not converge."); return(NULL) }

  aligned_est <- tryCatch(
    extract_aligned_esem_solution(
      esem_fit, factor_assignment = factor_assignment, factors = factors,
      standardized = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(aligned_est)) return(NULL)
  lambda_mat <- aligned_est$lambda

  max_abs_loads <- apply(abs(lambda_mat), 1, max, na.rm = TRUE)
  propriety <- diagnose_esem_solution_propriety(
    esem_fit,
    loading_ceiling = 1.00,
    residual_floor = -1e-6,
    boundary_loading = heywood_ceiling,
    boundary_residual = 0.01
  )
  raw_boundary_flag <- any(max_abs_loads >= heywood_ceiling, na.rm = TRUE)
  if (isTRUE(propriety$improper)) {
    if (verbose) {
      message(sprintf(
        "[Bootstrap ESEM] Improper standardized solution (%s). Using fallback.",
        paste(propriety$reason, collapse = "; ")
      ))
    }
    attr(propriety, "failure_reason") <- "improper_standardized_solution"
    return(NULL)
  }
  if (raw_boundary_flag && verbose) {
    message(sprintf(
      "[Bootstrap ESEM] High unstandardized loading flagged, but standardized solution is proper (max std loading = %.3f, min std residual variance = %.3f). Using fitted parameters.",
      propriety$max_std_loading,
      propriety$min_std_residual_variance
    ))
  } else if (isTRUE(propriety$near_boundary) && verbose) {
    message(sprintf(
      "[Bootstrap ESEM] Near-boundary standardized solution retained for DFI population (max std loading = %.3f, min std residual variance = %.3f).",
      propriety$max_std_loading,
      propriety$min_std_residual_variance
    ))
  }

  fitted_loadings <- lapply(setNames(factors, factors), function(f) {
    f_items <- names(factor_assignment[factor_assignment == f])
    f_items <- intersect(f_items, rownames(lambda_mat))
    if (length(f_items) == 0L) return(numeric(0))
    abs(lambda_mat[f_items, f, drop = TRUE])
  })

  fitted_factor_cors <- tryCatch({
    psi_mat <- aligned_est$psi
    if (is.null(psi_mat) || !is.matrix(psi_mat)) return(NULL)
    d_inv  <- diag(1 / sqrt(diag(psi_mat)), nrow(psi_mat))
    cor_lv <- d_inv %*% psi_mat %*% d_inv; diag(cor_lv) <- 1.0
    rownames(cor_lv) <- colnames(cor_lv) <- factors
    ut <- upper.tri(cor_lv); max_abs <- max(abs(cor_lv[ut]), na.rm = TRUE)
    if (max_abs >= max_fcor) {
      cor_lv[ut] <- pmin(pmax(cor_lv[ut], -(max_fcor - 0.01)), max_fcor - 0.01)
      cor_lv[lower.tri(cor_lv)] <- t(cor_lv)[lower.tri(cor_lv)]; diag(cor_lv) <- 1.0
    }
    cor_lv
  }, error = function(e) NULL)

  list(
    fitted_loadings = fitted_loadings,
    fitted_factor_cors = fitted_factor_cors,
    esem_fit = esem_fit,
    esem_cor = esem_cor,
    esem_syntax = esem_syntax,
    rotation_args = esem_rotation_args,
    propriety = propriety,
    loading_source = "ESEM-fitted"
  )
}

compute_heuristic_cutoffs <- function(n_factors, items_per_factor, n_obs = 300) {
  ipf <- min(as.integer(items_per_factor))
  base <- if (ipf <= 3L) list(cfi = 0.960, tli = 0.940, rmsea = 0.090, srmr = 0.080) else
    if (ipf <= 5L) list(cfi = 0.950, tli = 0.930, rmsea = 0.070, srmr = 0.065) else
      list(cfi = 0.930, tli = 0.910, rmsea = 0.060, srmr = 0.055)
  if (n_factors >= 5L) { base$cfi <- base$cfi - 0.015; base$rmsea <- base$rmsea + 0.005; base$srmr <- base$srmr + 0.005 }
  if (n_obs < 200L) { base$cfi <- base$cfi - 0.010; base$rmsea <- base$rmsea + 0.010 }
  base
}

# =================================================================
# 1, 2, 3  ESEM SYNTAX, COR TRANSFORMATION, SAFE RUNNER
# =================================================================
sanitize_lavaan_name <- function(x) gsub("[^A-Za-z0-9_]", "_", trimws(x))

build_esem_syntax_safe <- function(selected_items, factor_assignment, factors, block_name = "ESEM_BLOCK") {
  block_name <- sanitize_lavaan_name(block_name)
  factors_s  <- sanitize_lavaan_name(factors)
  all_items_rhs <- paste(selected_items, collapse = " + ")
  lines <- vapply(factors_s, function(f) sprintf('efa("%s")*%s =~ %s', block_name, f, all_items_rhs), character(1L))
  paste(lines, collapse = "\n")
}

build_esem_target_matrix <- function(selected_items, factor_assignment, factors) {
  if (is.null(selected_items) || is.null(factor_assignment) || is.null(factors)) return(NULL)
  selected_items <- as.character(selected_items)
  factors <- as.character(factors)
  target <- matrix(0, nrow = length(selected_items), ncol = length(factors),
                   dimnames = list(selected_items, sanitize_lavaan_name(factors)))
  fa <- as.character(factor_assignment[selected_items])
  for (i in seq_along(selected_items)) {
    f_idx <- match(fa[[i]], factors)
    if (is.na(f_idx)) next
    # lavaan target rotation treats NA as freely estimated and numeric values
    # as rotation targets. We target cross-loadings near zero while allowing
    # the intended primary loading to rotate freely.
    target[i, f_idx] <- NA_real_
  }
  target
}

prepare_esem_rotation_args <- function(rotation, rotation_args = list(),
                                       selected_items = NULL,
                                       factor_assignment = NULL,
                                       factors = NULL) {
  if (is.null(rotation_args)) rotation_args <- list()
  if (!is.list(rotation_args)) rotation_args <- as.list(rotation_args)
  rotation_l <- tolower(rotation %||% "")
  needs_target <- rotation_l %in% c("target", "pst", "targetq", "geominq")
  has_target <- any(tolower(names(rotation_args)) %in% c("target", "target.matrix", "target_matrix"))
  if (needs_target && !has_target) {
    target <- build_esem_target_matrix(selected_items, factor_assignment, factors)
    if (!is.null(target)) rotation_args$target <- target
  }
  rotation_args
}

transform_cosine_for_esem <- function(cos_matrix, factor_assignment = NULL, factors = NULL,
                                      material_change = 0.02) {
  if (!is.matrix(cos_matrix)) cos_matrix <- as.matrix(cos_matrix)
  material_change <- suppressWarnings(as.numeric(material_change[1L]))
  if (!is.finite(material_change) || material_change < 0) {
    stop("'material_change' must be a finite nonnegative number.", call. = FALSE)
  }
  p <- nrow(cos_matrix)
  if (p < 2L) {
    attr(cos_matrix, "semantica_matrix_repair") <- list(
      n_items = p, min_eigen_before = NA_real_, min_eigen_after = NA_real_,
      block_repairs = 0L, used_global_eigen_repair = FALSE, used_nearPD = FALSE,
      repair_required = FALSE, matrix_source = "raw_semantic_proxy",
      frobenius_change = 0, relative_frobenius_change = 0, max_abs_change = 0,
      mean_abs_offdiag_change = 0, offdiag_pearson = 1, offdiag_spearman = 1,
      proportion_materially_changed = 0, material_change = material_change,
      threshold_free_primary = TRUE
    )
    return(cos_matrix)
  }
  nms <- dimnames(cos_matrix)
  original <- (cos_matrix + t(cos_matrix)) / 2
  off_original <- original[row(original) != col(original)]
  original[row(original) != col(original)] <- pmin(pmax(off_original, -0.9999), 0.9999)
  diag(original) <- 1.0
  m <- original
  min_before <- min(eigen(m, symmetric = TRUE, only.values = TRUE)$values)
  block_repairs <- 0L
  used_global_eigen_repair <- FALSE
  used_nearPD <- FALSE

  if (!is.null(factor_assignment) && !is.null(factors)) {
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f])
      f_items <- intersect(f_items, rownames(m))
      if (length(f_items) < 2L) next
      block <- m[f_items, f_items, drop = FALSE]
      evals_b <- eigen(block, symmetric = TRUE, only.values = TRUE)$values
      if (min(evals_b) < 1e-6) {
        block_repairs <- block_repairs + 1L
        eig_b <- eigen(block, symmetric = TRUE)
        eig_b$values <- pmax(eig_b$values, 1e-4)
        block_fixed <- eig_b$vectors %*% diag(eig_b$values, length(eig_b$values)) %*% t(eig_b$vectors)
        D_inv <- diag(1 / sqrt(diag(block_fixed)), length(f_items))
        block_fixed <- D_inv %*% block_fixed %*% D_inv
        diag(block_fixed) <- 1.0
        m[f_items, f_items] <- block_fixed
      }
    }
  }

  evals <- eigen(m, symmetric = TRUE, only.values = TRUE)$values
  if (min(evals) < 1e-6) {
    used_global_eigen_repair <- TRUE
    eig <- eigen(m, symmetric = TRUE)
    eig$values <- pmax(eig$values, 1e-4)
    m_fixed <- eig$vectors %*% diag(eig$values, length(eig$values)) %*% t(eig$vectors)
    D_inv <- diag(1 / sqrt(diag(m_fixed)), p)
    m <- D_inv %*% m_fixed %*% D_inv
    diag(m) <- 1.0
    if (min(eigen(m, symmetric = TRUE, only.values = TRUE)$values) < 1e-8) {
      pd <- tryCatch(
        as.matrix(Matrix::nearPD(m, corr = TRUE, keepDiag = TRUE, do2eigen = TRUE, maxit = 1000)$mat),
        error = function(e) m
      )
      used_nearPD <- !isTRUE(all.equal(pd, m, tolerance = 1e-12))
      diag(pd) <- 1.0
      m <- pd
    }
  }
  dimnames(m) <- nms
  min_after <- min(eigen(m, symmetric = TRUE, only.values = TRUE)$values)
  delta <- m - original
  off_idx <- upper.tri(delta)
  original_off <- as.numeric(original[off_idx])
  repaired_off <- as.numeric(m[off_idx])
  frobenius_change <- sqrt(sum(delta^2))
  semantic_offdiag_norm <- sqrt(sum((original - diag(p))^2))
  relative_frobenius_change <- if (is.finite(semantic_offdiag_norm) && semantic_offdiag_norm > sqrt(.Machine$double.eps)) {
    frobenius_change / semantic_offdiag_norm
  } else if (frobenius_change <= sqrt(.Machine$double.eps)) 0 else NA_real_
  repair_required <- block_repairs > 0L || isTRUE(used_global_eigen_repair) || isTRUE(used_nearPD) ||
    frobenius_change > sqrt(.Machine$double.eps)
  repair_info <- list(
    n_items = p,
    min_eigen_before = unname(min_before),
    min_eigen_after = unname(min_after),
    block_repairs = block_repairs,
    used_global_eigen_repair = used_global_eigen_repair,
    used_nearPD = used_nearPD,
    repair_required = repair_required,
    matrix_source = if (repair_required) "repaired_semantic_proxy" else "raw_semantic_proxy",
    frobenius_change = frobenius_change,
    relative_frobenius_change = relative_frobenius_change,
    max_abs_change = max(abs(delta)),
    mean_abs_offdiag_change = mean(abs(delta[off_idx])),
    offdiag_pearson = if (length(original_off) > 1L && stats::sd(original_off) > 0 && stats::sd(repaired_off) > 0) {
      suppressWarnings(stats::cor(original_off, repaired_off, method = "pearson"))
    } else if (isTRUE(all.equal(original_off, repaired_off, tolerance = 1e-12))) 1 else NA_real_,
    offdiag_spearman = if (length(original_off) > 1L && length(unique(original_off)) > 1L && length(unique(repaired_off)) > 1L) {
      suppressWarnings(stats::cor(original_off, repaired_off, method = "spearman"))
    } else if (isTRUE(all.equal(original_off, repaired_off, tolerance = 1e-12))) 1 else NA_real_,
    # Retained only as a backwards-compatible descriptive count. Method
    # interpretation should prefer the continuous, threshold-free quantities
    # above because no universal acceptable repair threshold is established.
    proportion_materially_changed = mean(abs(delta[off_idx]) >= material_change),
    material_change = material_change,
    threshold_free_primary = TRUE,
    interpretation = paste(
      "Repair dependence is reported continuously through relative Frobenius",
      "change and off-diagonal Pearson/Spearman preservation. The legacy",
      "material-change proportion is descriptive only and is not a pass/fail",
      "criterion. Any repaired matrix remains a semantic structural proxy."
    )
  )
  attr(m, "semantica_matrix_repair") <- repair_info
  m
}

# Backward-compatible internal alias for older code paths.
transform_cosine_for_cfa <- transform_cosine_for_esem

# =================================================================
# 3-B  SAMPLE-FREE PFA AND RMSEA-POWER REFERENCE N
# =================================================================
efa_degrees_of_freedom <- function(n_indicators, n_factors) {
  p <- as.integer(n_indicators)
  m <- as.integer(n_factors)
  if (!is.finite(p) || !is.finite(m) || p < 2L || m < 1L || p <= m) return(NA_real_)
  ((p - m)^2 - p - m) / 2
}

rmsea_power <- function(n_obs, df, rmsea_null = 0.05, rmsea_alt = 0.06,
                        alpha = 0.05) {
  n_obs <- as.numeric(n_obs)
  df <- as.numeric(df)
  rmsea_null <- as.numeric(rmsea_null)
  rmsea_alt <- as.numeric(rmsea_alt)
  alpha <- as.numeric(alpha)
  if (!is.finite(n_obs) || !is.finite(df) || n_obs <= 1L || df <= 0 ||
      !is.finite(rmsea_null) || !is.finite(rmsea_alt) ||
      rmsea_null < 0 || rmsea_alt <= rmsea_null ||
      !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    return(NA_real_)
  }
  ncp_null <- (n_obs - 1) * df * rmsea_null^2
  ncp_alt <- (n_obs - 1) * df * rmsea_alt^2
  crit <- suppressWarnings(stats::qchisq(1 - alpha, df = df, ncp = ncp_null))
  if (!is.finite(crit)) return(NA_real_)
  pow <- suppressWarnings(1 - stats::pchisq(crit, df = df, ncp = ncp_alt))
  if (!is.finite(pow)) NA_real_ else max(0, min(1, pow))
}

required_n_for_rmsea_power <- function(df, n_indicators = NULL,
                                       rmsea_null = 0.05, rmsea_alt = 0.06,
                                       power = 0.80, alpha = 0.05,
                                       min_n = NULL, max_n = Inf) {
  df <- as.numeric(df)
  power <- as.numeric(power)
  max_n_value <- if (is.null(max_n)) Inf else suppressWarnings(as.numeric(max_n[1L]))
  if (length(max_n_value) == 0L || is.na(max_n_value) || max_n_value <= 0) {
    max_n_value <- Inf
  }
  lower <- if (!is.null(min_n)) {
    max(2L, as.integer(min_n))
  } else if (!is.null(n_indicators) && is.finite(as.numeric(n_indicators))) {
    as.integer(as.numeric(n_indicators)) + 3L
  } else {
    2L
  }
  if (!is.finite(df) || df <= 0 || !is.finite(power) || power <= 0 || power >= 1) {
    return(NA_integer_)
  }
  finite_max <- is.finite(max_n_value)
  hi <- if (finite_max) {
    max(lower, max(10L, as.integer(max_n_value)))
  } else {
    max(lower, 10L)
  }
  hi_power <- rmsea_power(hi, df, rmsea_null, rmsea_alt, alpha)
  if (!finite_max) {
    integer_ceiling <- .Machine$integer.max - 1L
    while ((!is.finite(hi_power) || hi_power < power) && hi < integer_ceiling) {
      next_hi <- min(integer_ceiling, max(hi + 1L, hi * 2L))
      if (!is.finite(next_hi) || next_hi <= hi) break
      hi <- as.integer(next_hi)
      hi_power <- rmsea_power(hi, df, rmsea_null, rmsea_alt, alpha)
    }
  }
  if (!is.finite(hi_power) || hi_power < power) return(NA_integer_)
  lo <- lower
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    mid_power <- rmsea_power(mid, df, rmsea_null, rmsea_alt, alpha)
    if (is.finite(mid_power) && mid_power >= power) hi <- mid else lo <- mid + 1L
  }
  as.integer(lo)
}

estimate_esem_reference_sample_size <- function(items_per_factor, n_factors = length(items_per_factor),
                                                rmsea_null = 0.05, rmsea_alt = 0.06,
                                                power = 0.80, alpha = 0.05,
                                                min_n = NULL, max_n = Inf) {
  counts <- suppressWarnings(as.integer(items_per_factor))
  counts <- counts[is.finite(counts) & counts > 0L]
  p <- sum(counts)
  m <- as.integer(n_factors)
  df <- efa_degrees_of_freedom(p, m)
  lower <- if (!is.null(min_n)) as.integer(min_n) else p + 3L
  max_n_value <- if (is.null(max_n)) Inf else suppressWarnings(as.numeric(max_n[1L]))
  if (length(max_n_value) == 0L || is.na(max_n_value) || max_n_value <= 0) {
    max_n_value <- Inf
  }
  max_n_eff <- if (is.finite(max_n_value)) {
    max(lower, max(10L, as.integer(max_n_value)))
  } else {
    Inf
  }
  n_req <- required_n_for_rmsea_power(
    df = df, n_indicators = p, rmsea_null = rmsea_null,
    rmsea_alt = rmsea_alt, power = power, alpha = alpha,
    min_n = lower, max_n = max_n_eff
  )
  achieved_power <- rmsea_power(n_req, df, rmsea_null, rmsea_alt, alpha)
  max_n_power <- if (is.finite(max_n_eff)) {
    rmsea_power(max_n_eff, df, rmsea_null, rmsea_alt, alpha)
  } else {
    NA_real_
  }
  target_power_reached <- is.finite(n_req) &&
    is.finite(achieved_power) && achieved_power >= power
  underpowered_at_max_n <- !target_power_reached &&
    is.finite(max_n_eff) &&
    is.finite(df) && df > 0 &&
    is.finite(max_n_power) && is.finite(power) &&
    max_n_power < power
  note <- "Reference fit N for semantic-proxy RMSEA testing, chosen by noncentral chi-square RMSEA power analysis; this is not a response-data validation sample-size recommendation."
  method <- "MacCallum-Browne-Sugawara RMSEA power"
  if (!is.finite(df) || df <= 0) {
    n_req <- max(lower, p + 3L)
    note <- paste(
      "Model has nonpositive or unstable approximate EFA degrees of freedom;",
      "using the minimum N required to sample a positive-definite correlation matrix."
    )
    method <- "positive-definite minimum fallback"
  } else if (underpowered_at_max_n) {
    n_req <- max_n_eff
    note <- paste(
      "Requested RMSEA-power target was not reached within reference_max_n;",
      "using reference_max_n as the semantic-proxy ESEM anchor instead of",
      "falling back to the positive-definite minimum. Increase reference_max_n",
      "or set reference_max_n = Inf if a larger proxy N is desired."
    )
    method <- "reference_max_n underpowered fallback"
  } else if (is.na(n_req)) {
    n_req <- max(lower, p + 3L)
    note <- paste(
      "RMSEA-power reference N could not be estimated from the supplied inputs;",
      "using the minimum N required to sample a positive-definite correlation matrix."
    )
    method <- "positive-definite minimum fallback"
  }
  low_df <- is.finite(df) && df > 0 && df < 10
  if (low_df) {
    note <- paste(
      note,
      "Low approximate EFA degrees of freedom can make RMSEA-power reference N sensitive;",
      "inspect semantic proxy N-sensitivity diagnostics."
    )
  }
  list(
    n_obs = as.integer(n_req),
    p = as.integer(p),
    n_factors = as.integer(m),
    df = as.numeric(df),
    rmsea_null = rmsea_null,
    rmsea_alt = rmsea_alt,
    power = power,
    alpha = alpha,
    max_n = if (is.finite(max_n_eff)) as.integer(max_n_eff) else Inf,
    max_n_power = as.numeric(max_n_power),
    achieved_power = as.numeric(achieved_power),
    target_power_reached = isTRUE(target_power_reached),
    underpowered_at_max_n = isTRUE(underpowered_at_max_n),
    method = method,
    note = note,
    role = "semantic_proxy_fit_anchor",
    respondent_sample_size = FALSE,
    used_for = c("semantic_proxy_esem", "semantic_proxy_dfi"),
    not_for = c("response_validation", "study_sample_size_recommendation"),
    low_df_warning = low_df
  )
}

pfa_harmonic_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L || any(x <= 0)) return(0)
  length(x) / sum(1 / pmax(x, 1e-6))
}

build_pfa_target_matrix <- function(items, factor_assignment, factors,
                                    primary_target = 1) {
  items <- as.character(items)
  factors <- as.character(factors)
  target <- matrix(0, nrow = length(items), ncol = length(factors),
                   dimnames = list(items, factors))
  primary_target <- suppressWarnings(as.numeric(primary_target))
  if (!is.finite(primary_target) || primary_target <= 0) primary_target <- 1
  for (item in items) {
    f <- as.character(factor_assignment[[item]])
    f_idx <- match(f, factors)
    if (!is.na(f_idx)) target[item, f_idx] <- primary_target
  }
  target
}

apply_pfa_loading_rotation <- function(loadings, rotation,
                                       target_matrix = NULL,
                                       target_random_starts = 10L) {
  rotation <- match.arg(rotation, c("promax", "target_oblique", "oblimin", "varimax", "none"))
  if (!is.matrix(loadings)) loadings <- as.matrix(loadings)
  phi <- diag(ncol(loadings))
  rotation_note <- NULL

  if (rotation == "none") {
    return(list(loadings = loadings, phi = phi, rotation = "none",
                requested_rotation = rotation, rotation_note = rotation_note))
  }
  if (rotation == "varimax") {
    rot <- tryCatch(stats::varimax(loadings), error = function(e) NULL)
    if (!is.null(rot)) loadings <- unclass(rot$loadings)
    return(list(loadings = loadings, phi = phi, rotation = "varimax",
                requested_rotation = rotation, rotation_note = rotation_note))
  }
  if (rotation == "promax") {
    rot <- tryCatch(stats::promax(loadings), error = function(e) NULL)
    if (!is.null(rot)) {
      loadings <- unclass(rot$loadings)
      phi <- tryCatch(solve(t(rot$rotmat) %*% rot$rotmat), error = function(e) diag(ncol(loadings)))
    }
    return(list(loadings = loadings, phi = phi, rotation = "promax",
                requested_rotation = rotation, rotation_note = rotation_note))
  }

  if (rotation == "oblimin") {
    if (requireNamespace("GPArotation", quietly = TRUE)) {
      oblimin_fn <- getExportedValue("GPArotation", "oblimin")
      rot <- tryCatch(
        suppressWarnings(oblimin_fn(
          loadings,
          randomStarts = max(0L, as.integer(target_random_starts))
        )),
        error = function(e) NULL
      )
      if (!is.null(rot) && !is.null(rot$loadings)) {
        loadings <- as.matrix(rot$loadings)
        phi <- if (!is.null(rot$Phi) && is.matrix(rot$Phi)) rot$Phi else diag(ncol(loadings))
        rownames(phi) <- colnames(phi) <- colnames(loadings)
        rotation_note <- "Direct oblimin oblique rotation via GPArotation::oblimin."
        return(list(loadings = loadings, phi = phi, rotation = "oblimin",
                    requested_rotation = rotation, rotation_note = rotation_note))
      }
      rotation_note <- "oblimin requested but GPArotation::oblimin failed; promax fallback used."
    } else {
      rotation_note <- "oblimin requested but GPArotation is not installed; promax fallback used."
    }
    fallback <- apply_pfa_loading_rotation(loadings, "promax")
    fallback$requested_rotation <- "oblimin"
    fallback$rotation_note <- rotation_note
    return(fallback)
  }

  if (rotation == "target_oblique") {
    target_ok <- !is.null(target_matrix) &&
      is.matrix(target_matrix) &&
      nrow(target_matrix) == nrow(loadings) &&
      ncol(target_matrix) == ncol(loadings)
    if (target_ok && !is.null(rownames(loadings)) && !is.null(rownames(target_matrix))) {
      target_matrix <- target_matrix[rownames(loadings), , drop = FALSE]
    }
    if (target_ok && requireNamespace("GPArotation", quietly = TRUE)) {
      target_q <- getExportedValue("GPArotation", "targetQ")
      colnames(loadings) <- colnames(target_matrix) %||% colnames(loadings)
      rot <- tryCatch(
        suppressWarnings(target_q(
          loadings,
          Target = target_matrix,
          randomStarts = max(0L, as.integer(target_random_starts))
        )),
        error = function(e) NULL
      )
      if (!is.null(rot) && !is.null(rot$loadings)) {
        loadings <- as.matrix(rot$loadings)
        rownames(loadings) <- rownames(target_matrix)
        colnames(loadings) <- colnames(target_matrix)
        phi <- if (!is.null(rot$Phi) && is.matrix(rot$Phi)) rot$Phi else diag(ncol(loadings))
        rownames(phi) <- colnames(phi) <- colnames(loadings)
        rotation_note <- "Fully specified oblique target rotation via GPArotation::targetQ; primary targets mark intended nonzero loadings and cross-loadings target zero."
        return(list(loadings = loadings, phi = phi, rotation = "target_oblique",
                    requested_rotation = rotation, rotation_note = rotation_note))
      }
      rotation_note <- "target_oblique requested but GPArotation::targetQ failed; promax fallback used."
    } else if (!requireNamespace("GPArotation", quietly = TRUE)) {
      rotation_note <- "target_oblique requested but GPArotation is not installed; promax fallback used."
    } else {
      rotation_note <- "target_oblique requested but a compatible target matrix was unavailable; promax fallback used."
    }
    fallback <- apply_pfa_loading_rotation(loadings, "promax")
    fallback$requested_rotation <- "target_oblique"
    fallback$rotation_note <- rotation_note
    return(fallback)
  }
}

extract_pfa_loadings <- function(cor_matrix, n_factors,
                                 extraction = c("principal", "ml"),
                                 rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
                                 target_matrix = NULL,
                                 already_transformed = FALSE) {
  extraction <- match.arg(extraction)
  rotation <- match.arg(rotation)
  if (!is.matrix(cor_matrix)) cor_matrix <- as.matrix(cor_matrix)
  p <- nrow(cor_matrix)
  if (p < 2L || n_factors < 1L || n_factors >= p) return(NULL)
  if (!isTRUE(already_transformed)) cor_matrix <- transform_cosine_for_esem(cor_matrix)

  if (extraction == "ml") {
    fit <- tryCatch(
      suppressWarnings(stats::factanal(
        covmat = list(cov = cor_matrix, n.obs = NA_integer_),
        factors = n_factors,
        rotation = if (rotation %in% c("none", "target_oblique", "oblimin")) "none" else rotation,
        control = list(maxit = 100L)
      )),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      loadings <- unclass(fit$loadings)
      rownames(loadings) <- rownames(cor_matrix)
      colnames(loadings) <- paste0("PFA", seq_len(ncol(loadings)))
      if (!(rotation %in% c("target_oblique", "oblimin"))) {
        phi <- fit$Phi %||% diag(ncol(loadings))
        return(list(loadings = loadings, phi = phi, extraction = "ml", rotation = rotation,
                    requested_rotation = rotation, rotation_note = NULL, fit = fit))
      }
      rot <- apply_pfa_loading_rotation(loadings, rotation, target_matrix)
      return(c(rot, list(extraction = "ml", fit = fit)))
    }
  }

  eig <- eigen(cor_matrix, symmetric = TRUE)
  vals <- pmax(eig$values[seq_len(n_factors)], 0)
  loadings <- eig$vectors[, seq_len(n_factors), drop = FALSE] %*% diag(sqrt(vals), n_factors)
  rownames(loadings) <- rownames(cor_matrix)
  colnames(loadings) <- paste0("PFA", seq_len(n_factors))
  rot <- apply_pfa_loading_rotation(loadings, rotation, target_matrix)
  loadings <- rot$loadings
  phi <- rot$phi
  rownames(loadings) <- rownames(cor_matrix)
  colnames(loadings) <- paste0("PFA", seq_len(ncol(loadings)))
  if (identical(rot$rotation, "target_oblique") && !is.null(target_matrix) && ncol(target_matrix) == ncol(loadings)) {
    colnames(loadings) <- colnames(target_matrix)
    if (is.matrix(phi) && nrow(phi) == ncol(loadings)) rownames(phi) <- colnames(phi) <- colnames(loadings)
  }
  list(loadings = loadings, phi = phi, extraction = "principal", rotation = rot$rotation,
       requested_rotation = rot$requested_rotation, rotation_note = rot$rotation_note, fit = NULL)
}

.semantica_adjusted_rand_index <- function(labels_a, labels_b) {
  if (length(labels_a) != length(labels_b) || length(labels_a) < 2L) {
    return(list(value = NA_real_, reason = "partitions must contain the same two or more observations"))
  }
  ok <- !is.na(labels_a) & !is.na(labels_b)
  labels_a <- as.character(labels_a[ok]); labels_b <- as.character(labels_b[ok])
  if (length(labels_a) < 2L) return(list(value = NA_real_, reason = "too few complete partition labels"))
  if (length(unique(labels_a)) < 2L || length(unique(labels_b)) < 2L) {
    return(list(value = NA_real_, reason = "degenerate one-cluster partition"))
  }
  tab <- table(labels_a, labels_b)
  choose2 <- function(x) x * (x - 1) / 2
  n <- sum(tab)
  total_pairs <- choose2(n)
  if (!is.finite(total_pairs) || total_pairs <= 0) return(list(value = NA_real_, reason = "insufficient pairs"))
  index <- sum(choose2(tab))
  row_pairs <- sum(choose2(rowSums(tab)))
  col_pairs <- sum(choose2(colSums(tab)))
  expected <- row_pairs * col_pairs / total_pairs
  max_index <- 0.5 * (row_pairs + col_pairs)
  denom <- max_index - expected
  if (!is.finite(denom) || abs(denom) <= .Machine$double.eps) {
    return(list(value = NA_real_, reason = "adjusted Rand index undefined for this partition"))
  }
  list(value = (index - expected) / denom, reason = NULL)
}

compute_pfa_diagnostics <- function(cos_matrix, factor_assignment, factors,
                                    extraction = c("principal", "ml"),
                                    rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
                                    min_loading = 0.40,
                                    min_margin = NULL) {
  extraction <- match.arg(extraction)
  rotation <- match.arg(rotation)
  fail <- list(
    available = FALSE, score = 0, recovery_score = 0, factor_presence_recovery = 0,
    partition_agreement_ari = NA_real_, partition_agreement_reason = "PFA diagnostics unavailable",
    salience_score = 0, clarity_score = 0,
    criterion_attainment_score = 0, continuous_salience_score = 0,
    continuous_clarity_score = 0, partition_quality_score = 0,
    score_schema = "pfa-continuous-geometry-v2",
    extraction = extraction, rotation = rotation,
    note = "PFA diagnostics unavailable."
  )
  if (is.null(cos_matrix) || is.null(factor_assignment) || is.null(factors)) return(fail)
  items <- intersect(rownames(cos_matrix), names(factor_assignment))
  factors <- as.character(factors)
  factor_assignment <- factor_assignment[items]
  if (length(items) < length(factors) + 2L || length(factors) < 2L) {
    fail$note <- "Too few indicators for sample-free PFA."
    return(fail)
  }
  cor_matrix <- tryCatch(transform_cosine_for_esem(cos_matrix[items, items, drop = FALSE]), error = function(e) NULL)
  if (is.null(cor_matrix)) return(fail)
  target_matrix <- NULL
  if (rotation == "target_oblique") {
    target_matrix <- build_pfa_target_matrix(rownames(cor_matrix), factor_assignment, factors, primary_target = 1)
  }
  pfa <- extract_pfa_loadings(
    cor_matrix, length(factors), extraction, rotation,
    target_matrix = target_matrix, already_transformed = TRUE
  )
  if (is.null(pfa) && extraction == "ml") {
    pfa <- extract_pfa_loadings(
      cor_matrix, length(factors), "principal", rotation,
      target_matrix = target_matrix, already_transformed = TRUE
    )
  }
  if (is.null(pfa)) return(fail)

  loadings <- pfa$loadings
  abs_load <- abs(loadings)
  avg_abs <- matrix(NA_real_, nrow = length(factors), ncol = ncol(abs_load),
                    dimnames = list(factors, colnames(abs_load)))
  for (f in factors) {
    f_items <- intersect(names(factor_assignment[factor_assignment == f]), rownames(abs_load))
    if (length(f_items) > 0L) avg_abs[f, ] <- colMeans(abs_load[f_items, , drop = FALSE], na.rm = TRUE)
  }
  dominant_labels <- apply(avg_abs, 2L, function(x) {
    if (all(!is.finite(x))) return(NA_character_)
    factors[which.max(x)]
  })
  recovered_factors <- unique(dominant_labels[!is.na(dominant_labels)])
  recovery_score <- length(recovered_factors) / length(factors)

  # Greedy one-to-one alignment from extracted PFA dimensions to intended factors.
  long <- which(is.finite(avg_abs), arr.ind = TRUE)
  if (nrow(long) == 0L) return(fail)
  long_score <- avg_abs[long]
  ord <- order(long_score, decreasing = TRUE)
  assigned_rows <- integer(0)
  assigned_cols <- integer(0)
  mapping <- rep(NA_integer_, length(factors))
  names(mapping) <- factors
  for (idx in ord) {
    r <- long[idx, 1L]
    c <- long[idx, 2L]
    if (r %in% assigned_rows || c %in% assigned_cols) next
    mapping[r] <- c
    assigned_rows <- c(assigned_rows, r)
    assigned_cols <- c(assigned_cols, c)
    if (length(assigned_rows) == length(factors)) break
  }

  # Factor axes are sign-indeterminate. Anchor each recovered PFA dimension so
  # the mean intended-factor loading is positive; this preserves the model while
  # making loading maps and PFA-derived factor correlations interpretable.
  sign_flips <- rep(1, ncol(loadings))
  names(sign_flips) <- colnames(loadings)
  for (f in factors) {
    f_col <- mapping[[f]]
    f_items <- intersect(names(factor_assignment[factor_assignment == f]), rownames(loadings))
    if (!is.finite(f_col) || is.na(f_col) || f_col < 1L || f_col > ncol(loadings) ||
        length(f_items) == 0L) {
      next
    }
    mean_signed <- mean(as.numeric(loadings[f_items, f_col, drop = TRUE]), na.rm = TRUE)
    if (is.finite(mean_signed) && mean_signed < 0) sign_flips[[f_col]] <- -1
  }
  if (any(sign_flips < 0)) {
    loadings <- sweep(loadings, 2L, sign_flips, `*`)
    if (!is.null(pfa$phi) && is.matrix(pfa$phi) &&
        nrow(pfa$phi) == length(sign_flips) && ncol(pfa$phi) == length(sign_flips)) {
      pfa$phi <- sweep(sweep(pfa$phi, 1L, sign_flips, `*`), 2L, sign_flips, `*`)
      rownames(pfa$phi) <- colnames(pfa$phi) <- colnames(loadings)
      diag(pfa$phi) <- 1
    }
    pfa$loadings <- loadings
    abs_load <- abs(loadings)
  }

  min_loading <- suppressWarnings(as.numeric(min_loading))
  if (!is.finite(min_loading) || min_loading <= 0) min_loading <- 0.40
  min_margin <- suppressWarnings(as.numeric(min_margin %||% (min_loading / 2)))
  if (!is.finite(min_margin) || min_margin <= 0) min_margin <- min_loading / 2

  item_rows <- vector("list", length(items))
  primary <- cross <- margin <- numeric(length(items))
  for (i in seq_along(items)) {
    item <- items[i]
    f <- as.character(factor_assignment[[item]])
    f_col <- if (f %in% names(mapping)) mapping[[f]] else NA_integer_
    primary[i] <- if (is.finite(f_col) && !is.na(f_col)) abs_load[item, f_col] else NA_real_
    other_cols <- setdiff(seq_len(ncol(abs_load)), f_col)
    cross[i] <- if (length(other_cols) > 0L) max(abs_load[item, other_cols], na.rm = TRUE) else 0
    if (!is.finite(cross[i])) cross[i] <- 0
    margin[i] <- primary[i] - cross[i]
    item_rows[[i]] <- data.frame(
      item = item, factor = f,
      primary_loading = primary[i],
      max_cross_loading = cross[i],
      loading_margin = margin[i],
      stringsAsFactors = FALSE
    )
  }
  item_diagnostics <- do.call(rbind, item_rows)
  dominant_component <- vapply(items, function(item) {
    vals <- abs_load[item, , drop = TRUE]
    if (!length(vals) || all(!is.finite(vals))) return(NA_character_)
    colnames(abs_load)[which.max(vals)]
  }, character(1L))
  partition_agreement <- .semantica_adjusted_rand_index(
    as.character(factor_assignment[items]), dominant_component
  )
  item_diagnostics$dominant_pfa_component <- unname(dominant_component[item_diagnostics$item])
  # Threshold-attainment fields are retained for interpretive continuity, but
  # they no longer define the PFA optimization score because ratios clipped at
  # one saturate as soon as every item clears a reference threshold.
  salience_score <- mean(pmin(pmax(primary / min_loading, 0), 1), na.rm = TRUE)
  clarity_score <- mean(pmin(pmax(margin / min_margin, 0), 1), na.rm = TRUE)
  if (!is.finite(salience_score)) salience_score <- 0
  if (!is.finite(clarity_score)) clarity_score <- 0
  criterion_attainment_score <- pfa_harmonic_mean(c(recovery_score, salience_score, clarity_score))

  finite_primary <- primary[is.finite(primary)]
  finite_margin <- margin[is.finite(margin)]
  mean_primary <- if (length(finite_primary) > 0L) mean(finite_primary) else NA_real_
  min_primary <- if (length(finite_primary) > 0L) min(finite_primary) else NA_real_
  mean_margin <- if (length(finite_margin) > 0L) mean(finite_margin) else NA_real_
  primary_ge_min <- if (length(finite_primary) > 0L) mean(finite_primary >= min_loading) else NA_real_
  margin_ge_min <- if (length(finite_margin) > 0L) mean(finite_margin >= min_margin) else NA_real_

  # Continuous geometry score: absolute primary loading and positive loading
  # margin already live on interpretable bounded loading scales. Factor
  # presence is conjunctively qualified by chance-adjusted item-partition
  # agreement when ARI is estimable. No loading cutoff is used in this score.
  continuous_salience_score <- if (length(finite_primary)) {
    mean(pmin(1, pmax(0, finite_primary)))
  } else 0
  continuous_clarity_score <- if (length(finite_margin)) {
    mean(pmin(1, pmax(0, finite_margin)))
  } else 0
  ari_quality <- suppressWarnings(as.numeric(partition_agreement$value))
  partition_quality_score <- if (is.finite(ari_quality)) {
    min(recovery_score, max(0, min(1, ari_quality)))
  } else recovery_score
  score <- pfa_harmonic_mean(c(
    partition_quality_score, continuous_salience_score, continuous_clarity_score
  ))
  factor_cor_max <- if (!is.null(pfa$phi) && is.matrix(pfa$phi) && nrow(pfa$phi) > 1L) {
    max(abs(pfa$phi[lower.tri(pfa$phi)]), na.rm = TRUE)
  } else NA_real_
  if (!is.finite(factor_cor_max)) factor_cor_max <- NA_real_
  list(
    available = TRUE,
    score = max(0, min(1, score)),
    recovery_score = max(0, min(1, recovery_score)),
    factor_presence_recovery = max(0, min(1, recovery_score)),
    factor_presence_role = "intended_factor_presence_only",
    partition_agreement_ari = partition_agreement$value,
    partition_agreement_reason = partition_agreement$reason,
    partition_agreement_role = "primary_chance_adjusted_partition_agreement_descriptor",
    salience_score = max(0, min(1, salience_score)),
    clarity_score = max(0, min(1, clarity_score)),
    salience_score_role = "threshold_attainment_descriptor",
    clarity_score_role = "threshold_attainment_descriptor",
    criterion_attainment_score = max(0, min(1, criterion_attainment_score)),
    continuous_salience_score = max(0, min(1, continuous_salience_score)),
    continuous_clarity_score = max(0, min(1, continuous_clarity_score)),
    partition_quality_score = max(0, min(1, partition_quality_score)),
    score_schema = "pfa-continuous-geometry-v2",
    score_role = "continuous_sample_free_structural_proxy",
    mean_primary_loading = mean_primary,
    min_primary_loading = min_primary,
    mean_loading_margin = mean_margin,
    primary_ge_min = primary_ge_min,
    margin_ge_min = margin_ge_min,
    recovered_factors = recovered_factors,
    missing_factors = setdiff(factors, recovered_factors),
    merged_or_duplicate_labels = names(which(table(dominant_labels) > 1L)),
    dominant_labels = dominant_labels,
    factor_mapping = mapping,
    avg_abs_loading_by_factor = avg_abs,
    loadings = loadings,
    factor_correlations = pfa$phi,
    factor_cor_max = factor_cor_max,
    sign_flips = sign_flips,
    sign_orientation = "intended_primary_mean_positive",
    item_diagnostics = item_diagnostics,
    extraction = pfa$extraction,
    requested_extraction = extraction,
    rotation = pfa$rotation %||% rotation,
    requested_rotation = pfa$requested_rotation %||% rotation,
    rotation_note = pfa$rotation_note,
    min_loading = min_loading,
    min_margin = min_margin,
    note = paste(
      "Sample-free PFA diagnostics: loadings are extracted from the semantic cosine/correlation proxy; no response-data N or global fit test is used.",
      "recovery_score/factor_presence_recovery measures intended factor presence, not item-level classification accuracy; partition_agreement_ari separately compares intended item partitions with dominant PFA components when defined.",
      "PFA axes are sign-indeterminate; reported axes are sign-anchored so each recovered intended factor has positive mean primary loadings.",
      pfa$rotation_note %||% ""
    )
  )
}

compute_pfa_unit_diagnostics <- function(embeddings, item_metadata,
                                         factors = NULL,
                                         id_col = "ID",
                                         factor_col = "Dimension",
                                         unit_col = "Facet",
                                         extraction = "ml",
                                         rotation = "promax",
                                         min_loading = 0.40,
                                         min_margin = NULL) {
  fail <- list(
    available = FALSE, score = 0, skipped = FALSE,
    unit_structure = "unknown",
    note = "Facet/unit-level PFA unavailable."
  )
  if (is.null(embeddings) || is.null(item_metadata) || !is.matrix(embeddings)) return(fail)
  if (!all(c(id_col, factor_col, unit_col) %in% names(item_metadata))) return(fail)
  ids <- as.character(item_metadata[[id_col]])
  common <- intersect(ids, rownames(embeddings))
  if (length(common) < 4L) return(fail)
  meta <- item_metadata[match(common, ids), , drop = FALSE]
  emb <- embeddings[common, , drop = FALSE]
  f_vals <- as.character(meta[[factor_col]])
  u_vals <- as.character(meta[[unit_col]])
  u_vals[is.na(u_vals) | !nzchar(u_vals)] <- f_vals[is.na(u_vals) | !nzchar(u_vals)]
  if (is.null(factors)) {
    factors <- unique(f_vals)
  } else {
    factors <- as.character(factors)
  }
  valid_model_rows <- f_vals %in% factors
  has_declared_units <- any(
    valid_model_rows &
      !is.na(u_vals) & nzchar(u_vals) &
      !is.na(f_vals) & nzchar(f_vals) &
      !identical(unit_col, factor_col) &
      u_vals != f_vals
  )
  if (!has_declared_units) {
    fail$skipped <- TRUE
    fail$unit_structure <- "none"
    fail$note <- paste(
      "Skipped because the item metadata do not define facets/units beyond",
      "the main theoretical factors."
    )
    return(fail)
  }
  unit_ids <- paste(f_vals, u_vals, sep = "::")
  units <- unique(unit_ids)
  unit_emb <- matrix(NA_real_, nrow = length(units), ncol = ncol(emb),
                     dimnames = list(units, colnames(emb)))
  unit_factor <- character(length(units))
  unit_label <- character(length(units))
  for (i in seq_along(units)) {
    idx <- which(unit_ids == units[i])
    unit_emb[i, ] <- colMeans(emb[idx, , drop = FALSE], na.rm = TRUE)
    unit_factor[i] <- f_vals[idx[1L]]
    unit_label[i] <- u_vals[idx[1L]]
  }
  norms <- sqrt(rowSums(unit_emb^2))
  ok <- is.finite(norms) & norms > .Machine$double.eps & unit_factor %in% factors
  unit_emb <- unit_emb[ok, , drop = FALSE]
  unit_factor <- unit_factor[ok]
  unit_label <- unit_label[ok]
  if (nrow(unit_emb) < length(factors) + 2L) {
    fail$unit_structure <- "detected_but_insufficient"
    fail$note <- "Facet/unit metadata detected, but too few facet/unit embeddings are available for factor extraction."
    return(fail)
  }
  units_by_factor <- table(unit_factor)
  if (length(units_by_factor) == 0L || all(units_by_factor <= 1L)) {
    fail$unit_structure <- "detected_but_single_unit_per_factor"
    fail$note <- paste(
      "Facet/unit metadata detected, but each factor has only one usable",
      "facet/unit; facet-level factor extraction is not identified."
    )
    return(fail)
  }
  unit_emb <- unit_emb / sqrt(rowSums(unit_emb^2))
  unit_cos <- tcrossprod(unit_emb)
  unit_cos <- (unit_cos + t(unit_cos)) / 2
  unit_cos[unit_cos > 1] <- 1
  unit_cos[unit_cos < -1] <- -1
  diag(unit_cos) <- 1
  fa <- stats::setNames(unit_factor, rownames(unit_cos))
  pfa <- compute_pfa_diagnostics(
    unit_cos, fa, factors,
    extraction = extraction, rotation = rotation,
    min_loading = min_loading, min_margin = min_margin
  )
  pfa$unit_metadata <- data.frame(
    unit_id = rownames(unit_cos),
    factor = unit_factor,
    unit = unit_label,
    stringsAsFactors = FALSE
  )
  pfa$unit_cosine_matrix <- unit_cos
  pfa$note <- paste(
    pfa$note %||% "",
    "Unit embeddings were computed by atomic averaging of item embeddings within facets/units."
  )
  pfa
}

pfa_diagnostics_to_dfi_params <- function(pfa_diagnostics, factor_assignment, factors,
                                          min_loading = 0.35, max_loading = 0.95,
                                          max_fcor = 0.90) {
  if (is.null(pfa_diagnostics) || !isTRUE(pfa_diagnostics$available) ||
      is.null(pfa_diagnostics$loadings) || is.null(pfa_diagnostics$factor_mapping)) {
    return(NULL)
  }
  loadings <- as.matrix(pfa_diagnostics$loadings)
  mapping <- pfa_diagnostics$factor_mapping
  factors <- as.character(factors)
  fitted_loadings <- lapply(setNames(factors, factors), function(f) {
    f_items <- intersect(names(factor_assignment[factor_assignment == f]), rownames(loadings))
    col_idx <- if (f %in% names(mapping)) suppressWarnings(as.integer(mapping[[f]])) else NA_integer_
    if (length(f_items) == 0L || !is.finite(col_idx) || col_idx < 1L || col_idx > ncol(loadings)) {
      return(numeric(0))
    }
    vals <- abs(loadings[f_items, col_idx, drop = TRUE])
    vals <- pmin(pmax(vals, min_loading), max_loading)
    as.numeric(vals)
  })
  phi <- pfa_diagnostics$factor_correlations
  fitted_factor_cors <- NULL
  idx <- suppressWarnings(as.integer(mapping[factors]))
  if (!is.null(phi) && is.matrix(phi) && length(idx) == length(factors) &&
      all(is.finite(idx)) && all(idx >= 1L) && all(idx <= nrow(phi))) {
    fitted_factor_cors <- phi[idx, idx, drop = FALSE]
    fitted_factor_cors <- as.matrix(fitted_factor_cors)
    rownames(fitted_factor_cors) <- colnames(fitted_factor_cors) <- factors
    fitted_factor_cors[!is.finite(fitted_factor_cors)] <- 0
    diag(fitted_factor_cors) <- 1
    ut <- upper.tri(fitted_factor_cors)
    fitted_factor_cors[ut] <- pmin(pmax(fitted_factor_cors[ut], -(max_fcor - 0.01)), max_fcor - 0.01)
    fitted_factor_cors[lower.tri(fitted_factor_cors)] <- t(fitted_factor_cors)[lower.tri(fitted_factor_cors)]
    diag(fitted_factor_cors) <- 1
  }
  if (all(vapply(fitted_loadings, length, integer(1L)) == 0L)) return(NULL)
  list(
    fitted_loadings = fitted_loadings,
    fitted_factor_cors = fitted_factor_cors,
    pfa_diagnostics = pfa_diagnostics,
    loading_source = "PFA-informed",
    note = "DFI fallback population uses sample-free PFA loadings and factor correlations from the semantic cosine proxy."
  )
}

build_pfa_population_correlation <- function(pfa_diagnostics, factor_assignment, factors,
                                             min_uniqueness = 0.10,
                                             max_abs_loading = 0.90,
                                             max_factor_cor = 0.90) {
  if (is.null(pfa_diagnostics) || !isTRUE(pfa_diagnostics$available) ||
      is.null(pfa_diagnostics$loadings) || is.null(pfa_diagnostics$factor_mapping)) {
    return(NULL)
  }
  factors <- as.character(factors)
  loadings_raw <- as.matrix(pfa_diagnostics$loadings)
  mapping <- suppressWarnings(as.integer(pfa_diagnostics$factor_mapping[factors]))
  if (any(!is.finite(mapping)) || any(mapping < 1L) || any(mapping > ncol(loadings_raw))) return(NULL)
  lambda <- loadings_raw[, mapping, drop = FALSE]
  colnames(lambda) <- factors
  lambda[!is.finite(lambda)] <- 0
  lambda <- pmin(pmax(lambda, -max_abs_loading), max_abs_loading)
  phi <- pfa_diagnostics$factor_correlations
  if (!is.null(phi) && is.matrix(phi) && nrow(phi) >= max(mapping)) {
    phi <- phi[mapping, mapping, drop = FALSE]
  } else {
    phi <- diag(length(factors))
  }
  phi <- as.matrix(phi)
  phi[!is.finite(phi)] <- 0
  diag(phi) <- 1
  if (nrow(phi) > 1L) {
    ut <- upper.tri(phi)
    phi[ut] <- pmin(pmax(phi[ut], -(max_factor_cor - 0.01)), max_factor_cor - 0.01)
    phi[lower.tri(phi)] <- t(phi)[lower.tri(phi)]
  }
  diag(phi) <- 1
  common <- intersect(rownames(lambda), names(factor_assignment))
  lambda <- lambda[common, , drop = FALSE]
  if (nrow(lambda) < length(factors) + 2L) return(NULL)
  sigma_common <- lambda %*% phi %*% t(lambda)
  communality <- diag(sigma_common)
  over <- is.finite(communality) & communality > (1 - min_uniqueness)
  if (any(over)) {
    scale_fac <- sqrt((1 - min_uniqueness) / pmax(communality[over], .Machine$double.eps))
    lambda[over, ] <- lambda[over, , drop = FALSE] * scale_fac
    sigma_common <- lambda %*% phi %*% t(lambda)
    communality <- diag(sigma_common)
  }
  theta <- pmax(min_uniqueness, 1 - communality)
  sigma <- sigma_common
  diag(sigma) <- diag(sigma) + theta
  d <- sqrt(pmax(diag(sigma), .Machine$double.eps))
  cor_mat <- sigma / tcrossprod(d)
  cor_mat <- (cor_mat + t(cor_mat)) / 2
  diag(cor_mat) <- 1
  rownames(cor_mat) <- colnames(cor_mat) <- rownames(lambda)
  list(cor = stabilize_correlation_matrix(cor_mat), lambda = lambda, phi = phi)
}

estimate_recommended_validation_n <- function(pfa_diagnostics, factor_assignment, factors,
                                              syntax, rotation = "geomin",
                                              rotation_args = list(geomin.epsilon = 0.50),
                                              estimator = "ML",
                                              n_grid = NULL, reps = 20L,
                                              convergence_target = 0.90,
                                              max_heywood_rate = 0.05,
                                              min_recovery = 0.90,
                                              max_primary_error = 0.10,
                                              min_dominance_recovery = NULL,
                                              max_crossloading_error = NULL,
                                              max_factor_cor_error = NULL,
                                              max_n = 2000L,
                                              iter_max = 300L,
                                              seed = NULL,
                                              verbose = FALSE,
                                              progress = verbose) {
  fail <- list(
    available = FALSE,
    recommended_n = NA_integer_,
    note = "Recommended validation N unavailable.",
    grid_results = NULL
  )
  pop <- build_pfa_population_correlation(pfa_diagnostics, factor_assignment, factors)
  if (is.null(pop)) {
    fail$note <- "PFA population correlation could not be constructed."
    return(fail)
  }
  p <- nrow(pop$cor)
  max_n <- max(p + 10L, as.integer(max_n))
  if (is.null(n_grid)) {
    n_grid <- unique(as.integer(round(c(
      max(150L, 10L * p),
      15L * p,
      20L * p,
      30L * p,
      40L * p,
      60L * p,
      max_n
    ))))
  }
  n_grid <- sort(unique(as.integer(n_grid[is.finite(n_grid) & n_grid > p + 5L & n_grid <= max_n])))
  if (length(n_grid) == 0L) {
    fail$note <- "Validation-N search grid is empty."
    return(fail)
  }
  reps <- max(5L, as.integer(reps))
  iter_max <- max(100L, as.integer(iter_max))
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }
  factors <- as.character(factors)
  target_primary <- abs(pop$lambda[, factors, drop = FALSE])
  target_primary_vec <- vapply(rownames(target_primary), function(it) {
    f <- as.character(factor_assignment[[it]])
    if (!f %in% colnames(target_primary)) return(NA_real_)
    target_primary[it, f]
  }, numeric(1L))
  target_lambda <- abs(pop$lambda[, factors, drop = FALSE])
  optional_upper_pass <- function(value, threshold) {
    if (is.null(threshold)) return(TRUE)
    is.finite(value) && value <= as.numeric(threshold[1L])
  }
  optional_lower_pass <- function(value, threshold) {
    if (is.null(threshold)) return(TRUE)
    is.finite(value) && value >= as.numeric(threshold[1L])
  }
  grid_rows <- vector("list", length(n_grid))
  for (g in seq_along(n_grid)) {
    n_obs <- n_grid[g]
    conv <- heywood <- recovery <- primary_err <- cross_err <- factor_cor_err <- numeric(reps)
    conv[] <- heywood[] <- recovery[] <- NA_real_
    primary_err[] <- cross_err[] <- factor_cor_err[] <- NA_real_
    progress_bar <- .semantica_progress_start(
      reps,
      sprintf("[VALIDATION N] Monte Carlo ESEM refits at candidate N=%d", n_obs),
      progress
    )
    for (r in seq_len(reps)) {
      s <- tryCatch(stats::rWishart(1L, df = n_obs - 1L, Sigma = pop$cor)[, , 1L] / (n_obs - 1L), error = function(e) NULL)
      if (is.null(s)) {
        .semantica_progress_update(progress_bar, r)
        next
      }
      d <- sqrt(pmax(diag(s), .Machine$double.eps))
      sample_cor <- s / tcrossprod(d)
      rownames(sample_cor) <- colnames(sample_cor) <- rownames(pop$cor)
      sample_cor <- stabilize_correlation_matrix(sample_cor)
      fit <- run_esem_on_matrix(
        syntax, sample_cor, n_obs = n_obs, estimator = estimator,
        rotation = rotation, rotation_args = rotation_args,
        iter_max = iter_max, fallback = FALSE,
        sample_cov_rescale = TRUE
      )
      converged <- !is.null(fit) && isTRUE(lavaan::lavInspect(fit, "converged"))
      conv[r] <- as.numeric(converged)
      if (!converged) {
        .semantica_progress_update(progress_bar, r)
        next
      }
      prop <- diagnose_esem_solution_propriety(fit)
      heywood[r] <- as.numeric(isTRUE(prop$improper))
      aligned_hat <- tryCatch(
        extract_aligned_esem_solution(
          fit,
          factor_assignment = factor_assignment,
          factors = factors,
          standardized = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(aligned_hat) || is.null(aligned_hat$lambda)) {
        .semantica_progress_update(progress_bar, r)
        next
      }
      lambda_hat <- aligned_hat$lambda
      item_names <- intersect(rownames(lambda_hat), names(factor_assignment))
      factor_cols <- stats::setNames(seq_along(factors), factors)
      if (anyNA(factor_cols) || length(item_names) == 0L) {
        .semantica_progress_update(progress_bar, r)
        next
      }
      correct <- vapply(item_names, function(it) {
        intended <- as.character(factor_assignment[[it]])
        if (!intended %in% names(factor_cols)) return(FALSE)
        intended_col <- factor_cols[[intended]]
        vals <- abs(lambda_hat[it, factor_cols, drop = TRUE])
        names(vals) <- factors
        intended %in% names(vals) && which.max(vals) == match(intended, names(vals)) &&
          is.finite(intended_col)
      }, logical(1L))
      recovery[r] <- mean(correct, na.rm = TRUE)
      fitted_primary <- vapply(item_names, function(it) {
        intended <- as.character(factor_assignment[[it]])
        if (!intended %in% names(factor_cols)) return(NA_real_)
        intended_col <- factor_cols[[intended]]
        if (!is.finite(intended_col)) return(NA_real_)
        abs(lambda_hat[it, intended_col])
      }, numeric(1L))
      primary_err[r] <- stats::median(abs(fitted_primary - target_primary_vec[item_names]), na.rm = TRUE)
      target_items <- intersect(item_names, rownames(target_lambda))
      if (length(target_items) > 0L) {
        fitted_abs <- abs(lambda_hat[target_items, factor_cols, drop = FALSE])
        colnames(fitted_abs) <- factors
        target_abs <- target_lambda[target_items, factors, drop = FALSE]
        cross_delta <- numeric(0L)
        for (it in target_items) {
          intended <- as.character(factor_assignment[[it]])
          other <- setdiff(factors, intended)
          if (length(other) > 0L) {
            cross_delta <- c(cross_delta, abs(fitted_abs[it, other, drop = TRUE] - target_abs[it, other, drop = TRUE]))
          }
        }
        cross_delta <- cross_delta[is.finite(cross_delta)]
        if (length(cross_delta) > 0L) cross_err[r] <- stats::median(cross_delta)
      }
      psi_hat <- aligned_hat$psi
      if (!is.null(psi_hat) && is.matrix(psi_hat) && !is.null(pop$phi) && is.matrix(pop$phi)) {
        if (!is.null(rownames(psi_hat)) && all(factors %in% rownames(psi_hat))) {
          psi_hat <- psi_hat[factors, factors, drop = FALSE]
        }
        if (nrow(psi_hat) == length(factors) && nrow(pop$phi) == length(factors)) {
          d_psi <- sqrt(pmax(diag(psi_hat), .Machine$double.eps))
          psi_cor <- psi_hat / tcrossprod(d_psi)
          diag(psi_cor) <- 1
          phi_target <- pop$phi
          if (!is.null(rownames(phi_target)) && !is.null(colnames(phi_target)) &&
              all(factors %in% rownames(phi_target)) &&
              all(factors %in% colnames(phi_target))) {
            phi_target <- phi_target[factors, factors, drop = FALSE]
          }
          cor_delta <- abs(abs(psi_cor[lower.tri(psi_cor)]) - abs(phi_target[lower.tri(phi_target)]))
          cor_delta <- cor_delta[is.finite(cor_delta)]
          if (length(cor_delta) > 0L) factor_cor_err[r] <- stats::median(cor_delta)
        }
      }
      .semantica_progress_update(progress_bar, r)
    }
    .semantica_progress_close(progress_bar)
    conv_rate <- mean(conv, na.rm = TRUE)
    hey_rate <- mean(heywood[conv == 1], na.rm = TRUE)
    rec_mean <- mean(recovery[conv == 1 & heywood == 0], na.rm = TRUE)
    err_med <- stats::median(primary_err[conv == 1 & heywood == 0], na.rm = TRUE)
    cross_med <- stats::median(cross_err[conv == 1 & heywood == 0], na.rm = TRUE)
    fcor_med <- stats::median(factor_cor_err[conv == 1 & heywood == 0], na.rm = TRUE)
    if (!is.finite(conv_rate)) conv_rate <- 0
    if (!is.finite(hey_rate)) hey_rate <- 1
    if (!is.finite(rec_mean)) rec_mean <- 0
    if (!is.finite(err_med)) err_med <- Inf
    if (!is.finite(cross_med)) cross_med <- NA_real_
    if (!is.finite(fcor_med)) fcor_med <- NA_real_
    grid_rows[[g]] <- data.frame(
      n = n_obs,
      reps = reps,
      convergence_rate = conv_rate,
      heywood_rate = hey_rate,
      loading_recovery = rec_mean,
      dominance_recovery = rec_mean,
      median_primary_loading_error = err_med,
      median_cross_loading_error = cross_med,
      median_factor_correlation_error = fcor_med,
      pass = conv_rate >= convergence_target &&
        hey_rate <= max_heywood_rate &&
        rec_mean >= min_recovery &&
        err_med <= max_primary_error &&
        optional_lower_pass(rec_mean, min_dominance_recovery) &&
        optional_upper_pass(cross_med, max_crossloading_error) &&
        optional_upper_pass(fcor_med, max_factor_cor_error),
      stringsAsFactors = FALSE
    )
    if (verbose) {
      cat(sprintf("  Validation-N grid N=%d | conv=%.2f heywood=%.2f dominance=%.2f primary err=%.3f cross err=%s factor-cor err=%s\n",
                  n_obs, conv_rate, hey_rate, rec_mean, err_med,
                  if (is.finite(cross_med)) sprintf("%.3f", cross_med) else "NA",
                  if (is.finite(fcor_med)) sprintf("%.3f", fcor_med) else "NA"))
    }
    if (isTRUE(grid_rows[[g]]$pass)) break
  }
  grid_df <- do.call(rbind, grid_rows[!vapply(grid_rows, is.null, logical(1L))])
  pass_idx <- which(grid_df$pass)
  rec_n <- if (length(pass_idx) > 0L) grid_df$n[pass_idx[1L]] else NA_integer_
  list(
    available = TRUE,
    recommended_n = as.integer(rec_n),
    population_method = "PFA-derived population correlation plus Wishart Monte Carlo ESEM refits",
    criteria = list(
      convergence_target = convergence_target,
      max_heywood_rate = max_heywood_rate,
      min_loading_recovery = min_recovery,
      max_primary_loading_error = max_primary_error,
      min_dominance_recovery = min_dominance_recovery,
      max_cross_loading_error = max_crossloading_error,
      max_factor_correlation_error = max_factor_cor_error
    ),
    grid_results = grid_df,
    reps = reps,
    note = if (is.na(rec_n)) {
      "No candidate N met all Monte Carlo criteria within the searched grid."
    } else {
      "Recommended validation N is a response-data planning diagnostic, not the semantic-proxy reference fit N."
    }
  )
}

run_esem_on_matrix <- function(syntax, cor_matrix, n_obs = 300, estimator = "ML",
                               rotation = "geomin", rotation_args = list(geomin.epsilon = 0.50),
                               iter_max = 2000L, fallback = TRUE,
                               sample_cov_rescale = FALSE,
                               return_diagnostics = FALSE) {
  last_rejection_assessment <- NULL
  rejected_attempts <- list()

  make_rejection_assessment <- function(reason, attempt = NA_integer_, fit = NULL) {
    assessment <- if (is.null(fit)) {
      assess_esem_admissibility(
        converged = FALSE, post_check = NA,
        lambda = NULL, theta = NULL, psi = NULL
      )
    } else {
      is_admissible_esem_fit(fit, return_assessment = TRUE)
    }
    reason <- as.character(reason %||% "esem_fit_rejected")
    assessment$reasons <- unique(c(assessment$reasons, reason))
    assessment$details$attempt <- suppressWarnings(as.integer(attempt[1L]))
    assessment$details$fit_error <- reason
    assessment
  }

  record_rejection <- function(assessment) {
    last_rejection_assessment <<- assessment
    rejected_attempts[[length(rejected_attempts) + 1L]] <<- list(
      attempt = assessment$details$attempt %||% NA_integer_,
      converged = assessment$details$converged %||% NA,
      reasons = assessment$reasons
    )
    invisible(NULL)
  }

  finish <- function(fit, solver_attempts_started) {
    if (!isTRUE(return_diagnostics)) return(fit)
    list(
      fit = fit,
      solver_attempts_started = as.integer(solver_attempts_started),
      accepted_attempt = if (is.null(fit)) {
        NA_integer_
      } else {
        as.integer(attr(fit, "semantica_fit_attempt") %||% NA_integer_)
      },
      rejection_assessment = if (is.null(fit)) last_rejection_assessment else NULL,
      rejected_attempts = rejected_attempts
    )
  }
  if (!is.matrix(cor_matrix)) cor_matrix <- as.matrix(cor_matrix)
  if (any(!is.finite(cor_matrix))) {
    record_rejection(make_rejection_assessment("nonfinite_correlation_matrix", 0L))
    return(finish(NULL, 0L))
  }
  if (min(eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values) < 1e-10) {
    record_rejection(make_rejection_assessment("correlation_matrix_not_positive_definite", 0L))
    return(finish(NULL, 0L))
  }

  iter_max <- max(100L, as.integer(iter_max))
  sample_cov_rescale <- isTRUE(sample_cov_rescale)
  lavaan_se <- .semantica_fast_lavaan_se(estimator)
  tag_attempt <- function(fit, attempt, assessment = NULL) {
    attr(fit, "semantica_fit_attempt") <- as.integer(attempt)
    attr(fit, "semantica_admissibility") <- assessment %||%
      is_admissible_esem_fit(fit, return_assessment = TRUE)
    fit
  }
  fit_attempt <- function(...) {
    fit_error <- NULL
    fit_warnings <- character(0)
    fit_args <- list(...)
    if (!is.null(lavaan_se) && is.null(fit_args$se)) fit_args$se <- lavaan_se
    fit <- tryCatch(
      withCallingHandlers(
        do.call(lavaan::sem, fit_args),
        warning = function(w) {
          fit_warnings <<- c(fit_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )
    list(fit = fit, error = fit_error, warnings = unique(fit_warnings))
  }
  accept_attempt <- function(fit, attempt, fit_error = NULL, fit_warnings = character(0)) {
    warning_msg <- if (length(fit_warnings) > 0L) {
      paste(unique(fit_warnings), collapse = " | ")
    } else {
      NULL
    }
    if (is.null(fit)) {
      record_rejection(make_rejection_assessment(
        fit_error %||% warning_msg %||% "lavaan_fit_failed", attempt
      ))
      return(NULL)
    }
    converged <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")), error = function(e) FALSE)
    assessment <- is_admissible_esem_fit(fit, return_assessment = TRUE)
    assessment$details$attempt <- suppressWarnings(as.integer(attempt[1L]))
    if (!is.null(fit_error)) assessment$details$fit_error <- fit_error
    if (!is.null(warning_msg)) assessment$details$fit_warning <- warning_msg
    if (!converged || !isTRUE(assessment$admissible)) {
      record_rejection(assessment)
      return(NULL)
    }
    tag_attempt(fit, attempt, assessment)
  }
  attempt1 <- fit_attempt(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = rotation, rotation.args = rotation_args, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = TRUE, control = list(iter.max = iter_max))
  accepted <- accept_attempt(attempt1$fit, 1L, attempt1$error, attempt1$warnings)
  if (!is.null(accepted)) return(finish(accepted, 1L))
  if (!isTRUE(fallback)) return(finish(NULL, 1L))

  attempt2 <- fit_attempt(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = rotation, rotation.args = rotation_args, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = FALSE, check.start = FALSE, optim.method = "BFGS", control = list(iter.max = iter_max))
  accepted <- accept_attempt(attempt2$fit, 2L, attempt2$error, attempt2$warnings)
  if (!is.null(accepted)) return(finish(accepted, 2L))

  # A requested no-rotation solution (notably the one-factor branch) must not
  # silently become a rotated solution during numerical fallback. For the
  # multidimensional geomin/oblimin pair, retain the historical alternate-
  # rotation rescue attempt.
  if (identical(rotation, "none")) return(finish(NULL, 2L))
  alt_rotation <- if (rotation == "geomin") "oblimin" else "geomin"
  attempt3 <- fit_attempt(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = alt_rotation, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = FALSE, control = list(iter.max = iter_max))
  accepted <- accept_attempt(attempt3$fit, 3L, attempt3$error, attempt3$warnings)
  if (!is.null(accepted)) return(finish(accepted, 3L))
  finish(NULL, 3L)
}

.semantica_attach_esem_rejection <- function(fit_result, esem_run) {
  assessment <- esem_run$rejection_assessment
  if (is.null(assessment)) return(fit_result)
  fit_result$admissibility <- assessment
  fit_result$converged <- isTRUE(assessment$details$converged)
  fit_result$admissible <- FALSE
  fit_result
}

run_esem_on_response_data <- function(syntax, data, selected_items,
                                      estimator = "ML",
                                      rotation = "geomin",
                                      rotation_args = list(geomin.epsilon = 0.50),
                                      ordered = NULL,
                                      iter_max = 2000L,
                                      fallback = TRUE) {
  if (is.null(data) || is.null(selected_items)) return(NULL)
  data <- as.data.frame(data)
  selected_items <- as.character(selected_items)
  if (!all(selected_items %in% names(data))) return(NULL)
  dat <- data[, selected_items, drop = FALSE]
  iter_max <- max(100L, as.integer(iter_max))
  ordered <- if (is.null(ordered)) NULL else intersect(as.character(ordered), selected_items)
  if (length(ordered) == 0L) ordered <- NULL
  lavaan_se <- .semantica_fast_lavaan_se(estimator)

  fit_args <- list(
    model = syntax, data = dat, estimator = estimator,
    rotation = rotation, rotation.args = rotation_args,
    warn = FALSE, check.post = TRUE,
    control = list(iter.max = iter_max)
  )
  if (!is.null(lavaan_se)) fit_args$se <- lavaan_se
  if (!is.null(ordered)) fit_args$ordered <- ordered
  fit <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                  error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit) && is_admissible_esem_fit(fit)) return(fit)
  if (!isTRUE(fallback)) return(NULL)

  fit_args$check.post <- FALSE
  fit_args$check.start <- FALSE
  fit_args$optim.method <- "BFGS"
  fit2 <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                   error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit2) && is_admissible_esem_fit(fit2)) return(fit2)

  if (identical(rotation, "none")) return(NULL)
  fit_args$rotation <- if (rotation == "geomin") "oblimin" else "geomin"
  fit_args$rotation.args <- list()
  fit_args$optim.method <- NULL
  fit3 <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                   error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit3) && is_admissible_esem_fit(fit3)) return(fit3)
  NULL
}

compute_response_cor <- function(data, selected_items) {
  tryCatch({
    data <- as.data.frame(data)
    selected_items <- intersect(as.character(selected_items), names(data))
    if (length(selected_items) < 2L) return(NULL)
    out <- suppressWarnings(stats::cor(data[, selected_items, drop = FALSE], use = "pairwise.complete.obs"))
    if (!is.matrix(out) || any(!is.finite(out))) return(NULL)
    stabilize_correlation_matrix(out)
  }, error = function(e) NULL)
}

# =================================================================
# 4, 5, 6, 7  MANUAL SRMR, AVE, HTMT, SCORING
# =================================================================
compute_manual_srmr <- function(esem_fit, observed_cor) {
  tryCatch({
    implied <- lavaan::fitted(esem_fit)$cov
    common  <- intersect(rownames(observed_cor), rownames(implied))
    if (length(common) < 2L) return(NA_real_)
    implied <- implied[common, common, drop = FALSE]
    d <- sqrt(diag(implied))
    if (any(!is.finite(d)) || any(d <= 0)) return(NA_real_)
    implied_cor <- implied / tcrossprod(d)
    diag(implied_cor) <- 1
    resid <- observed_cor[common, common, drop = FALSE] - implied_cor
    sqrt(mean(resid[lower.tri(resid)]^2))
  }, error = function(e) NA_real_)
}

compute_ave_esem <- function(esem_fit, factor_assignment, factors) {
  tryCatch({
    aligned <- extract_aligned_esem_solution(
      esem_fit, factor_assignment = factor_assignment, factors = factors,
      standardized = TRUE
    )
    lambda_mat <- aligned$lambda
    theta_mat <- aligned$theta
    factor_ave <- numeric(0)
    warnings <- character(0)
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f]); f_items <- intersect(f_items, rownames(lambda_mat))
      if (length(f_items) == 0L) next
      dom_load <- as.numeric(lambda_mat[f_items, f, drop = TRUE])
      if (any(abs(dom_load) > 1 + 1e-6, na.rm = TRUE)) {
        warnings <- c(warnings, sprintf("%s: standardized dominant loading outside [-1, 1]", f))
      }
      if (!is.null(theta_mat) && all(f_items %in% rownames(theta_mat))) {
        err <- diag(theta_mat[f_items, f_items, drop = FALSE])
        if (any(err < -1e-6, na.rm = TRUE)) {
          warnings <- c(warnings, sprintf("%s: negative standardized residual variance detected", f))
        }
      }
      # ESEM AVE is reported as a conservative dominant-loading AVE. This avoids
      # impossible AVE > 1 values from improper residual variances in rotated ESEM.
      dom_sq <- pmin(pmax(dom_load^2, 0), 1)
      factor_ave[f] <- mean(dom_sq, na.rm = TRUE)
    }
    if (length(factor_ave) == 0L) return(NA_real_)
    out <- min(factor_ave, na.rm = TRUE)
    attr(out, "factor_ave") <- factor_ave
    attr(out, "ave_method") <- "dominant_standardized_loading_mean_square"
    attr(out, "ave_warnings") <- unique(warnings)
    out
  }, error = function(e) NA_real_)
}

compute_htmt_esem <- function(esem_fit, factors, threshold = 0.85,
                              observed_cor = NULL, factor_assignment = NULL) {
  factors <- unique(as.character(factors %||% character(0L)))
  if (length(factors) < 2L) {
    return(list(
      max_cor = NA_real_, violations = NA_integer_, values = numeric(0L),
      method = "not_applicable_unidimensional", status = "not_applicable",
      reason = "HTMT requires at least two distinct constructs."
    ))
  }
  tryCatch({
    if (!is.null(observed_cor) && !is.null(factor_assignment)) {
      observed_cor <- as.matrix(observed_cor)
      observed_cor <- (observed_cor + t(observed_cor)) / 2
      diag(observed_cor) <- 1
      htmt_vals <- numeric(0)

      for (a in seq_len(max(0L, length(factors) - 1L))) {
        for (b in (a + 1L):length(factors)) {
          items_a <- intersect(names(factor_assignment[factor_assignment == factors[a]]), rownames(observed_cor))
          items_b <- intersect(names(factor_assignment[factor_assignment == factors[b]]), rownames(observed_cor))
          if (length(items_a) < 2L || length(items_b) < 2L) next

          heterotrait <- abs(as.vector(observed_cor[items_a, items_b, drop = FALSE]))
          monotrait_a <- abs(observed_cor[items_a, items_a, drop = FALSE][lower.tri(observed_cor[items_a, items_a, drop = FALSE])])
          monotrait_b <- abs(observed_cor[items_b, items_b, drop = FALSE][lower.tri(observed_cor[items_b, items_b, drop = FALSE])])
          denom <- sqrt(mean(monotrait_a, na.rm = TRUE) * mean(monotrait_b, na.rm = TRUE))
          htmt_vals <- c(htmt_vals, if (!is.finite(denom) || denom <= .Machine$double.eps) Inf else mean(heterotrait, na.rm = TRUE) / denom)
        }
      }

      if (length(htmt_vals) == 0L) return(list(
        # Preserve the historical multidimensional numeric contract while
        # exposing the evidence state explicitly. The unidimensional case is
        # handled before this branch and returns NA/not-applicable.
        max_cor = 0.0, violations = 0L, values = htmt_vals,
        method = "item_htmt", status = "unavailable",
        reason = "No eligible factor pair had enough indicators for HTMT."
      ))
      return(list(max_cor = max(htmt_vals, na.rm = TRUE),
                  violations = sum(htmt_vals > threshold, na.rm = TRUE),
                  values = htmt_vals, method = "item_htmt", status = "computed", reason = NULL))
    }

    # Backward-compatible fallback: this is a latent-factor correlation check,
    # not HTMT, and is only used when item-level information is unavailable.
    psi_mat <- lavaan::lavInspect(esem_fit, "est")$psi
    if (is.null(psi_mat) || !is.matrix(psi_mat)) return(list(max_cor = 1.0, violations = Inf))
    d_inv  <- diag(1 / sqrt(diag(psi_mat)), nrow(psi_mat))
    cor_lv <- d_inv %*% psi_mat %*% d_inv; diag(cor_lv) <- 1.0
    ut <- upper.tri(cor_lv); cors <- abs(cor_lv[ut])
    list(max_cor = if (length(cors) > 0L) max(cors) else 0.0,
         violations = if (length(cors) > 0L) sum(cors > threshold) else 0L,
         values = cors, method = "latent_correlation_fallback",
         status = if (length(cors) > 0L) "computed" else "unavailable", reason = NULL)
  }, error = function(e) list(
    # Preserve 0.4.0's conservative multidimensional failure numerics so this
    # unidimensional extension does not alter established ACO/ESEM scoring.
    max_cor = 1.0, violations = Inf, values = numeric(0L),
    method = "failed", status = "unavailable", reason = conditionMessage(e)
  ))
}

compute_esem_structure_diagnostics <- function(esem_fit, observed_cor = NULL,
                                               factor_assignment = NULL,
                                               factors = NULL,
                                               primary_min = 0.40,
                                               cross_max = 0.30) {
  empty <- list(
    n_factors = NA_integer_, dimensionality_mode = NA_character_,
    mean_primary_loading = NA_real_, median_primary_loading = NA_real_,
    min_primary_loading = NA_real_, primary_ge_40 = NA_real_,
    primary_ge_50 = NA_real_, mean_max_cross_loading = NA_real_,
    q90_max_cross_loading = NA_real_, max_cross_loading = NA_real_,
    no_large_cross_loading = NA_real_, correct_dominance = NA_real_,
    simple_structure = NA_real_, mean_salience_ratio = NA_real_,
    median_salience_ratio = NA_real_, mean_complexity = NA_real_,
    max_complexity = NA_real_, max_abs_residual = NA_real_,
    mean_abs_residual = NA_real_, q95_abs_residual = NA_real_,
    mean_residual = NA_real_, max_centered_residual = NA_real_,
    q95_centered_residual = NA_real_, max_abs_centered_residual = NA_real_,
    top_centered_residual_pairs = NULL,
    latent_cor_max = NA_real_, factor_score_determinacy = NA_real_,
    omega_dominant = NULL, item_diagnostics = NULL, factor_diagnostics = NULL,
    alignment = NULL,
    note = "ESEM structure diagnostics unavailable."
  )
  if (is.null(esem_fit) || is.null(factor_assignment) || is.null(factors)) return(empty)

  aligned <- tryCatch(
    extract_aligned_esem_solution(
      esem_fit, factor_assignment = factor_assignment, factors = factors,
      standardized = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(aligned)) return(empty)
  lambda_mat <- aligned$lambda
  valid_factors <- unique(as.character(factors))
  is_unidimensional <- length(valid_factors) == 1L
  factor_cols <- stats::setNames(seq_along(valid_factors), valid_factors)

  item_ids <- intersect(names(factor_assignment), rownames(lambda_mat))
  item_rows <- vector("list", length(item_ids))
  n_rows <- 0L
  for (item_id in item_ids) {
    assigned <- as.character(factor_assignment[[item_id]])
    assigned_col <- factor_cols[valid_factors == assigned]
    if (length(assigned_col) == 0L || is.na(assigned_col)) next
    loads <- as.numeric(lambda_mat[item_id, factor_cols, drop = TRUE])
    abs_loads <- abs(loads)
    primary <- abs(as.numeric(lambda_mat[item_id, assigned_col[1L], drop = TRUE]))
    cross <- abs_loads[valid_factors != assigned]
    max_cross <- if (length(cross) > 0L) max(cross, na.rm = TRUE) else NA_real_
    dominant_idx <- if (all(!is.finite(abs_loads))) NA_integer_ else which.max(abs_loads)
    dominant <- if (!is.na(dominant_idx)) valid_factors[[dominant_idx]] else NA_character_
    denom <- sum(abs_loads^4, na.rm = TRUE)
    complexity <- if (is.finite(denom) && denom > .Machine$double.eps) {
      (sum(abs_loads^2, na.rm = TRUE)^2) / denom
    } else NA_real_
    salience_ratio <- if (is_unidimensional) NA_real_ else primary / max(max_cross, .Machine$double.eps)
    issue <- character(0)
    if (!is.finite(primary) || primary < primary_min) issue <- c(issue, "weak_primary")
    if (!is_unidimensional && is.finite(max_cross) && max_cross > cross_max) issue <- c(issue, "large_cross_loading")
    if (!is_unidimensional && !identical(dominant, assigned)) issue <- c(issue, "dominant_factor_mismatch")
    n_rows <- n_rows + 1L
    item_rows[[n_rows]] <- data.frame(
      ID = item_id,
      assigned_factor = assigned,
      dominant_factor = dominant,
      primary_loading = primary,
      max_cross_loading = max_cross,
      salience_ratio = salience_ratio,
      complexity = complexity,
      simple_structure = if (is_unidimensional) {
        is.finite(primary) && primary >= primary_min
      } else {
        is.finite(primary) && primary >= primary_min &&
          is.finite(max_cross) && max_cross <= cross_max &&
          identical(dominant, assigned)
      },
      issue = if (length(issue) == 0L) "none" else paste(issue, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  item_diag <- if (n_rows > 0L) do.call(rbind, item_rows[seq_len(n_rows)]) else NULL
  if (is.null(item_diag) || nrow(item_diag) == 0L) return(empty)

  factor_diag_safe_mean <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x)) mean(x) else NA_real_
  }
  factor_diag_safe_min <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x)) min(x) else NA_real_
  }
  factor_diag_safe_prop <- function(x) {
    x <- as.logical(x)
    x <- x[!is.na(x)]
    if (length(x)) mean(x) else NA_real_
  }

  factor_diag <- do.call(rbind, lapply(valid_factors, function(f) {
    z <- item_diag[item_diag$assigned_factor == f, , drop = FALSE]
    if (nrow(z) == 0L) {
      return(data.frame(
        factor = f, n_items = 0L, mean_primary_loading = NA_real_,
        min_primary_loading = NA_real_, mean_max_cross_loading = NA_real_,
        correct_dominance = NA_real_, simple_structure = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      factor = f,
      n_items = nrow(z),
      mean_primary_loading = factor_diag_safe_mean(z$primary_loading),
      min_primary_loading = factor_diag_safe_min(z$primary_loading),
      mean_max_cross_loading = if (is_unidimensional) NA_real_ else factor_diag_safe_mean(z$max_cross_loading),
      correct_dominance = if (is_unidimensional) NA_real_ else factor_diag_safe_prop(z$assigned_factor == z$dominant_factor),
      simple_structure = if (is_unidimensional) NA_real_ else factor_diag_safe_prop(z$simple_structure),
      stringsAsFactors = FALSE
    )
  }))
  rownames(factor_diag) <- NULL

  residual_stats <- list(
    max_abs_residual = NA_real_, mean_abs_residual = NA_real_, q95_abs_residual = NA_real_,
    mean_residual = NA_real_, max_centered_residual = NA_real_, q95_centered_residual = NA_real_,
    max_abs_centered_residual = NA_real_, top_centered_residual_pairs = NULL
  )
  if (!is.null(observed_cor)) {
    residual_stats <- tryCatch({
      implied <- lavaan::fitted(esem_fit)$cov
      common <- intersect(rownames(observed_cor), rownames(implied))
      if (length(common) >= 2L) {
        implied <- implied[common, common, drop = FALSE]
        d <- sqrt(pmax(diag(implied), .Machine$double.eps))
        implied_cor <- implied / tcrossprod(d)
        diag(implied_cor) <- 1
        resid <- observed_cor[common, common, drop = FALSE] - implied_cor
        idx <- which(lower.tri(resid), arr.ind = TRUE)
        signed_vals <- resid[lower.tri(resid)]
        keep <- is.finite(signed_vals)
        signed_vals <- signed_vals[keep]
        idx <- idx[keep, , drop = FALSE]
        if (length(signed_vals) > 0L) {
          abs_vals <- abs(signed_vals)
          mean_signed <- mean(signed_vals)
          centered <- signed_vals - mean_signed
          ord <- order(centered, decreasing = TRUE)
          top_n <- min(5L, length(ord))
          top_pairs <- if (top_n > 0L) {
            take <- ord[seq_len(top_n)]
            data.frame(
              item_i = common[idx[take, 1L]],
              item_j = common[idx[take, 2L]],
              residual = signed_vals[take],
              centered_residual = centered[take],
              stringsAsFactors = FALSE
            )
          } else NULL
          list(
            max_abs_residual = max(abs_vals),
            mean_abs_residual = mean(abs_vals),
            q95_abs_residual = as.numeric(stats::quantile(abs_vals, 0.95, na.rm = TRUE, names = FALSE)),
            mean_residual = mean_signed,
            max_centered_residual = max(centered),
            q95_centered_residual = as.numeric(stats::quantile(centered, 0.95, na.rm = TRUE, names = FALSE)),
            max_abs_centered_residual = max(abs(centered)),
            top_centered_residual_pairs = top_pairs
          )
        } else residual_stats
      } else residual_stats
    }, error = function(e) residual_stats)
  }

  latent_cor_max <- tryCatch({
    psi <- aligned$psi
    d <- sqrt(pmax(diag(psi), .Machine$double.eps))
    cor_lv <- psi / tcrossprod(d)
    vals <- abs(cor_lv[upper.tri(cor_lv)])
    if (length(vals) > 0L) max(vals, na.rm = TRUE) else NA_real_
  }, error = function(e) NA_real_)
  factor_score_determinacy <- tryCatch({
    det <- lavaan::lavInspect(esem_fit, "fs.determinacy")
    det <- as.numeric(det)
    if (length(det) > 0L) mean(det[is.finite(det)], na.rm = TRUE) else NA_real_
  }, error = function(e) NA_real_)

  theta_diag <- tryCatch(diag(aligned$theta), error = function(e) NULL)
  omega_dominant <- NULL
  if (!is.null(theta_diag)) {
    omega_dominant <- vapply(valid_factors, function(f) {
      f_items <- intersect(names(factor_assignment[factor_assignment == f]), rownames(lambda_mat))
      f_col <- factor_cols[valid_factors == f]
      if (length(f_items) == 0L || length(f_col) == 0L) return(NA_real_)
      lam <- as.numeric(lambda_mat[f_items, f_col[1L], drop = TRUE])
      med_lam <- stats::median(lam, na.rm = TRUE)
      orient <- if (is.finite(med_lam) && med_lam < 0) -1 else 1
      lam <- lam * orient
      err <- theta_diag[f_items]
      num <- sum(lam, na.rm = TRUE)^2
      den <- num + sum(pmax(err, 0), na.rm = TRUE)
      if (!is.finite(den) || den <= .Machine$double.eps) NA_real_ else max(0, min(1, num / den))
    }, numeric(1L))
  }

  finite_salience <- item_diag$salience_ratio[is.finite(item_diag$salience_ratio)]
  list(
    n_factors = length(valid_factors),
    dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
    mean_primary_loading = mean(item_diag$primary_loading, na.rm = TRUE),
    median_primary_loading = stats::median(item_diag$primary_loading, na.rm = TRUE),
    min_primary_loading = min(item_diag$primary_loading, na.rm = TRUE),
    primary_ge_40 = mean(item_diag$primary_loading >= 0.40, na.rm = TRUE),
    primary_ge_50 = mean(item_diag$primary_loading >= 0.50, na.rm = TRUE),
    mean_max_cross_loading = if (is_unidimensional) NA_real_ else mean(item_diag$max_cross_loading, na.rm = TRUE),
    q90_max_cross_loading = if (is_unidimensional) NA_real_ else as.numeric(stats::quantile(item_diag$max_cross_loading, 0.90, na.rm = TRUE, names = FALSE)),
    max_cross_loading = if (is_unidimensional) NA_real_ else max(item_diag$max_cross_loading, na.rm = TRUE),
    no_large_cross_loading = if (is_unidimensional) NA_real_ else mean(item_diag$max_cross_loading <= cross_max, na.rm = TRUE),
    correct_dominance = if (is_unidimensional) NA_real_ else mean(item_diag$assigned_factor == item_diag$dominant_factor, na.rm = TRUE),
    simple_structure = if (is_unidimensional) NA_real_ else mean(item_diag$simple_structure, na.rm = TRUE),
    mean_salience_ratio = if (is_unidimensional || !length(finite_salience)) NA_real_ else mean(finite_salience),
    median_salience_ratio = if (is_unidimensional || !length(finite_salience)) NA_real_ else stats::median(finite_salience),
    mean_complexity = mean(item_diag$complexity, na.rm = TRUE),
    max_complexity = max(item_diag$complexity, na.rm = TRUE),
    max_abs_residual = residual_stats$max_abs_residual,
    mean_abs_residual = residual_stats$mean_abs_residual,
    q95_abs_residual = residual_stats$q95_abs_residual,
    mean_residual = residual_stats$mean_residual,
    max_centered_residual = residual_stats$max_centered_residual,
    q95_centered_residual = residual_stats$q95_centered_residual,
    max_abs_centered_residual = residual_stats$max_abs_centered_residual,
    top_centered_residual_pairs = residual_stats$top_centered_residual_pairs,
    latent_cor_max = latent_cor_max,
    factor_score_determinacy = factor_score_determinacy,
    omega_dominant = omega_dominant,
    item_diagnostics = item_diag,
    factor_diagnostics = factor_diag,
    alignment = aligned$diagnostics %||% list(
      mapping = aligned$mapping,
      assignment_method = aligned$assignment_method,
      globally_optimal_assignment = aligned$globally_optimal_assignment
    ),
    note = if (is_unidimensional) paste(
      "Diagnostics are computed on a one-factor semantic-proxy correlation model; they do not establish empirical unidimensionality.",
      "Cross-loading, dominance, and HTMT concepts are not used for a single factor; loading strength and residual reproduction remain informative proxy diagnostics.",
      "Centered residual summaries are descriptive local-dependence-like diagnostics and intentionally have no universal pass/fail cutoff."
    ) else paste(
      "Diagnostics are computed on the ESEM semantic-proxy correlation model; they assess factorial clarity, not observed response validity.",
      "Factor-level summaries aggregate the existing item-level loading, dominance, and simple-structure diagnostics without introducing new cutoffs."
    )
  )
}

extract_and_score_esem <- function(esem_fit, observed_cor = NULL, factor_assignment = NULL, factors = NULL,
                                   cutoffs = list(cfi = 0.95, tli = 0.95, rmsea = 0.06, srmr = 0.08),
                                   htmt_threshold = 0.85, verbose_decomp = FALSE,
                                   score_mode = c("current", "structure_weighted"),
                                   htmt_objective_role = c("diagnostic", "penalty")) {
  score_mode <- match.arg(score_mode)
  htmt_objective_role <- match.arg(htmt_objective_role)
  policy <- .semantica_decision_policy()
  esem_policy <- policy$esem
  factors <- unique(as.character(factors %||% character(0L)))
  is_unidimensional <- length(factors) == 1L
  fail <- list(cfi = NA, tli = NA, rmsea = NA, srmr = NA, ave = NA,
               factor_ave = NULL, ave_method = NA_character_, ave_warnings = character(0),
               htmt_max = NA, htmt_violations = Inf, loading_quality = 0,
               structure_diagnostics = NULL, alignment = NULL,
               converged = FALSE, admissible = FALSE,
               admissibility = NULL, score = 0, score_decomp = NULL)
  if (is.null(esem_fit)) return(fail)
  admissibility <- is_admissible_esem_fit(esem_fit, return_assessment = TRUE)
  fail$admissibility <- admissibility
  fail$converged <- isTRUE(admissibility$details$converged)
  if (!isTRUE(admissibility$admissible)) return(fail)
  aligned_solution <- tryCatch(
    extract_aligned_esem_solution(
      esem_fit, factor_assignment = factor_assignment, factors = factors,
      standardized = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(aligned_solution)) {
    fail$admissibility$admissible <- FALSE
    fail$admissibility$reasons <- unique(c(
      fail$admissibility$reasons, "factor_alignment_failed"
    ))
    return(fail)
  }

  fm <- tryCatch(lavaan::fitMeasures(esem_fit, c("cfi", "tli", "rmsea", "srmr")), error = function(e) c(cfi = NA, tli = NA, rmsea = NA, srmr = NA))
  cfi <- as.numeric(fm["cfi"]); tli <- as.numeric(fm["tli"]); rmsea <- as.numeric(fm["rmsea"]); srmr <- as.numeric(fm["srmr"])
  if ((is.na(srmr) || !is.finite(srmr)) && !is.null(observed_cor)) srmr <- compute_manual_srmr(esem_fit, observed_cor)
  cfi <- max(0, min(1, if (is.na(cfi) || !is.finite(cfi)) 0 else cfi))
  tli <- max(0, min(1, if (is.na(tli) || !is.finite(tli)) 0 else tli))
  rmsea <- max(0, if (is.na(rmsea) || !is.finite(rmsea)) 1 else rmsea)
  srmr <- max(0, if (is.na(srmr) || !is.finite(srmr)) 1 else srmr)

  loading_quality <- tryCatch({
    lambda_mat <- aligned_solution$lambda
    if (is.null(lambda_mat) || !is.matrix(lambda_mat) || is.null(factor_assignment) || is.null(factors)) return(0.5)
    dom_loads <- numeric(0)
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f]); f_items <- intersect(f_items, rownames(lambda_mat))
      if (length(f_items) == 0L) next
      dom_loads <- c(dom_loads, abs(lambda_mat[f_items, f, drop = TRUE]))
    }
    if (length(dom_loads) == 0L) return(0.5)
    lqp <- esem_policy$loading_quality
    in_range <- mean(dom_loads >= lqp$reference_lower & dom_loads <= lqp$reference_upper)
    mean_lam <- mean(dom_loads[dom_loads > 0], na.rm = TRUE)
    lam_qual <- 1 - abs(mean_lam - lqp$reference_center) / max(lqp$reference_center, 1e-6)
    lam_qual <- max(0, min(1, lam_qual))
    lqp$in_range_weight * in_range + lqp$centrality_weight * lam_qual
  }, error = function(e) 0.5)

  ave <- compute_ave_esem(esem_fit, factor_assignment, factors)
  factor_ave <- attr(ave, "factor_ave", exact = TRUE)
  ave_warnings <- attr(ave, "ave_warnings", exact = TRUE)
  ave_method <- attr(ave, "ave_method", exact = TRUE)
  ave_num <- if (!is.na(ave) && is.finite(ave)) unname(as.numeric(ave)) else NA_real_
  ave_score <- if (!is.na(ave_num) && is.finite(ave_num)) min(1.0, ave_num / esem_policy$ave_reference) else 0.5
  htmt_result <- compute_htmt_esem(esem_fit, factors, htmt_threshold, observed_cor, factor_assignment)
  htmt_violations_num <- suppressWarnings(as.numeric(htmt_result$violations[1L] %||% NA_real_))
  htmt_reference_exceeded <- if (is_unidimensional) {
    NA
  } else {
    htmt_max_num <- suppressWarnings(as.numeric(htmt_result$max_cor[1L] %||% NA_real_))
    is.finite(htmt_max_num) && is.finite(htmt_threshold) && htmt_max_num > htmt_threshold
  }
  htmt_penalty <- if (is_unidimensional || identical(htmt_objective_role, "diagnostic")) {
    # HTMT has no one-factor meaning, and in the default multidimensional mode
    # it is a descriptive semantic-overlap diagnostic rather than a validity
    # gate.  This avoids importing a fixed participant-SEM cutoff into the
    # optimization utility of an embedding-derived proxy matrix.
    NA_real_
  } else if (!is.finite(htmt_violations_num)) {
    # Compatibility/sensitivity mode: preserve the historical conservative
    # multidimensional failure penalty only when explicitly requested.
    0.60
  } else if (htmt_violations_num > 0) {
    max(0.60, 1 - 0.15 * htmt_violations_num)
  } else 1.0
  htmt_multiplier <- if (is.na(htmt_penalty)) 1.0 else htmt_penalty
  structure_diagnostics <- compute_esem_structure_diagnostics(
    esem_fit = esem_fit,
    observed_cor = observed_cor,
    factor_assignment = factor_assignment,
    factors = factors
  )

  cfi_s <- min(1.0, cfi / cutoffs$cfi)
  rmsea_s <- min(1.0, cutoffs$rmsea / max(rmsea, 1e-6))
  srmr_s <- min(1.0, cutoffs$srmr / max(srmr, 1e-6))
  current_w <- esem_policy$current_weights
  fit_component <- current_w[["cfi"]] * cfi_s + current_w[["rmsea"]] * rmsea_s +
    current_w[["srmr"]] * srmr_s + current_w[["ave"]] * ave_score
  safe01 <- function(x, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x[1L]))
    if (!is.finite(x)) default else max(0, min(1, x))
  }
  residual_structure_score <- if (!is.null(structure_diagnostics)) {
    resid <- suppressWarnings(as.numeric(structure_diagnostics$mean_abs_residual[1L]))
    if (is.finite(resid)) max(0, min(1, 1 - resid / esem_policy$residual_reference)) else NA_real_
  } else NA_real_
  structure_values <- if (is_unidimensional) {
    # With one factor, dominance/cross-loading criteria are vacuous and would
    # artificially inflate structural quality. Use only criteria that remain
    # defined: loading salience and one-factor residual reproduction.
    c(
      primary_ge_40 = safe01(structure_diagnostics$primary_ge_40 %||% NA_real_),
      primary_ge_50 = safe01(structure_diagnostics$primary_ge_50 %||% NA_real_),
      residual_reproduction = residual_structure_score
    )
  } else {
    c(
      primary_ge_40 = safe01(structure_diagnostics$primary_ge_40 %||% NA_real_),
      correct_dominance = safe01(structure_diagnostics$correct_dominance %||% NA_real_),
      simple_structure = safe01(structure_diagnostics$simple_structure %||% NA_real_),
      cross_loading_control = safe01(structure_diagnostics$no_large_cross_loading %||% NA_real_),
      residual_reproduction = residual_structure_score
    )
  }
  structure_values <- structure_values[is.finite(structure_values)]
  structure_component <- if (length(structure_values) > 0L) mean(structure_values) else 0.5
  base_score <- if (score_mode == "structure_weighted") {
    # Keep global fit in the score while giving semantic-proxy structure
    # diagnostics more influence than N-sensitive fit indexes.
    fit_w <- esem_policy$structure_weighted_fit
    top_w <- esem_policy$structure_weighted_top
    fit_core <- fit_w[["cfi"]] * cfi_s + fit_w[["rmsea"]] * rmsea_s + fit_w[["srmr"]] * srmr_s
    top_w[["fit"]] * fit_core + top_w[["ave"]] * ave_score + top_w[["structure"]] * structure_component
  } else {
    fit_component
  }
  score <- if (is_unidimensional) {
    base_score * loading_quality
  } else {
    base_score * loading_quality * htmt_multiplier
  }

  logistic_score <- function(x, center, steepness, higher_is_better = TRUE) {
    if (higher_is_better) 1 / (1 + exp(-steepness * (x - center))) else 1 / (1 + exp(steepness * (x - center)))
  }
  cfi_logistic <- logistic_score(cfi, center = cutoffs$cfi, steepness = 30, higher_is_better = TRUE)
  rmsea_logistic <- logistic_score(rmsea, center = cutoffs$rmsea, steepness = 30, higher_is_better = FALSE)
  srmr_logistic <- logistic_score(srmr, center = cutoffs$srmr, steepness = 30, higher_is_better = FALSE)

  score_decomp <- list(cfi_s = cfi_s, rmsea_s = rmsea_s, srmr_s = srmr_s, ave_score = ave_score,
                       loading_quality = loading_quality, htmt_penalty = htmt_penalty,
                       htmt_objective_role = htmt_objective_role,
                       htmt_reference = htmt_threshold,
                       htmt_reference_exceeded = htmt_reference_exceeded,
                       htmt_status = htmt_result$status %||% NA_character_,
                       decision_policy_schema = policy$schema_version,
                       objective_schema_version = policy$objective_schema_version,
                       weight_policy_origin = policy$policy_origin,
                       component_weights = list(
                         current = unname(policy$esem$current_weights),
                         current_names = names(policy$esem$current_weights),
                         loading_quality = c(
                           in_range = policy$esem$loading_quality$in_range_weight,
                           centrality = policy$esem$loading_quality$centrality_weight
                         ),
                         structure_weighted_fit = policy$esem$structure_weighted_fit,
                         structure_weighted_top = policy$esem$structure_weighted_top
                       ),
                       dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
                       base_score = base_score, final_score = max(0, min(1, score)),
                       score_mode = score_mode, fit_component = fit_component,
                       structure_component = structure_component,
                       structure_component_values = structure_values,
                       cfi_logistic = cfi_logistic, rmsea_logistic = rmsea_logistic, srmr_logistic = srmr_logistic,
                       factor_ave = factor_ave, ave_method = ave_method, ave_warnings = ave_warnings,
                       structure_diagnostics = structure_diagnostics)

  if (verbose_decomp) {
    cat("  [ESEM SCORE DECOMPOSITION]\n")
    cat(sprintf("    CFI=%.4f rm_s=%.4f log_s=%.4f | RMSEA=%.4f rm_s=%.4f log_s=%.4f | SRMR=%.4f rm_s=%.4f log_s=%.4f\n",
                cfi, cfi_s, cfi_logistic, rmsea, rmsea_s, rmsea_logistic, srmr, srmr_s, srmr_logistic))
    if (is_unidimensional) {
      cat(sprintf("    AVE=%.4f ave_s=%.4f | LQ=%.4f | HTMT_pen=N/A | Base=%.4f | Final=%.4f\n",
                  ave_num, ave_score, loading_quality, base_score, score))
    } else {
      cat(sprintf("    AVE=%.4f ave_s=%.4f | LQ=%.4f | HTMT_pen=%.4f | Base=%.4f | Final=%.4f\n",
                  ave_num, ave_score, loading_quality, htmt_penalty, base_score, score))
    }
  }

  list(cfi = cfi, tli = tli, rmsea = rmsea, srmr = srmr, ave = ave_num,
       factor_ave = factor_ave, ave_method = ave_method, ave_warnings = ave_warnings,
       htmt_max = htmt_result$max_cor, htmt_violations = htmt_result$violations,
       htmt_status = htmt_result$status %||% NA_character_,
       htmt_objective_role = htmt_objective_role,
       htmt_reference_exceeded = htmt_reference_exceeded,
       dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
       loading_quality = loading_quality, structure_diagnostics = structure_diagnostics,
       alignment = aligned_solution$diagnostics %||% aligned_solution$mapping,
       converged = TRUE, admissible = TRUE, admissibility = admissibility,
       fit_attempt = attr(esem_fit, "semantica_fit_attempt") %||% NA_integer_,
       score = max(0, min(1, score)), score_decomp = score_decomp)
}

build_semantic_reference_n_grid <- function(reference_n_info, n_grid = NULL,
                                            multipliers = c(0.5, 1, 1.5, 2),
                                            min_n = NULL, max_n = NULL) {
  ref_n <- suppressWarnings(as.numeric(reference_n_info$used_n_obs %||% reference_n_info$n_obs))
  p <- suppressWarnings(as.numeric(reference_n_info$p %||% NA_real_))
  lower <- if (!is.null(min_n)) {
    suppressWarnings(as.integer(min_n[1L]))
  } else if (is.finite(p)) {
    as.integer(p) + 3L
  } else {
    5L
  }
  if (!is.finite(lower) || lower < 2L) lower <- 2L
  vals <- if (!is.null(n_grid)) {
    suppressWarnings(as.integer(n_grid))
  } else if (is.finite(ref_n) && ref_n > 1L) {
    suppressWarnings(as.integer(round(ref_n * as.numeric(multipliers))))
  } else {
    integer(0L)
  }
  vals <- sort(unique(vals[is.finite(vals) & vals >= lower]))
  if (!is.null(max_n)) {
    max_n <- suppressWarnings(as.numeric(max_n[1L]))
    if (is.finite(max_n)) vals <- vals[vals <= as.integer(max_n)]
  }
  if (is.finite(ref_n) && ref_n >= lower) vals <- sort(unique(c(vals, as.integer(round(ref_n)))))
  vals
}

evaluate_semantic_n_sensitivity <- function(syntax, cor_matrix, factor_assignment, factors,
                                            n_grid, estimator = "ML", rotation = "geomin",
                                            rotation_args = list(geomin.epsilon = 0.50),
                                            cutoffs = list(cfi = 0.95, tli = 0.95, rmsea = 0.06, srmr = 0.08),
                                            htmt_threshold = 0.85,
                                            score_mode = c("current", "structure_weighted"),
                                            iter_max = 800L,
                                            sample_cov_rescale = FALSE,
                                            reference_n = NULL,
                                            progress = FALSE) {
  score_mode <- match.arg(score_mode)
  fail <- list(
    available = FALSE,
    n_grid = as.integer(n_grid),
    grid_results = NULL,
    item_stability = NULL,
    summary = NULL,
    note = "Semantic proxy N-sensitivity diagnostic unavailable."
  )
  if (is.null(syntax) || is.null(cor_matrix) || is.null(factor_assignment) ||
      is.null(factors) || length(n_grid) == 0L) {
    return(fail)
  }
  n_grid <- sort(unique(as.integer(n_grid[is.finite(n_grid) & n_grid > nrow(cor_matrix) + 2L])))
  if (length(n_grid) == 0L) return(fail)
  reference_n <- suppressWarnings(as.integer(reference_n[1L]))
  if (length(reference_n) == 0L || !is.finite(reference_n)) reference_n <- NA_integer_
  scalar <- function(x) {
    x <- suppressWarnings(as.numeric(x[1L]))
    if (length(x) == 0L || !is.finite(x)) NA_real_ else x
  }
  grid_rows <- vector("list", length(n_grid))
  item_rows <- vector("list", length(n_grid))
  progress_bar <- .semantica_progress_start(
    length(n_grid),
    "[SEMANTIC N] Reference-N sensitivity refits",
    progress
  )
  on.exit(.semantica_progress_close(progress_bar), add = TRUE)
  for (idx in seq_along(n_grid)) {
    n_obs <- n_grid[idx]
    fit <- run_esem_on_matrix(
      syntax, cor_matrix, n_obs = n_obs, estimator = estimator,
      rotation = rotation, rotation_args = rotation_args,
      iter_max = iter_max, fallback = TRUE,
      sample_cov_rescale = sample_cov_rescale
    )
    scored <- extract_and_score_esem(
      fit, cor_matrix, factor_assignment, factors,
      cutoffs = cutoffs, htmt_threshold = htmt_threshold,
      score_mode = score_mode
    )
    sdg <- scored$structure_diagnostics %||% list()
    grid_rows[[idx]] <- data.frame(
      n_obs = n_obs,
      is_reference_n = is.finite(reference_n) && identical(as.integer(n_obs), reference_n),
      converged = isTRUE(scored$converged),
      cfi = scalar(scored$cfi),
      tli = scalar(scored$tli),
      rmsea = scalar(scored$rmsea),
      srmr = scalar(scored$srmr),
      ave = scalar(scored$ave),
      htmt_max = scalar(scored$htmt_max),
      loading_quality = scalar(scored$loading_quality),
      score = scalar(scored$score),
      mean_primary_loading = scalar(sdg$mean_primary_loading),
      median_primary_loading = scalar(sdg$median_primary_loading),
      primary_ge_50 = scalar(sdg$primary_ge_50),
      q90_max_cross_loading = scalar(sdg$q90_max_cross_loading),
      correct_dominance = scalar(sdg$correct_dominance),
      simple_structure = scalar(sdg$simple_structure),
      mean_abs_residual = scalar(sdg$mean_abs_residual),
      stringsAsFactors = FALSE
    )
    diag_items <- sdg$item_diagnostics
    item_col <- if (!is.null(diag_items) && "item" %in% names(diag_items)) {
      "item"
    } else if (!is.null(diag_items) && "ID" %in% names(diag_items)) {
      "ID"
    } else {
      NA_character_
    }
    needed <- c("assigned_factor", "dominant_factor", "primary_loading", "max_cross_loading")
    if (!is.null(diag_items) && !is.na(item_col) && all(needed %in% names(diag_items))) {
      item_rows[[idx]] <- data.frame(
        n_obs = n_obs,
        item = as.character(diag_items[[item_col]]),
        assigned_factor = as.character(diag_items$assigned_factor),
        dominant_factor = as.character(diag_items$dominant_factor),
        primary_loading = suppressWarnings(as.numeric(diag_items$primary_loading)),
        max_cross_loading = suppressWarnings(as.numeric(diag_items$max_cross_loading)),
        stringsAsFactors = FALSE
      )
    }
    .semantica_progress_update(progress_bar, idx)
  }
  grid_df <- do.call(rbind, grid_rows)
  item_long <- do.call(rbind, item_rows[!vapply(item_rows, is.null, logical(1L))])
  item_stability <- NULL
  if (!is.null(item_long) && nrow(item_long) > 0L) {
    items <- unique(item_long$item)
    item_stability <- do.call(rbind, lapply(items, function(item) {
      rows <- item_long[item_long$item == item, , drop = FALSE]
      primary <- rows$primary_loading[is.finite(rows$primary_loading)]
      cross <- rows$max_cross_loading[is.finite(rows$max_cross_loading)]
      dom <- rows$dominant_factor[!is.na(rows$dominant_factor) & nzchar(rows$dominant_factor)]
      dom_tab <- if (length(dom) > 0L) table(dom) else integer(0L)
      data.frame(
        item = item,
        assigned_factor = rows$assigned_factor[1L],
        successful_n = length(unique(rows$n_obs)),
        dominant_factor_agreement = if (length(dom_tab) > 0L) max(dom_tab) / sum(dom_tab) else NA_real_,
        assigned_dominance_rate = if (length(dom) > 0L) mean(dom == rows$assigned_factor[1L]) else NA_real_,
        primary_loading_min = if (length(primary) > 0L) min(primary) else NA_real_,
        primary_loading_max = if (length(primary) > 0L) max(primary) else NA_real_,
        primary_loading_range = if (length(primary) > 0L) diff(range(primary)) else NA_real_,
        max_cross_loading_range = if (length(cross) > 0L) diff(range(cross)) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  }
  ok <- grid_df[grid_df$converged, , drop = FALSE]
  score_range <- if (nrow(ok) > 0L && any(is.finite(ok$score))) diff(range(ok$score, na.rm = TRUE)) else NA_real_
  rmsea_range <- if (nrow(ok) > 0L && any(is.finite(ok$rmsea))) diff(range(ok$rmsea, na.rm = TRUE)) else NA_real_
  is_unidimensional <- length(unique(as.character(factors))) == 1L
  dominance_floor <- if (!is_unidimensional && !is.null(item_stability) && any(is.finite(item_stability$dominant_factor_agreement))) {
    min(item_stability$dominant_factor_agreement, na.rm = TRUE)
  } else NA_real_
  primary_range_median <- if (!is.null(item_stability) && any(is.finite(item_stability$primary_loading_range))) {
    stats::median(item_stability$primary_loading_range, na.rm = TRUE)
  } else NA_real_
  list(
    available = TRUE,
    n_grid = n_grid,
    grid_results = grid_df,
    item_long = item_long,
    item_stability = item_stability,
    summary = list(
      successful_fits = nrow(ok),
      requested_fits = length(n_grid),
      score_range = score_range,
      rmsea_range = rmsea_range,
      dominant_factor_agreement_floor = dominance_floor,
      median_primary_loading_range = primary_range_median,
      dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
      structurally_stable = if (is_unidimensional && is.finite(primary_range_median)) {
        # Dominant-factor agreement is vacuous with one factor; retain the
        # existing loading-range stability criterion without inventing a new
        # one-factor cutoff.
        primary_range_median <= 0.10
      } else if (is.finite(dominance_floor) && is.finite(primary_range_median)) {
        dominance_floor >= 0.80 && primary_range_median <= 0.10
      } else NA
    ),
    dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
    score_mode = score_mode,
    sample_cov_rescale = isTRUE(sample_cov_rescale),
    note = if (is_unidimensional) {
      "N-sensitivity refits the selected one-factor semantic-proxy model over reference-N anchors; dominant-factor agreement is not used because it is vacuous with one factor. This does not estimate respondent sample size."
    } else {
      "N-sensitivity refits the selected semantic-proxy ESEM over reference-N anchors; it does not estimate respondent sample size."
    }
  )
}

# =================================================================
# 8, 9, 10  SEMANTIC INDEX, DUPLICATE CHECK, DIVERSITY FILTER
# =================================================================
estimate_within_similarity_targets <- function(
    list_items, cosine_sim_matrix, factors, within_similarity_target = NULL,
    redundancy_threshold = 0.85, within_similarity_band = 0.08,
    method = c("nonredundant_median", "legacy_q40")) {
  method <- match.arg(method)
  redundancy_threshold <- as.numeric(redundancy_threshold[1L])
  within_similarity_band <- as.numeric(within_similarity_band[1L])
  if (!is.finite(redundancy_threshold) || redundancy_threshold <= -1 || redundancy_threshold > 1) {
    stop("'redundancy_threshold' must be finite and in (-1, 1].")
  }
  if (!is.finite(within_similarity_band) || within_similarity_band < 0 || within_similarity_band >= 2) {
    stop("'within_similarity_band' must be finite and in [0, 2).")
  }

  if (!is.null(within_similarity_target)) {
    target <- within_similarity_target
    if (length(target) == 1L) {
      target <- stats::setNames(rep(as.numeric(target), length(factors)), factors)
    }
    if (!is.null(names(target)) && any(nzchar(names(target)))) {
      target <- as.numeric(target[factors])
    } else {
      target <- as.numeric(rep_len(target, length(factors)))
    }
    names(target) <- factors
    if (any(!is.finite(target)) || any(target < -1 | target > 1)) {
      stop("'within_similarity_target' must contain finite cosine-scale values between -1 and 1.")
    }
    attr(target, "method") <- "user_supplied"
    attr(target, "source") <- stats::setNames(rep("user_supplied", length(factors)), factors)
    return(target)
  }

  target <- stats::setNames(rep(NA_real_, length(factors)), factors)
  source <- stats::setNames(rep(NA_character_, length(factors)), factors)
  for (f in factors) {
    f_items <- intersect(list_items[[f]], rownames(cosine_sim_matrix))
    if (length(f_items) < 2L) next
    block <- cosine_sim_matrix[f_items, f_items, drop = FALSE]
    sims <- block[lower.tri(block)]
    sims <- sims[is.finite(sims)]
    if (!length(sims)) next

    if (identical(method, "legacy_q40")) {
      target[f] <- as.numeric(stats::quantile(
        sims, probs = 0.40, na.rm = TRUE, names = FALSE
      ))
      target[f] <- min(max(target[f], 0.25), 0.55)
      source[f] <- "legacy_q40_clamped_0.25_0.55"
      next
    }

    # The default target is the typical *nonredundant* within-factor relation
    # in the user's own pool/model. This keeps the target on the geometry of
    # the configured embedding model instead of imposing a universal .55 cap.
    nonredundant <- sims[sims < redundancy_threshold]
    if (length(nonredundant) >= 2L) {
      target[f] <- stats::median(nonredundant, na.rm = TRUE)
      source[f] <- "median_nonredundant_within_pool"
    } else if (length(nonredundant) == 1L) {
      target[f] <- nonredundant[1L]
      source[f] <- "single_nonredundant_within_pair"
    } else {
      # If every within-factor pair already exceeds the declared redundancy
      # threshold, the pool itself provides no nonredundant target. Pull the
      # target just inside the user/profile-defined redundancy boundary using
      # the configured tolerance band rather than a model-invariant constant.
      target[f] <- redundancy_threshold - max(within_similarity_band / 2, sqrt(.Machine$double.eps))
      source[f] <- "redundancy_boundary_fallback"
    }
    target[f] <- min(1, max(-1, target[f]))
  }

  # Factors with too few usable pairs borrow the median target from factors
  # that were estimable. Only if none are estimable do we fall back to a
  # conservative value derived from the declared redundancy boundary.
  missing <- !is.finite(target)
  if (any(missing)) {
    available <- target[is.finite(target)]
    fallback <- if (length(available)) {
      stats::median(available)
    } else {
      redundancy_threshold - max(within_similarity_band / 2, sqrt(.Machine$double.eps))
    }
    target[missing] <- min(1, max(-1, fallback))
    source[missing] <- if (length(available)) "borrowed_pool_median" else "redundancy_boundary_fallback"
  }
  attr(target, "method") <- method
  attr(target, "source") <- source
  target
}

# Robust target-centered loss used only when comparative between-factor evidence
# is unavailable. The transition point is the already-declared cohesion band,
# so this adds no free tuning parameter: deviations are quadratic near the
# target and linear beyond the band. The returned loss remains on the original
# cosine-similarity scale.
.semantica_huber_target_loss <- function(observed, target, delta) {
  observed <- suppressWarnings(as.numeric(observed[1L]))
  target <- suppressWarnings(as.numeric(target[1L]))
  delta <- suppressWarnings(as.numeric(delta[1L]))
  if (!is.finite(observed) || !is.finite(target)) return(NA_real_)
  if (!is.finite(delta) || delta <= 0) delta <- sqrt(.Machine$double.eps)
  d <- abs(observed - target)
  if (d <= delta) 0.5 * d^2 / delta else d - 0.5 * delta
}

# Resolve the expensive ESEM checkpoint cadence. Advanced/full-pipeline use
# remains adaptive by default for backward compatibility; the casual presets
# can request an exact fixed cadence so their documented computational budget
# is the cadence that is actually executed.
.semantica_resolve_esem_interval <- function(esem_every, pheromone_entropy,
                                              mode = c("adaptive", "fixed")) {
  mode <- match.arg(mode)
  esem_every <- .semantica_assert_positive_integer(
    esem_every, "esem_every", condition_class = "semantica_error_input"
  )
  if (identical(mode, "fixed")) return(esem_every)
  ph <- suppressWarnings(as.numeric(pheromone_entropy[1L]))
  if (!is.finite(ph)) ph <- 1
  as.integer(max(1L, floor(esem_every / 2L), as.integer(round(esem_every * ph))))
}

# Progress-only numeric formatting. Preserve familiar fixed decimals for ordinary
# values, but use scientific notation for nonzero values that would otherwise
# round to zero and obscure objective resolution. This never changes scoring.
.semantica_format_progress_number <- function(x, digits = 4L) {
  x <- suppressWarnings(as.numeric(x[1L]))
  digits <- suppressWarnings(as.integer(digits[1L]))
  if (!is.finite(x)) return("NA")
  if (!is.finite(digits) || digits < 1L) digits <- 4L
  threshold <- 10^(-digits)
  if (x != 0 && abs(x) < threshold) {
    formatC(x, format = "e", digits = max(2L, digits - 1L))
  } else {
    formatC(x, format = "f", digits = digits)
  }
}

.compute_semantic_sim_index_legacy <- function(sim_matrix, selected_items, factor_assignment, factors,
                                          redundancy_threshold = 0.85, sigmoid_center = 0.15,
                                          sigmoid_steepness = 10, within_similarity_target = NULL,
                                          within_similarity_band = 0.08,
                                          within_similarity_weight = 1.15,
                                          between_similarity_weight = 1.00,
                                          expected_factor_relations = NULL,
                                          nomological_weight = 0) {
  within_blocks <- vector("list", length(factors))
  between_blocks <- vector("list", length(factors))
  within_losses <- numeric(0L)
  within_factor_losses <- numeric(0L)
  within_factor_means <- numeric(0L)
  within_factor_q90 <- numeric(0L)
  n_within <- 0L
  n_between <- 0L
  safe_fz <- function(x) fisherz(pmin(pmax(x, -0.9999), 0.9999))
  target_for <- function(f) {
    if (is.null(within_similarity_target)) return(0.35)
    if (!is.null(names(within_similarity_target)) && f %in% names(within_similarity_target)) {
      val <- as.numeric(within_similarity_target[[f]])
    } else {
      val <- as.numeric(within_similarity_target[1L])
    }
    if (!is.finite(val)) 0.35 else val
  }
  within_similarity_band <- as.numeric(within_similarity_band)
  if (!is.finite(within_similarity_band) || within_similarity_band <= 0) within_similarity_band <- 0.08
  is_unidimensional <- length(factors) == 1L

  for (f in factors) {
    f_items <- names(factor_assignment[factor_assignment == f])
    if (length(f_items) < 2L) next
    sub <- sim_matrix[f_items, f_items, drop = FALSE]; lt <- sub[lower.tri(sub)]
    n_within <- n_within + 1L
    within_blocks[[n_within]] <- lt

    f_mean <- if (length(lt) > 0L) fisherz_inv(mean(safe_fz(lt))) else 0
    f_q90 <- if (length(lt) > 0L) as.numeric(stats::quantile(lt, 0.90, na.rm = TRUE, names = FALSE)) else 0
    target <- target_for(f)
    low_edge <- target - within_similarity_band
    high_edge <- min(redundancy_threshold, target + within_similarity_band)
    low_coherence_loss <- max(0, low_edge - f_mean)
    high_redundancy_loss <- max(0, f_mean - high_edge)
    q90_redundancy_loss <- max(0, f_q90 - min(redundancy_threshold, high_edge + within_similarity_band))
    if (is_unidimensional) {
      # A one-factor model has no between-factor discrimination term. A flat
      # acceptable band would therefore make many candidate forms exactly tied.
      # Use a Huber target loss centred on the empirically estimated
      # non-redundant target: this supplies ranking resolution without turning
      # cohesion into a quantity to maximize. The existing band is the Huber
      # transition point, and q90 still guards distributional redundancy.
      target_center_loss <- .semantica_huber_target_loss(
        observed = f_mean, target = target, delta = within_similarity_band
      )
      f_loss <- target_center_loss + 0.5 * q90_redundancy_loss
    } else {
      # Preserve the established multidimensional band-violation objective.
      f_loss <- 2.0 * low_coherence_loss + high_redundancy_loss + 0.5 * q90_redundancy_loss
    }
    within_losses <- c(within_losses, f_loss)
    within_factor_losses[f] <- f_loss
    within_factor_means[f] <- f_mean
    within_factor_q90[f] <- f_q90

    other_items <- setdiff(selected_items, f_items)
    if (length(other_items) > 0L) {
      cross <- sim_matrix[f_items, other_items, drop = FALSE]
      n_between <- n_between + 1L
      between_blocks[[n_between]] <- as.vector(cross)
    }
  }
  within_sims <- if (n_within > 0L) unlist(within_blocks[seq_len(n_within)], use.names = FALSE) else numeric(0L)
  between_sims <- if (n_between > 0L) unlist(between_blocks[seq_len(n_between)], use.names = FALSE) else numeric(0L)
  mean_within <- if (length(within_sims) > 0L) fisherz_inv(mean(safe_fz(within_sims))) else NA_real_
  mean_between <- if (length(between_sims) > 0L) fisherz_inv(mean(safe_fz(between_sims))) else NA_real_
  q_within <- if (length(within_sims) > 0L) as.numeric(stats::quantile(within_sims, 0.90, na.rm = TRUE, names = FALSE)) else NA_real_
  q_between <- if (length(between_sims) > 0L) as.numeric(stats::quantile(between_sims, 0.90, na.rm = TRUE, names = FALSE)) else NA_real_

  # Within-factor similarity is treated as a target band, not a quantity to
  # minimize without bound. This keeps item wording nonredundant while preserving
  # enough monotrait coherence for reflective scale development.
  within_burden <- if (length(within_losses) > 0L) {
    0.70 * mean(within_losses, na.rm = TRUE) + 0.30 * max(within_losses, na.rm = TRUE)
  } else 0
  between_burden <- if (length(between_sims) > 0L) 0.70 * mean_between + 0.30 * q_between else NA_real_
  # Normalize only by evidence components that actually exist. In a one-factor
  # model there is no between-factor term; dividing by a dormant between-factor
  # weight would artificially make the semantic loss look smaller.
  active_within <- length(within_sims) > 0L
  active_between <- length(between_sims) > 0L
  active_weight <- (if (active_within) within_similarity_weight else 0) +
    (if (active_between) between_similarity_weight else 0)
  if (!is.finite(active_weight) || active_weight <= 0) active_weight <- 1
  similarity_index <- (
    (if (active_within) within_similarity_weight * within_burden else 0) +
      (if (active_between) between_similarity_weight * between_burden else 0)
  ) / active_weight

  # Optional theory-alignment term. Expected relations are interpreted
  # in the semantic-similarity domain, not as empirical latent-factor
  # correlations. A non-zero weight is an explicit hypothesis that requires
  # external calibration before substantive interpretation.
  observed_factor_relations <- matrix(NA_real_, length(factors), length(factors),
                                      dimnames = list(factors, factors))
  diag(observed_factor_relations) <- 1
  if (length(factors) >= 2L) {
    for (ii in seq_len(length(factors) - 1L)) {
      for (jj in (ii + 1L):length(factors)) {
        a <- names(factor_assignment[factor_assignment == factors[ii]])
        b <- names(factor_assignment[factor_assignment == factors[jj]])
        if (length(a) == 0L || length(b) == 0L) next
        vals <- as.vector(sim_matrix[a, b, drop = FALSE])
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0L) next
        v <- fisherz_inv(mean(safe_fz(vals)))
        observed_factor_relations[ii, jj] <- v
        observed_factor_relations[jj, ii] <- v
      }
    }
  }
  nomological_loss <- NA_real_
  nomological_weight <- suppressWarnings(as.numeric(nomological_weight[1L]))
  if (!is.finite(nomological_weight)) nomological_weight <- 0
  nomological_weight <- max(0, min(1, nomological_weight))
  if (!is.null(expected_factor_relations) && nomological_weight > 0) {
    er <- tryCatch(as.matrix(expected_factor_relations), error = function(e) NULL)
    if (!is.null(er)) {
      if (!is.null(rownames(er)) && !is.null(colnames(er)) &&
          all(factors %in% rownames(er)) && all(factors %in% colnames(er))) {
        er <- er[factors, factors, drop = FALSE]
      } else if (!all(dim(er) == c(length(factors), length(factors)))) {
        er <- NULL
      }
    }
    if (!is.null(er)) {
      idx <- upper.tri(er) & is.finite(er) & is.finite(observed_factor_relations)
      if (any(idx)) {
        # Absolute deviation is bounded to [0, 2] and rescaled to [0, 1].
        nomological_loss <- mean(pmin(abs(observed_factor_relations[idx] - er[idx]), 2) / 2)
        similarity_index <- (1 - nomological_weight) * similarity_index +
          nomological_weight * nomological_loss
      }
    }
  }

  max_within <- if (length(within_sims) > 0L) max(within_sims, na.rm = TRUE) else NA_real_
  max_between <- if (length(between_sims) > 0L) max(between_sims, na.rm = TRUE) else NA_real_
  redundancy_penalty <- 0
  if (length(within_sims) > 0L && max_within > redundancy_threshold) {
    redundancy_penalty <- redundancy_penalty + (max_within - redundancy_threshold) * 2.5
  }
  if (length(between_sims) > 0L && max_between > redundancy_threshold) {
    redundancy_penalty <- redundancy_penalty + (max_between - redundancy_threshold) * 2.5
  }

  raw_index <- similarity_index + redundancy_penalty
  z <- sigmoid_steepness * (raw_index - sigmoid_center)
  z <- pmin(pmax(z, -60), 60)
  sigmoid_score <- 1 / (1 + exp(z))
  list(
    sem_score = sigmoid_score,
    discrimination = -similarity_index,
    similarity_index = similarity_index,
    mean_within = mean_within,
    mean_between = mean_between,
    q90_within = q_within,
    q90_between = q_between,
    within_target = within_similarity_target,
    within_target_band = within_similarity_band,
    within_target_loss = within_burden,
    within_factor_target_loss = within_factor_losses,
    within_factor_means = within_factor_means,
    within_factor_q90 = within_factor_q90,
    within_target_loss_mode = if (is_unidimensional) "huber_target_centered" else "band_violation",
    max_within = max_within,
    max_between = max_between,
    redundancy_penalty = redundancy_penalty,
    observed_factor_relations = observed_factor_relations,
    expected_factor_relations = expected_factor_relations,
    nomological_loss = nomological_loss,
    nomological_weight = nomological_weight,
    evidence_components = c(within = active_within, between = active_between),
    dimensionality_mode = if (length(factors) == 1L) "unidimensional" else "multidimensional",
    raw_index = raw_index
  )
}


.semantica_robust_relative_gap <- function(within, between) {
  within <- as.numeric(within); between <- as.numeric(between)
  within <- within[is.finite(within)]; between <- between[is.finite(between)]
  fail <- list(
    stochastic_superiority = NA_real_, rank_component = NA_real_,
    median_within = NA_real_, median_between = NA_real_, median_gap = NA_real_,
    robust_scale = NA_real_, standardized_gap = NA_real_, gap_component = NA_real_,
    conservative_score = NA_real_
  )
  if (!length(within) || !length(between)) return(fail)
  A <- .semantica_stochastic_superiority_vectors(within, between)
  med_w <- stats::median(within); med_b <- stats::median(between)
  gap <- med_w - med_b
  all_vals <- c(within, between)
  scale <- suppressWarnings(stats::IQR(all_vals, na.rm = TRUE, type = 8))
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
    scale <- suppressWarnings(stats::mad(all_vals, center = stats::median(all_vals), constant = 1, na.rm = TRUE))
  }
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
    rr <- range(all_vals, na.rm = TRUE)
    scale <- diff(rr)
  }
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) scale <- NA_real_
  z_gap <- if (is.finite(scale)) gap / scale else if (is.finite(gap) && abs(gap) <= sqrt(.Machine$double.eps)) 0 else NA_real_
  rank_component <- if (is.finite(A)) max(0, min(1, 2 * (A - 0.5))) else NA_real_
  positive_gap <- if (is.finite(z_gap)) max(0, z_gap) else NA_real_
  # A one-robust-spread separation maps to 0.5 and larger separations approach
  # one asymptotically. This transform is dimensionless and introduces no
  # embedding-model-specific cosine cutoff.
  gap_component <- if (is.finite(positive_gap)) positive_gap / (1 + positive_gap) else NA_real_
  conservative <- if (is.finite(rank_component) && is.finite(gap_component)) {
    min(rank_component, gap_component)
  } else NA_real_
  list(
    stochastic_superiority = A,
    rank_component = rank_component,
    median_within = med_w,
    median_between = med_b,
    median_gap = gap,
    robust_scale = scale,
    standardized_gap = z_gap,
    gap_component = gap_component,
    conservative_score = conservative
  )
}

.semantica_relative_semantic_components <- function(sim_matrix, factor_assignment, factors) {
  pv <- .semantica_semantic_pair_vectors(sim_matrix, factor_assignment)
  global <- .semantica_robust_relative_gap(pv$within, pv$between)
  factor_rows <- lapply(factors, function(f) {
    own <- names(factor_assignment[factor_assignment == f])
    other <- names(factor_assignment[factor_assignment != f])
    within <- numeric(0L)
    if (length(own) >= 2L) {
      block <- sim_matrix[own, own, drop = FALSE]
      within <- block[lower.tri(block)]
    }
    between <- if (length(own) && length(other)) as.numeric(sim_matrix[own, other, drop = FALSE]) else numeric(0L)
    z <- .semantica_robust_relative_gap(within, between)
    data.frame(
      factor = f,
      stochastic_superiority = z$stochastic_superiority,
      rank_component = z$rank_component,
      median_within = z$median_within,
      median_between = z$median_between,
      median_gap = z$median_gap,
      robust_scale = z$robust_scale,
      standardized_gap = z$standardized_gap,
      gap_component = z$gap_component,
      relative_score = z$conservative_score,
      stringsAsFactors = FALSE
    )
  })
  factor_table <- do.call(rbind, factor_rows)
  factor_scores <- factor_table$relative_score[is.finite(factor_table$relative_score)]
  weakest <- if (length(factor_scores)) min(factor_scores) else NA_real_
  aggregate <- if (is.finite(global$conservative_score) && is.finite(weakest)) {
    # A conservative conjunctive rule avoids an arbitrary compensatory weight:
    # the scale cannot score above either its global separation or its weakest
    # intended factor's separation.
    min(global$conservative_score, weakest)
  } else global$conservative_score
  list(global = global, factor_table = factor_table,
       weakest_factor_score = weakest, aggregate_score = aggregate)
}

.semantica_relative_cohesion_guard <- function(relative, factors,
                                                within_similarity_target = NULL,
                                                within_similarity_band = 0.08) {
  # In the relative semantic objective, adaptive raw-cosine targets are a
  # *guard* rather than a quality definition.  Compute that guard directly on
  # each factor's median within-factor similarity so that a common additive
  # shift in the representation and its declared target leaves the guard
  # unchanged.  Redundancy is handled separately by the duplicate penalty,
  # avoiding reintroduction of a provider-specific absolute cosine threshold.
  band <- suppressWarnings(as.numeric(within_similarity_band[1L]))
  if (!is.finite(band) || band <= 0) band <- 0.08
  tab <- relative$factor_table
  if (is.null(tab) || !nrow(tab) || is.null(within_similarity_target)) {
    return(list(overall = 1, per_factor = stats::setNames(rep(1, length(factors)), factors),
                loss = stats::setNames(rep(0, length(factors)), factors)))
  }

  target_for <- function(f) {
    if (!is.null(names(within_similarity_target)) && f %in% names(within_similarity_target)) {
      val <- suppressWarnings(as.numeric(within_similarity_target[[f]]))
    } else {
      val <- suppressWarnings(as.numeric(within_similarity_target[1L]))
    }
    if (is.finite(val)) val else NA_real_
  }

  guards <- losses <- stats::setNames(rep(1, length(factors)), factors)
  losses[] <- 0
  for (f in factors) {
    row <- tab[tab$factor == f, , drop = FALSE]
    observed <- if (nrow(row)) suppressWarnings(as.numeric(row$median_within[1L])) else NA_real_
    target <- target_for(f)
    if (!is.finite(observed) || !is.finite(target)) next
    loss <- max(0, abs(observed - target) - band)
    losses[[f]] <- loss
    guards[[f]] <- 1 / (1 + loss / band)
  }
  finite_guards <- guards[is.finite(guards)]
  list(
    overall = if (length(finite_guards)) min(finite_guards) else 1,
    per_factor = guards,
    loss = losses
  )
}

compute_semantic_sim_index_v2 <- function(sim_matrix, selected_items, factor_assignment, factors,
                                          redundancy_threshold = 0.85, sigmoid_center = 0.15,
                                          sigmoid_steepness = 10, within_similarity_target = NULL,
                                          within_similarity_band = 0.08,
                                          within_similarity_weight = 1.15,
                                          between_similarity_weight = 1.00,
                                          expected_factor_relations = NULL,
                                          nomological_weight = 0,
                                          semantic_objective_mode = c("relative_conservative", "legacy_target_burden")) {
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  legacy <- .compute_semantic_sim_index_legacy(
    sim_matrix = sim_matrix,
    selected_items = selected_items,
    factor_assignment = factor_assignment,
    factors = factors,
    redundancy_threshold = redundancy_threshold,
    sigmoid_center = sigmoid_center,
    sigmoid_steepness = sigmoid_steepness,
    within_similarity_target = within_similarity_target,
    within_similarity_band = within_similarity_band,
    within_similarity_weight = within_similarity_weight,
    between_similarity_weight = between_similarity_weight,
    expected_factor_relations = expected_factor_relations,
    nomological_weight = nomological_weight
  )
  is_unidimensional <- length(factors) == 1L || !isTRUE(legacy$evidence_components[["between"]])
  if (is_unidimensional || identical(semantic_objective_mode, "legacy_target_burden")) {
    legacy$semantic_objective_mode <- if (is_unidimensional) "target_centered_unidimensional" else "legacy_target_burden"
    legacy$relative_discrimination_score <- NA_real_
    legacy$relative_discrimination <- NULL
    legacy$legacy_similarity_index <- legacy$similarity_index
    return(legacy)
  }

  relative <- .semantica_relative_semantic_components(sim_matrix, factor_assignment, factors)
  core <- relative$aggregate_score
  if (!is.finite(core)) core <- 0
  cohesion <- .semantica_relative_cohesion_guard(
    relative = relative,
    factors = factors,
    within_similarity_target = within_similarity_target,
    within_similarity_band = within_similarity_band
  )
  cohesion_guard <- cohesion$overall

  nomological_guard <- 1
  if (is.finite(legacy$nomological_loss %||% NA_real_) &&
      is.finite(legacy$nomological_weight %||% NA_real_) && legacy$nomological_weight > 0) {
    nomological_guard <- max(0, 1 - legacy$nomological_weight * legacy$nomological_loss)
  }
  sem_score <- max(0, min(1, core * cohesion_guard * nomological_guard))

  legacy$legacy_sem_score <- legacy$sem_score
  legacy$legacy_similarity_index <- legacy$similarity_index
  legacy$semantic_objective_mode <- "relative_conservative"
  legacy$sem_score <- sem_score
  legacy$relative_discrimination_score <- core
  legacy$relative_discrimination <- relative
  legacy$stochastic_superiority <- relative$global$stochastic_superiority
  legacy$robust_median_gap <- relative$global$median_gap
  legacy$robust_gap_scale <- relative$global$robust_scale
  legacy$standardized_robust_gap <- relative$global$standardized_gap
  legacy$rank_discrimination_component <- relative$global$rank_component
  legacy$gap_discrimination_component <- relative$global$gap_component
  legacy$weakest_factor_relative_score <- relative$weakest_factor_score
  legacy$cohesion_guard <- cohesion_guard
  legacy$cohesion_guard_by_factor <- cohesion$per_factor
  legacy$relative_within_target_loss <- cohesion$loss
  legacy$nomological_guard <- nomological_guard
  # Compatibility loss orientation: lower is better. The raw adaptive-target
  # burden is retained separately above and no longer defines multidimensional
  # semantic quality.
  legacy$similarity_index <- 1 - sem_score
  legacy$discrimination <- sem_score
  legacy$raw_index <- 1 - sem_score
  legacy$within_target_role <- "cohesion_guard_not_primary_quality"
  legacy
}

mean_semantic_similarity_by_factor <- function(cos_mat, items, factor_assignment, factors,
                                               type = c("within", "between")) {
  type <- match.arg(type)
  if (is.null(cos_mat) || is.null(items) || is.null(factor_assignment)) return(NA_real_)
  items <- intersect(as.character(items), rownames(cos_mat))
  if (length(items) == 0L) return(NA_real_)
  factor_assignment <- factor_assignment[intersect(names(factor_assignment), items)]
  factors <- factors[factors %in% unique(factor_assignment)]
  if (length(factors) == 0L) return(NA_real_)

  sim_blocks <- vector("list", length(factors) * max(1L, length(factors)))
  n_blocks <- 0L
  if (type == "within") {
    for (f in factors) {
      f_items <- intersect(names(factor_assignment[factor_assignment == f]), items)
      if (length(f_items) < 2L) next
      sub <- cos_mat[f_items, f_items, drop = FALSE]
      n_blocks <- n_blocks + 1L
      sim_blocks[[n_blocks]] <- sub[lower.tri(sub)]
    }
  } else if (length(factors) >= 2L) {
    for (i in seq_len(length(factors) - 1L)) {
      for (j in (i + 1L):length(factors)) {
        i_items <- intersect(names(factor_assignment[factor_assignment == factors[i]]), items)
        j_items <- intersect(names(factor_assignment[factor_assignment == factors[j]]), items)
        if (length(i_items) == 0L || length(j_items) == 0L) next
        n_blocks <- n_blocks + 1L
        sim_blocks[[n_blocks]] <- as.vector(cos_mat[i_items, j_items, drop = FALSE])
      }
    }
  }
  if (n_blocks == 0L) return(NA_real_)
  sims <- unlist(sim_blocks[seq_len(n_blocks)], use.names = FALSE)
  sims <- sims[is.finite(sims)]
  if (length(sims) == 0L) return(NA_real_)
  mean(sims)
}

compute_semantic_similarity_reduction_summary <- function(cos_mat, pool_items,
                                                          pool_factor_assignment,
                                                          selected_items,
                                                          selected_factor_assignment,
                                                          factors,
                                                          within_similarity_target = NULL,
                                                          within_similarity_band = 0.08) {
  within_before <- mean_semantic_similarity_by_factor(
    cos_mat, pool_items, pool_factor_assignment, factors, "within"
  )
  within_after <- mean_semantic_similarity_by_factor(
    cos_mat, selected_items, selected_factor_assignment, factors, "within"
  )
  between_before <- mean_semantic_similarity_by_factor(
    cos_mat, pool_items, pool_factor_assignment, factors, "between"
  )
  between_after <- mean_semantic_similarity_by_factor(
    cos_mat, selected_items, selected_factor_assignment, factors, "between"
  )
  absolute_reduction <- if (!is.na(within_before) && !is.na(within_after)) within_before - within_after else NA_real_
  percent_reduction <- if (!is.na(absolute_reduction) && !is.na(within_before) && within_before > 0) {
    100 * absolute_reduction / within_before
  } else NA_real_
  between_reduction <- if (!is.na(between_before) && !is.na(between_after)) between_before - between_after else NA_real_
  dimensionality_mode <- if (length(unique(as.character(factors))) == 1L) "unidimensional" else "multidimensional"
  index_before <- if (identical(dimensionality_mode, "unidimensional")) within_before else mean(c(within_before, between_before), na.rm = TRUE)
  index_after <- if (identical(dimensionality_mode, "unidimensional")) within_after else mean(c(within_after, between_after), na.rm = TRUE)
  index_reduction <- index_before - index_after
  if (!is.finite(index_before)) index_before <- NA_real_
  if (!is.finite(index_after)) index_after <- NA_real_
  if (!is.finite(index_reduction)) index_reduction <- NA_real_
  target_value <- NA_real_
  if (!is.null(within_similarity_target)) {
    target_values <- if (!is.null(names(within_similarity_target))) {
      as.numeric(within_similarity_target[intersect(factors, names(within_similarity_target))])
    } else {
      as.numeric(within_similarity_target)
    }
    target_values <- target_values[is.finite(target_values)]
    if (length(target_values) > 0L) target_value <- mean(target_values)
  }
  within_target_deviation_before <- if (is.finite(target_value) && is.finite(within_before)) abs(within_before - target_value) else NA_real_
  within_target_deviation_after <- if (is.finite(target_value) && is.finite(within_after)) abs(within_after - target_value) else NA_real_
  within_target_improvement <- if (is.finite(within_target_deviation_before) && is.finite(within_target_deviation_after)) {
    within_target_deviation_before - within_target_deviation_after
  } else NA_real_
  target_band_status <- if (is.finite(target_value) && is.finite(within_after)) {
    if (abs(within_after - target_value) <= within_similarity_band) "inside target band" else "outside target band"
  } else NA_character_
  separation_gap_before <- if (identical(dimensionality_mode, "multidimensional") && is.finite(within_before) && is.finite(between_before)) {
    within_before - between_before
  } else NA_real_
  separation_gap_after <- if (identical(dimensionality_mode, "multidimensional") && is.finite(within_after) && is.finite(between_after)) {
    within_after - between_after
  } else NA_real_
  separation_gap_change <- if (is.finite(separation_gap_before) && is.finite(separation_gap_after)) {
    separation_gap_after - separation_gap_before
  } else NA_real_
  interpretation <- if (identical(dimensionality_mode, "unidimensional")) {
    if (!is.na(target_band_status)) {
      paste0("One-factor cohesion is ", target_band_status, "; between-factor separation is not applicable.")
    } else {
      "One-factor cohesion is reported descriptively; between-factor separation is not applicable."
    }
  } else if (is.finite(separation_gap_change)) {
    direction <- if (separation_gap_change > 0) "improved" else if (separation_gap_change < 0) "weakened" else "was unchanged"
    guard_txt <- if (!is.na(target_band_status)) paste0("; within-factor cohesion remained ", target_band_status) else ""
    paste0("Relative within-versus-between semantic separation ", direction, guard_txt, ".")
  } else {
    "Relative semantic separation could not be computed from the available items."
  }
  list(
    dimensionality_mode = dimensionality_mode,
    within_factor_before = within_before,
    within_factor_after = within_after,
    between_factor_before = between_before,
    between_factor_after = between_after,
    absolute_reduction = absolute_reduction,
    percent_reduction = percent_reduction,
    between_absolute_reduction = between_reduction,
    semantic_similarity_index_before = index_before,
    semantic_similarity_index_after = index_after,
    semantic_similarity_index_reduction = index_reduction,
    separation_gap_before = separation_gap_before,
    separation_gap_after = separation_gap_after,
    separation_gap_change = separation_gap_change,
    within_similarity_target = target_value,
    within_similarity_band = within_similarity_band,
    within_target_deviation_before = within_target_deviation_before,
    within_target_deviation_after = within_target_deviation_after,
    within_target_improvement = within_target_improvement,
    target_band_status = target_band_status,
    interpretation = interpretation
  )
}

print_semantic_similarity_reduction_summary <- function(metrics, prefix = "  ",
                                                        heading = TRUE) {
  if (is.null(metrics)) return(invisible(FALSE))
  fmt <- function(x) if (is.finite(x)) sprintf("%.4f", x) else "NA"
  pct <- metrics$percent_reduction
  if (isTRUE(heading)) cat("\n[SEMANTICA] Semantic Similarity Reduction Summary:\n")
  cat(sprintf("%sWithin-factor : %s -> %s | reduction = %s",
              prefix, fmt(metrics$within_factor_before), fmt(metrics$within_factor_after),
              fmt(metrics$absolute_reduction)))
  if (is.finite(pct)) cat(sprintf(" (%.2f%%)", pct))
  cat("\n")
  if (is.finite(metrics$within_similarity_target)) {
    cat(sprintf("%sWithin target : %.4f +/- %.4f | deviation %s -> %s | %s\n",
                prefix,
                metrics$within_similarity_target,
                metrics$within_similarity_band %||% NA_real_,
                fmt(metrics$within_target_deviation_before),
                fmt(metrics$within_target_deviation_after),
                metrics$target_band_status %||% "target status unavailable"))
  }
  if (identical(metrics$dimensionality_mode %||% "", "unidimensional")) {
    cat(sprintf("%sBetween-factor: not applicable (single intended factor)\n", prefix))
    cat(sprintf("%sWithin-only index: %s -> %s | change = %s\n",
                prefix, fmt(metrics$semantic_similarity_index_before),
                fmt(metrics$semantic_similarity_index_after),
                fmt(-metrics$semantic_similarity_index_reduction)))
  } else {
    cat(sprintf("%sBetween-factor: %s -> %s | reduction = %s\n",
                prefix, fmt(metrics$between_factor_before), fmt(metrics$between_factor_after),
                fmt(metrics$between_absolute_reduction)))
    if (is.finite(metrics$separation_gap_before %||% NA_real_) && is.finite(metrics$separation_gap_after %||% NA_real_)) {
      cat(sprintf("%sSeparation gap: %s -> %s | change = %+.4f\n",
                  prefix, fmt(metrics$separation_gap_before), fmt(metrics$separation_gap_after),
                  metrics$separation_gap_change %||% NA_real_))
    }
    cat(sprintf("%sComposite index: %s -> %s | reduction = %s\n",
                prefix, fmt(metrics$semantic_similarity_index_before),
                fmt(metrics$semantic_similarity_index_after),
                fmt(metrics$semantic_similarity_index_reduction)))
  }
  cat(sprintf("%sInterpretation: %s\n", prefix, metrics$interpretation %||% "NA"))
  invisible(TRUE)
}

print_semantica_phase3_summary <- function(result, digits = 4L) {
  scalar <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA_real_)
    tryCatch({
      val <- suppressWarnings(as.numeric(x[1L]))
      if (length(val) == 0L) NA_real_ else val
    }, error = function(e) NA_real_)
  }
  num <- function(x) {
    val <- scalar(x)
    if (is.finite(val)) sprintf(paste0("%.", digits, "f"), val) else "NA"
  }
  cut <- function(x) {
    val <- scalar(x)
    if (is.finite(val)) sprintf("%.3f", val) else "NA"
  }
  pct <- function(x) {
    val <- scalar(x)
    if (is.finite(val)) sprintf("%.1f%%", 100 * val) else "NA"
  }
  whole <- function(x, fallback = 0L) {
    val <- scalar(x)
    if (is.finite(val)) as.integer(round(val)) else as.integer(fallback)
  }
  text <- function(x, fallback = "unknown") {
    if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(as.character(x[1L]))) {
      fallback
    } else {
      as.character(x[1L])
    }
  }
  metric_status <- function(value, cutoff, direction) {
    .semantica_proxy_reference_status(
      scalar(value), scalar(cutoff),
      direction = if (identical(direction, ">=")) "higher" else "lower"
    )
  }
  fit_line <- function(label, value, cutoff, direction) {
    cat(sprintf("  %-13s = %s (%s %s) [%s]\n",
                label, num(value), direction, cut(cutoff),
                metric_status(value, cutoff, direction)))
  }
  summary_section <- function(title, note = NULL) {
    cat(sprintf("\n  %s\n", title))
    if (!is.null(note) && length(note) > 0L && nzchar(as.character(note[1L]))) {
      cat(sprintf("  Note: %s\n", as.character(note[1L])))
    }
  }

  cr <- result$esem_result %||% list()
  ac <- result$active_cutoffs %||% list()
  attempts <- whole(result$esem_attempts)
  failures <- whole(result$esem_failures)
  successes <- whole(result$esem_successes, max(0L, attempts - failures))
  assignment <- result$factor_assignment %||% character(0)
  factors <- unique(as.character(unname(assignment)))
  factors <- factors[!is.na(factors) & nzchar(factors)]
  is_unidimensional <- length(factors) == 1L ||
    identical(result$dimensionality_mode %||% "", "unidimensional")

  cat("\n-- FINAL RESULTS SUMMARY ----------------------------------\n")
  cat("  Read by section: selected scale, final fit checks, structural quality, semantic selection, then stability/planning diagnostics.\n")
  summary_section(
    "1. Selected scale and ACO search",
    "The selected items below are the final solution used by all following diagnostics."
  )
  cat("\n  ACO search outcome\n")
  cat(sprintf("  Iterations        : %d\n", whole(result$total_iterations)))
  cat(sprintf("  Unique search ESEM fits: %d / %d admissible\n", successes, attempts))
  cat(sprintf("  Elite archive     : %d solutions\n", length(result$elite_archive %||% list())))
  cat(sprintf("  Search guidance   : %s\n", text(result$search_guidance_status, "legacy/unknown")))
  cat(sprintf("  Optimization util.: %s\n", num(result$best_objective)))
  if (!is.null(result$objective_context)) {
    cat(sprintf("  Objective regime  : %s (optimization utility, not universal quality)\n",
                text(result$objective_context$evidence_regime, "unknown")))
  }
  cat(sprintf("  Selected solution : %d items across %d %s\n",
              length(result$best_items %||% character(0)), length(factors),
              if (length(factors) == 1L) "factor" else "factors"))

  cat("\n  Selected factorial solution\n")
  if (length(factors) == 0L) {
    cat("  No factor assignment is available for the selected items.\n")
  } else {
    for (factor in factors) {
      idx <- which(as.character(unname(assignment)) == factor)
      f_items <- names(assignment)[idx]
      if (is.null(f_items) || length(f_items) == 0L || any(!nzchar(f_items))) {
        f_items <- (result$best_items %||% character(0))[idx]
      }
      cat(sprintf("  %-18s : %s\n", factor, paste(f_items, collapse = ", ")))
    }
  }

  summary_section(
    "2. Final ESEM fit and cutoff checks",
    "Fit checks refer to the final semantic-proxy ESEM unless a response-data validation result is shown later."
  )
  cat("\n  Global fit against the active cutoff reference\n")
  if (!isTRUE(cr$admissible)) {
    ad <- cr$admissibility %||% list()
    reasons <- ad$reasons %||% character(0L)
    reason_txt <- if (length(reasons) > 0L) {
      paste(reasons, collapse = ", ")
    } else {
      "no admissible ESEM solution was returned"
    }
    if (isTRUE(ad$details$converged)) {
      cat("  Final ESEM fit converged but was rejected as inadmissible; fit indices are unavailable.\n")
    } else {
      cat("  Final ESEM fit did not converge to an admissible solution; fit indices are unavailable.\n")
    }
    cat(sprintf("  ESEM rejection   : %s\n", reason_txt))
  } else if (!is.null(result$final_dddfi_cutoffs)) {
    dd <- result$final_dddfi_cutoffs
    obs <- dd$observed %||% list()
    cat(sprintf("  Fit reference     : DDDFI %s MAD approximate-fit diagnostic\n",
                text(dd$target_label, "selected")))
    fit_line("CFI", obs$cfi %||% cr$cfi, dd$cfi, ">=")
    fit_line("RMSEA", obs$rmsea %||% cr$rmsea, dd$rmsea, "<=")
    fit_line("RMSEA 90% UL", obs$rmsea_ci, dd$rmsea_ci, "<=")
    cat(sprintf("  %-13s = %s (descriptive; DDDFI has no SRMR cutoff)\n",
                "SRMR", num(cr$srmr)))
    if (!is.finite(scalar(dd$cfi)) || !is.finite(scalar(dd$rmsea))) {
      cat("  DDDFI note        : one or more cutoffs were suppressed because sensitivity was below 50%.\n")
    } else if (isTRUE(dd$unusually_permissive)) {
      cat("  DDDFI note        : cutoffs are unusually permissive; triangulate with the other diagnostics.\n")
    }
    cat(sprintf("  Search reference  : CFI >= %s | RMSEA <= %s | SRMR <= %s\n",
                cut(ac$cfi), cut(ac$rmsea), cut(ac$srmr)))
  } else {
    cat("  Fit reference     : active search/DFI cutoffs\n")
    fit_line("CFI", cr$cfi, ac$cfi, ">=")
    fit_line("TLI", cr$tli, ac$tli, ">=")
    fit_line("RMSEA", cr$rmsea, ac$rmsea, "<=")
    fit_line("SRMR", cr$srmr, ac$srmr, "<=")
  }
  if (isTRUE(cr$admissible)) {
    score_decomp <- cr$score_decomp %||% list()
    score_mode <- text(score_decomp$score_mode, "current")
    score_label <- if (identical(score_mode, "structure_weighted")) {
      "Structure-weighted ESEM proxy score"
    } else {
      "ESEM proxy score"
    }
    cat(sprintf("  %-31s: %s\n", score_label, num(cr$score)))
    component_vals <- c(
      CFI = scalar(score_decomp$cfi_s), RMSEA = scalar(score_decomp$rmsea_s),
      SRMR = scalar(score_decomp$srmr_s), AVE = scalar(score_decomp$ave_score),
      loading = scalar(score_decomp$loading_quality), HTMT = scalar(score_decomp$htmt_penalty)
    )
    component_vals <- component_vals[is.finite(component_vals)]
    if (length(component_vals)) {
      cat(sprintf(
        "  Score components (scaled)       : %s\n",
        paste(sprintf("%s=%.3f", names(component_vals), component_vals), collapse = " | ")
      ))
    }
  }
  cat(sprintf("  DFI mode          : %s\n", text(result$dfi_mode)))
  cat(sprintf("  Cutoff source     : %s\n", text(result$cutoff_source)))
  if (!is.null(result$search_cutoff_source) &&
      !identical(result$cutoff_source, result$search_cutoff_source)) {
    cat(sprintf("  Search source     : %s\n", text(result$search_cutoff_source)))
  }

  summary_section(
    if (is_unidimensional) "3. One-factor structure and cohesion" else "3. Factorial structure, convergent evidence, and discrimination",
    if (is_unidimensional) {
      "These diagnostics describe a one-factor semantic-proxy model. They screen loading strength and residual structure but do not establish empirical unidimensionality."
    } else {
      "These diagnostics describe loading clarity and semantic-proxy construct separation."
    }
  )
  cat("\n  Factorial-structure diagnostics\n")
  sdg <- cr$structure_diagnostics %||% result$structure_diagnostics
  if (is.null(sdg)) {
    cat("  Final ESEM structure diagnostics are unavailable.\n")
  } else {
    cat(sprintf("  Dominant loadings : mean %s | median %s | min %s\n",
                num(sdg$mean_primary_loading), num(sdg$median_primary_loading),
                num(sdg$min_primary_loading)))
    cat(sprintf("  Primary >= .40/.50: %s / %s\n",
                pct(sdg$primary_ge_40), pct(sdg$primary_ge_50)))
    if (!is_unidimensional) {
      cat(sprintf("  Correct dominance : %s | simple structure: %s\n",
                  pct(sdg$correct_dominance), pct(sdg$simple_structure)))
      cat(sprintf("  Cross-loadings    : mean %s | q90 %s | max %s\n",
                  num(sdg$mean_max_cross_loading), num(sdg$q90_max_cross_loading),
                  num(sdg$max_cross_loading)))
    }
    cat(sprintf("  Complexity        : mean %s | max %s\n",
                num(sdg$mean_complexity), num(sdg$max_complexity)))
    cat(sprintf("  Residual |r|      : mean %s | q95 %s | max %s\n",
                num(sdg$mean_abs_residual), num(sdg$q95_abs_residual),
                num(sdg$max_abs_residual)))
    if (is_unidimensional && is.finite(scalar(sdg$max_abs_centered_residual))) {
      cat(sprintf("  Centered residual : max |r| %s (descriptive; no universal cutoff)\n",
                  num(sdg$max_abs_centered_residual)))
    }
    if (!is_unidimensional) {
      cat(sprintf("  Latent |r| / det. : %s / %s\n",
                  num(sdg$latent_cor_max), num(sdg$factor_score_determinacy)))
    } else {
      cat(sprintf("  Factor determinacy: %s\n", num(sdg$factor_score_determinacy)))
    }
  }
  if (isTRUE(cr$converged)) {
    cat(sprintf("\n  %s\n", if (is_unidimensional) "One-factor convergent diagnostics" else "Convergent and discriminant diagnostics"))
    cat(sprintf("  AVE (dominant)    : %s (%s)\n",
                num(cr$ave),
                if (!is.null(result$response_validation)) {
                  "compare with the response-data benchmark"
                } else {
                  "semantic-proxy descriptive index"
                }))
    if (is_unidimensional) {
      cat("  HTMT              : not applicable (single intended factor)\n")
      ud <- result$unidimensional_diagnostics %||% list()
      if (is.finite(scalar(ud$eigenvalue_ratio_1_to_2)) || is.finite(scalar(ud$first_eigenvalue_share))) {
        cat(sprintf("  Eigen dominance   : first/second %s | first positive-eigen share %s (descriptive)\n",
                    num(ud$eigenvalue_ratio_1_to_2), num(ud$first_eigenvalue_share)))
      }
    } else if (identical(result$model_info$htmt_objective_role %||% "diagnostic", "penalty")) {
      fit_line("HTMT max", cr$htmt_max, result$model_info$htmt_threshold, "<=")
    } else {
      cat(sprintf("  %-13s = %s (descriptive semantic-overlap proxy; reference %.3f does not affect the default objective)\n",
                  "HTMT max", num(cr$htmt_max), scalar(result$model_info$htmt_threshold)))
    }
  }
  factor_ave <- result$factor_ave %||% cr$factor_ave
  if (!is.null(factor_ave) && length(factor_ave) > 0L) {
    factor_labels <- names(factor_ave)
    if (is.null(factor_labels) || any(!nzchar(factor_labels))) {
      factor_labels <- paste("Factor", seq_along(factor_ave))
    }
    cat("  Factor AVE        : ")
    cat(paste(sprintf("%s=%s", factor_labels, vapply(factor_ave, num, character(1L))),
              collapse = " | "))
    cat("\n")
  }

  summary_section(
    "4. Semantic selection diagnostics",
    if (is_unidimensional) {
      "These metrics summarize one-factor semantic cohesion and redundancy change after selection; between-factor discrimination is not applicable."
    } else {
      "These metrics summarize semantic cohesion, discrimination, and redundancy change after selection."
    }
  )
  cat(sprintf("  Scores            : semantic %s | proposal objective %s | final objective %s\n",
              num(result$semantic_score), num(result$proposal_objective_score),
              num(result$best_objective)))
  if (is_unidimensional) {
    cat(sprintf("  Similarity        : raw %s | mean within %s | mean between not applicable\n",
                num(result$semantic_index), num(result$mean_within)))
  } else {
    cat(sprintf("  Similarity        : raw %s | mean within %s | mean between %s\n",
                num(result$semantic_index), num(result$mean_within),
                num(result$mean_between)))
  }
  print_semantic_similarity_reduction_summary(
    result$semantic_similarity_reduction,
    prefix = "  ",
    heading = FALSE
  )

  summary_section(
    "5. Sample-free companion structure diagnostic",
    if (is_unidimensional) {
      "PFA partition recovery is not applicable to a single intended factor; one-factor ESEM/loading/residual/eigen diagnostics provide the structural proxy instead."
    } else {
      "PFA is a semantic-proxy companion diagnostic; it is not response-data validation."
    }
  )
  pfa <- result$pfa_diagnostics
  if (is_unidimensional) {
    cat("  Sample-free PFA   : not applicable (single intended factor; no partition to recover)\n")
  } else if (!is.null(pfa)) {
    if (isTRUE(pfa$available)) {
      cat(sprintf("  Sample-free PFA   : continuous score %s | partition %s | primary %s | margin %s\n",
                  num(pfa$score), num(pfa$partition_quality_score %||% pfa$recovery_score),
                  num(pfa$continuous_salience_score %||% pfa$mean_primary_loading),
                  num(pfa$continuous_clarity_score %||% pfa$mean_loading_margin)))
      cat(sprintf("  Criterion attainment: recovery %s | loading-ref %s | margin-ref %s (descriptor; not the optimization score)\n",
                  num(pfa$recovery_score), num(pfa$salience_score), num(pfa$clarity_score)))
      cat(sprintf("  PFA loadings      : mean primary %s | mean margin %s | %s / %s\n",
                  num(pfa$mean_primary_loading), num(pfa$mean_loading_margin),
                  text(pfa$extraction, "unknown extraction"),
                  text(pfa$rotation, "unknown rotation")))
      cat(sprintf("  PFA role          : %s\n",
                  if (isTRUE(result$model_info$run_pfa_during_search)) {
                    sprintf(
                      "selection objective every %d iteration(s); objective extraction is stored separately",
                      result$model_info$pfa_every %||% 1L
                    )
                  } else if (identical(result$model_info$pfa_mode, "objective")) {
                    "final objective/report only; search-time PFA was disabled"
                  } else {
                    "descriptive diagnostic only; not used to select items"
                  }))
      if (!is.null(result$pfa_search_iterations)) {
        cat(sprintf("  Search-time PFA   : %d iteration(s), %d / %d available proposal diagnostics\n",
                    result$pfa_search_iterations %||% 0L,
                    result$pfa_search_successes %||% 0L,
                    result$pfa_search_attempts %||% 0L))
      }
      if (length(pfa$missing_factors %||% character(0)) > 0L) {
        cat(sprintf("  Missing factors   : %s\n",
                    paste(pfa$missing_factors, collapse = ", ")))
      }
    } else {
      cat(sprintf("  Sample-free PFA   : unavailable (%s)\n",
                  text(pfa$note, "diagnostic failed")))
    }
  } else {
    cat("  Sample-free PFA   : not available in this result object.\n")
  }
  summary_section(
    "6. Stability, planning, and response-validation companions",
    "Proxy reference-N stability and recommended validation N answer different questions."
  )
  resamp <- result$semantic_resampling_stability %||%
    result$semantic_pair_perturbation_stability$resampling %||% NULL
  if (is.list(resamp) && identical(resamp$status %||% "", "computed")) {
    bi <- resamp$pair_bootstrap %||% list()
    ai <- bi$stochastic_superiority_interval %||% c(lower = NA_real_, median = NA_real_, upper = NA_real_)
    gi <- bi$median_gap_interval %||% c(lower = NA_real_, median = NA_real_, upper = NA_real_)
    jr <- resamp$item_jackknife$stochastic_superiority_range %||% c(min = NA_real_, max = NA_real_, range = NA_real_)
    if (is_unidimensional) {
      wi <- bi$within_median_interval %||% c(lower = NA_real_, median = NA_real_, upper = NA_real_)
      cat(sprintf("  Semantic resampling: within-median bootstrap 95%% sensitivity interval [%s, %s]; item-jackknife range %s\n",
                  num(wi[["lower"]]), num(wi[["upper"]]),
                  num(resamp$item_jackknife$within_median_range[["range"]])))
    } else {
      cat(sprintf("  Semantic resampling: A bootstrap 95%% sensitivity interval [%s, %s] | median-gap interval [%s, %s]\n",
                  num(ai[["lower"]]), num(ai[["upper"]]), num(gi[["lower"]]), num(gi[["upper"]])))
      cat(sprintf("  Item jackknife    : stochastic-superiority range %s (descriptive; no universal stability cutoff)\n",
                  num(jr[["range"]])))
    }
    cat("  Resampling note   : pair similarities share items; intervals describe sensitivity of this semantic representation, not respondent sampling uncertainty.\n")
  }
  representation <- result$representation_stability %||% list()
  sensitivity <- representation$cosine_adjustment_sensitivity %||% list()
  if (isTRUE(sensitivity$available)) {
    cat(sprintf("  Representation sensitivity: raw-vs-mean-centered r=%s | top %.1f%% pair J=%s\n",
                num(sensitivity$offdiag_correlation),
                100 * scalar(sensitivity$top_pair_fraction %||% 0.05),
                num(sensitivity$top_pair_jaccard)))
    cat("  Representation note: reported descriptively; SEMANTICA does not automatically switch cosine preprocessing.\n")
  }
  if (!is.null(result$recommended_validation_n)) {
    rvn <- result$recommended_validation_n
    if (isTRUE(rvn$available) && is.finite(scalar(rvn$recommended_n))) {
      cat(sprintf("  Validation N      : %d recommended (%d reps/candidate)\n",
                  whole(rvn$recommended_n), whole(rvn$reps)))
    } else if (isTRUE(rvn$skipped)) {
      cat(sprintf("  Validation N      : skipped (%s)\n",
                  text(rvn$note, "base semantic-proxy ESEM was inadmissible")))
    } else {
      cat(sprintf("  Validation N      : unavailable (%s)\n",
                  text(rvn$note, "criteria not met")))
    }
  }
  sns <- result$semantic_n_sensitivity
  if (!is.null(sns)) {
    if (isTRUE(sns$available)) {
      sm <- sns$summary %||% list()
      cat(sprintf("  Proxy N anchors   : %s | refits %d / %d succeeded\n",
                  paste(sns$n_grid, collapse = ", "),
                  whole(sm$successful_fits), whole(sm$requested_fits)))
      if (!is.null(sm$structurally_stable) && !is.na(sm$structurally_stable)) {
        if (is_unidimensional) {
          cat(sprintf("  Proxy N structure : %s | median primary-loading range %s\n",
                      if (isTRUE(sm$structurally_stable)) "stable" else "changed",
                      num(sm$median_primary_loading_range)))
        } else {
          cat(sprintf("  Proxy N structure : %s | dominance floor %s | median primary range %s\n",
                      if (isTRUE(sm$structurally_stable)) "stable" else "changed",
                      num(sm$dominant_factor_agreement_floor),
                      num(sm$median_primary_loading_range)))
        }
      }
    } else {
      cat(sprintf("  Proxy N anchors   : unavailable (%s)\n",
                  text(sns$note, "diagnostic failed")))
    }
  }
  if (!is.null(result$final_equivtest_diagnostic)) {
    eq <- result$final_equivtest_diagnostic
    cat(sprintf("  EquivTest T-size  : CFI %s | RMSEA %s\n",
                num(eq$cfi_t_size), num(eq$rmsea_t_size)))
  }
  if (!is.null(result$response_validation) &&
      !is.null(result$response_validation$result) &&
      isTRUE(result$response_validation$result$converged)) {
    rv <- result$response_validation$result
    if (is_unidimensional) {
      cat(sprintf("  Response one-factor fit: CFI %s | RMSEA %s | SRMR %s | AVE %s | HTMT not applicable\n",
                  num(rv$cfi), num(rv$rmsea), num(rv$srmr), num(rv$ave)))
    } else {
      cat(sprintf("  Response ESEM fit : CFI %s | RMSEA %s | SRMR %s | AVE %s | HTMT %s\n",
                  num(rv$cfi), num(rv$rmsea), num(rv$srmr), num(rv$ave),
                  num(rv$htmt_max)))
    }
  }

  warnings <- result$summary$warnings
  has_warnings <- !is.null(warnings) && length(warnings) > 0L &&
    !all(is.na(warnings)) && !identical(warnings, "none")
  summary_section("7. Warnings")
  if (has_warnings) {
    for (warning_text in warnings) cat(sprintf("  [!] %s\n", warning_text))
  } else {
    cat("  No run warnings were recorded.\n")
  }

  if (!is.null(result$esem_syntax)) {
    summary_section(
      "8. Final ESEM syntax",
      if (is_unidimensional) {
        "The single factor loads on the full selected indicator set; no factor rotation is applied."
      } else {
        "Each ESEM factor loads on the full selected indicator set before rotation."
      }
    )
    cat(result$esem_syntax, "\n")
  }
  cat("-----------------------------------------------------------\n\n")
  invisible(result)
}

check_near_duplicates <- function(selected_items, sim_matrix, threshold = 0.90) {
  if (length(selected_items) < 2L) return(FALSE)
  sub <- sim_matrix[selected_items, selected_items, drop = FALSE]
  ut <- upper.tri(sub); any(sub[ut] > threshold, na.rm = TRUE)
}

identify_duplicate_clusters <- function(sim_matrix, items, threshold = 0.90,
                                        exact_threshold = 0.9995) {
  items <- intersect(as.character(items), rownames(sim_matrix))
  fail <- list(item_cluster = stats::setNames(rep(NA_character_, length(items)), items),
               clusters = list(), n_clusters = 0L, n_items_clustered = 0L,
               n_exact_pairs = 0L, threshold = threshold,
               exact_threshold = exact_threshold)
  if (length(items) < 2L) return(fail)
  sub <- sim_matrix[items, items, drop = FALSE]
  parent <- seq_along(items)
  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  union_root <- function(a, b) {
    ra <- find_root(a); rb <- find_root(b)
    if (ra != rb) parent[rb] <<- ra
  }
  pairs <- which(upper.tri(sub) & sub >= threshold, arr.ind = TRUE)
  if (nrow(pairs) == 0L) return(fail)
  for (k in seq_len(nrow(pairs))) union_root(pairs[k, 1L], pairs[k, 2L])
  roots <- vapply(seq_along(items), find_root, integer(1L))
  split_items <- split(items, roots)
  clusters <- split_items[vapply(split_items, length, integer(1L)) >= 2L]
  if (length(clusters) == 0L) return(fail)
  cluster_ids <- paste0("dup", seq_along(clusters))
  item_cluster <- stats::setNames(rep(NA_character_, length(items)), items)
  for (i in seq_along(clusters)) item_cluster[clusters[[i]]] <- cluster_ids[i]
  exact_pairs <- which(upper.tri(sub) & sub >= exact_threshold, arr.ind = TRUE)
  list(
    item_cluster = item_cluster,
    clusters = stats::setNames(clusters, cluster_ids),
    n_clusters = length(clusters),
    n_items_clustered = sum(!is.na(item_cluster)),
    n_exact_pairs = nrow(exact_pairs),
    threshold = threshold,
    exact_threshold = exact_threshold
  )
}

sample_items_with_duplicate_guard <- function(items, n_pick, probs = NULL,
                                              duplicate_cluster_id = NULL,
                                              used_clusters = character(0)) {
  items <- as.character(items)
  n_pick <- as.integer(n_pick)
  if (length(items) == 0L || n_pick <= 0L) {
    return(list(items = character(0), used_clusters = used_clusters))
  }
  if (is.null(probs) || length(probs) != length(items)) probs <- rep(1, length(items))
  probs <- as.numeric(probs)
  probs[!is.finite(probs) | probs < 0] <- 0
  if (sum(probs) <= 0) probs <- rep(1, length(items))
  names(probs) <- items
  selected <- character(0)
  used_clusters <- unique(as.character(used_clusters))
  for (k in seq_len(min(n_pick, length(items)))) {
    remaining <- setdiff(items, selected)
    if (length(remaining) == 0L) break
    if (!is.null(duplicate_cluster_id)) {
      cid <- duplicate_cluster_id[remaining]
      ok_cluster <- is.na(cid) | !nzchar(cid) | !(cid %in% used_clusters)
      candidates <- remaining[ok_cluster]
      if (length(candidates) == 0L) candidates <- remaining
    } else {
      candidates <- remaining
    }
    p <- probs[candidates]
    if (sum(p, na.rm = TRUE) <= 0) p <- rep(1, length(candidates))
    p <- p / sum(p)
    pick <- sample(candidates, size = 1L, prob = p)
    selected <- c(selected, pick)
    if (!is.null(duplicate_cluster_id)) {
      cid_pick <- duplicate_cluster_id[[pick]]
      if (!is.na(cid_pick) && nzchar(cid_pick)) used_clusters <- unique(c(used_clusters, cid_pick))
    }
  }
  list(items = selected, used_clusters = used_clusters)
}

compute_duplicate_penalty <- function(selected_items, factor_assignment, factors, sim_matrix, threshold = 0.90) {
  selected_items <- intersect(selected_items, rownames(sim_matrix))
  if (length(selected_items) < 2L) return(1.0)
  sub <- sim_matrix[selected_items, selected_items, drop = FALSE]
  pair_sims <- as.numeric(sub[upper.tri(sub)])
  pair_sims <- pair_sims[is.finite(pair_sims)]
  if (length(pair_sims) == 0L) return(1.0)
  excess <- pmax(pair_sims - threshold, 0)
  if (!any(excess > 0)) return(1.0)

  # A near-duplicate is a graded content-redundancy problem, not a switch.
  # Penalize both how far the worst pair exceeds the threshold and how many
  # flagged pairs occur, without overwhelming factorial/content evidence.
  scale <- max(1 - threshold, .Machine$double.eps)
  severity <- max(excess) / scale
  prevalence <- sum(excess > 0) / max(1, length(selected_items))
  burden <- max(0, severity + prevalence)
  max(0.05, 1 / (1 + burden))
}

compute_facet_coverage_multiplier <- function(selected_items, factor_assignment, item_facet_lookup = NULL,
                                              facets_by_factor = NULL, i_per_f = NULL,
                                              weight = 0.15) {
  weight <- as.numeric(weight)
  if (!is.finite(weight) || weight <= 0 || is.null(item_facet_lookup)) return(1.0)
  weight <- min(1.0, weight)
  factors <- unique(as.character(factor_assignment))
  coverage <- numeric(0L)
  for (f in factors) {
    f_items <- intersect(names(factor_assignment[factor_assignment == f]), selected_items)
    if (length(f_items) == 0L) next
    selected_facets <- unique(item_facet_lookup[f_items])
    selected_facets <- selected_facets[!is.na(selected_facets) & nzchar(selected_facets)]
    available_facets <- if (!is.null(facets_by_factor) && !is.null(facets_by_factor[[f]])) {
      facets_by_factor[[f]]
    } else {
      selected_facets
    }
    available_facets <- unique(available_facets[!is.na(available_facets) & nzchar(available_facets)])
    n_target <- if (!is.null(i_per_f) && f %in% names(i_per_f)) as.integer(i_per_f[[f]]) else length(f_items)
    max_possible <- max(1L, min(length(available_facets), n_target, length(f_items)))
    coverage <- c(coverage, min(1.0, length(selected_facets) / max_possible))
  }
  if (length(coverage) == 0L) return(1.0)
  multiplier <- (1 - weight) + weight * mean(coverage, na.rm = TRUE)
  attr(multiplier, "coverage") <- mean(coverage, na.rm = TRUE)
  multiplier
}

compute_psychometric_guard_penalty <- function(fit_result, min_loading_quality = 0.35,
                                               min_ave = 0.30,
                                               min_primary_loading = 0.40,
                                               min_primary_prop_ge_50 = 0.70,
                                               htmt_guard_threshold = Inf,
                                               warning_penalty = 0.85) {
  if (is.null(fit_result) || !isTRUE(fit_result$converged)) return(0.05)
  penalty <- 1.0
  lq <- suppressWarnings(as.numeric(fit_result$loading_quality))
  if (!is.finite(lq)) lq <- 0
  if (lq < min_loading_quality) penalty <- penalty * max(0.05, lq / max(min_loading_quality, 1e-6))
  ave <- suppressWarnings(as.numeric(fit_result$ave))
  if (!is.finite(ave)) ave <- 0
  if (ave < min_ave) penalty <- penalty * max(0.05, ave / max(min_ave, 1e-6))
  sdg <- fit_result$structure_diagnostics
  if (!is.null(sdg)) {
    min_primary <- suppressWarnings(as.numeric(sdg$min_primary_loading))
    if (is.finite(min_primary) && min_primary < min_primary_loading) {
      penalty <- penalty * max(0.05, min_primary / max(min_primary_loading, 1e-6))
    }
    prop_ge_50 <- suppressWarnings(as.numeric(sdg$primary_ge_50))
    if (is.finite(prop_ge_50) && prop_ge_50 < min_primary_prop_ge_50) {
      penalty <- penalty * max(0.05, prop_ge_50 / max(min_primary_prop_ge_50, 1e-6))
    }
  }
  htmt <- suppressWarnings(as.numeric(fit_result$htmt_max))
  if (is.finite(htmt) && htmt > htmt_guard_threshold) {
    penalty <- penalty * max(0.05, exp(-2.0 * (htmt - htmt_guard_threshold)))
  }
  if (!is.null(fit_result$ave_warnings) && length(fit_result$ave_warnings) > 0L) {
    penalty <- penalty * warning_penalty
  }
  max(0.05, min(1.0, penalty))
}

.semantica_item_relative_profiles <- function(items_by_factor, cosine_sim_matrix, factors,
                                             within_similarity_target = NULL,
                                             within_similarity_band = 0.08) {
  all_items <- unique(unlist(items_by_factor[factors], use.names = FALSE))
  all_items <- intersect(all_items, rownames(cosine_sim_matrix))
  profiles <- list()
  band <- suppressWarnings(as.numeric(within_similarity_band[1L]))
  if (!is.finite(band) || band <= 0) band <- 0.08
  for (f in factors) {
    f_items <- intersect(items_by_factor[[f]], all_items)
    other_items <- setdiff(all_items, f_items)
    if (length(f_items) == 0L) {
      profiles[[f]] <- data.frame()
      next
    }
    target <- if (!is.null(within_similarity_target) && f %in% names(within_similarity_target)) {
      suppressWarnings(as.numeric(within_similarity_target[[f]]))
    } else NA_real_
    rows <- lapply(f_items, function(item) {
      own <- setdiff(f_items, item)
      within <- if (length(own) > 0L) as.numeric(cosine_sim_matrix[item, own, drop = TRUE]) else numeric(0L)
      between <- if (length(other_items) > 0L) as.numeric(cosine_sim_matrix[item, other_items, drop = TRUE]) else numeric(0L)
      within <- within[is.finite(within)]
      between <- between[is.finite(between)]
      rel <- .semantica_robust_relative_gap(within, between)
      cohesion <- if (length(within) > 0L) stats::median(within, na.rm = TRUE) else NA_real_
      target_loss <- if (is.finite(target) && is.finite(cohesion)) {
        max(0, abs(cohesion - target) - band)
      } else 0
      # The same declared target band used by the scale objective serves only
      # as a soft cohesion/redundancy guard.  No additional cosine threshold or
      # provider-specific calibration constant is introduced here.
      cohesion_guard <- 1 / (1 + target_loss / band)
      relative_score <- suppressWarnings(as.numeric(rel$conservative_score))
      if (!is.finite(relative_score)) relative_score <- 0
      data.frame(
        item = item,
        relative_score = relative_score,
        guarded_relative_score = relative_score * cohesion_guard,
        stochastic_superiority = rel$stochastic_superiority,
        median_gap = rel$median_gap,
        standardized_gap = rel$standardized_gap,
        cohesion = cohesion,
        cohesion_guard = cohesion_guard,
        stringsAsFactors = FALSE
      )
    })
    profiles[[f]] <- do.call(rbind, rows)
  }
  profiles
}

compute_eligible_items <- function(list_items, cosine_sim_matrix, factors, i_per_f,
                                   cohesion_retention = 0.75, cohesion_floor_abs = 0.15,
                                   within_similarity_target = NULL,
                                   within_similarity_band = 0.08,
                                   semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
                                   content_alignment_margin = NULL) {
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  cohesion_retention <- suppressWarnings(as.numeric(cohesion_retention[1L]))
  if (!is.finite(cohesion_retention)) cohesion_retention <- 0.75
  cohesion_retention <- min(1, max(0, cohesion_retention))
  eligible <- list()
  use_relative <- identical(semantic_objective_mode, "relative_conservative") && length(factors) > 1L
  relative_profiles <- if (use_relative) {
    .semantica_item_relative_profiles(
      list_items, cosine_sim_matrix, factors,
      within_similarity_target = within_similarity_target,
      within_similarity_band = within_similarity_band
    )
  } else NULL

  for (f in factors) {
    f_items <- list_items[[f]]
    if (length(f_items) <= i_per_f[f]) { eligible[[f]] <- f_items; next }
    f_valid <- intersect(f_items, rownames(cosine_sim_matrix))
    if (length(f_valid) < 2L) { eligible[[f]] <- f_items; next }

    if (use_relative) {
      prof <- relative_profiles[[f]]
      prof <- prof[match(f_valid, prof$item), , drop = FALSE]
      score <- prof$guarded_relative_score
      score[!is.finite(score)] <- -Inf
      finite_score <- score[is.finite(score)]
      if (length(finite_score) > 0L) {
        score_cut <- stats::quantile(
          finite_score, probs = 1 - cohesion_retention,
          na.rm = TRUE, names = FALSE, type = 8
        )
        keep <- f_valid[score >= score_cut]
      } else keep <- character(0L)
      # Preserve the package's established search-freedom safeguard.  If the
      # retention fraction yields too few candidates, replenish by relative
      # discrimination rather than by an absolute cosine target.
      keep_n <- min(length(f_valid), max(i_per_f[f] * 3L, i_per_f[f] + 3L, 4L))
      if (length(keep) < keep_n) {
        align <- if (!is.null(content_alignment_margin)) {
          suppressWarnings(as.numeric(content_alignment_margin[f_valid]))
        } else rep(NA_real_, length(f_valid))
        # Construct-definition margin is a lexicographic tie-breaker, not a
        # weighted substitute for the within-vs-between semantic objective.
        align_ord <- align; align_ord[!is.finite(align_ord)] <- -Inf
        ord <- order(score, align_ord, prof$stochastic_superiority, prof$median_gap,
                     decreasing = TRUE, na.last = TRUE)
        keep <- f_valid[ord[seq_len(keep_n)]]
      }
      eligible[[f]] <- unique(keep)
      next
    }

    # Exact legacy behavior is retained for one-factor scales and for the
    # explicit legacy semantic objective mode.
    block <- cosine_sim_matrix[f_valid, f_valid, drop = FALSE]
    diag(block) <- NA_real_
    cohesion <- rowMeans(block, na.rm = TRUE)
    target <- if (!is.null(within_similarity_target) && f %in% names(within_similarity_target)) within_similarity_target[[f]] else 0.35
    target <- if (is.finite(target)) as.numeric(target) else 0.35
    dev <- abs(cohesion - target)
    dev_cut <- stats::quantile(dev, cohesion_retention, na.rm = TRUE, names = FALSE)
    keep <- f_valid[dev <= dev_cut]
    keep_n <- min(length(f_valid), max(i_per_f[f] * 3L, i_per_f[f] + 3L, 4L))
    if (length(keep) < keep_n) {
      keep <- f_valid[order(dev, cohesion, decreasing = FALSE)][seq_len(keep_n)]
    }
    eligible[[f]] <- keep
  }
  attr(eligible, "selection_basis") <- if (use_relative) {
    "relative_within_between_discrimination_with_target_guard"
  } else "legacy_nearest_within_target"
  eligible
}

compute_item_heuristics <- function(eligible_items, cosine_sim_matrix, factors,
                                    within_similarity_target = NULL,
                                    within_similarity_band = 0.08,
                                    semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
                                    content_alignment_margin = NULL) {
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  heuristics <- list()
  use_relative <- identical(semantic_objective_mode, "relative_conservative") && length(factors) > 1L
  relative_profiles <- if (use_relative) {
    .semantica_item_relative_profiles(
      eligible_items, cosine_sim_matrix, factors,
      within_similarity_target = within_similarity_target,
      within_similarity_band = within_similarity_band
    )
  } else NULL

  for (f in factors) {
    f_items <- eligible_items[[f]]
    if (length(f_items) < 2L) { heuristics[[f]] <- setNames(rep(1.0, length(f_items)), f_items); next }
    f_valid <- intersect(f_items, rownames(cosine_sim_matrix))
    if (length(f_valid) < 2L) { heuristics[[f]] <- setNames(rep(1.0, length(f_valid)), f_valid); next }

    if (use_relative) {
      prof <- relative_profiles[[f]]
      prof <- prof[match(f_valid, prof$item), , drop = FALSE]
      score <- prof$guarded_relative_score
      score[!is.finite(score)] <- 0
      n <- length(score)
      # A rank-normalized heuristic keeps ACO proposal pressure comparable
      # across embedding backends without introducing a raw-cosine scale.
      # Construct-definition margin is strictly lexicographic: it can break an
      # exact relative-score tie, but can never overturn a better semantic
      # discrimination score or contribute an arbitrary weighted increment.
      align <- if (!is.null(content_alignment_margin)) {
        suppressWarnings(as.numeric(content_alignment_margin[f_valid]))
      } else rep(NA_real_, n)
      align_ord <- align; align_ord[!is.finite(align_ord)] <- -Inf
      if (n <= 1L) {
        h_vals <- rep(1.0, n)
      } else {
        ord <- order(-score, -align_ord, na.last = TRUE)
        dense_rank <- integer(n)
        rank_id <- 1L
        dense_rank[ord[1L]] <- rank_id
        if (n > 1L) {
          for (jj in 2:n) {
            prev <- ord[jj - 1L]; cur <- ord[jj]
            same_score <- abs(score[cur] - score[prev]) <= 1e-12
            same_align <- identical(align_ord[cur], align_ord[prev]) ||
              (is.finite(align_ord[cur]) && is.finite(align_ord[prev]) &&
                 abs(align_ord[cur] - align_ord[prev]) <= 1e-12)
            if (!(same_score && same_align)) rank_id <- rank_id + 1L
            dense_rank[cur] <- rank_id
          }
        }
        if (rank_id <= 1L) {
          h_vals <- rep(1.0, n)
        } else {
          h_vals <- 0.1 + 0.9 * (rank_id - dense_rank) / (rank_id - 1L)
          h_vals[!is.finite(h_vals)] <- 0.1
        }
      }
      heuristics[[f]] <- stats::setNames(h_vals, f_valid)
      next
    }

    block <- cosine_sim_matrix[f_valid, f_valid, drop = FALSE]
    diag(block) <- NA_real_
    cohesion <- rowMeans(block, na.rm = TRUE)
    target <- if (!is.null(within_similarity_target) && f %in% names(within_similarity_target)) within_similarity_target[[f]] else 0.35
    target <- if (is.finite(target)) as.numeric(target) else 0.35
    dev <- abs(cohesion - target)
    dev_range <- max(dev) - min(dev)
    h_vals <- if (dev_range > 1e-6) 0.1 + 0.9 * (max(dev) - dev) / dev_range else rep(1.0, length(dev))
    low_outlier <- cohesion < (target - within_similarity_band)
    h_vals[low_outlier] <- h_vals[low_outlier] * 0.70
    heuristics[[f]] <- stats::setNames(h_vals, f_valid)
  }
  attr(heuristics, "selection_basis") <- if (use_relative) {
    "ranked_relative_within_between_discrimination_with_target_guard"
  } else "legacy_nearest_within_target"
  heuristics
}

# =================================================================
# 11, 11-B, 11-C  FIT FUNCTION, PHEROMONE ENTROPY & UPDATE
# =================================================================
compute_pheromone_entropy <- function(pheromone) {
  p <- pheromone[, "selected"]; p <- p / sum(p); p <- p[p > 0]
  H <- -sum(p * log(p)); H_max <- log(length(p))
  if (H_max < 1e-10) return(0); H / H_max
}

update_pheromone <- function(pheromone, ant_solutions, objectives, rho, ants, mode = c("top_elite", "best_ant")) {
  mode <- match.arg(mode)
  n_scored <- min(length(ant_solutions), length(objectives))
  if (n_scored < 1L) return(pheromone)
  ant_solutions <- ant_solutions[seq_len(n_scored)]
  objectives <- objectives[seq_len(n_scored)]
  pheromone <- pheromone * (1 - rho)
  add_deposit <- function(v, dep) {
    idx <- cbind(seq_along(v), ifelse(v == 1L, 2L, 1L))
    pheromone[idx] <<- pheromone[idx] + dep
  }
  if (mode == "best_ant") {
    best_idx <- which.max(objectives); v <- ant_solutions[[best_idx]]
    add_deposit(v, 1.0)
  } else {
    top_k <- min(n_scored, max(1L, floor(n_scored * 0.20)))
    top_ix <- order(objectives, decreasing = TRUE)[seq_len(top_k)]
    top_objs <- objectives[top_ix]; top_range <- max(top_objs) - min(top_objs)
    top_objs_norm <- if (top_range > 1e-6) (top_objs - min(top_objs)) / top_range else rep(1.0, length(top_objs))
    deposited_sigs <- character(0)
    for (ki in seq_along(top_ix)) {
      k <- top_ix[ki]; dep <- max(top_objs_norm[ki], 0.01); v <- ant_solutions[[k]]
      sig <- paste(sort(which(v == 1L)), collapse = "-")
      if (sig %in% deposited_sigs) next
      deposited_sigs <- c(deposited_sigs, sig)
      add_deposit(v, dep)
    }
  }
  pheromone[] <- pmax(pheromone, 0.01); pheromone[] <- pmin(pheromone, 50.0)
  pheromone
}

solution_signature <- function(vec) paste(sort(which(vec == 1L)), collapse = "-")

update_elite_archive <- function(archive, entries, elite_k,
                                 rank_field = "obj", score_schema = NULL) {
  entries <- Filter(Negate(is.null), entries)
  if (length(entries) == 0L) return(archive)
  combined <- c(archive, entries)
  if (!is.null(score_schema)) {
    schema_ok <- vapply(combined, function(e) {
      identical(as.character(e$score_schema %||% NA_character_), as.character(score_schema))
    }, logical(1L))
    combined <- combined[schema_ok]
  }
  if (length(combined) == 0L) return(list())
  scores <- vapply(combined, function(e) {
    value <- suppressWarnings(as.numeric(e[[rank_field]] %||% -Inf))
    if (length(value) != 1L || !is.finite(value)) -Inf else value
  }, numeric(1L))
  ord <- order(scores, decreasing = TRUE)
  combined <- combined[ord]
  sig <- vapply(combined, function(e) solution_signature(e$vec), character(1L))
  combined <- combined[!duplicated(sig)]
  combined[seq_len(min(length(combined), as.integer(elite_k)))]
}

.semantica_new_archive_state <- function() {
  list(last_signature = NULL, stable_count = 0L, updates = 0L)
}

.semantica_update_archive_state <- function(state, archive) {
  sig <- if (length(archive) == 0L) {
    ""
  } else {
    paste(vapply(archive, function(e) solution_signature(e$vec), character(1L)), collapse = "|")
  }
  unchanged <- !is.null(state$last_signature) && identical(sig, state$last_signature)
  state$stable_count <- if (unchanged) state$stable_count + 1L else 0L
  state$last_signature <- sig
  state$updates <- state$updates + 1L
  state
}

.semantica_union_evidence_archives <- function(archives, active_tracks) {
  active_tracks <- intersect(active_tracks, names(archives))
  combined <- unlist(archives[active_tracks], recursive = FALSE, use.names = FALSE)
  if (length(combined) == 0L) return(list())
  sig <- vapply(combined, function(e) solution_signature(e$vec), character(1L))
  combined[!duplicated(sig)]
}

.semantica_stratified_finalists <- function(archives, active_tracks, budget) {
  active_tracks <- intersect(active_tracks, names(archives))
  budget <- as.integer(budget[1L])
  if (!length(active_tracks) || !is.finite(budget) || budget < 1L) return(list())

  # Prefer the most structurally informed track first within each round, while
  # reserving representation from every active evidence track. No scores from
  # different schemas are compared at this stage.
  priority <- c("esem", "pfa", "semantic")
  tracks <- c(intersect(priority, active_tracks), setdiff(active_tracks, priority))
  pos <- stats::setNames(rep(1L, length(tracks)), tracks)
  finalists <- list()
  sig_to_index <- new.env(parent = emptyenv(), hash = TRUE)

  while (length(finalists) < budget) {
    added_this_round <- FALSE
    for (track in tracks) {
      archive <- archives[[track]] %||% list()
      while (pos[[track]] <= length(archive)) {
        entry <- archive[[pos[[track]]]]
        pos[[track]] <- pos[[track]] + 1L
        sig <- solution_signature(entry$vec)
        if (exists(sig, envir = sig_to_index, inherits = FALSE)) {
          idx <- get(sig, envir = sig_to_index, inherits = FALSE)
          finalists[[idx]]$source_tracks <- unique(c(finalists[[idx]]$source_tracks, track))
          next
        }
        entry$source_tracks <- track
        finalists[[length(finalists) + 1L]] <- entry
        assign(sig, length(finalists), envir = sig_to_index)
        added_this_round <- TRUE
        break
      }
      if (length(finalists) >= budget) break
    }
    if (!added_this_round) break
  }
  finalists
}

fit.function.v2 <- function(selected_vector, run_esem_now = FALSE, effective_esem_weight = 0.50,
                            run_pfa_now = NULL,
                            solution_cache = NULL, solution_history_env = NULL,
                            verbose_decomp = FALSE, return_payload = FALSE) {
  caller <- parent.frame()
  get_ctx <- function(name) {
    if (exists(name, envir = caller, inherits = TRUE)) {
      get(name, envir = caller, inherits = TRUE)
    } else {
      stop("fit.function.v2: missing ACO context object '", name, "'.")
    }
  }
  get_ctx_optional <- function(name, default = NULL) {
    if (exists(name, envir = caller, inherits = TRUE)) get(name, envir = caller, inherits = TRUE) else default
  }

  item.vector <- get_ctx("item.vector")
  cosine_sim_matrix <- get_ctx("cosine_sim_matrix")
  list.items <- get_ctx("list.items")
  factors <- get_ctx("factors")
  model_info <- get_ctx("model_info")
  item.factor.lookup <- get_ctx_optional("item.factor.lookup")
  item.facet.lookup <- get_ctx_optional("item.facet.lookup")
  facets.by.factor <- get_ctx_optional("facets.by.factor")
  i.per.f <- get_ctx_optional("i.per.f")
  pfa_objective_active <- (model_info$pfa_mode %||% "off") == "objective" &&
    (model_info$pfa_weight %||% 0) > 0
  if (is.null(run_pfa_now)) run_pfa_now <- pfa_objective_active
  run_pfa_now <- isTRUE(run_pfa_now) && pfa_objective_active
  pfa_weight_eff <- max(0, min(1, model_info$pfa_weight %||% 0))
  pfa_failure_policy <- model_info$pfa_failure_policy %||% "semantic_fallback"
  score_with_optional_pfa <- function(sem_score, pfa_score) {
    if (!run_pfa_now) return(sem_score)
    if (is.finite(pfa_score %||% NA_real_)) {
      return((1 - pfa_weight_eff) * sem_score + pfa_weight_eff * pfa_score)
    }
    if (identical(pfa_failure_policy, "semantic_fallback")) return(sem_score)
    if (identical(pfa_failure_policy, "penalize")) {
      return((1 - pfa_weight_eff) * sem_score)
    }
    stop("Objective-mode PFA was unavailable for this candidate under pfa_failure_policy='stop'.")
  }

  selected_items <- item.vector[selected_vector == 1L]
  n_selected     <- length(selected_items)
  if (!is.null(item.factor.lookup)) {
    factor_assignment <- item.factor.lookup[selected_items]
    names(factor_assignment) <- selected_items
    factor_idx <- match(factor_assignment, factors)
    items_in_factor <- tabulate(factor_idx, nbins = length(factors))
    names(items_in_factor) <- factors
  } else {
    factor_assignment <- character(n_selected); names(factor_assignment) <- selected_items
    items_in_factor <- integer(length(factors)); names(items_in_factor) <- factors
    for (f_idx in seq_along(factors)) {
      f_name <- factors[f_idx]; mask <- selected_items %in% list.items[[f_idx]]
      factor_assignment[mask] <- f_name; items_in_factor[f_idx] <- sum(mask)
    }
  }
  if (any(items_in_factor < 2L)) return(-Inf)

  cache_key <- make_solution_key(selected_vector)
  cached <- NULL
  if (!is.null(solution_cache)) {
    cached <- cache_get(solution_cache, cache_key)
    if (!is.null(cached) && !run_esem_now) {
      cached_sem <- cached$sem_score
      cached_pfa <- cached$pfa_score
      if (!run_pfa_now && !is.null(cached_sem) && is.finite(cached_sem)) {
        return(cached_sem)
      }
      if (run_pfa_now && !is.null(cached_sem) && is.finite(cached_sem) &&
          !is.null(cached$pfa_result)) {
        return(score_with_optional_pfa(cached_sem, cached_pfa))
      }
    }
    if (!is.null(cached) && run_esem_now && !is.null(cached$esem_score)) {
      # Recompute the mixture from cached components under the declared
      # objective weight; this keeps all guided solutions comparable.
      cached_sem <- cached$sem_score
      cached_pfa <- cached$pfa_score
      if (!run_pfa_now || !is.null(cached$pfa_result)) {
        guard_penalty <- cached$guard_penalty
        if (is.null(guard_penalty) || !is.finite(guard_penalty)) guard_penalty <- 1.0
        guard_weight <- model_info$psychometric_guard_weight %||% 0.50
        base_score <- if (!is.null(cached_sem) && is.finite(cached_sem)) {
          score_with_optional_pfa(cached_sem, cached_pfa)
        } else {
          cached$search_score %||% cached$sem_score
        }
        return(((1 - effective_esem_weight) * base_score + effective_esem_weight * cached$esem_score) * (guard_penalty ^ guard_weight))
      }
    }
  }

  cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, selected_items), error = function(e) NULL)
  if (is.null(cos_sub)) return(-Inf)

  use_cached_search <- run_esem_now &&
    !is.null(cached) &&
    !is.null(cached$sem_score) &&
    is.finite(cached$sem_score) &&
    (!run_pfa_now || !is.null(cached$pfa_result))
  if (use_cached_search) {
    sem_score <- cached$sem_score
    pfa_result <- cached$pfa_result
    pfa_score <- cached$pfa_score %||% NA_real_
    search_score <- score_with_optional_pfa(sem_score, pfa_score)
  } else {
    dup_penalty <- compute_duplicate_penalty(selected_items, factor_assignment, factors, cos_sub, model_info$dup_threshold)
    facet_multiplier <- compute_facet_coverage_multiplier(
      selected_items, factor_assignment, item.facet.lookup, facets.by.factor, i.per.f,
      weight = model_info$facet_coverage_weight %||% 0.15
    )
    sem_result <- compute_semantic_sim_index_v2(
      cos_sub, selected_items, factor_assignment, factors,
      model_info$redundancy_threshold, model_info$sigmoid_center, model_info$sigmoid_steepness,
      within_similarity_target = model_info$within_similarity_target,
      within_similarity_band = model_info$within_similarity_band %||% 0.08,
      expected_factor_relations = model_info$expected_factor_relations,
      nomological_weight = model_info$nomological_weight %||% 0,
      semantic_objective_mode = model_info$semantic_objective_mode %||% "relative_conservative"
    )
    sem_score <- sem_result$sem_score * dup_penalty * facet_multiplier
    pfa_result <- cached$pfa_result
    pfa_score <- cached$pfa_score %||% NA_real_
    search_score <- sem_score
    if (run_pfa_now) {
      pfa_result <- compute_pfa_diagnostics(
        cos_sub, factor_assignment, factors,
        extraction = model_info$pfa_extraction %||% "principal",
        rotation = model_info$pfa_rotation %||% "promax",
        min_loading = model_info$pfa_min_loading %||% 0.40,
        min_margin = model_info$pfa_min_margin
      )
      pfa_score <- if (isTRUE(pfa_result$available)) pfa_result$score else NA_real_
      search_score <- score_with_optional_pfa(sem_score, pfa_score)
    }
  }

  if (!run_esem_now) {
    total_score <- search_score
    if (!is.null(solution_cache)) {
      cache_set(solution_cache, cache_key, modifyList(cached %||% list(), list(
        sem_score = sem_score, pfa_score = pfa_score,
        pfa_result = pfa_result, search_score = search_score,
        total_score = total_score
      )))
    }
    if (!is.null(solution_history_env)) {
      .semantica_history_append(solution_history_env, list(
        key = cache_key, sem_score = sem_score, pfa_score = pfa_score,
        search_score = search_score, esem_score = NA_real_, total = total_score
      ))
    }
    return(total_score)
  }

  if (!is.null(cached) && !is.null(cached$esem_score)) {
    guard_penalty <- cached$guard_penalty
    if (is.null(guard_penalty) || !is.finite(guard_penalty)) guard_penalty <- 1.0
    guard_weight <- model_info$psychometric_guard_weight %||% 0.50
    total_score <- ((1 - effective_esem_weight) * search_score +
                      effective_esem_weight * cached$esem_score) *
      (guard_penalty ^ guard_weight)
    cache_entry <- modifyList(cached, list(
      sem_score = sem_score, pfa_score = pfa_score, pfa_result = pfa_result,
      search_score = search_score, total_score = total_score
    ))
    if (!is.null(solution_cache)) cache_set(solution_cache, cache_key, cache_entry)
    if (isTRUE(return_payload)) {
      return(list(score = total_score, key = cache_key, cache_entry = cache_entry,
                  error = if (!is.null(cached$fit_result) &&
                    isTRUE(cached$fit_result$converged) &&
                    isTRUE(cached$fit_result$admissible)) {
                    NA_character_
                  } else {
                    "ESEM model did not return an admissible scored solution."
                  }))
    }
    return(total_score)
  }

  esem_cor <- transform_cosine_for_esem(cos_sub, factor_assignment, factors)
  if (is.null(esem_cor)) {
    total_score <- search_score * (1 - effective_esem_weight)
    cache_entry <- modifyList(cached %||% list(), list(
      sem_score = sem_score, pfa_score = pfa_score, pfa_result = pfa_result,
      search_score = search_score, esem_score = 0, total_score = total_score,
      esem_evaluated = TRUE, esem_fit_started = FALSE,
      fit_result = list(
        converged = FALSE, admissible = FALSE,
        admissibility = list(
          admissible = FALSE,
          reasons = "esem_correlation_transformation_failed"
        )
      )
    ))
    if (!is.null(solution_cache)) cache_set(solution_cache, cache_key, cache_entry)
    if (isTRUE(return_payload)) {
      return(list(score = total_score, key = cache_key, cache_entry = cache_entry,
                  error = "ESEM correlation transformation failed."))
    }
    return(total_score)
  }

  esem_syntax <- build_esem_syntax_safe(selected_items, factor_assignment, factors)
  esem_rotation_args <- prepare_esem_rotation_args(
    model_info$rotation, model_info$rotation_args,
    selected_items, factor_assignment, factors
  )
  esem_run <- run_esem_on_matrix(
    esem_syntax, esem_cor, model_info$n_obs, model_info$estimator,
    model_info$rotation, esem_rotation_args,
    iter_max = model_info$fast_esem_iter_max %||% 500L,
    fallback = !isTRUE(model_info$fast_esem),
    return_diagnostics = TRUE
  )
  esem_fit <- esem_run$fit
  fit_result <- extract_and_score_esem(
    esem_fit, esem_cor, factor_assignment, factors,
    model_info$active_cutoffs, model_info$htmt_threshold, verbose_decomp,
    score_mode = model_info$semantic_esem_score_mode %||% "current",
    htmt_objective_role = model_info$htmt_objective_role %||% "diagnostic"
  )
  fit_result <- .semantica_attach_esem_rejection(fit_result, esem_run)

  esem_score <- fit_result$score
  guard_penalty <- compute_psychometric_guard_penalty(
    fit_result,
    min_ave = model_info$psychometric_guard_min_ave %||% 0.30,
    min_primary_loading = model_info$psychometric_guard_min_loading %||% 0.40,
    min_primary_prop_ge_50 = model_info$psychometric_guard_min_primary_ge_50 %||% 0.70,
    htmt_guard_threshold = if (identical(model_info$htmt_objective_role %||% "diagnostic", "penalty")) model_info$htmt_threshold else Inf
  )
  guard_weight <- model_info$psychometric_guard_weight %||% 0.50
  total_score <- ((1 - effective_esem_weight) * search_score + effective_esem_weight * esem_score) * (guard_penalty ^ guard_weight)
  cache_entry <- modifyList(cached %||% list(), list(
    sem_score = sem_score, pfa_score = pfa_score, pfa_result = pfa_result,
    search_score = search_score, esem_score = esem_score,
    guard_penalty = guard_penalty, total_score = total_score,
    esem_evaluated = TRUE, esem_fit_started = TRUE,
    fit_attempt = fit_result$fit_attempt %||%
      esem_run$accepted_attempt %||% NA_integer_,
    solver_attempts_started = esem_run$solver_attempts_started %||% 0L,
    fit_result = fit_result
  ))
  if (!is.null(solution_cache)) cache_set(solution_cache, cache_key, cache_entry)
  if (!is.null(solution_history_env)) {
    .semantica_history_append(solution_history_env, list(
      key = cache_key, sem_score = sem_score, pfa_score = pfa_score,
      search_score = search_score, esem_score = esem_score,
      guard_penalty = guard_penalty, total = total_score
    ))
  }
  if (isTRUE(return_payload)) {
    return(list(score = total_score, key = cache_key, cache_entry = cache_entry,
                error = if (isTRUE(fit_result$converged) && isTRUE(fit_result$admissible)) NA_character_ else "ESEM model did not return an admissible scored solution."))
  }
  total_score
}

.semantica_evaluate_esem_worker <- function(task) {
  v <- task$vector
  started <- proc.time()[["elapsed"]]
  # Worker dependencies are exported explicitly by ACO_with_ESEM(). Avoid
  # reaching back into the caller's .GlobalEnv or mutating function environments.
  tryCatch({
    payload <- .semantica_with_task_seed(
      task$seed,
      fit.function.v2(
        v,
        run_esem_now = TRUE,
        effective_esem_weight = task$effective_esem_weight,
        run_pfa_now = task$run_pfa_now,
        solution_cache = NULL,
        solution_history_env = NULL,
        return_payload = TRUE
      )
    )
    converged <- !is.null(payload$cache_entry$fit_result) &&
      isTRUE(payload$cache_entry$fit_result$converged)
    admissible <- converged && isTRUE(payload$cache_entry$fit_result$admissible)
    if (!is.finite(payload$score) || !admissible) {
      payload$score <- NA_real_
      if (is.null(payload$error) || is.na(payload$error)) {
        payload$error <- "ESEM model did not return an admissible scored solution."
      }
    }
    payload$elapsed_seconds <- proc.time()[["elapsed"]] - started
    payload
  }, error = function(e) {
    list(score = NA_real_, key = make_solution_key(v), cache_entry = NULL,
         error = conditionMessage(e), elapsed_seconds = proc.time()[["elapsed"]] - started)
  })
}

# Internal classifier for the legacy pair-perturbation boundary. The raw
# difference remains available in the public result; this binary label is
# deliberately described as an uncalibrated heuristic.
.semantica_pair_perturbation_classify <- function(difference, threshold = 0.10) {
  difference <- suppressWarnings(as.numeric(difference[1L]))
  threshold <- suppressWarnings(as.numeric(threshold[1L]))
  if (!is.finite(difference) || !is.finite(threshold) || threshold <= 0) {
    return(list(stable = NA, classification = "unavailable", threshold = threshold,
                threshold_status = "legacy_uncalibrated_heuristic"))
  }
  stable <- difference < threshold
  list(
    stable = stable,
    classification = if (stable) "heuristically_stable" else "heuristically_unstable",
    threshold = threshold,
    threshold_status = "legacy_uncalibrated_heuristic"
  )
}

# =================================================================
# 11-D  EVAPORATION CONTROL (SEMANTICA >= 0.3.0)
# =================================================================
#' Configure ACO pheromone evaporation
#'
#' Defines the pheromone-memory schedule independently from search stopping
#' patience. This separation is important for interpretable sensitivity tests:
#' changing how long SEMANTICA waits before stopping must not silently alter the
#' optimizer trajectory before that stopping point.
#'
#' @param mode `"adaptive"` (default) or `"fixed"`.
#' @param rho_start,rho_end Finite evaporation rates in `(0, 1)` used by the
#'   adaptive schedule. The rate changes linearly with iteration progress.
#' @param horizon Optional positive integer iteration horizon for the adaptive
#'   schedule. `NULL` resolves to finite `max_total_iter` when available and to
#'   50 iterations otherwise. It never depends on search patience.
#' @param rho Fixed evaporation rate in `(0, 1)` when `mode = "fixed"`.
#'
#' @return A validated `semantica_evaporation_config` list.
#' @export
semantica_evaporation_config <- function(
    mode = c("adaptive", "fixed"),
    rho_start = 0.35,
    rho_end = 0.10,
    horizon = NULL,
    rho = 0.05) {
  mode <- match.arg(mode)
  assert_rate <- function(x, name) {
    x <- suppressWarnings(as.numeric(x[1L]))
    if (length(x) != 1L || !is.finite(x) || x <= 0 || x >= 1) {
      stop("'", name, "' must be one finite number strictly between 0 and 1.", call. = FALSE)
    }
    x
  }
  rho_start <- assert_rate(rho_start, "rho_start")
  rho_end <- assert_rate(rho_end, "rho_end")
  rho <- assert_rate(rho, "rho")
  if (!is.null(horizon)) {
    horizon <- .semantica_assert_positive_integer(
      horizon, "horizon", condition_class = "semantica_error_input"
    )
  }
  structure(
    list(
      mode = mode,
      rho_start = rho_start,
      rho_end = rho_end,
      horizon = horizon,
      rho = rho
    ),
    class = c("semantica_evaporation_config", "list")
  )
}

.semantica_resolve_evaporation <- function(evaporation, max_total_iter,
                                            fixed_evaporation = NULL) {
  legacy_fixed <- !is.null(fixed_evaporation)
  if (legacy_fixed) {
    fixed_evaporation <- suppressWarnings(as.numeric(fixed_evaporation[1L]))
    if (length(fixed_evaporation) != 1L || !is.finite(fixed_evaporation) ||
        fixed_evaporation <= 0 || fixed_evaporation >= 1) {
      stop("'fixed_evaporation' must be NULL or one finite number strictly between 0 and 1.", call. = FALSE)
    }
    if (!is.null(evaporation)) {
      warning(
        "Both 'evaporation' and legacy 'fixed_evaporation' were supplied; ",
        "'fixed_evaporation' takes precedence for backward compatibility.",
        call. = FALSE
      )
    }
    cfg <- semantica_evaporation_config(mode = "fixed", rho = fixed_evaporation)
    cfg$source <- "legacy_fixed_evaporation"
    cfg$resolved_horizon <- NA_integer_
    return(cfg)
  }

  if (is.null(evaporation)) evaporation <- semantica_evaporation_config()
  if (!is.list(evaporation)) {
    stop("'evaporation' must be NULL or a semantica_evaporation_config/list.", call. = FALSE)
  }
  cfg <- do.call(
    semantica_evaporation_config,
    evaporation[intersect(names(evaporation), c("mode", "rho_start", "rho_end", "horizon", "rho"))]
  )
  cfg$source <- "evaporation_config"
  if (identical(cfg$mode, "adaptive")) {
    cfg$resolved_horizon <- if (!is.null(cfg$horizon)) {
      as.integer(cfg$horizon)
    } else if (is.finite(max_total_iter)) {
      as.integer(max_total_iter)
    } else {
      50L
    }
  } else {
    cfg$resolved_horizon <- NA_integer_
  }
  cfg
}

.semantica_evaporation_rho <- function(config, iteration) {
  if (identical(config$mode, "fixed")) return(as.numeric(config$rho))
  horizon <- max(1L, as.integer(config$resolved_horizon))
  progress <- min(1, max(0, as.numeric(iteration) / horizon))
  as.numeric(config$rho_start + progress * (config$rho_end - config$rho_start))
}

.semantica_constrained_search_space <- function(candidate_counts, selected_counts) {
  candidate_names <- names(candidate_counts)
  selected_names <- names(selected_counts)
  if (is.null(candidate_names) || is.null(selected_names)) {
    stop("Search-space counts must be named by factor.", call. = FALSE)
  }
  candidate_counts <- as.numeric(candidate_counts)
  selected_counts <- as.numeric(selected_counts)
  names(candidate_counts) <- candidate_names
  names(selected_counts) <- selected_names
  factors <- intersect(names(selected_counts), names(candidate_counts))
  candidate_counts <- candidate_counts[factors]
  selected_counts <- selected_counts[factors]
  impossible <- !is.finite(candidate_counts) | !is.finite(selected_counts) |
    selected_counts < 0 | candidate_counts < selected_counts
  log_by_factor <- rep(NA_real_, length(factors))
  log_by_factor[!impossible] <- lchoose(candidate_counts[!impossible], selected_counts[!impossible])
  log_total <- if (any(impossible)) Inf else sum(log_by_factor)
  log10_total <- if (is.finite(log_total)) log_total / log(10) else Inf
  exact_limit_log <- log(2^53) # largest integer range exactly representable by an R double
  total_exact <- if (is.finite(log_total) && log_total <= exact_limit_log) {
    round(exp(log_total))
  } else NA_real_
  total_approx <- if (is.finite(log_total) && log_total <= log(.Machine$double.xmax)) exp(log_total) else Inf
  by_factor <- data.frame(
    factor = factors,
    candidates = as.integer(candidate_counts),
    selected = as.integer(selected_counts),
    log10_combinations = log_by_factor / log(10),
    stringsAsFactors = FALSE
  )
  list(
    feasible = !any(impossible),
    by_factor = by_factor,
    log_total = log_total,
    log10_total = log10_total,
    total_combinations_exact = total_exact,
    total_combinations_approx = total_approx,
    exact_integer_representable = is.finite(log_total) && log_total <= exact_limit_log,
    interpretation = paste(
      "The constrained fixed-cardinality search space is the product of",
      "choose(n_f, k_f) across factors. SEMANTICA reports its size but does not",
      "claim that a fixed ACO budget covers a constant fraction of differently sized spaces."
    )
  )
}


# =================================================================
# 12  ACO_with_ESEM -- MAIN EXPORTED FUNCTION
# =================================================================
#' Ant Colony Optimization for Full-ESEM Scale Construction
#'
#' Performs exploratory item-subset selection for a target factor structure using
#' semantic similarity and full exploratory structural equation modeling (ESEM).
#' Search-time ESEM, PFA, and DFI diagnostics are sample-free proxies derived
#' from item embeddings. Optional response data add a separate final validation
#' fit; they do not change the proxy basis of the search.
#'
#' @details
#' The search constructs fixed-cardinality subsets from factor-labeled metadata:
#' the requested counts in `i.per.f` are enforced, while semantic, facet, PFA,
#' proxy-ESEM, loading, HTMT, and guard criteria are otherwise scored or softly
#' penalized according to the selected options. Factor membership is not inferred
#' or reassigned. The default `pfa_mode = "diagnostic"` reports PFA diagnostics
#' without adding them to the search objective.
#'
#' Search-time ESEM fits embedding-derived correlation matrices. In SEMANTICA
#' 0.3.0, search patience and pheromone evaporation are independent:
#' `search_patience` controls non-improvement/stagnation stopping, legacy
#' `max.iter` is its compatibility alias, `max_total_iter` is an optional hard
#' iteration ceiling, and `evaporation` controls pheromone memory. `max_esem_fits`
#' limits unique search-time ESEM candidate jobs admitted to execution. Cache
#' hits and coalesced duplicate requests do not consume that budget.
#' DFI calibration, solver fallback attempts, final archive/full refits, and
#' diagnostics are reported separately and can add fits outside that limit.
#' Treat the selected form as exploratory and confirm it with participant response
#' data; semantic similarity and proxy ESEM/PFA do not establish construct validity.
#'
#' @param cosine_sim_matrix Square symmetric matrix of cosine similarities.
#' @param df Dataframe with item metadata (`item`/`type` or `factor` columns).
#' @param model_type Currently unused placeholder for backward compatibility.
#' @param i.per.f Named integer vector of items per factor.
#' @param ants Number of ants in the colony.
#' @param max.iter Deprecated compatibility alias for `search_patience`. It no
#'   longer affects pheromone evaporation.
#' @param search_patience Optional positive integer non-improvement/stagnation
#'   patience. `NULL` resolves to `max.iter` for backward compatibility.
#' @param max_total_iter Optional hard total-iteration ceiling. `NULL`
#'   preserves the legacy patience-only stopping behavior; use a positive
#'   integer to impose a resource budget.
#' @param evaporation Pheromone-memory configuration from
#'   [semantica_evaporation_config()]. The default adaptive schedule depends on
#'   iteration progress, never on search patience.
#' @param max_esem_fits Optional ceiling for unique search-time ESEM candidate
#'   jobs admitted to execution. Cache hits and duplicate requests are not
#'   charged. DFI calibration, solver fallback attempts, archive/full refits,
#'   and final diagnostics are separately reported. `NULL` imposes no separate
#'   search-job ceiling.
#' @param esem_every Base interval for ESEM evaluations during ACO search.
#' @param esem_cadence_mode `"adaptive"` preserves entropy-responsive checkpoint
#'   scheduling; `"fixed"` evaluates ESEM exactly every `esem_every` iterations.
#' @param run_esem_during_search Logical; if `FALSE`, ACO selection uses semantic
#'   and, when configured, PFA criteria without search-time ESEM; ESEM is then run
#'   only for final diagnostics.
#' @param esem_weight Weight for the ESEM component in the objective function.
#' @param esem_failure_policy Behavior when an ESEM-guided search checkpoint
#'   produces no usable ESEM solutions. `"stop"` terminates rather than select
#'   without ESEM evidence. `"semantic_fallback"` uses semantic/PFA scoring for
#'   that checkpoint, records the fallback, and continues attempting ESEM at
#'   later checkpoints. Before final non-ESEM fallback, archived candidates are
#'   each given a full-ESEM refit opportunity.
#' @param esem_sample_size Sample size for DFI simulation and ESEM estimation.
#'   `"auto"` chooses a non-arbitrary reference N by RMSEA power analysis for
#'   detecting approximate misfit (`reference_rmsea_poor`) against close fit
#'   (`reference_rmsea_close`).
#' @param esem_eval_top_k Number of semantically best ants to evaluate with ESEM
#'   during ESEM iterations. `NULL` uses a conservative adaptive subset.
#' @param fast_esem Use a faster single-pass ESEM fit during ACO search. Final
#'   archive and final clean fits still use full ESEM fitting.
#' @param fast_esem_iter_max Iteration cap for fast ACO ESEM fits.
#' @param full_esem_iter_max Iteration cap for full final/archive ESEM fits.
#' @param elite_k Number of solutions to retain in the elite archive.
#' @param rotation Rotation method for ESEM (`"geomin"`, `"oblimin"`).
#' @param rotation_args Named list passed to `lavaan::sem(rotation.args)`.
#' @param data_type Data type for DFI (`"continuous"`, `"categorical"`, etc.).
#' @param original_data Original dataset (required for `likert`/`nonnormal` DFI).
#' @param target_loadings Target mean loading for DFI population model.
#' @param target_factor_cors Target factor correlation matrix for DFI.
#' @param dfi_reps Replications for DFI simulation.
#' @param dfi_level DFI cutoff level.
#' @param dfi_criterion DFI criterion (`"Sensitivity"`, `"Specificity"`).
#' @param dfi_mode Cutoff calibration mode. `"auto"` first tries semantic-ROC
#'   ESEM DFI, then semantic-approximate ESEM DFI, then exact ESEM-parametric
#'   DFI, then strict CFA-style DFI. `"semantic_roc_dfi"` calibrates cutoffs
#'   using acceptable and intentionally misspecified semantic-ESEM proxy
#'   populations. `"semantic_approx_dfi"` calibrates an ESEM approximate-fit
#'   population from the warm-up semantic residual structure. `"esem_parametric_dfi"`
#'   uses exact-H0 ESEM-parametric calibration, `"strict_cfa_dfi"` preserves the
#'   earlier CFA-style dynamic-fit calibration, and `"heuristic_semantic"` skips
#'   DFI simulation during search.
#' @param dfi_esem_reps Replications for ESEM DFI calibration. `NULL` uses a
#'   bounded value derived from `dfi_reps` to avoid excessive ESEM refits.
#' @param dfi_search_reps Optional explicit replication budget for search-time
#'   ESEM DFI calibration. When non-`NULL`, this overrides `dfi_esem_reps`;
#'   `final_dfi_reps` separately controls optional final recalibration.
#' @param dfi_roc_misspec_strength Strength of the structured misspecification
#'   alternatives used by `"semantic_roc_dfi"`.
#' @param dfi_esem_strategy ESEM DFI simulation strategy. `"fixed"` preserves
#'   the fixed replication budget; `"adaptive"` can stop opt-in batched ESEM DFI
#'   calibration only after simulated cutoffs stay within `dfi_adaptive_tol`.
#' @param dfi_adaptive_min_reps,dfi_adaptive_batch_reps,dfi_adaptive_tol,dfi_adaptive_stable_batches
#'   Opt-in adaptive ESEM DFI controls. `NULL` minimum reps uses a conservative
#'   floor relative to the requested ESEM DFI replication budget.
#' @param dfi_fallback_policy DFI fallback policy. `"conservative"` preserves
#'   SEMANTICA's ordered fallback calibration ladder; `"requested_only"` avoids
#'   extra ESEM DFI fallback calibrations for explicitly requested non-`"auto"`
#'   modes if their requested DFI calibration is unusable.
#' @param final_dfi_recalibrate Logical; if `TRUE`, recalibrate the selected
#'   semantic-proxy ESEM after ACO using the active ESEM DFI mode and fallback
#'   policy. The default is `FALSE` so final reporting uses the DFI cutoffs that
#'   guided the ACO search.
#' @param final_dfi_reps Optional replication count for final recalibration.
#'   `NULL` reuses `dfi_esem_reps`.
#' @param final_dddfi Logical; compute `dynamic::DDDFI()` for the final ESEM
#'   solution as an approximate-fit diagnostic. The default is `FALSE` so final
#'   reporting compares the selected solution to the DFI cutoffs already used by
#'   the search unless DDDFI is explicitly requested.
#' @param final_dddfi_reps Replications for final DDDFI.
#' @param final_dddfi_mad_target Which DDDFI MAD benchmark to use in the final
#'   verbose comparison (`"close"`, `"fair"`, or `"mediocre"`).
#' @param final_equivtest Logical; compute `dynamic::equivTest()` as a final
#'   equivalence-testing companion diagnostic when available.
#' @param loading_pattern Loading pattern for DFI fallback (`"varied"`, `"uniform"`).
#' @param embed_reliability Spearman attenuation factor (Fix B).
#' @param residual_inflation Residual inflation for DFI (Fix B).
#' @param dfi_warmup_iters Warm-up iterations before DFI calibration.
#' @param redundancy_threshold Semantic redundancy threshold.
#' @param dup_threshold Near-duplicate detection threshold.
#' @param htmt_threshold Descriptive reference for the HTMT-like semantic proxy.
#' @param htmt_objective_role Whether HTMT-like semantic overlap is diagnostic
#'   only (default) or enters the scalar proxy objective as a legacy penalty.
#' @param cohesion_quantile Deprecated inverse form of `cohesion_retention`.
#'   When supplied, retention is computed as `1 - cohesion_quantile`.
#' @param cohesion_retention Proportion of generated items nearest their
#'   factor's within-similarity target retained as ACO candidates before the
#'   minimum-pool safeguard is applied. Higher values retain broader content.
#' @param within_similarity_target Target within-factor semantic similarity.
#'   `NULL` estimates a dimension-specific target from the generated item pool.
#' @param within_similarity_band Tolerance around `within_similarity_target`.
#' @param semantic_objective_mode Multidimensional semantic objective.
#'   `"relative_conservative"` uses stochastic superiority plus a robust
#'   within-between median-gap score and treats the adaptive within target as a
#'   cohesion guard. `"legacy_target_burden"` reproduces the 0.4.x scoring
#'   regime. One-factor models always retain target-centered scoring because
#'   between-factor discrimination is not defined.
#' @param expected_factor_relations Optional theory-specified factor-relation matrix interpreted in the semantic-similarity domain, not as empirical latent correlations.
#' @param nomological_weight Optional weight between 0 and 1 for matching `expected_factor_relations`; defaults to zero and requires an explicit validation rationale before use.
#' @param content_alignment_mode Content-definition selection policy.
#'   `"diagnostic"` (default) reports alignment without restricting candidates;
#'   `"guard"` excludes only pool-relative clear factor mismatches or explicit
#'   exclusion conflicts when enough alternatives remain; `"off"` disables it.
#'   Facet ambiguity is retained as soft evidence rather than a hard exclusion.
#' @param polarity_action Wording-polarity selection policy. `"guard"` excludes
#'   flagged wording only when the requested factor size remains feasible;
#'   `"diagnostic"` (default) reports flags and `"off"` disables guarding.
#' @param within_target_method Automatic cohesion-target estimator when
#'   `within_similarity_target = NULL`: `"nonredundant_median"` (default) uses
#'   nonredundant within-factor similarities from the current pool/model;
#'   `"legacy_q40"` reproduces the older 0.25--0.55-clamped rule.
#' @param validation_n_on_inadmissible When validation-N planning is requested
#'   and the selected semantic-proxy ESEM is inadmissible, `"skip"` (default)
#'   avoids a misleading sample-size calculation; `"run"` forces the legacy
#'   PFA-informed sensitivity exercise.
#' @param facet_coverage_weight Soft weight rewarding coverage of distinct facets
#'   within each dimension.
#' @param psychometric_guard_weight Soft penalty/diagnostic strength for ESEM
#'   solutions with weak loadings, very low AVE, high HTMT, or improper
#'   standardized parameters; it is not a strict admissibility predicate.
#' @param psychometric_guard_min_ave,psychometric_guard_min_loading,psychometric_guard_min_primary_ge_50
#'   Minimum sample-free proxy-structure diagnostics used by the soft psychometric guard.
#' @param pfa_mode Sample-free pseudo-factor-analysis mode. `"diagnostic"`
#' @param pfa_failure_policy Behavior when objective-mode PFA is unavailable for
#'   a candidate: `"semantic_fallback"` (default) preserves its semantic score,
#'   `"penalize"` uses a zero PFA component for backward-compatible penalization,
#'   and `"stop"` rejects the candidate and aborts a checkpoint if none are usable.
#'   (the default) reports PFA diagnostics without adding them to the ACO
#'   objective, `"objective"` includes PFA simple-structure diagnostics in the
#'   ACO objective, and `"off"` disables them.
#' @param pfa_weight Weight for the PFA component in the semantic/PFA part of
#'   the ACO objective. The ESEM weight is applied after this composite is formed.
#' @param run_pfa_during_search Logical; when `pfa_mode = "objective"`, allow
#'   sample-free PFA diagnostics to enter ACO proposal scoring during search.
#'   Set to `FALSE` to keep PFA for final/diagnostic reporting only.
#' @param pfa_every Positive integer interval for PFA-guided ACO proposal
#'   scoring. The default `1L` preserves the previous objective-mode behavior;
#'   for example, `pfa_every = 10L` runs PFA-guided proposal scoring every tenth
#'   ACO iteration.
#' @param pfa_extraction Extraction used for ACO PFA scoring (`"principal"` is
#'   fast and sample-free; `"ml"` uses covariance-matrix ML via `factanal`).
#' @param pfa_final_extraction Extraction used for final reported PFA diagnostics.
#' @param pfa_rotation Rotation for PFA diagnostics. `"target_oblique"` uses
#'   fully specified oblique target rotation when `GPArotation` is available;
#'   `"oblimin"` uses direct oblimin via `GPArotation`; `"promax"` is an
#'   uninformed oblique fallback, `"varimax"` is orthogonal, and `"none"` is
#'   unrotated.
#' @param pfa_min_loading,pfa_min_margin Simple-structure thresholds for PFA.
#'   `pfa_min_margin = NULL` uses half of `pfa_min_loading`.
#' @param reference_rmsea_close,reference_rmsea_poor,reference_power,reference_alpha
#'   RMSEA-power settings used when `esem_sample_size = "auto"`.
#' @param reference_max_n Optional maximum reference N searched by the
#'   RMSEA-power solver. The default `Inf` lets `"auto"` use the calculated
#'   RMSEA-power solution. Set a finite value to impose a computational ceiling;
#'   if the requested target is not reachable within that ceiling, the ceiling
#'   itself is used as the semantic-proxy ESEM anchor rather than a very small
#'   positive-definite fallback.
#' @param semantic_n_sensitivity Logical; refit the final selected semantic-proxy
#'   ESEM over nearby reference-N anchors and report fit/structure stability.
#' @param semantic_n_grid Optional integer grid for semantic proxy N-sensitivity.
#'   `NULL` derives the grid from the chosen semantic reference N.
#' @param semantic_n_multipliers Multipliers applied to the reference N when
#'   `semantic_n_grid = NULL`.
#' @param semantic_n_iter_max Iteration cap for final semantic proxy
#'   N-sensitivity refits.
#' @param semantic_esem_score_mode ESEM score mode. `"current"` preserves the
#'   existing fit-index-weighted score; `"structure_weighted"` gives semantic
#'   factorial-structure diagnostics more weight.
#' @param validation_n_diagnostic Logical; estimate a response-data planning N
#'   from a PFA-derived population using fast Wishart Monte Carlo ESEM refits.
#' @param validation_n_reps Number of Monte Carlo replications per candidate N.
#' @param validation_n_grid Optional integer grid of candidate validation sample
#'   sizes. `NULL` uses an adaptive indicators-per-N grid.
#' @param validation_n_max Maximum candidate N for the validation-N diagnostic.
#' @param validation_n_convergence,validation_n_max_heywood,validation_n_min_recovery,validation_n_max_loading_error
#'   Monte Carlo criteria for the recommended validation N.
#' @param validation_n_min_dominance,validation_n_max_cross_error,validation_n_max_factor_cor_error
#'   Optional extra response-data planning criteria for dominance recovery,
#'   cross-loading recovery, and factor-correlation recovery. `NULL` reports
#'   the metric without using it to pass/fail the candidate N.
#' @param elite_multicriteria_rerank Logical; apply the scalar final ESEM
#'   diagnostic rerank/quality bonus to the elite archive. This is a
#'   multicriteria decision-utility rerank, not Pareto dominance or a
#'   multiobjective ACO archive. `NULL` resolves to `TRUE` unless the deprecated
#'   compatibility alias is supplied.
#' @param elite_pareto_rerank Deprecated compatibility alias for
#'   `elite_multicriteria_rerank`. Supplying it emits a deprecation warning;
#'   do not supply both arguments.
#' @param validation_data Optional item-response dataset for a separate final
#'   response-data validation fit using the selected items. It does not turn
#'   search-time proxy diagnostics into proof of construct validity.
#' @param validation_ordered Optional ordered item names for ordinal response-data
#'   validation.
#' @param sigmoid_center Sigmoid center for semantic scoring.
#' @param sigmoid_steepness Sigmoid steepness for semantic scoring.
#' @param heuristic_beta Heuristic influence in ACO probability.
#' @param archive_stable_window Stable semantic-archive updates required before
#'   semantic stabilization can contribute to early stopping. This is a search
#'   heuristic, not evidence of global optimality.
#' @param structural_archive_stable_window Stable successful PFA/ESEM archive
#'   updates required for each active structural evidence track.
#' @param min_successful_pfa_checkpoints,min_successful_esem_checkpoints Minimum
#'   successful structural checkpoints required before archive-based early
#'   stopping is eligible. These counts refer to evidence-bearing checkpoints,
#'   not raw iterations.
#' @param pheromone_update Pheromone update mode (`"top_elite"`, `"best_ant"`).
#' @param fixed_evaporation Fixed evaporation rate (NULL = adaptive).
#' @param debug_mode Simplified debug settings.
#' @param keep_solution_history Record evaluation history. Retained for
#'   backward compatibility; `FALSE` forces `history_mode = "none"`.
#' @param history_mode History retention policy. `"full"` preserves the legacy
#'   candidate-level evaluation history, `"summary"` stores one compact row per
#'   ACO iteration for lighter plotting and diagnostics, and `"none"` retains
#'   no history.
#' @param use_parallel Enable parallel processing.
#' @param n.cores Requested number of parallel workers, or `"auto"`. Explicit
#'   numeric requests are bounded by the CPU allocation visible to R and
#'   `max.cores`; automatic mode also applies `reserve.cores`.
#' @param reserve.cores Nonnegative number of cores retained for the operating
#'   system/user only when `n.cores = "auto"`.
#' @param max.cores Optional user ceiling on effective workers.
#' @param seed Optional master seed. Ant construction remains serial, while
#'   expensive task seeds are generated before dispatch so worker scheduling
#'   does not determine random-number streams.
#' @param verbose Print progress.
#' @param ... Additional arguments (unused).
#'
#' @section Side effects:
#' Consumes R RNG state intentionally for ACO search and enabled simulation/
#' sensitivity procedures, may create parallel workers, and performs CPU-based
#' ESEM/PFA/DFI computation. It does not perform provider network I/O.
#'
#' @section Reproducibility:
#' Use `seed` for repeatable stochastic execution. The result records the master
#' seed, task-level seeds, RNG kind, resource decisions, and evaluation telemetry.
#' Parallel and serial execution remain subject to the documented algorithmic
#' seed ledger rather than incidental cache/plot RNG use.
#'
#' @return A named list containing `best_items`, `esem_fit`, `dfi_cutoffs`,
#'   a unique `elite_archive`, psychometric indices, diagnostic metadata, and
#'   `selected_items_detail` when item text is available in `df`. Objective
#'   transparency fields include `proposal_objective_score`,
#'   `final_guided_objective_score`, `search_guidance_status`, and
#'   `objective_context`; the latter records the evidence regime and explicitly
#'   marks the objective as an optimization utility rather than a universal
#'   quality score. `selection_semantic_context` retains candidate-pool versus
#'   selected semantic discrimination/gap values, while
#'   `factor_semantic_diagnostics` exposes local factor separation.
#'   `candidate_counts` and ESEM attempt/success/failure telemetry are also
#'   returned. Quality and
#'   execution metadata are returned in `esem_alignment`,
#'   `esem_admissibility`, `semantic_pair_perturbation_stability`,
#'   `resource_plan`, `performance`, `evaluation_telemetry`, and
#'   `reproducibility`; `split_half_stability` remains a compatibility alias.
#'   `search_objective_score` is retained as a backward-compatible alias for
#'   `proposal_objective_score`.
#'
#' @references
#' Asparouhov, T., & Muthen, B. (2009). Exploratory structural equation
#' modeling. \emph{Structural Equation Modeling, 16}(3), 397-438.
#' \doi{10.1080/10705510903008204}
#'
#' Clark, L. A., & Watson, D. (2019). Constructing validity: New developments
#' in creating objective measuring instruments. \emph{Psychological
#' Assessment, 31}(12), 1412-1427. \doi{10.1037/pas0000626}
#'
#' Dorigo, M., & Stutzle, T. (2004). \emph{Ant Colony Optimization}. MIT Press.
#'
#' @export
#' @examples
#' \dontrun{
#' # See vignette("function-reference", package = "SEMANTICA") for an
#' # offline construction of the prepared object called `wrapped`.
#' result <- ACO_with_ESEM(
#'   cosine_sim_matrix = wrapped$cosine_sim_matrix,
#'   df = wrapped$df,
#'   i.per.f = c(Clarity = 3L, Flexibility = 3L),
#'   ants = 40L,
#'   max.iter = 20L,
#'   run_esem_during_search = FALSE,
#'   dfi_mode = "heuristic_semantic",
#'   pfa_mode = "diagnostic",
#'   use_parallel = FALSE,
#'   verbose = FALSE
#' )
#' }
ACO_with_ESEM <- function(
    cosine_sim_matrix, df = NULL, model_type = "correlated", i.per.f,
    ants = 90, max.iter = 50, search_patience = NULL,
    esem_every = 10, run_esem_during_search = TRUE,
    max_total_iter = NULL, max_esem_fits = NULL, evaporation = NULL,
    esem_weight = 0.50, esem_failure_policy = c("stop", "semantic_fallback"),
    esem_sample_size = "auto", elite_k = 10,
    esem_eval_top_k = NULL, fast_esem = TRUE, fast_esem_iter_max = 500L, full_esem_iter_max = 2000L,
    rotation = "geomin", rotation_args = list(geomin.epsilon = 0.50),
    data_type = "continuous", original_data = NULL, target_loadings = 0.70, target_factor_cors = NULL,
    dfi_reps = 500, dfi_level = 1, dfi_criterion = "Sensitivity",
    dfi_mode = c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi", "strict_cfa_dfi", "heuristic_semantic"),
    dfi_esem_reps = NULL, dfi_search_reps = NULL,
    final_dfi_recalibrate = FALSE, final_dfi_reps = NULL,
    dfi_roc_misspec_strength = 1.0,
    dfi_esem_strategy = c("fixed", "adaptive"),
    dfi_adaptive_min_reps = NULL, dfi_adaptive_batch_reps = 50L,
    dfi_adaptive_tol = 0.002, dfi_adaptive_stable_batches = 2L,
    dfi_fallback_policy = c("conservative", "requested_only"),
    final_dddfi = FALSE, final_dddfi_reps = 250L,
    final_dddfi_mad_target = c("close", "fair", "mediocre"),
    final_equivtest = TRUE,
    loading_pattern = "varied",
    embed_reliability = 1.0, residual_inflation = 0.0, dfi_warmup_iters = 5L,
    redundancy_threshold = 0.85, dup_threshold = 0.90, htmt_threshold = 0.85,
    cohesion_quantile = NULL, cohesion_retention = 0.75,
    within_similarity_target = NULL, within_similarity_band = 0.08,
    semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
    expected_factor_relations = NULL, nomological_weight = 0,
    facet_coverage_weight = 0.15, psychometric_guard_weight = 0.50,
    psychometric_guard_min_ave = 0.30,
    psychometric_guard_min_loading = 0.40,
    psychometric_guard_min_primary_ge_50 = 0.70,
    pfa_mode = c("diagnostic", "objective", "off"),
    pfa_weight = 0.20,
    pfa_failure_policy = c("semantic_fallback", "penalize", "stop"),
    run_pfa_during_search = TRUE,
    pfa_every = 1L,
    pfa_extraction = c("principal", "ml"),
    pfa_final_extraction = c("ml", "principal"),
    pfa_rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
    pfa_min_loading = psychometric_guard_min_loading,
    pfa_min_margin = NULL,
    reference_rmsea_close = 0.05,
    reference_rmsea_poor = 0.06,
    reference_power = 0.80,
    reference_alpha = 0.05,
    reference_max_n = Inf,
    semantic_n_sensitivity = TRUE,
    semantic_n_grid = NULL,
    semantic_n_multipliers = c(0.5, 1, 1.5, 2),
    semantic_n_iter_max = 800L,
    semantic_esem_score_mode = c("current", "structure_weighted"),
    validation_n_diagnostic = FALSE,
    validation_n_reps = 20L,
    validation_n_grid = NULL,
    validation_n_max = 2000L,
    validation_n_convergence = 0.90,
    validation_n_max_heywood = 0.05,
    validation_n_min_recovery = 0.90,
    validation_n_max_loading_error = 0.10,
    validation_n_min_dominance = NULL,
    validation_n_max_cross_error = NULL,
    validation_n_max_factor_cor_error = NULL,
    sigmoid_center = 0.15, sigmoid_steepness = 10,
    elite_pareto_rerank = NULL,
    validation_data = NULL, validation_ordered = NULL,
    heuristic_beta = 0.50, archive_stable_window = 8L,
    structural_archive_stable_window = 2L,
    min_successful_pfa_checkpoints = 2L,
    min_successful_esem_checkpoints = 2L,
    pheromone_update = c("top_elite", "best_ant"),
    fixed_evaporation = NULL, debug_mode = FALSE, keep_solution_history = TRUE,
    history_mode = c("full", "summary", "none"),
    use_parallel = TRUE, n.cores = 2L, reserve.cores = 1L,
    max.cores = NULL, seed = NULL, verbose = TRUE,
    content_alignment_mode = c("diagnostic", "guard", "off"),
    polarity_action = c("diagnostic", "guard", "off"),
    within_target_method = c("nonredundant_median", "legacy_q40"),
    validation_n_on_inadmissible = c("skip", "run"),
    esem_cadence_mode = c("adaptive", "fixed"),
    htmt_objective_role = c("diagnostic", "penalty"),
    elite_multicriteria_rerank = NULL, ...) {

  if (debug_mode) { pheromone_update <- "best_ant"; fixed_evaporation <- 0.05; archive_stable_window <- 3L; verbose <- TRUE }
  pheromone_update <- match.arg(pheromone_update)
  esem_failure_policy <- match.arg(esem_failure_policy)
  dfi_mode <- match.arg(dfi_mode)
  dfi_esem_strategy <- match.arg(dfi_esem_strategy)
  dfi_fallback_policy <- match.arg(dfi_fallback_policy)
  final_dddfi_mad_target <- match.arg(final_dddfi_mad_target)
  pfa_mode <- match.arg(pfa_mode)
  pfa_failure_policy <- match.arg(pfa_failure_policy)
  pfa_extraction <- match.arg(pfa_extraction)
  pfa_final_extraction <- match.arg(pfa_final_extraction)
  pfa_rotation <- match.arg(pfa_rotation)
  semantic_esem_score_mode <- match.arg(semantic_esem_score_mode)
  esem_cadence_mode <- match.arg(esem_cadence_mode)
  history_mode <- match.arg(history_mode)
  content_alignment_mode <- match.arg(content_alignment_mode)
  polarity_action <- match.arg(polarity_action)
  within_target_method <- match.arg(within_target_method)
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  htmt_objective_role <- match.arg(htmt_objective_role)
  validation_n_on_inadmissible <- match.arg(validation_n_on_inadmissible)
  if (!is.null(elite_multicriteria_rerank) && !is.null(elite_pareto_rerank)) {
    stop("Supply only 'elite_multicriteria_rerank'; 'elite_pareto_rerank' is a deprecated compatibility alias.", call. = FALSE)
  }
  if (is.null(elite_multicriteria_rerank)) {
    elite_multicriteria_rerank <- if (is.null(elite_pareto_rerank)) TRUE else elite_pareto_rerank
  }
  elite_multicriteria_rerank <- .semantica_assert_flag(elite_multicriteria_rerank, "elite_multicriteria_rerank")
  if (!is.null(elite_pareto_rerank)) {
    warning("'elite_pareto_rerank' is deprecated because the procedure is scalar multicriteria reranking, not Pareto dominance; use 'elite_multicriteria_rerank'.", call. = FALSE)
  }
  nomological_weight <- suppressWarnings(as.numeric(nomological_weight[1L]))
  if (length(nomological_weight) != 1L || !is.finite(nomological_weight) || nomological_weight < 0 || nomological_weight > 1) {
    stop("'nomological_weight' must be a finite number in [0, 1].")
  }
  if (nomological_weight > 0 && is.null(expected_factor_relations)) {
    warning("'nomological_weight' > 0 but no 'expected_factor_relations' were supplied; the nomological term will be inactive.", call. = FALSE)
  }
  if (!isTRUE(keep_solution_history)) history_mode <- "none"
  dots <- list(...)
  if (!is.null(dots$cfa_every)) esem_every <- dots$cfa_every
  if (!is.null(dots$cfa_weight)) esem_weight <- dots$cfa_weight
  if (!is.null(dots$cfa_sample_size)) esem_sample_size <- dots$cfa_sample_size
  # Validate controls that directly define ACO/search cadence before any RNG or
  # resource side effect. Compatibility aliases resolve into the same contract.
  esem_every <- .semantica_assert_positive_integer(
    esem_every, "esem_every", condition_class = "semantica_error_input"
  )
  ants <- .semantica_assert_positive_integer(
    ants, "ants", condition_class = "semantica_error_input"
  )
  max_iter_explicit <- !missing(max.iter)
  max.iter <- .semantica_assert_positive_integer(
    max.iter, "max.iter", condition_class = "semantica_error_input"
  )
  if (is.null(search_patience)) {
    search_patience <- max.iter
  } else {
    search_patience <- .semantica_assert_positive_integer(
      search_patience, "search_patience", condition_class = "semantica_error_input"
    )
    if (isTRUE(max_iter_explicit) && !identical(as.integer(max.iter), as.integer(search_patience))) {
      warning(
        "Both 'search_patience' and legacy 'max.iter' were supplied with different values; ",
        "'search_patience' controls stopping and 'max.iter' is ignored.",
        call. = FALSE
      )
    }
  }
  archive_stable_window <- .semantica_assert_positive_integer(
    archive_stable_window, "archive_stable_window", condition_class = "semantica_error_input"
  )
  structural_archive_stable_window <- .semantica_assert_positive_integer(
    structural_archive_stable_window, "structural_archive_stable_window",
    condition_class = "semantica_error_input"
  )
  min_successful_pfa_checkpoints <- .semantica_assert_positive_integer(
    min_successful_pfa_checkpoints, "min_successful_pfa_checkpoints",
    condition_class = "semantica_error_input"
  )
  min_successful_esem_checkpoints <- .semantica_assert_positive_integer(
    min_successful_esem_checkpoints, "min_successful_esem_checkpoints",
    condition_class = "semantica_error_input"
  )
  aco_start_time <- proc.time()[["elapsed"]]
  if (!is.null(seed)) {
    seed <- suppressWarnings(as.integer(seed[1L]))
    if (length(seed) != 1L || !is.finite(seed) || seed < 0L) {
      stop("'seed' must be NULL or one nonnegative integer.")
    }
    set.seed(seed)
  }
  rng_kind_initial <- RNGkind()
  rng_state_initial <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  resource_plan <- semantica_resource_plan(
    n.cores = n.cores,
    use_parallel = use_parallel,
    reserve.cores = reserve.cores,
    max.cores = max.cores
  )
  requested_n_cores <- resource_plan$requested_workers
  n.cores <- resource_plan$effective_workers
  use_parallel <- isTRUE(use_parallel) && n.cores > 1L
  if (isTRUE(verbose)) {
    cat("\n[SEMANTICA] Resolved resource plan before ACO/ESEM execution:\n")
    print(resource_plan)
  }
  pfa_every <- suppressWarnings(as.integer(pfa_every[1L]))
  if (length(pfa_every) != 1L || !is.finite(pfa_every) || pfa_every < 1L) {
    stop("'pfa_every' must be a positive integer.")
  }
  if (is.null(max_total_iter)) {
    max_total_iter <- Inf
  } else {
    max_total_iter <- suppressWarnings(as.numeric(max_total_iter[1L]))
    if (length(max_total_iter) != 1L || is.na(max_total_iter) ||
        (is.infinite(max_total_iter) && max_total_iter < 0) ||
        (!is.infinite(max_total_iter) && (!is.finite(max_total_iter) || max_total_iter < 1L))) {
      stop("'max_total_iter' must be NULL, Inf, or a positive integer.")
    }
    if (is.finite(max_total_iter)) max_total_iter <- as.integer(max_total_iter)
  }
  evaporation_resolved <- .semantica_resolve_evaporation(
    evaporation = evaporation,
    max_total_iter = max_total_iter,
    fixed_evaporation = fixed_evaporation
  )
  if (is.null(max_esem_fits)) {
    max_esem_fits <- Inf
  } else {
    max_esem_fits <- suppressWarnings(as.numeric(max_esem_fits[1L]))
    if (length(max_esem_fits) != 1L || is.na(max_esem_fits) ||
        (is.infinite(max_esem_fits) && max_esem_fits < 0) ||
        (!is.infinite(max_esem_fits) && (!is.finite(max_esem_fits) || max_esem_fits < 1L))) {
      stop("'max_esem_fits' must be NULL, Inf, or a positive integer.")
    }
    if (is.finite(max_esem_fits)) max_esem_fits <- as.integer(max_esem_fits)
  }
  esem_weight <- as.numeric(esem_weight)
  if (length(esem_weight) != 1L || is.na(esem_weight) || esem_weight < 0 || esem_weight > 1) {
    stop("'esem_weight' must be a single number between 0 and 1.")
  }
  run_esem_during_search <- isTRUE(run_esem_during_search) && esem_weight > 0
  if (!is.null(cohesion_quantile)) {
    cohesion_quantile <- suppressWarnings(as.numeric(cohesion_quantile[1L]))
    if (!is.finite(cohesion_quantile) || cohesion_quantile < 0 || cohesion_quantile > 1) {
      stop("'cohesion_quantile' must be NULL or a number between 0 and 1.")
    }
    cohesion_retention <- 1 - cohesion_quantile
    warning(
      "'cohesion_quantile' is deprecated because its direction is counterintuitive; ",
      "use 'cohesion_retention = ", sprintf("%.3f", cohesion_retention),
      "' to retain the same candidate proportion.",
      call. = FALSE
    )
  }
  cohesion_retention <- suppressWarnings(as.numeric(cohesion_retention[1L]))
  if (!is.finite(cohesion_retention) || cohesion_retention <= 0 || cohesion_retention > 1) {
    stop("'cohesion_retention' must be a number greater than 0 and no greater than 1.")
  }
  if (!is.null(dfi_search_reps)) {
    dfi_search_reps <- suppressWarnings(as.integer(dfi_search_reps[1L]))
    if (!is.finite(dfi_search_reps) || dfi_search_reps < 1L) {
      stop("'dfi_search_reps' must be NULL or a positive replication count.")
    }
    dfi_esem_reps <- max(20L, dfi_search_reps)
  } else if (!is.null(dfi_esem_reps)) {
    dfi_esem_reps <- suppressWarnings(as.integer(dfi_esem_reps[1L]))
    if (!is.finite(dfi_esem_reps) || dfi_esem_reps < 1L) {
      stop("'dfi_esem_reps' must be NULL or a positive replication count.")
    }
    dfi_esem_reps <- max(20L, dfi_esem_reps)
  } else {
    # ESEM-parametric DFI is much more expensive than CFA-style DFI because
    # each replication refits a rotated ESEM model. Cap the automatic default,
    # while allowing authors to request more precision explicitly.
    dfi_esem_reps <- max(50L, min(150L, as.integer(dfi_reps)))
  }
  dfi_adaptive_batch_reps <- suppressWarnings(as.integer(dfi_adaptive_batch_reps[1L]))
  if (!is.finite(dfi_adaptive_batch_reps) || dfi_adaptive_batch_reps < 1L) {
    stop("'dfi_adaptive_batch_reps' must be a positive replication count.")
  }
  dfi_adaptive_tol <- suppressWarnings(as.numeric(dfi_adaptive_tol[1L]))
  if (!is.finite(dfi_adaptive_tol) || dfi_adaptive_tol <= 0) {
    stop("'dfi_adaptive_tol' must be a positive numeric cutoff tolerance.")
  }
  dfi_adaptive_stable_batches <- suppressWarnings(as.integer(dfi_adaptive_stable_batches[1L]))
  if (!is.finite(dfi_adaptive_stable_batches) || dfi_adaptive_stable_batches < 1L) {
    stop("'dfi_adaptive_stable_batches' must be a positive integer.")
  }
  if (!is.null(dfi_adaptive_min_reps)) {
    dfi_adaptive_min_reps <- suppressWarnings(as.integer(dfi_adaptive_min_reps[1L]))
    if (!is.finite(dfi_adaptive_min_reps) || dfi_adaptive_min_reps < 1L) {
      stop("'dfi_adaptive_min_reps' must be NULL or a positive replication count.")
    }
  }
  final_dfi_recalibrate <- isTRUE(final_dfi_recalibrate)
  if (!is.null(final_dfi_reps)) {
    final_dfi_reps <- suppressWarnings(as.integer(final_dfi_reps[1L]))
    if (!is.finite(final_dfi_reps) || final_dfi_reps < 1L) {
      stop("'final_dfi_reps' must be NULL or a positive replication count.")
    }
    final_dfi_reps <- max(20L, final_dfi_reps)
  } else {
    final_dfi_reps <- dfi_esem_reps
  }
  final_dddfi <- isTRUE(final_dddfi)
  final_equivtest <- isTRUE(final_equivtest)
  final_dddfi_reps <- max(20L, as.integer(final_dddfi_reps))
  fast_esem_iter_max <- max(100L, as.integer(fast_esem_iter_max))
  full_esem_iter_max <- max(fast_esem_iter_max, as.integer(full_esem_iter_max))
  within_similarity_band <- as.numeric(within_similarity_band)
  if (length(within_similarity_band) != 1L || is.na(within_similarity_band) || within_similarity_band <= 0) {
    stop("'within_similarity_band' must be a positive number.")
  }
  facet_coverage_weight <- as.numeric(facet_coverage_weight)
  if (length(facet_coverage_weight) != 1L || is.na(facet_coverage_weight) || facet_coverage_weight < 0 || facet_coverage_weight > 1) {
    stop("'facet_coverage_weight' must be a single number between 0 and 1.")
  }
  psychometric_guard_weight <- as.numeric(psychometric_guard_weight)
  if (length(psychometric_guard_weight) != 1L || is.na(psychometric_guard_weight) || psychometric_guard_weight < 0 || psychometric_guard_weight > 1) {
    stop("'psychometric_guard_weight' must be a single number between 0 and 1.")
  }
  psychometric_guard_min_ave <- as.numeric(psychometric_guard_min_ave)
  if (length(psychometric_guard_min_ave) != 1L || is.na(psychometric_guard_min_ave) || psychometric_guard_min_ave < 0 || psychometric_guard_min_ave > 1) {
    stop("'psychometric_guard_min_ave' must be a single number between 0 and 1.")
  }
  psychometric_guard_min_loading <- as.numeric(psychometric_guard_min_loading)
  if (length(psychometric_guard_min_loading) != 1L || is.na(psychometric_guard_min_loading) || psychometric_guard_min_loading < 0 || psychometric_guard_min_loading > 1) {
    stop("'psychometric_guard_min_loading' must be a single number between 0 and 1.")
  }
  psychometric_guard_min_primary_ge_50 <- as.numeric(psychometric_guard_min_primary_ge_50)
  if (length(psychometric_guard_min_primary_ge_50) != 1L || is.na(psychometric_guard_min_primary_ge_50) || psychometric_guard_min_primary_ge_50 < 0 || psychometric_guard_min_primary_ge_50 > 1) {
    stop("'psychometric_guard_min_primary_ge_50' must be a single number between 0 and 1.")
  }
  pfa_weight <- as.numeric(pfa_weight)
  if (length(pfa_weight) != 1L || is.na(pfa_weight) || pfa_weight < 0 || pfa_weight > 1) {
    stop("'pfa_weight' must be a single number between 0 and 1.")
  }
  if (pfa_mode != "objective") pfa_weight <- 0
  run_pfa_during_search <- isTRUE(run_pfa_during_search) &&
    pfa_mode == "objective" && pfa_weight > 0
  pfa_min_loading <- as.numeric(pfa_min_loading)
  if (length(pfa_min_loading) != 1L || is.na(pfa_min_loading) || pfa_min_loading <= 0 || pfa_min_loading > 1) {
    stop("'pfa_min_loading' must be a single number in (0, 1].")
  }
  if (!is.null(pfa_min_margin)) {
    pfa_min_margin <- as.numeric(pfa_min_margin)
    if (length(pfa_min_margin) != 1L || is.na(pfa_min_margin) || pfa_min_margin <= 0 || pfa_min_margin > 1) {
      stop("'pfa_min_margin' must be NULL or a single number in (0, 1].")
    }
  }
  reference_rmsea_close <- as.numeric(reference_rmsea_close)
  reference_rmsea_poor <- as.numeric(reference_rmsea_poor)
  reference_power <- as.numeric(reference_power)
  reference_alpha <- as.numeric(reference_alpha)
  reference_max_n_value <- if (is.null(reference_max_n)) {
    Inf
  } else {
    suppressWarnings(as.numeric(reference_max_n[1L]))
  }
  if (length(reference_max_n_value) == 0L ||
      is.na(reference_max_n_value) ||
      reference_max_n_value <= 0) {
    reference_max_n_value <- Inf
  }
  reference_max_n <- if (is.finite(reference_max_n_value)) {
    max(50L, as.integer(reference_max_n_value))
  } else {
    Inf
  }
  semantic_n_sensitivity <- isTRUE(semantic_n_sensitivity)
  semantic_n_iter_max <- max(100L, as.integer(semantic_n_iter_max))
  semantic_n_multipliers <- sort(unique(as.numeric(semantic_n_multipliers)))
  semantic_n_multipliers <- semantic_n_multipliers[is.finite(semantic_n_multipliers) & semantic_n_multipliers > 0]
  if (length(semantic_n_multipliers) == 0L) semantic_n_multipliers <- c(0.5, 1, 1.5, 2)
  if (!is.null(semantic_n_grid)) {
    semantic_n_grid <- sort(unique(as.integer(semantic_n_grid)))
    semantic_n_grid <- semantic_n_grid[is.finite(semantic_n_grid) & semantic_n_grid > 0L]
    if (length(semantic_n_grid) == 0L) semantic_n_grid <- NULL
  }
  validation_n_diagnostic <- isTRUE(validation_n_diagnostic)
  validation_n_reps <- max(5L, as.integer(validation_n_reps))
  validation_n_max <- max(100L, as.integer(validation_n_max))
  validation_n_convergence <- as.numeric(validation_n_convergence)
  validation_n_max_heywood <- as.numeric(validation_n_max_heywood)
  validation_n_min_recovery <- as.numeric(validation_n_min_recovery)
  validation_n_max_loading_error <- as.numeric(validation_n_max_loading_error)
  if (!is.finite(reference_rmsea_close) || !is.finite(reference_rmsea_poor) ||
      reference_rmsea_close < 0 || reference_rmsea_poor <= reference_rmsea_close) {
    stop("'reference_rmsea_close' must be >= 0 and smaller than 'reference_rmsea_poor'.")
  }
  if (!is.finite(reference_power) || reference_power <= 0 || reference_power >= 1) {
    stop("'reference_power' must be a probability between 0 and 1.")
  }
  if (!is.finite(reference_alpha) || reference_alpha <= 0 || reference_alpha >= 1) {
    stop("'reference_alpha' must be a probability between 0 and 1.")
  }
  if (!is.finite(validation_n_convergence) || validation_n_convergence <= 0 || validation_n_convergence > 1 ||
      !is.finite(validation_n_max_heywood) || validation_n_max_heywood < 0 || validation_n_max_heywood >= 1 ||
      !is.finite(validation_n_min_recovery) || validation_n_min_recovery <= 0 || validation_n_min_recovery > 1 ||
      !is.finite(validation_n_max_loading_error) || validation_n_max_loading_error <= 0 || validation_n_max_loading_error > 1) {
    stop("Validation-N diagnostic criteria must be finite probabilities/error thresholds in valid ranges.")
  }
  optional_validation_values <- list(
    validation_n_min_dominance = validation_n_min_dominance,
    validation_n_max_cross_error = validation_n_max_cross_error,
    validation_n_max_factor_cor_error = validation_n_max_factor_cor_error
  )
  for (nm in names(optional_validation_values)) {
    val <- optional_validation_values[[nm]]
    if (!is.null(val)) {
      val <- suppressWarnings(as.numeric(val[1L]))
      if (!is.finite(val) || val < 0 || val > 1) {
        stop(sprintf("'%s' must be NULL or a finite number between 0 and 1.", nm))
      }
      assign(nm, val)
    }
  }
  if (!is.null(validation_n_grid)) {
    validation_n_grid <- sort(unique(as.integer(validation_n_grid)))
    validation_n_grid <- validation_n_grid[is.finite(validation_n_grid) & validation_n_grid > 0L]
    if (length(validation_n_grid) == 0L) validation_n_grid <- NULL
  }
  dfi_roc_misspec_strength <- as.numeric(dfi_roc_misspec_strength)
  if (length(dfi_roc_misspec_strength) != 1L || is.na(dfi_roc_misspec_strength) || dfi_roc_misspec_strength <= 0) {
    stop("'dfi_roc_misspec_strength' must be a positive number.")
  }
  dfi_roc_misspec_strength <- min(2.0, dfi_roc_misspec_strength)

  if (is.data.frame(cosine_sim_matrix)) cosine_sim_matrix <- as.matrix(cosine_sim_matrix)
  if (inherits(cosine_sim_matrix, "Matrix")) cosine_sim_matrix <- as.matrix(cosine_sim_matrix)
  storage.mode(cosine_sim_matrix) <- "double"
  if (!is.matrix(cosine_sim_matrix) || length(dim(cosine_sim_matrix)) != 2L) stop("cosine_sim_matrix must be a matrix.")
  if (nrow(cosine_sim_matrix) != ncol(cosine_sim_matrix)) stop("cosine_sim_matrix must be square.")
  if (is.null(rownames(cosine_sim_matrix)) || is.null(colnames(cosine_sim_matrix))) stop("cosine_sim_matrix must have rownames and colnames.")

  if (is.null(df)) stop("'df' (item metadata dataframe) is required.")
  id_col <- NULL
  # Prefer explicit identifier columns over `item`, which often contains item
  # wording in generated metadata and would not match the cosine matrix names.
  for (cc in c("ID", "item_id", "id", "item_name", "Item", "item")) { if (cc %in% colnames(df)) { id_col <- cc; break } }
  item_ids <- if (!is.null(id_col)) as.character(df[[id_col]]) else rownames(df)
  if (anyNA(item_ids) || any(!nzchar(trimws(item_ids)))) stop("Item IDs in 'df' must be non-missing and non-empty.")
  if (anyDuplicated(item_ids)) stop("Item IDs in 'df' must be unique.")

  type_col <- NULL
  for (cc in c("type", "Type", "factor", "Factor", "dimension", "Dimension", "scale", "Scale")) { if (cc %in% colnames(df)) { type_col <- cc; break } }
  if (is.null(type_col)) stop("Cannot find factor-assignment column in 'df'.")
  item_types <- as.character(df[[type_col]])

  facet_col <- NULL
  for (cc in c("Facet", "facet", "subfacet", "domain", "Domain")) { if (cc %in% colnames(df)) { facet_col <- cc; break } }
  item_facets_all <- if (!is.null(facet_col)) as.character(df[[facet_col]]) else item_types

  if (missing(i.per.f) || is.null(i.per.f)) stop("'i.per.f' must be a named integer vector.")
  i_per_names <- names(i.per.f)
  if (is.null(i_per_names) || any(!nzchar(i_per_names))) {
    stop("'i.per.f' must be named with the factor names to optimize.")
  }
  i.per.f <- suppressWarnings(as.integer(i.per.f))
  names(i.per.f) <- i_per_names
  if (anyNA(i.per.f) || any(i.per.f < 1L)) {
    stop("'i.per.f' values must be positive integers.")
  }
  factors <- names(i.per.f); n_factors <- length(factors)
  if (is.null(esem_eval_top_k)) {
    esem_eval_top_k_eff <- min(as.integer(ants), max(as.integer(elite_k), ceiling(as.integer(ants) * 0.35)))
  } else {
    if (length(esem_eval_top_k) != 1L || is.na(esem_eval_top_k)) {
      stop("'esem_eval_top_k' must be NULL, Inf, or a positive integer.")
    }
    if (is.infinite(esem_eval_top_k)) {
      esem_eval_top_k_eff <- as.integer(ants)
    } else {
      esem_eval_top_k <- suppressWarnings(as.integer(esem_eval_top_k))
      esem_eval_top_k_eff <- max(1L, min(as.integer(ants), esem_eval_top_k))
    }
  }
  missing_f <- setdiff(factors, unique(item_types))
  if (length(missing_f) > 0L) stop("Factors not found in df: ", paste(missing_f, collapse = ", "))

  list.items <- lapply(setNames(factors, factors), function(f) intersect(item_ids[item_types == f], rownames(cosine_sim_matrix)))
  generated_counts <- vapply(list.items, length, integer(1L))
  generated_search_space <- .semantica_constrained_search_space(generated_counts, i.per.f)

  # Content/polarity guards are feasibility-aware: they only restrict a factor
  # when enough alternatives remain to satisfy its requested item count. This
  # prevents method QA from making valid user configurations impossible.
  guard_audit <- list()
  apply_feasible_guard <- function(lists, eligible_ids, label) {
    audit <- list()
    if (is.null(eligible_ids)) return(list(lists = lists, audit = audit))
    for (f in factors) {
      before <- length(lists[[f]])
      candidate <- intersect(lists[[f]], eligible_ids)
      enforce <- length(candidate) >= i.per.f[[f]]
      if (enforce) {
        lists[[f]] <- candidate
      } else if (verbose) {
        warning(sprintf("%s guard not enforced for factor '%s': only %d eligible candidate(s) for a target of %d; retaining the broader pool.",
                        label, f, length(candidate), i.per.f[[f]]), call.=FALSE)
      }
      audit[[f]] <- list(before = before, eligible = length(candidate), after = length(lists[[f]]), enforced = enforce)
    }
    list(lists = lists, audit = audit)
  }
  if (identical(content_alignment_mode, "guard")) {
    if ("semantica_content_guard_pass" %in% names(df)) {
      guard_pass <- is.na(df$semantica_content_guard_pass) |
        as.logical(df$semantica_content_guard_pass)
      aligned_ids <- item_ids[guard_pass]
      tmp_guard <- apply_feasible_guard(
        list.items, aligned_ids, "Construct-alignment"
      )
      list.items <- tmp_guard$lists
      guard_audit$content_alignment <- tmp_guard$audit
      guard_audit$content_alignment_rule <- paste(
        "Pool-relative conservative guard: raw clear factor mismatches and explicit",
        "exclusion conflicts are removed only when the exclusionary conclusion",
        "also survives the mean-centered sensitivity view and enough alternatives",
        "remain; ambiguous or preprocessing-sensitive cases remain eligible.",
        "The centered view never enters the ACO objective."
      )
    } else if ("semantica_factor_aligned" %in% names(df)) {
      # Backward-compatible fallback for metadata created by older SEMANTICA
      # versions. New 0.2.7 pipelines provide semantica_content_guard_pass.
      factor_ok <- is.na(df$semantica_factor_aligned) |
        as.logical(df$semantica_factor_aligned)
      aligned_ids <- item_ids[factor_ok]
      tmp_guard <- apply_feasible_guard(
        list.items, aligned_ids, "Construct-alignment (legacy rank)"
      )
      list.items <- tmp_guard$lists
      guard_audit$content_alignment <- tmp_guard$audit
      guard_audit$content_alignment_rule <- "legacy assigned-factor top-rank fallback"
    }
  }
  if (identical(polarity_action, "guard") && "semantica_polarity_flag" %in% names(df)) {
    safe_ids <- item_ids[is.na(df$semantica_polarity_flag) | !as.logical(df$semantica_polarity_flag)]
    tmp_guard <- apply_feasible_guard(list.items, safe_ids, "Polarity")
    list.items <- tmp_guard$lists; guard_audit$polarity <- tmp_guard$audit
  }
  guarded_counts <- vapply(list.items, length, integer(1L))
  eligible_search_space <- .semantica_constrained_search_space(guarded_counts, i.per.f)
  item.vector <- unlist(list.items, use.names = FALSE)
  item.factor.lookup <- stats::setNames(
    rep(factors, vapply(list.items, length, integer(1L))),
    item.vector
  )
  item.facet.lookup <- stats::setNames(item_facets_all[match(item.vector, item_ids)], item.vector)
  # Facet labels keep coverage credit through ordinary ambiguity. Only a clear
  # pool-relative facet mismatch removes facet credit; this avoids turning tiny
  # top-rank differences between neighboring facets into hard coverage losses.
  if ("semantica_facet_clear_mismatch" %in% names(df)) {
    facet_diag <- df$semantica_facet_clear_mismatch[match(item.vector, item_ids)]
    facet_misaligned <- !is.na(facet_diag) & as.logical(facet_diag)
    item.facet.lookup[facet_misaligned] <- item.factor.lookup[names(item.facet.lookup)[facet_misaligned]]
  } else if ("semantica_facet_aligned" %in% names(df)) {
    # Backward-compatible fallback for metadata produced before 0.2.7.
    facet_diag <- df$semantica_facet_aligned[match(item.vector, item_ids)]
    facet_misaligned <- !is.na(facet_diag) & !as.logical(facet_diag)
    item.facet.lookup[facet_misaligned] <- item.factor.lookup[names(item.facet.lookup)[facet_misaligned]]
  }
  bad_facet <- is.na(item.facet.lookup) | !nzchar(item.facet.lookup)
  bad_facet[is.na(bad_facet)] <- TRUE
  item.facet.lookup[bad_facet] <- item.factor.lookup[names(item.facet.lookup)[bad_facet]]
  facets.by.factor <- lapply(setNames(factors, factors), function(f) {
    f_facets <- unique(item.facet.lookup[intersect(list.items[[f]], names(item.facet.lookup))])
    f_facets[!is.na(f_facets) & nzchar(f_facets)]
  })
  n_items <- length(item.vector)
  if (!all(item.vector %in% rownames(cosine_sim_matrix))) stop("Some items from df not found in cosine_sim_matrix.")
  if (any(vapply(list.items, length, integer(1L)) < i.per.f[factors])) stop("Not enough items for at least one factor.")
  duplicate_clusters <- identify_duplicate_clusters(
    cosine_sim_matrix, item.vector,
    threshold = dup_threshold,
    exact_threshold = max(0.9995, dup_threshold)
  )
  duplicate_cluster_id <- duplicate_clusters$item_cluster
  duplicate_guard_infeasible <- FALSE
  duplicate_feasibility <- do.call(rbind, lapply(factors, function(f) {
    f_items <- list.items[[f]]
    cid <- duplicate_cluster_id[f_items]
    clustered <- !is.na(cid) & nzchar(cid)
    independent_units <- sum(!clustered) + length(unique(cid[clustered]))
    requested <- as.integer(i.per.f[[f]])
    feasible <- independent_units >= requested
    data.frame(
      factor = f,
      eligible_items = length(f_items),
      requested_selected = requested,
      independent_duplicate_units = independent_units,
      feasible = feasible,
      status = if (feasible) "feasible" else "infeasible",
      stringsAsFactors = FALSE
    )
  }))
  rownames(duplicate_feasibility) <- NULL
  duplicate_guard_infeasible <- any(!duplicate_feasibility$feasible)

  reference_n_info <- estimate_esem_reference_sample_size(
    items_per_factor = i.per.f,
    n_factors = n_factors,
    rmsea_null = reference_rmsea_close,
    rmsea_alt = reference_rmsea_poor,
    power = reference_power,
    alpha = reference_alpha,
    min_n = NULL,
    max_n = reference_max_n
  )
  sample_size_auto <- is.null(esem_sample_size) ||
    (length(esem_sample_size) == 1L && is.character(esem_sample_size) &&
       tolower(esem_sample_size) %in% c("auto", "adaptive", "rmsea_power")) ||
    (length(esem_sample_size) == 1L && is.numeric(esem_sample_size) && is.na(esem_sample_size))
  if (sample_size_auto) {
    esem_sample_size <- reference_n_info$n_obs
  } else {
    esem_sample_size <- suppressWarnings(as.integer(esem_sample_size[[1L]]))
    if (is.na(esem_sample_size) || esem_sample_size <= reference_n_info$p + 2L) {
      stop("'esem_sample_size' must be 'auto' or an integer greater than the number of selected indicators + 2.")
    }
  }
  reference_n_info$used_n_obs <- as.integer(esem_sample_size)
  reference_n_info$auto <- isTRUE(sample_size_auto)

  if (verbose) { cat("\n============================================================\nPHASE 0 -- ITEM COHESION PRE-FILTER\n============================================================\n") }
  if (verbose) {
    cat(sprintf("  Semantic-proxy reference N for RMSEA-power fit sensitivity: %d (%s; df=%.1f, RMSEA %.3f vs %.3f, power %.2f)\n",
                esem_sample_size,
                if (isTRUE(reference_n_info$auto)) "auto RMSEA-power" else "user supplied",
                reference_n_info$df,
                reference_n_info$rmsea_null,
                reference_n_info$rmsea_alt,
                reference_n_info$power))
    cat("  Reference N role : fit/DFI anchor for the embedding-derived correlation proxy; not a respondent validation sample size.\n")
    if (isTRUE(reference_n_info$underpowered_at_max_n)) {
      cat("  Reference N note : target power was not reached within reference_max_n; using the ceiling as the proxy ESEM anchor.\n")
    }
    if (isTRUE(reference_n_info$low_df_warning)) {
      cat("  Reference N note : low approximate EFA df; inspect semantic proxy N-sensitivity diagnostics.\n")
    }
    if (duplicate_clusters$n_clusters > 0L) {
      cat(sprintf("  Duplicate clusters: %d clusters / %d items at cosine >= %.3f%s\n",
                  duplicate_clusters$n_clusters,
                  duplicate_clusters$n_items_clustered,
                  duplicate_clusters$threshold,
                  if (duplicate_clusters$n_exact_pairs > 0L) sprintf(" (%d near-exact pairs)", duplicate_clusters$n_exact_pairs) else ""))
      if (isTRUE(duplicate_guard_infeasible)) {
        bad_dup <- duplicate_feasibility$factor[!duplicate_feasibility$feasible]
        cat(sprintf(
          "  Duplicate guard: infeasible for %s; local soft-penalty fallback may be used.\n",
          paste(bad_dup, collapse = ", ")
        ))
      }
    }
  }
  within_similarity_target_eff <- estimate_within_similarity_targets(
    list.items, cosine_sim_matrix, factors, within_similarity_target,
    redundancy_threshold = redundancy_threshold,
    within_similarity_band = within_similarity_band,
    method = within_target_method
  )
  content_alignment_margin <- NULL
  if ("semantica_factor_margin" %in% names(df)) {
    vals <- suppressWarnings(as.numeric(df$semantica_factor_margin))
    content_alignment_margin <- stats::setNames(vals, item_ids)
  }
  eligible.items <- compute_eligible_items(
    list.items, cosine_sim_matrix, factors, i.per.f, cohesion_retention, 0.15,
    within_similarity_target_eff, within_similarity_band,
    semantic_objective_mode = semantic_objective_mode,
    content_alignment_margin = content_alignment_margin
  )
  item_heuristics <- compute_item_heuristics(
    eligible.items, cosine_sim_matrix, factors,
    within_similarity_target_eff, within_similarity_band,
    semantic_objective_mode = semantic_objective_mode,
    content_alignment_margin = content_alignment_margin
  )
  if (verbose) {
    retention_basis <- attr(eligible.items, "selection_basis") %||% "unknown"
    cat(sprintf("  Candidate retention: %.1f%% semantic pool retained before minimum-pool safeguard (%s)\n",
                100 * cohesion_retention, retention_basis))
    cat(sprintf("  Automatic within-target method: %s (%s)\n",
                attr(within_similarity_target_eff, "method") %||% within_target_method,
                if (identical(semantic_objective_mode, "relative_conservative") && length(factors) > 1L) {
                  "cohesion guard; not primary multidimensional quality"
                } else "active target-centered objective"))
    for (f in factors) {
      cat(sprintf(
        "  %-28s: %d generated -> %d after quality guards -> %d semantic-eligible -> %d selected target\n",
        f, generated_counts[[f]], guarded_counts[[f]],
        length(eligible.items[[f]]), i.per.f[[f]]
      ))
    }
  }
  candidate_counts <- data.frame(
    factor = factors,
    generated = as.integer(generated_counts[factors]),
    after_quality_guards = as.integer(guarded_counts[factors]),
    eligible = vapply(eligible.items[factors], length, integer(1L)),
    selected_target = as.integer(i.per.f[factors]),
    stringsAsFactors = FALSE
  )
  safe_ratio <- function(num, den) {
    out <- rep(NA_real_, length(num))
    ok <- is.finite(num) & is.finite(den) & den > 0
    out[ok] <- num[ok] / den[ok]
    out
  }
  candidate_counts$guard_retention <- safe_ratio(
    candidate_counts$after_quality_guards, candidate_counts$generated
  )
  candidate_counts$semantic_retention_after_guard <- safe_ratio(
    candidate_counts$eligible, candidate_counts$after_quality_guards
  )
  candidate_counts$selection_pressure_after_guard <- safe_ratio(
    candidate_counts$selected_target, candidate_counts$after_quality_guards
  )
  candidate_counts$selection_pressure_eligible <- safe_ratio(
    candidate_counts$selected_target, candidate_counts$eligible
  )
  guard_audit$pressure <- candidate_counts
  guard_audit$pressure_note <- paste(
    "Guard-retention and selection-pressure ratios are descriptive capacity diagnostics only;",
    "they do not alter ACO weights, semantic scores, or validity conclusions."
  )

  # Decompose decisions already made by the robust content guard. This table is
  # provenance only and does not introduce an additional exclusion criterion.
  flag_col <- function(name) {
    if (!name %in% names(df)) return(rep(FALSE, length(item_ids)))
    x <- df[[name]]
    !is.na(x) & as.logical(x)
  }
  robust_factor_conflict_all <- flag_col("semantica_factor_clear_mismatch") &
    flag_col("semantica_factor_clear_mismatch_centered")
  robust_forbidden_conflict_all <- flag_col("semantica_exclusion_conflict") &
    flag_col("semantica_exclusion_conflict_centered")
  robust_any_conflict_all <- robust_factor_conflict_all | robust_forbidden_conflict_all
  raw_any_conflict_all <- flag_col("semantica_factor_clear_mismatch") |
    flag_col("semantica_exclusion_conflict")
  retained_by_sensitivity_all <- raw_any_conflict_all & !robust_any_conflict_all
  reason_table <- data.frame(
    item_id = item_ids, factor = item_types,
    robust_factor_mismatch = robust_factor_conflict_all,
    robust_forbidden_conflict = robust_forbidden_conflict_all,
    robust_both = robust_factor_conflict_all & robust_forbidden_conflict_all,
    raw_conflict_retained_by_sensitivity = retained_by_sensitivity_all,
    stringsAsFactors = FALSE
  )
  guard_audit$content_exclusion_reasons <- reason_table[
    robust_any_conflict_all | retained_by_sensitivity_all, , drop = FALSE
  ]
  guard_audit$content_exclusion_reason_note <- paste(
    "Reason counts decompose the already-applied robust content guard into",
    "factor-definition mismatch and analyst-specified forbidden-concept conflict;",
    "overlap is reported explicitly and is not double-counted in total exclusions."
  )

  # Pool-health context distinguishes a constrained candidate pool from an
  # optimizer failure. These labels describe operational selection capacity;
  # they are not construct-validity judgments and use no model-specific cutoff.
  pool_health <- do.call(rbind, lapply(factors, function(f) {
    ii <- which(as.character(df[[type_col]]) == f)
    f_status <- if ("semantica_factor_alignment_status" %in% names(df)) {
      as.character(df$semantica_factor_alignment_status[ii])
    } else rep(NA_character_, length(ii))
    facet_status <- if ("semantica_facet_alignment_status" %in% names(df)) {
      as.character(df$semantica_facet_alignment_status[ii])
    } else rep(NA_character_, length(ii))
    dup_row <- duplicate_feasibility[duplicate_feasibility$factor == f, , drop = FALSE]
    alignment_available <- any(!is.na(f_status) & nzchar(f_status))
    facet_alignment_available <- any(!is.na(facet_status) & nzchar(facet_status))
    aligned_n <- if (alignment_available) sum(f_status == "aligned", na.rm = TRUE) else NA_integer_
    ambiguous_n <- if (alignment_available) sum(f_status == "ambiguous", na.rm = TRUE) else NA_integer_
    mismatch_n <- if (alignment_available) sum(f_status == "clear_mismatch", na.rm = TRUE) else NA_integer_
    facet_ambiguous_n <- if (facet_alignment_available) sum(facet_status == "ambiguous", na.rm = TRUE) else NA_integer_
    facet_mismatch_n <- if (facet_alignment_available) sum(facet_status == "clear_mismatch", na.rm = TRUE) else NA_integer_
    guard_sensitivity <- if ("semantica_content_guard_sensitivity" %in% names(df)) {
      as.character(df$semantica_content_guard_sensitivity[ii])
    } else rep(NA_character_, length(ii))
    raw_exclusion_retained_n <- sum(
      guard_sensitivity == "raw_exclusion_not_robust_to_centering_retained", na.rm = TRUE
    )
    robust_exclusion_n <- sum(
      guard_sensitivity == "raw_exclusion_confirmed_by_centering", na.rm = TRUE
    )
    f_reason <- reason_table[ii, , drop = FALSE]
    robust_factor_n <- sum(f_reason$robust_factor_mismatch, na.rm = TRUE)
    robust_forbidden_n <- sum(f_reason$robust_forbidden_conflict, na.rm = TRUE)
    robust_both_n <- sum(f_reason$robust_both, na.rm = TRUE)
    requested <- as.integer(i.per.f[[f]])
    dup_feasible <- if (nrow(dup_row)) isTRUE(dup_row$feasible[[1L]]) else TRUE
    status <- if (!dup_feasible) {
      "duplicate_constraint_infeasible"
    } else if (!alignment_available) {
      "alignment_diagnostic_unavailable"
    } else if (aligned_n < requested) {
      "content_constrained"
    } else if (mismatch_n > 0L || robust_exclusion_n > 0L ||
               (facet_alignment_available && facet_mismatch_n > 0L)) {
      "content_mixed"
    } else if (ambiguous_n > 0L ||
               (facet_alignment_available && facet_ambiguous_n > 0L)) {
      "adequate_capacity_with_ambiguity"
    } else {
      "adequate_capacity"
    }
    data.frame(
      factor = f,
      generated = as.integer(generated_counts[[f]]),
      after_quality_guards = as.integer(guarded_counts[[f]]),
      cohesion_eligible = length(eligible.items[[f]]),
      selected_target = requested,
      factor_aligned = aligned_n,
      factor_ambiguous = ambiguous_n,
      factor_clear_mismatch = mismatch_n,
      raw_exclusion_retained_by_sensitivity = raw_exclusion_retained_n,
      robust_content_exclusions = robust_exclusion_n,
      robust_factor_mismatch_exclusions = robust_factor_n,
      robust_forbidden_conflict_exclusions = robust_forbidden_n,
      robust_both_exclusions = robust_both_n,
      facet_ambiguous = facet_ambiguous_n,
      facet_clear_mismatch = facet_mismatch_n,
      duplicate_independent_units = if (nrow(dup_row)) dup_row$independent_duplicate_units[[1L]] else length(list.items[[f]]),
      duplicate_constraint_feasible = dup_feasible,
      guard_retention = if (generated_counts[[f]] > 0L) guarded_counts[[f]] / generated_counts[[f]] else NA_real_,
      selection_pressure_after_guard = if (guarded_counts[[f]] > 0L) requested / guarded_counts[[f]] else NA_real_,
      selection_pressure_eligible = if (length(eligible.items[[f]]) > 0L) requested / length(eligible.items[[f]]) else NA_real_,
      operational_status = status,
      stringsAsFactors = FALSE
    )
  }))
  rownames(pool_health) <- NULL
  if (verbose) {
    cat("  Pool health (operational, not validity):\n")
    for (rr in seq_len(nrow(pool_health))) {
      z <- pool_health[rr, , drop = FALSE]
      cat(sprintf(
        "    %-26s: %s | aligned %d, ambiguous %d, mismatch %d | independent duplicate units %d/%d required\n",
        z$factor, z$operational_status, z$factor_aligned, z$factor_ambiguous,
        z$factor_clear_mismatch, z$duplicate_independent_units, z$selected_target
      ))
      cat(sprintf(
        "      guard retention %.1f%% | select %.1f%% of post-guard pool (%.1f%% of semantic-eligible pool)\n",
        100 * z$guard_retention,
        100 * z$selection_pressure_after_guard,
        100 * z$selection_pressure_eligible
      ))
      if (z$raw_exclusion_retained_by_sensitivity > 0L || z$robust_content_exclusions > 0L) {
        cat(sprintf(
          paste0(
            "      alignment sensitivity: %d raw exclusion(s) retained after preprocessing disagreement; ",
            "%d robust exclusion(s) confirmed [factor mismatch=%d, forbidden conflict=%d, both=%d]\n"
          ),
          z$raw_exclusion_retained_by_sensitivity, z$robust_content_exclusions,
          z$robust_factor_mismatch_exclusions, z$robust_forbidden_conflict_exclusions,
          z$robust_both_exclusions
        ))
      }
    }
  }

  solution_cache <- new.env(hash = TRUE, parent = emptyenv())
  solution_history_env <- if (history_mode != "none") {
    e <- new.env(hash = FALSE, parent = emptyenv())
    e$n <- 0L
    e$history <- list()
    e
  } else NULL
  pheromone <- matrix(1.0, nrow = n_items, ncol = 2L, dimnames = list(item.vector, c("not_selected", "selected")))

  heuristic_cutoffs <- compute_heuristic_cutoffs(n_factors, i.per.f, esem_sample_size)
  dfi_enabled <- !identical(dfi_mode, "heuristic_semantic")
  dfi_bootstrap_requested <- run_esem_during_search && dfi_enabled
  if (verbose) {
    phase1_label <- if (dfi_bootstrap_requested) {
      "DFI CUTOFFS (ESEM-FITTED, TWO-PASS)"
    } else if (run_esem_during_search) {
      "SEMANTIC WARM-UP (DFI DISABLED; ESEM SEARCH ENABLED)"
    } else {
      "SEMANTIC-ONLY WARM-UP (ESEM DISABLED DURING SEARCH)"
    }
    cat(sprintf("\nPHASE 1 -- %s\nWarm-up: %d iters | Rotation: %s\n", phase1_label, dfi_warmup_iters, rotation))
  }

  model_info_warmup <- list(
    model_type = model_type, n_obs = esem_sample_size,
    estimator = switch(data_type, "categorical" = "WLSMV", "nonnormal" = "MLR", "ML"),
    data_type = data_type, esem_weight = esem_weight, dfi_mode = dfi_mode,
    active_cutoffs = heuristic_cutoffs, cutoff_source = "Heuristic (warm-up)",
    redundancy_threshold = redundancy_threshold, dup_threshold = dup_threshold,
    htmt_threshold = htmt_threshold, htmt_objective_role = htmt_objective_role, within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band,
    semantic_objective_mode = semantic_objective_mode,
    within_target_method = attr(within_similarity_target_eff, "method") %||% within_target_method,
    within_target_source = attr(within_similarity_target_eff, "source"),
    expected_factor_relations = expected_factor_relations,
    nomological_weight = nomological_weight, content_alignment_mode = content_alignment_mode,
    polarity_action = polarity_action,
    facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode, pfa_weight = pfa_weight,
    pfa_failure_policy = pfa_failure_policy,
    run_pfa_during_search = run_pfa_during_search, pfa_every = pfa_every,
    pfa_extraction = pfa_extraction,
    pfa_final_extraction = pfa_final_extraction, pfa_rotation = pfa_rotation,
    pfa_min_loading = pfa_min_loading, pfa_min_margin = pfa_min_margin,
    semantic_esem_score_mode = semantic_esem_score_mode,
    cohesion_retention = cohesion_retention,
    esem_failure_policy = esem_failure_policy,
    reference_n = reference_n_info,
    semantic_n_sensitivity = semantic_n_sensitivity,
    semantic_n_grid = semantic_n_grid,
    semantic_n_multipliers = semantic_n_multipliers,
    semantic_n_iter_max = semantic_n_iter_max,
    validation_n_diagnostic = validation_n_diagnostic,
    validation_n_reps = validation_n_reps,
    validation_n_grid = validation_n_grid,
    validation_n_max = validation_n_max,
    validation_n_convergence = validation_n_convergence,
    validation_n_max_heywood = validation_n_max_heywood,
    validation_n_min_recovery = validation_n_min_recovery,
    validation_n_max_loading_error = validation_n_max_loading_error,
    validation_n_min_dominance = validation_n_min_dominance,
    validation_n_max_cross_error = validation_n_max_cross_error,
    validation_n_max_factor_cor_error = validation_n_max_factor_cor_error,
    sigmoid_center = sigmoid_center, sigmoid_steepness = sigmoid_steepness,
    rotation = rotation, rotation_args = rotation_args, fast_esem = fast_esem,
    fast_esem_iter_max = fast_esem_iter_max, full_esem_iter_max = full_esem_iter_max,
    max_total_iter = max_total_iter, max_esem_fits = max_esem_fits,
    history_mode = history_mode
  )
  model_info <- model_info_warmup
  warmup_best_obj <- -Inf; warmup_best_vector <- NULL

  for (wu_iter in seq_len(max(1L, as.integer(dfi_warmup_iters)))) {
    wu_solutions <- lapply(seq_len(ants), function(a) {
      vec <- integer(n_items); names(vec) <- item.vector
      used_dup_clusters <- character(0)
      for (f_idx in seq_along(factors)) {
        f_name <- factors[f_idx]; f_items <- eligible.items[[f_name]]; n_pick <- i.per.f[f_idx]
        if (length(f_items) < n_pick) f_items <- list.items[[f_name]]
        tau <- pheromone[f_items, "selected"]; eta <- item_heuristics[[f_name]][f_items]
        if (is.null(eta) || length(eta) != length(f_items)) eta <- rep(1.0, length(f_items))
        probs <- pmax(tau * (eta ^ heuristic_beta), 1e-9); probs <- probs / sum(probs)
        picked <- sample_items_with_duplicate_guard(
          f_items, n_pick, probs,
          duplicate_cluster_id = duplicate_cluster_id,
          used_clusters = used_dup_clusters
        )
        used_dup_clusters <- picked$used_clusters
        vec[picked$items] <- 1L
      }
      vec
    })
    wu_objs <- vapply(wu_solutions, function(v) tryCatch(fit.function.v2(
      v, run_esem_now = FALSE, effective_esem_weight = 0,
      run_pfa_now = run_pfa_during_search && (wu_iter %% pfa_every == 0L),
      solution_cache = solution_cache,
      solution_history_env = if (history_mode == "full") solution_history_env else NULL
    ), error = function(e) NA_real_), numeric(1L))
    wu_objs[is.na(wu_objs)] <- -1e6
    best_wu_idx <- which.max(wu_objs)
    if (wu_objs[best_wu_idx] > warmup_best_obj) { warmup_best_obj <- wu_objs[best_wu_idx]; warmup_best_vector <- wu_solutions[[best_wu_idx]] }
    pheromone <- pheromone * (1 - 0.30)
    top_k_wu <- max(1L, floor(ants * 0.20))
    for (k in order(wu_objs, decreasing = TRUE)[seq_len(top_k_wu)]) {
      v <- wu_solutions[[k]]
      idx <- cbind(seq_along(v), ifelse(v == 1L, 2L, 1L))
      pheromone[idx] <- pheromone[idx] + 1.0
    }
    pheromone[] <- pmax(pmin(pheromone, 50.0), 0.01)
    if (verbose) cat(sprintf("  Warm-up %2d/%2d | best semantic obj: %.4f\n", wu_iter, dfi_warmup_iters, warmup_best_obj))
  }

  bootstrap_params <- NULL
  pfa_dfi_params <- NULL
  dfi_population_params <- NULL
  dfi_loading_source <- if (dfi_bootstrap_requested) {
    "prior-based"
  } else {
    "not-used"
  }
  warmup_esem_items <- NULL
  warmup_esem_fa <- NULL
  if (dfi_bootstrap_requested && !is.null(warmup_best_vector)) {
    wu_items <- item.vector[warmup_best_vector == 1L]
    wu_fa <- item.factor.lookup[wu_items]
    warmup_esem_items <- wu_items
    warmup_esem_fa <- wu_fa
    if (verbose) cat(sprintf("\n  Step 1B -- bootstrap ESEM on %d items\n", length(wu_items)))
    bootstrap_params <- extract_fitted_dfi_params_esem(wu_items, wu_fa, factors, cosine_sim_matrix, esem_sample_size, model_info_warmup$estimator, rotation, rotation_args, 0.97, 0.90, verbose)
    if (!is.null(bootstrap_params)) {
      dfi_population_params <- bootstrap_params
      dfi_loading_source <- "ESEM-fitted"
    } else {
      pfa_warmup <- tryCatch(
        compute_pfa_diagnostics(
          extract_similarity_submatrix(cosine_sim_matrix, wu_items),
          wu_fa, factors,
          extraction = pfa_extraction,
          rotation = pfa_rotation,
          min_loading = pfa_min_loading,
          min_margin = pfa_min_margin
        ),
        error = function(e) NULL
      )
      pfa_dfi_params <- pfa_diagnostics_to_dfi_params(
        pfa_warmup, wu_fa, factors,
        min_loading = 0.35,
        max_loading = 0.95,
        max_fcor = 0.90
      )
      if (!is.null(pfa_dfi_params)) {
        dfi_population_params <- pfa_dfi_params
        dfi_loading_source <- "PFA-informed"
        if (verbose) message("  [DFI] Bootstrap ESEM failed -- using PFA-informed loadings/factor correlations for fallback DFI.")
      } else if (verbose) {
        message("  [DFI] Bootstrap ESEM failed -- PFA fallback unavailable; using prior-based loadings if DFI fallback is needed.")
      }
    }
  }

  strict_dfi_cutoffs <- NULL
  semantic_roc_cutoffs <- NULL
  semantic_approx_cutoffs <- NULL
  esem_parametric_cutoffs <- NULL
  dfi_cutoffs <- NULL
  dfi_calibration_cache <- .semantica_new_dfi_cache()
  dfi_n_cores <- function() {
    if (use_parallel && n.cores > 1L) as.integer(n.cores) else 1L
  }
  search_dfi_cl <- NULL
  get_search_dfi_cluster <- function() {
    if (is.null(search_dfi_cl) && dfi_n_cores() > 1L) {
      search_dfi_cl <<- .semantica_make_dfi_cluster(dfi_n_cores())
    }
    search_dfi_cl
  }
  on.exit({
    if (!is.null(search_dfi_cl)) .semantica_stop_cluster(search_dfi_cl)
  }, add = TRUE)

  if (run_esem_during_search && dfi_mode %in% c("auto", "semantic_roc_dfi") && !is.null(bootstrap_params$esem_fit)) {
    warmup_syntax <- if (!is.null(bootstrap_params$esem_syntax)) {
      bootstrap_params$esem_syntax
    } else {
      build_esem_syntax_safe(warmup_esem_items, warmup_esem_fa, factors)
    }
    warmup_observed_cor <- bootstrap_params$esem_cor
    warmup_rotation_args <- bootstrap_params$rotation_args %||%
      prepare_esem_rotation_args(rotation, rotation_args, warmup_esem_items, warmup_esem_fa, factors)
    semantic_roc_cutoffs <- compute_semantic_roc_dfi_cutoffs(
      esem_fit = bootstrap_params$esem_fit,
      esem_syntax = warmup_syntax,
      factors = factors,
      items_per_factor = i.per.f,
      observed_cor = warmup_observed_cor,
      factor_assignment = warmup_esem_fa,
      n_obs = esem_sample_size,
      estimator = model_info_warmup$estimator,
      rotation = rotation,
      rotation_args = warmup_rotation_args,
      reps = dfi_esem_reps,
      level = dfi_level,
      criterion = dfi_criterion,
      n_cores = dfi_n_cores(),
      iter_max = min(full_esem_iter_max, 1200L),
      embed_reliability = embed_reliability,
      misspec_strength = dfi_roc_misspec_strength,
      verbose = verbose,
      progress = verbose,
      cache = dfi_calibration_cache,
      cluster = get_search_dfi_cluster,
      adaptive = identical(dfi_esem_strategy, "adaptive"),
      adaptive_min_reps = dfi_adaptive_min_reps,
      adaptive_batch_reps = dfi_adaptive_batch_reps,
      adaptive_tol = dfi_adaptive_tol,
      adaptive_stable_batches = dfi_adaptive_stable_batches
    )
  }

  if (
    run_esem_during_search &&
    dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi") &&
    !is.null(bootstrap_params$esem_fit) &&
    (is.null(semantic_roc_cutoffs) || isTRUE(semantic_roc_cutoffs$was_degenerate)) &&
    (identical(dfi_fallback_policy, "conservative") ||
       dfi_mode %in% c("auto", "semantic_approx_dfi"))
  ) {
    warmup_syntax <- if (!is.null(bootstrap_params$esem_syntax)) {
      bootstrap_params$esem_syntax
    } else {
      build_esem_syntax_safe(warmup_esem_items, warmup_esem_fa, factors)
    }
    warmup_observed_cor <- bootstrap_params$esem_cor
    warmup_rotation_args <- bootstrap_params$rotation_args %||%
      prepare_esem_rotation_args(rotation, rotation_args, warmup_esem_items, warmup_esem_fa, factors)
    semantic_approx_cutoffs <- compute_semantic_approx_dfi_cutoffs(
      esem_fit = bootstrap_params$esem_fit,
      esem_syntax = warmup_syntax,
      factors = factors,
      items_per_factor = i.per.f,
      observed_cor = warmup_observed_cor,
      n_obs = esem_sample_size,
      estimator = model_info_warmup$estimator,
      rotation = rotation,
      rotation_args = warmup_rotation_args,
      reps = dfi_esem_reps,
      level = dfi_level,
      criterion = dfi_criterion,
      n_cores = dfi_n_cores(),
      iter_max = min(full_esem_iter_max, 1200L),
      embed_reliability = embed_reliability,
      verbose = verbose,
      progress = verbose,
      cache = dfi_calibration_cache,
      cluster = get_search_dfi_cluster,
      adaptive = identical(dfi_esem_strategy, "adaptive"),
      adaptive_min_reps = dfi_adaptive_min_reps,
      adaptive_batch_reps = dfi_adaptive_batch_reps,
      adaptive_tol = dfi_adaptive_tol,
      adaptive_stable_batches = dfi_adaptive_stable_batches
    )
  }

  need_exact_esem_dfi <- run_esem_during_search &&
    dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi") &&
    !is.null(bootstrap_params$esem_fit) &&
    (
      dfi_mode == "esem_parametric_dfi" ||
        (
          (identical(dfi_fallback_policy, "conservative") || dfi_mode == "auto") &&
          (is.null(semantic_roc_cutoffs) || isTRUE(semantic_roc_cutoffs$was_degenerate)) &&
            (is.null(semantic_approx_cutoffs) || isTRUE(semantic_approx_cutoffs$was_degenerate))
        )
    )
  if (need_exact_esem_dfi) {
    warmup_syntax <- if (!is.null(bootstrap_params$esem_syntax)) {
      bootstrap_params$esem_syntax
    } else {
      build_esem_syntax_safe(warmup_esem_items, warmup_esem_fa, factors)
    }
    warmup_rotation_args <- bootstrap_params$rotation_args %||%
      prepare_esem_rotation_args(rotation, rotation_args, warmup_esem_items, warmup_esem_fa, factors)
    esem_parametric_cutoffs <- compute_esem_parametric_dfi_cutoffs(
      esem_fit = bootstrap_params$esem_fit,
      esem_syntax = warmup_syntax,
      factors = factors,
      items_per_factor = i.per.f,
      n_obs = esem_sample_size,
      estimator = model_info_warmup$estimator,
      rotation = rotation,
      rotation_args = warmup_rotation_args,
      reps = dfi_esem_reps,
      level = dfi_level,
      criterion = dfi_criterion,
      n_cores = dfi_n_cores(),
      iter_max = min(full_esem_iter_max, 1200L),
      verbose = verbose,
      progress = verbose,
      cache = dfi_calibration_cache,
      cluster = get_search_dfi_cluster,
      adaptive = identical(dfi_esem_strategy, "adaptive"),
      adaptive_min_reps = dfi_adaptive_min_reps,
      adaptive_batch_reps = dfi_adaptive_batch_reps,
      adaptive_tol = dfi_adaptive_tol,
      adaptive_stable_batches = dfi_adaptive_stable_batches
    )
  }
  if (!is.null(search_dfi_cl)) {
    .semantica_stop_cluster(search_dfi_cl)
    search_dfi_cl <- NULL
  }

  if (run_esem_during_search && dfi_mode == "strict_cfa_dfi") {
    strict_dfi_cutoffs <- compute_dfi_cutoffs_from_model_spec(
      factors, i.per.f, esem_sample_size,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_loadings else NULL,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_factor_cors else NULL,
      loading_pattern, target_loadings, target_factor_cors, embed_reliability,
      residual_inflation, data_type, original_data, NULL, dfi_reps, dfi_level,
      dfi_criterion, verbose, dfi_loading_source,
      n_cores = dfi_n_cores()
    )
  }

  if (
    run_esem_during_search &&
    dfi_mode == "auto" &&
    (is.null(semantic_roc_cutoffs) || isTRUE(semantic_roc_cutoffs$was_degenerate)) &&
    (is.null(semantic_approx_cutoffs) || isTRUE(semantic_approx_cutoffs$was_degenerate)) &&
    is.null(esem_parametric_cutoffs)
  ) {
    strict_dfi_cutoffs <- compute_dfi_cutoffs_from_model_spec(
      factors, i.per.f, esem_sample_size,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_loadings else NULL,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_factor_cors else NULL,
      loading_pattern, target_loadings, target_factor_cors, embed_reliability,
      residual_inflation, data_type, original_data, NULL, dfi_reps, dfi_level,
      dfi_criterion, verbose, dfi_loading_source,
      n_cores = dfi_n_cores()
    )
  }

  if (
    run_esem_during_search &&
    !(dfi_mode %in% c("heuristic_semantic", "strict_cfa_dfi")) &&
    is.null(semantic_roc_cutoffs) &&
    is.null(semantic_approx_cutoffs) &&
    is.null(esem_parametric_cutoffs) &&
    is.null(strict_dfi_cutoffs) &&
    (identical(dfi_fallback_policy, "conservative") || dfi_mode == "auto")
  ) {
    strict_dfi_cutoffs <- compute_dfi_cutoffs_from_model_spec(
      factors, i.per.f, esem_sample_size,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_loadings else NULL,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_factor_cors else NULL,
      loading_pattern, target_loadings, target_factor_cors, embed_reliability,
      residual_inflation, data_type, original_data, NULL,
      max(20L, min(dfi_reps, dfi_esem_reps)),
      dfi_level, dfi_criterion, verbose, dfi_loading_source,
      n_cores = dfi_n_cores()
    )
    if (!is.null(strict_dfi_cutoffs)) {
      strict_dfi_cutoffs$cutoff_calibration <- paste("strict-CFA fallback", dfi_loading_source)
    }
  }

  for (candidate in list(semantic_roc_cutoffs, semantic_approx_cutoffs, esem_parametric_cutoffs, strict_dfi_cutoffs)) {
    if (is.null(dfi_cutoffs) && !is.null(candidate) && !isTRUE(candidate$was_degenerate)) {
      dfi_cutoffs <- candidate
    }
  }
  if (is.null(dfi_cutoffs)) {
    for (candidate in list(semantic_roc_cutoffs, semantic_approx_cutoffs, esem_parametric_cutoffs, strict_dfi_cutoffs)) {
      if (!is.null(candidate)) {
        dfi_cutoffs <- candidate
        break
      }
    }
  }
  active_cutoffs <- if (!is.null(dfi_cutoffs) && !isTRUE(dfi_cutoffs$was_degenerate) && dfi_mode != "heuristic_semantic") dfi_cutoffs else heuristic_cutoffs
  cutoff_source <- if (!run_esem_during_search) {
    "Heuristic (semantic-only ACO search)"
  } else if (dfi_mode == "heuristic_semantic") {
    "Heuristic (DFI disabled by dfi_mode)"
  } else if (!is.null(semantic_roc_cutoffs) && identical(active_cutoffs, semantic_roc_cutoffs)) {
    sprintf("DFI (ESEM semantic-ROC proxy, %s)", semantic_roc_cutoffs$dfi_function)
  } else if (!is.null(semantic_approx_cutoffs) && identical(active_cutoffs, semantic_approx_cutoffs)) {
    sprintf("DFI (ESEM semantic-approximate proxy, %s)", semantic_approx_cutoffs$dfi_function)
  } else if (!is.null(esem_parametric_cutoffs) && identical(active_cutoffs, esem_parametric_cutoffs)) {
    sprintf("DFI (ESEM-parametric semantic proxy, %s)", esem_parametric_cutoffs$dfi_function)
  } else if (!is.null(strict_dfi_cutoffs) && identical(active_cutoffs, strict_dfi_cutoffs)) {
    sprintf("DFI (strict CFA-style, %s loadings, %s)", dfi_loading_source, strict_dfi_cutoffs$dfi_function)
  } else {
    "Heuristic (DFI failed)"
  }
  search_active_cutoffs <- active_cutoffs
  search_cutoff_source <- cutoff_source

  model_info <- list(
    model_type = model_type, n_obs = esem_sample_size,
    estimator = switch(data_type, "categorical" = "WLSMV", "nonnormal" = "MLR", "ML"),
    data_type = data_type, esem_weight = esem_weight, dfi_mode = dfi_mode,
    dfi_loading_source = dfi_loading_source,
    dfi_search_reps = dfi_esem_reps,
    final_dfi_recalibrate = final_dfi_recalibrate,
    final_dfi_reps = final_dfi_reps,
    dfi_roc_misspec_strength = dfi_roc_misspec_strength,
    dfi_esem_strategy = dfi_esem_strategy,
    dfi_adaptive_min_reps = dfi_adaptive_min_reps,
    dfi_adaptive_batch_reps = dfi_adaptive_batch_reps,
    dfi_adaptive_tol = dfi_adaptive_tol,
    dfi_adaptive_stable_batches = dfi_adaptive_stable_batches,
    dfi_fallback_policy = dfi_fallback_policy,
    active_cutoffs = active_cutoffs, cutoff_source = cutoff_source,
    redundancy_threshold = redundancy_threshold, dup_threshold = dup_threshold,
    htmt_threshold = htmt_threshold, htmt_objective_role = htmt_objective_role, within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band,
    semantic_objective_mode = semantic_objective_mode,
    within_target_method = attr(within_similarity_target_eff, "method") %||% within_target_method,
    within_target_source = attr(within_similarity_target_eff, "source"),
    expected_factor_relations = expected_factor_relations,
    nomological_weight = nomological_weight, content_alignment_mode = content_alignment_mode,
    polarity_action = polarity_action,
    facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode, pfa_weight = pfa_weight,
    pfa_failure_policy = pfa_failure_policy,
    run_pfa_during_search = run_pfa_during_search, pfa_every = pfa_every,
    pfa_extraction = pfa_extraction,
    pfa_final_extraction = pfa_final_extraction, pfa_rotation = pfa_rotation,
    pfa_min_loading = pfa_min_loading, pfa_min_margin = pfa_min_margin,
    semantic_esem_score_mode = semantic_esem_score_mode,
    cohesion_retention = cohesion_retention,
    esem_failure_policy = esem_failure_policy,
    reference_n = reference_n_info,
    validation_n_diagnostic = validation_n_diagnostic,
    validation_n_reps = validation_n_reps,
    validation_n_grid = validation_n_grid,
    validation_n_max = validation_n_max,
    validation_n_convergence = validation_n_convergence,
    validation_n_max_heywood = validation_n_max_heywood,
    validation_n_min_recovery = validation_n_min_recovery,
    validation_n_max_loading_error = validation_n_max_loading_error,
    sigmoid_center = sigmoid_center, sigmoid_steepness = sigmoid_steepness,
    rotation = rotation, rotation_args = rotation_args, fast_esem = fast_esem,
    fast_esem_iter_max = fast_esem_iter_max, full_esem_iter_max = full_esem_iter_max,
    max_total_iter = max_total_iter, max_esem_fits = max_esem_fits,
    history_mode = history_mode
  )

  if (verbose) {
    search_label <- if (run_esem_during_search) {
      "full-ESEM"
    } else if (run_pfa_during_search) {
      "semantic+PFA"
    } else {
      "semantic-only"
    }
    cat(sprintf("\nPHASE 2 -- ACO OPTIMIZATION (%s)\n  Cutoffs: %s | CFI >=%.3f RMSEA <=%.3f SRMR <=%.3f\n", search_label, cutoff_source, active_cutoffs$cfi, active_cutoffs$rmsea, active_cutoffs$srmr))
    cat(sprintf("  Ants: %d | Max patience: %d | ESEM search: %s | ESEM weight: %.0f%% | ESEM ants/iter: %d | Cores: %d\n", ants, search_patience, if (run_esem_during_search) "on" else "off", esem_weight * 100, esem_eval_top_k_eff, if (use_parallel && n.cores > 1L) n.cores else 1L))
    if (run_esem_during_search) {
      if (identical(esem_cadence_mode, "fixed")) {
        cat(sprintf("  ESEM cadence: fixed | every %d iteration(s)\n", esem_every))
      } else {
        cat(sprintf("  ESEM cadence: adaptive | base interval %d iteration(s), entropy-adjusted during search\n", esem_every))
      }
    }
    if (run_pfa_during_search) {
      cat(sprintf("  PFA search: on | PFA weight: %.0f%% | PFA every: %d iteration(s)\n",
                  pfa_weight * 100, pfa_every))
    } else {
      cat(sprintf("  PFA search: off | PFA weight: %.0f%% | PFA cadence: not applicable\n",
                  pfa_weight * 100))
    }
    cat(sprintf("  Resource budget : total iterations <= %s | search ESEM candidate jobs <= %s | history=%s\n",
                if (is.infinite(max_total_iter)) "Inf" else as.character(max_total_iter),
                if (is.infinite(max_esem_fits)) "Inf" else as.character(max_esem_fits),
                history_mode))
    cat(sprintf("  Within-targets : %s | band=%.3f | facet weight=%.2f | guard weight=%.2f\n", paste(names(within_similarity_target_eff), sprintf("%.3f", within_similarity_target_eff), sep = "=", collapse = ", "), within_similarity_band, facet_coverage_weight, psychometric_guard_weight))
  }

  cl <- NULL
  stop_search_cluster <- function() {
    if (!is.null(cl)) {
      .semantica_stop_cluster(cl)
      cl <<- NULL
    }
    invisible(NULL)
  }
  on.exit(stop_search_cluster(), add = TRUE)
  if (use_parallel && n.cores > 1L) {
    cl <- .semantica_make_cluster(resource_plan)
    parallel::clusterEvalQ(cl, { suppressPackageStartupMessages({ library(lavaan); library(Matrix) }) })
    export_env <- new.env(parent = emptyenv())
    export_env$cosine_sim_matrix <- cosine_sim_matrix[item.vector, item.vector, drop = FALSE]
    export_env$list.items <- list.items; export_env$eligible.items <- eligible.items
    export_env$factors <- factors; export_env$i.per.f <- i.per.f; export_env$item.vector <- item.vector; export_env$model_info <- model_info; export_env$item.factor.lookup <- item.factor.lookup; export_env$item.facet.lookup <- item.facet.lookup; export_env$facets.by.factor <- facets.by.factor
    fns <- c(
      "%||%", ".semantica_fast_lavaan_se",
      "fit.function.v2", ".semantica_evaluate_esem_worker",
      ".semantica_with_task_seed", "build_esem_syntax_safe",
      "build_esem_target_matrix", "prepare_esem_rotation_args",
      "sanitize_lavaan_name", "extract_similarity_submatrix",
      "compute_semantic_sim_index_v2", "compute_manual_srmr",
      "transform_cosine_for_esem", "run_esem_on_matrix",
      ".semantica_attach_esem_rejection", "extract_and_score_esem",
      "compute_ave_esem", "compute_htmt_esem",
      "compute_esem_structure_diagnostics", "compute_duplicate_penalty",
      "compute_facet_coverage_multiplier", "compute_psychometric_guard_penalty",
      "compute_pfa_diagnostics", "extract_pfa_loadings",
      "build_pfa_target_matrix", "apply_pfa_loading_rotation",
      "pfa_harmonic_mean", "efa_degrees_of_freedom",
      "check_near_duplicates", "fisherz", "fisherz_inv",
      "make_solution_key", "is_admissible_esem_fit",
      ".semantica_assess_esem_fit", "assess_esem_admissibility",
      ".semantica_safe_lav_inspect", ".semantica_collect_numeric",
      ".semantica_numeric_matrix", "extract_aligned_esem_solution",
      "align_esem_to_intended_structure", ".semantica_solve_factor_assignment",
      ".semantica_large_factor_assignment", ".semantica_lexicographically_less"
    )
    for (fn in fns) if (exists(fn, mode = "function")) export_env[[fn]] <- get(fn)
    .semantica_cluster_export_environment(cl, export_env)
  }

  requested_esem_search <- run_esem_during_search
  requested_pfa_search <- isTRUE(run_pfa_during_search) &&
    identical(pfa_mode, "objective") && pfa_weight > 0
  semantic_score_schema <- if (identical(semantic_objective_mode, "relative_conservative") && length(factors) > 1L) {
    "semantic-relative-v2"
  } else "semantic-v1"
  pfa_score_schema <- "pfa-proposal-v2"
  objective_schema <- list(
    version = "SEMANTICA-objective-v4",
    semantic = list(active = TRUE, score_schema = semantic_score_schema, mode = semantic_objective_mode),
    pfa = list(
      active = requested_pfa_search, mode = pfa_mode, weight = pfa_weight,
      every = pfa_every, failure_policy = pfa_failure_policy,
      score_schema = if (requested_pfa_search) pfa_score_schema else NA_character_
    ),
    esem = list(
      active = requested_esem_search, weight = esem_weight, every = esem_every,
      failure_policy = esem_failure_policy,
      score_schema = if (requested_esem_search) "esem-guided-v1" else NA_character_
    ),
    psychometric_guard = list(weight = psychometric_guard_weight),
    evidence_grouping = list(
      semantic_content = list(
        source_family = "embedding_semantic",
        components = "semantic"
      ),
      proxy_structure = list(
        source_family = "embedding_semantic",
        components = c(if (requested_pfa_search) "pfa" else NULL, if (requested_esem_search) "esem" else NULL),
        dependency = "shared_embedding_representation"
      ),
      independence_upgrade = list(
        status = "not_established_by_same_embedding_representation",
        requires = "held_out_empirical_calibration"
      )
    ),
    weight_policy = .semantica_decision_policy()$policy_origin,
    htmt_objective_role = htmt_objective_role,
    finalist_policy = "evidence_stratified_then_canonical_rerank",
    cross_schema_raw_score_comparison = FALSE
  )
  search_guidance_status <- if (requested_esem_search) {
    "esem_guided"
  } else if (requested_pfa_search) {
    "pfa_guided"
  } else {
    "semantic_only_requested"
  }
  esem_error_log <- character(0)
  esem_successes <- 0L
  esem_checkpoint_successes <- 0L
  esem_checkpoint_failures <- 0L
  esem_had_temporary_fallback <- FALSE
  evaluation_broker <- .semantica_new_evaluation_broker(max_esem_fits)
  esem_task_seed_records <- list()
  candidate_evaluations <- 0L
  esem_search_seconds <- 0
  best_stage_obj <- list(semantic = -Inf, pfa_guided = -Inf, esem_guided = -Inf)
  best_vector <- NULL
  patience <- 0L; iteration <- 0L; run_counter <- 0L; esem_attempts <- 0L; esem_failures <- 0L
  pfa_search_iterations <- 0L; pfa_search_attempts <- 0L; pfa_search_successes <- 0L
  pfa_checkpoint_successes <- 0L
  recent_tops <- list(semantic = numeric(0), pfa_guided = numeric(0), esem_guided = numeric(0))
  stagnation_window <- 10L
  elite_archives <- list(semantic = list(), pfa = list(), esem = list())
  archive_states <- list(
    semantic = .semantica_new_archive_state(),
    pfa = .semantica_new_archive_state(),
    esem = .semantica_new_archive_state()
  )
  termination_reason <- "patience_exhausted"
  search_started <- proc.time()[["elapsed"]]

  while (patience < search_patience && iteration < max_total_iter) {
    if (run_esem_during_search && is.finite(max_esem_fits) &&
        evaluation_broker$esem_fits_started >= max_esem_fits &&
        sum(vapply(elite_archives, length, integer(1L))) > 0L) {
      termination_reason <- "max_esem_fits_reached"
      break
    }
    iteration <- iteration + 1L
    ph_entropy <- compute_pheromone_entropy(pheromone)
    esem_interval <- .semantica_resolve_esem_interval(
      esem_every = esem_every, pheromone_entropy = ph_entropy, mode = esem_cadence_mode
    )
    do_esem <- run_esem_during_search && (iteration %% esem_interval == 0L)
    do_pfa <- requested_pfa_search && (iteration %% pfa_every == 0L)
    # Elite entries must be comparable across checkpoints; apply the declared
    # ESEM weight whenever a solution is scored by ESEM.
    effective_esem_weight <- if (!do_esem) 0 else esem_weight
    rho <- .semantica_evaporation_rho(evaporation_resolved, iteration)

    ant_solutions <- lapply(seq_len(ants), function(a) {
      vec <- integer(n_items); names(vec) <- item.vector
      used_dup_clusters <- character(0)
      for (f_idx in seq_along(factors)) {
        f_name <- factors[f_idx]; f_items <- eligible.items[[f_name]]; n_pick <- i.per.f[f_idx]
        if (length(f_items) < n_pick) f_items <- list.items[[f_name]]
        tau <- pheromone[f_items, "selected"]; eta <- item_heuristics[[f_name]][f_items]
        if (is.null(eta) || length(eta) != length(f_items)) eta <- rep(1.0, length(f_items))
        probs <- tau * (eta ^ heuristic_beta); probs <- pmax(probs, 1e-9); probs <- probs / sum(probs)
        picked <- sample_items_with_duplicate_guard(
          f_items, n_pick, probs,
          duplicate_cluster_id = duplicate_cluster_id,
          used_clusters = used_dup_clusters
        )
        used_dup_clusters <- picked$used_clusters
        vec[picked$items] <- 1L
      }
      vec
    })

    eval_sem_fn <- function(v) tryCatch(
      fit.function.v2(v, run_esem_now = FALSE, effective_esem_weight = 0,
                      run_pfa_now = do_pfa,
                      solution_cache = solution_cache,
                      solution_history_env = if (history_mode == "full") solution_history_env else NULL),
      error = function(e) NA_real_
    )
    proposal_objectives <- vapply(ant_solutions, eval_sem_fn, numeric(1L))
    candidate_evaluations <- candidate_evaluations + length(ant_solutions)
    if (do_pfa && identical(pfa_failure_policy, "stop") &&
        !any(is.finite(proposal_objectives))) {
      stop(
        "Objective-mode PFA was unavailable for every candidate at a required PFA checkpoint.",
        call. = FALSE
      )
    }
    if (do_pfa) {
      pfa_search_iterations <- pfa_search_iterations + 1L
      pfa_search_attempts <- pfa_search_attempts + length(ant_solutions)
      pfa_search_successes <- pfa_search_successes + sum(vapply(ant_solutions, function(v) {
        entry <- cache_get(solution_cache, make_solution_key(v))
        isTRUE(entry$pfa_result$available)
      }, logical(1L)))
    }
    score_stage <- if (do_pfa) "pfa_guided" else "semantic"
    scored_solutions <- ant_solutions
    scored_objectives <- proposal_objectives

    # Evidence-stratified archives: stage-specific scores never compete directly.
    semantic_entries <- lapply(seq_along(ant_solutions), function(i) {
      entry <- cache_get(solution_cache, make_solution_key(ant_solutions[[i]])) %||% list()
      list(
        vec = ant_solutions[[i]],
        semantic_score = entry$sem_score %||% -Inf,
        proposal_score = entry$search_score %||% proposal_objectives[i],
        iteration = iteration,
        score_type = "semantic",
        score_schema = semantic_score_schema
      )
    })
    elite_archives$semantic <- update_elite_archive(
      elite_archives$semantic, semantic_entries, elite_k,
      rank_field = "semantic_score", score_schema = semantic_score_schema
    )
    archive_states$semantic <- .semantica_update_archive_state(
      archive_states$semantic, elite_archives$semantic
    )

    if (do_pfa) {
      pfa_entries <- Filter(Negate(is.null), lapply(seq_along(ant_solutions), function(i) {
        entry <- cache_get(solution_cache, make_solution_key(ant_solutions[[i]])) %||% list()
        if (!isTRUE(entry$pfa_result$available) || !is.finite(entry$search_score %||% NA_real_)) return(NULL)
        list(
          vec = ant_solutions[[i]],
          semantic_score = entry$sem_score %||% NA_real_,
          pfa_score = entry$pfa_score %||% NA_real_,
          proposal_score = entry$search_score,
          iteration = iteration,
          score_type = "pfa_guided",
          score_schema = pfa_score_schema
        )
      }))
      if (length(pfa_entries) > 0L) {
        pfa_checkpoint_successes <- pfa_checkpoint_successes + 1L
        elite_archives$pfa <- update_elite_archive(
          elite_archives$pfa, pfa_entries, elite_k,
          rank_field = "proposal_score", score_schema = pfa_score_schema
        )
        archive_states$pfa <- .semantica_update_archive_state(
          archive_states$pfa, elite_archives$pfa
        )
      }
    }

    esem_failed <- 0L
    if (do_esem) {
        checkpoint_started <- proc.time()[["elapsed"]]
        esem_candidates <- which(is.finite(proposal_objectives) & !is.na(proposal_objectives))
        if (length(esem_candidates) > 0L) {
          esem_candidates <- esem_candidates[order(proposal_objectives[esem_candidates], decreasing = TRUE)]
          esem_candidates <- esem_candidates[seq_len(min(length(esem_candidates), esem_eval_top_k_eff))]
          batch_plan <- .semantica_plan_esem_batch(
            evaluation_broker,
            candidate_indices = esem_candidates,
            vectors = ant_solutions,
            cache = solution_cache
          )
          evaluation_candidates <- batch_plan$indices
          if (length(evaluation_candidates) == 0L) {
            termination_reason <- "max_esem_fits_reached"
            break
          }
          task_seeds <- rep.int(NA_integer_, length(evaluation_candidates))
          if (any(batch_plan$jobs_started)) {
            task_seeds[batch_plan$jobs_started] <- sample.int(
              .Machine$integer.max,
              sum(batch_plan$jobs_started)
            )
            seeded_positions <- which(batch_plan$jobs_started)
            esem_task_seed_records[[length(esem_task_seed_records) + 1L]] <-
              data.frame(
                iteration = rep.int(iteration, length(seeded_positions)),
                candidate_key = batch_plan$keys[seeded_positions],
                seed = task_seeds[seeded_positions],
                stringsAsFactors = FALSE
              )
          }
        eval_esem_payload <- function(v, task_seed = NA_integer_) {
          started <- proc.time()[["elapsed"]]
          tryCatch({
            key <- make_solution_key(v)
            evaluate <- function() fit.function.v2(
                v, run_esem_now = TRUE, effective_esem_weight = effective_esem_weight,
                run_pfa_now = requested_pfa_search,
                solution_cache = solution_cache, solution_history_env = NULL
              )
            score <- if (is.finite(task_seed)) {
              .semantica_with_task_seed(task_seed, evaluate())
            } else {
              evaluate()
            }
            cache_entry <- cache_get(solution_cache, key)
            converged <- !is.null(cache_entry$fit_result) && isTRUE(cache_entry$fit_result$converged)
            admissible <- converged && isTRUE(cache_entry$fit_result$admissible)
            if (!is.finite(score) || !admissible) {
              return(list(score = NA_real_, key = key, cache_entry = cache_entry,
                          error = "ESEM model did not return an admissible scored solution.",
                          elapsed_seconds = proc.time()[["elapsed"]] - started))
            }
            list(score = score, key = key, cache_entry = cache_entry, error = NA_character_,
                 elapsed_seconds = proc.time()[["elapsed"]] - started)
          }, error = function(e) {
            list(score = NA_real_, key = make_solution_key(v), cache_entry = NULL,
                 error = conditionMessage(e), elapsed_seconds = proc.time()[["elapsed"]] - started)
          })
        }
        evaluation_payloads <- vector("list", length(evaluation_candidates))
        cached_positions <- which(batch_plan$cached)
        for (position in cached_positions) {
          evaluation_payloads[[position]] <- eval_esem_payload(
            ant_solutions[[evaluation_candidates[position]]]
          )
        }
        job_positions <- which(batch_plan$jobs_started)
        if (length(job_positions) > 0L && !is.null(cl)) {
          tasks <- lapply(job_positions, function(position) {
            list(
              vector = ant_solutions[[evaluation_candidates[position]]],
              effective_esem_weight = effective_esem_weight,
              run_pfa_now = requested_pfa_search,
              seed = task_seeds[position]
            )
          })
          evaluation_payloads[job_positions] <- parallel::parLapplyLB(
            cl, tasks, .semantica_evaluate_esem_worker
          )
        } else if (length(job_positions) > 0L) {
          for (position in job_positions) {
            evaluation_payloads[[position]] <- eval_esem_payload(
              ant_solutions[[evaluation_candidates[position]]],
              task_seed = task_seeds[position]
            )
          }
        }
        for (payload in evaluation_payloads) {
          if (!is.null(payload$key) && !is.null(payload$cache_entry)) {
            cache_set(solution_cache, payload$key, payload$cache_entry)
          }
        }
        .semantica_record_esem_payloads(
          evaluation_broker, evaluation_payloads, batch_plan$jobs_started,
          keys = batch_plan$keys, cached = batch_plan$cached,
          coalesced_requests = max(0L, length(batch_plan$request_keys) - length(batch_plan$keys)),
          stage = "search"
        )
        evaluation_now <- .semantica_evaluation_snapshot(evaluation_broker)
        esem_attempts <- evaluation_now$esem_fits_started
        esem_successes <- evaluation_now$esem_fits_admissible
        esem_failures <- evaluation_now$esem_fits_failed
        evaluation_ok <- vapply(
          evaluation_payloads, function(x) is.finite(x$score), logical(1L)
        )
        esem_failed <- sum(!evaluation_ok & batch_plan$jobs_started)

        # Fan each unique evaluation back to every original ant request. This
        # preserves the optimizer's duplicate-ant weighting while still doing
        # at most one expensive fit for each canonical candidate key.
        esem_candidates <- batch_plan$request_indices
        esem_payloads <- evaluation_payloads[batch_plan$request_to_evaluation]
        esem_vals <- vapply(esem_payloads, function(x) x$score, numeric(1L))
        errors_now <- vapply(
          evaluation_payloads,
          function(x) x$error %||% NA_character_,
          character(1L)
        )
        errors_now <- errors_now[!is.na(errors_now) & nzchar(errors_now)]
        if (length(errors_now) > 0L) esem_error_log <- unique(c(esem_error_log, errors_now))
        ok_esem <- is.finite(esem_vals)
        checkpoint_elapsed <- proc.time()[["elapsed"]] - checkpoint_started
        esem_search_seconds <- esem_search_seconds + checkpoint_elapsed
        if (verbose) {
          checkpoint_counts <- .semantica_esem_checkpoint_telemetry(batch_plan, ok_esem)
          cat(sprintf(
            paste0(
              "  ESEM checkpoint %d | requests=%d | unique=%d | workers=%d | ",
              "cache_hits=%d | coalesced=%d | new_fits=%d | admissible_requests=%d | elapsed=%.1fs\n"
            ),
            iteration, checkpoint_counts$requests, checkpoint_counts$unique_candidates,
            if (is.null(cl)) 1L else length(cl),
            checkpoint_counts$cache_hits, checkpoint_counts$coalesced_requests,
            checkpoint_counts$new_fits, checkpoint_counts$admissible_requests, checkpoint_elapsed
          ))
        }
        if (!is.null(solution_history_env) && history_mode == "full") {
          for (i in seq_along(esem_payloads)) {
            payload <- esem_payloads[[i]]
            entry <- payload$cache_entry %||% list()
            .semantica_history_append(solution_history_env, list(
              key = payload$key,
              sem_score = entry$sem_score %||% NA_real_,
              pfa_score = entry$pfa_score %||% NA_real_,
              search_score = entry$search_score %||% proposal_objectives[esem_candidates[i]],
              esem_score = if (ok_esem[i]) entry$esem_score %||% NA_real_ else NA_real_,
              guard_penalty = entry$guard_penalty %||% NA_real_,
              total = if (ok_esem[i]) payload$score else NA_real_,
              stage = "esem_guided",
              converged = ok_esem[i],
              error = payload$error
            ))
          }
        }
        if (any(ok_esem)) {
          esem_checkpoint_successes <- esem_checkpoint_successes + 1L
          score_stage <- "esem_guided"
          search_guidance_status <- if (isTRUE(esem_had_temporary_fallback)) {
            "esem_guided_with_checkpoint_fallbacks"
          } else {
            "esem_guided"
          }
          successful_ix <- esem_candidates[ok_esem]
          scored_solutions <- ant_solutions[successful_ix]
          scored_objectives <- esem_vals[ok_esem]
          esem_entries <- lapply(seq_along(successful_ix), function(i) {
            cache_entry <- cache_get(
              solution_cache, make_solution_key(ant_solutions[[successful_ix[i]]])
            ) %||% list()
            list(
              vec = ant_solutions[[successful_ix[i]]],
              semantic_score = cache_entry$sem_score %||% NA_real_,
              pfa_score = cache_entry$pfa_score %||% NA_real_,
              proposal_score = cache_entry$search_score %||% -Inf,
              esem_score = cache_entry$esem_score %||% NA_real_,
              guard_penalty = cache_entry$guard_penalty %||% NA_real_,
              esem_guided_score = scored_objectives[i],
              iteration = iteration,
              score_type = "esem_guided",
              score_schema = "esem-guided-v1"
            )
          })
          elite_archives$esem <- update_elite_archive(
            elite_archives$esem, esem_entries, elite_k,
            rank_field = "esem_guided_score", score_schema = "esem-guided-v1"
          )
          archive_states$esem <- .semantica_update_archive_state(
            archive_states$esem, elite_archives$esem
          )
        } else if (identical(esem_failure_policy, "stop")) {
          first_error <- if (length(esem_error_log) > 0L) esem_error_log[1L] else "no converged ESEM solution"
          stop(sprintf(
            "Search-time ESEM failed for all %d candidate solutions at iteration %d. First failure: %s. No ESEM-guided ACO result was produced.",
            length(esem_candidates), iteration, first_error
          ))
        } else {
          # `semantic_fallback` is checkpoint-local: score this iteration using
          # semantic/PFA evidence but keep ESEM enabled for later, genuinely new
          # candidates. A single failed checkpoint is not evidence that every
          # future candidate will be inadmissible.
          esem_checkpoint_failures <- esem_checkpoint_failures + 1L
          esem_had_temporary_fallback <- TRUE
          search_guidance_status <- if (requested_pfa_search) {
            "pfa_checkpoint_fallback"
          } else {
            "semantic_checkpoint_fallback"
          }
          score_stage <- if (do_pfa && requested_pfa_search) "pfa_guided" else "semantic"
          scored_solutions <- ant_solutions
          scored_objectives <- proposal_objectives
        }
      }
    }

    n_failed <- sum(!is.finite(scored_objectives))
    if (do_esem) esem_failures <- evaluation_broker$esem_fits_failed
    scored_objectives[!is.finite(scored_objectives)] <- -1e6

    best_ant_idx <- which.max(scored_objectives)
    best_ant_obj <- scored_objectives[best_ant_idx]
    best_ant_vec <- scored_solutions[[best_ant_idx]]
    if (!is.null(solution_history_env) && history_mode == "summary") {
      best_entry <- cache_get(solution_cache, make_solution_key(best_ant_vec)) %||% list()
      .semantica_history_append(solution_history_env, list(
        key = make_solution_key(best_ant_vec),
        sem_score = best_entry$sem_score %||% NA_real_,
        pfa_score = best_entry$pfa_score %||% NA_real_,
        search_score = best_entry$search_score %||% NA_real_,
        esem_score = best_entry$esem_score %||% NA_real_,
        guard_penalty = best_entry$guard_penalty %||% NA_real_,
        total = best_ant_obj,
        stage = paste0(score_stage, "_iteration"),
        converged = if (identical(score_stage, "esem_guided")) is.finite(best_entry$esem_score %||% NA_real_) else NA,
        iteration = iteration
      ))
    }
    run_counter <- run_counter + 1L
    benchmark <- best_stage_obj[[score_stage]] %||% -Inf
    improved <- best_ant_obj > benchmark
    if (improved) {
      best_stage_obj[[score_stage]] <- best_ant_obj
      if (identical(score_stage, "esem_guided") || !requested_esem_search) {
        best_vector <- best_ant_vec
      }
      if (verbose) {
        sel_items <- item.vector[best_ant_vec == 1L]
        cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, sel_items), error = function(e) NULL)
        sem_idx <- if (!is.null(cos_sub)) {
          fa <- item.factor.lookup[sel_items]
          compute_semantic_sim_index_v2(
            cos_sub, sel_items, fa, factors,
            model_info$redundancy_threshold, model_info$sigmoid_center, model_info$sigmoid_steepness,
            within_similarity_target = model_info$within_similarity_target,
            within_similarity_band = model_info$within_similarity_band,
            expected_factor_relations = model_info$expected_factor_relations,
            nomological_weight = model_info$nomological_weight %||% 0,
            semantic_objective_mode = model_info$semantic_objective_mode %||% "relative_conservative"
          )$raw_index
        } else NA_real_
        cached_best <- cache_get(solution_cache, make_solution_key(best_ant_vec))
        label_obj <- best_stage_obj[[score_stage]]
        semraw_text <- if (!is.na(sem_idx)) {
          .semantica_format_progress_number(sem_idx, digits = 4L)
        } else {
          "NA"
        }
        cat(sprintf("Run %3d | * NEW %s BEST * | Score: %.4f | SemRaw: %s | rho=%.3f | H=%.3f\n",
                    run_counter, toupper(score_stage), label_obj,
                    semraw_text, rho, ph_entropy))
        if (!is.null(cached_best) && !is.null(cached_best$fit_result) && !is.null(cached_best$fit_result$score_decomp)) {
          d <- cached_best$fit_result$score_decomp
          if (identical(d$dimensionality_mode %||% "", "unidimensional")) {
            cat(sprintf("         Sem=%.4f | CFI_s=%.3f | RMSEA_s=%.3f | SRMR_s=%.3f | AVE_s=%.3f | LQ=%.3f | HTMT_pen=N/A | ESEM_score=%.4f\n", cached_best$sem_score, d$cfi_s, d$rmsea_s, d$srmr_s, d$ave_score, d$loading_quality, d$final_score))
          } else {
            cat(sprintf("         Sem=%.4f | CFI_s=%.3f | RMSEA_s=%.3f | SRMR_s=%.3f | AVE_s=%.3f | LQ=%.3f | HTMT_pen=%.3f | ESEM_score=%.4f\n", cached_best$sem_score, d$cfi_s, d$rmsea_s, d$srmr_s, d$ave_score, d$loading_quality, d$htmt_penalty, d$final_score))
          }
        }
      }
    } else if (verbose && iteration %% 5L == 0L) {
      cat(sprintf("Run %3d | Stage: %-11s | Best: %.4f | Pat: %d/%d | rho=%.3f | H=%.3f | ESEM@%d | Fail: %d/%d\n",
                  run_counter, score_stage, best_ant_obj, patience, search_patience,
                  rho, ph_entropy, esem_interval, n_failed, length(scored_objectives)))
    }

    pheromone <- update_pheromone(pheromone, scored_solutions, scored_objectives, rho,
                                  length(scored_solutions), pheromone_update)
    valid_obj <- scored_objectives[scored_objectives > -1e5]
    top5_mean <- if (length(valid_obj) >= 5L) mean(sort(valid_obj, decreasing = TRUE)[1:5]) else mean(valid_obj)
    recent_tops[[score_stage]] <- c(recent_tops[[score_stage]], top5_mean)
    if (length(recent_tops[[score_stage]]) > stagnation_window) {
      recent_tops[[score_stage]] <- tail(recent_tops[[score_stage]], stagnation_window)
    }
    stagnated <- (length(recent_tops[[score_stage]]) >= stagnation_window) &&
      (diff(range(recent_tops[[score_stage]])) < 0.001)

    if (!improved || stagnated) patience <- patience + 1L else patience <- 0L

    semantic_ready <- archive_states$semantic$stable_count >= archive_stable_window
    pfa_ready <- !requested_pfa_search || (
      pfa_checkpoint_successes >= min_successful_pfa_checkpoints &&
        archive_states$pfa$stable_count >= structural_archive_stable_window
    )
    esem_ready <- !requested_esem_search || (
      esem_checkpoint_successes >= min_successful_esem_checkpoints &&
        archive_states$esem$stable_count >= structural_archive_stable_window
    )
    if (semantic_ready && pfa_ready && esem_ready) {
      termination_reason <- "evidence_archives_stable"
      if (verbose) {
        cat(sprintf(
          paste0(
            "\n  [STOP] Eligible evidence archives stabilized: semantic=%d; ",
            "PFA=%d/%d checkpoints; ESEM=%d/%d checkpoints. ",
            "Terminated by stability heuristic; global optimality is not established.\n"
          ),
          archive_states$semantic$stable_count,
          pfa_checkpoint_successes, min_successful_pfa_checkpoints,
          esem_checkpoint_successes, min_successful_esem_checkpoints
        ))
      }
      break
    }
  }
  if (identical(termination_reason, "patience_exhausted") && iteration >= max_total_iter && patience < search_patience) {
    termination_reason <- "max_total_iter_reached"
    if (verbose) cat(sprintf("\n  [STOP] Reached the hard ACO iteration ceiling (%s).\n", as.character(max_total_iter)))
  } else if (identical(termination_reason, "patience_exhausted") && patience >= search_patience) {
    if (verbose) {
      cat(sprintf(
        "\n  [STOP] Search patience exhausted (%d/%d) after %d iteration(s); global optimality is not established.\n",
        patience, search_patience, iteration
      ))
    }
  }
  aco_search_seconds <- proc.time()[["elapsed"]] - search_started
  stop_search_cluster()
  finalization_started <- proc.time()[["elapsed"]]

  active_archive_tracks <- c(
    "semantic",
    if (requested_pfa_search) "pfa" else character(0L),
    if (requested_esem_search) "esem" else character(0L)
  )
  elite_archive <- .semantica_stratified_finalists(
    elite_archives, active_archive_tracks, budget = elite_k
  )

  if (length(elite_archive) == 0L) {
    stop("No comparable elite solutions were available for final evaluation. Increase the search budget or inspect ESEM checkpoint failures.")
  }

  if (requested_esem_search) {
    search_guidance_status <- if (esem_successes > 0L) {
      if (esem_checkpoint_failures > 0L) "esem_guided_with_checkpoint_fallbacks" else "esem_guided"
    } else if (requested_pfa_search) {
      "pfa_fallback_no_admissible_search_esem"
    } else {
      "semantic_fallback_no_admissible_search_esem"
    }
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat("PHASE 3 -- FINAL EVALUATION\n")
    cat("============================================================\n")
    cat(sprintf("  Finalizing %d unique evidence-stratified candidates from %d iterations; admissible unique search ESEM fits: %d/%d.\n",
                length(elite_archive), iteration, esem_attempts - esem_failures,
                esem_attempts))
    cat(sprintf(
      "  Archive tracks    : semantic=%d | PFA=%d | ESEM=%d\n",
      length(elite_archives$semantic), length(elite_archives$pfa), length(elite_archives$esem)
    ))
    cat(sprintf("  Search guidance   : %s\n", search_guidance_status))
    if (requested_esem_search && esem_weight > 0) {
      cat("  Final phase order: full-ESEM refit each archived candidate; use admissible ESEM-guided reranking when available, otherwise apply the explicitly requested semantic/PFA fallback.\n")
    } else {
      cat("  Final phase order: rerank archived candidates with the active semantic/PFA objective, then run requested post-selection diagnostics.\n")
    }
  }

  .evaluate_archive_solution <- function(entry) {
    v <- entry$vec; sel_items <- item.vector[v == 1L]; fa <- item.factor.lookup[sel_items]
    cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, sel_items), error = function(e) NULL)
    if (is.null(cos_sub)) {
      return(list(score = -Inf, rejection_reason = "similarity_submatrix_failed"))
    }
    sem_r <- compute_semantic_sim_index_v2(
      cos_sub, sel_items, fa, factors, redundancy_threshold, sigmoid_center, sigmoid_steepness,
      within_similarity_target = within_similarity_target_eff,
      within_similarity_band = within_similarity_band,
      expected_factor_relations = expected_factor_relations,
      nomological_weight = nomological_weight,
      semantic_objective_mode = semantic_objective_mode
    )
    dup_pen <- compute_duplicate_penalty(sel_items, fa, factors, cos_sub, dup_threshold)
    facet_mult <- compute_facet_coverage_multiplier(sel_items, fa, item.facet.lookup, facets.by.factor, i.per.f, facet_coverage_weight)
    sem_score_final <- sem_r$sem_score * dup_pen * facet_mult
    pfa_score_final <- NA_real_
    search_score_final <- sem_score_final
    if (pfa_mode == "objective" && pfa_weight > 0) {
      pfa_r <- compute_pfa_diagnostics(
        cos_sub, fa, factors,
        extraction = pfa_extraction,
        rotation = pfa_rotation,
        min_loading = pfa_min_loading,
        min_margin = pfa_min_margin
      )
      pfa_score_final <- if (isTRUE(pfa_r$available)) pfa_r$score else NA_real_
      if (is.finite(pfa_score_final)) {
        search_score_final <- (1 - pfa_weight) * sem_score_final + pfa_weight * pfa_score_final
      } else if (identical(pfa_failure_policy, "semantic_fallback")) {
        search_score_final <- sem_score_final
      } else if (identical(pfa_failure_policy, "penalize")) {
        search_score_final <- (1 - pfa_weight) * sem_score_final
      } else {
        search_score_final <- -Inf
      }
    }
    if (!requested_esem_search || esem_weight <= 0) {
      return(list(score = search_score_final, proposal_score = search_score_final))
    }
    esem_cor <- transform_cosine_for_esem(cos_sub, fa, factors)
    if (is.null(esem_cor)) {
      return(list(score = -Inf, rejection_reason = "esem_correlation_transformation_failed"))
    }
    syntax <- build_esem_syntax_safe(sel_items, fa, factors)
    archive_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, sel_items, fa, factors)
    .semantica_record_archive_esem_fit(evaluation_broker)
    archive_esem_started <- proc.time()[["elapsed"]]
    esem_run <- run_esem_on_matrix(
      syntax, esem_cor, esem_sample_size, model_info$estimator, rotation, archive_rotation_args,
      iter_max = full_esem_iter_max, fallback = TRUE,
      return_diagnostics = TRUE
    )
    esem_fit <- esem_run$fit
    r <- extract_and_score_esem(
      esem_fit, esem_cor, fa, factors, active_cutoffs, htmt_threshold,
      score_mode = semantic_esem_score_mode,
      htmt_objective_role = htmt_objective_role
    )
    r <- .semantica_attach_esem_rejection(r, esem_run)
    .semantica_append_esem_event(
      evaluation_broker, stage = "archive",
      candidate_key = .semantica_object_md5(sort(sel_items)), cache_hit = FALSE,
      elapsed_seconds = proc.time()[["elapsed"]] - archive_esem_started,
      fit_result = r, error = if (isTRUE(r$converged) && isTRUE(r$admissible)) NA_character_ else "archive ESEM unavailable or inadmissible",
      fallback_used = esem_run$fallback_used %||% NA
    )
    if (!isTRUE(r$converged) || !isTRUE(r$admissible)) {
      return(list(
        score = -Inf,
        proposal_score = search_score_final,
        esem_fit = esem_fit,
        esem_result = r,
        esem_cor = esem_cor,
        syntax = syntax,
        rotation_args = archive_rotation_args,
        rejection_reason = paste(
          r$admissibility$reasons %||% "inadmissible_archive_esem",
          collapse = ","
        )
      ))
    }
    guard_pen <- compute_psychometric_guard_penalty(
      r,
      min_ave = psychometric_guard_min_ave,
      min_primary_loading = psychometric_guard_min_loading,
      min_primary_prop_ge_50 = psychometric_guard_min_primary_ge_50,
      htmt_guard_threshold = if (identical(htmt_objective_role, "penalty")) htmt_threshold else Inf
    )
    base_total <- ((1 - esem_weight) * search_score_final + esem_weight * r$score) * (guard_pen ^ psychometric_guard_weight)
    if (elite_multicriteria_rerank && !is.null(r$structure_diagnostics)) {
      sdg <- r$structure_diagnostics
      multicriteria_bonus <- mean(c(
        min(1, max(0, r$ave / 0.50)),
        min(1, max(0, sdg$primary_ge_50 / max(psychometric_guard_min_primary_ge_50, 1e-6))),
        min(1, max(0, sdg$correct_dominance)),
        min(1, max(0, sdg$simple_structure))
      ), na.rm = TRUE)
      rerank_policy <- .semantica_decision_policy()$final_multicriteria
      base_total <- rerank_policy$base_weight * base_total +
        rerank_policy$diagnostic_weight * multicriteria_bonus
    }
    list(
      score = base_total,
      proposal_score = search_score_final,
      esem_fit = esem_fit,
      esem_result = r,
      esem_cor = esem_cor,
      syntax = syntax,
      rotation_args = archive_rotation_args
    )
  }

  # Evaluate each archived candidate at most once. Full ESEM refits are one of
  # the most expensive finalization steps; retaining their payloads avoids a
  # duplicate refit when fallback selection later chooses an inadmissible
  # candidate by its semantic/PFA proposal score.
  archive_evaluations <- lapply(elite_archive, .evaluate_archive_solution)
  archive_final_scores <- vapply(archive_evaluations, function(evaluated) {
    evaluated$score %||% -Inf
  }, numeric(1L))

  archive_selection_mode <- if (!requested_esem_search || esem_weight <= 0) {
    if (pfa_mode == "objective" && pfa_weight > 0) "pfa_semantic_guided" else "semantic_only"
  } else {
    "esem_guided"
  }
  if (!any(is.finite(archive_final_scores))) {
    if (identical(esem_failure_policy, "stop")) {
      stop(
        paste(
          "No archived solution produced an admissible full-ESEM refit for",
          "final ESEM-guided selection. Inadmissible fits were excluded."
        ),
        call. = FALSE
      )
    }
    # Explicit fallback: all archived candidates were given a full ESEM chance.
    # Select the strongest semantic/PFA proposal and reuse that candidate's
    # already-computed inadmissible ESEM payload for transparent diagnostics.
    proposal_scores <- vapply(archive_evaluations, function(x) {
      value <- suppressWarnings(as.numeric(x$proposal_score %||% -Inf))
      if (length(value) != 1L || !is.finite(value)) -Inf else value
    }, numeric(1L))
    if (!any(is.finite(proposal_scores))) {
      stop("No archived solution had a finite semantic/PFA proposal score.", call. = FALSE)
    }
    ord_final <- order(proposal_scores, decreasing = TRUE, na.last = TRUE)
    elite_archive <- elite_archive[ord_final]
    archive_evaluations <- archive_evaluations[ord_final]
    archive_final_scores <- proposal_scores[ord_final]
    best_archive_idx <- 1L
    best_archive_evaluation <- archive_evaluations[[best_archive_idx]]
    archive_selection_mode <- if (requested_pfa_search) {
      "pfa_fallback_no_admissible_archive_esem"
    } else {
      "semantic_fallback_no_admissible_archive_esem"
    }
    search_guidance_status <- archive_selection_mode
  } else {
    ord_final <- order(archive_final_scores, decreasing = TRUE, na.last = TRUE)
    elite_archive <- elite_archive[ord_final]
    archive_evaluations <- archive_evaluations[ord_final]
    archive_final_scores <- archive_final_scores[ord_final]
    best_archive_idx <- 1L
    best_archive_evaluation <- archive_evaluations[[best_archive_idx]]
  }
  best_vector <- elite_archive[[best_archive_idx]]$vec
  best_items <- item.vector[best_vector == 1L]
  factor_assignment <- item.factor.lookup[best_items]

  cos_sub_best <- extract_similarity_submatrix(cosine_sim_matrix, best_items)
  sem_final <- compute_semantic_sim_index_v2(
    cos_sub_best, best_items, factor_assignment, factors, redundancy_threshold, sigmoid_center, sigmoid_steepness,
    within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band,
    expected_factor_relations = expected_factor_relations,
    nomological_weight = nomological_weight,
    semantic_objective_mode = semantic_objective_mode
  )
  final_dup_penalty <- compute_duplicate_penalty(best_items, factor_assignment, factors, cos_sub_best, dup_threshold)
  final_facet_multiplier <- compute_facet_coverage_multiplier(
    best_items, factor_assignment, item.facet.lookup, facets.by.factor, i.per.f, facet_coverage_weight
  )
  final_facet_coverage <- attr(final_facet_multiplier, "coverage", exact = TRUE)
  if (is.null(final_facet_coverage) || !is.finite(final_facet_coverage)) final_facet_coverage <- NA_real_
  final_semantic_objective_score <- sem_final$sem_score * final_dup_penalty * final_facet_multiplier
  final_pfa_diagnostics <- NULL
  final_pfa_score <- NA_real_
  final_pfa_objective_diagnostics <- NULL
  final_pfa_objective_score <- NA_real_
  final_search_objective_score <- final_semantic_objective_score
  if (pfa_mode == "objective" && pfa_weight > 0) {
    final_pfa_objective_diagnostics <- compute_pfa_diagnostics(
      cos_sub_best, factor_assignment, factors,
      extraction = pfa_extraction,
      rotation = pfa_rotation,
      min_loading = pfa_min_loading,
      min_margin = pfa_min_margin
    )
    final_pfa_objective_score <- if (isTRUE(final_pfa_objective_diagnostics$available)) {
      final_pfa_objective_diagnostics$score
    } else {
      NA_real_
    }
    if (is.finite(final_pfa_objective_score)) {
      final_search_objective_score <- (1 - pfa_weight) * final_semantic_objective_score +
        pfa_weight * final_pfa_objective_score
    } else if (identical(pfa_failure_policy, "semantic_fallback")) {
      final_search_objective_score <- final_semantic_objective_score
    } else if (identical(pfa_failure_policy, "penalize")) {
      final_search_objective_score <- (1 - pfa_weight) * final_semantic_objective_score
    } else {
      final_search_objective_score <- NA_real_
    }
  }
  if (pfa_mode == "objective" && pfa_weight > 0 &&
      identical(pfa_extraction, pfa_final_extraction)) {
    final_pfa_diagnostics <- final_pfa_objective_diagnostics
    final_pfa_score <- final_pfa_objective_score
  } else if (pfa_mode != "off") {
    final_pfa_diagnostics <- compute_pfa_diagnostics(
      cos_sub_best, factor_assignment, factors,
      extraction = pfa_final_extraction,
      rotation = pfa_rotation,
      min_loading = pfa_min_loading,
      min_margin = pfa_min_margin
    )
    final_pfa_score <- if (isTRUE(final_pfa_diagnostics$available)) final_pfa_diagnostics$score else NA_real_
  }

  final_syntax <- build_esem_syntax_safe(best_items, factor_assignment, factors)
  final_esem_cor <- best_archive_evaluation$esem_cor %||%
    transform_cosine_for_esem(cos_sub_best, factor_assignment, factors)
  final_esem_fit <- NULL
  final_dfi_cutoffs <- NULL
  final_dddfi_cutoffs <- NULL
  final_equivtest_diagnostic <- NULL
  semantic_n_sensitivity_result <- NULL
  recommended_validation_n <- NULL
  response_validation <- NULL
  final_active_cutoffs <- active_cutoffs
  final_cutoff_source <- cutoff_source
  final_esem_run <- NULL
  final_esem_result <- list(converged = FALSE, score = 0, cfi = NA, tli = NA, rmsea = NA, srmr = NA,
                            ave = NA, htmt_max = NA, htmt_violations = Inf,
                            loading_quality = 0, score_decomp = NULL)
  if (!is.null(final_esem_cor)) {
    final_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, best_items, factor_assignment, factors)
    if (!is.null(best_archive_evaluation$esem_fit)) {
      if (verbose) cat("\n  [FINAL MODEL] Reusing the selected archive solution's full ESEM fit...\n")
      final_esem_fit <- best_archive_evaluation$esem_fit
      final_rotation_args <- best_archive_evaluation$rotation_args %||% final_rotation_args
    } else {
      if (verbose) cat("\n  [FINAL MODEL] Fitting the selected semantic-proxy ESEM...\n")
      .semantica_record_final_esem_fit(evaluation_broker)
      final_esem_started <- proc.time()[["elapsed"]]
      final_esem_run <- run_esem_on_matrix(
        final_syntax, final_esem_cor, esem_sample_size, model_info$estimator, rotation, final_rotation_args,
        iter_max = full_esem_iter_max, fallback = TRUE,
        return_diagnostics = TRUE
      )
      final_esem_fit <- final_esem_run$fit
    }

    # Final DFI recalibration is a post-selection diagnostic: it calibrates
    # reporting cutoffs to the exact selected full-ESEM solution, consistent
    # with dynamic-fit logic. It is intentionally not used to rerank the archive,
    # which would make the search objective depend on a post-hoc target.
    if (
      final_dfi_recalibrate &&
      requested_esem_search &&
      dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi") &&
      !is.null(final_esem_fit) &&
      isTRUE(lavaan::lavInspect(final_esem_fit, "converged"))
    ) {
      if (verbose) cat("\n  [FINAL DFI] Recalibrating cutoff diagnostics for the selected ESEM...\n")
      final_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, best_items, factor_assignment, factors)
      final_dfi_cl <- NULL
      get_final_dfi_cluster <- function() {
        if (is.null(final_dfi_cl) && dfi_n_cores() > 1L) {
          final_dfi_cl <<- .semantica_make_dfi_cluster(dfi_n_cores())
        }
        final_dfi_cl
      }
      on.exit({
        if (!is.null(final_dfi_cl)) .semantica_stop_cluster(final_dfi_cl)
      }, add = TRUE)
      if (dfi_mode %in% c("auto", "semantic_roc_dfi")) {
        final_dfi_cutoffs <- compute_semantic_roc_dfi_cutoffs(
          esem_fit = final_esem_fit,
          esem_syntax = final_syntax,
          factors = factors,
          items_per_factor = i.per.f,
          observed_cor = final_esem_cor,
          factor_assignment = factor_assignment,
          n_obs = esem_sample_size,
          estimator = model_info$estimator,
          rotation = rotation,
          rotation_args = final_rotation_args,
          reps = final_dfi_reps,
          level = dfi_level,
          criterion = dfi_criterion,
          n_cores = dfi_n_cores(),
          iter_max = min(full_esem_iter_max, 1200L),
          embed_reliability = embed_reliability,
          misspec_strength = dfi_roc_misspec_strength,
          verbose = FALSE,
          progress = verbose,
          cache = dfi_calibration_cache,
          cluster = get_final_dfi_cluster,
          adaptive = identical(dfi_esem_strategy, "adaptive"),
          adaptive_min_reps = dfi_adaptive_min_reps,
          adaptive_batch_reps = dfi_adaptive_batch_reps,
          adaptive_tol = dfi_adaptive_tol,
          adaptive_stable_batches = dfi_adaptive_stable_batches
        )
      }
      if ((is.null(final_dfi_cutoffs) || isTRUE(final_dfi_cutoffs$was_degenerate)) &&
          dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi") &&
          (identical(dfi_fallback_policy, "conservative") ||
             dfi_mode %in% c("auto", "semantic_approx_dfi"))) {
        final_dfi_cutoffs <- compute_semantic_approx_dfi_cutoffs(
          esem_fit = final_esem_fit,
          esem_syntax = final_syntax,
          factors = factors,
          items_per_factor = i.per.f,
          observed_cor = final_esem_cor,
          n_obs = esem_sample_size,
          estimator = model_info$estimator,
          rotation = rotation,
          rotation_args = final_rotation_args,
          reps = final_dfi_reps,
          level = dfi_level,
          criterion = dfi_criterion,
          n_cores = dfi_n_cores(),
          iter_max = min(full_esem_iter_max, 1200L),
          embed_reliability = embed_reliability,
          verbose = FALSE,
          progress = verbose,
          cache = dfi_calibration_cache,
          cluster = get_final_dfi_cluster,
          adaptive = identical(dfi_esem_strategy, "adaptive"),
          adaptive_min_reps = dfi_adaptive_min_reps,
          adaptive_batch_reps = dfi_adaptive_batch_reps,
          adaptive_tol = dfi_adaptive_tol,
          adaptive_stable_batches = dfi_adaptive_stable_batches
        )
      }
      need_final_exact_esem_dfi <- dfi_mode == "esem_parametric_dfi" ||
        (
          (identical(dfi_fallback_policy, "conservative") || dfi_mode == "auto") &&
            (is.null(final_dfi_cutoffs) || isTRUE(final_dfi_cutoffs$was_degenerate))
        )
      if (need_final_exact_esem_dfi) {
        final_dfi_cutoffs <- compute_esem_parametric_dfi_cutoffs(
          esem_fit = final_esem_fit,
          esem_syntax = final_syntax,
          factors = factors,
          items_per_factor = i.per.f,
          n_obs = esem_sample_size,
          estimator = model_info$estimator,
          rotation = rotation,
          rotation_args = final_rotation_args,
          reps = final_dfi_reps,
          level = dfi_level,
          criterion = dfi_criterion,
          n_cores = dfi_n_cores(),
          iter_max = min(full_esem_iter_max, 1200L),
          verbose = FALSE,
          progress = verbose,
          cache = dfi_calibration_cache,
          cluster = get_final_dfi_cluster,
          adaptive = identical(dfi_esem_strategy, "adaptive"),
          adaptive_min_reps = dfi_adaptive_min_reps,
          adaptive_batch_reps = dfi_adaptive_batch_reps,
          adaptive_tol = dfi_adaptive_tol,
          adaptive_stable_batches = dfi_adaptive_stable_batches
        )
      }
      if (!is.null(final_dfi_cl)) {
        .semantica_stop_cluster(final_dfi_cl)
        final_dfi_cl <- NULL
      }
      if (!is.null(final_dfi_cutoffs) && !isTRUE(final_dfi_cutoffs$was_degenerate)) {
        final_active_cutoffs <- final_dfi_cutoffs
        final_cutoff_source <- if (identical(final_dfi_cutoffs$cutoff_calibration, "ESEM-semantic-ROC")) {
          sprintf("DFI (final ESEM semantic-ROC proxy, %s)", final_dfi_cutoffs$dfi_function)
        } else if (identical(final_dfi_cutoffs$cutoff_calibration, "ESEM-semantic-approximate")) {
          sprintf("DFI (final ESEM semantic-approximate proxy, %s)", final_dfi_cutoffs$dfi_function)
        } else {
          sprintf("DFI (final ESEM-parametric semantic proxy, %s)", final_dfi_cutoffs$dfi_function)
        }
      }
    }

    final_esem_result <- extract_and_score_esem(
      final_esem_fit, final_esem_cor, factor_assignment, factors,
      final_active_cutoffs, htmt_threshold, verbose_decomp = FALSE,
      score_mode = semantic_esem_score_mode,
      htmt_objective_role = htmt_objective_role
    )
    if (!is.null(final_esem_run)) {
      final_esem_result <- .semantica_attach_esem_rejection(
        final_esem_result, final_esem_run
      )
    } else if (!is.null(best_archive_evaluation$esem_result$admissibility)) {
      # When finalization reuses an archive fit, preserve the solver/admissibility
      # assessment that was already attached during the archive evaluation.
      # Re-extracting fit indices alone can otherwise lose the precise rejection
      # reasons that justified the fallback decision.
      final_esem_result$admissibility <- best_archive_evaluation$esem_result$admissibility
      final_esem_result$converged <- isTRUE(best_archive_evaluation$esem_result$converged)
      final_esem_result$admissible <- isTRUE(best_archive_evaluation$esem_result$admissible)
    }

    if (!is.null(final_esem_run)) {
      .semantica_append_esem_event(
        evaluation_broker, stage = "final",
        candidate_key = .semantica_object_md5(sort(best_items)), cache_hit = FALSE,
        elapsed_seconds = if (exists("final_esem_started", inherits = FALSE)) proc.time()[["elapsed"]] - final_esem_started else NA_real_,
        fit_result = final_esem_result,
        error = if (isTRUE(final_esem_result$converged) && isTRUE(final_esem_result$admissible)) NA_character_ else "final ESEM unavailable or inadmissible",
        fallback_used = final_esem_run$fallback_used %||% NA
      )
    }

    if (semantic_n_sensitivity) {
      semantic_n_grid_eff <- build_semantic_reference_n_grid(
        reference_n_info,
        n_grid = semantic_n_grid,
        multipliers = semantic_n_multipliers,
        min_n = nrow(final_esem_cor) + 3L,
        max_n = reference_max_n
      )
      if (verbose) {
        cat(sprintf("\n  [PROXY N STABILITY] Refitting selected semantic-proxy ESEM over reference-N anchors: %s\n",
                    if (length(semantic_n_grid_eff) > 0L) paste(semantic_n_grid_eff, collapse = ", ") else "unavailable"))
      }
      .semantica_record_final_esem_fit(
        evaluation_broker, length(semantic_n_grid_eff)
      )
      semantic_n_sensitivity_result <- evaluate_semantic_n_sensitivity(
        syntax = final_syntax,
        cor_matrix = final_esem_cor,
        factor_assignment = factor_assignment,
        factors = factors,
        n_grid = semantic_n_grid_eff,
        estimator = model_info$estimator,
        rotation = rotation,
        rotation_args = final_rotation_args,
        cutoffs = final_active_cutoffs,
        htmt_threshold = htmt_threshold,
        score_mode = semantic_esem_score_mode,
        iter_max = min(full_esem_iter_max, semantic_n_iter_max),
        sample_cov_rescale = FALSE,
        reference_n = reference_n_info$used_n_obs %||% reference_n_info$n_obs,
        progress = verbose
      )
    }

    if (!is.null(validation_data)) {
      response_estimator <- if (data_type %in% c("categorical", "likert")) "WLSMV" else model_info$estimator
      .semantica_record_final_esem_fit(evaluation_broker)
      response_fit <- run_esem_on_response_data(
        final_syntax, validation_data, best_items,
        estimator = response_estimator,
        rotation = rotation,
        rotation_args = final_rotation_args,
        ordered = validation_ordered,
        iter_max = full_esem_iter_max,
        fallback = TRUE
      )
      response_cor <- compute_response_cor(validation_data, best_items)
      response_result <- extract_and_score_esem(
        response_fit, response_cor, factor_assignment, factors,
        final_active_cutoffs, htmt_threshold, verbose_decomp = FALSE,
        score_mode = semantic_esem_score_mode
      )
      response_validation <- list(
        fit = response_fit,
        result = response_result,
        estimator = response_estimator,
        ordered = validation_ordered,
        n_obs = if (is.data.frame(validation_data) || is.matrix(validation_data)) nrow(validation_data) else NA_integer_,
        note = "Response-data validation is based on observed item responses and should take priority over semantic-proxy ESEM for final scale validation."
      )
    }

    if (
      final_dddfi &&
      !is.null(final_esem_fit) &&
      isTRUE(lavaan::lavInspect(final_esem_fit, "converged"))
    ) {
      if (verbose) cat("\n  [FINAL DDDFI] Computing direct-discrepancy approximate-fit cutoffs...\n")
      final_dddfi_cutoffs <- compute_dddfi_final_cutoffs(
        esem_fit = final_esem_fit,
        reps = final_dddfi_reps,
        mad_target = final_dddfi_mad_target,
        estimator = model_info$estimator,
        scale = "normal",
        verbose = FALSE
      )
    }
    if (
      final_equivtest &&
      !is.null(final_esem_fit) &&
      isTRUE(lavaan::lavInspect(final_esem_fit, "converged"))
    ) {
      final_equivtest_diagnostic <- compute_equivtest_final_diagnostic(
        final_esem_fit, verbose = FALSE
      )
    }
    if (validation_n_diagnostic && !is.null(final_pfa_diagnostics) &&
        isTRUE(final_pfa_diagnostics$available)) {
      proxy_inadmissible <- requested_esem_search && !isTRUE(final_esem_result$admissible)
      if (proxy_inadmissible && identical(validation_n_on_inadmissible, "skip")) {
        recommended_validation_n <- list(
          available = FALSE, skipped = TRUE, recommended_n = NA_integer_,
          reason = "base_semantic_proxy_esem_inadmissible",
          note = paste(
            "Validation-N planning was skipped because the selected semantic-proxy",
            "ESEM was inadmissible. Increase N cannot repair a structurally",
            "inadmissible proxy model; set validation_planning_on_inadmissible",
            "= 'run' only for a deliberate legacy sensitivity analysis."
          )
        )
        if (verbose) {
          cat("\n  [VALIDATION N] Skipped: selected semantic-proxy ESEM is inadmissible; sample-size planning would be misleading.\n")
        }
      } else {
        if (verbose) cat("\n  [VALIDATION N] Estimating PFA-informed response-data planning N...\n")
        validation_grid_eff <- validation_n_grid
        if (is.null(validation_grid_eff)) {
          validation_grid_eff <- unique(as.integer(round(c(
            reference_n_info$used_n_obs %||% reference_n_info$n_obs,
            1.5 * (reference_n_info$used_n_obs %||% reference_n_info$n_obs),
            2.0 * (reference_n_info$used_n_obs %||% reference_n_info$n_obs),
            3.0 * (reference_n_info$used_n_obs %||% reference_n_info$n_obs),
            validation_n_max
          ))))
        }
        .semantica_record_final_esem_fit(
          evaluation_broker,
          length(validation_grid_eff) * as.integer(validation_n_reps)
        )
        recommended_validation_n <- estimate_recommended_validation_n(
          final_pfa_diagnostics,
          factor_assignment,
          factors,
          syntax = final_syntax,
          rotation = rotation,
          rotation_args = final_rotation_args,
          estimator = model_info$estimator,
          n_grid = validation_grid_eff,
          reps = validation_n_reps,
          convergence_target = validation_n_convergence,
          max_heywood_rate = validation_n_max_heywood,
          min_recovery = validation_n_min_recovery,
          max_primary_error = validation_n_max_loading_error,
          min_dominance_recovery = validation_n_min_dominance,
          max_crossloading_error = validation_n_max_cross_error,
          max_factor_cor_error = validation_n_max_factor_cor_error,
          max_n = validation_n_max,
          iter_max = min(full_esem_iter_max, 400L),
          seed = NULL,
          verbose = FALSE,
          progress = verbose
        )
      }
    }
  }

  semantic_similarity_reduction <- compute_semantic_similarity_reduction_summary(
    cos_mat = cosine_sim_matrix,
    pool_items = item.vector,
    pool_factor_assignment = item.factor.lookup,
    selected_items = best_items,
    selected_factor_assignment = factor_assignment,
    factors = factors,
    within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band
  )

  semantic_perturbation_seed <- sample.int(.Machine$integer.max, 1L)
  semantic_resampling_stability <- tryCatch(
    semantica_semantic_resampling_stability(
      similarity_matrix = cos_sub_best,
      factor_assignment = factor_assignment,
      reps = 1000L,
      seed = semantic_perturbation_seed
    ),
    error = function(e) list(
      status = "unavailable",
      error = conditionMessage(e),
      seed = semantic_perturbation_seed,
      evidence_family = "embedding_semantic",
      participant_based = FALSE,
      note = "Semantic resampling sensitivity could not be computed."
    )
  )

  # Backward-compatible aliases remain present so serialized-result consumers do
  # not fail, but the legacy random half-pair zeroing rule and its uncalibrated
  # 0.10 stable/unstable boundary are no longer used for new analyses.
  semantic_pair_perturbation_stability <- list(
    sem_half_A = NA_real_, sem_half_B = NA_real_, difference = NA_real_,
    stable = NA, classification = "superseded_by_resampling_sensitivity",
    heuristic_threshold = NA_real_, threshold_status = "retired_legacy_heuristic",
    seed = semantic_perturbation_seed,
    method = "stratified_pair_bootstrap_and_item_jackknife",
    resampling = semantic_resampling_stability
  )
  split_half_stability <- semantic_pair_perturbation_stability

  solution_history_list <- if (!is.null(solution_history_env) && solution_history_env$n > 0L) head(solution_history_env$history, solution_history_env$n) else NULL

  run_warnings <- character(0)
  if (!is.null(dfi_cutoffs) && isTRUE(dfi_cutoffs$was_degenerate)) run_warnings <- c(run_warnings, "DFI cutoffs degenerate -- heuristics used")
  if (requested_esem_search && dfi_enabled && is.null(bootstrap_params)) {
    run_warnings <- c(run_warnings, sprintf("Bootstrap ESEM failed -- %s fallback", dfi_loading_source))
  }
  if (requested_esem_search && dfi_mode %in% c("auto", "semantic_roc_dfi") && is.null(semantic_roc_cutoffs)) {
    run_warnings <- c(run_warnings, "Semantic-ROC ESEM DFI unavailable -- fallback cutoffs used")
  }
  if (!is.null(semantic_roc_cutoffs) && isTRUE(semantic_roc_cutoffs$was_degenerate)) {
    run_warnings <- c(run_warnings, "Semantic-ROC ESEM DFI cutoffs degenerate -- fallback cutoffs used")
  }
  if (requested_esem_search && dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi") && is.null(semantic_approx_cutoffs) && (is.null(semantic_roc_cutoffs) || isTRUE(semantic_roc_cutoffs$was_degenerate))) {
    run_warnings <- c(run_warnings, "Semantic-approximate ESEM DFI unavailable -- fallback cutoffs used")
  }
  if (!is.null(semantic_approx_cutoffs) && isTRUE(semantic_approx_cutoffs$was_degenerate)) {
    run_warnings <- c(run_warnings, "Semantic-approximate ESEM DFI cutoffs unusually permissive -- fallback cutoffs used")
  }
  if (requested_esem_search && dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi") && is.null(esem_parametric_cutoffs) && is.null(semantic_approx_cutoffs) && is.null(semantic_roc_cutoffs)) {
    run_warnings <- c(run_warnings, "ESEM-parametric DFI unavailable -- fallback cutoffs used")
  }
  if (final_dfi_recalibrate && requested_esem_search && dfi_mode %in% c("auto", "semantic_roc_dfi", "semantic_approx_dfi", "esem_parametric_dfi") && is.null(final_dfi_cutoffs)) {
    run_warnings <- c(run_warnings, "Final ESEM DFI recalibration unavailable -- search cutoffs reported")
  }
  if (final_dddfi && is.null(final_dddfi_cutoffs)) {
    run_warnings <- c(run_warnings, "Final DDDFI unavailable -- ESEM-parametric cutoffs reported")
  }
  if (final_equivtest && is.null(final_equivtest_diagnostic)) {
    run_warnings <- c(run_warnings, "Final dynamic equivalence-test diagnostic unavailable")
  }
  if (!isTRUE(final_esem_result$admissible)) {
    final_reasons <- final_esem_result$admissibility$reasons %||%
      "no admissible ESEM solution"
    run_warnings <- c(run_warnings, sprintf(
      "Final ESEM diagnostics unavailable -- %s",
      paste(final_reasons, collapse = ", ")
    ))
  }
  if (validation_n_diagnostic && !is.null(recommended_validation_n) &&
      isTRUE(recommended_validation_n$skipped)) {
    run_warnings <- c(
      run_warnings,
      "Validation-N planning was intentionally skipped because the selected semantic-proxy ESEM was inadmissible"
    )
  } else if (validation_n_diagnostic && (is.null(recommended_validation_n) ||
      !isTRUE(recommended_validation_n$available) ||
      !is.finite(recommended_validation_n$recommended_n))) {
    run_warnings <- c(run_warnings, "Recommended validation-N diagnostic unavailable or no candidate N met criteria")
  }
  if (semantic_n_sensitivity && (is.null(semantic_n_sensitivity_result) ||
      !isTRUE(semantic_n_sensitivity_result$available))) {
    run_warnings <- c(run_warnings, "Semantic proxy N-sensitivity diagnostic unavailable")
  }
  if (!is.null(semantic_n_sensitivity_result) &&
      isTRUE(semantic_n_sensitivity_result$available) &&
      identical(semantic_n_sensitivity_result$summary$structurally_stable, FALSE)) {
    run_warnings <- c(run_warnings, "Selected semantic-proxy ESEM structure changed across reference-N anchors")
  }
  if (!is.null(final_pfa_diagnostics) && !isTRUE(final_pfa_diagnostics$available)) {
    run_warnings <- c(run_warnings, sprintf("Sample-free PFA unavailable -- %s", final_pfa_diagnostics$note %||% "diagnostic failed"))
  }
  if (duplicate_clusters$n_clusters > 0L && isTRUE(duplicate_guard_infeasible)) {
    run_warnings <- c(run_warnings, sprintf(
      "Duplicate-cluster guard infeasible for factor(s): %s; duplicate penalty remained active",
      paste(duplicate_feasibility$factor[!duplicate_feasibility$feasible], collapse = ", ")
    ))
  }
  if (!is.null(final_pfa_diagnostics) && isTRUE(final_pfa_diagnostics$available) &&
      length(final_pfa_diagnostics$missing_factors) > 0L) {
    run_warnings <- c(run_warnings, sprintf(
      "Sample-free PFA did not recover all intended factors: %s",
      paste(final_pfa_diagnostics$missing_factors, collapse = ", ")
    ))
  }
  if (requested_esem_search && esem_failures > 0L) {
    run_warnings <- c(run_warnings, sprintf(
      "Search-time ESEM scoring failed for %d of %d unique fitted candidates",
      esem_failures, esem_attempts
    ))
  }
  fallback_final_statuses <- c(
    "semantic_fallback_no_admissible_search_esem",
    "pfa_fallback_no_admissible_search_esem",
    "semantic_fallback_no_admissible_archive_esem",
    "pfa_fallback_no_admissible_archive_esem"
  )
  if (search_guidance_status %in% fallback_final_statuses) {
    run_warnings <- c(
      run_warnings,
      if (grepl("^pfa_", search_guidance_status)) {
        "No admissible search/archive ESEM solution was found; final selection used the explicit PFA/semantic fallback objective"
      } else {
        "No admissible search/archive ESEM solution was found; final selection used the explicit semantic fallback objective"
      }
    )
  } else if (identical(search_guidance_status, "esem_guided_with_checkpoint_fallbacks")) {
    run_warnings <- c(
      run_warnings,
      "One or more search-time ESEM checkpoints failed, but later admissible ESEM guidance was recovered"
    )
  }

  dfi_stage_results <- list(
    search_semantic_roc = semantic_roc_cutoffs,
    search_semantic_approx = semantic_approx_cutoffs,
    search_esem_parametric = esem_parametric_cutoffs,
    search_strict = strict_dfi_cutoffs,
    final_recalibration = final_dfi_cutoffs,
    final_dddfi = final_dddfi_cutoffs
  )
  dfi_stage_fits <- vapply(dfi_stage_results, function(x) {
    telemetry <- x$telemetry %||% NULL
    if (is.null(telemetry) || isTRUE(telemetry$cache_hit)) return(0L)
    n <- suppressWarnings(as.integer(telemetry$completed_reps %||% 0L))
    if (length(n) != 1L || !is.finite(n) || n < 0L) 0L else n
  }, integer(1L))
  evaluation_broker$dfi_fits_started <- sum(dfi_stage_fits)
  dfi_elapsed_seconds <- sum(vapply(dfi_stage_results, function(x) {
    value <- suppressWarnings(as.numeric(x$telemetry$elapsed_seconds %||% 0))
    if (length(value) != 1L || !is.finite(value) || value < 0) 0 else value
  }, numeric(1L)))
  dfi_uninstrumented_stages <- names(dfi_stage_results)[vapply(
    dfi_stage_results,
    function(x) !is.null(x) && is.null(x$telemetry),
    logical(1L)
  )]

  evaluation_telemetry <- .semantica_evaluation_snapshot(evaluation_broker)
  evaluation_telemetry$dfi_stage_fits_started <- dfi_stage_fits
  evaluation_telemetry$dfi_uninstrumented_stages <- dfi_uninstrumented_stages
  evaluation_telemetry$dfi_accounting_note <- paste(
    "DFI counts cover SEMANTICA simulation jobs that expose telemetry;",
    "third-party dynamic/DDD-FI internals may not expose individual fit starts."
  )
  esem_attempts <- evaluation_telemetry$esem_fits_started
  esem_successes <- evaluation_telemetry$esem_fits_admissible
  esem_failures <- evaluation_telemetry$esem_fits_failed

  safe_version <- function(package) {
    # Read installed metadata without loading optional namespaces. Some optional
    # accelerators (notably torch) initialize native runtimes during namespace
    # loading; reproducibility metadata must never trigger that side effect.
    desc <- tryCatch(
      utils::packageDescription(package, fields = "Version"),
      error = function(e) NA_character_
    )
    value <- suppressWarnings(as.character(desc[[1L]] %||% desc))
    if (!length(value) || is.na(value) || !nzchar(value)) NA_character_ else value
  }
  reproducibility_metadata <- list(
    semantica_version = safe_version("SEMANTICA"),
    r_version = R.version.string,
    package_versions = list(
      lavaan = safe_version("lavaan"),
      Matrix = safe_version("Matrix"),
      dynamic = safe_version("dynamic"),
      parallelly = safe_version("parallelly"),
      torch = safe_version("torch")
    ),
    rng_kind = rng_kind_initial,
    master_seed = seed,
    initial_rng_state = rng_state_initial,
    semantic_pair_perturbation_seed = semantic_perturbation_seed,
    search_esem_task_seeds = if (length(esem_task_seed_records) > 0L) {
      do.call(rbind, esem_task_seed_records)
    } else {
      data.frame(
        iteration = integer(0L), candidate_key = character(0L),
        seed = integer(0L), stringsAsFactors = FALSE
      )
    },
    dfi_task_seeds = lapply(dfi_stage_results, function(stage) {
      stage$telemetry$task_seeds %||% integer(0L)
    }),
    effective_workers = n.cores,
    requested_workers = requested_n_cores,
    optimizer = list(
      ants = ants, search_patience = search_patience, max_patience = search_patience,
      legacy_max_iter = max.iter, max_total_iter = max_total_iter,
      max_esem_fits = max_esem_fits, esem_every = esem_every, esem_cadence_mode = esem_cadence_mode,
      esem_eval_top_k = esem_eval_top_k_eff,
      run_esem_during_search = requested_esem_search,
      pfa_mode = pfa_mode, pfa_failure_policy = pfa_failure_policy,
      pfa_every = pfa_every,
      archive_stable_window = archive_stable_window,
      structural_archive_stable_window = structural_archive_stable_window,
      min_successful_pfa_checkpoints = min_successful_pfa_checkpoints,
      min_successful_esem_checkpoints = min_successful_esem_checkpoints,
      evidence_archive_sizes = vapply(elite_archives, length, integer(1L)),
      finalist_budget = elite_k,
      finalist_count = length(elite_archive),
      finalist_source_tracks = lapply(elite_archive, function(e) e$source_tracks %||% e$score_type %||% NA_character_),
      objective_schema = objective_schema,
      pheromone_update = pheromone_update,
      evaporation = evaporation_resolved,
      fixed_evaporation = fixed_evaporation
    ),
    semantic_proxy = list(
      reference_n = reference_n_info,
      cosine_adjustment = NA_character_,
      dfi_mode = dfi_mode
    )
  )
  finalization_seconds <- proc.time()[["elapsed"]] - finalization_started
  resource_telemetry <- .semantica_resource_telemetry(
    resource_plan,
    elapsed_seconds = proc.time()[["elapsed"]] - aco_start_time
  )
  resource_telemetry$workers_created <- if (use_parallel) n.cores else 0L
  performance <- list(
    resource = resource_telemetry,
    compute = list(
      requested_device = "cpu",
      resolved_device = "cpu",
      note = "lavaan, DFI, PFA, and ACO execution remain CPU-based."
    ),
    timing = list(
      warmup_and_search_dfi_seconds = unname(search_started - aco_start_time),
      aco_search_seconds = unname(aco_search_seconds),
      esem_search_seconds = unname(esem_search_seconds),
      dfi_reported_seconds = unname(dfi_elapsed_seconds),
      finalization_seconds = unname(finalization_seconds),
      total_seconds = unname(proc.time()[["elapsed"]] - aco_start_time)
    ),
    evaluations = evaluation_telemetry,
    candidate_evaluations = candidate_evaluations,
    iterations = iteration
  )

  pfa_esem_discrepancy <- semantica_pfa_esem_discrepancy(
    final_pfa_diagnostics, final_esem_result
  )
  final_esem_state <- semantica_esem_state(
    requested = TRUE, attempted = TRUE, esem_result = final_esem_result,
    failure_reason = final_esem_result$admissibility$reasons %||% NULL,
    fallback_policy = esem_failure_policy, stage = "final"
  )

  selection_semantic_context <- tryCatch(
    semantica_selection_context(
      cosine_sim_matrix[item.vector, item.vector, drop = FALSE],
      item.factor.lookup[item.vector],
      best_items
    ),
    error = function(e) list(
      status = "unavailable", reason = conditionMessage(e),
      selection_conditioned = TRUE, participant_based = FALSE
    )
  )
  if (is.list(selection_semantic_context) && identical(selection_semantic_context$status %||% "", "computed")) {
    selection_semantic_context$pool_scope <- "aco_eligible_candidate_pool_after_guards"
  }
  factor_semantic_diagnostics <- selection_semantic_context$selected_factor_diagnostics %||% NULL
  dimensionality_mode <- if (length(unique(as.character(factors))) == 1L) "unidimensional" else "multidimensional"
  unidimensional_diagnostics <- if (identical(dimensionality_mode, "unidimensional")) {
    .semantica_unidimensional_proxy_diagnostics(
      final_esem_result, final_esem_cor, selection_semantic_context
    )
  } else NULL

  # Typed evidence records keep "not obtained" distinct from an observed poor
  # value. They are reporting/provenance metadata and do not modify scoring.
  selection_context_record <- if (identical(selection_semantic_context$status %||% "", "computed")) {
    .semantica_evidence_record(
      "computed", value = selection_semantic_context, participant_based = FALSE,
      selection_conditioned = TRUE,
      evidence_scope = "aco_eligible_candidate_pool_and_selected_set"
    )
  } else {
    .semantica_evidence_record(
      "unavailable", value = selection_semantic_context,
      reason = selection_semantic_context$reason %||% "selection semantic context unavailable",
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "aco_eligible_candidate_pool_and_selected_set"
    )
  }
  pfa_record <- if (identical(pfa_mode, "off")) {
    .semantica_evidence_record(
      "not_requested",
      reason = if (identical(dimensionality_mode, "unidimensional")) {
        "PFA factor-recovery/partition diagnostics are not applicable to a one-factor model."
      } else NULL,
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "selected semantic proxy"
    )
  } else if (isTRUE(final_pfa_diagnostics$available)) {
    .semantica_evidence_record(
      "computed", value = final_pfa_diagnostics, participant_based = FALSE,
      selection_conditioned = TRUE, evidence_scope = "selected semantic proxy"
    )
  } else if (identical(pfa_failure_policy, "semantic_fallback")) {
    .semantica_evidence_record(
      "fallback", value = final_pfa_diagnostics,
      reason = final_pfa_diagnostics$note %||% "PFA unavailable; semantic fallback used where needed",
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "selected semantic proxy"
    )
  } else {
    .semantica_evidence_record(
      "unavailable", value = final_pfa_diagnostics,
      reason = final_pfa_diagnostics$note %||% "PFA unavailable",
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "selected semantic proxy"
    )
  }
  esem_record <- if (isTRUE(final_esem_result$admissible)) {
    .semantica_evidence_record(
      "computed", value = final_esem_state, participant_based = FALSE,
      selection_conditioned = TRUE, evidence_scope = "selected semantic proxy"
    )
  } else if (grepl("fallback", archive_selection_mode %||% "", fixed = TRUE)) {
    .semantica_evidence_record(
      "fallback", value = final_esem_state,
      reason = paste(final_esem_result$admissibility$reasons %||% "final ESEM inadmissible", collapse = ", "),
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "selected semantic proxy"
    )
  } else {
    .semantica_evidence_record(
      "unavailable", value = final_esem_state,
      reason = paste(final_esem_result$admissibility$reasons %||% "final ESEM unavailable", collapse = ", "),
      participant_based = FALSE, selection_conditioned = TRUE,
      evidence_scope = "selected semantic proxy"
    )
  }
  unidimensional_record <- if (identical(dimensionality_mode, "unidimensional")) {
    if (identical(unidimensional_diagnostics$status %||% "", "computed")) {
      .semantica_evidence_record(
        "computed", value = unidimensional_diagnostics, participant_based = FALSE,
        selection_conditioned = TRUE, evidence_scope = "selected one-factor semantic proxy"
      )
    } else {
      .semantica_evidence_record(
        "unavailable", value = unidimensional_diagnostics,
        reason = "One-factor structural proxy diagnostics were unavailable.",
        participant_based = FALSE, selection_conditioned = TRUE,
        evidence_scope = "selected one-factor semantic proxy"
      )
    }
  } else NULL
  evidence_records <- list(
    selection_semantic_context = selection_context_record,
    pfa = pfa_record,
    esem = esem_record,
    unidimensional_structure = unidimensional_record
  )

  nominal_proposals_drawn <- as.numeric(ants) * as.numeric(iteration)
  eligible_total_exact <- eligible_search_space$total_combinations_exact %||% NA_real_
  eligible_search_space$aco_effort <- list(
    iterations_completed = as.integer(iteration),
    ants_per_iteration = as.integer(ants),
    nominal_proposals_drawn = nominal_proposals_drawn,
    nominal_proposal_to_space_ratio = if (is.finite(eligible_total_exact) && eligible_total_exact > 0) {
      nominal_proposals_drawn / eligible_total_exact
    } else NA_real_,
    unique_space_coverage_not_claimed = TRUE,
    note = paste(
      "Nominal proposals are not unique search-space coverage because ACO can",
      "revisit candidate forms. The ratio is descriptive only and does not",
      "establish optimization adequacy or global optimality."
    )
  )

  objective_evidence_regime <- archive_selection_mode
  pfa_component_used <- identical(pfa_mode, "objective") && pfa_weight > 0 &&
    (is.finite(final_pfa_objective_score) || identical(pfa_failure_policy, "penalize"))

  # Headline evidence accounting is source-family based rather than metric-count
  # based. Semantic discrimination, PFA, ESEM, HTMT-like overlap, and semantic
  # DFI are intentionally grouped because they are transformations of the same
  # embedding-derived representation and therefore are not independent
  # validation replications.
  participant_response_available <- !is.null(response_validation) &&
    !is.null(response_validation$result)
  evidence_profile <- list(
    schema = .semantica_decision_policy()$evidence_schema_version,
    source_families = data.frame(
      family = c("theory_constraints", "embedding_semantic_structural", "participant_response"),
      status = c(
        "available",
        "available",
        if (participant_response_available) "available" else "not_supplied"
      ),
      independent_of_embedding = c(TRUE, FALSE, TRUE),
      selection_conditioned = c(FALSE, TRUE, participant_response_available),
      stringsAsFactors = FALSE
    ),
    embedding_subdiagnostics = c(
      "semantic_discrimination", "content_alignment", "pfa_proxy",
      "esem_proxy", "htmt_like_proxy", "semantic_dfi_when_enabled"
    ),
    analysis_source_family_count = 2L + as.integer(participant_response_available),
    independent_empirical_evidence_family_count = as.integer(participant_response_available),
    participant_response_family_available = participant_response_available,
    selection_conditioned = TRUE,
    dfi_evidence_role = "secondary_semantic_proxy_cutoff_sensitivity_not_independent_evidence",
    reference_n_role = "proxy_fit_sensitivity_anchor_not_observed_or_recommended_participant_sample_size",
    dependency_note = paste(
      "Embedding-derived semantic, PFA, ESEM, HTMT-like, and DFI quantities are",
      "subdiagnostics of one source family, not independent confirmations."
    ),
    interpretation = if (participant_response_available) {
      paste(
        "Independent participant-response evidence was supplied in addition to",
        "the embedding-semantic family; keep their inferential roles separate."
      )
    } else {
      paste(
        "The selected form is supported by theory constraints and one",
        "embedding-derived evidence family only; no participant-response evidence",
        "was supplied to this run."
      )
    }
  )

  objective_context <- list(
    value = archive_final_scores[best_archive_idx],
    type = "optimization_utility",
    decision_policy = .semantica_policy_metadata(),
    selection_mode = archive_selection_mode,
    search_guidance_status = search_guidance_status,
    evidence_regime = objective_evidence_regime,
    objective_schema = objective_schema,
    components = list(
      semantic = TRUE,
      pfa = pfa_component_used,
      esem = identical(objective_evidence_regime, "esem_guided") && esem_weight > 0
    ),
    evidence_families = list(
      embedding_semantic = c(
        "semantic",
        if (pfa_component_used) "pfa" else NULL,
        if (identical(objective_evidence_regime, "esem_guided") && esem_weight > 0) "esem" else NULL
      )
    ),
    evidence_dependency_note = paste(
      "Semantic discrimination, sample-free PFA, and semantic-proxy ESEM are different analyses of",
      "the same embedding-derived representation. Their agreement is not independent validation."
    ),
    independence_upgrade = list(
      status = "not_established_by_same_embedding_representation",
      requires = "held_out_empirical_calibration"
    ),
    grouped_components = list(
      semantic_content = list(
        score = final_semantic_objective_score,
        source_family = "embedding_semantic",
        role = "construct_alignment_discrimination_redundancy_coverage"
      ),
      proxy_structure = list(
        pfa_score = final_pfa_objective_score,
        esem_score = final_esem_result$score %||% NA_real_,
        source_family = "embedding_semantic",
        role = "structural_regularization_same_representation",
        aggregation = "existing_configurable_weights_not_claimed_as_independent_evidence"
      )
    ),
    weight_policy = .semantica_decision_policy()$policy_origin,
    threshold_policy = .semantica_decision_policy()$semantic_thresholds$provenance,
    htmt_objective_role = htmt_objective_role,
    dfi_evidence_role = "secondary_semantic_proxy_cutoff_sensitivity_not_independent_evidence",
    reference_n_role = "proxy_fit_sensitivity_anchor_not_observed_or_recommended_participant_sample_size",
    factor_relation_policy = if (!is.null(expected_factor_relations) && is.finite(nomological_weight) && nomological_weight > 0) {
      "declared_expected_factor_relations_included_in_semantic_utility"
    } else {
      "default_relative_separation_policy_no_claim_of_construct_ontological_independence"
    },
    proposal_score_schema = if (pfa_component_used) pfa_score_schema else semantic_score_schema,
    final_score_schema = if (identical(objective_evidence_regime, "esem_guided")) {
      if (isTRUE(elite_multicriteria_rerank)) "final-esem-multicriteria-rerank-v2" else "esem-guided-v1"
    } else {
      if (pfa_component_used) pfa_score_schema else semantic_score_schema
    },
    final_esem_admissible = isTRUE(final_esem_result$admissible),
    universal_quality_score = FALSE,
    cross_run_comparability = "conditional",
    comparison_scope = paste(
      "Compare objective magnitudes only when the objective definition, weights, candidate-pool construction,",
      "and evidence regime are held fixed. Fallback and ESEM-guided objectives are not universal scale-quality scores."
    ),
    participant_based = FALSE
  )

  compact_summary <- list(best_items = best_items, factor_assignment = factor_assignment, esem_syntax = final_syntax,
                          cfi = final_esem_result$cfi, rmsea = final_esem_result$rmsea, srmr = final_esem_result$srmr,
                          ave = final_esem_result$ave, factor_ave = final_esem_result$factor_ave,
                          htmt_max = final_esem_result$htmt_max,
                          dimensionality_mode = dimensionality_mode,
                          unidimensional_diagnostics = unidimensional_diagnostics,
                          structure_diagnostics = final_esem_result$structure_diagnostics,
                          semantic_score = sem_final$sem_score, semantic_objective_score = final_semantic_objective_score,
                          search_objective_score = final_search_objective_score,
                          proposal_objective_score = final_search_objective_score,
                          final_guided_objective_score = archive_final_scores[best_archive_idx],
                          pfa_score = final_pfa_score,
                          pfa_objective_score = final_pfa_objective_score,
                          pfa_objective_diagnostics = final_pfa_objective_diagnostics,
                          pfa_recovery_score = final_pfa_diagnostics$recovery_score %||% NA_real_,
                          pfa_salience_score = final_pfa_diagnostics$salience_score %||% NA_real_,
                          pfa_clarity_score = final_pfa_diagnostics$clarity_score %||% NA_real_,
                          pfa_partition_agreement_ari = final_pfa_diagnostics$partition_agreement_ari %||% NA_real_,
                          pfa_esem_discrepancy = pfa_esem_discrepancy,
                          esem_state = final_esem_state,
                          raw_sem_index = sem_final$raw_index,
                          selection_semantic_context = selection_semantic_context,
                          factor_semantic_diagnostics = factor_semantic_diagnostics,
                          objective_context = objective_context,
                          evidence_profile = evidence_profile,
                          evidence_records = evidence_records,
                          best_objective = archive_final_scores[best_archive_idx], cutoff_source = final_cutoff_source,
                          search_cutoff_source = search_cutoff_source,
                          reference_sample_size = reference_n_info,
                          semantic_n_sensitivity = if (!is.null(semantic_n_sensitivity_result)) semantic_n_sensitivity_result$summary else NULL,
                          dddfi_cutoffs = final_dddfi_cutoffs,
                          equivtest_diagnostic = final_equivtest_diagnostic,
                          response_validation = if (!is.null(response_validation)) response_validation$result else NULL,
                          recommended_validation_n = recommended_validation_n,
                          semantic_similarity_reduction = semantic_similarity_reduction,
                          candidate_counts = candidate_counts,
                          search_space = list(generated = generated_search_space, eligible = eligible_search_space),
                          pool_health = pool_health,
                          cohesion_retention = cohesion_retention,
                          within_target_method = attr(within_similarity_target_eff, "method") %||% within_target_method,
                          archive_selection_mode = archive_selection_mode,
                          esem_checkpoint_successes = esem_checkpoint_successes,
                          esem_checkpoint_failures = esem_checkpoint_failures,
                          search_guidance_status = search_guidance_status,
                          termination_reason = termination_reason,
                          total_iterations = iteration,
                          max_total_iter = max_total_iter,
                          max_esem_fits = max_esem_fits,
                          run_pfa_during_search = run_pfa_during_search,
                          pfa_every = pfa_every,
                          pfa_search_iterations = pfa_search_iterations,
                          pfa_search_attempts = pfa_search_attempts,
                          pfa_search_successes = pfa_search_successes,
                          history_mode = history_mode,
                           esem_attempts = esem_attempts,
                           esem_successes = esem_successes,
                           esem_failures = esem_failures,
                           evaluation_telemetry = evaluation_telemetry,
                           performance = performance,
                           duplicate_clusters = duplicate_clusters,
                           duplicate_feasibility = duplicate_feasibility,
                          dfi_mode = dfi_mode,
                          dfi_loading_source = dfi_loading_source,
                           semantic_pair_perturbation_stable = semantic_pair_perturbation_stability$stable,
                           split_half_stable = split_half_stability$stable,
                           warnings = if (length(run_warnings) > 0L) run_warnings else "none")

  # ---- Build detailed selected items table ----
  selected_items_detail <- NULL
  selected_item_metadata <- NULL
  text_col <- intersect(c("item_text", "text", "wording", "item_wording", "label"), names(df))
  if (length(text_col) > 0L) {
    # Use the ACO input metadata, not full-pipeline objects that are out of scope here.
    key_values <- if (!is.null(id_col)) as.character(df[[id_col]]) else rownames(df)
    txt_map <- setNames(as.character(df[[text_col[1L]]]), key_values)
    selected_items_detail <- data.frame(
      item_id   = best_items,
      factor    = unname(factor_assignment[best_items]),
      item_text = unname(txt_map[best_items]),
      stringsAsFactors = FALSE
    )
  }
  if (exists("semantica_standardize_item_metadata", mode = "function")) {
    selected_item_metadata <- tryCatch({
      meta_all <- semantica_standardize_item_metadata(
        df,
        id_col = if (!is.null(id_col)) id_col else NULL,
        dimension_col = type_col,
        item_col = if (length(text_col) > 0L) text_col[1L] else NULL
      )
      idx <- match(best_items, meta_all$ID)
      if (anyNA(idx)) stop("Selected item IDs are missing from metadata.")
      meta_all[idx, c("ID", "Dimension", "Facet", "item"), drop = FALSE]
    }, error = function(e) NULL)
  }

  result <- list(best_items = best_items, factor_assignment = factor_assignment,
                 selected_items_detail = selected_items_detail, selected_item_metadata = selected_item_metadata,
                 best_objective = archive_final_scores[best_archive_idx],
                 objective_context = objective_context,
                 evidence_profile = evidence_profile,
                 evidence_records = evidence_records,
                 dimensionality_mode = dimensionality_mode,
                 unidimensional_diagnostics = unidimensional_diagnostics,
                 selection_semantic_context = selection_semantic_context,
                 factor_semantic_diagnostics = factor_semantic_diagnostics,
                 esem_syntax = final_syntax, esem_fit = final_esem_fit,
                 esem_result = final_esem_result,
                 esem_state = final_esem_state,
                 semantic_index = sem_final$raw_index, semantic_score = sem_final$sem_score,
                 semantic_objective_score = final_semantic_objective_score,
                 search_objective_score = final_search_objective_score,
                 proposal_objective_score = final_search_objective_score,
                 final_guided_objective_score = archive_final_scores[best_archive_idx],
                 pfa_score = final_pfa_score,
                 pfa_diagnostics = final_pfa_diagnostics,
                 pfa_esem_discrepancy = pfa_esem_discrepancy,
                 pfa_objective_score = final_pfa_objective_score,
                 pfa_objective_diagnostics = final_pfa_objective_diagnostics,
                 mean_within = sem_final$mean_within, mean_between = sem_final$mean_between,
                 q90_within = sem_final$q90_within, q90_between = sem_final$q90_between,
                 within_similarity_target = within_similarity_target_eff,
                 within_target_loss = sem_final$within_target_loss,
                 within_factor_target_loss = sem_final$within_factor_target_loss,
                 duplicate_penalty = final_dup_penalty,
                 facet_coverage = final_facet_coverage,
                 facet_coverage_multiplier = final_facet_multiplier,
                 redundancy_penalty = sem_final$redundancy_penalty,
                 ave = final_esem_result$ave, factor_ave = final_esem_result$factor_ave,
                 ave_method = final_esem_result$ave_method, ave_warnings = final_esem_result$ave_warnings,
                 htmt_max = final_esem_result$htmt_max, htmt_violations = final_esem_result$htmt_violations,
                  loading_quality = final_esem_result$loading_quality,
                  esem_admissible = final_esem_result$admissible %||% FALSE,
                  esem_admissibility = final_esem_result$admissibility,
                  esem_alignment = final_esem_result$alignment,
                  structure_diagnostics = final_esem_result$structure_diagnostics,
                 item_structure_diagnostics = final_esem_result$structure_diagnostics$item_diagnostics %||% NULL,
                 dfi_cutoffs = dfi_cutoffs, heuristic_cutoffs = heuristic_cutoffs,
                 strict_dfi_cutoffs = strict_dfi_cutoffs, semantic_roc_cutoffs = semantic_roc_cutoffs,
                 semantic_approx_cutoffs = semantic_approx_cutoffs,
                 esem_parametric_cutoffs = esem_parametric_cutoffs,
                 final_dfi_cutoffs = final_dfi_cutoffs, final_dddfi_cutoffs = final_dddfi_cutoffs,
                 final_equivtest_diagnostic = final_equivtest_diagnostic,
                 response_validation = response_validation,
                 active_cutoffs = final_active_cutoffs, cutoff_source = final_cutoff_source,
                 search_active_cutoffs = search_active_cutoffs, search_cutoff_source = search_cutoff_source,
                 dfi_mode = dfi_mode, final_dfi_recalibrate = final_dfi_recalibrate,
                 reference_sample_size = reference_n_info,
                 semantic_reference_n = reference_n_info,
                 semantic_n_sensitivity = semantic_n_sensitivity_result,
                 recommended_validation_n = recommended_validation_n,
                 bootstrap_esem_params = bootstrap_params,
                 pfa_dfi_params = pfa_dfi_params,
                 dfi_population_params = dfi_population_params,
                 dfi_loading_source = dfi_loading_source,
                 embed_reliability = embed_reliability, residual_inflation = residual_inflation,
                 elite_archive = elite_archive,
                 evidence_archives = elite_archives,
                 evidence_archive_states = archive_states,
                 objective_schema = objective_schema,
                 decision_policy = .semantica_policy_metadata(),
                 elite_archive_scores = archive_final_scores, total_iterations = iteration,
                 termination_reason = termination_reason,
                 max_total_iter = max_total_iter, max_esem_fits = max_esem_fits,
                 run_pfa_during_search = run_pfa_during_search,
                 pfa_every = pfa_every,
                 pfa_search_iterations = pfa_search_iterations,
                 pfa_search_attempts = pfa_search_attempts,
                 pfa_search_successes = pfa_search_successes,
                 history_mode = history_mode,
                  esem_attempts = esem_attempts, esem_successes = esem_successes,
                  esem_failures = esem_failures, esem_error_log = esem_error_log,
                  evaluation_telemetry = evaluation_telemetry,
                  resource_plan = resource_plan,
                  performance = performance,
                  reproducibility = reproducibility_metadata,
                 pheromone = pheromone, model_info = model_info, eligible_items = eligible.items,
                 selection_guard_audit = guard_audit,
                 candidate_counts = candidate_counts,
                 search_space = list(generated = generated_search_space, eligible = eligible_search_space),
                 pool_health = pool_health,
                 cohesion_retention = cohesion_retention,
                 within_target_method = attr(within_similarity_target_eff, "method") %||% within_target_method,
                 within_target_source = attr(within_similarity_target_eff, "source"),
                 archive_selection_mode = archive_selection_mode,
                 esem_checkpoint_successes = esem_checkpoint_successes,
                 esem_checkpoint_failures = esem_checkpoint_failures,
                 search_guidance_status = search_guidance_status,
                 duplicate_clusters = duplicate_clusters, duplicate_cluster_id = duplicate_cluster_id,
                 duplicate_feasibility = duplicate_feasibility,
                  esem_cor_matrix = final_esem_cor,
                  semantic_similarity_reduction = semantic_similarity_reduction,
                  semantic_pair_perturbation_stability = semantic_pair_perturbation_stability,
                  semantic_resampling_stability = semantic_resampling_stability,
                  split_half_stability = split_half_stability,
                  solution_history = solution_history_list, summary = compact_summary)
  class(result) <- c("semantica_result", "list")
  if (verbose) print_semantica_phase3_summary(result)
  result
}

# =================================================================
# 13-B  MULTI-SEED WRAPPER
# =================================================================
#' Run ACO_with_ESEM across multiple random seeds
#'
#' @param seeds Integer vector of seeds.
#' @param cosine_sim_matrix,df,i.per.f Passed to ACO_with_ESEM.
#' @param verbose_seeds Print seed-level progress.
#' @param x A `semantica_multi_seed_result` or its summary, as appropriate for the S3 method.
#' @param object A `semantica_multi_seed_result` passed to `summary()`.
#' @param ... Additional arguments passed to `ACO_with_ESEM()` by the runner; S3 presentation methods currently ignore additional arguments.
#' @return A `semantica_multi_seed_result` list with consensus items, item frequencies, seed-level optimization
#'   utilities and their evidence-regime comparability metadata, ESEM-scoring
#'   telemetry, a selection matrix, and pairwise Jaccard agreement across
#'   successful seeds.
#' @export
#' @examples
#' \dontrun{
#' multi <- run_multi_seed_semantica(
#'   seeds = 1:3,
#'   cosine_sim_matrix = wrapped$cosine_sim_matrix,
#'   df = wrapped$df,
#'   i.per.f = c(Clarity = 3L, Flexibility = 3L),
#'   ants = 30L,
#'   max.iter = 10L,
#'   use_parallel = FALSE,
#'   verbose = FALSE
#' )
#' }
run_multi_seed_semantica <- function(seeds = 1:5, cosine_sim_matrix, df, i.per.f, verbose_seeds = TRUE, ...) {
  seeds <- as.integer(seeds)
  if (length(seeds) == 0L || any(!is.finite(seeds) | seeds < 0L)) {
    stop("'seeds' must contain nonnegative integers.")
  }
  dots <- list(...)
  if ("seed" %in% names(dots)) {
    warning(
      "The per-run 'seed' argument is controlled by 'seeds' and was ignored.",
      call. = FALSE
    )
    dots$seed <- NULL
  }
  n_seeds <- length(seeds); all_results <- vector("list", n_seeds)
  for (s_idx in seq_along(seeds)) {
    seed <- seeds[s_idx]
    if (verbose_seeds) cat(sprintf("\nMULTI-SEED RUN %d/%d (seed = %d)\n", s_idx, n_seeds, seed))
    all_results[[s_idx]] <- tryCatch(
      do.call(
        ACO_with_ESEM,
        c(
          list(
            cosine_sim_matrix = cosine_sim_matrix,
            df = df,
            i.per.f = i.per.f,
            seed = seed
          ),
          dots
        )
      ),
      error = function(e) {
        message(sprintf("[Seed %d] ACO failed: %s", seed, conditionMessage(e)))
        NULL
      }
    )
  }
  valid <- Filter(Negate(is.null), all_results); n_ok <- length(valid)
  if (n_ok == 0L) { warning("All seeds failed. Returning NULL."); return(NULL) }

  all_selected <- unlist(lapply(valid, function(r) r$best_items))
  item_freq <- sort(table(all_selected), decreasing = TRUE)
  score_dist <- vapply(valid, function(r) r$best_objective, numeric(1L))
  proposal_dist <- vapply(valid, function(r) r$proposal_objective_score %||% r$search_objective_score %||% NA_real_, numeric(1L))
  normalize_objective_regime <- function(r) {
    regime <- r$objective_context$evidence_regime %||% r$search_guidance_status %||% "legacy_unknown"
    if (identical(regime, "semantic_only_requested")) regime <- "semantic_only"
    if (identical(regime, "pfa_guided")) regime <- "pfa_semantic_guided"
    regime
  }
  objective_regimes <- vapply(valid, normalize_objective_regime, character(1L))
  objective_regime_consistent <- length(unique(objective_regimes)) == 1L
  successful_seeds <- seeds[!vapply(all_results, is.null, logical(1L))]
  universe <- sort(unique(all_selected))
  selection_matrix <- vapply(
    valid,
    function(r) as.integer(universe %in% r$best_items),
    integer(length(universe))
  )
  rownames(selection_matrix) <- universe
  colnames(selection_matrix) <- paste0("seed_", successful_seeds)
  pairwise_jaccard <- if (n_ok >= 2L) {
    seed_pairs <- utils::combn(seq_len(n_ok), 2L)
    vals <- apply(seed_pairs, 2L, function(idx) {
      a <- valid[[idx[1L]]]$best_items
      b <- valid[[idx[2L]]]$best_items
      length(intersect(a, b)) / length(union(a, b))
    })
    names(vals) <- apply(seed_pairs, 2L, function(idx) {
      paste(colnames(selection_matrix)[idx], collapse = "_vs_")
    })
    vals
  } else {
    numeric(0L)
  }
  esem_telemetry <- data.frame(
    seed = successful_seeds,
    guidance = vapply(valid, function(r) r$search_guidance_status %||% "legacy/unknown", character(1L)),
    objective_regime = objective_regimes,
    attempted = vapply(valid, function(r) as.integer(r$esem_attempts %||% 0L), integer(1L)),
    succeeded = vapply(valid, function(r) as.integer(r$esem_successes %||% ((r$esem_attempts %||% 0L) - (r$esem_failures %||% 0L))), integer(1L)),
    failed = vapply(valid, function(r) as.integer(r$esem_failures %||% 0L), integer(1L)),
    final_objective = score_dist,
    proposal_objective = proposal_dist,
    stringsAsFactors = FALSE
  )
  majority_threshold <- ceiling(n_ok / 2)
  consensus_items <- names(item_freq[item_freq >= majority_threshold])

  # Equifinality summaries: expose how many distinct best-found item sets occur
  # without treating the modal set as a global optimum. Item order is ignored.
  solution_keys <- vapply(valid, function(r) {
    paste(sort(unique(as.character(r$best_items %||% character()))), collapse = "\r")
  }, character(1L))
  unique_solution_keys <- unique(solution_keys)
  unique_solutions <- lapply(unique_solution_keys, function(key) {
    if (!nzchar(key)) character() else strsplit(key, "\r", fixed = TRUE)[[1L]]
  })
  names(unique_solutions) <- paste0("solution_", seq_along(unique_solutions))
  objective_dispersion <- list(
    sd = if (length(score_dist) > 1L) stats::sd(score_dist) else 0,
    iqr = if (length(score_dist) > 1L) stats::IQR(score_dist) else 0,
    range = if (length(score_dist)) diff(range(score_dist)) else NA_real_,
    min = if (length(score_dist)) min(score_dist) else NA_real_,
    median = if (length(score_dist)) stats::median(score_dist) else NA_real_,
    max = if (length(score_dist)) max(score_dist) else NA_real_
  )

  # Factor-specific inclusion frequency is derived only from explicit factor
  # assignments attached to successful results; no factor labels are inferred.
  factor_frequency_rows <- list()
  ff_idx <- 0L
  for (r in valid) {
    assignment <- r$factor_assignment %||% NULL
    if (is.null(assignment) || is.null(names(assignment))) next
    selected <- intersect(as.character(r$best_items %||% character()), names(assignment))
    for (id in selected) {
      ff_idx <- ff_idx + 1L
      factor_frequency_rows[[ff_idx]] <- data.frame(
        item_id = id, factor = as.character(assignment[[id]]), stringsAsFactors = FALSE
      )
    }
  }
  factor_inclusion_frequency <- if (length(factor_frequency_rows)) {
    ff <- do.call(rbind, factor_frequency_rows)
    counts <- stats::aggregate(rep(1L, nrow(ff)), by = list(item_id = ff$item_id, factor = ff$factor), FUN = sum)
    names(counts)[[3L]] <- "count"
    counts$frequency <- counts$count / n_ok
    counts[order(counts$factor, -counts$frequency, counts$item_id), , drop = FALSE]
  } else {
    data.frame(item_id = character(), factor = character(), count = integer(), frequency = numeric(), stringsAsFactors = FALSE)
  }

  objective_comparability <- list(
    comparable_across_seeds = objective_regime_consistent,
    evidence_regimes = stats::setNames(objective_regimes, successful_seeds),
    note = if (objective_regime_consistent) {
      paste(
        "Optimization-utility dispersion is descriptively comparable across these seeds because the recorded evidence regime is constant.",
        "Interpretation still assumes the same objective definition, weights, and candidate-pool construction."
      )
    } else {
      paste(
        "Optimization-utility magnitudes span different evidence regimes and should not be interpreted as a common cross-seed quality scale.",
        "Use item-set stability and regime-specific diagnostics instead."
      )
    }
  )

  stability_scope <- "optimizer_only_frozen_item_pool_and_similarity_matrix"
  stability_note <- paste(
    "This diagnostic varies only the ACO seed while holding the candidate item",
    "pool and supplied similarity matrix fixed. It does not estimate LLM generation,",
    "embedding-model, representation, or end-to-end pipeline stochasticity."
  )

  if (verbose_seeds) {
    cat(sprintf("\n  Seeds run      : %d / %d succeeded\n", n_ok, n_seeds))
    cat("  Stability scope : optimizer-only, frozen item pool and similarity matrix\n")
    cat(sprintf("  Optimization util.: min=%.4f | median=%.4f | max=%.4f\n", min(score_dist), median(score_dist), max(score_dist)))
    if (!objective_regime_consistent) {
      cat(sprintf("  Utility regimes : %s\n", paste(unique(objective_regimes), collapse = ", ")))
      cat("  [!] Cross-seed utility magnitudes are not on one evidence regime; do not read their dispersion as a universal quality scale.\n")
    }
    if (any(is.finite(proposal_dist))) {
      cat(sprintf("  Proposal obj.  : min=%.4f | median=%.4f | max=%.4f\n",
                  min(proposal_dist, na.rm = TRUE), median(proposal_dist, na.rm = TRUE),
                  max(proposal_dist, na.rm = TRUE)))
    }
    if (length(pairwise_jaccard) > 0L) {
      cat(sprintf("  Item agreement : pairwise Jaccard median=%.3f | min=%.3f\n",
                  median(pairwise_jaccard), min(pairwise_jaccard)))
    }
    cat(sprintf("  ESEM scoring   : %d / %d attempted solutions succeeded\n",
                sum(esem_telemetry$succeeded), sum(esem_telemetry$attempted)))
    cat("\n  Item selection frequencies:\n")
    for (nm in names(item_freq)) cat(sprintf("    %-20s: %d / %d runs (%.0f%%)\n", nm, item_freq[nm], n_ok, 100 * item_freq[nm] / n_ok))
    cat(sprintf("\n  Consensus items (>= %d/%d runs): %s\n", majority_threshold, n_ok, if (length(consensus_items) > 0L) paste(consensus_items, collapse = ", ") else "(none)"))
  }
  out <- list(
    item_frequencies = item_freq,
    score_distribution = score_dist,
    proposal_score_distribution = proposal_dist,
    consensus_items = consensus_items,
    selection_matrix = selection_matrix,
    pairwise_jaccard = pairwise_jaccard,
    mean_pairwise_jaccard = if (length(pairwise_jaccard) > 0L) mean(pairwise_jaccard) else NA_real_,
    n_unique_solutions = length(unique_solutions),
    unique_solutions = unique_solutions,
    objective_dispersion = objective_dispersion,
    objective_comparability = objective_comparability,
    factor_inclusion_frequency = factor_inclusion_frequency,
    esem_telemetry = esem_telemetry,
    requested_seeds = seeds,
    successful_seeds = successful_seeds,
    stability_scope = stability_scope,
    stability_note = stability_note,
    reproducibility = list(
      rng_kind = RNGkind(),
      master_seeds = seeds,
      successful_seeds = successful_seeds,
      optimizer_argument_names = names(dots)
    ),
    n_successful = n_ok,
    all_results = all_results
  )
  class(out) <- c("semantica_multi_seed_result", "list")
  out
}

# =================================================================
# 14, 15, 16  REPORTING & INSPECTION UTILITIES
# =================================================================
#' Print a SEMANTICA results report
#' @param result Output from ACO_with_ESEM.
#' @param digits Decimal places for formatting.
#' @return Invisibly returns `result`.
#' @export
#' @examples
#' \dontrun{
#' report_semantica_v2(result, digits = 3L)
#' }
report_semantica_v2 <- function(result, digits = 4) {
  is_unidimensional <- identical(result$dimensionality_mode %||% "", "unidimensional") ||
    length(unique(result$factor_assignment %||% character(0))) == 1L
  cat("\n===========================================================-\n")
  cat("|          SEMANTICA -- RESULTS REPORT (full-ESEM)           |\n")
  cat("============================================================\n\n")
  cat("-- SELECTED ITEMS ------------------------------------------\n")
  for (f in unique(result$factor_assignment)) {
    f_items <- names(result$factor_assignment[result$factor_assignment == f])
    cat(sprintf("  %-28s: %s\n", f, paste(f_items, collapse = ", ")))
  }
  esem_syn <- result$esem_syntax
  cat("\n-- FINAL ESEM SYNTAX -------------------------------------\n", esem_syn, "\n")
  cat("\n-- SEMANTIC-PROXY FIT vs REFERENCE VALUES ----------------\n")
  cat("  Reference comparisons are screening anchors, not participant-data validity PASS/FAIL tests.\n")
  cr <- result$esem_result; ac <- result$active_cutoffs
  es_state <- result$esem_state %||% NULL
  if (!is.null(es_state)) {
    cat(sprintf("  ESEM technical state : %s\n", es_state$technical_state %||% "unavailable"))
    cat(sprintf("  ESEM structural qual.: %s\n", es_state$structural_quality %||% "not_assessed"))
    if (identical(es_state$structural_quality %||% "", "admissible_but_structurally_mixed")) {
      cat("  [!] ESEM is technically admissible but structurally mixed; do not read admissibility as clean intended structure.\n")
    }
  }
  fmt_line <- function(name, val, cutoff, direction = " >= ") {
    reference_status <- .semantica_proxy_reference_status(
      val, cutoff, direction = if (direction == " >= ") "higher" else "lower"
    )
    cutoff_txt <- if (is.na(cutoff)) "unavailable" else sprintf("%.3f", cutoff)
    cat(sprintf("  %-8s = %s  (%s %s)  [%s]\n", name, if (is.na(val)) "  NA   " else sprintf("%.4f", val), direction, cutoff_txt, reference_status))
  }
  if (!is.null(result$final_dddfi_cutoffs)) {
    dd <- result$final_dddfi_cutoffs
    cat(sprintf("  DDDFI target     : %s MAD (approximate-fit diagnostic)\n", dd$target_label))
    fmt_line("CFI", dd$observed$cfi, dd$cfi, " >= ")
    fmt_line("RMSEA", dd$observed$rmsea, dd$rmsea, " <= ")
    fmt_line("RMSEA90", dd$observed$rmsea_ci, dd$rmsea_ci, " <= ")
    cat(sprintf("  SRMR     = %.4f  (descriptive; no DDDFI SRMR cutoff)\n", cr$srmr))
    cat(sprintf("  Search reference : CFI >= %.3f | RMSEA <= %.3f | SRMR <= %.3f\n", ac$cfi, ac$rmsea, ac$srmr))
  } else {
    fmt_line("CFI", cr$cfi, ac$cfi, " >= "); fmt_line("TLI", cr$tli, ac$tli, " >= ")
    fmt_line("RMSEA", cr$rmsea, ac$rmsea, " <= "); fmt_line("SRMR", cr$srmr, ac$srmr, " <= ")
  }
  cat(sprintf("  DFI mode         : %s\n", result$dfi_mode %||% "unknown"))
  cat(sprintf("  Cutoff source    : %s\n", result$cutoff_source))
  cat(sprintf("  DFI loading src  : %s\n", result$dfi_loading_source %||% "unknown"))
  cat(sprintf("  Search guidance  : %s\n", result$search_guidance_status %||% "legacy/unknown"))
  fix_b <- !is.null(result$embed_reliability) && result$embed_reliability < 1.0
  cat(sprintf("  Fix B (attenuat.): %s\n", if (fix_b) sprintf("ACTIVE (rho_tt=%.2f, delta=%.2f)", result$embed_reliability, result$residual_inflation) else "disabled"))
  ref_n <- result$reference_sample_size %||% result$model_info$reference_n
  if (!is.null(ref_n)) {
    cat(sprintf("  Proxy reference N: %d for RMSEA-power semantic fit sensitivity (%s; df=%.1f, RMSEA %.3f vs %.3f, power %.2f)\n",
                ref_n$used_n_obs %||% ref_n$n_obs,
                if (isTRUE(ref_n$auto)) "auto RMSEA-power" else ref_n$method %||% "user supplied",
                ref_n$df %||% NA_real_,
                ref_n$rmsea_null %||% NA_real_,
                ref_n$rmsea_alt %||% NA_real_,
                ref_n$power %||% NA_real_))
    cat("  Proxy N role     : embedding-correlation fit/DFI anchor; not a respondent validation N.\n")
    if (isTRUE(ref_n$low_df_warning)) cat("  Proxy N note     : low approximate EFA df; read N-sensitivity with care.\n")
  }
  if (!is.null(result$semantic_n_sensitivity) && isTRUE(result$semantic_n_sensitivity$available)) {
    sns <- result$semantic_n_sensitivity
    sm <- sns$summary %||% list()
    cat(sprintf("  Proxy N grid     : %s | ESEM refits %d/%d succeeded\n",
                paste(sns$n_grid, collapse = ", "),
                sm$successful_fits %||% 0L,
                sm$requested_fits %||% length(sns$n_grid)))
    if (!is.null(sm$structurally_stable) && !is.na(sm$structurally_stable)) {
      if (is_unidimensional) {
        cat(sprintf("  Proxy N structure: %s | median primary range=%s (one-factor proxy)\n",
                    if (isTRUE(sm$structurally_stable)) "stable across anchors" else "changed across anchors",
                    if (is.finite(sm$median_primary_loading_range)) sprintf("%.3f", sm$median_primary_loading_range) else "NA"))
      } else {
        cat(sprintf("  Proxy N structure: %s | dominance floor=%s | median primary range=%s\n",
                    if (isTRUE(sm$structurally_stable)) "stable across anchors" else "changed across anchors",
                    if (is.finite(sm$dominant_factor_agreement_floor)) sprintf("%.3f", sm$dominant_factor_agreement_floor) else "NA",
                    if (is.finite(sm$median_primary_loading_range)) sprintf("%.3f", sm$median_primary_loading_range) else "NA"))
      }
    }
  }
  if (!is.null(result$recommended_validation_n)) {
    rvn <- result$recommended_validation_n
    if (isTRUE(rvn$available) && is.finite(rvn$recommended_n)) {
      cat(sprintf("  Recommended validation N: %d (PFA-informed Monte Carlo planning diagnostic)\n", rvn$recommended_n))
    }
  }
  if (!is.null(result$response_validation) && !is.null(result$response_validation$result)) {
    rv <- result$response_validation$result
    if (isTRUE(rv$converged)) {
      cat(sprintf("  Response-data fit: CFI=%.4f | RMSEA=%.4f | SRMR=%.4f | AVE=%.4f\n",
                  rv$cfi, rv$rmsea, rv$srmr, rv$ave))
    }
  }
  if (!is.null(cr$score_decomp)) {
    cat("\n-- SCORE DECOMPOSITION -----------------------------------\n")
    d <- cr$score_decomp
    cat(sprintf("  CFI ratio score  : %.4f  | logistic: %.4f\n", d$cfi_s, d$cfi_logistic))
    cat(sprintf("  RMSEA ratio score: %.4f  | logistic: %.4f\n", d$rmsea_s, d$rmsea_logistic))
    cat(sprintf("  SRMR ratio score : %.4f  | logistic: %.4f\n", d$srmr_s, d$srmr_logistic))
    cat(sprintf("  AVE score (dom.) : %.4f\n", d$ave_score))
    if (!is.null(d$score_mode)) cat(sprintf("  Score mode       : %s\n", d$score_mode))
    if (!is.null(d$structure_component)) cat(sprintf("  Structure comp.  : %.4f\n", d$structure_component))
    cat(sprintf("  Loading quality  : %.4f\n", d$loading_quality))
    if (is_unidimensional) {
      cat("  HTMT penalty     : N/A (one-factor model)\n")
    } else {
      cat(sprintf("  HTMT penalty     : %.4f\n", d$htmt_penalty))
    }
    cat(sprintf("  Base score       : %.4f\n", d$base_score))
    cat(sprintf("  Final ESEM score : %.4f\n", d$final_score))
  }
  cat("\n-- VALIDITY CRITERIA -------------------------------------\n")
  if (!is.null(result$response_validation) && !is.null(result$response_validation$result)) {
    fmt_line("AVE (dom.)", result$ave, 0.50, " >= ")
  } else {
    cat(sprintf("  AVE (dom.)  = %s  (semantic-proxy descriptive; .50 is a response-data benchmark)\n",
                if (is.na(result$ave)) "  NA   " else sprintf("%.4f", result$ave)))
    if (!is.na(result$ave) && result$ave < 0.50) {
      if (is_unidimensional) {
        cat("  AVE note    : below the conventional response-data benchmark; interpret with one-factor loadings, residual reproduction, eigen dominance, and later participant-data validation.\n")
      } else {
        cat("  AVE note    : below conventional response-data AVE; interpret with PFA recovery, loading dominance, HTMT, and later response-data validation.\n")
      }
    }
  }
  if (!is.null(result$factor_ave) && length(result$factor_ave) > 0L) {
    cat("  Factor AVE (dominant ESEM loadings):\n")
    for (nm in names(result$factor_ave)) cat(sprintf("    %-28s %.4f\n", nm, result$factor_ave[[nm]]))
  }
  if (!is.null(result$ave_warnings) && length(result$ave_warnings) > 0L) {
    cat("  AVE diagnostics:\n")
    for (w in result$ave_warnings) cat(sprintf("    [!] %s\n", w))
  }
  if (is_unidimensional) {
    cat("  HTMT max        : N/A (requires at least two constructs)\n")
    cat("  HTMT violations : N/A\n")
  } else {
    fmt_line("HTMT max", result$htmt_max, result$model_info$htmt_threshold, " <= ")
    cat(sprintf("  HTMT violations: %d\n", if (is.infinite(result$htmt_violations)) 999L else as.integer(result$htmt_violations)))
  }
  cat(sprintf("  Loading quality (dominant): %.4f\n", result$loading_quality))
  if (!is.null(result$structure_diagnostics)) {
    sdg <- result$structure_diagnostics
    pct <- function(x) if (is.finite(x)) sprintf("%.1f%%", 100 * x) else "NA"
    num <- function(x) if (is.finite(x)) sprintf("%.4f", x) else "NA"
    cat("  ESEM structure diagnostics:\n")
    cat(sprintf("    Dominant loading mean/median/min: %s / %s / %s\n",
                num(sdg$mean_primary_loading), num(sdg$median_primary_loading), num(sdg$min_primary_loading)))
    if (is_unidimensional) {
      cat("    Comparative dominance/cross-loadings: N/A (one-factor model)\n")
      cat(sprintf("    Residual |r| mean/q95/max      : %s / %s / %s\n",
                  num(sdg$mean_abs_residual), num(sdg$q95_abs_residual), num(sdg$max_abs_residual)))
      cat(sprintf("    Centered residual max |.|      : %s\n", num(sdg$max_abs_centered_residual)))
    } else {
      cat(sprintf("    Correct dominant factor        : %s\n", pct(sdg$correct_dominance)))
      cat(sprintf("    Simple-structure items         : %s\n", pct(sdg$simple_structure)))
      cat(sprintf("    Max cross-loading mean/q90/max : %s / %s / %s\n",
                  num(sdg$mean_max_cross_loading), num(sdg$q90_max_cross_loading), num(sdg$max_cross_loading)))
      cat(sprintf("    Item complexity mean/max       : %s / %s\n",
                  num(sdg$mean_complexity), num(sdg$max_complexity)))
      cat(sprintf("    Residual |r| mean/q95/max      : %s / %s / %s\n",
                  num(sdg$mean_abs_residual), num(sdg$q95_abs_residual), num(sdg$max_abs_residual)))
      fd <- sdg$factor_diagnostics %||% NULL
      if (is.data.frame(fd) && nrow(fd) > 0L && any(is.finite(fd$simple_structure))) {
        z <- fd[is.finite(fd$simple_structure), , drop = FALSE]
        weak <- z[which.min(z$simple_structure), , drop = FALSE]
        cat(sprintf("    Weakest intended factor         : %s | dominance %s | simple structure %s\n",
                    weak$factor[[1L]], pct(weak$correct_dominance[[1L]]), pct(weak$simple_structure[[1L]])))
      }
    }
  }
  cat("\n-- SEMANTIC PROPERTIES -----------------------------------\n")
  cat(sprintf("  Sigmoid sem. score : %.4f\n", result$semantic_score))
  if (!is.null(result$semantic_objective_score)) cat(sprintf("  Semantic objective : %.4f\n", result$semantic_objective_score))
  if (!is.null(result$proposal_objective_score)) cat(sprintf("  Proposal utility   : %.4f\n", result$proposal_objective_score))
  if (!is.null(result$final_guided_objective_score)) cat(sprintf("  Guided utility     : %.4f\n", result$final_guided_objective_score))
  cat(sprintf("  Raw similarity idx : %.4f\n", result$semantic_index))
  cat(sprintf("  Mean within-factor : %.4f\n", result$mean_within))
  if (is_unidimensional) {
    cat("  Mean between-factor: N/A (one-factor model)\n")
  } else {
    cat(sprintf("  Mean between-factor: %.4f\n", result$mean_between))
  }
  sel_ctx <- result$selection_semantic_context %||% NULL
  if (!is.null(sel_ctx)) {
    if (is.finite(sel_ctx$pool$estimate %||% NA_real_)) {
      cat(sprintf("  Pool superiority A : %.4f\n", sel_ctx$pool$estimate))
    }
    if (is.finite(sel_ctx$selected$estimate %||% NA_real_)) {
      cat(sprintf("  Selected A          : %.4f (post-selection descriptive)\n", sel_ctx$selected$estimate))
    }
    if (is.finite(sel_ctx$stochastic_superiority_gain %||% NA_real_)) {
      cat(sprintf("  Selection change A : %+.4f\n", sel_ctx$stochastic_superiority_gain))
    }
    factor_sem <- sel_ctx$selected_factor_diagnostics %||% result$factor_semantic_diagnostics %||% NULL
    if (is.data.frame(factor_sem) && nrow(factor_sem) > 0L && any(is.finite(factor_sem$gap))) {
      z <- factor_sem[is.finite(factor_sem$gap), , drop = FALSE]
      weak <- z[which.min(z$gap), , drop = FALSE]
      cat(sprintf("  Weakest factor     : %s | gap %+.4f | A %s\n",
                  weak$factor[[1L]], weak$gap[[1L]],
                  if (is.finite(weak$stochastic_superiority[[1L]])) sprintf("%.4f", weak$stochastic_superiority[[1L]]) else "NA"))
    }
  }
  if (!is.null(result$q90_within)) cat(sprintf("  Q90 within-factor  : %.4f\n", result$q90_within))
  if (!is.null(result$q90_between)) cat(sprintf("  Q90 between-factor : %.4f\n", result$q90_between))
  if (!is.null(result$within_target_loss)) cat(sprintf("  Within target loss : %.4f\n", result$within_target_loss))
  if (!is.null(result$duplicate_penalty)) cat(sprintf("  Duplicate penalty  : %.4f\n", result$duplicate_penalty))
  if (!is.null(result$facet_coverage) && is.finite(result$facet_coverage)) cat(sprintf("  Facet coverage     : %.4f\n", result$facet_coverage))
  cat(sprintf("  Redundancy penalty : %.4f\n", result$redundancy_penalty))
  stab <- result$semantic_pair_perturbation_stability %||%
    result$split_half_stability
  if (!is.null(stab)) {
    if (!is.na(stab$stable)) cat(sprintf(
      "  Pair-perturbation heuristic: diff=%.4f [%s; boundary %.2f, uncalibrated]\n",
      stab$difference,
      if (isTRUE(stab$stable)) "HEURISTICALLY STABLE" else "HEURISTICALLY UNSTABLE",
      stab$heuristic_threshold %||% 0.10
    ))
  }
  if (is_unidimensional) {
    cat("\n-- SAMPLE-FREE PFA DIAGNOSTICS --------------------------\n")
    cat("  not applicable: partition/factor-recovery PFA requires at least two intended factors.\n")
  } else if (!is.null(result$pfa_diagnostics)) {
    pfa <- result$pfa_diagnostics
    cat("\n-- SAMPLE-FREE PFA DIAGNOSTICS --------------------------\n")
    if (isTRUE(pfa$available)) {
      cat(sprintf("  PFA score          : %.4f\n", pfa$score))
      cat(sprintf("  PFA role           : %s\n",
                  if (isTRUE(result$model_info$run_pfa_during_search)) {
                    sprintf("selection objective every %d iteration(s)", result$model_info$pfa_every %||% 1L)
                  } else if (identical(result$model_info$pfa_mode, "objective")) {
                    "final objective only"
                  } else {
                    "descriptive only"
                  }))
      if (!is.null(result$pfa_search_iterations)) {
        cat(sprintf("  Search-time PFA    : %d iteration(s), %d / %d available proposal diagnostics\n",
                    result$pfa_search_iterations %||% 0L,
                    result$pfa_search_successes %||% 0L,
                    result$pfa_search_attempts %||% 0L))
      }
      if (!is.null(result$pfa_objective_score) && is.finite(result$pfa_objective_score)) {
        cat(sprintf("  Objective PFA score: %.4f (%s extraction)\n",
                    result$pfa_objective_score, result$model_info$pfa_extraction %||% "search"))
      }
      if (is.finite(pfa$partition_agreement_ari %||% NA_real_)) {
        cat(sprintf("  Partition agreement ARI : %.4f (chance-adjusted)\n", pfa$partition_agreement_ari))
      }
      cat(sprintf("  Continuous PFA geometry: partition %.4f | primary %.4f | margin %.4f\n",
                  pfa$partition_quality_score %||% pfa$recovery_score,
                  pfa$continuous_salience_score %||% pfa$mean_primary_loading,
                  pfa$continuous_clarity_score %||% pfa$mean_loading_margin))
      cat(sprintf("  Threshold attainment   : presence %.4f | loading-ref %.4f | margin-ref %.4f (descriptive)\n",
                  pfa$recovery_score, pfa$salience_score, pfa$clarity_score))
      cat("  Presence note      : factor presence alone is not item-level partition accuracy; finite ARI now qualifies the continuous partition component.\n")
      cat(sprintf("  Mean primary/margin: %.4f / %.4f\n",
                  pfa$mean_primary_loading, pfa$mean_loading_margin))
      cat(sprintf("  Extraction/rotation: %s / %s\n", pfa$extraction, pfa$rotation))
      if (length(pfa$missing_factors) > 0L) {
        cat(sprintf("  Missing factors    : %s\n", paste(pfa$missing_factors, collapse = ", ")))
      }
    } else {
      cat(sprintf("  unavailable: %s\n", pfa$note %||% "diagnostic failed"))
    }
  }
  cat("\n-- ACO METADATA ------------------------------------------\n")
  cat(sprintf("  Total iterations   : %d\n", result$total_iterations))
  esem_att <- result$esem_attempts; esem_fai <- result$esem_failures
  if (is.null(esem_att)) esem_att <- 0L
  if (is.null(esem_fai)) esem_fai <- 0L
  esem_ok <- result$esem_successes %||% (esem_att - esem_fai)
  cat(sprintf("  ESEM successes     : %d / %d\n", esem_ok, esem_att))
  cat(sprintf("  Elite archive size : %d\n", length(result$elite_archive)))
  cat(sprintf("  Optimization util. : %.4f\n", result$best_objective))
  if (!is.null(result$objective_context)) {
    cat(sprintf("  Objective regime   : %s\n", result$objective_context$evidence_regime %||% "unknown"))
    if (grepl("fallback", result$objective_context$evidence_regime %||% "", fixed = TRUE)) {
      cat("  [!] Fallback objective: do not compare as an ESEM-guided scale-quality score.\n")
    }
  }
  if (!is.null(result$duplicate_clusters) && result$duplicate_clusters$n_clusters > 0L) {
    cat(sprintf("  Duplicate clusters : %d clusters / %d items guarded\n",
                result$duplicate_clusters$n_clusters,
                result$duplicate_clusters$n_items_clustered))
  }
  if (!is.null(result$summary$warnings) && !identical(result$summary$warnings, "none")) {
    cat("\n-- WARNINGS --------------------------------------------\n")
    for (w in result$summary$warnings) cat(sprintf("  [!] %s\n", w))
  }
  cat("-----------------------------------------------------------\n\n")
  invisible(result)
}

#' Inspect elite archive solutions
#' @param result Output from ACO_with_ESEM.
#' @param top_n Number of top solutions to display.
#' @return Invisibly returns the elite archive list.
#' @export
#' @examples
#' \dontrun{
#' inspect_elite_archive(result, top_n = 5L)
#' }
inspect_elite_archive <- function(result, top_n = 5) {
  archive <- result$elite_archive; scores <- result$elite_archive_scores
  cat(sprintf("\n-- TOP-%d ELITE SOLUTIONS ----------------------------\n", min(top_n, length(archive))))
  for (i in seq_len(min(top_n, length(archive)))) {
    entry <- archive[[i]]
    cat(sprintf("\n  [%d]  Final score: %.4f  (ACO iter: %d; %s)\n",
                i, scores[i], entry$iteration,
                entry$score_type %||% if (isTRUE(entry$do_esem)) "ESEM-scored" else "semantic"))
  }
  invisible(archive)
}

#' Inspect solution evaluation history
#' @param result Output from ACO_with_ESEM.
#' @param top_n Number of top solutions to display.
#' @param sort_by Column to sort by ("total", "sem_score", "pfa_score",
#'   "search_score", "esem_score").
#' @return Invisibly returns a data frame with all solution-history rows.
#' @export
#' @examples
#' \dontrun{
#' inspect_solution_history(result, top_n = 10L, sort_by = "total")
#' }
inspect_solution_history <- function(result, top_n = 10, sort_by = "total") {
  hist <- result$solution_history
  if (is.null(hist) || length(hist) == 0L) { cat("No solution history recorded (keep_solution_history=FALSE).\n"); return(invisible(NULL)) }
  df_hist <- data.frame(key = vapply(hist, `[[`, character(1L), "key"),
                        sem_score = vapply(hist, function(x) if (is.na(x$sem_score)) NA_real_ else x$sem_score, numeric(1L)),
                        pfa_score = vapply(hist, function(x) if (is.null(x$pfa_score) || is.na(x$pfa_score)) NA_real_ else x$pfa_score, numeric(1L)),
                        search_score = vapply(hist, function(x) if (is.null(x$search_score) || is.na(x$search_score)) NA_real_ else x$search_score, numeric(1L)),
                        esem_score = vapply(hist, function(x) if (is.null(x$esem_score) || is.na(x$esem_score)) NA_real_ else x$esem_score, numeric(1L)),
                        total = vapply(hist, function(x) x$total %||% NA_real_, numeric(1L)),
                        stage = vapply(hist, function(x) x$stage %||% "proposal", character(1L)),
                        converged = vapply(hist, function(x) isTRUE(x$converged), logical(1L)),
                        stringsAsFactors = FALSE)
  ord <- switch(sort_by,
                "total" = order(df_hist$total, decreasing = TRUE),
                "sem_score" = order(df_hist$sem_score, decreasing = TRUE),
                "pfa_score" = order(df_hist$pfa_score, decreasing = TRUE, na.last = TRUE),
                "search_score" = order(df_hist$search_score, decreasing = TRUE, na.last = TRUE),
                "esem_score" = order(df_hist$esem_score, decreasing = TRUE, na.last = TRUE),
                seq_len(nrow(df_hist)))
  df_top <- df_hist[ord[seq_len(min(top_n, nrow(df_hist)))], ]
  cat(sprintf("\n-- TOP-%d EVALUATED SOLUTIONS (sorted by %s) --------\n", nrow(df_top), sort_by))
  cat(sprintf("  Total unique solutions: %d\n\n", nrow(df_hist)))
  cat(sprintf("  %-42s  %7s  %7s  %7s  %7s  %7s\n", "Key", "Sem", "PFA", "Search", "ESEM", "Total"))
  cat(sprintf("  %s\n", strrep("-", 96)))
  for (i in seq_len(nrow(df_top))) {
    row <- df_top[i, ]
    cat(sprintf("  %-42s  %7.4f  %7s  %7s  %7s  %7.4f\n",
                substr(row$key, 1L, 42L),
                row$sem_score,
                if (is.na(row$pfa_score)) "  NA   " else sprintf("%.4f", row$pfa_score),
                if (is.na(row$search_score)) "  NA   " else sprintf("%.4f", row$search_score),
                if (is.na(row$esem_score)) "  NA   " else sprintf("%.4f", row$esem_score),
                row$total))
  }
  invisible(df_hist)
}
