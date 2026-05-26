# SEMANTICA core ACO-ESEM optimization helpers.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
}

.semantica_max_workers <- function(n) {
  n <- suppressWarnings(as.integer(n[1L]))
  if (length(n) == 0L || !is.finite(n) || n < 1L) return(1L)
  min(n, 2L)
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
  n_cores <- .semantica_max_workers(n_cores)
  if (n_cores <= 1L) return(NULL)
  cl <- parallel::makeCluster(n_cores, type = "PSOCK")
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(lavaan)
      library(Matrix)
    })
  })
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
  n_cores <- .semantica_max_workers(n_cores)
  pop_model <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors,
                                                  loading_pattern, mean_loading, target_factor_cors,
                                                  embed_reliability, residual_inflation, "simulation")
  fit_syntax <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors,
                                                   loading_pattern, mean_loading, target_factor_cors,
                                                   embed_reliability, 0.0, "dfi_package")
  if (verbose) cat("\n[DFI-SIM] Population model for simulation:\n", pop_model, "\n\n")

  single_rep <- function(seed) {
    set.seed(seed)
    dat <- tryCatch(lavaan::simulateData(pop_model, sample.nobs = n_obs), error = function(e) NULL)
    if (is.null(dat)) return(NULL)
    fit <- tryCatch(lavaan::cfa(model = fit_syntax, data = dat, std.lv = TRUE, estimator = estimator), error = function(e) NULL)
    if (is.null(fit) || !lavaan::lavInspect(fit, "converged")) return(NULL)
    fm <- tryCatch(lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")), error = function(e) NULL)
    if (is.null(fm)) return(NULL)
    list(cfi = as.numeric(fm["cfi"]), tli = as.numeric(fm["tli"]),
         rmsea = as.numeric(fm["rmsea"]), srmr = as.numeric(fm["srmr"]))
  }

  seeds <- sample.int(.Machine$integer.max, reps)
  results <- vector("list", reps)

  if (n_cores > 1L) {
    cl <- parallel::makeCluster(n_cores, type = "PSOCK")
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(lavaan)))
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
  list(cfi = cfi_cut, tli = tli_cut, rmsea = rmsea_cut, srmr = srmr_cut, was_degenerate = FALSE)
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

  dyn_out <- tryCatch({
    withCallingHandlers({
      switch(dfi_fn,
             "cfaHB"  = dynamic::cfaHB(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = estimator),
             "cfaOne" = dynamic::cfaOne(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = estimator),
             "catHB"  = dynamic::catHB(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = "WLSMV"),
             "catOne" = dynamic::catOne(model = model_syntax, n = n_obs, reps = reps, plot = FALSE, manual = TRUE, estimator = "WLSMV"),
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
                                                loading_source_label = NULL) {
  criterion <- match.arg(criterion); data_type <- match.arg(data_type)
  if (data_type %in% c("likert", "nonnormal") && is.null(original_data)) { if (verbose) message("DFI: falling back to 'continuous'."); data_type <- "continuous" }
  if (is.null(estimator)) estimator <- switch(data_type, "continuous" = "ML", "categorical" = "WLSMV", "likert" = "ML", "nonnormal" = "MLR")

  n_factors <- length(factors)
  using_fitted <- !is.null(fitted_loadings)
  source_label <- loading_source_label %||% if (using_fitted) "ESEM-fitted" else "prior-based"
  fix_b_active <- embed_reliability < 1.0 || residual_inflation > 0.0

  if (verbose) {
    cat("\n============================================================\n COMPUTING DFI CUTOFFS -- SEMANTICA v8\n")
    cat(sprintf("  Factors          : %d\n  Items per factor : %s\n  Sample size (N)  : %d\n", n_factors, paste(names(items_per_factor), items_per_factor, sep="=", collapse=", "), n_obs))
    cat(sprintf("  Loading source   : %s\n", source_label))
    cat(sprintf("  Data type        : %s | Estimator: %s | Reps: %d\n\n", data_type, estimator, reps))
  }

  model_syntax <- build_population_syntax_modelbased(items_per_factor, fitted_loadings, fitted_factor_cors, loading_pattern, mean_loading, target_factor_cors, embed_reliability, 0.0, "dfi_package")

  cutoffs <- safe_compute_dfi(model_syntax, factors, items_per_factor, n_obs, fitted_loadings, fitted_factor_cors, loading_pattern, mean_loading, target_factor_cors, embed_reliability, residual_inflation, data_type, estimator, reps, level, criterion, max(200L, min(1000L, reps * 2L)), 2L, verbose)

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

  single_rep <- function(seed) {
    set.seed(seed)
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
    if (owns_cluster) on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
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
    parallel::clusterExport(cl, varlist = ls(export_env), envir = export_env)
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
        parallel_workers = n_cores
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

  single_rep <- function(seed) {
    set.seed(seed)
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
    if (!is.null(cl)) on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
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
    parallel::clusterExport(cl, varlist = ls(export_env), envir = export_env)
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
        parallel_workers = n_cores
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

  single_job <- function(job) {
    set.seed(job$seed)
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
    if (!is.null(cl)) on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
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
    parallel::clusterExport(cl, varlist = ls(export_env), envir = export_env)
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
        parallel_workers = n_cores
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
    if (is.finite(out$max_std_loading) && out$max_std_loading > loading_ceiling + 1e-6) {
      out$improper <- TRUE
      out$reason <- c(out$reason, sprintf("standardized loading > %.2f", loading_ceiling))
    }
    if (is.finite(out$max_std_loading) && out$max_std_loading >= boundary_loading) {
      out$near_boundary <- TRUE
    }
  }
  if (!is.null(theta) && is.matrix(theta) && nrow(theta) == ncol(theta)) {
    resid_var <- diag(theta)
    out$min_std_residual_variance <- suppressWarnings(min(resid_var, na.rm = TRUE))
    if (is.finite(out$min_std_residual_variance) && out$min_std_residual_variance < residual_floor) {
      out$improper <- TRUE
      out$reason <- c(out$reason, sprintf("standardized residual variance < %.4g", residual_floor))
    }
    if (is.finite(out$min_std_residual_variance) && out$min_std_residual_variance <= boundary_residual) {
      out$near_boundary <- TRUE
    }
  }
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

  lambda_mat <- tryCatch(lavaan::lavInspect(esem_fit, "est")$lambda, error = function(e) NULL)
  if (is.null(lambda_mat)) return(NULL)

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
    f_col <- which(colnames(lambda_mat) == f)
    if (length(f_col) == 0L) f_col <- which(seq_along(factors) == which(factors == f))
    if (length(f_col) == 0L) return(numeric(0))
    abs(lambda_mat[f_items, f_col[1L], drop = TRUE])
  })

  fitted_factor_cors <- tryCatch({
    psi_mat <- lavaan::lavInspect(esem_fit, "est")$psi
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

transform_cosine_for_esem <- function(cos_matrix, factor_assignment = NULL, factors = NULL) {
  if (!is.matrix(cos_matrix)) cos_matrix <- as.matrix(cos_matrix)
  p <- nrow(cos_matrix); if (p < 2L) return(cos_matrix)
  nms <- dimnames(cos_matrix)
  m <- (cos_matrix + t(cos_matrix)) / 2
  off <- m[row(m) != col(m)]; m[row(m) != col(m)] <- pmin(pmax(off, -0.9999), 0.9999)
  diag(m) <- 1.0

  if (!is.null(factor_assignment) && !is.null(factors)) {
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f]); f_items <- intersect(f_items, rownames(m))
      if (length(f_items) < 2L) next
      block <- m[f_items, f_items, drop = FALSE]; evals_b <- eigen(block, symmetric = TRUE, only.values = TRUE)$values
      if (min(evals_b) < 1e-6) {
        eig_b <- eigen(block, symmetric = TRUE); eig_b$values <- pmax(eig_b$values, 1e-4)
        block_fixed <- eig_b$vectors %*% diag(eig_b$values, length(eig_b$values)) %*% t(eig_b$vectors)
        D_inv <- diag(1 / sqrt(diag(block_fixed)), length(f_items))
        block_fixed <- D_inv %*% block_fixed %*% D_inv; diag(block_fixed) <- 1.0
        m[f_items, f_items] <- block_fixed
      }
    }
  }

  evals <- eigen(m, symmetric = TRUE, only.values = TRUE)$values
  if (min(evals) < 1e-6) {
    eig <- eigen(m, symmetric = TRUE); eig$values <- pmax(eig$values, 1e-4)
    m_fixed <- eig$vectors %*% diag(eig$values, length(eig$values)) %*% t(eig$vectors)
    D_inv <- diag(1 / sqrt(diag(m_fixed)), p)
    m <- D_inv %*% m_fixed %*% D_inv; diag(m) <- 1.0
    if (min(eigen(m, symmetric = TRUE, only.values = TRUE)$values) < 1e-8) {
      pd <- tryCatch(as.matrix(Matrix::nearPD(m, corr = TRUE, keepDiag = TRUE, do2eigen = TRUE, maxit = 1000)$mat), error = function(e) m)
      diag(pd) <- 1.0; m <- pd
    }
  }
  dimnames(m) <- nms; m
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

rmsea_power <- function(n_obs, df, rmsea_null = 0.05, rmsea_alt = 0.08,
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
                                       rmsea_null = 0.05, rmsea_alt = 0.08,
                                       power = 0.80, alpha = 0.05,
                                       min_n = NULL, max_n = 5000L) {
  df <- as.numeric(df)
  power <- as.numeric(power)
  max_n <- max(10L, as.integer(max_n))
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
  if (lower > max_n) max_n <- lower
  hi_power <- rmsea_power(max_n, df, rmsea_null, rmsea_alt, alpha)
  if (!is.finite(hi_power) || hi_power < power) return(NA_integer_)
  lo <- lower
  hi <- max_n
  while (lo < hi) {
    mid <- floor((lo + hi) / 2)
    mid_power <- rmsea_power(mid, df, rmsea_null, rmsea_alt, alpha)
    if (is.finite(mid_power) && mid_power >= power) hi <- mid else lo <- mid + 1L
  }
  as.integer(lo)
}

estimate_esem_reference_sample_size <- function(items_per_factor, n_factors = length(items_per_factor),
                                                rmsea_null = 0.05, rmsea_alt = 0.08,
                                                power = 0.80, alpha = 0.05,
                                                min_n = NULL, max_n = 5000L) {
  counts <- suppressWarnings(as.integer(items_per_factor))
  counts <- counts[is.finite(counts) & counts > 0L]
  p <- sum(counts)
  m <- as.integer(n_factors)
  df <- efa_degrees_of_freedom(p, m)
  lower <- if (!is.null(min_n)) as.integer(min_n) else p + 3L
  n_req <- required_n_for_rmsea_power(
    df = df, n_indicators = p, rmsea_null = rmsea_null,
    rmsea_alt = rmsea_alt, power = power, alpha = alpha,
    min_n = lower, max_n = max_n
  )
  note <- "Reference fit N for semantic-proxy RMSEA testing, chosen by noncentral chi-square RMSEA power analysis; this is not a response-data validation sample-size recommendation."
  method <- "MacCallum-Browne-Sugawara RMSEA power"
  if (!is.finite(df) || df <= 0 || is.na(n_req)) {
    n_req <- max(lower, p + 3L)
    note <- paste(
      "Model has nonpositive or unstable approximate EFA degrees of freedom;",
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

compute_pfa_diagnostics <- function(cos_matrix, factor_assignment, factors,
                                    extraction = c("principal", "ml"),
                                    rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
                                    min_loading = 0.40,
                                    min_margin = NULL) {
  extraction <- match.arg(extraction)
  rotation <- match.arg(rotation)
  fail <- list(
    available = FALSE, score = 0, recovery_score = 0,
    salience_score = 0, clarity_score = 0,
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
  salience_score <- mean(pmin(pmax(primary / min_loading, 0), 1), na.rm = TRUE)
  clarity_score <- mean(pmin(pmax(margin / min_margin, 0), 1), na.rm = TRUE)
  if (!is.finite(salience_score)) salience_score <- 0
  if (!is.finite(clarity_score)) clarity_score <- 0
  finite_primary <- primary[is.finite(primary)]
  finite_margin <- margin[is.finite(margin)]
  mean_primary <- if (length(finite_primary) > 0L) mean(finite_primary) else NA_real_
  min_primary <- if (length(finite_primary) > 0L) min(finite_primary) else NA_real_
  mean_margin <- if (length(finite_margin) > 0L) mean(finite_margin) else NA_real_
  primary_ge_min <- if (length(finite_primary) > 0L) mean(finite_primary >= min_loading) else NA_real_
  margin_ge_min <- if (length(finite_margin) > 0L) mean(finite_margin >= min_margin) else NA_real_
  score <- pfa_harmonic_mean(c(recovery_score, salience_score, clarity_score))
  factor_cor_max <- if (!is.null(pfa$phi) && is.matrix(pfa$phi) && nrow(pfa$phi) > 1L) {
    max(abs(pfa$phi[lower.tri(pfa$phi)]), na.rm = TRUE)
  } else NA_real_
  if (!is.finite(factor_cor_max)) factor_cor_max <- NA_real_
  list(
    available = TRUE,
    score = max(0, min(1, score)),
    recovery_score = max(0, min(1, recovery_score)),
    salience_score = max(0, min(1, salience_score)),
    clarity_score = max(0, min(1, clarity_score)),
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
  fail <- list(available = FALSE, score = 0, note = "Facet/unit-level PFA unavailable.")
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
  unit_ids <- paste(f_vals, u_vals, sep = "::")
  units <- unique(unit_ids)
  if (is.null(factors)) factors <- unique(f_vals)
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
    fail$note <- "Too few facet/unit embeddings for factor extraction."
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
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
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
      lambda_hat <- tryCatch(lavaan::lavInspect(fit, "std")$lambda, error = function(e) NULL)
      if (is.null(lambda_hat)) {
        .semantica_progress_update(progress_bar, r)
        next
      }
      item_names <- intersect(rownames(lambda_hat), names(factor_assignment))
      factor_cols <- vapply(factors, function(f) {
        idx <- which(colnames(lambda_hat) == f)
        if (length(idx) == 0L) idx <- which(sanitize_lavaan_name(colnames(lambda_hat)) == sanitize_lavaan_name(f))
        if (length(idx) == 0L) NA_integer_ else idx[1L]
      }, integer(1L))
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
      psi_hat <- tryCatch(lavaan::lavInspect(fit, "std")$psi, error = function(e) NULL)
      if (!is.null(psi_hat) && is.matrix(psi_hat) && !is.null(pop$phi) && is.matrix(pop$phi)) {
        psi_names <- colnames(lambda_hat)[factor_cols]
        if (!is.null(rownames(psi_hat)) && all(psi_names %in% rownames(psi_hat))) {
          psi_hat <- psi_hat[psi_names, psi_names, drop = FALSE]
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
                               sample_cov_rescale = FALSE) {
  if (!is.matrix(cor_matrix)) cor_matrix <- as.matrix(cor_matrix)
  if (any(!is.finite(cor_matrix))) return(NULL)
  if (min(eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values) < 1e-10) return(NULL)

  iter_max <- max(100L, as.integer(iter_max))
  sample_cov_rescale <- isTRUE(sample_cov_rescale)
  tag_attempt <- function(fit, attempt) {
    attr(fit, "semantica_fit_attempt") <- as.integer(attempt)
    fit
  }
  fit <- tryCatch(suppressWarnings(lavaan::sem(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = rotation, rotation.args = rotation_args, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = TRUE, control = list(iter.max = iter_max))), error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit) && lavaan::lavInspect(fit, "converged")) return(tag_attempt(fit, 1L))
  if (!isTRUE(fallback)) return(NULL)

  fit2 <- tryCatch(suppressWarnings(lavaan::sem(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = rotation, rotation.args = rotation_args, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = FALSE, check.start = FALSE, optim.method = "BFGS", control = list(iter.max = iter_max))), error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit2) && lavaan::lavInspect(fit2, "converged")) return(tag_attempt(fit2, 2L))

  alt_rotation <- if (rotation == "geomin") "oblimin" else "geomin"
  fit3 <- tryCatch(suppressWarnings(lavaan::sem(model = syntax, sample.cov = cor_matrix, sample.nobs = n_obs, estimator = estimator, rotation = alt_rotation, sample.cov.rescale = sample_cov_rescale, warn = FALSE, check.post = FALSE, control = list(iter.max = iter_max))), error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit3) && lavaan::lavInspect(fit3, "converged")) return(tag_attempt(fit3, 3L))
  NULL
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

  fit_args <- list(
    model = syntax, data = dat, estimator = estimator,
    rotation = rotation, rotation.args = rotation_args,
    warn = FALSE, check.post = TRUE,
    control = list(iter.max = iter_max)
  )
  if (!is.null(ordered)) fit_args$ordered <- ordered
  fit <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                  error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit) && lavaan::lavInspect(fit, "converged")) return(fit)
  if (!isTRUE(fallback)) return(NULL)

  fit_args$check.post <- FALSE
  fit_args$check.start <- FALSE
  fit_args$optim.method <- "BFGS"
  fit2 <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                   error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit2) && lavaan::lavInspect(fit2, "converged")) return(fit2)

  fit_args$rotation <- if (rotation == "geomin") "oblimin" else "geomin"
  fit_args$rotation.args <- list()
  fit_args$optim.method <- NULL
  fit3 <- tryCatch(suppressWarnings(do.call(lavaan::sem, fit_args)),
                   error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(fit3) && lavaan::lavInspect(fit3, "converged")) return(fit3)
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
    lambda_mat <- tryCatch(lavaan::lavInspect(esem_fit, "std")$lambda,
                           error = function(e) lavaan::lavInspect(esem_fit, "est")$lambda)
    if (is.null(lambda_mat) || !is.matrix(lambda_mat)) return(NA_real_)
    theta_mat <- tryCatch(lavaan::lavInspect(esem_fit, "std")$theta, error = function(e) NULL)
    factor_ave <- numeric(0)
    warnings <- character(0)
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f]); f_items <- intersect(f_items, rownames(lambda_mat))
      if (length(f_items) == 0L) next
      f_col <- which(colnames(lambda_mat) == f)
      if (length(f_col) == 0L) f_col <- which(sanitize_lavaan_name(colnames(lambda_mat)) == sanitize_lavaan_name(f))
      if (length(f_col) == 0L) next
      dom_load <- as.numeric(lambda_mat[f_items, f_col[1L], drop = TRUE])
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

      if (length(htmt_vals) == 0L) return(list(max_cor = 0.0, violations = 0L, values = htmt_vals, method = "item_htmt"))
      return(list(max_cor = max(htmt_vals, na.rm = TRUE),
                  violations = sum(htmt_vals > threshold, na.rm = TRUE),
                  values = htmt_vals, method = "item_htmt"))
    }

    # Backward-compatible fallback: this is a latent-factor correlation check,
    # not HTMT, and is only used when item-level information is unavailable.
    psi_mat <- lavaan::lavInspect(esem_fit, "est")$psi
    if (is.null(psi_mat) || !is.matrix(psi_mat)) return(list(max_cor = 1.0, violations = Inf))
    d_inv  <- diag(1 / sqrt(diag(psi_mat)), nrow(psi_mat))
    cor_lv <- d_inv %*% psi_mat %*% d_inv; diag(cor_lv) <- 1.0
    ut <- upper.tri(cor_lv); cors <- abs(cor_lv[ut])
    list(max_cor = if (length(cors) > 0L) max(cors) else 0.0,
         violations = sum(cors > threshold), values = cors, method = "latent_correlation_fallback")
  }, error = function(e) list(max_cor = 1.0, violations = Inf))
}

compute_esem_structure_diagnostics <- function(esem_fit, observed_cor = NULL,
                                               factor_assignment = NULL,
                                               factors = NULL,
                                               primary_min = 0.40,
                                               cross_max = 0.30) {
  empty <- list(
    mean_primary_loading = NA_real_, median_primary_loading = NA_real_,
    min_primary_loading = NA_real_, primary_ge_40 = NA_real_,
    primary_ge_50 = NA_real_, mean_max_cross_loading = NA_real_,
    q90_max_cross_loading = NA_real_, max_cross_loading = NA_real_,
    no_large_cross_loading = NA_real_, correct_dominance = NA_real_,
    simple_structure = NA_real_, mean_salience_ratio = NA_real_,
    median_salience_ratio = NA_real_, mean_complexity = NA_real_,
    max_complexity = NA_real_, max_abs_residual = NA_real_,
    mean_abs_residual = NA_real_, q95_abs_residual = NA_real_,
    latent_cor_max = NA_real_, factor_score_determinacy = NA_real_,
    omega_dominant = NULL, item_diagnostics = NULL,
    note = "ESEM structure diagnostics unavailable."
  )
  if (is.null(esem_fit) || is.null(factor_assignment) || is.null(factors)) return(empty)

  lambda_mat <- tryCatch(lavaan::lavInspect(esem_fit, "std")$lambda,
                         error = function(e) lavaan::lavInspect(esem_fit, "est")$lambda)
  if (is.null(lambda_mat) || !is.matrix(lambda_mat)) return(empty)
  factor_cols <- vapply(factors, function(f) {
    idx <- which(colnames(lambda_mat) == f)
    if (length(idx) == 0L) idx <- which(sanitize_lavaan_name(colnames(lambda_mat)) == sanitize_lavaan_name(f))
    if (length(idx) == 0L) NA_integer_ else idx[1L]
  }, integer(1L))
  valid_factors <- factors[!is.na(factor_cols)]
  factor_cols <- factor_cols[!is.na(factor_cols)]
  if (length(factor_cols) == 0L) return(empty)

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
    max_cross <- if (length(cross) > 0L) max(cross, na.rm = TRUE) else 0
    dominant_idx <- if (all(!is.finite(abs_loads))) NA_integer_ else which.max(abs_loads)
    dominant <- if (!is.na(dominant_idx)) valid_factors[[dominant_idx]] else NA_character_
    denom <- sum(abs_loads^4, na.rm = TRUE)
    complexity <- if (is.finite(denom) && denom > .Machine$double.eps) {
      (sum(abs_loads^2, na.rm = TRUE)^2) / denom
    } else NA_real_
    salience_ratio <- primary / max(max_cross, .Machine$double.eps)
    issue <- character(0)
    if (!is.finite(primary) || primary < primary_min) issue <- c(issue, "weak_primary")
    if (is.finite(max_cross) && max_cross > cross_max) issue <- c(issue, "large_cross_loading")
    if (!identical(dominant, assigned)) issue <- c(issue, "dominant_factor_mismatch")
    n_rows <- n_rows + 1L
    item_rows[[n_rows]] <- data.frame(
      ID = item_id,
      assigned_factor = assigned,
      dominant_factor = dominant,
      primary_loading = primary,
      max_cross_loading = max_cross,
      salience_ratio = salience_ratio,
      complexity = complexity,
      simple_structure = is.finite(primary) && primary >= primary_min &&
        is.finite(max_cross) && max_cross <= cross_max &&
        identical(dominant, assigned),
      issue = if (length(issue) == 0L) "none" else paste(issue, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  item_diag <- if (n_rows > 0L) do.call(rbind, item_rows[seq_len(n_rows)]) else NULL
  if (is.null(item_diag) || nrow(item_diag) == 0L) return(empty)

  residual_stats <- list(max_abs_residual = NA_real_, mean_abs_residual = NA_real_, q95_abs_residual = NA_real_)
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
        vals <- abs(resid[lower.tri(resid)])
        vals <- vals[is.finite(vals)]
        if (length(vals) > 0L) {
          list(
            max_abs_residual = max(vals),
            mean_abs_residual = mean(vals),
            q95_abs_residual = as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE, names = FALSE))
          )
        } else residual_stats
      } else residual_stats
    }, error = function(e) residual_stats)
  }

  latent_cor_max <- tryCatch({
    psi <- lavaan::lavInspect(esem_fit, "est")$psi
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

  theta_diag <- tryCatch(diag(lavaan::lavInspect(esem_fit, "std")$theta),
                         error = function(e) NULL)
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

  list(
    mean_primary_loading = mean(item_diag$primary_loading, na.rm = TRUE),
    median_primary_loading = stats::median(item_diag$primary_loading, na.rm = TRUE),
    min_primary_loading = min(item_diag$primary_loading, na.rm = TRUE),
    primary_ge_40 = mean(item_diag$primary_loading >= 0.40, na.rm = TRUE),
    primary_ge_50 = mean(item_diag$primary_loading >= 0.50, na.rm = TRUE),
    mean_max_cross_loading = mean(item_diag$max_cross_loading, na.rm = TRUE),
    q90_max_cross_loading = as.numeric(stats::quantile(item_diag$max_cross_loading, 0.90, na.rm = TRUE, names = FALSE)),
    max_cross_loading = max(item_diag$max_cross_loading, na.rm = TRUE),
    no_large_cross_loading = mean(item_diag$max_cross_loading <= cross_max, na.rm = TRUE),
    correct_dominance = mean(item_diag$assigned_factor == item_diag$dominant_factor, na.rm = TRUE),
    simple_structure = mean(item_diag$simple_structure, na.rm = TRUE),
    mean_salience_ratio = mean(item_diag$salience_ratio[is.finite(item_diag$salience_ratio)], na.rm = TRUE),
    median_salience_ratio = stats::median(item_diag$salience_ratio[is.finite(item_diag$salience_ratio)], na.rm = TRUE),
    mean_complexity = mean(item_diag$complexity, na.rm = TRUE),
    max_complexity = max(item_diag$complexity, na.rm = TRUE),
    max_abs_residual = residual_stats$max_abs_residual,
    mean_abs_residual = residual_stats$mean_abs_residual,
    q95_abs_residual = residual_stats$q95_abs_residual,
    latent_cor_max = latent_cor_max,
    factor_score_determinacy = factor_score_determinacy,
    omega_dominant = omega_dominant,
    item_diagnostics = item_diag,
    note = "Diagnostics are computed on the ESEM semantic-proxy correlation model; they assess factorial clarity, not observed response validity."
  )
}

extract_and_score_esem <- function(esem_fit, observed_cor = NULL, factor_assignment = NULL, factors = NULL,
                                   cutoffs = list(cfi = 0.95, tli = 0.95, rmsea = 0.06, srmr = 0.08),
                                   htmt_threshold = 0.85, verbose_decomp = FALSE,
                                   score_mode = c("current", "structure_weighted")) {
  score_mode <- match.arg(score_mode)
  fail <- list(cfi = NA, tli = NA, rmsea = NA, srmr = NA, ave = NA,
               factor_ave = NULL, ave_method = NA_character_, ave_warnings = character(0),
               htmt_max = NA, htmt_violations = Inf, loading_quality = 0,
               structure_diagnostics = NULL,
               converged = FALSE, score = 0, score_decomp = NULL)
  if (is.null(esem_fit)) return(fail)

  fm <- tryCatch(lavaan::fitMeasures(esem_fit, c("cfi", "tli", "rmsea", "srmr")), error = function(e) c(cfi = NA, tli = NA, rmsea = NA, srmr = NA))
  cfi <- as.numeric(fm["cfi"]); tli <- as.numeric(fm["tli"]); rmsea <- as.numeric(fm["rmsea"]); srmr <- as.numeric(fm["srmr"])
  if ((is.na(srmr) || !is.finite(srmr)) && !is.null(observed_cor)) srmr <- compute_manual_srmr(esem_fit, observed_cor)
  cfi <- max(0, min(1, if (is.na(cfi) || !is.finite(cfi)) 0 else cfi))
  tli <- max(0, min(1, if (is.na(tli) || !is.finite(tli)) 0 else tli))
  rmsea <- max(0, if (is.na(rmsea) || !is.finite(rmsea)) 1 else rmsea)
  srmr <- max(0, if (is.na(srmr) || !is.finite(srmr)) 1 else srmr)

  loading_quality <- tryCatch({
    lambda_mat <- tryCatch(lavaan::lavInspect(esem_fit, "std")$lambda,
                           error = function(e) lavaan::lavInspect(esem_fit, "est")$lambda)
    if (is.null(lambda_mat) || !is.matrix(lambda_mat) || is.null(factor_assignment) || is.null(factors)) return(0.5)
    dom_loads <- numeric(0)
    for (f in factors) {
      f_items <- names(factor_assignment[factor_assignment == f]); f_items <- intersect(f_items, rownames(lambda_mat))
      if (length(f_items) == 0L) next
      f_col <- which(colnames(lambda_mat) == f)
      if (length(f_col) == 0L) f_col <- which(sanitize_lavaan_name(colnames(lambda_mat)) == sanitize_lavaan_name(f))
      if (length(f_col) == 0L) next
      dom_loads <- c(dom_loads, abs(lambda_mat[f_items, f_col[1L], drop = TRUE]))
    }
    if (length(dom_loads) == 0L) return(0.5)
    in_range <- mean(dom_loads >= 0.50 & dom_loads <= 0.95)
    mean_lam <- mean(dom_loads[dom_loads > 0], na.rm = TRUE)
    lam_qual <- 1 - abs(mean_lam - 0.75) / 0.75; lam_qual <- max(0, min(1, lam_qual))
    0.65 * in_range + 0.35 * lam_qual
  }, error = function(e) 0.5)

  ave <- compute_ave_esem(esem_fit, factor_assignment, factors)
  factor_ave <- attr(ave, "factor_ave", exact = TRUE)
  ave_warnings <- attr(ave, "ave_warnings", exact = TRUE)
  ave_method <- attr(ave, "ave_method", exact = TRUE)
  ave_num <- if (!is.na(ave) && is.finite(ave)) unname(as.numeric(ave)) else NA_real_
  ave_score <- if (!is.na(ave_num) && is.finite(ave_num)) min(1.0, ave_num / 0.50) else 0.5
  htmt_result <- compute_htmt_esem(esem_fit, factors, htmt_threshold, observed_cor, factor_assignment)
  htmt_penalty <- if (htmt_result$violations > 0) max(0.60, 1 - 0.15 * htmt_result$violations) else 1.0
  structure_diagnostics <- compute_esem_structure_diagnostics(
    esem_fit = esem_fit,
    observed_cor = observed_cor,
    factor_assignment = factor_assignment,
    factors = factors
  )

  cfi_s <- min(1.0, cfi / cutoffs$cfi)
  rmsea_s <- min(1.0, cutoffs$rmsea / max(rmsea, 1e-6))
  srmr_s <- min(1.0, cutoffs$srmr / max(srmr, 1e-6))
  fit_component <- 0.30 * cfi_s + 0.28 * rmsea_s + 0.22 * srmr_s + 0.20 * ave_score
  safe01 <- function(x, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x[1L]))
    if (!is.finite(x)) default else max(0, min(1, x))
  }
  residual_structure_score <- if (!is.null(structure_diagnostics)) {
    resid <- suppressWarnings(as.numeric(structure_diagnostics$mean_abs_residual[1L]))
    if (is.finite(resid)) max(0, min(1, 1 - resid / 0.10)) else NA_real_
  } else NA_real_
  structure_values <- c(
    primary_ge_40 = safe01(structure_diagnostics$primary_ge_40 %||% NA_real_),
    correct_dominance = safe01(structure_diagnostics$correct_dominance %||% NA_real_),
    simple_structure = safe01(structure_diagnostics$simple_structure %||% NA_real_),
    cross_loading_control = safe01(structure_diagnostics$no_large_cross_loading %||% NA_real_),
    residual_reproduction = residual_structure_score
  )
  structure_values <- structure_values[is.finite(structure_values)]
  structure_component <- if (length(structure_values) > 0L) mean(structure_values) else 0.5
  base_score <- if (score_mode == "structure_weighted") {
    # Keep global fit in the score while giving semantic-proxy structure
    # diagnostics more influence than N-sensitive fit indexes.
    fit_core <- 0.40 * cfi_s + 0.35 * rmsea_s + 0.25 * srmr_s
    0.35 * fit_core + 0.20 * ave_score + 0.45 * structure_component
  } else {
    fit_component
  }
  score <- base_score * loading_quality * htmt_penalty

  logistic_score <- function(x, center, steepness, higher_is_better = TRUE) {
    if (higher_is_better) 1 / (1 + exp(-steepness * (x - center))) else 1 / (1 + exp(steepness * (x - center)))
  }
  cfi_logistic <- logistic_score(cfi, center = cutoffs$cfi, steepness = 30, higher_is_better = TRUE)
  rmsea_logistic <- logistic_score(rmsea, center = cutoffs$rmsea, steepness = 30, higher_is_better = FALSE)
  srmr_logistic <- logistic_score(srmr, center = cutoffs$srmr, steepness = 30, higher_is_better = FALSE)

  score_decomp <- list(cfi_s = cfi_s, rmsea_s = rmsea_s, srmr_s = srmr_s, ave_score = ave_score,
                       loading_quality = loading_quality, htmt_penalty = htmt_penalty,
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
    cat(sprintf("    AVE=%.4f ave_s=%.4f | LQ=%.4f | HTMT_pen=%.4f | Base=%.4f | Final=%.4f\n",
                ave_num, ave_score, loading_quality, htmt_penalty, base_score, score))
  }

  list(cfi = cfi, tli = tli, rmsea = rmsea, srmr = srmr, ave = ave_num,
       factor_ave = factor_ave, ave_method = ave_method, ave_warnings = ave_warnings,
       htmt_max = htmt_result$max_cor, htmt_violations = htmt_result$violations,
       loading_quality = loading_quality, structure_diagnostics = structure_diagnostics,
       converged = TRUE, score = max(0, min(1, score)), score_decomp = score_decomp)
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
    max_n <- suppressWarnings(as.integer(max_n[1L]))
    if (is.finite(max_n)) vals <- vals[vals <= max_n]
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
  dominance_floor <- if (!is.null(item_stability) && any(is.finite(item_stability$dominant_factor_agreement))) {
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
      structurally_stable = if (is.finite(dominance_floor) && is.finite(primary_range_median)) {
        dominance_floor >= 0.80 && primary_range_median <= 0.10
      } else NA
    ),
    score_mode = score_mode,
    sample_cov_rescale = isTRUE(sample_cov_rescale),
    note = "N-sensitivity refits the selected semantic-proxy ESEM over reference-N anchors; it does not estimate respondent sample size."
  )
}

# =================================================================
# 8, 9, 10  SEMANTIC INDEX, DUPLICATE CHECK, DIVERSITY FILTER
# =================================================================
estimate_within_similarity_targets <- function(list_items, cosine_sim_matrix, factors,
                                               within_similarity_target = NULL,
                                               lower = 0.25, upper = 0.55) {
  lower <- as.numeric(lower); upper <- as.numeric(upper)
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    lower <- 0.25; upper <- 0.55
  }

  if (!is.null(within_similarity_target)) {
    target <- within_similarity_target
    if (length(target) == 1L) target <- stats::setNames(rep(as.numeric(target), length(factors)), factors)
    if (!is.null(names(target)) && any(nzchar(names(target)))) {
      target <- as.numeric(target[factors])
    } else {
      target <- as.numeric(rep_len(target, length(factors)))
    }
    names(target) <- factors
    target[!is.finite(target)] <- 0.35
    return(pmin(pmax(target, lower), upper))
  }

  target <- stats::setNames(rep(0.35, length(factors)), factors)
  for (f in factors) {
    f_items <- intersect(list_items[[f]], rownames(cosine_sim_matrix))
    if (length(f_items) < 2L) next
    block <- cosine_sim_matrix[f_items, f_items, drop = FALSE]
    sims <- block[lower.tri(block)]
    sims <- sims[is.finite(sims)]
    if (length(sims) == 0L) next
    # Use the lower-middle part of the generated pool as the target: less
    # redundant than the pool average, but not so low that monotrait coherence
    # is destroyed.
    target[f] <- as.numeric(stats::quantile(sims, probs = 0.40, na.rm = TRUE, names = FALSE))
  }
  pmin(pmax(target, lower), upper)
}

compute_semantic_sim_index_v2 <- function(sim_matrix, selected_items, factor_assignment, factors,
                                          redundancy_threshold = 0.85, sigmoid_center = 0.15,
                                          sigmoid_steepness = 10, within_similarity_target = NULL,
                                          within_similarity_band = 0.08,
                                          within_similarity_weight = 1.15,
                                          between_similarity_weight = 1.00) {
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
    f_loss <- 2.0 * low_coherence_loss + high_redundancy_loss + 0.5 * q90_redundancy_loss
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
  mean_within <- if (length(within_sims) > 0L) fisherz_inv(mean(safe_fz(within_sims))) else 0
  mean_between <- if (length(between_sims) > 0L) fisherz_inv(mean(safe_fz(between_sims))) else 0
  q_within <- if (length(within_sims) > 0L) as.numeric(stats::quantile(within_sims, 0.90, na.rm = TRUE, names = FALSE)) else 0
  q_between <- if (length(between_sims) > 0L) as.numeric(stats::quantile(between_sims, 0.90, na.rm = TRUE, names = FALSE)) else 0

  # Within-factor similarity is treated as a target band, not a quantity to
  # minimize without bound. This keeps item wording nonredundant while preserving
  # enough monotrait coherence for reflective scale development.
  within_burden <- if (length(within_losses) > 0L) {
    0.70 * mean(within_losses, na.rm = TRUE) + 0.30 * max(within_losses, na.rm = TRUE)
  } else 0
  between_burden <- if (length(between_sims) > 0L) 0.70 * mean_between + 0.30 * q_between else 0
  denom_w <- within_similarity_weight + between_similarity_weight
  if (!is.finite(denom_w) || denom_w <= 0) denom_w <- 1
  similarity_index <- (within_similarity_weight * within_burden + between_similarity_weight * between_burden) / denom_w

  max_within <- if (length(within_sims) > 0L) max(within_sims, na.rm = TRUE) else 0
  max_between <- if (length(between_sims) > 0L) max(between_sims, na.rm = TRUE) else 0
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
    max_within = max_within,
    max_between = max_between,
    redundancy_penalty = redundancy_penalty,
    raw_index = raw_index
  )
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
  index_before <- mean(c(within_before, between_before), na.rm = TRUE)
  index_after <- mean(c(within_after, between_after), na.rm = TRUE)
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
  interpretation <- if (is.finite(target_value) && is.finite(within_target_improvement)) {
    between_txt <- if (is.finite(between_reduction) && between_reduction > 0.02) {
      "between-factor similarity decreased"
    } else if (is.finite(between_reduction) && between_reduction >= 0) {
      "between-factor similarity was stable/slightly lower"
    } else {
      "between-factor similarity increased"
    }
    if (within_target_improvement > 0.02) {
      paste("Within-factor similarity moved meaningfully toward the target;", between_txt)
    } else if (within_target_improvement >= -0.01) {
      paste("Within-factor similarity stayed near the target;", between_txt)
    } else {
      paste("Within-factor similarity moved away from the target;", between_txt)
    }
  } else if (!is.na(index_reduction)) {
    if (index_reduction > 0.05) "Strong reduction in semantic similarity"
    else if (index_reduction > 0.02) "Moderate reduction"
    else if (index_reduction >= 0) "Slight reduction or stable"
    else "Warning: semantic similarity increased"
  } else {
    "Could not compute (insufficient items)"
  }
  list(
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
  cat(sprintf("%sBetween-factor: %s -> %s | reduction = %s\n",
              prefix, fmt(metrics$between_factor_before), fmt(metrics$between_factor_after),
              fmt(metrics$between_absolute_reduction)))
  cat(sprintf("%sComposite index: %s -> %s | reduction = %s\n",
              prefix, fmt(metrics$semantic_similarity_index_before),
              fmt(metrics$semantic_similarity_index_after),
              fmt(metrics$semantic_similarity_index_reduction)))
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
    value <- scalar(value)
    cutoff <- scalar(cutoff)
    if (!is.finite(value) || !is.finite(cutoff)) return("N/A")
    if (identical(direction, ">=")) {
      if (value >= cutoff) "PASS" else "FAIL"
    } else {
      if (value <= cutoff) "PASS" else "FAIL"
    }
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

  cat("\n-- FINAL RESULTS SUMMARY ----------------------------------\n")
  cat("  Read by section: selected scale, final fit checks, structural quality, semantic selection, then stability/planning diagnostics.\n")
  summary_section(
    "1. Selected scale and ACO search",
    "The selected items below are the final solution used by all following diagnostics."
  )
  cat("\n  ACO search outcome\n")
  cat(sprintf("  Iterations        : %d\n", whole(result$total_iterations)))
  cat(sprintf("  Search ESEM fits  : %d / %d succeeded\n", successes, attempts))
  cat(sprintf("  Elite archive     : %d solutions\n", length(result$elite_archive %||% list())))
  cat(sprintf("  Search guidance   : %s\n", text(result$search_guidance_status, "legacy/unknown")))
  cat(sprintf("  Final objective   : %s\n", num(result$best_objective)))
  cat(sprintf("  Selected solution : %d items across %d factors\n",
              length(result$best_items %||% character(0)), length(factors)))

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
  if (!isTRUE(cr$converged)) {
    cat("  Final ESEM fit did not converge; fit indices are unavailable.\n")
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
  if (isTRUE(cr$converged)) {
    cat(sprintf("  Final ESEM score  : %s\n", num(cr$score)))
  }
  cat(sprintf("  DFI mode          : %s\n", text(result$dfi_mode)))
  cat(sprintf("  Cutoff source     : %s\n", text(result$cutoff_source)))
  if (!is.null(result$search_cutoff_source) &&
      !identical(result$cutoff_source, result$search_cutoff_source)) {
    cat(sprintf("  Search source     : %s\n", text(result$search_cutoff_source)))
  }

  summary_section(
    "3. Factorial structure, convergent evidence, and discrimination",
    "These diagnostics describe loading clarity and semantic-proxy construct separation."
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
    cat(sprintf("  Correct dominance : %s | simple structure: %s\n",
                pct(sdg$correct_dominance), pct(sdg$simple_structure)))
    cat(sprintf("  Cross-loadings    : mean %s | q90 %s | max %s\n",
                num(sdg$mean_max_cross_loading), num(sdg$q90_max_cross_loading),
                num(sdg$max_cross_loading)))
    cat(sprintf("  Complexity        : mean %s | max %s\n",
                num(sdg$mean_complexity), num(sdg$max_complexity)))
    cat(sprintf("  Residual |r|      : mean %s | q95 %s | max %s\n",
                num(sdg$mean_abs_residual), num(sdg$q95_abs_residual),
                num(sdg$max_abs_residual)))
    cat(sprintf("  Latent |r| / det. : %s / %s\n",
                num(sdg$latent_cor_max), num(sdg$factor_score_determinacy)))
  }
  if (isTRUE(cr$converged)) {
    cat("\n  Convergent and discriminant diagnostics\n")
    cat(sprintf("  AVE (dominant)    : %s (%s)\n",
                num(cr$ave),
                if (!is.null(result$response_validation)) {
                  "compare with the response-data benchmark"
                } else {
                  "semantic-proxy descriptive index"
                }))
    fit_line("HTMT max", cr$htmt_max, result$model_info$htmt_threshold, "<=")
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
    "These metrics summarize semantic cohesion, discrimination, and redundancy change after selection."
  )
  cat(sprintf("  Scores            : semantic %s | proposal objective %s | final objective %s\n",
              num(result$semantic_score), num(result$semantic_objective_score),
              num(result$best_objective)))
  cat(sprintf("  Similarity        : raw %s | mean within %s | mean between %s\n",
              num(result$semantic_index), num(result$mean_within),
              num(result$mean_between)))
  print_semantic_similarity_reduction_summary(
    result$semantic_similarity_reduction,
    prefix = "  ",
    heading = FALSE
  )

  summary_section(
    "5. Sample-free companion structure diagnostic",
    "PFA is a semantic-proxy companion diagnostic; it is not response-data validation."
  )
  pfa <- result$pfa_diagnostics
  if (!is.null(pfa)) {
    if (isTRUE(pfa$available)) {
      cat(sprintf("  Sample-free PFA   : score %s | recovery %s | salience %s | clarity %s\n",
                  num(pfa$score), num(pfa$recovery_score), num(pfa$salience_score),
                  num(pfa$clarity_score)))
      cat(sprintf("  PFA loadings      : mean primary %s | mean margin %s | %s / %s\n",
                  num(pfa$mean_primary_loading), num(pfa$mean_loading_margin),
                  text(pfa$extraction, "unknown extraction"),
                  text(pfa$rotation, "unknown rotation")))
      cat(sprintf("  PFA role          : %s\n",
                  if (identical(result$model_info$pfa_mode, "objective")) {
                    "selection objective and final report (objective extraction is stored separately)"
                  } else {
                    "descriptive diagnostic only; not used to select items"
                  }))
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
  stab <- result$split_half_stability
  stable <- if (!is.null(stab)) stab$stable else NA
  if (length(stable) > 0L && !is.na(stable[1L])) {
    cat(sprintf("  Semantic stability: half A %s | half B %s | diff %s [%s]\n",
                num(stab$sem_half_A), num(stab$sem_half_B), num(stab$difference),
                if (isTRUE(stable[1L])) "STABLE" else "UNSTABLE"))
  }
  if (!is.null(result$recommended_validation_n)) {
    rvn <- result$recommended_validation_n
    if (isTRUE(rvn$available) && is.finite(scalar(rvn$recommended_n))) {
      cat(sprintf("  Validation N      : %d recommended (%d reps/candidate)\n",
                  whole(rvn$recommended_n), whole(rvn$reps)))
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
        cat(sprintf("  Proxy N structure : %s | dominance floor %s | median primary range %s\n",
                    if (isTRUE(sm$structurally_stable)) "stable" else "changed",
                    num(sm$dominant_factor_agreement_floor),
                    num(sm$median_primary_loading_range)))
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
    cat(sprintf("  Response ESEM fit : CFI %s | RMSEA %s | SRMR %s | AVE %s | HTMT %s\n",
                num(rv$cfi), num(rv$rmsea), num(rv$srmr), num(rv$ave),
                num(rv$htmt_max)))
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
      "Each ESEM factor loads on the full selected indicator set before rotation."
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
                                               htmt_guard_threshold = 0.95,
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

compute_eligible_items <- function(list_items, cosine_sim_matrix, factors, i_per_f,
                                   cohesion_retention = 0.75, cohesion_floor_abs = 0.15,
                                   within_similarity_target = NULL,
                                   within_similarity_band = 0.08) {
  cohesion_retention <- suppressWarnings(as.numeric(cohesion_retention[1L]))
  if (!is.finite(cohesion_retention)) cohesion_retention <- 0.75
  cohesion_retention <- min(1, max(0, cohesion_retention))
  eligible <- list()
  for (f in factors) {
    f_items <- list_items[[f]]
    if (length(f_items) <= i_per_f[f]) { eligible[[f]] <- f_items; next }
    f_valid <- intersect(f_items, rownames(cosine_sim_matrix))
    if (length(f_valid) < 2L) { eligible[[f]] <- f_items; next }
    block <- cosine_sim_matrix[f_valid, f_valid, drop = FALSE]
    diag(block) <- NA_real_
    cohesion <- rowMeans(block, na.rm = TRUE)
    target <- if (!is.null(within_similarity_target) && f %in% names(within_similarity_target)) within_similarity_target[[f]] else 0.35
    target <- if (is.finite(target)) as.numeric(target) else 0.35
    dev <- abs(cohesion - target)
    dev_cut <- stats::quantile(dev, cohesion_retention, na.rm = TRUE, names = FALSE)
    keep <- f_valid[dev <= dev_cut]
    # Preserve selection freedom: a nominal ACO search should not be forced
    # to choose almost every screened item. The pre-existing three-to-one
    # safeguard is enforced whenever the generated pool permits it.
    keep_n <- min(length(f_valid), max(i_per_f[f] * 3L, i_per_f[f] + 3L, 4L))
    if (length(keep) < keep_n) {
      keep <- f_valid[order(dev, cohesion, decreasing = FALSE)][seq_len(keep_n)]
    }
    eligible[[f]] <- keep
  }
  eligible
}

compute_item_heuristics <- function(eligible_items, cosine_sim_matrix, factors,
                                    within_similarity_target = NULL,
                                    within_similarity_band = 0.08) {
  heuristics <- list()
  for (f in factors) {
    f_items <- eligible_items[[f]]
    if (length(f_items) < 2L) { heuristics[[f]] <- setNames(rep(1.0, length(f_items)), f_items); next }
    f_valid <- intersect(f_items, rownames(cosine_sim_matrix))
    if (length(f_valid) < 2L) { heuristics[[f]] <- setNames(rep(1.0, length(f_valid)), f_valid); next }
    block <- cosine_sim_matrix[f_valid, f_valid, drop = FALSE]
    diag(block) <- NA_real_
    cohesion <- rowMeans(block, na.rm = TRUE)
    target <- if (!is.null(within_similarity_target) && f %in% names(within_similarity_target)) within_similarity_target[[f]] else 0.35
    target <- if (is.finite(target)) as.numeric(target) else 0.35
    dev <- abs(cohesion - target)
    dev_range <- max(dev) - min(dev)
    h_vals <- if (dev_range > 1e-6) 0.1 + 0.9 * (max(dev) - dev) / dev_range else rep(1.0, length(dev))
    # Mildly discourage items whose average within-factor similarity is below
    # the coherence floor; these often behave like semantic outliers.
    low_outlier <- cohesion < (target - within_similarity_band)
    h_vals[low_outlier] <- h_vals[low_outlier] * 0.70
    heuristics[[f]] <- stats::setNames(h_vals, f_valid)
  }
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

update_elite_archive <- function(archive, entries, elite_k) {
  entries <- Filter(Negate(is.null), entries)
  if (length(entries) == 0L) return(archive)
  combined <- c(archive, entries)
  ord <- order(vapply(combined, `[[`, numeric(1L), "obj"), decreasing = TRUE)
  combined <- combined[ord]
  sig <- vapply(combined, function(e) solution_signature(e$vec), character(1L))
  combined <- combined[!duplicated(sig)]
  combined[seq_len(min(length(combined), as.integer(elite_k)))]
}

fit.function.v2 <- function(selected_vector, run_esem_now = FALSE, effective_esem_weight = 0.50,
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
      if (!is.null(cached$search_score)) return(cached$search_score)
      if (!is.null(cached$sem_score)) return(cached$sem_score)
    }
    if (!is.null(cached) && run_esem_now && !is.null(cached$esem_score)) {
      # Recompute the mixture from cached components under the declared
      # objective weight; this keeps all guided solutions comparable.
      guard_penalty <- cached$guard_penalty
      if (is.null(guard_penalty) || !is.finite(guard_penalty)) guard_penalty <- 1.0
      guard_weight <- model_info$psychometric_guard_weight %||% 0.50
      base_score <- cached$search_score %||% cached$sem_score
      return(((1 - effective_esem_weight) * base_score + effective_esem_weight * cached$esem_score) * (guard_penalty ^ guard_weight))
    }
  }

  cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, selected_items), error = function(e) NULL)
  if (is.null(cos_sub)) return(-Inf)

  use_cached_search <- run_esem_now &&
    !is.null(cached) &&
    !is.null(cached$search_score) &&
    !is.null(cached$sem_score) &&
    is.finite(cached$search_score) &&
    is.finite(cached$sem_score)
  if (use_cached_search) {
    sem_score <- cached$sem_score
    pfa_result <- cached$pfa_result
    pfa_score <- cached$pfa_score %||% NA_real_
    search_score <- cached$search_score
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
      within_similarity_band = model_info$within_similarity_band %||% 0.08
    )
    sem_score <- sem_result$sem_score * dup_penalty * facet_multiplier
    pfa_result <- NULL
    pfa_score <- NA_real_
    search_score <- sem_score
    if ((model_info$pfa_mode %||% "off") == "objective" && (model_info$pfa_weight %||% 0) > 0) {
      pfa_result <- compute_pfa_diagnostics(
        cos_sub, factor_assignment, factors,
        extraction = model_info$pfa_extraction %||% "principal",
        rotation = model_info$pfa_rotation %||% "promax",
        min_loading = model_info$pfa_min_loading %||% 0.40,
        min_margin = model_info$pfa_min_margin
      )
      pfa_score <- if (isTRUE(pfa_result$available)) pfa_result$score else 0
      w_pfa <- max(0, min(1, model_info$pfa_weight %||% 0))
      search_score <- (1 - w_pfa) * sem_score + w_pfa * pfa_score
    }
  }

  if (!run_esem_now) {
    total_score <- search_score
    if (!is.null(solution_cache) && is.null(cached)) {
      cache_set(solution_cache, cache_key, list(
        sem_score = sem_score, pfa_score = pfa_score,
        pfa_result = pfa_result, search_score = search_score,
        total_score = total_score
      ))
    }
    if (!is.null(solution_history_env)) {
      .semantica_history_append(solution_history_env, list(
        key = cache_key, sem_score = sem_score, pfa_score = pfa_score,
        search_score = search_score, esem_score = NA_real_, total = total_score
      ))
    }
    return(total_score)
  }

  esem_cor <- transform_cosine_for_esem(cos_sub, factor_assignment, factors)
  if (is.null(esem_cor)) {
    total_score <- search_score * (1 - effective_esem_weight)
    cache_entry <- list(
      sem_score = sem_score, pfa_score = pfa_score, pfa_result = pfa_result,
      search_score = search_score, esem_score = 0, total_score = total_score
    )
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
  esem_fit <- run_esem_on_matrix(
    esem_syntax, esem_cor, model_info$n_obs, model_info$estimator,
    model_info$rotation, esem_rotation_args,
    iter_max = model_info$fast_esem_iter_max %||% 500L,
    fallback = !isTRUE(model_info$fast_esem)
  )
  fit_result <- extract_and_score_esem(
    esem_fit, esem_cor, factor_assignment, factors,
    model_info$active_cutoffs, model_info$htmt_threshold, verbose_decomp,
    score_mode = model_info$semantic_esem_score_mode %||% "current"
  )

  esem_score <- fit_result$score
  guard_penalty <- compute_psychometric_guard_penalty(
    fit_result,
    min_ave = model_info$psychometric_guard_min_ave %||% 0.30,
    min_primary_loading = model_info$psychometric_guard_min_loading %||% 0.40,
    min_primary_prop_ge_50 = model_info$psychometric_guard_min_primary_ge_50 %||% 0.70
  )
  guard_weight <- model_info$psychometric_guard_weight %||% 0.50
  total_score <- ((1 - effective_esem_weight) * search_score + effective_esem_weight * esem_score) * (guard_penalty ^ guard_weight)
  cache_entry <- list(
    sem_score = sem_score, pfa_score = pfa_score, pfa_result = pfa_result,
    search_score = search_score, esem_score = esem_score,
    guard_penalty = guard_penalty, total_score = total_score, fit_result = fit_result
  )
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
                error = if (isTRUE(fit_result$converged)) NA_character_ else "ESEM model did not return a converged scored solution."))
  }
  total_score
}

.semantica_evaluate_esem_worker <- function(task) {
  v <- task$vector
  fit_fun <- get("fit.function.v2", envir = .GlobalEnv, inherits = FALSE)
  environment(fit_fun) <- .GlobalEnv
  key_fun <- get("make_solution_key", envir = .GlobalEnv, inherits = FALSE)
  tryCatch({
    payload <- fit_fun(
      v,
      run_esem_now = TRUE,
      effective_esem_weight = task$effective_esem_weight,
      solution_cache = NULL,
      solution_history_env = NULL,
      return_payload = TRUE
    )
    converged <- !is.null(payload$cache_entry$fit_result) &&
      isTRUE(payload$cache_entry$fit_result$converged)
    if (!is.finite(payload$score) || !converged) {
      payload$score <- NA_real_
      if (is.null(payload$error) || is.na(payload$error)) {
        payload$error <- "ESEM model did not return a converged scored solution."
      }
    }
    payload
  }, error = function(e) {
    list(score = NA_real_, key = key_fun(v), cache_entry = NULL,
         error = conditionMessage(e))
  })
}

# =================================================================
# 12  ACO_with_ESEM -- MAIN EXPORTED FUNCTION
# =================================================================
#' Ant Colony Optimization for Full-ESEM Scale Construction
#'
#' Optimizes item selection for a target factor structure using semantic similarity
#' and exploratory structural equation modeling (full-ESEM). Includes DFI-calibrated
#' cutoffs, heuristic pre-filtering, and parallel ACO search.
#'
#' @param cosine_sim_matrix Square symmetric matrix of cosine similarities.
#' @param df Dataframe with item metadata (`item`/`type` or `factor` columns).
#' @param model_type Currently unused placeholder for backward compatibility.
#' @param i.per.f Named integer vector of items per factor.
#' @param ants Number of ants in the colony.
#' @param max.iter Maximum consecutive non-improving iterations before patience
#'   stopping.
#' @param max_total_iter Optional hard ceiling for all ACO iterations. `NULL`
#'   preserves the legacy patience-only stopping behavior; use a positive
#'   integer to impose a resource budget.
#' @param max_esem_fits Optional hard ceiling for ESEM fits attempted during
#'   ACO search checkpoints. `NULL` imposes no separate checkpoint-fit ceiling.
#' @param esem_every Base interval for ESEM evaluations during ACO search.
#' @param run_esem_during_search Logical; if `FALSE`, ACO selection is based on
#'   semantic redundancy only and ESEM is run only for final diagnostics.
#' @param esem_weight Weight for the ESEM component in the objective function.
#' @param esem_failure_policy Behavior when an ESEM-guided search checkpoint
#'   produces no usable ESEM solutions. `"stop"` prevents a semantic-only
#'   selection from being mislabeled as ESEM-guided; `"semantic_fallback"`
#'   continues explicitly as a semantic/PFA-only search and records a warning.
#' @param esem_sample_size Sample size for DFI simulation and ESEM estimation.
#'   `"auto"` chooses a non-arbitrary reference N by RMSEA power analysis for
#'   detecting poor approximate fit (`reference_rmsea_poor`) against close fit
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
#'   solution as an approximate-fit diagnostic.
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
#' @param htmt_threshold HTMT validity threshold.
#' @param cohesion_quantile Deprecated inverse form of `cohesion_retention`.
#'   When supplied, retention is computed as `1 - cohesion_quantile`.
#' @param cohesion_retention Proportion of generated items nearest their
#'   factor's within-similarity target retained as ACO candidates before the
#'   minimum-pool safeguard is applied. Higher values retain broader content.
#' @param within_similarity_target Target within-factor semantic similarity.
#'   `NULL` estimates a dimension-specific target from the generated item pool.
#' @param within_similarity_band Tolerance around `within_similarity_target`.
#' @param facet_coverage_weight Soft weight rewarding coverage of distinct facets
#'   within each dimension.
#' @param psychometric_guard_weight Soft penalty strength for ESEM solutions with
#'   weak loadings, very low AVE, high HTMT, or improper standardized parameters.
#' @param psychometric_guard_min_ave,psychometric_guard_min_loading,psychometric_guard_min_primary_ge_50
#'   Minimum convergent-validity diagnostics used by the soft psychometric guard.
#' @param pfa_mode Sample-free pseudo-factor-analysis mode. `"diagnostic"`
#'   (the default) reports PFA diagnostics without adding them to the ACO
#'   objective, `"objective"` includes PFA simple-structure diagnostics in the
#'   ACO objective, and `"off"` disables them.
#' @param pfa_weight Weight for the PFA component in the semantic/PFA part of
#'   the ACO objective. The ESEM weight is applied after this composite is formed.
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
#' @param reference_max_n Maximum reference N searched by the RMSEA-power solver.
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
#' @param elite_pareto_rerank Logical; use final elite-archive ESEM diagnostics
#'   to rerank solutions with a Pareto-style quality bonus.
#' @param validation_data Optional item-response dataset for a final response-data
#'   ESEM validation fit using the selected items.
#' @param validation_ordered Optional ordered item names for ordinal response-data
#'   validation.
#' @param sigmoid_center Sigmoid center for semantic scoring.
#' @param sigmoid_steepness Sigmoid steepness for semantic scoring.
#' @param heuristic_beta Heuristic influence in ACO probability.
#' @param archive_stable_window Iterations of stable archive to trigger early stop.
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
#' @param n.cores Requested number of parallel workers. Values greater than
#'   `2` are capped at two workers for resource-conscious execution.
#' @param verbose Print progress.
#' @param ... Additional arguments (unused).
#'
#' @return A named list containing `best_items`, `esem_fit`, `dfi_cutoffs`,
#'   a unique `elite_archive`, psychometric indices, diagnostic metadata, and
#'   `selected_items_detail` when item text is available in `df`. Objective
#'   transparency fields include `proposal_objective_score`,
#'   `final_guided_objective_score`, `search_guidance_status`,
#'   `candidate_counts`, and ESEM attempt/success/failure telemetry.
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
    ants = 90, max.iter = 50, esem_every = 10, run_esem_during_search = TRUE,
    max_total_iter = NULL, max_esem_fits = NULL,
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
    final_dddfi = TRUE, final_dddfi_reps = 250L,
    final_dddfi_mad_target = c("close", "fair", "mediocre"),
    final_equivtest = TRUE,
    loading_pattern = "varied",
    embed_reliability = 1.0, residual_inflation = 0.0, dfi_warmup_iters = 5L,
    redundancy_threshold = 0.85, dup_threshold = 0.90, htmt_threshold = 0.85,
    cohesion_quantile = NULL, cohesion_retention = 0.75,
    within_similarity_target = NULL, within_similarity_band = 0.08,
    facet_coverage_weight = 0.15, psychometric_guard_weight = 0.50,
    psychometric_guard_min_ave = 0.30,
    psychometric_guard_min_loading = 0.40,
    psychometric_guard_min_primary_ge_50 = 0.70,
    pfa_mode = c("diagnostic", "objective", "off"),
    pfa_weight = 0.20,
    pfa_extraction = c("principal", "ml"),
    pfa_final_extraction = c("ml", "principal"),
    pfa_rotation = c("promax", "target_oblique", "oblimin", "varimax", "none"),
    pfa_min_loading = psychometric_guard_min_loading,
    pfa_min_margin = NULL,
    reference_rmsea_close = 0.05,
    reference_rmsea_poor = 0.08,
    reference_power = 0.80,
    reference_alpha = 0.05,
    reference_max_n = 5000L,
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
    elite_pareto_rerank = TRUE,
    validation_data = NULL, validation_ordered = NULL,
    heuristic_beta = 0.50, archive_stable_window = 8L, pheromone_update = c("top_elite", "best_ant"),
    fixed_evaporation = NULL, debug_mode = FALSE, keep_solution_history = TRUE,
    history_mode = c("full", "summary", "none"),
    use_parallel = TRUE, n.cores = 2L, verbose = TRUE, ...) {

  if (debug_mode) { pheromone_update <- "best_ant"; fixed_evaporation <- 0.05; archive_stable_window <- 3L; verbose <- TRUE }
  pheromone_update <- match.arg(pheromone_update)
  esem_failure_policy <- match.arg(esem_failure_policy)
  dfi_mode <- match.arg(dfi_mode)
  dfi_esem_strategy <- match.arg(dfi_esem_strategy)
  dfi_fallback_policy <- match.arg(dfi_fallback_policy)
  final_dddfi_mad_target <- match.arg(final_dddfi_mad_target)
  pfa_mode <- match.arg(pfa_mode)
  pfa_extraction <- match.arg(pfa_extraction)
  pfa_final_extraction <- match.arg(pfa_final_extraction)
  pfa_rotation <- match.arg(pfa_rotation)
  semantic_esem_score_mode <- match.arg(semantic_esem_score_mode)
  history_mode <- match.arg(history_mode)
  if (!isTRUE(keep_solution_history)) history_mode <- "none"
  requested_n_cores <- suppressWarnings(as.integer(n.cores[1L]))
  if (length(requested_n_cores) != 1L || !is.finite(requested_n_cores) || requested_n_cores < 1L) {
    stop("'n.cores' must be a positive integer.")
  }
  n.cores <- .semantica_max_workers(requested_n_cores)
  if (isTRUE(use_parallel) && requested_n_cores > n.cores) {
    warning("'n.cores' is capped at 2 parallel workers for resource-conscious execution.", call. = FALSE)
  }
  dots <- list(...)
  if (!is.null(dots$cfa_every)) esem_every <- dots$cfa_every
  if (!is.null(dots$cfa_weight)) esem_weight <- dots$cfa_weight
  if (!is.null(dots$cfa_sample_size)) esem_sample_size <- dots$cfa_sample_size
  esem_every <- max(1L, as.integer(esem_every))
  ants <- max(1L, as.integer(ants))
  max.iter <- max(1L, as.integer(max.iter))
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
  reference_max_n <- max(50L, as.integer(reference_max_n))
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
  elite_pareto_rerank <- isTRUE(elite_pareto_rerank)

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
  item.vector <- unlist(list.items, use.names = FALSE)
  item.factor.lookup <- stats::setNames(
    rep(factors, vapply(list.items, length, integer(1L))),
    item.vector
  )
  item.facet.lookup <- stats::setNames(item_facets_all[match(item.vector, item_ids)], item.vector)
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
  if (duplicate_clusters$n_clusters > 0L) {
    for (f in factors) {
      f_items <- list.items[[f]]
      cid <- duplicate_cluster_id[f_items]
      effective_units <- sum(is.na(cid) | !nzchar(cid)) + length(unique(cid[!is.na(cid) & nzchar(cid)]))
      if (effective_units < i.per.f[[f]]) duplicate_guard_infeasible <- TRUE
    }
  }

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
        cat("  Duplicate guard: active, but one or more factors have too few unique clusters; local fallback sampling may be used.\n")
      }
    }
  }
  within_similarity_target_eff <- estimate_within_similarity_targets(
    list.items, cosine_sim_matrix, factors, within_similarity_target,
    lower = 0.25, upper = min(0.55, max(0.30, redundancy_threshold - 0.05))
  )
  eligible.items <- compute_eligible_items(
    list.items, cosine_sim_matrix, factors, i.per.f, cohesion_retention, 0.15,
    within_similarity_target_eff, within_similarity_band
  )
  item_heuristics <- compute_item_heuristics(
    eligible.items, cosine_sim_matrix, factors,
    within_similarity_target_eff, within_similarity_band
  )
  if (verbose) {
    cat(sprintf("  Candidate retention: %.1f%% nearest-target pool retained before minimum-pool safeguard\n",
                100 * cohesion_retention))
    for (f in factors) cat(sprintf("  %-28s: %d generated -> %d eligible -> %d selected target\n",
                                   f, length(list.items[[f]]), length(eligible.items[[f]]), i.per.f[[f]]))
  }
  candidate_counts <- data.frame(
    factor = factors,
    generated = vapply(list.items[factors], length, integer(1L)),
    eligible = vapply(eligible.items[factors], length, integer(1L)),
    selected_target = as.integer(i.per.f[factors]),
    stringsAsFactors = FALSE
  )

  solution_cache <- new.env(hash = TRUE, parent = emptyenv())
  solution_history_env <- if (history_mode != "none") {
    e <- new.env(hash = FALSE, parent = emptyenv())
    e$n <- 0L
    e$history <- list()
    e
  } else NULL
  pheromone <- matrix(1.0, nrow = n_items, ncol = 2L, dimnames = list(item.vector, c("not_selected", "selected")))

  heuristic_cutoffs <- compute_heuristic_cutoffs(n_factors, i.per.f, esem_sample_size)
  if (verbose) {
    phase1_label <- if (run_esem_during_search) {
      "DFI CUTOFFS (ESEM-FITTED, TWO-PASS)"
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
    htmt_threshold = htmt_threshold, within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band, facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode, pfa_weight = pfa_weight, pfa_extraction = pfa_extraction,
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
  dfi_loading_source <- if (run_esem_during_search) "prior-based" else "not-used"
  warmup_esem_items <- NULL
  warmup_esem_fa <- NULL
  if (run_esem_during_search && !is.null(warmup_best_vector)) {
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
    detected <- suppressWarnings(parallel::detectCores())
    if (!is.finite(detected)) detected <- 1L
    if (use_parallel && n.cores > 1L) min(as.integer(n.cores), as.integer(detected)) else 1L
  }
  search_dfi_cl <- NULL
  get_search_dfi_cluster <- function() {
    if (is.null(search_dfi_cl) && dfi_n_cores() > 1L) {
      search_dfi_cl <<- .semantica_make_dfi_cluster(dfi_n_cores())
    }
    search_dfi_cl
  }
  on.exit({
    if (!is.null(search_dfi_cl)) try(parallel::stopCluster(search_dfi_cl), silent = TRUE)
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
    try(parallel::stopCluster(search_dfi_cl), silent = TRUE)
    search_dfi_cl <- NULL
  }

  if (run_esem_during_search && dfi_mode == "strict_cfa_dfi") {
    strict_dfi_cutoffs <- compute_dfi_cutoffs_from_model_spec(
      factors, i.per.f, esem_sample_size,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_loadings else NULL,
      if (!is.null(dfi_population_params)) dfi_population_params$fitted_factor_cors else NULL,
      loading_pattern, target_loadings, target_factor_cors, embed_reliability,
      residual_inflation, data_type, original_data, NULL, dfi_reps, dfi_level,
      dfi_criterion, verbose, dfi_loading_source
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
      dfi_criterion, verbose, dfi_loading_source
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
      dfi_level, dfi_criterion, verbose, dfi_loading_source
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
    htmt_threshold = htmt_threshold, within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band, facet_coverage_weight = facet_coverage_weight,
    psychometric_guard_weight = psychometric_guard_weight,
    psychometric_guard_min_ave = psychometric_guard_min_ave,
    psychometric_guard_min_loading = psychometric_guard_min_loading,
    psychometric_guard_min_primary_ge_50 = psychometric_guard_min_primary_ge_50,
    pfa_mode = pfa_mode, pfa_weight = pfa_weight, pfa_extraction = pfa_extraction,
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
    search_label <- if (run_esem_during_search) "full-ESEM" else "semantic-only"
    cat(sprintf("\nPHASE 2 -- ACO OPTIMIZATION v8 (%s)\n  Cutoffs: %s | CFI >=%.3f RMSEA <=%.3f SRMR <=%.3f\n", search_label, cutoff_source, active_cutoffs$cfi, active_cutoffs$rmsea, active_cutoffs$srmr))
    cat(sprintf("  Ants: %d | Max patience: %d | ESEM search: %s | ESEM weight: %.0f%% | ESEM ants/iter: %d | Cores: %d\n", ants, max.iter, if (run_esem_during_search) "on" else "off", esem_weight * 100, esem_eval_top_k_eff, if (use_parallel && n.cores > 1L) n.cores else 1L))
    cat(sprintf("  Resource budget : total iterations <= %s | search ESEM fits <= %s | history=%s\n",
                if (is.infinite(max_total_iter)) "Inf" else as.character(max_total_iter),
                if (is.infinite(max_esem_fits)) "Inf" else as.character(max_esem_fits),
                history_mode))
    cat(sprintf("  Within-targets : %s | band=%.3f | facet weight=%.2f | guard weight=%.2f\n", paste(names(within_similarity_target_eff), sprintf("%.3f", within_similarity_target_eff), sep = "=", collapse = ", "), within_similarity_band, facet_coverage_weight, psychometric_guard_weight))
  }

  cl <- NULL
  stop_search_cluster <- function() {
    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
      cl <<- NULL
    }
    invisible(NULL)
  }
  on.exit(stop_search_cluster(), add = TRUE)
  if (use_parallel && n.cores > 1L) {
    cl <- parallel::makeCluster(n.cores, type = "PSOCK")
    parallel::clusterEvalQ(cl, { suppressPackageStartupMessages({ library(lavaan); library(Matrix) }) })
    export_env <- new.env(parent = emptyenv())
    export_env$cosine_sim_matrix <- cosine_sim_matrix[item.vector, item.vector, drop = FALSE]
    export_env$list.items <- list.items; export_env$eligible.items <- eligible.items
    export_env$factors <- factors; export_env$i.per.f <- i.per.f; export_env$item.vector <- item.vector; export_env$model_info <- model_info; export_env$item.factor.lookup <- item.factor.lookup; export_env$item.facet.lookup <- item.facet.lookup; export_env$facets.by.factor <- facets.by.factor
    fns <- c("%||%", "fit.function.v2", ".semantica_evaluate_esem_worker", "build_esem_syntax_safe", "build_esem_target_matrix", "prepare_esem_rotation_args", "sanitize_lavaan_name", "extract_similarity_submatrix", "compute_semantic_sim_index_v2", "compute_manual_srmr", "transform_cosine_for_esem", "run_esem_on_matrix", "extract_and_score_esem", "compute_ave_esem", "compute_htmt_esem", "compute_esem_structure_diagnostics", "compute_duplicate_penalty", "compute_facet_coverage_multiplier", "compute_psychometric_guard_penalty", "compute_pfa_diagnostics", "extract_pfa_loadings", "build_pfa_target_matrix", "apply_pfa_loading_rotation", "pfa_harmonic_mean", "efa_degrees_of_freedom", "check_near_duplicates", "fisherz", "fisherz_inv", "make_solution_key")
    for (fn in fns) if (exists(fn, mode = "function")) export_env[[fn]] <- get(fn)
    parallel::clusterExport(cl, varlist = ls(export_env), envir = export_env)
  }

  requested_esem_search <- run_esem_during_search
  search_guidance_status <- if (requested_esem_search) "esem_guided" else "semantic_only_requested"
  esem_error_log <- character(0)
  esem_successes <- 0L
  best_obj <- -Inf; best_proposal_obj <- -Inf; best_vector <- NULL
  patience <- 0L; iteration <- 0L; run_counter <- 0L; esem_attempts <- 0L; esem_failures <- 0L
  recent_tops <- list(semantic = numeric(0), esem_guided = numeric(0))
  stagnation_window <- 10L; elite_archive <- list(); archive_sig_history <- character(0); archive_stable_count <- 0L
  termination_reason <- "patience_exhausted"

  while (patience < max.iter && iteration < max_total_iter) {
    if (run_esem_during_search && is.finite(max_esem_fits) && esem_attempts >= max_esem_fits &&
        length(elite_archive) > 0L) {
      termination_reason <- "max_esem_fits_reached"
      break
    }
    iteration <- iteration + 1L
    ph_entropy <- compute_pheromone_entropy(pheromone)
    esem_interval <- max(1L, floor(esem_every / 2L), round(esem_every * ph_entropy))
    do_esem <- run_esem_during_search && (iteration %% esem_interval == 0L)
    # Elite entries must be comparable across checkpoints; apply the declared
    # ESEM weight whenever a solution is scored by ESEM.
    effective_esem_weight <- if (!do_esem) 0 else esem_weight
    rho <- if (!is.null(fixed_evaporation)) as.numeric(fixed_evaporation) else { progress <- min(1.0, (iteration + patience) / (max.iter + patience + 1)); max(0.05, min(0.40, 0.35 - 0.25 * progress)) }

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
                      solution_cache = solution_cache,
                      solution_history_env = if (history_mode == "full") solution_history_env else NULL),
      error = function(e) NA_real_
    )
    proposal_objectives <- vapply(ant_solutions, eval_sem_fn, numeric(1L))
    score_stage <- "semantic"
    scored_solutions <- ant_solutions
    scored_objectives <- proposal_objectives
    archive_entries <- list()
    archive_updated <- FALSE

    esem_failed <- 0L
    if (do_esem) {
        esem_candidates <- which(is.finite(proposal_objectives) & !is.na(proposal_objectives))
        if (length(esem_candidates) > 0L) {
          esem_candidates <- esem_candidates[order(proposal_objectives[esem_candidates], decreasing = TRUE)]
          esem_candidates <- esem_candidates[seq_len(min(length(esem_candidates), esem_eval_top_k_eff))]
          if (is.finite(max_esem_fits)) {
            remaining_esem_fits <- max(0L, as.integer(max_esem_fits - esem_attempts))
            esem_candidates <- utils::head(esem_candidates, remaining_esem_fits)
          }
          if (length(esem_candidates) == 0L) {
            termination_reason <- "max_esem_fits_reached"
            break
          }
          esem_attempts <- esem_attempts + length(esem_candidates)
        eval_esem_payload <- function(v) {
          tryCatch({
            key <- make_solution_key(v)
            score <- fit.function.v2(
              v, run_esem_now = TRUE, effective_esem_weight = effective_esem_weight,
              solution_cache = solution_cache, solution_history_env = NULL
            )
            cache_entry <- cache_get(solution_cache, key)
            converged <- !is.null(cache_entry$fit_result) && isTRUE(cache_entry$fit_result$converged)
            if (!is.finite(score) || !converged) {
              return(list(score = NA_real_, key = key, cache_entry = cache_entry,
                          error = "ESEM model did not return a converged scored solution."))
            }
            list(score = score, key = key, cache_entry = cache_entry, error = NA_character_)
          }, error = function(e) {
            list(score = NA_real_, key = make_solution_key(v), cache_entry = NULL,
                 error = conditionMessage(e))
          })
        }
        esem_payloads <- if (!is.null(cl)) {
          tasks <- lapply(ant_solutions[esem_candidates], function(v) {
            list(vector = v, effective_esem_weight = effective_esem_weight)
          })
          parallel::parLapply(cl, tasks, .semantica_evaluate_esem_worker)
        } else {
          lapply(ant_solutions[esem_candidates], eval_esem_payload)
        }
        for (payload in esem_payloads) {
          if (!is.null(payload$key) && !is.null(payload$cache_entry)) {
            cache_set(solution_cache, payload$key, payload$cache_entry)
          }
        }
        esem_vals <- vapply(esem_payloads, function(x) x$score, numeric(1L))
        errors_now <- vapply(esem_payloads, function(x) x$error %||% NA_character_, character(1L))
        errors_now <- errors_now[!is.na(errors_now) & nzchar(errors_now)]
        if (length(errors_now) > 0L) esem_error_log <- unique(c(esem_error_log, errors_now))
        ok_esem <- is.finite(esem_vals)
        esem_failed <- sum(!ok_esem)
        esem_successes <- esem_successes + sum(ok_esem)
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
          score_stage <- "esem_guided"
          successful_ix <- esem_candidates[ok_esem]
          scored_solutions <- ant_solutions[successful_ix]
          scored_objectives <- esem_vals[ok_esem]
          archive_entries <- lapply(seq_along(successful_ix), function(i) {
            list(
              vec = ant_solutions[[successful_ix[i]]],
              obj = scored_objectives[i],
              proposal_obj = proposal_objectives[successful_ix[i]],
              iteration = iteration,
              do_esem = TRUE,
              esem_success = TRUE,
              score_type = "esem_guided"
            )
          })
        } else if (identical(esem_failure_policy, "stop")) {
          first_error <- if (length(esem_error_log) > 0L) esem_error_log[1L] else "no converged ESEM solution"
          stop(sprintf(
            "Search-time ESEM failed for all %d candidate solutions at iteration %d. First failure: %s. No ESEM-guided ACO result was produced.",
            length(esem_candidates), iteration, first_error
          ))
        } else {
          run_esem_during_search <- FALSE
          search_guidance_status <- "semantic_fallback_after_esem_failure"
          score_stage <- "semantic"
          scored_solutions <- ant_solutions
          scored_objectives <- proposal_objectives
        }
      }
    }

    n_failed <- sum(!is.finite(scored_objectives))
    if (do_esem) esem_failures <- esem_failures + esem_failed
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
    if (!requested_esem_search || identical(search_guidance_status, "semantic_fallback_after_esem_failure")) {
      archive_entries <- list(list(vec = best_ant_vec, obj = best_ant_obj,
                                   proposal_obj = best_ant_obj, iteration = iteration,
                                   do_esem = FALSE, esem_success = FALSE,
                                   score_type = "semantic"))
    }
    if (length(archive_entries) > 0L) {
      elite_archive <- update_elite_archive(elite_archive, archive_entries, elite_k)
      archive_updated <- TRUE
      archive_sig_now <- paste(vapply(elite_archive, function(e) solution_signature(e$vec), character(1L)), collapse = "|")
      if (length(archive_sig_history) > 0L && archive_sig_now == tail(archive_sig_history, 1L)) {
        archive_stable_count <- archive_stable_count + 1L
      } else {
        archive_stable_count <- 0L
      }
      archive_sig_history <- c(archive_sig_history, archive_sig_now)
    }

    benchmark <- if (identical(score_stage, "esem_guided") ||
                     !requested_esem_search ||
                     identical(search_guidance_status, "semantic_fallback_after_esem_failure")) {
      best_obj
    } else {
      best_proposal_obj
    }
    improved <- best_ant_obj > benchmark
    if (improved) {
      if (identical(score_stage, "esem_guided") ||
          !requested_esem_search ||
          identical(search_guidance_status, "semantic_fallback_after_esem_failure")) {
        best_obj <- best_ant_obj
        best_vector <- best_ant_vec
      } else {
        best_proposal_obj <- best_ant_obj
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
            within_similarity_band = model_info$within_similarity_band
          )$raw_index
        } else NA_real_
        cached_best <- cache_get(solution_cache, make_solution_key(best_ant_vec))
        label_obj <- if (identical(score_stage, "esem_guided") || !requested_esem_search) best_obj else best_proposal_obj
        cat(sprintf("Run %3d | * NEW %s BEST * | Score: %.4f | SemRaw: %.4f | rho=%.3f | H=%.3f\n",
                    run_counter, toupper(score_stage), label_obj,
                    if (!is.na(sem_idx)) sem_idx else -99, rho, ph_entropy))
        if (!is.null(cached_best) && !is.null(cached_best$fit_result) && !is.null(cached_best$fit_result$score_decomp)) {
          d <- cached_best$fit_result$score_decomp
          cat(sprintf("         Sem=%.4f | CFI_s=%.3f | RMSEA_s=%.3f | SRMR_s=%.3f | AVE_s=%.3f | LQ=%.3f | HTMT_pen=%.3f | ESEM_score=%.4f\n", cached_best$sem_score, d$cfi_s, d$rmsea_s, d$srmr_s, d$ave_score, d$loading_quality, d$htmt_penalty, d$final_score))
        }
      }
    } else if (verbose && iteration %% 5L == 0L) {
      cat(sprintf("Run %3d | Stage: %-11s | Best: %.4f | Pat: %d/%d | rho=%.3f | H=%.3f | ESEM@%d | Fail: %d/%d\n",
                  run_counter, score_stage, best_ant_obj, patience, max.iter,
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
    if (archive_updated && archive_stable_count >= archive_stable_window) {
      termination_reason <- "archive_stable"
      if (verbose) cat(sprintf("\n  [STOP] Archive stable for %d scored checkpoints -- converged.\n", archive_stable_window))
      break
    }
  }
  if (identical(termination_reason, "patience_exhausted") && iteration >= max_total_iter && patience < max.iter) {
    termination_reason <- "max_total_iter_reached"
    if (verbose) cat(sprintf("\n  [STOP] Reached the hard ACO iteration ceiling (%s).\n", as.character(max_total_iter)))
  }
  stop_search_cluster()

  if (length(elite_archive) == 0L) {
    stop("No comparable elite solutions were available for final evaluation. Increase the search budget or inspect ESEM checkpoint failures.")
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat("PHASE 3 -- FINAL EVALUATION\n")
    cat("============================================================\n")
    cat(sprintf("  Finalizing %d unique archived solutions from %d iterations; search ESEM fits: %d/%d succeeded.\n",
                length(elite_archive), iteration, esem_attempts - esem_failures,
                esem_attempts))
    cat(sprintf("  Search guidance   : %s\n", search_guidance_status))
    cat("  Final phase order: choose the best archive solution, retain its full ESEM fit, then run requested post-selection diagnostics.\n")
  }

  .evaluate_archive_solution <- function(entry) {
    v <- entry$vec; sel_items <- item.vector[v == 1L]; fa <- item.factor.lookup[sel_items]
    cos_sub <- tryCatch(extract_similarity_submatrix(cosine_sim_matrix, sel_items), error = function(e) NULL)
    if (is.null(cos_sub)) return(list(score = -1e6))
    sem_r <- compute_semantic_sim_index_v2(
      cos_sub, sel_items, fa, factors, redundancy_threshold, sigmoid_center, sigmoid_steepness,
      within_similarity_target = within_similarity_target_eff,
      within_similarity_band = within_similarity_band
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
      pfa_score_final <- if (isTRUE(pfa_r$available)) pfa_r$score else 0
      search_score_final <- (1 - pfa_weight) * sem_score_final + pfa_weight * pfa_score_final
    }
    if (!run_esem_during_search || esem_weight <= 0) {
      return(list(score = search_score_final))
    }
    esem_cor <- transform_cosine_for_esem(cos_sub, fa, factors)
    if (is.null(esem_cor)) return(list(score = -1e6))
    syntax <- build_esem_syntax_safe(sel_items, fa, factors)
    archive_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, sel_items, fa, factors)
    esem_fit <- run_esem_on_matrix(
      syntax, esem_cor, esem_sample_size, model_info$estimator, rotation, archive_rotation_args,
      iter_max = full_esem_iter_max, fallback = TRUE
    )
    r <- extract_and_score_esem(
      esem_fit, esem_cor, fa, factors, active_cutoffs, htmt_threshold,
      score_mode = semantic_esem_score_mode
    )
    guard_pen <- compute_psychometric_guard_penalty(
      r,
      min_ave = psychometric_guard_min_ave,
      min_primary_loading = psychometric_guard_min_loading,
      min_primary_prop_ge_50 = psychometric_guard_min_primary_ge_50
    )
    base_total <- ((1 - esem_weight) * search_score_final + esem_weight * r$score) * (guard_pen ^ psychometric_guard_weight)
    if (elite_pareto_rerank && !is.null(r$structure_diagnostics)) {
      sdg <- r$structure_diagnostics
      pareto_bonus <- mean(c(
        min(1, max(0, r$ave / 0.50)),
        min(1, max(0, sdg$primary_ge_50 / max(psychometric_guard_min_primary_ge_50, 1e-6))),
        min(1, max(0, sdg$correct_dominance)),
        min(1, max(0, sdg$simple_structure))
      ), na.rm = TRUE)
      base_total <- 0.85 * base_total + 0.15 * pareto_bonus
    }
    list(
      score = base_total,
      esem_fit = esem_fit,
      esem_cor = esem_cor,
      syntax = syntax,
      rotation_args = archive_rotation_args
    )
  }

  best_archive_evaluation <- NULL
  best_archive_evaluation_idx <- NA_integer_
  best_archive_evaluation_score <- -Inf
  archive_final_scores <- vapply(seq_along(elite_archive), function(archive_idx) {
    evaluated <- .evaluate_archive_solution(elite_archive[[archive_idx]])
    score <- evaluated$score %||% -1e6
    if (is.finite(score) && score > best_archive_evaluation_score) {
      best_archive_evaluation <<- evaluated
      best_archive_evaluation_idx <<- archive_idx
      best_archive_evaluation_score <<- score
    }
    score
  }, numeric(1L))

  best_archive_idx <- which.max(archive_final_scores)
  if (is.na(best_archive_evaluation_idx) || best_archive_evaluation_idx != best_archive_idx) {
    best_archive_evaluation <- .evaluate_archive_solution(elite_archive[[best_archive_idx]])
  }
  best_vector <- elite_archive[[best_archive_idx]]$vec
  best_items <- item.vector[best_vector == 1L]
  factor_assignment <- item.factor.lookup[best_items]

  cos_sub_best <- extract_similarity_submatrix(cosine_sim_matrix, best_items)
  sem_final <- compute_semantic_sim_index_v2(
    cos_sub_best, best_items, factor_assignment, factors, redundancy_threshold, sigmoid_center, sigmoid_steepness,
    within_similarity_target = within_similarity_target_eff,
    within_similarity_band = within_similarity_band
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
      0
    }
    final_search_objective_score <- (1 - pfa_weight) * final_semantic_objective_score +
      pfa_weight * final_pfa_objective_score
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
    final_pfa_score <- if (isTRUE(final_pfa_diagnostics$available)) final_pfa_diagnostics$score else 0
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
      final_esem_fit <- run_esem_on_matrix(
        final_syntax, final_esem_cor, esem_sample_size, model_info$estimator, rotation, final_rotation_args,
        iter_max = full_esem_iter_max, fallback = TRUE
      )
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
        if (!is.null(final_dfi_cl)) try(parallel::stopCluster(final_dfi_cl), silent = TRUE)
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
          n_cores = {
            detected <- suppressWarnings(parallel::detectCores())
            if (!is.finite(detected)) detected <- 1L
            if (use_parallel && n.cores > 1L) min(as.integer(n.cores), as.integer(detected)) else 1L
          },
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
          n_cores = {
            detected <- suppressWarnings(parallel::detectCores())
            if (!is.finite(detected)) detected <- 1L
            if (use_parallel && n.cores > 1L) min(as.integer(n.cores), as.integer(detected)) else 1L
          },
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
          n_cores = {
            detected <- suppressWarnings(parallel::detectCores())
            if (!is.finite(detected)) detected <- 1L
            if (use_parallel && n.cores > 1L) min(as.integer(n.cores), as.integer(detected)) else 1L
          },
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
        try(parallel::stopCluster(final_dfi_cl), silent = TRUE)
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
      score_mode = semantic_esem_score_mode
    )

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

  split_half_stability <- tryCatch({
    all_pairs <- which(lower.tri(cos_sub_best), arr.ind = TRUE)
    if (nrow(all_pairs) >= 4L) {
      half <- sample(seq_len(nrow(all_pairs)), floor(nrow(all_pairs) / 2))
      mat_A <- mat_B <- cos_sub_best; mat_A[all_pairs[-half, , drop = FALSE]] <- 0; mat_B[all_pairs[half, , drop = FALSE]] <- 0
      mat_A <- (mat_A + t(mat_A)) / 2; diag(mat_A) <- 1; mat_B <- (mat_B + t(mat_B)) / 2; diag(mat_B) <- 1
      sem_A <- compute_semantic_sim_index_v2(
        mat_A, best_items, factor_assignment, factors,
        within_similarity_target = within_similarity_target_eff,
        within_similarity_band = within_similarity_band
      )$raw_index
      sem_B <- compute_semantic_sim_index_v2(
        mat_B, best_items, factor_assignment, factors,
        within_similarity_target = within_similarity_target_eff,
        within_similarity_band = within_similarity_band
      )$raw_index
      list(sem_half_A = sem_A, sem_half_B = sem_B, difference = abs(sem_A - sem_B), stable = abs(sem_A - sem_B) < 0.10)
    } else list(sem_half_A = NA, sem_half_B = NA, difference = NA, stable = NA)
  }, error = function(e) list(sem_half_A = NA, sem_half_B = NA, difference = NA, stable = NA))

  solution_history_list <- if (!is.null(solution_history_env) && solution_history_env$n > 0L) head(solution_history_env$history, solution_history_env$n) else NULL

  run_warnings <- character(0)
  if (!isTRUE(split_half_stability$stable) && !is.na(split_half_stability$stable)) run_warnings <- c(run_warnings, sprintf("Split-half semantic instability (diff=%.4f)", split_half_stability$difference))
  if (!is.null(dfi_cutoffs) && isTRUE(dfi_cutoffs$was_degenerate)) run_warnings <- c(run_warnings, "DFI cutoffs degenerate -- heuristics used")
  if (requested_esem_search && is.null(bootstrap_params)) {
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
  if (validation_n_diagnostic && (is.null(recommended_validation_n) ||
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
    run_warnings <- c(run_warnings, "Duplicate-cluster guard partly infeasible for at least one factor; duplicate penalty remained active")
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
      "Search-time ESEM scoring failed for %d of %d attempted solutions",
      esem_failures, esem_attempts
    ))
  }
  if (identical(search_guidance_status, "semantic_fallback_after_esem_failure")) {
    run_warnings <- c(
      run_warnings,
      "ACO continued in explicit semantic fallback mode; final ESEM diagnostics did not guide selection"
    )
  }

  compact_summary <- list(best_items = best_items, factor_assignment = factor_assignment, esem_syntax = final_syntax,
                          cfi = final_esem_result$cfi, rmsea = final_esem_result$rmsea, srmr = final_esem_result$srmr,
                          ave = final_esem_result$ave, factor_ave = final_esem_result$factor_ave,
                          htmt_max = final_esem_result$htmt_max,
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
                          raw_sem_index = sem_final$raw_index,
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
                          cohesion_retention = cohesion_retention,
                          search_guidance_status = search_guidance_status,
                          termination_reason = termination_reason,
                          total_iterations = iteration,
                          max_total_iter = max_total_iter,
                          max_esem_fits = max_esem_fits,
                          history_mode = history_mode,
                          esem_attempts = esem_attempts,
                          esem_successes = esem_successes,
                          esem_failures = esem_failures,
                          duplicate_clusters = duplicate_clusters,
                          dfi_mode = dfi_mode,
                          dfi_loading_source = dfi_loading_source,
                          split_half_stable = split_half_stability$stable, warnings = if (length(run_warnings) > 0L) run_warnings else "none")

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
                 esem_syntax = final_syntax, esem_fit = final_esem_fit,
                 esem_result = final_esem_result,
                 semantic_index = sem_final$raw_index, semantic_score = sem_final$sem_score,
                 semantic_objective_score = final_semantic_objective_score,
                 search_objective_score = final_search_objective_score,
                 proposal_objective_score = final_search_objective_score,
                 final_guided_objective_score = archive_final_scores[best_archive_idx],
                 pfa_score = final_pfa_score,
                 pfa_diagnostics = final_pfa_diagnostics,
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
                 elite_archive = elite_archive, elite_archive_scores = archive_final_scores, total_iterations = iteration,
                 termination_reason = termination_reason,
                 max_total_iter = max_total_iter, max_esem_fits = max_esem_fits,
                 history_mode = history_mode,
                 esem_attempts = esem_attempts, esem_successes = esem_successes,
                 esem_failures = esem_failures, esem_error_log = esem_error_log,
                 pheromone = pheromone, model_info = model_info, eligible_items = eligible.items,
                 candidate_counts = candidate_counts, cohesion_retention = cohesion_retention,
                 search_guidance_status = search_guidance_status,
                 duplicate_clusters = duplicate_clusters, duplicate_cluster_id = duplicate_cluster_id,
                 esem_cor_matrix = final_esem_cor,
                 semantic_similarity_reduction = semantic_similarity_reduction,
                 split_half_stability = split_half_stability, solution_history = solution_history_list, summary = compact_summary)
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
#' @param ... Additional arguments passed to ACO_with_ESEM.
#' @return List with consensus items, item frequencies, seed-level objective
#'   and ESEM-scoring telemetry, a selection matrix, and pairwise Jaccard
#'   agreement across successful seeds.
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
  n_seeds <- length(seeds); all_results <- vector("list", n_seeds)
  for (s_idx in seq_along(seeds)) {
    seed <- seeds[s_idx]
    if (verbose_seeds) cat(sprintf("\nMULTI-SEED RUN %d/%d (seed = %d)\n", s_idx, n_seeds, seed))
    set.seed(seed)
    all_results[[s_idx]] <- tryCatch(ACO_with_ESEM(cosine_sim_matrix = cosine_sim_matrix, df = df, i.per.f = i.per.f, ...), error = function(e) { message(sprintf("[Seed %d] ACO failed: %s", seed, conditionMessage(e))); NULL })
  }
  valid <- Filter(Negate(is.null), all_results); n_ok <- length(valid)
  if (n_ok == 0L) { warning("All seeds failed. Returning NULL."); return(NULL) }

  all_selected <- unlist(lapply(valid, function(r) r$best_items))
  item_freq <- sort(table(all_selected), decreasing = TRUE)
  score_dist <- vapply(valid, function(r) r$best_objective, numeric(1L))
  proposal_dist <- vapply(valid, function(r) r$proposal_objective_score %||% r$search_objective_score %||% NA_real_, numeric(1L))
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
    attempted = vapply(valid, function(r) as.integer(r$esem_attempts %||% 0L), integer(1L)),
    succeeded = vapply(valid, function(r) as.integer(r$esem_successes %||% ((r$esem_attempts %||% 0L) - (r$esem_failures %||% 0L))), integer(1L)),
    failed = vapply(valid, function(r) as.integer(r$esem_failures %||% 0L), integer(1L)),
    final_objective = score_dist,
    proposal_objective = proposal_dist,
    stringsAsFactors = FALSE
  )
  majority_threshold <- ceiling(n_ok / 2)
  consensus_items <- names(item_freq[item_freq >= majority_threshold])

  if (verbose_seeds) {
    cat(sprintf("\n  Seeds run      : %d / %d succeeded\n", n_ok, n_seeds))
    cat(sprintf("  Final objective: min=%.4f | median=%.4f | max=%.4f\n", min(score_dist), median(score_dist), max(score_dist)))
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
  list(
    item_frequencies = item_freq,
    score_distribution = score_dist,
    proposal_score_distribution = proposal_dist,
    consensus_items = consensus_items,
    selection_matrix = selection_matrix,
    pairwise_jaccard = pairwise_jaccard,
    mean_pairwise_jaccard = if (length(pairwise_jaccard) > 0L) mean(pairwise_jaccard) else NA_real_,
    esem_telemetry = esem_telemetry,
    n_successful = n_ok,
    all_results = all_results
  )
}

# =================================================================
# 14, 15, 16  REPORTING & INSPECTION UTILITIES
# =================================================================
#' Print SEMANTICA v8 results report
#' @param result Output from ACO_with_ESEM.
#' @param digits Decimal places for formatting.
#' @return Invisibly returns `result`.
#' @export
#' @examples
#' \dontrun{
#' report_semantica_v2(result, digits = 3L)
#' }
report_semantica_v2 <- function(result, digits = 4) {
  cat("\n===========================================================-\n")
  cat("|        SEMANTICA v8 -- RESULTS REPORT (full-ESEM)          |\n")
  cat("============================================================\n\n")
  cat("-- SELECTED ITEMS ------------------------------------------\n")
  for (f in unique(result$factor_assignment)) {
    f_items <- names(result$factor_assignment[result$factor_assignment == f])
    cat(sprintf("  %-28s: %s\n", f, paste(f_items, collapse = ", ")))
  }
  esem_syn <- result$esem_syntax
  cat("\n-- FINAL ESEM SYNTAX -------------------------------------\n", esem_syn, "\n")
  cat("\n-- FIT INDICES vs CUTOFFS --------------------------------\n")
  cr <- result$esem_result; ac <- result$active_cutoffs
  fmt_line <- function(name, val, cutoff, direction = " >= ") {
    pass_fail <- if (is.na(val) || is.na(cutoff)) "N/A" else if (direction == " >= ") { if (val >= cutoff) "PASS" else "FAIL" } else { if (val <= cutoff) "PASS" else "FAIL" }
    cutoff_txt <- if (is.na(cutoff)) "unavailable" else sprintf("%.3f", cutoff)
    cat(sprintf("  %-8s = %s  (%s %s)  [%s]\n", name, if (is.na(val)) "  NA   " else sprintf("%.4f", val), direction, cutoff_txt, pass_fail))
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
      cat(sprintf("  Proxy N structure: %s | dominance floor=%s | median primary range=%s\n",
                  if (isTRUE(sm$structurally_stable)) "stable across anchors" else "changed across anchors",
                  if (is.finite(sm$dominant_factor_agreement_floor)) sprintf("%.3f", sm$dominant_factor_agreement_floor) else "NA",
                  if (is.finite(sm$median_primary_loading_range)) sprintf("%.3f", sm$median_primary_loading_range) else "NA"))
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
    cat(sprintf("  HTMT penalty     : %.4f\n", d$htmt_penalty))
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
      cat("  AVE note    : below conventional response-data AVE; interpret with PFA recovery, loading dominance, HTMT, and later response-data validation.\n")
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
  fmt_line("HTMT max", result$htmt_max, result$model_info$htmt_threshold, " <= ")
  cat(sprintf("  HTMT violations: %d\n", if (is.infinite(result$htmt_violations)) 999L else as.integer(result$htmt_violations)))
  cat(sprintf("  Loading quality (dominant): %.4f\n", result$loading_quality))
  if (!is.null(result$structure_diagnostics)) {
    sdg <- result$structure_diagnostics
    pct <- function(x) if (is.finite(x)) sprintf("%.1f%%", 100 * x) else "NA"
    num <- function(x) if (is.finite(x)) sprintf("%.4f", x) else "NA"
    cat("  ESEM structure diagnostics:\n")
    cat(sprintf("    Dominant loading mean/median/min: %s / %s / %s\n",
                num(sdg$mean_primary_loading), num(sdg$median_primary_loading), num(sdg$min_primary_loading)))
    cat(sprintf("    Correct dominant factor        : %s\n", pct(sdg$correct_dominance)))
    cat(sprintf("    Simple-structure items         : %s\n", pct(sdg$simple_structure)))
    cat(sprintf("    Max cross-loading mean/q90/max : %s / %s / %s\n",
                num(sdg$mean_max_cross_loading), num(sdg$q90_max_cross_loading), num(sdg$max_cross_loading)))
    cat(sprintf("    Item complexity mean/max       : %s / %s\n",
                num(sdg$mean_complexity), num(sdg$max_complexity)))
    cat(sprintf("    Residual |r| mean/q95/max      : %s / %s / %s\n",
                num(sdg$mean_abs_residual), num(sdg$q95_abs_residual), num(sdg$max_abs_residual)))
  }
  cat("\n-- SEMANTIC PROPERTIES -----------------------------------\n")
  cat(sprintf("  Sigmoid sem. score : %.4f\n", result$semantic_score))
  if (!is.null(result$semantic_objective_score)) cat(sprintf("  Semantic objective : %.4f\n", result$semantic_objective_score))
  if (!is.null(result$proposal_objective_score)) cat(sprintf("  Proposal objective : %.4f\n", result$proposal_objective_score))
  if (!is.null(result$final_guided_objective_score)) cat(sprintf("  Final objective    : %.4f\n", result$final_guided_objective_score))
  cat(sprintf("  Raw similarity idx : %.4f\n", result$semantic_index))
  cat(sprintf("  Mean within-factor : %.4f\n", result$mean_within))
  cat(sprintf("  Mean between-factor: %.4f\n", result$mean_between))
  if (!is.null(result$q90_within)) cat(sprintf("  Q90 within-factor  : %.4f\n", result$q90_within))
  if (!is.null(result$q90_between)) cat(sprintf("  Q90 between-factor : %.4f\n", result$q90_between))
  if (!is.null(result$within_target_loss)) cat(sprintf("  Within target loss : %.4f\n", result$within_target_loss))
  if (!is.null(result$duplicate_penalty)) cat(sprintf("  Duplicate penalty  : %.4f\n", result$duplicate_penalty))
  if (!is.null(result$facet_coverage) && is.finite(result$facet_coverage)) cat(sprintf("  Facet coverage     : %.4f\n", result$facet_coverage))
  cat(sprintf("  Redundancy penalty : %.4f\n", result$redundancy_penalty))
  if (!is.null(result$split_half_stability)) {
    stab <- result$split_half_stability
    if (!is.na(stab$stable)) cat(sprintf("  Split-half stability: diff=%.4f [%s]\n", stab$difference, if (isTRUE(stab$stable)) "STABLE" else "UNSTABLE"))
  }
  if (!is.null(result$pfa_diagnostics)) {
    pfa <- result$pfa_diagnostics
    cat("\n-- SAMPLE-FREE PFA DIAGNOSTICS --------------------------\n")
    if (isTRUE(pfa$available)) {
      cat(sprintf("  PFA score          : %.4f\n", pfa$score))
      cat(sprintf("  PFA role           : %s\n",
                  if (identical(result$model_info$pfa_mode, "objective")) "selection objective" else "descriptive only"))
      if (!is.null(result$pfa_objective_score) && is.finite(result$pfa_objective_score)) {
        cat(sprintf("  Objective PFA score: %.4f (%s extraction)\n",
                    result$pfa_objective_score, result$model_info$pfa_extraction %||% "search"))
      }
      cat(sprintf("  Recovery/salience/clarity: %.4f / %.4f / %.4f\n",
                  pfa$recovery_score, pfa$salience_score, pfa$clarity_score))
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
  cat(sprintf("  Final objective    : %.4f\n", result$best_objective))
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
