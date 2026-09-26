semantica_test_three_factor_fixture <- function(condition = c("separable", "overlapping", "shuffled"), per_factor = 4L) {
  condition <- match.arg(condition)
  factors <- rep(c("F1", "F2", "F3"), each = per_factor)
  ids <- paste0("i", seq_along(factors))
  if (condition == "separable" || condition == "shuffled") {
    base <- matrix(0.08, length(ids), length(ids), dimnames = list(ids, ids))
    diag(base) <- 1
    for (f in unique(factors)) {
      idx <- which(factors == f)
      base[idx, idx] <- 0.84
      diag(base)[idx] <- 1
    }
  } else {
    base <- matrix(0.55, length(ids), length(ids), dimnames = list(ids, ids))
    diag(base) <- 1
    for (f in unique(factors)) {
      idx <- which(factors == f)
      base[idx, idx] <- 0.62
      diag(base)[idx] <- 1
    }
  }
  labels <- factors
  if (condition == "shuffled") labels <- c("F2", "F3", "F1")[match(factors, c("F1", "F2", "F3"))]
  df <- data.frame(id = ids, factor = labels, item = paste("Item", ids), stringsAsFactors = FALSE)
  list(matrix = base, df = df, factors = labels, ids = ids)
}


# Keep ACO/search invariant tests focused on ACO semantics rather than paying for
# unrelated final lavaan/DFI work. Dedicated ESEM tests exercise the real solver.
semantica_test_mock_esem_unavailable <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    run_esem_on_matrix = function(..., return_diagnostics = FALSE) {
      if (!isTRUE(return_diagnostics)) return(NULL)
      list(
        fit = NULL,
        rejection_assessment = NULL,
        rejected_attempts = list(),
        fallback_used = FALSE,
        attempts = 0L
      )
    },
    .package = "SEMANTICA",
    .env = .local_envir
  )
  invisible(NULL)
}


semantica_test_aco_args <- function(fx, seed = 123L, history_mode = "summary") {
  list(
    cosine_sim_matrix = fx$matrix,
    df = fx$df,
    i.per.f = c(F1 = 2L, F2 = 2L, F3 = 2L),
    ants = 5L, max.iter = 3L, max_total_iter = 3L,
    max_esem_fits = 1L, run_esem_during_search = FALSE,
    esem_failure_policy = "semantic_fallback",
    esem_sample_size = 200L, full_esem_iter_max = 200L,
    dfi_mode = "heuristic_semantic", elite_multicriteria_rerank = FALSE,
    pfa_mode = "diagnostic", run_pfa_during_search = FALSE,
    semantic_n_sensitivity = FALSE,
    final_dddfi = FALSE, final_equivtest = FALSE,
    validation_n_diagnostic = FALSE,
    use_parallel = FALSE, seed = seed, verbose = FALSE,
    history_mode = history_mode, elite_k = 3L
  )
}

semantica_test_run_aco <- function(seed = 123L, history_mode = "summary",
                                   condition = "separable") {
  semantica_test_mock_esem_unavailable()
  fx <- semantica_test_three_factor_fixture(condition)
  do.call(ACO_with_ESEM, semantica_test_aco_args(fx, seed, history_mode))
}

# Shared deterministic control used by directional PFA/ESEM regression tests.
make_semantica_control_fixture <- function(kind = c("separable", "overlapping", "shuffled")) {
  kind <- match.arg(kind)
  per_factor <- 5L
  factors <- rep(c("A", "B", "C"), each = per_factor)
  ids <- paste0(factors, rep(seq_len(per_factor), 3L))
  n_items <- length(ids)

  # Candidate-pool fixtures used by optimizer tests must themselves satisfy the
  # production pre-ACO contract. Give each item a shared construct direction
  # plus its own orthogonal local-information component. This preserves clear
  # within-factor structure without manufacturing near-duplicate items simply
  # to make the test numerically easy.
  emb <- matrix(0, nrow = n_items, ncol = 3L + n_items + 1L)
  rownames(emb) <- ids
  primary <- if (kind == "overlapping") 0.55 else 0.78
  common <- if (kind == "overlapping") 0.55 else 0
  unique_weight <- sqrt(max(0, 1 - primary^2 - common^2))
  for (i in seq_len(n_items)) {
    f_idx <- match(factors[[i]], c("A", "B", "C"))
    emb[i, f_idx] <- primary
    if (common > 0) emb[i, 4L] <- common
    emb[i, 4L + i] <- unique_weight
  }
  emb <- emb / sqrt(rowSums(emb^2))
  cosine <- tcrossprod(emb)
  dimnames(cosine) <- list(ids, ids)

  assignment <- stats::setNames(factors, ids)
  if (kind == "shuffled") {
    assignment <- stats::setNames(rep(c("A", "B", "C"), length.out = n_items), ids)
  }
  list(
    ids = ids,
    embeddings = emb,
    cosine = cosine,
    assignment = assignment,
    factors = c("A", "B", "C"),
    df = data.frame(item = ids, type = unname(assignment), stringsAsFactors = FALSE),
    i.per.f = c(A = 3L, B = 3L, C = 3L)
  )
}
