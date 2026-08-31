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
  ids <- paste0(rep(c("A", "B", "C"), each = 4L), rep(1:4, 3L))
  factors <- rep(c("A", "B", "C"), each = 4L)
  # Deterministic unit-vector geometry: clean controls concentrate on one axis;
  # overlapping controls share a strong common component.
  clean <- rbind(
    c(1.00, .05, .05, .10, .00, .00), c(.96, .08, .04, -.08, .02, .00),
    c(.94, .04, .08, .00, -.08, .03), c(.92, .06, .05, .04, .05, -.08),
    c(.05, 1.00, .05, .00, .10, .00), c(.08, .96, .04, .02, -.08, .00),
    c(.04, .94, .08, -.08, .00, .03), c(.06, .92, .05, .05, .04, -.08),
    c(.05, .05, 1.00, .00, .00, .10), c(.08, .04, .96, -.08, .02, .00),
    c(.04, .08, .94, .00, -.08, .03), c(.06, .05, .92, .04, .05, -.08)
  )
  overlap <- rbind(
    c(.75,.65,.60,.08,0,0), c(.72,.68,.61,-.06,.02,0), c(.70,.66,.64,0,-.06,.02), c(.69,.64,.62,.03,.04,-.06),
    c(.64,.75,.61,0,.08,0), c(.68,.72,.60,.02,-.06,0), c(.66,.70,.64,-.06,0,.02), c(.63,.69,.62,.04,.03,-.06),
    c(.61,.64,.75,0,0,.08), c(.60,.68,.72,-.06,.02,0), c(.64,.66,.70,0,-.06,.02), c(.62,.63,.69,.03,.04,-.06)
  )
  emb <- if (kind == "overlapping") overlap else clean
  emb <- emb / sqrt(rowSums(emb^2))
  rownames(emb) <- ids
  cosine <- tcrossprod(emb)
  dimnames(cosine) <- list(ids, ids)
  assignment <- stats::setNames(factors, ids)
  if (kind == "shuffled") {
    assignment <- stats::setNames(rep(c("A", "B", "C"), times = 4L), ids)
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
