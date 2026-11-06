# Bounded first-improvement subset search. The caller supplies the existing
# objective and feasibility rules; this helper introduces no loading targets.
.semantica_objective_polish <- function(vector, evaluation, items_by_factor,
                                        heuristics, evaluate, feasible,
                                        max_evals, max_seconds, prefer = NULL) {
  started <- proc.time()[["elapsed"]]
  deadline <- started + max_seconds
  seen <- new.env(parent = emptyenv())
  assign(make_solution_key(vector), TRUE, seen)
  initial_score <- evaluation$score
  rows <- list(); accepted <- proposed <- 0L
  reason <- "neighborhood_exhausted"
  repeat {
    selected <- names(vector)[vector == 1L]
    # Cheap heuristics order moves only; factors take turns, including when
    # their pool sizes differ. No Cartesian neighborhood is materialized.
    moves <- lapply(names(items_by_factor), function(f) {
      h <- heuristics[[f]]
      rank_items <- function(ids, decreasing) {
        v <- if (is.null(h)) rep(0, length(ids)) else as.numeric(h[ids])
        v[!is.finite(v)] <- 0
        ids[order(if (decreasing) -v else v, enc2utf8(ids), method = "radix")]
      }
      list(remove = rank_items(intersect(items_by_factor[[f]], selected), FALSE),
           add = rank_items(setdiff(items_by_factor[[f]], selected), TRUE))
    })
    cursor <- rep(0, length(moves)); improved <- FALSE
    repeat {
      available <- which(vapply(seq_along(moves), function(j)
        cursor[j] < length(moves[[j]]$remove) * length(moves[[j]]$add), logical(1L)))
      if (!length(available)) break
      for (j in available) {
        if (length(rows) >= max_evals || proc.time()[["elapsed"]] >= deadline) {
          reason <- if (length(rows) >= max_evals) "max_evals_exhausted" else "max_seconds_exhausted"
          break
        }
        m <- moves[[j]]; pos <- cursor[j]; cursor[j] <- pos + 1
        remove <- m$remove[pos %% length(m$remove) + 1]
        add <- m$add[pos %/% length(m$remove) + 1]
        candidate <- vector; candidate[remove] <- 0L; candidate[add] <- 1L
        key <- make_solution_key(candidate)
        if (exists(key, seen, inherits = FALSE)) next
        assign(key, TRUE, seen); proposed <- proposed + 1L
        if (!isTRUE(feasible(candidate))) next
        if (proc.time()[["elapsed"]] >= deadline) { reason <- "max_seconds_exhausted"; break }
        result <- evaluate(candidate)
        score <- result$score %||% -Inf
        # Scale-dependent roundoff is not evidence of an improvement.
        tolerance <- sqrt(.Machine$double.eps) * max(1, abs(evaluation$score))
        gain <- is.finite(score) && score > evaluation$score + tolerance &&
          (is.null(prefer) || isTRUE(prefer(evaluation, result)))
        rows[[length(rows) + 1L]] <- data.frame(factor = names(items_by_factor)[j],
          removed = remove, added = add, score_before = evaluation$score,
          candidate_score = score, accepted = gain,
          semantic = result$sem_score %||% NA_real_, pfa = result$pfa_score %||% NA_real_,
          esem = result$esem_result$score %||% NA_real_,
          structure = result$esem_result$continuous_utility$components["structure"] %||% NA_real_,
          reconstruction = result$esem_result$continuous_utility$components["reconstruction"] %||% NA_real_,
          stringsAsFactors = FALSE, row.names = NULL)
        if (gain) {
          vector <- candidate; evaluation <- result
          accepted <- accepted + 1L; improved <- TRUE
          break
        }
      }
      if (improved || reason != "neighborhood_exhausted") break
    }
    if (!improved || reason != "neighborhood_exhausted") break
  }
  list(vector = vector, evaluation = evaluation, diagnostics = list(enabled = TRUE,
    reason = reason, evaluated = length(rows), proposed = proposed, accepted = accepted,
    max_evals = max_evals, max_seconds = max_seconds,
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    score_before = initial_score, score_after = evaluation$score,
    history = if (length(rows)) do.call(rbind, rows) else data.frame()))
}
