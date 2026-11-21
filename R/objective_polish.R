# Bounded first-improvement subset search. The caller supplies the existing
# objective and feasibility rules; this helper introduces no loading targets.
.semantica_objective_polish <- function(vector, evaluation, items_by_factor,
                                        heuristics, evaluate, feasible,
                                        max_evals, max_seconds, prefer = NULL, screen = NULL, better = NULL) {
  started <- proc.time()[["elapsed"]]
  deadline <- started + max_seconds
  seen <- new.env(parent = emptyenv())
  assign(make_solution_key(vector), TRUE, seen)
  screens <- new.env(parent = emptyenv())
  discovered <- new.env(parent = emptyenv())
  initial_score <- evaluation$score
  rows <- list(); accepted <- proposed <- screened <- evaluated <- infeasible <- 0L
  reason <- "neighborhood_exhausted"
  expired <- function() proc.time()[["elapsed"]] >= deadline
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
    cursor <- rep(0, length(moves)); turn <- 0L; improved <- FALSE
    next_move <- function() {
      for (unused in seq_along(moves)) {
        j <- turn %% length(moves) + 1L; turn <<- turn + 1L
        m <- moves[[j]]; pos <- cursor[j]
        if (pos >= length(m$remove) * length(m$add)) next
        cursor[j] <<- pos + 1
        remove <- m$remove[pos %% length(m$remove) + 1]
        add <- m$add[pos %/% length(m$remove) + 1]
        candidate <- vector; candidate[remove] <- 0L; candidate[add] <- 1L
        return(list(vector = candidate, key = make_solution_key(candidate),
          factor = names(items_by_factor)[j], remove = remove, add = add))
      }
      NULL
    }
    repeat {
      pending <- list(); exhausted <- FALSE; blocked <- NULL
      repeat {
        if (expired()) { blocked <- "max_seconds_exhausted"; break }
        move <- next_move()
        if (is.null(move)) { exhausted <- TRUE; break }
        key <- move$key
        if (exists(key, seen, inherits = FALSE)) next
        if (!exists(key, discovered, inherits = FALSE)) {
          assign(key, TRUE, discovered); proposed <- proposed + 1L
          if (!isTRUE(feasible(move$vector))) {
            assign(key, TRUE, seen); infeasible <- infeasible + 1L; next
          }
        }
        move$upper_bound <- move$proposal <- NA_real_
        if (is.null(screen)) {
          if (evaluated >= max_evals) { blocked <- "max_evals_exhausted"; break }
        } else {
          if (!exists(key, screens, inherits = FALSE)) {
            if (screened >= max_evals) { blocked <- "max_evals_exhausted"; break }
            assign(key, screen(move$vector), screens); screened <- screened + 1L
          }
          info <- get(key, screens, inherits = FALSE)
          move$upper_bound <- info$upper_bound %||% NA_real_
          move$proposal <- info$proposal %||% NA_real_
        }
        pending[[length(pending) + 1L]] <- move
        if (is.null(screen)) break
      }
      if (length(pending) && !is.null(screen)) {
        bounds <- vapply(pending, `[[`, numeric(1L), "upper_bound")
        pending <- pending[order(bounds, decreasing = TRUE, na.last = TRUE)]
      }
      for (move in pending) {
        if (expired()) { blocked <- "max_seconds_exhausted"; break }
        pruned <- is.finite(move$upper_bound) && move$upper_bound < evaluation$score - 1e-10
        result <- if (pruned) list(bound_dominated = TRUE) else evaluate(move$vector)
        pruned <- pruned || isTRUE(result$bound_dominated)
        evaluated <- evaluated + as.integer(!pruned)
        assign(move$key, TRUE, seen)
        score <- result$score %||% -Inf
        if (is.finite(score) && is.finite(move$upper_bound) && score > move$upper_bound + 1e-10)
          stop("Refinement final score exceeds its declared upper bound.")
        # Scale-dependent roundoff is not evidence of an improvement.
        tolerance <- sqrt(.Machine$double.eps) * max(1, abs(evaluation$score))
        gain <- !pruned && is.finite(score) &&
          (if (is.null(better)) score > evaluation$score + tolerance else isTRUE(better(result, evaluation))) &&
          (is.null(prefer) || isTRUE(prefer(evaluation, result)))
        rows[[length(rows) + 1L]] <- data.frame(factor = move$factor,
          removed = move$remove, added = move$add, score_before = evaluation$score,
          candidate_score = if (pruned) NA_real_ else score, accepted = gain,
          status = if (pruned) "bound_excluded" else if (is.finite(score)) "evaluated" else "unavailable",
          upper_bound = move$upper_bound, proposal = move$proposal,
          semantic = result$sem_score %||% NA_real_, pfa = result$pfa_score %||% NA_real_,
          esem = result$esem_result$score %||% NA_real_,
          structure = result$esem_result$continuous_utility$components["structure"] %||% NA_real_,
          reconstruction = result$esem_result$continuous_utility$components["reconstruction"] %||% NA_real_,
          stringsAsFactors = FALSE, row.names = NULL)
        if (gain) {
          vector <- move$vector; evaluation <- result
          accepted <- accepted + 1L; improved <- TRUE
          break
        }
      }
      if (improved) break
      if (!is.null(blocked)) { reason <- blocked; break }
      if (exhausted) break
    }
    if (!improved || reason != "neighborhood_exhausted") break
  }
  history <- if (length(rows)) do.call(rbind, rows) else data.frame()
  unavailable <- if (length(rows)) sum(history$status == "unavailable") else 0L
  complete <- reason == "neighborhood_exhausted"
  list(vector = vector, evaluation = evaluation, diagnostics = list(enabled = TRUE,
    reason = reason, evaluated = evaluated, proposed = proposed, accepted = accepted,
    screened = screened, infeasible = infeasible,
    bound_excluded = if (length(rows)) sum(history$status == "bound_excluded") else 0L,
    unavailable = unavailable,
    unvisited_screened = sum(!ls(screens) %in% ls(seen)),
    neighborhood_complete = complete,
    certificate_scope = if (complete && unavailable == 0L && is.null(prefer) && is.null(better))
      "one_replacement_neighborhood_within_tolerance" else "none",
    comparison_tolerance = sqrt(.Machine$double.eps) * max(1, abs(evaluation$score)),
    budget_used = if (is.null(screen)) evaluated else screened,
    max_evals = max_evals, max_seconds = max_seconds,
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    score_before = initial_score, score_after = evaluation$score,
    history = history))
}
