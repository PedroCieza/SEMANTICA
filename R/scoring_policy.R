# Resolve compatibility aliases without coupling reference scaling and cohesion.
.semantica_resolve_semantic_policy <- function(policy = "candidate",
    gap_reference = NULL, cohesion_guard = NULL) {
  policy <- match.arg(policy, c("candidate", "admitted_pool_lower_guard"))
  gap_reference <- if (is.null(gap_reference)) {
    if (policy == "candidate") "candidate" else "admitted_pool"
  } else match.arg(gap_reference, c("candidate", "admitted_pool"))
  cohesion_guard <- if (is.null(cohesion_guard)) {
    if (policy == "candidate") "two_sided" else "lower_only"
  } else match.arg(cohesion_guard, c("two_sided", "lower_only"))
  id <- if (gap_reference == "candidate") {
    if (cohesion_guard == "two_sided") "candidate" else "candidate_lower_guard"
  } else if (cohesion_guard == "lower_only") "admitted_pool_lower_guard" else "admitted_pool_two_sided"
  list(schema = "semantica-semantic-policy-v2", requested_alias = policy,
    gap_reference = gap_reference, cohesion_guard = cohesion_guard, id = id,
    calibration_status = "not_externally_calibrated",
    evidence_scope = "representation_geometry_only")
}

# One arithmetic contract for proposal, guided search and native final scores.
# Numeric fallback behavior is retained; missing evidence remains explicit.
.semantica_mix_esem <- function(proposal, esem, weight, guard = 1, guard_weight = 0) {
  ((1 - weight) * proposal + weight * esem) * (guard ^ guard_weight)
}

.semantica_compose_score <- function(semantic, pfa = NA_real_, esem = NA_real_,
    pfa_active = FALSE, pfa_weight = 0, pfa_failure_policy = "semantic_fallback",
    esem_active = FALSE, esem_weight = 0, guard_penalty = 1, guard_weight = 0,
    stage = c("proposal", "search", "final"),
    pfa_unavailable = c("error", "reject", "na"), esem_available = NULL) {
  stage <- match.arg(stage); pfa_unavailable <- match.arg(pfa_unavailable)
  pfa_failure_policy <- match.arg(pfa_failure_policy, c("semantic_fallback", "penalize", "stop"))
  for (w in list(pfa_weight, esem_weight))
    if (!is.numeric(w) || length(w) != 1L || !is.finite(w) || w < 0 || w > 1)
      stop("Score weights must be finite scalars in [0, 1].")
  finite_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (is.null(pfa)) pfa <- NA_real_
  if (is.null(esem)) esem <- NA_real_
  pfa_active <- isTRUE(pfa_active) && pfa_weight > 0
  wp <- if (pfa_active) pfa_weight else 0
  we <- if (isTRUE(esem_active)) esem_weight else 0
  regime <- "semantic"
  proposal <- semantic
  if (pfa_active) {
    if (finite_scalar(pfa)) {
      proposal <- (1 - wp) * semantic + wp * pfa
      regime <- "semantic_pfa"
    } else if (pfa_failure_policy == "semantic_fallback") {
      wp <- 0; regime <- "semantic_fallback_pfa_unavailable"
    } else if (pfa_failure_policy == "penalize") {
      proposal <- (1 - wp) * semantic; regime <- "pfa_unavailable_penalized"
    } else {
      if (pfa_unavailable == "error")
        stop("Objective-mode PFA was unavailable for this candidate under pfa_failure_policy='stop'.")
      proposal <- if (pfa_unavailable == "reject") -Inf else NA_real_
      regime <- "pfa_unavailable_rejected"
    }
  }
  total <- proposal
  if (isTRUE(esem_active)) {
    total <- .semantica_mix_esem(proposal, esem, we, guard_penalty, guard_weight)
    regime <- paste0(regime, "_esem_guided")
  }
  availability <- c(semantic = finite_scalar(semantic), pfa = finite_scalar(pfa),
    esem = if (is.null(esem_available)) finite_scalar(esem) else isTRUE(esem_available))
  list(schema = "semantica-score-composition-v1", stage = stage,
    proposal = proposal, score = total, regime = regime,
    components = c(semantic = semantic, pfa = pfa, esem = esem), availability = availability,
    weights = c(semantic = (1 - we) * (1 - wp), pfa = (1 - we) * wp, esem = we),
    guard = list(penalty = guard_penalty, weight = guard_weight),
    evidence_scope = "decision_utility_not_a_success_probability")
}
