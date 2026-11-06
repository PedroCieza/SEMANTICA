# Actual native final evaluator, shared by ACO and bounded fixed-form audits.
.semantica_archive_context <- function(envir, overrides = list()) {
  keys <- c("active_cutoffs", "candidate_master_seed", "continuous_scoring", 
    "cosine_sim_matrix", "dup_threshold", "duplicate_cluster_id", 
    "eligible.items", "elite_multicriteria_rerank", "esem_bounds", 
    "esem_sample_size", "esem_weight", "evaluation_log_dir", "expected_factor_relations", 
    "facet_constraint_mode", "facet_coverage_weight", "facet_max_imbalance", 
    "facet_min_per_facet", "facets.by.factor", "factors", "full_esem_iter_max", 
    "htmt_objective_role", "htmt_threshold", "i.per.f", "item.facet.lookup", 
    "item.factor.lookup", "item.vector", "model_info", "nomological_weight", 
    "objective_polish", "pfa_extraction", "pfa_failure_policy", "pfa_max_abs_loading", 
    "pfa_min_loading", "pfa_min_margin", "pfa_mode", "pfa_rotation", 
    "pfa_weight", "psychometric_guard_action", "psychometric_guard_ave_warning_action", 
    "psychometric_guard_include_htmt", "psychometric_guard_max_cross_loading", 
    "psychometric_guard_min_ave", "psychometric_guard_min_dominance", 
    "psychometric_guard_min_loading", "psychometric_guard_min_primary_ge_50", 
    "psychometric_guard_min_simple_structure", "psychometric_guard_weight", 
    "redundancy_threshold", "requested_esem_search", "rotation", 
    "rotation_args", "seed", "semantic_cohesion_guard", "semantic_esem_score_mode", 
    "semantic_objective_mode", "semantic_reference", "sigmoid_center",
    "sigmoid_steepness", "within_similarity_band", "within_similarity_target_eff",
    "content_review", "content_witness", "fit_cache", "final_fit_pruning"
    )
  values <- mget(setdiff(keys, names(overrides)), envir = envir, inherits = TRUE)
  values[names(overrides)] <- overrides
  values[keys]
}

.semantica_make_archive_evaluator <- function(context) {
  # Explicit lexical bindings keep the closure portable and statically checkable.
  active_cutoffs <- context$active_cutoffs
  candidate_master_seed <- context$candidate_master_seed
  continuous_scoring <- context$continuous_scoring
  cosine_sim_matrix <- context$cosine_sim_matrix
  dup_threshold <- context$dup_threshold
  duplicate_cluster_id <- context$duplicate_cluster_id
  eligible.items <- context$eligible.items
  elite_multicriteria_rerank <- context$elite_multicriteria_rerank
  esem_bounds <- context$esem_bounds
  esem_sample_size <- context$esem_sample_size
  esem_weight <- context$esem_weight
  evaluation_log_dir <- context$evaluation_log_dir
  expected_factor_relations <- context$expected_factor_relations
  facet_constraint_mode <- context$facet_constraint_mode
  facet_coverage_weight <- context$facet_coverage_weight
  facet_max_imbalance <- context$facet_max_imbalance
  facet_min_per_facet <- context$facet_min_per_facet
  facets.by.factor <- context$facets.by.factor
  factors <- context$factors
  full_esem_iter_max <- context$full_esem_iter_max
  htmt_objective_role <- context$htmt_objective_role
  htmt_threshold <- context$htmt_threshold
  i.per.f <- context$i.per.f
  item.facet.lookup <- context$item.facet.lookup
  item.factor.lookup <- context$item.factor.lookup
  item.vector <- context$item.vector
  model_info <- context$model_info
  nomological_weight <- context$nomological_weight
  objective_polish <- context$objective_polish
  pfa_extraction <- context$pfa_extraction
  pfa_failure_policy <- context$pfa_failure_policy
  pfa_max_abs_loading <- context$pfa_max_abs_loading
  pfa_min_loading <- context$pfa_min_loading
  pfa_min_margin <- context$pfa_min_margin
  pfa_mode <- context$pfa_mode
  pfa_rotation <- context$pfa_rotation
  pfa_weight <- context$pfa_weight
  psychometric_guard_action <- context$psychometric_guard_action
  psychometric_guard_ave_warning_action <- context$psychometric_guard_ave_warning_action
  psychometric_guard_include_htmt <- context$psychometric_guard_include_htmt
  psychometric_guard_max_cross_loading <- context$psychometric_guard_max_cross_loading
  psychometric_guard_min_ave <- context$psychometric_guard_min_ave
  psychometric_guard_min_dominance <- context$psychometric_guard_min_dominance
  psychometric_guard_min_loading <- context$psychometric_guard_min_loading
  psychometric_guard_min_primary_ge_50 <- context$psychometric_guard_min_primary_ge_50
  psychometric_guard_min_simple_structure <- context$psychometric_guard_min_simple_structure
  psychometric_guard_weight <- context$psychometric_guard_weight
  redundancy_threshold <- context$redundancy_threshold
  requested_esem_search <- context$requested_esem_search
  rotation <- context$rotation
  rotation_args <- context$rotation_args
  seed <- context$seed
  semantic_cohesion_guard <- context$semantic_cohesion_guard
  semantic_esem_score_mode <- context$semantic_esem_score_mode
  semantic_objective_mode <- context$semantic_objective_mode
  semantic_reference <- context$semantic_reference
  sigmoid_center <- context$sigmoid_center
  sigmoid_steepness <- context$sigmoid_steepness
  within_similarity_band <- context$within_similarity_band
  within_similarity_target_eff <- context$within_similarity_target_eff
  fit_cache <- context$fit_cache
  final_fit_pruning <- isTRUE(context$final_fit_pruning)
  evaluator <- function(entry) {
    v <- entry$vec; sel_items <- item.vector[v == 1L]; fa <- item.factor.lookup[sel_items]
    facet_constraint <- .semantica_facet_constraint_diagnostics(
      selected_items = sel_items,
      factor_assignment = fa,
      item_facet_lookup = item.facet.lookup,
      facets_by_factor = facets.by.factor,
      i_per_f = i.per.f,
      mode = facet_constraint_mode,
      min_per_facet = facet_min_per_facet,
      max_imbalance = facet_max_imbalance,
      duplicate_cluster_id = duplicate_cluster_id
    )
    if (isTRUE(facet_constraint$active) && !isTRUE(facet_constraint$passed)) {
      return(list(
        score = -Inf,
        proposal_score = -Inf,
        facet_constraint = facet_constraint,
        rejection_reason = "facet_constraint_violation"
      ))
    }
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
      semantic_objective_mode = semantic_objective_mode,
      semantic_reference = semantic_reference,
      semantic_cohesion_guard = semantic_cohesion_guard
    )
    dup_pen <- compute_duplicate_penalty(sel_items, fa, factors, cos_sub, dup_threshold)
    facet_mult <- compute_facet_coverage_multiplier(sel_items, fa, item.facet.lookup, facets.by.factor, i.per.f, facet_coverage_weight)
    facet_coverage_final <- attr(facet_mult, "coverage", exact = TRUE) %||% 1
    content_coverage_final <- .semantica_content_coverage_profile(
      sel_items, fa, eligible.items, cosine_sim_matrix,
      facet_coverage = facet_coverage_final, duplicate_penalty = dup_pen
    )
    sem_score_final <- sem_r$sem_score * dup_pen * facet_mult
    pfa_score_final <- NA_real_
    pfa_r <- NULL
    search_score_final <- sem_score_final
    pfa_objective_final <- pfa_mode == "objective" && pfa_weight > 0
    pfa_guard_final <- pfa_mode != "off" && is.finite(pfa_max_abs_loading)
    if (pfa_objective_final || pfa_guard_final) {
      pfa_r <- compute_pfa_diagnostics(
        cos_sub, fa, factors,
        extraction = pfa_extraction,
        rotation = pfa_rotation,
        min_loading = pfa_min_loading,
        min_margin = pfa_min_margin,
        max_abs_loading = pfa_max_abs_loading, fit_cache = fit_cache
      )
      pfa_score_final <- if (isTRUE(pfa_r$available)) pfa_r$score else NA_real_
    }
    proposal_composition <- .semantica_compose_score(sem_score_final, pfa_score_final,
      pfa_active = pfa_objective_final, pfa_weight = pfa_weight,
      pfa_failure_policy = pfa_failure_policy, pfa_unavailable = "reject", stage = "final")
    search_score_final <- proposal_composition$proposal
    if (!requested_esem_search || esem_weight <= 0) {
      return(list(
        score = search_score_final,
        proposal_score = search_score_final,
        sem_score = sem_score_final,
        pfa_score = pfa_score_final,
        pfa_result = pfa_r,
        duplicate_penalty = dup_pen,
        facet_coverage = facet_coverage_final,
        content_coverage = content_coverage_final,
        score_composition = proposal_composition
      ))
    }
    if (final_fit_pruning && !isTRUE(entry$protected) && is.finite(search_score_final) && is.finite(entry$incumbent %||% -Inf) &&
        (1 - esem_weight) * search_score_final + esem_weight < entry$incumbent - 1e-10)
      return(list(score = -Inf, proposal_score = search_score_final,
        bound_dominated = TRUE, upper_bound = (1 - esem_weight) * search_score_final + esem_weight,
        rejection_reason = "cannot_win_current_scalar_rule"))
    esem_cor <- transform_cosine_for_esem(cos_sub, fa, factors)
    if (is.null(esem_cor)) {
      return(list(score = -Inf, rejection_reason = "esem_correlation_transformation_failed"))
    }
    syntax <- build_esem_syntax_safe(sel_items, fa, factors)
    archive_rotation_args <- prepare_esem_rotation_args(rotation, rotation_args, sel_items, fa, factors)
    archive_esem_started <- proc.time()[["elapsed"]]
    fit_archive <- function() run_esem_on_matrix(
      syntax, esem_cor, esem_sample_size, model_info$estimator, rotation, archive_rotation_args,
      iter_max = full_esem_iter_max, fallback = TRUE,
      return_diagnostics = TRUE, bounds = esem_bounds,
      progress = list(directory = evaluation_log_dir, stage = "archive", key = .semantica_object_md5(sort(sel_items)))
    )
    # Stable candidate-specific starts make opt-in archive scores comparable
    # across serial and worker execution without advancing the caller's RNG.
    archive_seed <- if (continuous_scoring || objective_polish) {
      .semantica_utility_fit_seed(if (objective_polish) candidate_master_seed else seed, sel_items)
    } else NULL
    cached_fit <- .semantica_cached_fit(fit_cache,
      list(kind = "esem", matrix = esem_cor, syntax = syntax, sample_size = esem_sample_size,
        estimator = model_info$estimator, rotation = rotation, rotation_args = archive_rotation_args,
        iter_max = full_esem_iter_max, fallback = TRUE, bounds = esem_bounds),
      fit_archive, seed = archive_seed, allow_compute = entry$allow_new_fit %||% TRUE)
    if (!cached_fit$available) return(list(score = NA_real_, proposal_score = search_score_final,
      budget_exhausted = TRUE, rejection_reason = "final_fit_budget_exhausted"))
    esem_run <- cached_fit$value
    esem_fit <- esem_run$fit
    if (!is.null(esem_fit) && !is.null(archive_seed)) attr(esem_fit, "semantica_utility_fit_seed") <- archive_seed
    r <- extract_and_score_esem(
      esem_fit, esem_cor, fa, factors, active_cutoffs, htmt_threshold,
      score_mode = semantic_esem_score_mode,
      htmt_objective_role = htmt_objective_role
    )
    r <- .semantica_attach_esem_rejection(r, esem_run)
    archive_esem_event <- list(
      stage = "archive",
      candidate_key = .semantica_object_md5(sort(sel_items)), cache_hit = cached_fit$cache_hit,
      elapsed_seconds = proc.time()[["elapsed"]] - archive_esem_started,
      fit_result = r, error = if (isTRUE(r$converged) && isTRUE(r$admissible)) NA_character_ else "archive ESEM unavailable or inadmissible",
      fallback_used = esem_run$fallback_used %||% NA
    )
    if (!isTRUE(r$converged) || !isTRUE(r$admissible) || identical(r$objective_available, FALSE)) {
      return(list(
        score = -Inf,
        proposal_score = search_score_final,
        esem_fit = esem_fit,
        esem_result = r,
        esem_cor = esem_cor,
        syntax = syntax,
        rotation_args = archive_rotation_args,
        sem_score = sem_score_final,
        pfa_score = pfa_score_final,
        pfa_result = pfa_r,
        duplicate_penalty = dup_pen,
        facet_coverage = facet_coverage_final,
        content_coverage = content_coverage_final,
        archive_esem_fit_started = !cached_fit$cache_hit,
        archive_esem_cache_hit = cached_fit$cache_hit,
        archive_esem_event = archive_esem_event,
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
      min_simple_structure = psychometric_guard_min_simple_structure,
      min_correct_dominance = psychometric_guard_min_dominance,
      max_cross_loading = psychometric_guard_max_cross_loading,
      htmt_guard_threshold = if (isTRUE(psychometric_guard_include_htmt)) htmt_threshold else Inf,
      pfa_result = pfa_r,
      pfa_max_abs_loading = pfa_max_abs_loading,
      ave_warning_action = psychometric_guard_ave_warning_action,
      guard_action = psychometric_guard_action
    )
    guard_diagnostics <- attr(guard_pen, "diagnostics", exact = TRUE)
    guard_violation <- .semantica_guard_violation(guard_diagnostics)
    if (identical(psychometric_guard_action, "final_constraint") &&
        !isTRUE(guard_diagnostics$passed)) {
      return(list(
        score = -Inf,
        proposal_score = search_score_final,
        sem_score = sem_score_final,
        pfa_score = pfa_score_final,
        pfa_result = pfa_r,
        duplicate_penalty = dup_pen,
        facet_coverage = facet_coverage_final,
        facet_constraint = facet_constraint,
        content_coverage = content_coverage_final,
        guard_penalty = guard_pen,
        guard_diagnostics = guard_diagnostics,
        guard_violation = guard_violation,
        esem_fit = esem_fit,
        esem_result = r,
        esem_cor = esem_cor,
        syntax = syntax,
        rotation_args = archive_rotation_args,
        archive_esem_fit_started = !cached_fit$cache_hit,
        archive_esem_cache_hit = cached_fit$cache_hit,
        archive_esem_event = archive_esem_event,
        rejection_reason = "psychometric_guard_failed"
      ))
    }
    composition <- .semantica_compose_score(sem_score_final, pfa_score_final, r$score,
      pfa_active = pfa_objective_final, pfa_weight = pfa_weight,
      pfa_failure_policy = pfa_failure_policy, pfa_unavailable = "reject",
      esem_active = TRUE, esem_weight = esem_weight,
      guard_penalty = guard_pen, guard_weight = psychometric_guard_weight, stage = "final",
      esem_available = isTRUE(r$objective_available %||% r$admissible))
    base_total <- composition$score
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
      composition$score_before_rerank <- composition$score
      composition$score <- base_total
      composition$final_rerank <- list(bonus = multicriteria_bonus, policy = rerank_policy)
    }
    list(
      score = base_total,
      score_composition = composition,
      proposal_score = search_score_final,
      sem_score = sem_score_final,
      pfa_score = pfa_score_final,
      pfa_result = pfa_r,
      duplicate_penalty = dup_pen,
      facet_coverage = facet_coverage_final,
      facet_constraint = facet_constraint,
      content_coverage = content_coverage_final,
      guard_penalty = guard_pen,
      guard_diagnostics = guard_diagnostics,
      guard_violation = guard_violation,
      esem_fit = esem_fit,
      esem_result = r,
      esem_cor = esem_cor,
      syntax = syntax,
      rotation_args = archive_rotation_args,
      archive_esem_fit_started = !cached_fit$cache_hit,
      archive_esem_cache_hit = cached_fit$cache_hit,
      archive_esem_event = archive_esem_event
    )
  }
  evaluator
}

.semantica_run_solution_audit <- function(context, request) {
  if (length(context$content_witness$items)) context$content_witness$status <-
    if (any(vapply(request$solutions, setequal, logical(1L), y = context$content_witness$items)))
      if (request$stage == "both") "included_in_audit" else "proposal_only" else "available_not_requested"
  evaluate_final <- .semantica_make_archive_evaluator(context)
  proposal_env <- list2env(context, parent = asNamespace("SEMANTICA"))
  proposal_env$cache <- new.env(parent = emptyenv())
  attr(proposal_env$cache, "policy_id") <- .semantica_object_md5(list(
    schema = .semantica_decision_policy()$objective_schema_version,
    model = context$model_info, matrix = context$cosine_sim_matrix,
    admission = context$eligible.items, review = context$content_review))
  records <- evaluations <- list(); reserved <- 0L; incumbent <- -Inf
  for (id in names(request$solutions)) {
    index <- match(id, names(request$solutions))
    ids <- request$solutions[[id]]
    if (!is.null(request$output_dir)) .semantica_checkpoint_save(list(
      solution_id = id, selected_ids = ids, stage = request$stage, controller_pid = Sys.getpid(),
      started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
      file.path(request$output_dir, "forms", sprintf("form-%03d-started.rds", index)))
    count <- table(factor(context$item.factor.lookup[ids], levels = names(context$i.per.f)))
    admitted <- all(ids %in% unlist(context$eligible.items, use.names = FALSE))
    legal <- admitted &&
      length(ids) == sum(context$i.per.f) && all(count == context$i.per.f)
    rejection <- if (!admitted) "item_not_admitted" else if (!legal) "domain_quota_mismatch" else ""
    if (legal) legal <- isTRUE(.semantica_facet_constraint_diagnostics(ids,
      context$item.factor.lookup[ids], context$item.facet.lookup, context$facets.by.factor,
      context$i.per.f, context$facet_constraint_mode, context$facet_min_per_facet,
      context$facet_max_imbalance, context$duplicate_cluster_id)$passed)
    if (!legal && !nzchar(rejection)) rejection <- "facet_or_duplicate_constraint"
    v <- stats::setNames(as.integer(context$item.vector %in% ids), context$item.vector)
    proposal_env$v <- v
    proposed <- if (legal) tryCatch(eval(quote(fit.function.v2(v, run_esem_now = FALSE,
      run_pfa_now = TRUE, solution_cache = cache)), proposal_env), error = identity) else -Inf
    error <- if (inherits(proposed, "error")) conditionMessage(proposed) else ""
    proposal <- if (inherits(proposed, "error")) NA_real_ else as.numeric(proposed)
    entry <- cache_get(proposal_env$cache, make_solution_key(v))
    final <- list(score = NA_real_)
    status <- if (legal) "proposal_evaluated" else "native_constraint_rejection"
    fit_reserved <- FALSE
    if (request$stage == "both" && legal && !inherits(proposed, "error")) {
      needs_fit <- isTRUE(context$requested_esem_search) && context$esem_weight > 0
      allow_fit <- !needs_fit || reserved < request$max_new_esem_fits
      final <- tryCatch(evaluate_final(list(vec = v, allow_new_fit = allow_fit, incumbent = incumbent,
        protected = setequal(ids, context$content_witness$items))), error = identity)
      if (inherits(final, "error")) {
        error <- conditionMessage(final); final <- list(score = NA_real_, error = error)
        fit_reserved <- needs_fit && allow_fit
        status <- "final_execution_error"
      } else {
        fit_reserved <- isTRUE(final$archive_esem_fit_started)
        status <- if (isTRUE(final$budget_exhausted)) "final_budget_exhausted" else
          if (isTRUE(final$bound_dominated)) "final_bound_dominated" else
          if (is.finite(final$score)) "native_final_evaluated" else "native_final_rejected"
      }
      if (fit_reserved) reserved <- reserved + 1L
      if (is.finite(final$score)) incumbent <- max(incumbent, final$score)
    } else if (inherits(proposed, "error")) status <- "proposal_execution_error"
    records[[id]] <- data.frame(solution_id = id, native_legal = legal,
      content_review_status = if (is.null(context$content_review)) "not_requested" else
        if (all(context$content_review$status[match(ids, context$content_review$item_id)] %in% "supported"))
          "supported" else "review_required",
      proposal_score = proposal, final_score = as.numeric(final$score), status = status,
      semantic = as.numeric(final$sem_score %||% entry$sem_score %||% NA_real_),
      pfa = as.numeric(final$pfa_score %||% entry$pfa_score %||% NA_real_),
      esem = as.numeric(final$esem_result$score %||% NA_real_),
      esem_job_reserved = fit_reserved,
      esem_cache_hit = isTRUE(final$archive_esem_cache_hit),
      esem_fit_started = if (status == "final_execution_error") NA else isTRUE(final$archive_esem_fit_started),
      rejection_reason = if (nzchar(rejection)) rejection else final$rejection_reason %||% "",
      error = error)
    evaluations[[id]] <- list(selected_ids = ids, proposal = entry, final = final)
    if (!is.null(request$output_dir)) {
      .semantica_checkpoint_save(list(record = records[[id]], evaluation = evaluations[[id]]),
        file.path(request$output_dir, "forms", sprintf("form-%03d-outcome.rds", index)))
      utils::write.csv(do.call(rbind, records), file.path(request$output_dir, "progress.csv"), row.names = FALSE)
    }
  }
  answer <- list(schema = "semantica-native-solution-audit-v1", results = do.call(rbind, records),
    evaluations = evaluations, semantic_policy = context$model_info$semantic_policy,
    objective_schema = .semantica_decision_policy()$objective_schema_version,
    pool_fingerprint = .semantica_object_md5(context$cosine_sim_matrix),
    source_scope = "native_scoring_without_ACO_or_population_truth",
    content_review = .semantica_content_review_result(context$content_review,
      context$eligible.items, witness = context$content_witness),
    admission = list(eligible_items = context$eligible.items,
      duplicate_cluster_id = context$duplicate_cluster_id, selected_counts = context$i.per.f,
      facet_constraint_mode = context$facet_constraint_mode),
    qualification = "not_externally_calibrated", new_search_jobs = 0L,
    esem_jobs_reserved = reserved, maximum_new_esem_jobs = request$max_new_esem_fits)
  if (!is.null(request$output_dir)) {
    .semantica_checkpoint_save(answer, file.path(request$output_dir, "audit.rds"))
    utils::write.csv(answer$results, file.path(request$output_dir, "scores.csv"), row.names = FALSE)
    writeLines(c("# Native fixed-form scoring audit", "",
      paste("Forms:", nrow(answer$results)), paste("Reserved final ESEM jobs:", reserved),
      "ACO search jobs: 0. No providers or population truth used.",
      "This reports native scores and execution states, not psychometric calibration or validity.",
      "Grade known-answer forms separately; unavailable or rejected evaluations are retained."),
      file.path(request$output_dir, "REPORT.md"))
  }
  answer
}

#' Audit fixed forms with native proposal and final evaluation
#'
#' Uses actual ACO admission and scoring with no ant search, provider requests or
#' hidden population truth. Supports continuous report-only objectives. Fits are
#' bounded by the number of supplied forms and the final ESEM job allowance.
#' A final ESEM job may contain native solver fallback attempts.
#' @param cosine_sim_matrix Named item cosine matrix, as for [ACO_with_ESEM()].
#' @param df Native item metadata including `ID` and `Dimension` (and facets when
#'   used). Active content guards require their computed definition-alignment
#'   evidence; this audit does not invent that evidence or relax guards.
#' @param i.per.f Named selected-item counts per domain.
#' @param solutions Named list of unique character item-ID vectors.
#' @param stage `"both"` evaluates proposals and final scores; `"proposal"` skips ESEM.
#' @param max_forms Maximum number of forms accepted by this audit.
#' @param max_new_esem_fits Maximum reserved final ESEM candidate jobs. Zero is allowed.
#'   Reused cached fits consume no new-fit allowance.
#' @param seed Native seed; caller RNG is preserved.
#' @param output_dir Optional new directory for sealed results and readable scores.
#' @param execute FALSE previews the audit without evaluating or writing.
#' @param ... Additional [ACO_with_ESEM()] settings. Unsupported repair or reranking
#'   contexts are refused; configure weights, rotation and constraints explicitly.
#' @return A preview, or an audit containing native scores, availability, bounded
#'   fit counts and fitted evaluation payloads. Does not certify calibration.
#' @export
semantica_audit_solutions <- function(cosine_sim_matrix, df, i.per.f, solutions,
    stage = c("both", "proposal"), max_forms = 14L, max_new_esem_fits = 14L,
    seed = NULL, output_dir = NULL, execute = FALSE, ...) {
  stage <- match.arg(stage)
  max_forms <- .semantica_assert_positive_integer(max_forms, "max_forms")
  if (!is.numeric(max_new_esem_fits) || length(max_new_esem_fits) != 1L ||
      !is.finite(max_new_esem_fits) || max_new_esem_fits < 0 || max_new_esem_fits != floor(max_new_esem_fits))
    stop("max_new_esem_fits must be a nonnegative integer.")
  if (!is.list(solutions) || !length(solutions) || length(solutions) > max_forms ||
      any(!vapply(solutions, function(ids) is.character(ids) && length(ids) > 0L &&
        !anyNA(ids) && !anyDuplicated(ids), logical(1L)))) stop("Supply valid unique-ID forms within max_forms.")
  if (is.null(names(solutions))) names(solutions) <- sprintf("form%03d", seq_along(solutions))
  if (anyNA(names(solutions)) || any(!nzchar(names(solutions))) || anyDuplicated(names(solutions)))
    stop("Form names must be unique and nonempty.")
  execute <- .semantica_assert_flag(execute, "execute")
  dots <- list(...)
  allowed <- setdiff(names(formals(ACO_with_ESEM)), c("...", ".scoring_audit"))
  if (length(dots) && (is.null(names(dots)) || anyNA(names(dots)) ||
      any(!nzchar(names(dots))) || any(!names(dots) %in% allowed)))
    stop("Audit settings must be named ACO_with_ESEM arguments; unknown settings are not ignored.")
  request <- list(solutions = solutions, stage = stage, max_new_esem_fits = max_new_esem_fits,
    output_dir = output_dir)
  if (!execute) return(invisible(c(request, list(new_search_jobs = 0L, fits_executed = 0L))))
  if (!is.null(output_dir)) {
    if (length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir) || file.exists(output_dir) || dir.exists(output_dir))
      stop("Use a new output directory for the native audit.")
    if (!dir.create(output_dir, recursive = TRUE)) stop("Could not create the audit output directory.")
  }
  arguments <- utils::modifyList(list(cosine_sim_matrix = cosine_sim_matrix, df = df, i.per.f = i.per.f,
    semantic_esem_score_mode = "continuous_balanced", dfi_mode = "heuristic_semantic",
    psychometric_guard_action = "report_only", elite_multicriteria_rerank = FALSE,
    final_selection_mode = "scalar", final_structure_repair = FALSE, objective_polish = FALSE,
    dfi_warmup_iters = 0L, semantic_n_sensitivity = FALSE, validation_n_diagnostic = FALSE,
    final_dfi_recalibrate = FALSE, final_dddfi = FALSE, final_equivtest = FALSE,
    use_parallel = FALSE, n.cores = 1L, verbose = FALSE, seed = seed,
    evaluation_log_dir = if (is.null(output_dir)) NULL else file.path(output_dir, "esem-progress")),
    dots, keep.null = TRUE)
  arguments$.scoring_audit <- request
  .semantica_preserve_caller_rng(tryCatch(do.call(ACO_with_ESEM, arguments),
    error = function(e) {
      if (!is.null(output_dir)) .semantica_checkpoint_save(list(status = "audit_execution_error",
        error_class = class(e), message = conditionMessage(e)), file.path(output_dir, "error.rds"))
      stop(e)
    }, interrupt = function(e) {
      if (!is.null(output_dir)) .semantica_checkpoint_save(list(status = "audit_interrupted",
        reason = "Interrupted; inspect immutable per-form records. Unresolved forms were not replaced."),
        file.path(output_dir, "interrupted.rds"))
      stop(e)
    }))
}
