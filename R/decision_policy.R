# Central decision-policy constants and schema identifiers.
#
# SEMANTICA distinguishes numerical decision utilities used to rank candidate
# forms from empirical psychometric estimands.  The constants below therefore
# have explicit policy provenance.  Keeping them in one place reduces the risk
# that the casual API, advanced API, and core optimizer silently drift apart.

.SEMANTICA_DECISION_POLICY <- list(
  schema_version = "semantica-decision-policy-v2",
  objective_schema_version = "semantica-objective-schema-v7",
  evidence_schema_version = "semantica-evidence-schema-v4",
  search_schema_version = "semantica-search-schema-v3",
  policy_origin = paste(
    "SEMANTICA decision-utility policy; literature-informed but not treated as",
    "an empirical psychometric parameter. Calibrate changes against held-out",
    "external evidence rather than tuning them on the analyzed item pool."
  ),
  quality_profiles = list(
    lenient = list(
      redundancy_threshold = 0.90, dup_threshold = 0.95,
      htmt_threshold = 0.90, cohesion_retention = 0.80,
      within_similarity_band = 0.10, facet_coverage_weight = 0.10,
      psychometric_guard_weight = 0.35,
      psychometric_guard_min_ave = 0.25,
      psychometric_guard_min_loading = 0.35,
      psychometric_guard_min_primary_ge_50 = 0.60
    ),
    strict = list(
      redundancy_threshold = 0.80, dup_threshold = 0.85,
      htmt_threshold = 0.85, cohesion_retention = 0.70,
      within_similarity_band = 0.06, facet_coverage_weight = 0.20,
      psychometric_guard_weight = 0.75,
      psychometric_guard_min_ave = 0.50,
      psychometric_guard_min_loading = 0.50,
      psychometric_guard_min_primary_ge_50 = 0.80
    ),
    standard = list(
      redundancy_threshold = 0.85, dup_threshold = 0.90,
      htmt_threshold = 0.85, cohesion_retention = 0.75,
      within_similarity_band = 0.08, facet_coverage_weight = 0.15,
      psychometric_guard_weight = 0.50,
      psychometric_guard_min_ave = 0.30,
      psychometric_guard_min_loading = 0.40,
      psychometric_guard_min_primary_ge_50 = 0.70
    )
  ),
  semantic_thresholds = list(
    provenance = paste(
      "Profile thresholds are SEMANTICA decision-policy references, not",
      "embedding-model-invariant psychometric cutoffs. Pool-relative adaptive",
      "thresholds remain experimental until externally calibrated."
    )
  ),
  esem = list(
    loading_quality = list(
      in_range_weight = 0.65,
      centrality_weight = 0.35,
      reference_lower = 0.50,
      reference_upper = 0.95,
      reference_center = 0.75
    ),
    ave_reference = 0.50,
    residual_reference = 0.10,
    current_weights = c(cfi = 0.30, rmsea = 0.28, srmr = 0.22, ave = 0.20),
    structure_weighted_fit = c(cfi = 0.40, rmsea = 0.35, srmr = 0.25),
    structure_weighted_top = c(fit = 0.35, ave = 0.20, structure = 0.45),
    # HTMT-like quantities are derived from the same semantic representation.
    # Fixed empirical HTMT cutoffs therefore remain descriptive references by
    # default instead of silently becoming participant-level validity gates.
    htmt_default_role = "diagnostic"
  ),
  final_multicriteria = list(
    base_weight = 0.85,
    diagnostic_weight = 0.15,
    origin = paste(
      "Scalar final diagnostic reranking retained for backward-compatible",
      "decision support; it is not Pareto dominance and is not a universal",
      "quality score."
    )
  )
)

.semantica_decision_policy <- function() .SEMANTICA_DECISION_POLICY

# Scalar validation is shared by evidence evaluation and presentation. A numeric
# compatibility alias is never sufficient when its evidence status is unavailable.
.semantica_metric_scalar <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) return(NA_real_)
  as.numeric(x)
}

.semantica_evaluate_criteria <- function(spec, measurements, statuses = list(),
                                        source = "semantic_geometry", role = "final_constraint") {
  spec$value <- vapply(spec$criterion, function(id) {
    .semantica_metric_scalar(measurements[[id]])
  }, numeric(1L))
  # Historical numeric aliases (notably unavailable HTMT = 0) are retained
  # separately and never presented as observed evidence.
  spec$numeric_compatibility_alias <- spec$value
  no_measurement <- vapply(spec$criterion, function(id)
    identical(statuses[[id]], "unavailable") || identical(statuses[[id]], "not_applicable"), logical(1L))
  spec$value[no_measurement] <- NA_real_
  spec$state <- vapply(seq_len(nrow(spec)), function(i) {
    id <- spec$criterion[i]
    if (!spec$constraint_active[i]) return("inactive")
    if (identical(statuses[[id]], "not_applicable")) return("not_applicable")
    if (identical(statuses[[id]], "unavailable") || !is.finite(spec$value[i]) || !is.finite(spec$threshold[i])) return("unavailable")
    met <- switch(spec$direction[i], ">=" = spec$value[i] >= spec$threshold[i],
                  "<=" = spec$value[i] <= spec$threshold[i], "==" = spec$value[i] == spec$threshold[i])
    if (isTRUE(met)) "met" else "not_met"
  }, character(1L))
  spec$passed <- spec$state %in% c("met", "not_applicable", "inactive")
  spec$availability <- ifelse(!is.finite(spec$value) | spec$state == "unavailable", "unavailable",
                             ifelse(spec$state == "not_applicable", "not_applicable", "observed"))
  spec$reason <- ifelse(spec$state == "unavailable", "required scalar measurement unavailable",
                       ifelse(spec$state == "not_applicable", "statistic not applicable to this model", ""))
  spec$criterion_id <- paste0("semantica.", spec$criterion)
  spec$metric_method_version <- paste0(spec$criterion, "-legacy-v1")
  spec$metric_method_version[spec$criterion == "ave"] <- "dominant_standardized_loading_mean_square-v1"
  spec$evidence_source <- source
  spec$matrix_representation <- if (source == "observed_responses") "response_correlation" else "configured_semantic_matrix"
  spec$scope <- "whole_form"
  spec$aggregation <- ifelse(spec$criterion == "ave", "minimum_factor_mean_assigned_loading_square", "metric_specific")
  spec$role <- ifelse(spec$constraint_active, rep_len(role, nrow(spec)), "inactive")
  spec$units <- "dimensionless"
  spec$origin <- "resolved_legacy_operational_policy"
  spec$calibration_id <- NA_character_
  spec$applicability <- "operational_reference_no_external_calibration_claim"
  spec$policy_version <- "legacy-integrity-v2"
  spec$resolved_configuration_hash <- .semantica_criterion_config_hash(
    spec[, c("criterion", "threshold", "direction", "constraint_active", "evidence_source", "role", "policy_version")])
  spec
}

.semantica_criterion_config_hash <- local({
  previous <- NULL
  previous_hash <- NULL
  function(config) {
    if (!identical(config, previous)) {
      previous_hash <<- .semantica_object_md5(config)
      previous <<- config
    }
    previous_hash
  }
})

.semantica_policy_metadata <- function() {
  p <- .semantica_decision_policy()
  list(
    decision_policy_schema = p$schema_version,
    operational_policy = "legacy-integrity-v2",
    objective_schema_version = p$objective_schema_version,
    evidence_schema_version = p$evidence_schema_version,
    search_schema_version = p$search_schema_version,
    policy_origin = p$policy_origin
  )
}

# Score references are metadata over the existing policy, never editable copies.
.semantica_score_reference_spec <- function(cutoffs, source = "semantic_geometry") {
  p <- .semantica_decision_policy()$esem
  ids <- c("cfi", "tli", "rmsea", "srmr")
  refs <- c(setNames(lapply(ids, function(id) cutoffs[[id]]), ids), list(
    ave_utility = p$ave_reference,
    loading_range_lower = p$loading_quality$reference_lower,
    loading_range_upper = p$loading_quality$reference_upper,
    loading_center = p$loading_quality$reference_center,
    residual_reference = p$residual_reference))
  tab <- data.frame(criterion = names(refs),
    threshold = vapply(refs, .semantica_metric_scalar, numeric(1L)),
    direction = c(">=", ">=", "<=", "<=", ">=", ">=", "<=", "==", "<="),
    constraint_active = FALSE)
  tab <- .semantica_evaluate_criteria(tab, list(), source = source)
  tab$role <- ifelse(tab$criterion == "tli", "descriptive", "optimization_utility")
  tab$availability <- ifelse(is.finite(tab$threshold), "reference_available", "unavailable")
  tab$origin <- "legacy_policy_or_declared_fit_reference"
  tab
}
