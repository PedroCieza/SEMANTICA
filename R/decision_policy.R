# Central decision-policy constants and schema identifiers.
#
# SEMANTICA distinguishes numerical decision utilities used to rank candidate
# forms from empirical psychometric estimands.  The constants below therefore
# have explicit policy provenance.  Keeping them in one place reduces the risk
# that the casual API, advanced API, and core optimizer silently drift apart.

.SEMANTICA_DECISION_POLICY <- list(
  schema_version = "semantica-decision-policy-v1",
  objective_schema_version = "semantica-objective-schema-v6",
  evidence_schema_version = "semantica-evidence-schema-v3",
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

.semantica_policy_metadata <- function() {
  p <- .semantica_decision_policy()
  list(
    decision_policy_schema = p$schema_version,
    objective_schema_version = p$objective_schema_version,
    evidence_schema_version = p$evidence_schema_version,
    search_schema_version = p$search_schema_version,
    policy_origin = p$policy_origin
  )
}
