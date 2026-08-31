% Generated from roxygen comments in R/analysis_extensions.R
\name{semantica_exact_semantic_reference}
\alias{semantica_exact_semantic_reference}
\title{Exact semantic-stage reference for tractable constrained forms}
\usage{
semantica_exact_semantic_reference(
  similarity_matrix,
  factor_assignment,
  select_per_factor,
  max_combinations = NULL,
  eligible_items = NULL,
  redundancy_threshold = 0.85,
  dup_threshold = 0.9,
  within_similarity_target = NULL,
  within_similarity_band = 0.08,
  semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
  expected_factor_relations = NULL,
  nomological_weight = 0,
  item_facet_lookup = NULL,
  facets_by_factor = NULL,
  facet_coverage_weight = 0.15
)
}
\arguments{
\item{similarity_matrix}{Named square semantic similarity matrix.}
\item{factor_assignment}{Named vector assigning candidate item IDs to factors.}
\item{select_per_factor}{Named integer vector giving the exact number selected from each factor.}
\item{max_combinations}{Explicit maximum number of complete candidate forms the caller authorizes for exhaustive enumeration. There is deliberately no implicit default.}
\item{eligible_items}{Optional item IDs defining a pre-screened eligible pool.}
\item{redundancy_threshold, dup_threshold}{Semantic redundancy-policy references; these are selection-policy values rather than universal psychometric cutoffs.}
\item{within_similarity_target, within_similarity_band}{Semantic-objective controls. When the target is omitted it is estimated from the frozen eligible pool using SEMANTICA's internal target-estimation helper.}
\item{semantic_objective_mode}{Semantic objective used by SEMANTICA.}
\item{expected_factor_relations, nomological_weight}{Optional declared semantic nomological hypothesis used by the existing objective.}
\item{item_facet_lookup, facets_by_factor, facet_coverage_weight}{Optional facet metadata and weight matching the semantic-stage coverage multiplier.}
}
\value{A \code{semantica_exact_semantic_reference} object containing the exact best utility, all exactly tied best forms, constrained search-space size, and number of forms evaluated.}
\description{Exhaustively enumerates a factor-stratified item-selection problem when the constrained search space is small enough for an exact reference calculation. This is an optimizer-integrity benchmark for the frozen semantic-stage objective; it does not run PFA, ESEM, DFI, or participant-data analysis.}
\references{
Kilmen, S., & Bulut, O. (2025). Shortening Psychological Scales: Semantic Similarity Matters. \emph{Educational and Psychological Measurement, 85}(5), 910--934. \doi{10.1177/00131644251319047}

Jung, S.-J., & Seo, J.-W. (2025). A transformer-based embedding approach to developing short-form psychological measures. \emph{Frontiers in Psychology, 16}, 1640864. \doi{10.3389/fpsyg.2025.1640864}
}
