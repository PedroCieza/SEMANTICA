\name{semantica_pool_adequacy}
\alias{semantica_pool_adequacy}
\title{Audit response-supported forms in a small candidate pool}
\usage{
semantica_pool_adequacy(correlation, factor_assignment, select_per_factor,
  max_combinations, eligible_items = NULL, feasible = NULL,
  min_alpha = 0.70, max_htmt = 0.85)
}
\arguments{
\item{correlation}{Trait-keyed participant response correlation matrix with
matching item IDs on both axes and unit diagonal.}
\item{factor_assignment}{Character vector of factor labels named by item ID.}
\item{select_per_factor}{Named integer counts of at least two items for each
of at least two declared factors.}
\item{max_combinations}{Positive whole-number cap on complete forms.}
\item{eligible_items}{Optional item IDs to consider before enumeration.}
\item{feasible}{Optional function of selected item IDs returning a single TRUE
or FALSE. Use it to apply the same declared constraints as selection.}
\item{min_alpha}{Minimum standardized alpha for each domain.}
\item{max_htmt}{Exclusive upper limit for the maximum absolute-correlation HTMT.}
}
\description{
Enumerates complete fixed-length forms within an explicit cap. A form has
response support when each domain meets the alpha floor, each pair meets the
HTMT ceiling, and each item's signed correlation with its corrected own
composite exceeds its largest absolute correlation with any rival composite.
Use held-out participant responses for evaluation only. A ceiling describes
the supplied matrix, item pool, lengths, and criteria.
}
\value{
A list with the number of complete and evaluated forms, response-supported
counts before and after the feasibility callback, per-domain maximum alpha
before the callback, and a table of item IDs and metrics for each form.
Status is \code{supported_forms_available}, \code{no_supported_forms},
\code{no_feasible_forms}, \code{response_evidence_unavailable}, or
\code{budget_exceeded}. When the budget is exceeded the form counts that
require evaluation are unknown.
}
