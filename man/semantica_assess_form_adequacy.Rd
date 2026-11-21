\name{semantica_assess_form_adequacy}
\alias{semantica_assess_form_adequacy}
\title{Assess scale adequacy for a fixed form}
\usage{
semantica_assess_form_adequacy(
  correlation = NULL,
  factor_assignment,
  structure_disposition = c("unavailable", "advance", "defer"),
  min_alpha = 0.70,
  max_htmt = 0.85
)
}
\arguments{
\item{correlation}{Named, trait-keyed participant-response correlation matrix
covering the selected items. Use \code{NULL} when evidence is unavailable.}
\item{factor_assignment}{Character vector of intended factors named by
selected item ID. Supply at least two items in each of at least two factors.}
\item{structure_disposition}{Recorded PFA decision for the same form.}
\item{min_alpha}{Minimum standardized alpha in each domain.}
\item{max_htmt}{Exclusive upper limit for absolute HTMT.}
}
\description{
Reports per-factor standardized alpha, maximum absolute HTMT, and the
minimum signed intended-versus-rival corrected item contrast separately
from the PFA disposition. The item contrast must be positive.
}
\details{
Use a participant sample separate from the reference used to grade the
decision. Supply correlations from correctly keyed responses.
Matrices from \code{\link{semantica_apply_empirical_calibration}} retain
prediction provenance when passed directly. Their metrics are exploratory
predictions and cannot yield a joint advance. Keep the returned matrix
attributes when using predicted correlations.
}
\value{
A list with the response-criterion status, evidence source, structure and
joint dispositions, configured criteria, and response metrics. Joint
advancement requires favorable PFA and supported declared response
correlations.
}
