\name{semantica_corroborate_structure}
\alias{semantica_corroborate_structure}
\title{Challenge the selected form with separately sourced structural evidence}
\usage{
semantica_corroborate_structure(result, matrix = NULL, source = NULL,
  source_family = NULL, gate_policy = c("all_items", "one_borderline"))
}
\arguments{
\item{result}{SEMANTICA pipeline or ACO result with saved final PFA.}
\item{matrix}{A \code{\link{semantica_embedding_view}} or a named cosine or
correlation matrix covering the selected items. An embedding view checks the
selected wording and supplies its recorded provenance.}
\item{source}{Description identifying the separate source for a bare matrix.}
\item{source_family}{One of \code{"alternate_encoder"}, \code{"response_data"} or
\code{"synthetic"}. Required for a bare matrix.}
\item{gate_policy}{Structural gate applied to both views. See
\code{\link{semantica_pfa_gate}}.}
}
\description{
Refits the saved exploratory PFA specification using the same selected items
and a separately sourced matrix. Identical geometry or an embedding view
declaring the recorded primary encoder leaves a favorable primary fit
uncorroborated. A conflicting observed view defers. Agreement describes the
supplied evidence. Provenance is declared by the caller.
}
\value{The overall status, primary and corroborating dispositions, source
declarations and optional companion diagnostics.}
