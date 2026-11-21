\name{semantica_source_nomination}
\alias{semantica_source_nomination}
\title{Bind an initial nomination to recorded wording and its source}
\usage{
semantica_source_nomination(
  items,
  item_texts,
  source,
  source_family = c("alternate_encoder", "response_data", "synthetic"),
  fallback_items = NULL
)
}
\arguments{
\item{items}{Unique IDs of the nominated items.}
\item{item_texts}{Recorded wording from the nomination source, named by ID.}
\item{source}{Nonempty identifier for the nomination evidence.}
\item{source_family}{Declared kind of evidence.}
\item{fallback_items}{Optional primary-source form to try if the nominee fails
native eligibility, facet, or duplicate checks.}
}
\value{A source-bound initial nomination for one existing ant slot.}
\description{
Pass the nomination as \code{initial_items} in \code{\link{semantica_quality_config}}
or \code{\link{ACO_with_ESEM}}. SEMANTICA checks the fingerprint and exact
wording against the active pool. A wording mismatch stops the run.
Both forms use ordinary native feasibility checks. Source declarations are
recorded as provenance and do not establish validity.
}
