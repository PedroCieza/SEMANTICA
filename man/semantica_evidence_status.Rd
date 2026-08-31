\name{semantica_evidence_status}
\alias{semantica_evidence_status}
\title{Summarize evidence families in a SEMANTICA result}
\usage{semantica_evidence_status(result, labels = c("raw", "human", "both"))}
\arguments{
\item{result}{A SEMANTICA pipeline/full-pipeline result.}
\item{labels}{Status-label format. \code{"raw"} preserves stable machine-readable state tokens; \code{"human"} returns readable labels; \code{"both"} retains both.}
}
\description{Returns a compact evidence-family table distinguishing theory/definition, embedding-derived proxy, and participant-response evidence.}
\value{A data frame describing evidence status, participant basis, and scope.}
\seealso{\code{\link{semantica_overview}}, \code{\link{semantica_validate}}}
