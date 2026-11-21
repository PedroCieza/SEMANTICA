\name{semantica_pfa_gate}
\alias{semantica_pfa_gate}
\title{Apply a structural gate to recorded PFA diagnostics}
\usage{
semantica_pfa_gate(pfa, policy = c("all_items", "one_borderline"), details = FALSE)
}
\arguments{
\item{pfa}{The final PFA diagnostics from a SEMANTICA run.}
\item{policy}{Use \code{"all_items"} for the recorded disposition or
\code{"one_borderline"} for the experimental rule.}
\item{details}{Return threshold diagnostics for the evaluated rule.}
}
\value{The structural disposition by default. With \code{details = TRUE},
returns a list with the disposition, failed and unresolved item IDs, and
whether the borderline exception changed the recorded decision.}
\description{
The experimental rule allows one item up to .05 below the configured
primary loading threshold if the affected factor retains at least three
indicators meeting the threshold and all items meet the loading-margin rule.
Request \code{"one_borderline"} explicitly to use the exception. A favorable
disposition describes the fitted structure on its input matrix. Response
reliability and scale adequacy require their own evidence.
}
