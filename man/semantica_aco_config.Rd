% Generated manually for the 0.4.0 source release;
% devtools::document() will regenerate from roxygen.
\name{semantica_aco_config}
\alias{semantica_aco_config}
\title{Configure an ACO search preset for semantica_run()}
\usage{
semantica_aco_config(
  mode = c("standard", "fast", "full"),
  ants = NULL,
  search_patience = NULL,
  max_total_iter = NULL
)
}
\arguments{
\item{mode}{One of \code{"fast"}, \code{"standard"}, or \code{"full"}.}
\item{ants}{Optional positive integer colony-size override.}
\item{search_patience}{Optional positive integer non-improvement patience override.}
\item{max_total_iter}{Optional positive integer hard iteration ceiling.}
}
\description{
Defines coherent literature-informed ACO search-effort/evidence presets for
\code{semantica_run()} with additional ACO controls available in \code{semantica_full_pipeline()} and
\code{ACO_with_ESEM()}.
}
\details{
The standard preset uses 60 ants, 40 non-improvement iterations of patience,
and a 60-iteration hard ceiling. Fast uses 20 ants and a smaller bounded
search; full retains the 60-ant base, extends the hard ceiling, and uses a fixed
five-iteration PFA/ESEM cadence with strict ESEM-parametric DFI. Standard uses a
fixed ten-iteration ESEM cadence. \code{semantica_full_pipeline()} retains adaptive ESEM scheduling unless explicitly changed.
All modes preserve evidence-stratified archives and canonical reranking.
}
\value{A \code{semantica_aco_config} list for \code{semantica_run()}.}
\references{
Leite, W. L., Huang, I.-C., & Marcoulides, G. A. (2008).
Item Selection for the Development of Short Forms of Scales Using an Ant
Colony Optimization Algorithm.
\emph{Multivariate Behavioral Research, 43}(3), 411--431.
\doi{10.1080/00273170802285743}

Raborn, A. W., & Leite, W. L. (2018). ShortForm: An R Package to Select Scale
Short Forms With the Ant Colony Optimization Algorithm.
\emph{Applied Psychological Measurement, 42}(6), 516--517.
\doi{10.1177/0146621617752993}
}
