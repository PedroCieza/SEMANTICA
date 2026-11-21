\name{semantica_structure_probe}
\alias{semantica_structure_probe}
\title{Probe item structure without a target assignment}
\usage{
semantica_structure_probe(
  cosine_matrix,
  n_factors = NULL,
  extraction = c("ml", "minres", "principal"),
  rotation = c("promax", "oblimin", "varimax", "none")
)
}
\arguments{
\item{cosine_matrix}{Finite, symmetric item cosine matrix with unit diagonal.}
\item{n_factors}{Positive factor count, or NULL for the heuristic count.}
\item{extraction}{Extraction method. Minimum residual requires package psych.}
\item{rotation}{Exploratory rotation. One-factor solutions are unrotated.}
}
\value{Counts, fit status, loadings, factor correlations, item assignments,
residual RMS, uniquenesses and matrix repair diagnostics.}
\description{
Fits sample-free factors using a supplied count or the descriptive
eigenvalue-greater-than-one heuristic.
}
