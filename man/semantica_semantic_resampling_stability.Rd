% Generated from roxygen comments in R/analysis_extensions.R
\name{semantica_semantic_resampling_stability}
\alias{semantica_semantic_resampling_stability}
\title{Resampling sensitivity for sample-free semantic separation}
\usage{
semantica_semantic_resampling_stability(
  similarity_matrix,
  factor_assignment,
  reps = 1000L,
  seed = 1L
)
}
\arguments{
\item{similarity_matrix}{Named square semantic similarity matrix.}
\item{factor_assignment}{Named vector mapping item IDs to intended factors.}
\item{reps}{Number of stratified pair bootstrap replicates.}
\item{seed}{Optional integer seed. The caller's RNG state is restored.}
}
\value{A list containing observed semantic-separation statistics, stratified pair-resampling sensitivity intervals, leave-one-item-out ranges, and explicit evidence/provenance notes.}
\description{Replaces the legacy random half-pair zeroing heuristic with two transparent descriptive sensitivity analyses: stratified resampling of the observed within- and between-factor pair distributions and an item jackknife. Pair similarities share items and are therefore dependent; the reported intervals describe sensitivity of the active embedding representation and are not respondent-population confidence intervals.}
