% Generated from roxygen comments in R/analysis_extensions.R
\name{semantica_cluster_consensus}
\alias{semantica_cluster_consensus}
\title{Ensemble clustering consensus for semantic scale structure}
\usage{
semantica_cluster_consensus(
  similarity_matrix,
  factor_assignment,
  methods = c("average", "complete", "mcquitty")
)
}
\arguments{
\item{similarity_matrix}{Named square semantic similarity matrix.}
\item{factor_assignment}{Named intended factor assignment for each item.}
\item{methods}{Agglomerative-linkage methods passed to \code{stats::hclust()}.}
}
\value{A \code{semantica_cluster_consensus} object containing algorithm-level intended-factor recovery, between-algorithm agreement, and consensus co-assignment discrimination summaries.}
\description{Applies several deterministic agglomerative clustering rules to the same embedding-derived semantic representation and summarizes their co-assignment consensus and agreement with intended factor labels. Because every clustering is derived from the same embeddings, this is tagged as embedding-semantic structural evidence rather than an independent validation source and is not an additional ACO objective in the 0.5.x series.}
