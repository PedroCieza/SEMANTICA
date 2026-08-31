% Generated for SEMANTICA 0.5.8 QoL surface
\name{semantica_cache_info}
\alias{semantica_cache_info}
\title{Inspect the persistent embedding cache}
\usage{
semantica_cache_info(result = NULL, cache_dir = NULL)
}
\arguments{
\item{result}{Optional SEMANTICA result. When supplied, available cache telemetry from that run is included.}
\item{cache_dir}{Optional cache directory. \code{NULL} uses the run-recorded path when available, otherwise SEMANTICA's OS-appropriate default.}
}
\value{A named list with cache path, existence, entry count, disk size, and optional hit/miss telemetry.}
\description{
Answers operational questions about SEMANTICA's existing persistent embedding cache without changing it.
}
