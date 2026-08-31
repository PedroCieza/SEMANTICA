% Generated for SEMANTICA 0.5.8 QoL surface
\name{semantica_clear_cache}
\alias{semantica_clear_cache}
\title{Clear the persistent embedding cache explicitly}
\usage{
semantica_clear_cache(cache_dir = NULL, confirm = FALSE)
}
\arguments{
\item{cache_dir}{Optional cache directory; \code{NULL} uses SEMANTICA's default cache directory.}
\item{confirm}{Must be \code{TRUE} before any cache contents are deleted.}
}
\value{Invisibly returns the cache directory.}
\description{
Deletes only hashed \code{.rds} embedding-cache entries under the selected SEMANTICA cache directory. Other files in a user-supplied directory are left untouched. The explicit confirmation requirement is intended to prevent accidental deletion. Use \code{semantica_cache_info()} first when unsure which path is active.
}
