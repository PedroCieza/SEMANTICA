\name{semantica_export}
\alias{semantica_export}
\title{Export SEMANTICA results for reporting or optimizer interchange}
\usage{
semantica_export(
  pipeline_result, prefix = "SEMANTICA", include_candidates = FALSE,
  format = c("auto", "report", "optimizer"), quiet = FALSE
)
}
\arguments{
\item{pipeline_result}{A high-level result from \code{semantica_run()} or \code{semantica_full_pipeline()}, or a component result from \code{semantica_pipeline()} or \code{semantica_wrap()}.}
\item{prefix}{Character string prepended to output filenames; it may include a directory path.}
\item{include_candidates}{Logical; for report exports, also write the generated candidate table when available.}
\item{format}{Export contract. \code{"auto"} chooses report output for a high-level result and optimizer interchange for a component result. Explicit \code{"report"} and \code{"optimizer"} values require the matching result type.}
\item{quiet}{Logical; suppress completion messages while still returning output paths.}
}
\value{For report exports, invisibly returns a named list of written paths. Optimizer-interchange exports retain the historical invisible prefix return.}
\description{High-level results export the selected scale, evidence status, readable summary, and sanitized configuration. Component results retain the historical item/metadata/cosine CSV interchange format.}
\details{Report exports are not reconstructive. Use \code{semantica_save_bundle()} to preserve a complete analysis for later restoration. \code{semantica_reload()} and \code{semantica_reload_optimizer()} read only the optimizer-interchange format.}
\seealso{\code{\link{semantica_save_bundle}}, \code{\link{semantica_reload_optimizer}}}
