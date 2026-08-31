% Generated for SEMANTICA 0.5.8 QoL surface
\name{semantica_items}
\alias{semantica_items}
\title{Extract the final selected scale}
\usage{
semantica_items(result, details = FALSE)
}
\arguments{
\item{result}{A high-level result returned by \code{semantica_run()} or \code{semantica_full_pipeline()}, or a compatible result containing selected-item metadata.}
\item{details}{Logical; append original selected-item diagnostic columns after the stable user-facing columns.}
}
\value{A data frame ordered to match the selected form when selection order is available. The stable leading columns are \code{item_id}, \code{factor}, \code{facet}, and \code{item_text}.}
\description{
Returns the selected item wording without requiring users to navigate SEMANTICA's nested result structure. Historical internal metadata names are normalized to one documented user schema.
}
\details{
This accessor does not recompute selection, embeddings, or diagnostics. It only reads the selected-item metadata already present in the result.
}
\examples{
\dontrun{
result <- semantica_run(...)
semantica_items(result)
}
}
