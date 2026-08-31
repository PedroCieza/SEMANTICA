\name{plot.semantica_full_pipeline_result}
\alias{plot.semantica_full_pipeline_result}
\title{Plot a high-level SEMANTICA result}
\usage{\method{plot}{semantica_full_pipeline_result}(x, which = "summary", ...)}
\arguments{
\item{x}{A high-level result returned by \code{semantica_run()} or \code{semantica_full_pipeline()}.}
\item{which}{\code{"summary"}, \code{"all"}, or one or more diagnostic families accepted by \code{semantica_plot_all()}, including family shortcuts such as \code{"aco"}, \code{"esem"}, \code{"semantic"}, \code{"pfa"}, and \code{"dfi"}.}
\item{...}{Additional arguments passed to \code{semantica_plot_all()} for non-summary requests.}
}
\value{The stored/reconstructed summary plot, or a named diagnostic plot list.}
\description{Provides the conventional R \code{plot(result)} workflow while reusing stored analysis objects. Plotting does not change scale selection.}
