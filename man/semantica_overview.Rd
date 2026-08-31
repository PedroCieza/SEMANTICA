% Generated for SEMANTICA 0.5.8 QoL surface
\name{semantica_overview}
\alias{semantica_overview}
\title{Print a plain-language overview of a SEMANTICA result}
\usage{
semantica_overview(result, print_items = TRUE)
}
\arguments{
\item{result}{A high-level SEMANTICA result.}
\item{print_items}{Logical; print selected item wording grouped by intended factor.}
}
\value{Invisibly returns a named list containing the overview information.}
\description{
Displays the selected-form size, intended factors, participant-data status, a relative wording-review cue when available, the final item wording, and practical next steps. Use \code{summary(result)} for the full diagnostic report.
}
\details{
Relative review cues are descriptive within the current selected form; no universal pass/fail cutoff is applied to embedding-derived diagnostics.
}
