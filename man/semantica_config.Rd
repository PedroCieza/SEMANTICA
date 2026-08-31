\name{semantica_config}
\alias{semantica_config}
\title{Inspect the resolved configuration of a SEMANTICA run}
\usage{
semantica_config(result, section = NULL, changes_only = FALSE)
}
\arguments{
\item{result}{A high-level SEMANTICA result.}
\item{section}{Optional top-level configuration section, for example \code{"generation"}, \code{"models"}, \code{"resources"}, \code{"quality"}, or \code{"optimizer"}.}
\item{changes_only}{Logical; show only fields that differ from stable constructor defaults where a baseline is available. Runtime-resolved fields without a constructor baseline are retained.}
}
\value{The sanitized resolved configuration, the requested section, or a recursive difference view.}
\description{Shows the settings that reached the high-level execution boundary after defaults, presets, and configuration precedence were resolved.}
\details{This is a read-only accessor. It does not change or rerun the analysis.}
