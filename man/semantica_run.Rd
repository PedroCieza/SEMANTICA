\name{semantica_run}
\alias{semantica_run}
\title{Run a SEMANTICA scale-development workflow}
\usage{
semantica_run(
  scale_name,
  scale_description,
  factors,
  pool_items = 15L,
  selected_items = NULL,
  overgenerate = 2,
  prompts = NULL,
  aco = c("standard", "fast", "full"),
  llm = "openai",
  chat_model = NULL,
  embed_model = NULL,
  seed = NULL,
  verbose = TRUE,
  workers = "auto",
  language = "English",
  response_format = "5-point Likert",
  item_style = "first-person declarative sentence",
  temperature = 0.8,
  structured_output = c("auto", "numbered", "json"),
  progress = c("normal", "detailed", "quiet")
)
}
\arguments{
\item{scale_name}{Non-empty scale name.}
\item{scale_description}{Description of the overall construct.}
\item{factors}{Named factor list; each factor may be a description string or a richer factor/facet specification.}
\item{pool_items}{Retained candidate-item target per factor. Facets divide this factor-level target.}
\item{selected_items}{Final target per factor. \code{NULL} uses 4 items for one factor and 3 per factor otherwise.}
\item{overgenerate}{Positive multiplier for the approximate raw generation target before retention.}
\item{prompts}{Optional global/factor-specific prompt augmentation.}
\item{aco}{\code{"fast"}, \code{"standard"}, \code{"full"}, a \code{semantica_aco_config}, or small override list.}
\item{llm}{Backend string, \code{semantica_llm_config}, or named LLM list.}
\item{chat_model,embed_model}{Optional explicit model names.}
\item{seed}{Optional master seed for SEMANTICA stochastic analysis and supported generation seed contracts.}
\item{verbose}{Logical compatibility switch. \code{FALSE} forces quiet progress.}
\item{workers}{\code{"auto"}, \code{"serial"}, or a positive worker request.}
\item{language}{Requested item-generation language.}
\item{response_format}{Requested response-scale format included in generation instructions.}
\item{item_style}{Requested item-wording style.}
\item{temperature}{Non-negative provider sampling temperature passed through the established generation config.}
\item{structured_output}{\code{"auto"}, \code{"numbered"}, or \code{"json"}.}
\item{progress}{\code{"normal"} for concise user progress, \code{"detailed"} for component telemetry, or \code{"quiet"}.}
}
\description{
Main entry point for scale development. It uses the same analysis pipeline as
\code{semantica_full_pipeline()} with a smaller set of commonly used arguments.
}
\details{
Use \code{semantica_run_plan()} first when item/facet counts or approximate API
work are not obvious. \code{pool_items} is per factor, while
\code{overgenerate} controls the larger approximate raw target.

The ACO presets change search effort/evidence cadence without selecting a
different optimizer. \code{"full"} refers to the ACO preset, not to
\code{semantica_full_pipeline()}.

The default workflow uses the conservative content-alignment guard and the
relative-conservative semantic objective. These settings can be configured
through \code{semantica_full_pipeline()}.

When one factor is supplied, the established unidimensional branch is used. A
one-factor run requires at least four selected items so the one-factor
covariance proxy is overidentified rather than the saturated three-indicator
case.
}
\value{A compact \code{semantica_run_result} facade with six top-level groups: \code{scale}, \code{items}, \code{diagnostics}, \code{plots}, \code{provenance}, and \code{advanced}. The \code{plots} field retains the five core report graphics, while \code{advanced} contains the complete canonical \code{semantica_full_pipeline_result}, including \code{run_config}, unchanged. Historical direct field access such as \code{result$optimization} remains available through compatibility accessors.}
\seealso{\code{\link{semantica_run_plan}}, \code{\link{semantica_check_setup}}, \code{\link{semantica_view}}, \code{\link{semantica_items}}, \code{\link{semantica_full_pipeline}}}
\examples{
\dontrun{
result <- semantica_run(
  scale_name = "Emotion Regulation Scale",
  scale_description = "Related emotion-regulation dimensions.",
  factors = list(
    Awareness = "Noticing and recognizing one's emotional state.",
    Clarity = "Understanding and differentiating one's emotional state."
  ),
  llm = "ollama",
  language = "Spanish",
  response_format = "7-point Likert",
  aco = "standard"
)
result$scale
result$items
result$diagnostics
result$provenance
result$advanced
summary(result)
plot(result)
}
}
