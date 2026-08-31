\name{semantica_run_plan}
\alias{semantica_run_plan}
\alias{print.semantica_run_plan}
\title{Preview a SEMANTICA run without model calls}
\usage{
semantica_run_plan(
  scale_name, scale_description, factors, pool_items = 15L,
  selected_items = NULL, overgenerate = 2,
  aco = c("standard", "fast", "full"), llm = "openai",
  chat_model = NULL, embed_model = NULL, workers = "auto",
  language = "English", response_format = "5-point Likert",
  item_style = "first-person declarative sentence", temperature = 0.8,
  structured_output = c("auto", "numbered", "json"), prompts = NULL,
  seed = 1234L, progress = c("normal", "detailed", "quiet")
)
\method{print}{semantica_run_plan}(x, ...)
}
\arguments{
\item{scale_name,scale_description,factors,pool_items,selected_items,overgenerate,aco,llm,chat_model,embed_model,workers,language,response_format,item_style,temperature,structured_output,prompts,seed,progress}{Arguments matching the corresponding \code{semantica_run()} controls.}
\item{x}{A \code{semantica_run_plan} object.}
\item{...}{Unused by the print method.}
}
\value{A \code{semantica_run_plan} containing the visible workload summary and a credential-free execution specification.}
\description{Resolves factor allocation, generation workload, backend/model pairing, ACO preset, and worker requests without contacting providers or running the analysis.}
\details{The raw generation target is a planning estimate. Use \code{semantica_execute(plan)} to run the inspected specification. Credentials are not stored in plans.}
\seealso{\code{\link{semantica_execute}}, \code{\link{semantica_run}}}
