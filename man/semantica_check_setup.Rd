% Generated for SEMANTICA 0.5.8 QoL surface
\name{semantica_check_setup}
\alias{semantica_check_setup}
\alias{print.semantica_setup_check}
\title{Check backend and model setup before a SEMANTICA run}
\usage{
semantica_check_setup(
  llm = "openai",
  chat_model = NULL,
  embed_model = NULL,
  probe = FALSE,
  verify_models = TRUE,
  timeout_s = 10L
)

\method{print}{semantica_setup_check}(x, ...)
}
\arguments{
\item{llm,chat_model,embed_model}{The backend/model specification accepted by \code{semantica_run()}.}
\item{probe}{Logical; contact configured provider or local model registries using SEMANTICA's existing preflight machinery.}
\item{verify_models}{Logical; when probing, verify requested model IDs where the provider exposes a model registry.}
\item{timeout_s}{Positive preflight timeout in seconds.}
\item{x}{A \code{semantica_setup_check} object.}
\item{...}{Unused by the print method.}
}
\value{A \code{semantica_setup_check} list with readiness state, generation and embedding setup, blocking issues, warnings, and suggested actions.}
\description{
Provides a user-oriented setup check before a paid or lengthy run. The default check is local and verifies backend capabilities and credential presence; \code{probe = TRUE} additionally delegates to SEMANTICA's established backend preflight functions. It never generates questionnaire items or computes embeddings.
}
