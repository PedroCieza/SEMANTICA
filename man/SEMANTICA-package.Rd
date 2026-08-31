\name{SEMANTICA-package}
\alias{SEMANTICA}
\alias{SEMANTICA-package}
\docType{package}
\title{SEMANTICA: Start Here}
\description{SEMANTICA supports semantic-assisted psychometric scale development and pre-data item screening. Use \code{semantica_run()} for the main workflow and \code{semantica_full_pipeline()} when additional configuration or participant-response validation is needed.}
\details{
A typical workflow is to check model setup with \code{semantica_check_setup()}, preview workload with \code{semantica_run_plan()}, run or execute the plan, inspect the six-part \code{semantica_run()} surface (\code{scale}, \code{items}, \code{diagnostics}, \code{plots}, \code{provenance}, \code{advanced}), use \code{summary()} for detailed diagnostics, plot with \code{plot()}, and preserve the analysis with \code{semantica_save_bundle()}. The complete canonical result is retained under \code{advanced}; direct \code{semantica_full_pipeline()} calls continue to return that advanced object directly.

Advanced inspection is available through \code{semantica_item_review()}, \code{semantica_factor_review()}, \code{semantica_diagnostics()}, \code{semantica_provenance()}, and \code{semantica_config()}. Participant responses collected later can be attached to an existing selected scale with \code{semantica_validate()} without regenerating items or rerunning ACO.

If item text already exists, use the documented embedding -> \code{semantica_wrap()} -> optimizer path. If embeddings already exist, use \code{semantica_import_embeddings()}.

Embedding-derived semantic/PFA/ESEM/HTMT-like/DFI outputs are proxy diagnostics from one representation family. Participant-response evidence is reported separately when supplied.
}
\seealso{\code{\link{semantica_run}}, \code{\link{semantica_run_plan}}, \code{\link{semantica_execute}}, \code{\link{semantica_view}}, \code{\link{semantica_items}}, \code{\link{semantica_diagnostics}}, \code{\link{semantica_validate}}, \code{\link{semantica_save_bundle}}}
