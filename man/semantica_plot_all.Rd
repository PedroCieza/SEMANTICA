\name{semantica_plot_all}
\alias{semantica_plot_all}
\title{Generate selected SEMANTICA diagnostic plots}
\usage{
semantica_plot_all(
  result, cosine_sim_matrix = NULL, df = NULL, multi_result = NULL,
  interactive_mode = c("2d", "3d"), save = FALSE, out_dir = ".",
  device = "png", width = 12, height = 8, dpi = 180,
  before_path_model = c("proxy", "refit"), before_path_refit_max_items = 60L,
  network_max_items = 200L, mds_max_items = 250L,
  path_proxy_max_items = 150L, include_interactive = TRUE,
  progress = TRUE, which = "all"
)
}
\arguments{
\item{result}{A high-level SEMANTICA result or an optimizer result from \code{ACO_with_ESEM()}.}
\item{cosine_sim_matrix}{Full cosine matrix. Optional for high-level results because the stored matrix is reused.}
\item{df}{Item metadata data frame.}
\item{multi_result}{Optional output from \code{run_multi_seed_semantica()}.}
\item{interactive_mode}{\code{"2d"} or \code{"3d"} for the interactive semantic-space plot.}
\item{save,out_dir,device,width,height,dpi}{Plot-saving controls.}
\item{before_path_model,before_path_refit_max_items,network_max_items,mds_max_items,path_proxy_max_items,include_interactive}{Existing diagnostic-plot controls.}
\item{progress}{Logical; when \code{FALSE}, suppress plotting progress and completion messages.}
\item{which}{\code{"all"}, a diagnostic-family shortcut (\code{"aco"}, \code{"esem"}, \code{"semantic"}, \code{"pfa"}, or \code{"dfi"}), or one or more plot names: \code{"pheromone"}, \code{"fitness"}, \code{"networks"}, \code{"loadings"}, \code{"loading_profiles"}, \code{"dfi"}, \code{"radar"}, \code{"discrimination"}, \code{"interactive"}, \code{"paths"}, \code{"selection_frequency"}, \code{"specificity"}, \code{"pfa"}, \code{"reference_n"}, or \code{"summary"}.}
}
\description{Builds only the requested diagnostic plots from already-computed analysis objects.}
\details{Optional plot failures are retained in the \code{semantica_plot_failures} attribute. A \code{semantica_plot_manifest} attribute records whether each requested plot was generated and, when saving is enabled, its output path. An explicit \code{before_path_model = "refit"} may run the existing guarded plotting refit; the default proxy path does not.}
\value{A named plot list with failure and output-manifest attributes.}
