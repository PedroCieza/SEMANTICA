#' SEMANTICA: Semantic Item Generation and ACO-ESEM Scale Construction
#'
#' SEMANTICA supports a complete psychometric item-development workflow:
#' generate candidate items with a large language model, embed the items,
#' convert embeddings into a semantic similarity matrix, optimize item
#' selection with ant colony optimization, evaluate a full exploratory
#' structural equation model (ESEM), and visualize the resulting solution.
#'
#' @section Main workflow:
#' 1. Connect to a generation or embedding backend with [semantica_connect()].
#' 2. Generate items with [semantica_generate_items()] or provide existing item
#'    text.
#' 3. Embed item text with [semantica_embed()] and prepare optimizer inputs with
#'    [semantica_wrap()].
#' 4. Select items with [ACO_with_ESEM()].
#' 5. Inspect and plot results with [report_semantica_v2()],
#'    [inspect_elite_archive()], [inspect_solution_history()], and
#'    [semantica_plot_all()].
#'
#' @section One-call workflow:
#' [semantica_pipeline()] runs generation, embedding, and wrapping.
#' [semantica_full_pipeline()] runs the full generation-to-visualization
#' workflow.
#'
#' @section Interpretation:
#' SEMANTICA uses semantic similarity as a content-screening proxy and ESEM/PFA
#' diagnostics as sample-free structural guidance. These results do not replace
#' validation on item-response data. Final construct validity decisions should
#' be based on response-data ESEM/CFA, reliability, validity evidence, and
#' replication in an appropriate validation sample.
#'
#' Candidate-pool retention is intentionally reported separately from the
#' selected solution: broad item pools support content coverage, while the ACO
#' objective evaluates the selected short form. When ESEM is requested during
#' search, only solutions successfully scored by ESEM enter the ESEM-guided
#' elite archive; a semantic-only fallback must be explicitly requested.
#'
#' @references
#' Asparouhov, T., & Muthen, B. (2009). Exploratory structural equation
#' modeling. \emph{Structural Equation Modeling, 16}(3), 397-438.
#' \doi{10.1080/10705510903008204}
#'
#' Clark, L. A., & Watson, D. (2019). Constructing validity: New developments
#' in creating objective measuring instruments. \emph{Psychological
#' Assessment, 31}(12), 1412-1427. \doi{10.1037/pas0000626}
#'
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
#' assessing discriminant validity in variance-based structural equation
#' modeling. \emph{Journal of the Academy of Marketing Science, 43}, 115-135.
#' \doi{10.1007/s11747-014-0403-8}
#'
#' Dorigo, M., & Stutzle, T. (2004). \emph{Ant Colony Optimization}. MIT Press.
#'
#' @section Automated examples:
#' Functions that require API credentials, local model servers, Python
#' environments, or long ESEM simulations use commented or `\dontrun{}` examples
#' so package checks do not depend on network services or external software.
#'
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom dynamic cfaHB cfaOne catHB catOne DDDFI equivTest
#' @importFrom foreach foreach %dopar%
#' @importFrom ggforce geom_ellipse
#' @importFrom ggplot2 ggplot aes annotate coord_cartesian coord_fixed element_blank element_line element_rect element_text facet_grid facet_wrap geom_col geom_crossbar geom_errorbar geom_errorbarh geom_hline geom_jitter geom_label geom_line geom_path geom_point geom_polygon geom_rect geom_ribbon geom_segment geom_smooth geom_text geom_tile geom_violin geom_vline ggsave guide_legend labs margin scale_alpha_continuous scale_colour_identity scale_colour_manual scale_fill_gradient2 scale_fill_identity scale_fill_manual scale_shape_manual scale_size_manual scale_x_continuous scale_y_continuous theme theme_minimal unit vars
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text scale_edge_alpha scale_edge_width
#' @importFrom GPArotation oblimin targetQ
#' @importFrom grDevices colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom httr2 request req_timeout req_body_json req_headers req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom htmlwidgets saveWidget
#' @importFrom igraph E V
#' @importFrom lavaan cfa fitted fitMeasures lavInspect sem simulateData
#' @importFrom Matrix nearPD
#' @importFrom patchwork plot_annotation wrap_plots
#' @importFrom plotly add_markers layout plot_ly
#' @importFrom reticulate conda_binary conda_list conda_remove conda_create conda_install import py_available py_config py_to_r r_to_py use_condaenv
#' @importFrom rlang := .data
#' @importFrom scales number_format percent_format rescale
#' @importFrom stats aggregate cmdscale complete.cases cor factanal median pchisq promax qchisq quantile reorder rWishart runif sd setNames var varimax
#' @importFrom stringr str_match str_split str_trim
#' @importFrom tibble as_tibble
#' @importFrom tidygraph tbl_graph
#' @importFrom utils combn flush.console globalVariables head modifyList read.csv setTxtProgressBar tail txtProgressBar write.csv
"_PACKAGE"
