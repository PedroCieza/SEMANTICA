#' SEMANTICA: Start Here
#'
#' SEMANTICA supports semantic-assisted psychometric scale development and
#' pre-data item screening. Start a scale-development workflow with
#' [semantica_run()]. Use [semantica_full_pipeline()] when you need finer
#' control over configuration or participant-response validation.
#'
#' @section Five-minute workflow:
#' ```r
#' library(SEMANTICA)
#'
#' # 1. Check the backend before expensive work.
#' semantica_check_setup(llm = "ollama", probe = FALSE)
#'
#' # 2. Preview item counts and approximate generation workload.
#' plan <- semantica_run_plan(
#'   scale_name = "Example Scale",
#'   scale_description = "Two related dimensions.",
#'   factors = list(
#'     Awareness = "Noticing relevant internal states.",
#'     Clarity = "Understanding and differentiating those states."
#'   ),
#'   llm = "ollama"
#' )
#' plan
#'
#' # 3. Run the scale-development workflow.
#' # result <- semantica_run(
#' #   scale_name = "Example Scale",
#' #   scale_description = "Two related dimensions.",
#' #   factors = list(
#' #     Awareness = "Noticing relevant internal states.",
#' #     Clarity = "Understanding and differentiating those states."
#' #   ),
#' #   llm = "ollama",
#' #   language = "English",
#' #   response_format = "5-point Likert"
#' # )
#'
#' # 4. After a run:
#' # result$scale
#' # result$items
#' # result$diagnostics
#' # result$provenance
#' # result$advanced
#' # summary(result)
#' # plot(result)
#' # semantica_save_bundle(result, "example_bundle.rds")
#' ```
#'
#' @section Choose an interface:
#' **Start a run:** [semantica_run()], [semantica_run_plan()],
#' [semantica_check_setup()], [semantica_view()], [semantica_items()],
#' [semantica_evidence_status()], [semantica_export()], and
#' [semantica_save_bundle()].
#'
#' **Configure further:** [semantica_full_pipeline()] and the
#' `semantica_*_config()` objects expose additional validation, quality,
#' compute, and diagnostic settings.
#'
#' **Build custom workflows:** generation, embedding, ACO/ESEM, calibration,
#' robustness, and telemetry functions are available as component tools.
#'
#' @section What the main arguments mean:
#' In [semantica_run()], `pool_items` is the number of **retained candidate
#' items per factor**. `selected_items` is the final target per factor.
#' `overgenerate` requests an approximately larger raw pool before
#' deduplication/retention. Use [semantica_run_plan()] to see these totals and
#' facet allocations before any model call.
#'
#' [semantica_run()] directly accepts `language`, `response_format`,
#' `item_style`, `temperature`, and `structured_output`. `aco = "fast"`,
#' `"standard"`, or `"full"` sets the search effort and evidence cadence.
#' `workers = "auto"` uses SEMANTICA's adaptive resource policy.
#'
#' @section Backends:
#' Run [semantica_list_backends()] for the packaged registry and
#' [semantica_check_setup()] before an expensive run. OpenAI uses
#' `OPENAI_API_KEY`, Anthropic uses `ANTHROPIC_API_KEY`, Groq uses
#' `GROQ_API_KEY`, and Hugging Face may use `HF_TOKEN`. Anthropic and Groq are
#' generation-only in the built-in registry and therefore need a separate
#' embedding backend for a full SEMANTICA workflow.
#'
#' @section Existing item pools:
#' If you already have item text, use the standard input columns `item_id`,
#' `factor`, optional `facet`, and `item_text`, then follow the documented
#' embedding -> [semantica_wrap()] -> optimizer workflow. If you
#' already have external embeddings, use [semantica_import_embeddings()] rather
#' than manually constructing an embedding-result list. See
#' `vignette("semantica-user-workflows", package = "SEMANTICA")`.
#'
#' @section Participant response data:
#' Participant-response validation is configured through
#' [semantica_full_pipeline()] using `validation_data` and, when needed,
#' `validation_ordered`. Response-data results are reported as a separate
#' evidence family. See the user-workflows vignette for the required ID/column
#' alignment.
#'
#' @section Results:
#' [semantica_run()] returns a compact six-part surface: `scale`, `items`,
#' `diagnostics`, `plots`, `provenance`, and `advanced`. The `plots` group keeps
#' the summary, ACO fitness evolution, BEFORE/AFTER ESEM path views, and PFA
#' solution plot immediately available. The complete canonical result is
#' retained under `advanced`, and historical direct component access remains
#' available for compatibility. Direct [semantica_full_pipeline()] calls keep
#' returning the complete advanced result object. [semantica_view()] provides
#' grouped navigation over the canonical result, [semantica_items()] returns the
#' selected scale in a stable table, and `summary(result)` remains the detailed
#' diagnostic report. `plot(result)` returns the summary visualization; the
#' stored core plots are also available through `result$plots`, and
#' [semantica_plot_all()] accepts the high-level result directly.
#' [semantica_config()] and [semantica_models()] expose resolved, sanitized run
#' settings and model identities.
#'
#' @section Saving and reproducibility:
#' [semantica_export()] creates human-readable selected-item/diagnostic files
#' for a high-level result. [semantica_save_bundle()] preserves the complete
#' analysis provenance and should be used when exact downstream
#' reproduction matters. A seed controls SEMANTICA stochastic analysis and only
#' controls generation when the backend implements that contract; it does not
#' universally guarantee byte-identical LLM text.
#'
#' @section Cache and recovery:
#' [semantica_cache_info()] shows persistent embedding-cache location/usage and
#' [semantica_clear_cache()] requires explicit confirmation before deletion.
#' If an interrupted parallel session leaves resources in an uncertain state,
#' use [semantica_reset_resources()]. See
#' `vignette("semantica-troubleshooting", package = "SEMANTICA")`.
#'
#' @section Interpretation boundary:
#' Sample-free semantic, PFA, ESEM, HTMT-like, and DFI outputs are proxy
#' diagnostics for pre-data screening. They do **not** establish reliability,
#' construct validity, measurement invariance, DIF, criterion validity, or the
#' participant-response factor structure. When participant data are supplied,
#' participant-based results are reported separately and should take precedence
#' for response-data claims.
#'
#' @docType package
#' @name SEMANTICA-package
#' @importFrom dynamic cfaHB cfaOne DDDFI equivTest
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
