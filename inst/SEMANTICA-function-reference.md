# SEMANTICA Function Reference

This file is a compact command reference for the stable user-facing API.
The installed vignette `vignette("function-reference", package = "SEMANTICA")`
contains the complete argument catalogue and worked call patterns for every
public function. The Rd help pages generated from roxygen2 remain the
authoritative per-function reference. Helper functions without `@export` are
internal implementation details and may change without notice.

## Backend and Environment

### `SEMANTICA_BACKENDS`

Named backend registry. Use `names(SEMANTICA_BACKENDS)` to see supported backends and inspect each element for protocol, endpoints, default models, and authentication environment variables.

```r
names(SEMANTICA_BACKENDS)
SEMANTICA_BACKENDS$openai$auth_env
```

### `semantica_setup_conda(env_name, conda, python_ver, packages, force, verbose)`

Creates a Conda environment for local Python backends.

```r
semantica_setup_conda(
  env_name = "semantica",
  python_ver = "3.11",
  force = FALSE
)
```

### `semantica_activate_conda(env_name, conda, verbose)`

Activates a Conda environment for the current R session.

```r
semantica_activate_conda("semantica")
```

### `semantica_connect(backend, api_key, chat_model, embed_model, base_url, gguf_path, hf_token, timeout_s, verbose)`

Creates a `semantica_session` for cloud, local server, or Python backends.

```r
session <- semantica_connect(
  backend = "openai",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  timeout_s = 120
)
```

### `print.semantica_session(x, ...)`

Prints a compact summary of a session.

```r
print(session)
```

### `semantica_list_backends()`

Prints and invisibly returns the backend registry.

```r
backends <- semantica_list_backends()
```

## Generation, Embedding, and Wrapping

### `semantica_standardize_item_metadata(x, id_col, dimension_col, facet_col, item_col, strict)`

Converts item metadata into canonical `ID`, `Dimension`, `Facet`, and `item` columns.

```r
standard <- semantica_standardize_item_metadata(raw_items)
```

### `semantica_generate_items(session, scale_name, scale_description, factors, response_format, item_style, language, n_per_factor, overgenerate, max_retries, global_forbidden_max, temperature, verbose)`

Generates candidate items for each dimension or facet.

```r
items <- semantica_generate_items(
  session = session,
  scale_name = "Cognitive Agility",
  scale_description = "Adaptive and clear cognitive self-regulation.",
  factors = list(
    Clarity = list(description = "Clear thinking.", n_items = 8),
    Flexibility = list(description = "Adaptive thinking.", n_items = 8)
  ),
  response_format = "5-point Likert",
  item_style = "first-person declarative sentence",
  language = "English"
)
```

### `semantica_embed(items_tbl, session, embed_session, text_col, id_col, batch_size, normalize, verbose)`

Embeds item text with the active embedding backend.

```r
embedded <- semantica_embed(
  items_tbl = items,
  session = session,
  text_col = "item_text",
  id_col = "item_id",
  batch_size = 64,
  normalize = TRUE
)
```

### `semantica_wrap(embed_result, items_tbl, id_col, factor_col, min_items_per_factor, cosine_adjustment, semantic_calibration, verbose)`

Builds `cosine_sim_matrix`, item metadata, and factor selection targets for the optimizer.

```r
wrapped <- semantica_wrap(
  embed_result = embedded,
  id_col = "item_id",
  factor_col = "factor",
  min_items_per_factor = 3,
  cosine_adjustment = "none"
)
```

### `semantica_pipeline(backend, embed_backend, base_url, embed_base_url, api_key, embed_api_key, chat_model, embed_model, gguf_path, scale_name, scale_description, factors, n_per_factor, cosine_adjustment, semantic_calibration, verbose, ...)`

Runs connection, generation, embedding, and wrapping.

```r
prepared <- semantica_pipeline(
  backend = "openai",
  scale_name = "Cognitive Agility",
  scale_description = "Adaptive and clear cognitive self-regulation.",
  factors = factors,
  n_per_factor = 12,
  verbose = FALSE
)
```

### `semantica_full_pipeline(scale_name, scale_description, factors, backend, ...)`

Runs generation, embedding, ACO-ESEM optimization, and optional plotting in one call. The visible interface keeps the construct definition and selection targets up front; generation, optimizer, DFI/PFA, validation, and plotting controls live in named option lists. Legacy long-form arguments are still accepted by name through `...`.

```r
full <- semantica_full_pipeline(
  scale_name = "Cognitive Agility",
  scale_description = "Adaptive and clear cognitive self-regulation.",
  factors = factors,
  backend = "openai",
  items_per_factor = c(Clarity = 4, Flexibility = 4),
  optimization_options = list(ants = 60, max.iter = 30),
  plot_options = list(generate = TRUE)
)
```

## Export and Reload

### `semantica_print_items(items_tbl, max_chars)`

Prints generated items grouped by factor.

```r
semantica_print_items(items, max_chars = 80)
```

### `semantica_export(pipeline_result, prefix)`

Writes item text, metadata, and cosine matrix CSV files.

```r
semantica_export(wrapped, prefix = "cognitive_agility")
```

### `semantica_reload(prefix, i.per.f, default_i_per_f)`

Reloads CSV files created by `semantica_export()`.

```r
reloaded <- semantica_reload("cognitive_agility", default_i_per_f = 4)
```

## ACO-ESEM Optimization and Reporting

### `ACO_with_ESEM(cosine_sim_matrix, df, i.per.f, ...)`

Optimizes item selection by combining semantic redundancy control, sample-free PFA diagnostics, and ESEM/DFI diagnostics.
Use `?ACO_with_ESEM` or the function-reference vignette for the complete set
of search-budget, ESEM/DFI, PFA, validation-planning, and history arguments.
Parallel execution is capped at two workers for resource-conscious execution.

```r
result <- ACO_with_ESEM(
  cosine_sim_matrix = wrapped$cosine_sim_matrix,
  df = wrapped$df,
  i.per.f = c(Clarity = 4, Flexibility = 4),
  ants = 60,
  max.iter = 30,
  run_esem_during_search = TRUE,
  esem_sample_size = "auto",
  dfi_mode = "auto",
  use_parallel = FALSE
)
```

### `run_multi_seed_semantica(seeds, cosine_sim_matrix, df, i.per.f, verbose_seeds, ...)`

Runs the optimizer across several random seeds to estimate item-selection stability.

```r
multi <- run_multi_seed_semantica(
  seeds = 1:5,
  cosine_sim_matrix = wrapped$cosine_sim_matrix,
  df = wrapped$df,
  i.per.f = c(Clarity = 4, Flexibility = 4),
  ants = 40,
  max.iter = 20
)
```

### `report_semantica_v2(result, digits)`

Prints a compact results report.

```r
report_semantica_v2(result, digits = 4)
```

### `inspect_elite_archive(result, top_n)`

Prints top elite archive solutions.

```r
inspect_elite_archive(result, top_n = 5)
```

### `inspect_solution_history(result, top_n, sort_by)`

Prints top evaluated solutions by objective component.

```r
inspect_solution_history(result, top_n = 10, sort_by = "total")
```

## Visualization

All visualization functions expect an `ACO_with_ESEM()` result. Functions that
compare semantic structure also need the full cosine similarity matrix.
Function-specific display controls are documented in
`vignette("function-reference", package = "SEMANTICA")`.

```r
plot_pheromone_heatmap(result)
plot_fitness_evolution(result)
plot_semantic_networks(result, wrapped$cosine_sim_matrix)
plot_esem_loadings(result)
plot_loading_profiles(result)
plot_dfi_gauges(result)
plot_score_radar(result)
plot_semantic_discrimination(result, wrapped$cosine_sim_matrix)
plot_interactive_semantic_space(result, wrapped$cosine_sim_matrix, mode = "2d")
paths <- plot_esem_path_diagrams(result, wrapped$cosine_sim_matrix, df = wrapped$df)
plot_item_selection_frequency(result = result)
plot_crossloading_specificity(result)
plot_pfa_diagnostics(result)
plot_semantic_n_sensitivity(result)
plot_summary_of_results(result, wrapped$cosine_sim_matrix)
the_coolest_plot_ever(result, wrapped$cosine_sim_matrix)

plots <- semantica_plot_all(
  result = result,
  cosine_sim_matrix = wrapped$cosine_sim_matrix,
  df = wrapped$df,
  save = FALSE
)
```

`plot_esem_path_diagrams()` returns `p10a` and `p10b`. The BEFORE diagram now
uses item metadata when available and falls back to a labeled sample-free PFA
or semantic-proxy path view if a full-pool ESEM refit is unavailable. The AFTER
diagram includes factor-correlation rails when latent factor correlations can
be extracted.

`plot_pfa_diagnostics()` displays sign-anchored sample-free PFA loadings.
Factor-analysis axes are sign-indeterminate, so SEMANTICA orients recovered
PFA dimensions so intended-factor primary loadings are positive on average.
Negative cross-loadings can still appear and are not, by themselves, an
extraction error.

`plot_summary_of_results()` is a compact dashboard of the most important
SEMANTICA outputs: optimization/objective scores, fit checks, ESEM structure
diagnostics, sample-free PFA recovery, and semantic before/after change. It is
also included in `semantica_plot_all()` as `plots$plot_summary_of_results`.
`the_coolest_plot_ever()` is a manual-only semantic distillation map: it shows
how the candidate semantic item pool contracts into the selected factor
constellation, with centroid shifts, selected-factor breadth, and remaining
between-factor semantic overlap. It is exported for users to call directly but
is intentionally not run by `semantica_plot_all()`.

## Internal Helpers

The source files contain many unexported helpers for DFI simulation, ESEM syntax construction, PFA rotation, semantic similarity scoring, duplicate guarding, cache handling, and plotting layout. They are intentionally not exported because they are lower-level implementation details. Advanced users can inspect them in the source, but package code should rely on the exported functions above.
