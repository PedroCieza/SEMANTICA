# SEMANTICA

[![R-CMD-check](https://github.com/PedroCieza/SEMANTICA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PedroCieza/SEMANTICA/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/PedroCieza/SEMANTICA/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/PedroCieza/SEMANTICA/actions/workflows/pkgdown.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

SEMANTICA is an R package for **semantic-assisted psychometric scale development and pre-data item screening**. It combines LLM item generation, semantic embeddings, construct-coverage diagnostics, ant colony optimization (ACO), sample-free PFA/ESEM/DFI proxy diagnostics, optional participant-response validation, reproducibility bundles, and visualization tools.

<img width="2276" height="1018" alt="semantica logo" src="https://github.com/user-attachments/assets/aa2033aa-aa45-4d5e-a64c-4a181dfe4c88" />

**Current build: 0.2.** Start a scale-development workflow with `semantica_run()`. Use `semantica_full_pipeline()` when you need additional configuration or participant-response validation.

> **Evidence boundary.** Sample-free semantic, PFA, ESEM, HTMT-like, and DFI outputs are pre-data proxy diagnostics derived from item representations. They do not establish reliability, construct validity, measurement invariance, DIF, criterion validity, or the factor structure that will occur in participant responses. When participant data are supplied, participant-based results are reported separately and should take precedence for response-data claims.

## Install

Install the current GitHub version with `pak` or `remotes`:

```r
# Option 1
install.packages("pak")
pak::pak("PedroCieza/SEMANTICA")

# Option 2
install.packages("remotes")
remotes::install_github("PedroCieza/SEMANTICA")

library(SEMANTICA)
```

For reproducible research, install the exact tagged version used in the analysis
(for example `PedroCieza/SEMANTICA@v0.2`) once that release tag is published.
A local source archive can also be installed with `install.packages(..., repos = NULL,
type = "source")`.

### Upgrading from the early public 0.1.0 repository

The recommended ordinary entry point is now `semantica_run()`. The established
`semantica_full_pipeline()` and lower-level analytical functions remain available
for advanced workflows; the compact `semantica_run()` result is a presentation
façade and retains the canonical result under `advanced` rather than replacing
or recalculating it. Review `NEWS.md` for the development history and use the
function documentation for current arguments and defaults.

## Five-minute workflow

### 1. Check the backend before spending time or API calls

```r
semantica_list_backends()

semantica_check_setup(
  llm = "ollama",
  probe = TRUE
)
```

`probe = FALSE` checks registry capabilities and credential presence only. `probe = TRUE` also uses SEMANTICA's existing backend preflight machinery to check reachable model registries when available.

For cloud APIs, place credentials in `.Renviron` so they do not appear in scripts:

```text
OPENAI_API_KEY=...
ANTHROPIC_API_KEY=...
GROQ_API_KEY=...
HF_TOKEN=...
```

Restart R after editing `.Renviron`.

### 2. Preview what the run will do

```r
plan <- semantica_run_plan(
  scale_name = "Academic Self-Regulation Scale",
  scale_description = "Goal direction, monitoring, and persistence in academic work.",
  factors = list(
    Goal_Direction = "Maintaining academic behavior around personally endorsed goals.",
    Monitoring = "Evaluating whether current study strategies are working."
  ),
  pool_items = 12,
  selected_items = 4,
  overgenerate = 2,
  llm = "ollama",
  language = "English",
  response_format = "5-point Likert"
)

plan
```

The plan performs **no model calls and no analysis**. It shows retained candidate counts, facet allocation, approximate raw-generation workload, final-form size, models/backends, ACO preset, and worker request. After inspection, `semantica_execute(plan)` runs that stored specification; credentials and custom backend functions are supplied at execution time rather than stored in the plan.

`pool_items` means **retained candidate items per factor**, not total items across the scale. `overgenerate` controls the approximate raw candidate target before deduplication/retention. Retries and deficit-aware generation can change the actual number of provider requests.

### 3. Run SEMANTICA

```r
result <- semantica_run(
  scale_name = "Academic Self-Regulation Scale",
  scale_description = "Goal direction, monitoring, and persistence in academic work.",
  factors = list(
    Goal_Direction = "Maintaining academic behavior around personally endorsed goals.",
    Monitoring = "Evaluating whether current study strategies are working."
  ),
  pool_items = 12,
  selected_items = 4,
  llm = list(
    backend = "ollama",
    chat_model = "llama3.2",
    embed_model = "nomic-embed-text"
  ),
  language = "English",
  response_format = "5-point Likert",
  item_style = "brief first-person declarative sentence",
  aco = "standard",
  seed = 20260828L
)
```

`semantica_run()` provides the main workflow with commonly used settings. `semantica_full_pipeline()` exposes the same pipeline with additional configuration.

Console progress is concise by default. Use `progress = "detailed"` for component-level telemetry or `progress = "quiet"` for scripting.

### 4. Inspect the final scale

```r
result                                 # compact six-part semantica_run() result
result$scale                           # what was selected and basic run context
result$items                           # final selected wording
result$diagnostics                     # immediate review/evidence summaries
result$plots                           # five core report plots, already generated
result$provenance                      # model/run provenance
result$advanced                        # complete canonical result, unchanged

summary(result)                        # detailed diagnostic report
summary(result, sections = c("semantic", "structural"))
semantica_evidence_status(result)

# Advanced navigation without manually expanding the canonical result:
semantica_view(result, view = "advanced")
semantica_view(result, view = "advanced", section = "structural")
semantica_view(result, view = "raw")  # identical to result$advanced
```

`semantica_run()` returns a genuinely compact six-part surface, so RStudio's Environment pane no longer asks regular users to start from dozens of top-level fields. The added `plots` group keeps the five core report graphics immediately available without asking users to regenerate them. No analytical information is discarded: the complete canonical result is retained under `result$advanced`. Historical direct access such as `result$optimization` or `result$fit_indices` is also preserved through compatibility accessors, while `semantica_items()`, `summary()`, diagnostics, validation, and bundle helpers continue to accept the compact object.
Accordingly, `length(result)` is 6 and `names(result)` reports those six user-facing groups. Researchers who explicitly need the canonical inventory can use `length(result$advanced)` and `names(result$advanced)`.

Direct `semantica_full_pipeline()` calls remain intentionally advanced and return the complete canonical object directly. With the default plot configuration, their canonical `$plots` field contains the same five core report plots; `semantica_plot_config(level = "full")` retains the complete plot suite. `semantica_view(result)` therefore defaults to the nine-section component map for direct full-pipeline results, while `semantica_run()` users normally work from the six visible groups above.

To see the resolved settings and model identities:

```r
semantica_config(result)
semantica_config(result, "generation")
semantica_config(result, changes_only = TRUE)
semantica_models(result)
semantica_provenance(result)
semantica_item_review(result)
semantica_factor_review(result)
```

### 5. Plot

```r
plot(result)                                      # summary plot
result$plots$plot_fitness_evolution               # stored ACO fitness history
result$plots$plot_esem_before                     # stored BEFORE path view
result$plots$plot_esem_after                      # stored final ESEM path view
result$plots$plot_pfa_diagnostics                 # stored PFA solution plot
plot(result, which = "aco")                      # generate/access broader ACO family
plot(result, which = c("esem", "pfa"))
plots <- semantica_plot_all(result, which = "all", progress = FALSE)
attr(plots, "semantica_plot_manifest")
```

The default BEFORE path view intentionally uses SEMANTICA's established sample-free proxy representation, so simply retaining the plot does not trigger a new full-pool ESEM estimation. Advanced users can opt into the existing refit behavior through `semantica_plot_config(level = "full", before_path_model = "refit")` when using `semantica_full_pipeline()`.

Optional plot failures are reported at the end without discarding plots that were generated successfully.

### 6. Export and preserve

For a readable copy of the final scale and diagnostics:

```r
semantica_export(result, prefix = "academic_self_regulation")
```

This writes selected items, evidence status, a text summary, and a sanitized configuration JSON. Set `include_candidates = TRUE` if the candidate-item metadata should also be exported.

To preserve the complete analysis and provenance, save a bundle:

```r
semantica_save_bundle(result, "academic_self_regulation_bundle.rds")
reloaded <- semantica_load_bundle("academic_self_regulation_bundle.rds")
```

CSV/JSON/text report exports are for reading and interchange; they are not reloaded into a complete analysis object. `semantica_reload()` / `semantica_reload_optimizer()` read the separate component-level optimizer-interchange format. The bundle is the provenance-preserving restoration artifact.


### Add participant responses later

If responses are collected after scale selection, attach them to the preserved result without regenerating items or rerunning ACO:

```r
validated <- semantica_validate(
  result,
  responses = pilot_responses,
  ordered = names(pilot_responses)
)

semantica_evidence_status(validated, labels = "both")
summary(validated, sections = "participant")
```

`semantica_validate()` uses the selected item IDs, stored factor structure, stored ESEM settings, and stored fit-cutoff configuration from the completed run.

## Backends and model setup

Run `semantica_list_backends()` for the live registry packaged with your installed version.

| Backend | Typical location | Credential | Generation | Embeddings |
|---|---|---|---|---|
| OpenAI | cloud | `OPENAI_API_KEY` | yes | yes |
| Anthropic | cloud | `ANTHROPIC_API_KEY` | yes | **no built-in embedding endpoint** |
| Groq | cloud | `GROQ_API_KEY` | yes | **no built-in embedding endpoint** |
| Ollama | local server | none | yes | yes |
| llama.cpp server | local server | usually none | yes | yes if server exposes endpoint |
| Generic OpenAI-compatible | local/custom server | service dependent | yes | yes if endpoint exists |
| Python Hugging Face | local Python/Conda | `HF_TOKEN` when model requires it | yes | yes |
| llama-cpp-python | local Python | none/model dependent | yes | yes |

A generation-only backend must be paired with an embedding backend. For example:

```r
llm_cfg <- semantica_llm_config(
  backend = "anthropic",
  embed_backend = "openai"
)

semantica_check_setup(
  llm = llm_cfg,
  chat_model = "your-claude-model",
  embed_model = "text-embedding-3-small",
  probe = TRUE
)
```

Provider model identifiers are passed through as supplied. Because provider catalogs can change independently of SEMANTICA, use preflight to check the current configuration before a run.

## ACO presets

`semantica_run()` provides three ACO effort presets:

| Preset | Use | Relative effort |
|---|---|---|
| `"fast"` | iteration, development, first-pass screening | lower |
| `"standard"` | routine scale-development runs | default |
| `"full"` | more intensive structural checking/calibration | higher |

These presets change search effort and diagnostic cadence.

`"full"` refers to the **ACO preset**, not to `semantica_full_pipeline()`.

## Factors and facets

Simple factors can be written as named descriptions:

```r
factors <- list(
  Awareness = "Noticing one's current emotional state.",
  Clarity = "Understanding and differentiating one's emotional state."
)
```

Richer factor specifications can include facets, examples, exclusions, and extra instructions:

```r
factors <- list(
  Persistence = list(
    description = "Maintaining purposeful academic effort despite difficulty.",
    facets = list(
      Sustained_Effort = "Continuing while the task remains difficult.",
      Setback_Recovery = "Resuming effort after an academic setback."
    ),
    forbidden = c("general optimism", "academic ability")
  )
)
```

With `semantica_run()`, `pool_items` is the retained total **per factor** and is allocated across its facets. Use `semantica_run_plan()` to see the resolved allocation before generation.

## Existing item pools

Choose the workflow that matches the material you already have:

### A. You have construct definitions but no items

Use `semantica_run()` as shown above.

### B. You already have item text but no embeddings

Use this input schema:

```r
items <- data.frame(
  item_id = c("a1", "a2", "a3", "b1", "b2", "b3"),
  factor = rep(c("A", "B"), each = 3),
  item_text = c("...", "...", "...", "...", "...", "...")
)
```

Create an embedding-purpose session, embed, then prepare the optimizer input:

```r
embed_session <- semantica_connect(
  backend = "ollama",
  embed_model = "nomic-embed-text",
  purpose = "embed"
)

embedded <- semantica_embed(
  items_tbl = items,
  session = embed_session,
  verbose = FALSE
)

prepared <- semantica_wrap(embedded, verbose = FALSE)
```

`prepared` can then be passed to the optimizer. See `vignette("semantica-user-workflows", package = "SEMANTICA")` for the complete sequence and selection-count setup.

### C. You already have item text and external embeddings

Do **not** manually fabricate SEMANTICA's embedding-result list. Use the validated import boundary:

```r
imported <- semantica_import_embeddings(
  embeddings = embedding_matrix,
  items_tbl = items,
  embedding_ids = rownames(embedding_matrix),
  provider = "external",
  model = "my-embedding-model"
)

prepared <- semantica_wrap(imported, verbose = FALSE)
```

SEMANTICA does not guess positional embedding/item alignment; IDs must match explicitly.

## Participant-response validation after a pilot

Participant-response validation is configured through `semantica_full_pipeline()`.

Participant data should be a data frame whose selected-item columns are named with the item IDs used by SEMANTICA. After collecting a pilot sample, rerun the pipeline with `validation_data` (and `validation_ordered` when appropriate).

```r
result_with_responses <- semantica_full_pipeline(
  scale_name = "Academic Self-Regulation Scale",
  scale_description = "...",
  factors = factors,
  llm = llm_cfg,
  chat_model = "...",
  embed_model = "...",
  validation_data = pilot_responses,
  validation_ordered = names(pilot_responses),
  seed = 20260828L
)
```

Participant-based results are stored and reported as a separate evidence family. They do not retroactively convert the earlier embedding-derived search diagnostics into participant evidence.

See `vignette("semantica-user-workflows", package = "SEMANTICA")` for data-shape and ID-matching guidance.

## Resource and GPU controls

For routine runs, use the `workers` argument:

```r
semantica_run(..., workers = "auto")
semantica_run(..., workers = 4)
semantica_run(..., workers = "serial")
```

Resource configuration also provides `cpu_cores`, `n.cores`, `max.cores`, and related controls when finer control is needed.

GPU use is split by task:

| Work | User control |
|---|---|
| local LLM generation | `chat_device` / backend-specific device map |
| local embeddings | `embedding_device` |
| dense cosine computation | `compute_device` |
| lavaan/ESEM/statistical work | CPU |

Do not assume that enabling one GPU setting moves every SEMANTICA stage to the GPU.

## Persistent embedding cache

Caching is enabled by default in the high-level configuration because repeated item text can reuse exact cached vectors for the same cache key.

```r
semantica_cache_info()
semantica_cache_info(result)
```

To bypass caching for a run:

```r
llm_cfg <- semantica_llm_config(
  backend = "ollama",
  embedding_cache = FALSE
)
```

To clear SEMANTICA's embedding cache, first inspect the target and then opt in explicitly:

```r
semantica_cache_info()
semantica_clear_cache(confirm = TRUE)
```

## Local Hugging Face / Python workflow

Python backends use the following setup workflow:

```r
semantica_setup_conda()
semantica_activate_conda()
semantica_check_setup(
  llm = "python_hf",
  probe = TRUE
)
```

See `?semantica_setup_conda`, `?semantica_activate_conda`, and the user-workflows vignette before configuring CUDA/device maps manually.

## Reproducibility: seed versus exact text replay

`seed` controls SEMANTICA's stochastic analysis and is forwarded to generation only when the backend has an implemented seed contract. A seed does **not** imply byte-identical remote/local LLM text across all providers or runtimes.

For exact downstream replay of a realized candidate pool, preserve the result/bundle and its provenance rather than relying on regeneration alone.

## If something fails

Start with:

```r
semantica_check_setup(llm = your_llm_config, probe = TRUE)
semantica_run_plan(...)
```

Common causes are:

- missing API credentials;
- a generation-only provider used without an embedding backend;
- an unreachable Ollama/llama.cpp/local server;
- a requested model not installed or not exposed by the provider registry;
- an existing item table without stable unique IDs/factor labels;
- external embeddings whose row IDs do not exactly match item IDs;
- an interrupted parallel session that needs `semantica_reset_resources()`;
- local Python/Conda not activated for Python backends.

See `vignette("semantica-troubleshooting", package = "SEMANTICA")` for the recovery path for each case.

## API layers

The package reference is organized into three working levels:

### Main workflow

Start here:

```text
semantica_run()
semantica_run_plan() / semantica_execute()
semantica_check_setup()
semantica_view() / semantica_items() / semantica_item_review() / semantica_factor_review()
semantica_overview() / semantica_diagnostics()
summary()
plot()
semantica_evidence_status() / semantica_provenance()
semantica_export()
semantica_save_bundle() / semantica_load_bundle()
```

### Extended configuration

Use when a setting is not available directly in `semantica_run()`:

```text
semantica_full_pipeline()
semantica_llm_config()
semantica_generation_config()
semantica_item_count_config()
semantica_quality_config()
semantica_compute_config()
semantica_resource_config()
semantica_pfa_config()
semantica_esem_config()
semantica_diagnostics_config()
semantica_plot_config()
```

### Components and research tools

Generation, embedding, `ACO_with_ESEM()`, calibration, telemetry, robustness, and related methods are available for custom workflows and methodological studies.

## Terminology

A compact glossary is available in `vignette("semantica-glossary", package = "SEMANTICA")`. In particular:

- **semantic proxy**: a quantity derived from item embeddings, not participant responses;
- **PFA**: sample-free factor-like diagnostic on semantic representation;
- **proxy ESEM**: ESEM fit to representation-derived structure, not observed participant covariance;
- **evidence family**: a source family such as theory/definitions, embeddings, or participant responses;
- **content guard**: conservative feasibility filtering used by `semantica_run()` for clear definition/exclusion conflicts when alternatives remain;
- **ACO preset**: a search-effort/evidence-cadence preset, not a different optimizer.

## Interpretation

SEMANTICA is best used as a **pre-data scale-development and screening tool**. Review selected wording substantively, retain provenance, collect participant responses, and perform appropriate response-based psychometric validation before making claims about score reliability or validity.

For interpretation guidance and validation-oriented methods, read:

```r
vignette("evidence-interpretation", package = "SEMANTICA")
```

## Citation and methodological references

To obtain the citation for the installed SEMANTICA version:

```r
citation("SEMANTICA")
```

The GitHub repository also includes `CITATION.cff`, which enables GitHub's
**Cite this repository** interface. Cite the specific software version used in
your analysis.

