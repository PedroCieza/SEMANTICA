# SEMANTICA

<img width="1600" height="715" alt="image" src="https://github.com/user-attachments/assets/3a8cd997-cca7-4fa0-b437-8df34740834f" />

SEMANTICA is an R package for semantic-assisted psychometric scale construction. It is designed around a one-call workflow that starts from a construct specification, generates candidate items with an LLM, embeds the item pool, screens semantic redundancy, selects items with ant colony optimization, evaluates the candidate structure with ESEM, and returns diagnostic plots and summaries.

SEMANTICA is intended for item-pool development and early structural screening. A selected item set still requires validation using participant response data; semantic similarity, proxy-ESEM, and sample-free PFA diagnostics do not by themselves establish construct validity.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("PedroCieza/SEMANTICA")
```

```r
library(SEMANTICA)
```

Issues and feature requests: <https://github.com/PedroCieza/SEMANTICA/issues>

## Recommended Workflow

For most users, start with `semantica_full_pipeline()`. It runs the complete SEMANTICA workflow:

1. Connect to generation and embedding backends.
2. Generate candidate psychometric items from factor prompts.
3. Embed the generated item pool.
4. Build semantic similarity matrices.
5. Select a reduced item set with ACO.
6. Evaluate the selected solution with ESEM and fit diagnostics.
7. Return selected items, summaries, diagnostics, and plots.

The lower-level functions (`semantica_connect()`, `semantica_generate_items()`, `semantica_embed()`, `semantica_wrap()`, `ACO_with_ESEM()`, and `semantica_plot_all()`) remain available for advanced workflows, debugging, or reusing existing item pools.

## Simple Full-Pipeline Example

This example uses Groq for item generation and OpenAI for embeddings. Store real API keys in `.Renviron` rather than typing them directly into scripts.

```r
library(SEMANTICA)

groq_key <- Sys.getenv("GROQ_API_KEY")
openai_key <- Sys.getenv("OPENAI_API_KEY")

factors <- list(
  Clarity = list(
    description = "Clear, focused, and organized thinking."
  ),
  Flexibility = list(
    description = "Adaptive thinking when information or circumstances change."
  )
)

i_per_factor <- c(
  Clarity = 3L,
  Flexibility = 3L
)

set.seed(123)

result <- semantica_full_pipeline(
  # Scale specification
  scale_name = "Cognitive Agility",
  scale_description = paste(
    "A brief self-report scale for adaptive, organized,",
    "and flexible thinking in everyday problem solving."
  ),
  factors = factors,
  backend = "groq",
  candidate_items_per_factor = 10L,
  items_per_factor = i_per_factor,

  # LLM and embedding setup
  generation_options = list(
    embed_backend = "openai",
    api_key = groq_key,
    embed_api_key = openai_key,
    chat_model = "meta-llama/llama-4-scout-17b-16e-instruct",
    embed_model = "text-embedding-3-small",
    temperature = 1
  ),

  # Faster exploratory run
  dfi_options = list(dfi_mode = "heuristic_semantic"),

  # ACO-ESEM search
  optimization_options = list(
    ants = 50L,
    max.iter = 20L,
    esem_every = 10L,
    run_esem_during_search = TRUE,
    esem_weight = 0.50,
    use_parallel = TRUE,
    n.cores = 6L
  )
)
```

Inspect the selected items and main diagnostics:

```r
result$best_items
result$factor_assignment
result$selected_item_metadata
result$fit_indices[c("cfi", "rmsea", "srmr", "htmt_max")]
result$semantic_score
result$summary
names(result$plots)
```

Notes on the main arguments:

- `backend` controls the LLM used for item generation.
- `generation_options$embed_backend` controls the model provider used for embeddings. If omitted, SEMANTICA uses the same backend as `backend`.
- `factors` is the named construct specification used as prompt material.
- `candidate_items_per_factor` controls the generated candidate item count per factor.
- `items_per_factor` controls the number of selected final items per factor.
- `dfi_options = list(dfi_mode = "heuristic_semantic")` skips simulated DFI calibration for a faster exploratory run.
- `optimization_options$run_esem_during_search = TRUE` allows ESEM diagnostics to guide the ACO search.
- `optimization_options$esem_weight` controls how much the ESEM component contributes to the search objective.
- Plots default to `TRUE`; use `plot_options = list(generate = FALSE)` to skip plot generation.
- `dfi_options$final_dddfi` defaults to `FALSE`, so final DDDFI cutoffs are not computed unless explicitly requested.

## Complete HEXACO Example

The package includes an editable simple full-pipeline example with a HEXACO factor specification:

```r
file.show(system.file(
  "examples",
  "simple_full_pipeline_example.R",
  package = "SEMANTICA"
))
```

A longer worked HEXACO example is also installed:

```r
file.show(system.file(
  "examples",
  "hexaco_full_pipeline_example.R",
  package = "SEMANTICA"
))
```

## Existing Item Pools

If item text and embeddings already exist, generation can be skipped. In that case, use `semantica_wrap()` to prepare the item pool and `ACO_with_ESEM()` to run selection:

```r
items_tbl <- data.frame(
  item_id = paste0("item_", 1:6),
  factor = rep(c("Clarity", "Flexibility"), each = 3),
  item_text = c(
    "I can keep my thoughts organized under pressure.",
    "I explain complex ideas in a clear sequence.",
    "I notice when my thinking becomes scattered.",
    "I adapt my approach when a plan stops working.",
    "I can shift perspectives when new evidence appears.",
    "I adjust my thinking when conditions change."
  )
)

embeddings <- matrix(
  c(
    0.90, 0.10, 0.20,
    0.88, 0.12, 0.18,
    0.86, 0.15, 0.22,
    0.15, 0.88, 0.25,
    0.12, 0.86, 0.28,
    0.18, 0.84, 0.23
  ),
  nrow = 6,
  byrow = TRUE,
  dimnames = list(items_tbl$item_id, NULL)
)

embed_result <- list(
  embeddings = embeddings,
  items_tbl = items_tbl,
  embed_model = "manual-example",
  embedding_diagnostics = list(normalized = FALSE)
)

wrapped <- semantica_wrap(
  embed_result,
  min_items_per_factor = 3,
  verbose = FALSE
)

result <- ACO_with_ESEM(
  cosine_sim_matrix = wrapped$cosine_sim_matrix,
  df = wrapped$df,
  i.per.f = c(Clarity = 3L, Flexibility = 3L),
  dfi_mode = "heuristic_semantic",
  run_esem_during_search = FALSE,
  verbose = FALSE
)
```

## Documentation

Function help is available inside R:

```r
?semantica_full_pipeline
?ACO_with_ESEM
?semantica_wrap
```

The package includes vignettes:

```r
vignette("semantica-workflow", package = "SEMANTICA")
vignette("function-reference", package = "SEMANTICA")
```

## Development Checks

To validate local changes:

```r
devtools::document()
devtools::test()
devtools::check()
```
