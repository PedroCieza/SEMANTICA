# ==============================================================================
# SEMANTICA simple full-pipeline example
# ==============================================================================
#
# This script shows the smallest practical use of semantica_full_pipeline().
# It uses the package defaults wherever possible.
#
# In this version, the example remains "simple" because the analysis is run
# through one high-level function call. Several common options are explicit so
# the run is reproducible: Groq is used for item generation, a free local
# open-source embedding backend is used for embeddings, heuristic semantic
# cutoffs are used for speed, and ESEM is allowed to guide the ACO search.
#
# Required inputs used below:
# - scale_name: a short name for the scale.
# - scale_description: a brief description of the construct to measure.
# - factors_tfeq: a named list of the theoretical factors. It contains the
#   prompts for the LLM to generate content.
# - groq_key: API credential for item generation.
# - A local embedding backend. The default below is Ollama with
#   `nomic-embed-text`, which is free after local installation.
#
# The concrete call below overrides some package defaults. For example, it uses
# backend = "groq" and generation_options$embed_backend = "ollama" instead of
# the single-backend default. The following default list applies only when
# these arguments are not supplied by the user.
#
# Important default choices used implicitly in this example:
# - backend = "openai"
# - generation_options$embed_backend is omitted, so the same backend is used
#   for embeddings. In this script we set it explicitly to a local backend.
# - candidate_items_per_factor = 15L, so 15 candidate items are generated per
#   factor.
# - items_per_factor = NULL, so 3 items are selected per factor.
# - dfi_options$final_dddfi is omitted, so final DDDFI cutoffs are not computed
#   unless you explicitly request them.
# - plot_options$generate is omitted, so diagnostic plot objects are returned.
# - plot_options$save is omitted, so plots are not written to disk.
#
# Optional arguments commonly added for a quick test run:
# - candidate_items_per_factor: number of candidate items to generate per
#   factor.
# - items_per_factor: named vector with the number of items to select per
#   factor.
# - optimization_options: ACO search size, ESEM cadence, and parallel settings.
# - plot_options = list(generate = FALSE): skip diagnostic plot generation.
# - verbose: set FALSE for a quieter console.
#
# This example uses the exported function name in the package:
# semantica_full_pipeline().

library(SEMANTICA)


# OBJECTS NEEDED AS INPUTS

# API keys

groq_key <- "your_groq_key" # used for Groq item generation
openai_key <- "your_openai_key" # optional; only used for OpenAI embeddings

# Prefer storing real keys in .Renviron instead of typing them into scripts.
# Example:
# groq_key <- Sys.getenv("GROQ_API_KEY")
# openai_key <- Sys.getenv("OPENAI_API_KEY")


# EMBEDDING OPTIONS
#
# The active pipeline call below uses Ollama for free local embeddings.
# To use a different embedding backend, replace only the embedding lines inside
# `generation_options`.
#
# 1. Ollama, free and local:
#    Install Ollama, run `ollama pull nomic-embed-text`, and keep Ollama open
#    or run `ollama serve`.
#
# 2. Hugging Face, free and local:
#    Run `semantica_setup_conda("semantica")` once, restart R, then run
#    `semantica_activate_conda("semantica")` before the pipeline.
#
# 3. OpenAI:
#    Requires OpenAI API credits and `openai_key`.
#
# Groq is still used for item generation in all three examples.


# Prompts
#
# Keep the prompt object short: define general item-writing rules once, then
# give SEMANTICA the TFEQ-R-18 dimensions.

tfeq_item_rules <- paste(
  "Generate brief first-person declarative items about everyday eating behavior.",
  "Write items for a high level of the target eating-behavior dimension.",
  "Use everyday adult language; avoid clinical, moralizing, extreme, or diagnostic wording.",
  "Avoid double-barrelled items, causal explanations, abstract construct labels, and rare scenarios.",
  "Avoid negations such as 'not', 'never', 'hardly', and 'do not' unless absolutely necessary.",
  "Do not mention the dimension name directly in the item.",
  "Keep each item focused on one observable tendency.",
  "Be creative and explore different ways in generating items.",
  sep = "\n"
)

# Factors assumed by the theoretical model. Each factor only needs a short
# description and the shared item-writing rules.

factors_tfeq <- list(
  Cognitive_Restraint = list(
    description = paste(
      "Conscious effort to limit food intake in order to manage body weight."
    ),
    extra_instructions = tfeq_item_rules
  ),

  Uncontrolled_Eating = list(
    description = paste(
      "Tendency to eat in response to internal cues regardless of dietary plans."
    ),
    extra_instructions = tfeq_item_rules
  ),

  Emotional_Eating = list(
    description = paste(
      "Tendency to consume food in response to negative emotions."
    ),
    extra_instructions = tfeq_item_rules
  )
)

# Your desired final scale length

i_per_dimension <- c(
  Cognitive_Restraint = 6L,
  Uncontrolled_Eating = 9L,
  Emotional_Eating = 3L
)

# SEMANTICA USAGE
#
# Argument guide for the call below:
# - backend chooses the provider for item generation. Groq is used here.
# - generation_options groups embedding, credential, model, and prompt-tuning
#   controls. A local open-source model is used here for embeddings by default.
# - factors is the construct specification and prompt material.
# - candidate_items_per_factor controls how many candidate items are retained
#   from generation per broad factor before ACO selection.
# - items_per_factor controls the final selected item count per factor.
# - dfi_options = list(dfi_mode = "heuristic_semantic") skips simulated DFI
#   calibration. This is faster and useful for examples or exploratory runs,
#   but less calibrated than "auto" or the ESEM DFI modes.
# - optimization_options controls the ACO search effort, ESEM cadence, ESEM
#   objective weight, and parallel settings.

set.seed(123)

result <- semantica_full_pipeline(
  # Information about your desired scale
  scale_name = "TFEQ-R-18-AI",
  scale_description = paste(
    "Three-dimensional eating behavior scale for general adults,",
    "measuring cognitive restraint, uncontrolled eating, and emotional eating."
  ),
  factors = factors_tfeq, # User prompts
  backend = "groq",
  candidate_items_per_factor = 10L, # number of items to be generated per factor
  items_per_factor = i_per_dimension, # number of items to be retained

  # LLM and embedding setup
  generation_options = list(
    api_key = groq_key,
    chat_model = "meta-llama/llama-4-scout-17b-16e-instruct", # content generation
    temperature = 1, # generation variability

    # Embeddings, option 1: Ollama, free and local.
    embed_backend = "ollama",
    embed_model = "nomic-embed-text",
    embed_batch_size = 1L

    # Embeddings, option 2: Hugging Face, free and local.
    # First run `semantica_activate_conda("semantica")`, then replace the
    # three Ollama lines above with:
    # embed_backend = "python_hf",
    # embed_model = "sentence-transformers/all-MiniLM-L6-v2",
    # embed_batch_size = 1L

    # Embeddings, option 3: OpenAI.
    # Replace the three Ollama lines above with:
    # embed_backend = "openai",
    # embed_api_key = openai_key,
    # embed_model = "text-embedding-3-small",
    # embed_batch_size = 64L
  ),

  # DFI calibration
  # The heuristic mode avoids the long DFI simulation step. The final ESEM fit
  # is still compared against the active heuristic/search cutoffs.
  dfi_options = list(
    dfi_mode = "heuristic_semantic"
  ),

  # ACO search
  optimization_options = list(
    ants = 50L, # Ants to be used in the ACO search
    max.iter = 20L, # Max iterations to be tolerated with no improvements
    esem_every = 10L, # ESEM search checkpoint interval
    run_esem_during_search = TRUE,
    esem_weight = 0.50,
    use_parallel = TRUE,
    n.cores = 6L
  )
)

# Selected item IDs and their intended factor assignment.
print(result$best_items)
print(result$factor_assignment)

# Selected item text and standardized metadata.
print(result$selected_item_metadata)

# Main fit and diagnostic summaries returned by the full pipeline.
print(result$fit_indices[c("cfi", "rmsea", "srmr", "htmt_max")])
print(result$semantic_score)
print(result$summary)

# Because plot generation defaults to TRUE, diagnostic plots are stored here.
names(result$plots)
