# High-quality HEXACO run for the latest SEMANTICA development version.
#
# This example prioritizes output quality over runtime:
# - larger balanced facet pool
# - ESEM-guided ACO search
# - structure-weighted ESEM scoring
# - PFA objective with boundary-loading penalty
# - strict structural guardrails
# - final same-factor structure repair
# - semantic-N stability and validation-planning diagnostics

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(value))) default else value
}

env_or_null <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(value))) NULL else value
}

positive_int_env <- function(name, default) {
  value <- suppressWarnings(as.integer(env_or_default(name, as.character(default))))
  if (!is.finite(value) || value < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  value
}

strip_v1 <- function(url) {
  sub("/v1/?$", "", sub("/+$", "", url))
}

set.seed(20260216L)

# Prefer the local checkout when this script is run from the SEMANTICA repo.
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(SEMANTICA)
}

# -------------------------------------------------------------------------
# Local OpenAI-compatible server configuration
# -------------------------------------------------------------------------
# Set these environment variables if your local server/model ids differ:
#   SEMANTICA_UNSLOTH_BASE_URL, SEMANTICA_CHAT_MODEL, SEMANTICA_EMBED_MODEL,
#   UNSLOTH_API_KEY, NOMIC_EMBED_DIM
#
# SEMANTICA_UNSLOTH_BASE_URL may be either "http://host:port" or
# "http://host:port/v1"; this script normalizes it for SEMANTICA.

UNSLOTH_ORIGIN <- strip_v1(env_or_default(
  "SEMANTICA_UNSLOTH_BASE_URL",
  env_or_default("UNSLOTH_BASE_URL", "http://localhost:8888")
))
UNSLOTH_API_BASE <- paste0(UNSLOTH_ORIGIN, "/v1")
UNSLOTH_CHAT_URL <- paste0(UNSLOTH_API_BASE, "/chat/completions")
UNSLOTH_EMBED_URL <- paste0(UNSLOTH_API_BASE, "/embeddings")

CHAT_MODEL_ID <- env_or_default("SEMANTICA_CHAT_MODEL", "local-model")
EMBED_MODEL_ID <- env_or_default("SEMANTICA_EMBED_MODEL", "nomic-embed-text")
NOMIC_EMBED_DIM <- positive_int_env("NOMIC_EMBED_DIM", 384L)

nomic_backend <- semantica_backend_spec(
  protocol = "openai_compat",
  label = "Local Nomic embeddings via OpenAI-compatible endpoint",
  chat_url = NULL,
  embed_url = UNSLOTH_EMBED_URL,
  can_chat = FALSE,
  can_embed = TRUE,
  supports_structured_output = FALSE,
  supports_batch_embeddings = TRUE,
  default_embed_model = EMBED_MODEL_ID,
  embed_dim = NOMIC_EMBED_DIM,
  auth_header = "Bearer",
  auth_env = "UNSLOTH_API_KEY"
)

nomic_embedding_contract <- semantica_embedding_spec(
  instruction_mode = "prefix",
  task_map = c(
    psychometric_similarity = "clustering",
    clustering = "clustering",
    classification = "classification",
    search_document = "search_document",
    search_query = "search_query"
  ),
  prefix_template = "{task}: ",
  requires_instruction = TRUE,
  note = "Nomic Embed Text: use the documented clustering prefix for symmetric psychometric similarity."
)

# -------------------------------------------------------------------------
# HEXACO construct specification
# -------------------------------------------------------------------------

general_hexaco_rules <- paste(
  "Generate high-pole HEXACO personality items.",
  "Every item must be a short first-person present-tense statement.",
  "Avoid double-barreled wording, trait labels, negations, sarcasm, idioms, clinical symptoms, and moralizing language.",
  "Prefer observable everyday tendencies over abstract self-praise.",
  "Keep each item primarily about its assigned factor and facet; do not blend HEXACO domains.",
  "Do not write reverse-keyed items in this run.",
  sep = "\n"
)

hexaco_factors <- list(
  Honesty_Humility = list(
    description = paste(
      "High Honesty-Humility reflects sincerity, fairness, modesty, and low interest in exploiting others for personal gain.",
      "Items should describe truthful, fair, unpretentious behavior without sounding saintly."
    ),
    forbidden = c("general sociability", "work organization", "fearfulness", "creativity"),
    facets = list(
      Sincerity = list(description = "Straightforward, genuine, and not manipulative in relationships."),
      Fairness = list(description = "Avoids cheating, taking advantage, or bending rules for personal benefit."),
      Greed_Avoidance = list(description = "Low attraction to luxury, status symbols, and special privilege."),
      Modesty = list(description = "Does not seek superiority, entitlement, or special admiration.")
    )
  ),
  Emotionality = list(
    description = paste(
      "High Emotionality reflects fearfulness, anxiety, dependence on close others, and sentimentality.",
      "Items should capture emotional sensitivity and attachment without clinical or disorder-like wording."
    ),
    forbidden = c("anger", "forgiveness", "social boldness", "organization"),
    facets = list(
      Fearfulness = list(description = "Cautious and easily concerned about physical danger."),
      Anxiety = list(description = "Tends to worry about uncertain or stressful outcomes."),
      Dependence = list(description = "Seeks support and reassurance from trusted close others."),
      Sentimentality = list(description = "Feels strong emotional bonds and compassion toward others.")
    )
  ),
  Extraversion = list(
    description = paste(
      "High Extraversion reflects social self-esteem, social boldness, sociability, and liveliness.",
      "Items should describe energetic social engagement without implying dominance or low humility."
    ),
    forbidden = c("status seeking", "rule following", "emotional dependence", "intellectual curiosity"),
    facets = list(
      Social_Self_Esteem = list(description = "Feels comfortable and worthwhile in social settings."),
      Social_Boldness = list(description = "Approaches social situations with confidence."),
      Sociability = list(description = "Enjoys conversation, company, and group interaction."),
      Liveliness = list(description = "Shows enthusiasm, energy, and positive activation.")
    )
  ),
  Agreeableness = list(
    description = paste(
      "High Agreeableness reflects forgiveness, gentleness, flexibility, and patience.",
      "Items should capture tolerance and cooperative conflict handling without describing passivity."
    ),
    forbidden = c("sentimentality", "social enthusiasm", "modesty", "perfectionism"),
    facets = list(
      Forgiveness = list(description = "Lets go of resentment after interpersonal conflict."),
      Gentleness = list(description = "Responds softly and avoids harsh confrontation."),
      Flexibility = list(description = "Compromises and adapts when others disagree."),
      Patience = list(description = "Stays calm with delays, mistakes, or irritation.")
    )
  ),
  Conscientiousness = list(
    description = paste(
      "High Conscientiousness reflects organization, diligence, carefulness, and prudence.",
      "Items should describe reliable self-regulation without sounding obsessive or moralistic."
    ),
    forbidden = c("fairness", "anxiety", "social confidence", "aesthetic appreciation"),
    facets = list(
      Organization = list(description = "Keeps tasks, spaces, and plans orderly."),
      Diligence = list(description = "Works persistently and follows through on responsibilities."),
      Perfectionism = list(description = "Checks details and aims for careful, high-quality work."),
      Prudence = list(description = "Thinks before acting and avoids careless decisions.")
    )
  ),
  Openness_to_Experience = list(
    description = paste(
      "High Openness to Experience reflects aesthetic appreciation, inquisitiveness, creativity, and unconventionality.",
      "Items should describe curiosity and imagination without drifting into general intelligence or sociability."
    ),
    forbidden = c("liveliness", "rule compliance", "emotional attachment", "status avoidance"),
    facets = list(
      Aesthetic_Appreciation = list(description = "Notices and values art, beauty, design, or expressive detail."),
      Inquisitiveness = list(description = "Enjoys learning, exploring ideas, and understanding how things work."),
      Creativity = list(description = "Generates original ideas, possibilities, or expressive solutions."),
      Unconventionality = list(description = "Is comfortable with unusual perspectives and nonstandard ideas.")
    )
  )
)

# -------------------------------------------------------------------------
# High-quality SEMANTICA run
# -------------------------------------------------------------------------

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path("semantica_runs", paste0("hexaco_best_", run_id))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

result_hexaco_best <- semantica_run_custom(
  scale_name = "HEXACO High-Pole Best-Outcome Run",
  scale_description = paste(
    "High-pole HEXACO personality item screening with strict semantic, PFA, and ESEM structure diagnostics.",
    general_hexaco_rules,
    sep = "\n\n"
  ),
  factors = hexaco_factors,

  llm = semantica_llm_config(
    backend = "unsloth",
    embed_backend = "local_nomic_unsloth",
    api_key = env_or_null("UNSLOTH_API_KEY"),
    embed_api_key = env_or_null("UNSLOTH_API_KEY"),
    base_url = UNSLOTH_ORIGIN,
    embed_base_url = UNSLOTH_ORIGIN,
    embed_backend_spec = nomic_backend,
    embedding_spec = nomic_embedding_contract,
    embedding_task = "psychometric_similarity",
    embed_batch_size = 32L,
    embedding_cache = TRUE,
    embedding_cache_dir = "semantica_embedding_cache",
    embedding_cache_namespace = paste("hexaco-best", EMBED_MODEL_ID, NOMIC_EMBED_DIM, sep = "_"),
    retry_max_tries = 5L,
    retry_on_failure = TRUE,
    timeout_s = 240L,
    embed_timeout_s = 240L,
    preflight = FALSE
  ),

  chat_model = CHAT_MODEL_ID,
  embed_model = EMBED_MODEL_ID,

  item_counts = semantica_item_count_config(
    pool = 36L,
    selected = 6L,
    override_pool_counts = TRUE
  ),

  generation = semantica_generation_config(
    language = "English",
    response_format = "5-point Likert agreement scale",
    item_style = "first-person present-tense personality statement",
    overgenerate = 2.5,
    max_retries = 4L,
    global_forbidden_max = 80L,
    temperature = 0.65,
    structured_output = "numbered"
  ),

  resources = semantica_resource_config(
    cpu_cores = 4L,
    reserve_cpu_cores = 1L,
    max_cpu_cores = 6L
  ),

  compute = semantica_compute_config(
    cosine_adjustment = "mean_center",
    compute_cosine_sensitivity = TRUE,
    cosine_sensitivity_max_items = 1500L,
    cosine_sensitivity_seed = 20260216L,
    compute_device = "cpu",
    gpu_precision = "double",
    retain_embeddings = TRUE
  ),

  quality = semantica_quality_config(
    profile = "strict",
    content_alignment_mode = "guard",
    polarity_action = "guard",
    semantic_objective_mode = "relative_conservative",
    within_target_method = "nonredundant_median",
    htmt_objective_role = "penalty",
    htmt_threshold = 0.85,
    facet_coverage_weight = 0.25,
    psychometric_guard_weight = 0.85,
    psychometric_guard_min_ave = 0.25,
    psychometric_guard_min_loading = 0.45,
    psychometric_guard_min_primary_ge_50 = 0.75,
    psychometric_guard_min_simple_structure = 0.75,
    psychometric_guard_min_dominance = 0.85,
    psychometric_guard_max_cross_loading = 0.30,
    psychometric_guard_include_htmt = TRUE,
    final_structure_repair = TRUE,
    final_structure_repair_max_swaps = 10L,
    final_structure_repair_candidates_per_factor = 10L,
    final_structure_repair_min_delta = 1e-6,
    threshold_mode = "fixed",
    polarity_screen = TRUE
  ),

  pfa = semantica_pfa_config(
    mode = "objective",
    weight = 0.30,
    failure_policy = "semantic_fallback",
    during_search = TRUE,
    every = 2L,
    extraction = "principal",
    final_extraction = "ml",
    rotation = "oblimin",
    min_loading = 0.40,
    min_margin = 0.12,
    max_abs_loading = 0.95,
    unit_diagnostics = TRUE
  ),

  esem = semantica_esem_config(
    proxy_reference_n = "auto",
    rotation = "geomin",
    rotation_args = list(geomin.epsilon = 0.50),
    score_mode = "structure_weighted",
    cadence_mode = "adaptive",
    esem_every = 3L,
    run_esem_during_search = TRUE,
    esem_weight = 0.65,
    esem_failure_policy = "semantic_fallback",
    fast_esem = TRUE,
    fast_esem_iter_max = 900L,
    full_esem_iter_max = 3000L,
    esem_eval_top_k = 8L
  ),

  fit_calibration = semantica_fit_calibration_config(
    mode = "auto",
    reps = 600L,
    esem_reps = 300L,
    search_reps = 150L,
    final_recalibrate = TRUE,
    final_reps = 400L,
    strategy = "adaptive",
    adaptive_min_reps = 150L,
    adaptive_batch_reps = 50L,
    adaptive_tol = 0.002,
    adaptive_stable_batches = 2L,
    fallback_policy = "conservative",
    data_type = "continuous",
    target_loadings = 0.65,
    target_factor_cors = 0.25,
    loading_pattern = "varied",
    embed_reliability = 0.90,
    residual_inflation = 0.02,
    warmup_iters = 5L
  ),

  diagnostics = semantica_diagnostics_config(
    final_fit = "extended",
    semantic_stability = TRUE,
    reference_rmsea_close = 0.05,
    reference_rmsea_poor = 0.06,
    reference_power = 0.80,
    reference_alpha = 0.05,
    reference_max_n = 5000L,
    semantic_n_multipliers = c(0.5, 0.75, 1, 1.25, 1.5, 2),
    semantic_n_iter_max = 1200L,
    validation_planning = TRUE,
    validation_n_reps = 30L,
    validation_n_grid = seq(300L, 1800L, by = 150L),
    validation_n_max = 2000L,
    validation_n_convergence = 0.90,
    validation_n_max_heywood = 0.05,
    validation_n_min_recovery = 0.90,
    validation_n_max_loading_error = 0.10,
    validation_n_min_dominance = 0.85,
    validation_n_max_cross_error = 0.10,
    validation_n_max_factor_cor_error = 0.10,
    validation_planning_on_inadmissible = "skip"
  ),

  plots = semantica_plot_config(
    level = "full",
    interactive_mode = "2d",
    save = FALSE,
    out_dir = file.path(output_dir, "plots"),
    device = "png",
    width = 14,
    height = 9,
    dpi = 220,
    before_path_model = "proxy",
    include_interactive = FALSE,
    progress = TRUE
  ),

  ants = 120L,
  search_patience = 28L,
  max_total_iter = 45L,
  max_esem_fits = 220L,
  elite_k = 16L,
  archive_stable_window = 10L,
  structural_archive_stable_window = 3L,
  min_successful_pfa_checkpoints = 3L,
  min_successful_esem_checkpoints = 3L,
  seed = 20260216L,
  verbose = TRUE
)

report_semantica_v2(result_hexaco_best, digits = 3L)

bundle_path <- file.path(output_dir, "hexaco_best_semantica_bundle.rds")
semantica_save_bundle(result_hexaco_best, path = bundle_path)

saveRDS(
  list(
    result = result_hexaco_best,
    timing = result_hexaco_best$performance$timing,
    warnings = result_hexaco_best$warnings,
    output_dir = output_dir,
    bundle_path = bundle_path
  ),
  file.path(output_dir, "hexaco_best_result_snapshot.rds")
)

cat("\nSaved SEMANTICA run to:\n", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
cat("Bundle:\n", normalizePath(bundle_path, winslash = "/", mustWork = FALSE), "\n")

if (!is.null(result_hexaco_best$performance$timing$finalization_breakdown_seconds)) {
  cat("\nFinalization timing breakdown:\n")
  print(result_hexaco_best$performance$timing$finalization_breakdown_seconds)
}
