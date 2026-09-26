# Better-input HEXACO diagnostic run for the current SEMANTICA development version.
#
# Goal:
# - test whether the weak speed-test outcome was input/search-budget limited
#   rather than a package-level fault
# - strengthen the generation prompt so each candidate is facet-specific,
#   behaviorally concrete, and contrasted against neighboring HEXACO domains
# - keep the run much faster than the "best outcome" script by avoiding the
#   slowest optional final diagnostics by default
#
# Run from the SEMANTICA repository root:
#   Rscript examples/hexaco-fast-quality-run.R
#
# Useful environment knobs:
#   SEMANTICA_UNSLOTH_BASE_URL       default http://localhost:8888
#   SEMANTICA_CHAT_MODEL             default local-model
#   SEMANTICA_EMBED_MODEL            default nomic-embed-text
#   NOMIC_EMBED_DIM                  default 384
#   SEMANTICA_FAST_WORKERS           default min(6, physical cores - 1)
#   SEMANTICA_FAST_POOL              default 42 items/factor
#   SEMANTICA_FAST_SELECTED          default 6 items/factor
#   SEMANTICA_FAST_FINAL_DIAGNOSTICS default false; if true runs DDDFI by default
#   SEMANTICA_FAST_SEMANTIC_N        default false
#   SEMANTICA_FAST_VALIDATION_N      default false; if true uses adaptive validation-N
#   SEMANTICA_FAST_PLOT_LEVEL        default summary; use none for the fastest wall time

`%||%` <- function(x, y) if (is.null(x)) y else x

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

number_env <- function(name, default, min = -Inf, max = Inf) {
  value <- suppressWarnings(as.numeric(env_or_default(name, as.character(default))))
  if (!is.finite(value) || value < min || value > max) {
    stop(name, " must be a finite number in [", min, ", ", max, "].", call. = FALSE)
  }
  value
}

flag_env <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(name, unset = "")))
  if (!nzchar(value)) return(isTRUE(default))
  if (value %in% c("1", "true", "t", "yes", "y", "on")) return(TRUE)
  if (value %in% c("0", "false", "f", "no", "n", "off")) return(FALSE)
  stop(name, " must be true/false, yes/no, or 1/0.", call. = FALSE)
}

strip_v1 <- function(url) {
  sub("/v1/?$", "", sub("/+$", "", url))
}

detected_cores <- function() {
  physical <- suppressWarnings(tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_))
  logical <- suppressWarnings(tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_))
  cores <- physical
  if (!is.finite(cores) || cores < 1L) cores <- logical
  if (!is.finite(cores) || cores < 1L) cores <- 1L
  as.integer(cores)
}

safe_print_timing <- function(x, title) {
  if (is.null(x) || !length(x)) return(invisible(FALSE))
  cat("\n", title, "\n", sep = "")
  print(x)
  invisible(TRUE)
}

# Keep PSOCK workers from multiplying BLAS/OpenMP threads.
Sys.setenv(
  OMP_NUM_THREADS = env_or_default("OMP_NUM_THREADS", "1"),
  OPENBLAS_NUM_THREADS = env_or_default("OPENBLAS_NUM_THREADS", "1"),
  MKL_NUM_THREADS = env_or_default("MKL_NUM_THREADS", "1"),
  VECLIB_MAXIMUM_THREADS = env_or_default("VECLIB_MAXIMUM_THREADS", "1"),
  NUMEXPR_NUM_THREADS = env_or_default("NUMEXPR_NUM_THREADS", "1")
)

set.seed(20260920L)

if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(SEMANTICA)
}

# -------------------------------------------------------------------------
# Local OpenAI-compatible server configuration
# -------------------------------------------------------------------------

UNSLOTH_ORIGIN <- strip_v1(env_or_default(
  "SEMANTICA_UNSLOTH_BASE_URL",
  env_or_default("UNSLOTH_BASE_URL", "http://localhost:8888")
))
CHAT_MODEL_ID <- env_or_default("SEMANTICA_CHAT_MODEL", "local-model")
EMBED_MODEL_ID <- env_or_default("SEMANTICA_EMBED_MODEL", "nomic-embed-text")
NOMIC_EMBED_DIM <- positive_int_env("NOMIC_EMBED_DIM", 384L)

nomic_backend <- semantica_backend_spec(
  protocol = "openai_compat",
  label = "Local Nomic embeddings via OpenAI-compatible endpoint",
  chat_url = NULL,
  embed_url = paste0(UNSLOTH_ORIGIN, "/v1/embeddings"),
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

diagnostic_input_rules <- paste(
  "Diagnostic input design for this run:",
  "Use one primary behavioral cue per item and keep the cue specific to the requested facet.",
  "Write items that a respondent could endorse from ordinary daily behavior, not from idealized self-evaluation.",
  "Avoid broad virtue, competence, positivity, or maturity statements that could fit several HEXACO domains.",
  "Do not mention HEXACO factor names, facet names, or psychometric terminology inside the item text.",
  "Prefer 7 to 14 words unless a slightly longer item is needed for clarity.",
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

hexaco_generation_contrasts <- list(
  Honesty_Humility = paste(
    "Contrast Honesty-Humility against Agreeableness and Conscientiousness.",
    "Items should be about sincerity, fairness, modesty, or low status/luxury interest,",
    "not simply kindness, patience, duty, or effort."
  ),
  Emotionality = paste(
    "Contrast Emotionality against Agreeableness and Extraversion.",
    "Items should be about fear, worry, reassurance seeking, attachment, or sentiment,",
    "not social warmth, low conflict, or general positivity."
  ),
  Extraversion = paste(
    "Contrast Extraversion against Honesty-Humility and Openness.",
    "Items should be about social confidence, sociability, energy, or liveliness,",
    "not status seeking, intellectual curiosity, or unusual ideas."
  ),
  Agreeableness = paste(
    "Contrast Agreeableness against Emotionality and Honesty-Humility.",
    "Items should be about forgiving, gentle, flexible, or patient responses to conflict,",
    "not emotional dependence, compassion alone, modesty, or fairness."
  ),
  Conscientiousness = paste(
    "Contrast Conscientiousness against Honesty-Humility and Emotionality.",
    "Items should be about planning, persistence, checking, order, or prudence,",
    "not moral fairness, anxiety, or fear of mistakes."
  ),
  Openness_to_Experience = paste(
    "Contrast Openness against Extraversion and Conscientiousness.",
    "Items should be about aesthetic attention, curiosity, creativity, or unusual perspectives,",
    "not social enthusiasm, work effort, or rule following."
  )
)

strengthen_hexaco_generation_inputs <- function(factors) {
  shared_forbidden <- c(
    "being a good person",
    "doing my best",
    "being mature",
    "having strong values",
    "being positive",
    "being responsible",
    "people like me",
    "success in life",
    "personal growth",
    "mental health symptoms",
    "clinical anxiety",
    "generic kindness",
    "generic confidence",
    "generic creativity",
    "generic organization"
  )

  facet_rule <- paste(
    "Make the item primarily diagnostic of this specific facet.",
    "Avoid wording that mainly signals a different HEXACO factor or a neighboring facet.",
    "Use concrete everyday behavior rather than broad self-description."
  )

  for (dimension in names(factors)) {
    spec <- factors[[dimension]]
    spec$forbidden <- unique(c(spec$forbidden %||% character(0L), shared_forbidden))
    spec$extra_instructions <- paste(
      c(
        spec$extra_instructions %||% character(0L),
        diagnostic_input_rules,
        hexaco_generation_contrasts[[dimension]] %||% character(0L)
      ),
      collapse = "\n"
    )

    if (is.list(spec$facets)) {
      for (facet in names(spec$facets)) {
        facet_spec <- spec$facets[[facet]]
        if (!is.list(facet_spec)) {
          facet_spec <- list(description = as.character(facet_spec))
        }
        facet_spec$extra_instructions <- paste(
          c(facet_spec$extra_instructions %||% character(0L), facet_rule),
          collapse = "\n"
        )
        spec$facets[[facet]] <- facet_spec
      }
    }

    factors[[dimension]] <- spec
  }

  factors
}

hexaco_factors <- strengthen_hexaco_generation_inputs(hexaco_factors)

# -------------------------------------------------------------------------
# Better-input diagnostic run controls
# -------------------------------------------------------------------------

physical_cores <- detected_cores()
default_workers <- min(6L, max(2L, physical_cores - 1L))
workers <- positive_int_env("SEMANTICA_FAST_WORKERS", default_workers)

pool_per_factor <- positive_int_env("SEMANTICA_FAST_POOL", 42L)
selected_per_factor <- positive_int_env("SEMANTICA_FAST_SELECTED", 6L)

run_final_diagnostics <- flag_env("SEMANTICA_FAST_FINAL_DIAGNOSTICS", FALSE)
run_semantic_n <- flag_env("SEMANTICA_FAST_SEMANTIC_N", FALSE)
run_validation_n <- flag_env("SEMANTICA_FAST_VALIDATION_N", FALSE)
plot_level <- env_or_default("SEMANTICA_FAST_PLOT_LEVEL", "summary")
if (!plot_level %in% c("none", "summary", "full")) {
  stop("SEMANTICA_FAST_PLOT_LEVEL must be one of: none, summary, full.", call. = FALSE)
}

final_fit_mode <- if (run_final_diagnostics) {
  env_or_default("SEMANTICA_FAST_FINAL_FIT", "dddfi")
} else {
  "off"
}
if (!final_fit_mode %in% c("off", "dddfi", "extended")) {
  stop("SEMANTICA_FAST_FINAL_FIT must be one of: off, dddfi, extended.", call. = FALSE)
}

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path("semantica_runs", paste0("hexaco_input_diagnostic_", run_id))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nSEMANTICA HEXACO better-input diagnostic run\n")
cat("Output dir      :", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
cat("Server          :", UNSLOTH_ORIGIN, "\n")
cat("Chat model      :", CHAT_MODEL_ID, "\n")
cat("Embed model     :", EMBED_MODEL_ID, "(", NOMIC_EMBED_DIM, "dim )\n")
cat("Workers         :", workers, "\n")
cat("Items/factor    :", pool_per_factor, "pool,", selected_per_factor, "selected\n")
cat("Diagnostic aim  : test input/search-budget limits before blaming package internals\n")
cat("Final diagnostics:", final_fit_mode,
    "| semantic-N:", run_semantic_n,
    "| validation-N:", run_validation_n,
    "| plots:", plot_level, "\n\n")

diagnostic_design <- list(
  purpose = "Test whether the previous fast HEXACO result was limited by input quality and search budget.",
  interpretation_rules = c(
    "If PFA clarity, loading quality, ESEM structure, and reference tiles improve materially, the earlier outcome was probably input/search-budget limited.",
    "If the richer pool and larger bounded search still fail in the same pattern, investigate package-level objectives, embeddings, and structure proxies next.",
    "Keep final diagnostics off for this diagnostic unless the selected structure is good enough to justify slower validation."
  ),
  changed_defaults = list(
    pool_per_factor = pool_per_factor,
    selected_per_factor = selected_per_factor,
    overgenerate = number_env("SEMANTICA_FAST_OVERGENERATE", 2.30, min = 1),
    temperature = number_env("SEMANTICA_FAST_TEMPERATURE", 0.52, min = 0),
    pfa_every = 1L,
    esem_every = 3L,
    ants = positive_int_env("SEMANTICA_FAST_ANTS", 90L),
    max_total_iter = positive_int_env("SEMANTICA_FAST_MAX_ITER", 40L),
    max_esem_fits = positive_int_env("SEMANTICA_FAST_MAX_ESEM_FITS", 160L)
  ),
  factor_contrasts = hexaco_generation_contrasts
)
saveRDS(diagnostic_design, file.path(output_dir, "input_diagnostic_design.rds"))
writeLines(
  c(
    "SEMANTICA HEXACO better-input diagnostic design",
    "",
    paste("Purpose:", diagnostic_design$purpose),
    "",
    "Interpretation rules:",
    paste("-", diagnostic_design$interpretation_rules),
    "",
    "Changed defaults:",
    paste("- pool_per_factor:", diagnostic_design$changed_defaults$pool_per_factor),
    paste("- selected_per_factor:", diagnostic_design$changed_defaults$selected_per_factor),
    paste("- overgenerate:", diagnostic_design$changed_defaults$overgenerate),
    paste("- temperature:", diagnostic_design$changed_defaults$temperature),
    paste("- pfa_every:", diagnostic_design$changed_defaults$pfa_every),
    paste("- esem_every:", diagnostic_design$changed_defaults$esem_every),
    paste("- ants:", diagnostic_design$changed_defaults$ants),
    paste("- max_total_iter:", diagnostic_design$changed_defaults$max_total_iter),
    paste("- max_esem_fits:", diagnostic_design$changed_defaults$max_esem_fits)
  ),
  file.path(output_dir, "input_diagnostic_design.txt")
)

started_at <- Sys.time()

result_hexaco_fast_quality <- semantica_run_custom(
  scale_name = "HEXACO High-Pole Better-Input Diagnostic Run",
  scale_description = paste(
    "High-pole HEXACO personality item screening with strengthened generation inputs and bounded ESEM/PFA-guided selection.",
    general_hexaco_rules,
    diagnostic_input_rules,
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
    embed_batch_size = positive_int_env("SEMANTICA_FAST_EMBED_BATCH", 64L),
    embedding_cache = TRUE,
    embedding_cache_dir = "semantica_embedding_cache",
    embedding_cache_namespace = paste("hexaco-input-diagnostic", EMBED_MODEL_ID, NOMIC_EMBED_DIM, sep = "_"),
    retry_max_tries = positive_int_env("SEMANTICA_FAST_RETRIES", 3L),
    retry_on_failure = TRUE,
    timeout_s = positive_int_env("SEMANTICA_FAST_TIMEOUT", 180L),
    embed_timeout_s = positive_int_env("SEMANTICA_FAST_EMBED_TIMEOUT", 180L),
    preflight = flag_env("SEMANTICA_FAST_PREFLIGHT", FALSE)
  ),

  chat_model = CHAT_MODEL_ID,
  embed_model = EMBED_MODEL_ID,

  item_counts = semantica_item_count_config(
    pool = pool_per_factor,
    selected = selected_per_factor,
    override_pool_counts = TRUE
  ),

  generation = semantica_generation_config(
    language = "English",
    response_format = "5-point Likert agreement scale",
    item_style = "first-person present-tense personality statement",
    overgenerate = number_env("SEMANTICA_FAST_OVERGENERATE", 2.30, min = 1),
    max_retries = positive_int_env("SEMANTICA_FAST_GENERATION_RETRIES", 4L),
    global_forbidden_max = 90L,
    temperature = number_env("SEMANTICA_FAST_TEMPERATURE", 0.52, min = 0),
    structured_output = "numbered"
  ),

  resources = semantica_resource_config(
    cpu_cores = workers,
    reserve_cpu_cores = 1L,
    max_cpu_cores = workers,
    memory_aware = flag_env("SEMANTICA_FAST_MEMORY_AWARE", TRUE)
  ),

  compute = semantica_compute_config(
    cosine_adjustment = "mean_center",
    compute_cosine_sensitivity = flag_env("SEMANTICA_FAST_COSINE_SENSITIVITY", FALSE),
    cosine_sensitivity_max_items = 800L,
    cosine_sensitivity_seed = 20260920L,
    compute_device = env_or_default("SEMANTICA_FAST_COMPUTE_DEVICE", "cpu"),
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
    facet_coverage_weight = 0.30,
    psychometric_guard_weight = 0.90,
    psychometric_guard_min_ave = 0.25,
    psychometric_guard_min_loading = 0.45,
    psychometric_guard_min_primary_ge_50 = 0.75,
    psychometric_guard_min_simple_structure = 0.75,
    psychometric_guard_min_dominance = 0.86,
    psychometric_guard_max_cross_loading = 0.30,
    psychometric_guard_include_htmt = TRUE,
    final_structure_repair = TRUE,
    final_structure_repair_max_swaps = 8L,
    final_structure_repair_candidates_per_factor = 8L,
    final_structure_repair_min_delta = 1e-6,
    threshold_mode = "fixed",
    polarity_screen = TRUE
  ),

  pfa = semantica_pfa_config(
    mode = "objective",
    weight = 0.35,
    failure_policy = "semantic_fallback",
    during_search = TRUE,
    every = 1L,
    extraction = "principal",
    final_extraction = "ml",
    rotation = "oblimin",
    min_loading = 0.42,
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
    esem_weight = 0.64,
    esem_failure_policy = "semantic_fallback",
    fast_esem = TRUE,
    fast_esem_iter_max = 750L,
    full_esem_iter_max = 2400L,
    esem_eval_top_k = 6L
  ),

  fit_calibration = semantica_fit_calibration_config(
    mode = "esem_parametric_dfi",
    reps = positive_int_env("SEMANTICA_FAST_DFI_REPS", 260L),
    esem_reps = positive_int_env("SEMANTICA_FAST_DFI_ESEM_REPS", 220L),
    search_reps = positive_int_env("SEMANTICA_FAST_DFI_SEARCH_REPS", 120L),
    final_recalibrate = FALSE,
    strategy = "adaptive",
    adaptive_min_reps = positive_int_env("SEMANTICA_FAST_DFI_MIN_REPS", 100L),
    adaptive_batch_reps = positive_int_env("SEMANTICA_FAST_DFI_BATCH_REPS", 50L),
    adaptive_tol = 0.0025,
    adaptive_stable_batches = 2L,
    fallback_policy = "conservative",
    data_type = "continuous",
    target_loadings = 0.65,
    target_factor_cors = 0.25,
    loading_pattern = "varied",
    embed_reliability = 0.90,
    residual_inflation = 0.02,
    warmup_iters = 4L
  ),

  diagnostics = semantica_diagnostics_config(
    final_fit = final_fit_mode,
    final_dddfi_reps = positive_int_env("SEMANTICA_FAST_FINAL_DDDFI_REPS", 80L),
    final_dddfi_mad_target = "close",
    semantic_stability = run_semantic_n,
    reference_rmsea_close = 0.05,
    reference_rmsea_poor = 0.06,
    reference_power = 0.80,
    reference_alpha = 0.05,
    reference_max_n = 3000L,
    semantic_n_multipliers = c(0.75, 1, 1.25),
    semantic_n_iter_max = 550L,
    validation_planning = run_validation_n,
    validation_n_reps = positive_int_env("SEMANTICA_FAST_VALIDATION_REPS", 12L),
    validation_n_grid = seq(300L, 1500L, by = 300L),
    validation_n_max = 1800L,
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
    level = plot_level,
    interactive_mode = "2d",
    save = FALSE,
    out_dir = file.path(output_dir, "plots"),
    device = "png",
    width = 13,
    height = 8,
    dpi = 180,
    before_path_model = "proxy",
    before_path_refit_max_items = 45L,
    network_max_items = min(220L, pool_per_factor * length(hexaco_factors)),
    mds_max_items = min(220L, pool_per_factor * length(hexaco_factors)),
    path_proxy_max_items = 120L,
    include_interactive = FALSE,
    progress = TRUE
  ),

  ants = positive_int_env("SEMANTICA_FAST_ANTS", 90L),
  search_patience = positive_int_env("SEMANTICA_FAST_PATIENCE", 22L),
  max_total_iter = positive_int_env("SEMANTICA_FAST_MAX_ITER", 40L),
  max_esem_fits = positive_int_env("SEMANTICA_FAST_MAX_ESEM_FITS", 160L),
  elite_k = positive_int_env("SEMANTICA_FAST_ELITE_K", 18L),
  archive_stable_window = 8L,
  structural_archive_stable_window = 3L,
  min_successful_pfa_checkpoints = 3L,
  min_successful_esem_checkpoints = 3L,
  seed = 20260920L,
  verbose = TRUE
)

finished_at <- Sys.time()
elapsed_seconds <- as.numeric(difftime(finished_at, started_at, units = "secs"))

bundle_path <- file.path(output_dir, "hexaco_input_diagnostic_bundle.rds")
semantica_save_bundle(
  result_hexaco_fast_quality,
  path = bundle_path,
  include_embeddings = TRUE,
  write_manifest = TRUE
)

semantica_export(
  result_hexaco_fast_quality,
  prefix = file.path(output_dir, "hexaco_input_diagnostic"),
  include_candidates = TRUE
)

capture.output(
  str(
    list(
      performance = result_hexaco_fast_quality$performance,
      evaluation_telemetry = result_hexaco_fast_quality$evaluation_telemetry,
      resource_plan = result_hexaco_fast_quality$resource_plan,
      generation_provenance = result_hexaco_fast_quality$generation_provenance,
      pool_health = result_hexaco_fast_quality$pool_health,
      content_alignment = result_hexaco_fast_quality$content_alignment,
      construct_coverage_pool = result_hexaco_fast_quality$construct_coverage_pool,
      selection_guard_audit = result_hexaco_fast_quality$selection_guard_audit,
      semantic_cluster_consensus = result_hexaco_fast_quality$semantic_cluster_consensus,
      fit_indices = result_hexaco_fast_quality$fit_indices,
      pfa_diagnostics = result_hexaco_fast_quality$pfa_diagnostics,
      final_structure_repair = result_hexaco_fast_quality$final_structure_repair
    ),
    max.level = 4L
  ),
  file = file.path(output_dir, "diagnostic_details.txt")
)
capture.output(sessionInfo(), file = file.path(output_dir, "session_info.txt"))

cat("\nRun complete\n")
cat("Elapsed minutes :", sprintf("%.2f", elapsed_seconds / 60), "\n")
cat("Selected items  :", length(result_hexaco_fast_quality$best_items %||% character()), "\n")
cat("Bundle          :", normalizePath(bundle_path, winslash = "/", mustWork = FALSE), "\n")
cat("Output dir      :", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
cat("Design notes    :", normalizePath(file.path(output_dir, "input_diagnostic_design.txt"),
                                      winslash = "/", mustWork = FALSE), "\n")
cat("\nInterpretation cue:\n")
cat("- Better PFA/ESEM/reference-tile pattern than the speed test supports an input/search-budget explanation.\n")
cat("- The same weak pattern despite this stronger pool points back to package objectives, embeddings, or proxy scoring.\n")

safe_print_timing(
  list(
    total_seconds = result_hexaco_fast_quality$performance$total_seconds,
    plotting_seconds = result_hexaco_fast_quality$performance$plotting_seconds
  ),
  "Full-pipeline timing"
)
optimizer_timing <- result_hexaco_fast_quality$performance$optimization$timing %||%
  result_hexaco_fast_quality$performance$timing
safe_print_timing(optimizer_timing, "Optimization timing")
safe_print_timing(
  optimizer_timing$finalization_breakdown_seconds,
  "Finalization timing breakdown"
)
safe_print_timing(
  result_hexaco_fast_quality$performance$plotting_breakdown_seconds,
  "Plot timing breakdown"
)

cat("\nEvidence status\n")
print(semantica_evidence_status(result_hexaco_fast_quality, labels = "both"))
