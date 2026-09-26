# Run from the SEMANTICA repository root. This script defines the scale objects,
# backend contract, and model IDs it uses, then executes ONE live pipeline run.
# Calibration and final diagnostics add model fits beyond the search-only
# max_esem_fits budget.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first to test the current working source.")
}
if (!file.exists("R/full_pipeline.R")) {
  stop("Set the working directory to the SEMANTICA repository root first.")
}
pkgload::load_all(".", quiet = TRUE)

# =============================================================================
# 0. Objects used by the pipeline
# =============================================================================
#
# Edit these environment variables or the defaults below to match the exact
# models exposed by your local Unsloth server:
#
#   UNSLOTH_API_KEY      optional for many local servers
#   UNSLOTH_BASE_URL     default: http://localhost:8888
#   CHAT_MODEL_ID        default: local-model
#   EMBED_MODEL_ID       default: nomic-embed-text
#   NOMIC_EMBED_DIM      default: 768
#
# The object names match the original example so the call below stays readable.

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

url_without_trailing_slash <- function(x) sub("/+$", "", x)

UNSLOTH_BASE_URL <- url_without_trailing_slash(
  env_or_default("UNSLOTH_BASE_URL", "http://localhost:8888")
)
CHAT_MODEL_ID <- env_or_default("CHAT_MODEL_ID", "local-model")
EMBED_MODEL_ID <- env_or_default("EMBED_MODEL_ID", "nomic-embed-text")
NOMIC_EMBED_DIM <- suppressWarnings(as.integer(
  env_or_default("NOMIC_EMBED_DIM", "768")
))
if (!is.finite(NOMIC_EMBED_DIM) || NOMIC_EMBED_DIM < 1L) {
  stop("NOMIC_EMBED_DIM must be a positive integer.")
}

nomic_backend <- semantica_backend_spec(
  protocol = "openai_compat",
  label = "Local Nomic embeddings through Unsloth/OpenAI-compatible server",
  embed_url = paste0(UNSLOTH_BASE_URL, "/v1/embeddings"),
  can_chat = FALSE,
  can_embed = TRUE,
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
  note = paste(
    "Nomic Embed Text uses task prefixes. SEMANTICA maps the symmetric",
    "psychometric-similarity intent to the clustering task."
  )
)

general_hexaco_rules <- paste(
  "Generate brief first-person present-tense statements about typical adult behavior.",
  "Write each item for the high pole of the target construct.",
  "Use concrete everyday language and avoid clinical, moralizing, or extreme wording.",
  "Avoid double-barrelled items, causal explanations, abstract trait labels, and rare scenarios.",
  "Avoid negations such as not, never, hardly, and do not unless needed for natural wording.",
  "Do not mention the dimension or facet name directly in the item text.",
  "Keep every item focused on one observable tendency.",
  sep = "\n"
)

make_facet <- function(description, examples, forbidden) {
  list(
    description = description,
    examples = examples,
    forbidden = forbidden
  )
}

make_hexaco_dimension <- function(description, focus, facets, forbidden) {
  list(
    description = description,
    forbidden = forbidden,
    extra_instructions = paste(general_hexaco_rules, focus, sep = "\n"),
    facets = facets
  )
}

hexaco_factors <- list(
  Honesty_Humility = make_hexaco_dimension(
    description = paste(
      "Fairness, sincerity, modest self-presentation, and low exploitation",
      "of others for personal gain."
    ),
    focus = paste(
      "Emphasize genuineness, fairness, modesty, and low attraction to",
      "status or luxury. Do not drift into general kindness or diligence."
    ),
    forbidden = c("general politeness", "fear of punishment", "work diligence"),
    facets = list(
      Honesty = make_facet(
        "Straightforward and genuine interpersonal behavior.",
        c("I speak plainly about my intentions.", "I deal with people directly."),
        c("general warmth", "talkativeness")
      ),
      Fairness = make_facet(
        "Avoidance of cheating, fraud, corruption, and exploitation.",
        c("I keep agreements even when shortcuts would benefit me."),
        c("obedience to authority", "perfectionism")
      ),
      Modesty = make_facet(
        "Low entitlement and unassuming self-presentation.",
        c("I avoid acting as though I deserve special treatment."),
        c("low confidence", "social anxiety")
      ),
      Greed_Avoidance = make_facet(
        "Limited attraction to wealth, luxury, and status symbols.",
        c("I place little personal value on symbols of high status."),
        c("poverty", "financial irresponsibility")
      )
    )
  ),
  Emotionality = make_hexaco_dimension(
    description = paste(
      "Emotional sensitivity, harm avoidance, attachment, and need for",
      "support under stress."
    ),
    focus = paste(
      "Emphasize threat sensitivity, worry under pressure, support seeking,",
      "and emotional bonds without making items clinical."
    ),
    forbidden = c("depression", "anger", "social popularity", "general kindness"),
    facets = list(
      Fearfulness = make_facet(
        "Sensitivity to physical danger and situational threat.",
        c("I become cautious when a situation could put me in danger."),
        c("general worry", "social embarrassment")
      ),
      Anxiety = make_facet(
        "Anticipatory tension about stressful events or uncertain outcomes.",
        c("I feel tense when an important outcome is uncertain."),
        c("fear of injury", "sadness")
      ),
      Dependence = make_facet(
        "Seeking reassurance and support during distress.",
        c("I turn to trusted people when I feel overwhelmed."),
        c("submissiveness", "low competence")
      ),
      Sentimentality = make_facet(
        "Tender attachment to close others and meaningful memories.",
        c("I feel deeply moved by close emotional bonds."),
        c("romantic jealousy", "need for attention")
      )
    )
  ),
  Extraversion = make_hexaco_dimension(
    description = paste(
      "Social confidence, social approach, enjoyment of company, and",
      "energetic positive affect."
    ),
    focus = paste(
      "Emphasize social self-regard, initiative, sociability, and liveliness.",
      "Avoid dominance, arrogance, and status seeking."
    ),
    forbidden = c("dominance", "manipulation", "attention seeking", "work ambition"),
    facets = list(
      Social_Self_Esteem = make_facet(
        "Positive evaluation of one's social worth.",
        c("I usually feel accepted when I am with other people."),
        c("superiority", "vanity")
      ),
      Social_Boldness = make_facet(
        "Confidence speaking up and entering social situations.",
        c("I feel comfortable starting conversations with new people."),
        c("aggression", "risk taking")
      ),
      Sociability = make_facet(
        "Preference for social contact and shared activity.",
        c("I seek out opportunities to be with others."),
        c("dependence", "social status")
      ),
      Liveliness = make_facet(
        "Energetic enthusiasm and expressive positive affect.",
        c("I bring energy into everyday activities."),
        c("impulsivity", "recklessness")
      )
    )
  ),
  Agreeableness = make_hexaco_dimension(
    description = paste(
      "Forgiveness, gentleness, flexibility, and patience in interpersonal",
      "conflict."
    ),
    focus = paste(
      "Focus on responses to disagreement, provocation, frustration, and",
      "compromise. Avoid honesty, charity, and emotional dependence."
    ),
    forbidden = c("honesty", "fairness", "sentimentality", "general altruism"),
    facets = list(
      Forgiveness = make_facet(
        "Letting go of resentment after interpersonal offenses.",
        c("I can move on after someone has offended me."),
        c("trusting everyone", "excusing exploitation")
      ),
      Gentleness = make_facet(
        "Mild and considerate behavior when annoyed or disagreeing.",
        c("I handle tense conversations without becoming harsh."),
        c("submissiveness", "shyness")
      ),
      Flexibility = make_facet(
        "Readiness to compromise and adjust one's position.",
        c("I can adjust my position when others make a reasonable point."),
        c("indecision", "carelessness")
      ),
      Patience = make_facet(
        "Control over irritation and hostile reactions.",
        c("I stay composed when small frustrations build up."),
        c("low standards", "avoidance")
      )
    )
  ),
  Conscientiousness = make_hexaco_dimension(
    description = paste(
      "Organization, diligence, accuracy, persistence, and deliberation in",
      "goal-directed behavior."
    ),
    focus = paste(
      "Emphasize planning, follow-through, careful work, and considered",
      "action. Avoid moral virtue, fear-based avoidance, or status ambition."
    ),
    forbidden = c("moral fairness", "fear of danger", "agreeableness", "creativity"),
    facets = list(
      Organization = make_facet(
        "Preference for order, structure, and planned task management.",
        c("I keep my tasks arranged in a clear order."),
        c("cleanliness obsession", "social control")
      ),
      Diligence = make_facet(
        "Sustained effort and responsibility in completing tasks.",
        c("I follow through on tasks even when they become demanding."),
        c("status ambition", "competitiveness")
      ),
      Perfectionism = make_facet(
        "Concern for accuracy, completeness, and quality standards.",
        c("I check important details before considering my work finished."),
        c("clinical perfectionism", "fear of failure")
      ),
      Prudence = make_facet(
        "Deliberation and impulse control before acting.",
        c("I think through consequences before making important choices."),
        c("fearfulness", "indecision")
      )
    )
  ),
  Openness_to_Experience = make_hexaco_dimension(
    description = paste(
      "Aesthetic sensitivity, intellectual curiosity, imagination, and",
      "openness to unconventional ideas."
    ),
    focus = paste(
      "Emphasize curiosity, imagination, aesthetic sensitivity, originality,",
      "and openness to unusual ideas."
    ),
    forbidden = c("sociability", "rebellious attention seeking", "impulsivity", "status"),
    facets = list(
      Aesthetic_Appreciation = make_facet(
        "Sensitivity to beauty in art, nature, music, or design.",
        c("I notice aesthetic details that others might overlook."),
        c("luxury", "fashion prestige")
      ),
      Inquisitiveness = make_facet(
        "Interest in understanding ideas, systems, cultures, or complex topics.",
        c("I enjoy exploring ideas that make me think deeply."),
        c("school grades", "expert status")
      ),
      Creativity = make_facet(
        "Imaginative and original thinking.",
        c("I enjoy coming up with original ways to approach a problem."),
        c("carelessness", "attention seeking")
      ),
      Unconventionality = make_facet(
        "Openness to unusual perspectives and nontraditional ideas.",
        c("I consider unusual perspectives before dismissing them."),
        c("antisocial behavior", "rule breaking")
      )
    )
  )
)

# A fresh directory per run keeps previous results intact.
run_dir <- tempfile(
  pattern = paste0("hexaco_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"),
  tmpdir = getwd()
)
dir.create(run_dir, recursive = TRUE)

result_dfi_parallel <- semantica_run_custom(
  scale_name = "HEXACO Full Capabilities Test",
  scale_description = paste(
    "Testing SEMANTICA generation, structural search, calibration and diagnostics.",
    general_hexaco_rules,
    sep = "\n\n"
  ),
  factors = hexaco_factors,

  llm = semantica_llm_config(
    backend = "unsloth",
    embed_backend = "local_nomic_unsloth",
    api_key = Sys.getenv("UNSLOTH_API_KEY"),
    embed_api_key = Sys.getenv("UNSLOTH_API_KEY"),
    base_url = UNSLOTH_BASE_URL,
    embed_base_url = UNSLOTH_BASE_URL,
    embed_backend_spec = nomic_backend,
    embedding_spec = nomic_embedding_contract,
    embedding_task = "psychometric_similarity",
    embedding_cache = TRUE,
    preflight = FALSE
  ),
  chat_model = CHAT_MODEL_ID,
  embed_model = EMBED_MODEL_ID,

  item_counts = semantica_item_count_config(pool = 12L, selected = 8L),
  generation = semantica_generation_config(
    language = "English",
    response_format = "5-point Likert",
    item_style = "first-person present-tense personality statement",
    temperature = 1,
    structured_output = "auto"
  ),
  resources = semantica_resource_config(cpu_cores = 4L, max_cpu_cores = 5L),
  compute = semantica_compute_config(
    compute_device = "cpu",
    cosine_adjustment = "none",
    compute_cosine_sensitivity = TRUE,
    cosine_sensitivity_seed = 20260216L,
    retain_embeddings = TRUE
  ),

  # Auto can choose another calibration engine; inspect cutoff_source below.
  # These replication counts exercise the machinery, not precision estimation.
  fit_calibration = semantica_fit_calibration_config(
    mode = "auto",
    search_reps = 50L,
    final_recalibrate = TRUE,
    final_reps = 100L,
    strategy = "fixed",
    warmup_iters = 2L,
    fallback_policy = "requested_only"
  ),
  diagnostics = semantica_diagnostics_config(
    final_fit = "extended",
    final_dddfi_reps = 50L,
    semantic_stability = TRUE,
    semantic_n_multipliers = c(0.5, 1, 2),
    # No participant data are supplied. Simulation-based sample-size planning
    # is left off because it introduces a separate, expensive simulation grid.
    validation_planning = FALSE
  ),
  plots = semantica_plot_config(
    level = "full",
    save = TRUE,
    out_dir = file.path(run_dir, "plots"),
    before_path_model = "proxy",
    include_interactive = TRUE
  ),

  quality = semantica_quality_config(
    profile = "strict",
    threshold_mode = "adaptive_pool",
    content_alignment_mode = "guard",
    semantic_objective_mode = "relative_conservative",
    polarity_screen = TRUE,
    polarity_action = "diagnostic",
    psychometric_guard_min_loading = 0.40,
    psychometric_guard_min_primary_ge_50 = 0.70,
    psychometric_guard_min_simple_structure = 0.80,
    psychometric_guard_min_dominance = 0.85,
    psychometric_guard_max_cross_loading = 0.30,
    psychometric_guard_include_htmt = TRUE,
    final_structure_repair = TRUE,
    final_structure_repair_max_swaps = 2L,
    final_structure_repair_candidates_per_factor = 2L
  ),
  pfa = semantica_pfa_config(
    mode = "objective",
    weight = 0.25,
    during_search = TRUE,
    every = 2L,
    extraction = "ml",
    final_extraction = "ml",
    rotation = "oblimin",
    max_abs_loading = 0.95,
    unit_diagnostics = TRUE,
    failure_policy = "penalize"
  ),
  esem = semantica_esem_config(
    score_mode = "structure_weighted",
    cadence_mode = "fixed"
  ),

  ants = 8L,
  search_patience = 8L,
  max_total_iter = 8L,
  elite_k = 6L,
  esem_every = 2L,
  # Multiple candidates permit worker use; actual concurrency depends on
  # distinct eligible candidates, available resources, and cached evaluations.
  esem_eval_top_k = 4L,
  max_esem_fits = 16L,
  run_esem_during_search = TRUE,
  esem_weight = 0.60,
  esem_failure_policy = "stop",
  seed = 20260216L,
  verbose = TRUE
)

# Save the full sanitized result before producing convenience reports.
bundle_path <- file.path(run_dir, "hexaco_bundle.rds")
semantica_save_bundle(
  result_dfi_parallel, path = bundle_path,
  include_embeddings = TRUE, write_manifest = TRUE
)
# This checks serialization; it does not run the pipeline again.
verified_bundle <- semantica_load_bundle(bundle_path, verify = TRUE)

print(semantica_evidence_status(result_dfi_parallel, labels = "both"))
print(semantica_esem_telemetry(result_dfi_parallel))

# Preserve actual stage outputs, including failure/skip information. Enabling
# a diagnostic does not guarantee convergence, admissibility, or an available
# optional dependency. Semantic N sensitivity varies the proxy reference N;
# it is not evidence from participant samples.
inspection_fields <- c(
  "resource_plan", "performance", "evaluation_telemetry",
  "search_guidance_status", "fit_indices", "pfa_diagnostics",
  "pfa_unit_diagnostics", "final_structure_repair", "semantic_n_sensitivity"
)
inspection <- result_dfi_parallel[
  intersect(inspection_fields, names(result_dfi_parallel))
]
capture.output(str(inspection, max.level = 4L),
               file = file.path(run_dir, "diagnostic_details.txt"))
capture.output(sessionInfo(), file = file.path(run_dir, "session_info.txt"))
semantica_export(
  result_dfi_parallel, prefix = file.path(run_dir, "hexaco"),
  include_candidates = TRUE
)
message("Run outputs: ", normalizePath(run_dir, winslash = "/"))
