# ==============================================================================
# SEMANTICA worked example: testing the full pipeline with a HEXACO item pool
# ==============================================================================
#
# Purpose
# -------
# This script demonstrates the complete SEMANTICA flow:
#
#   1. define a scale and its dimension/facet content specifications;
#   2. generate item text with a language-model backend;
#   3. embed the generated items with an embedding backend;
#   4. select a reduced item set with ACO-ESEM;
#   5. inspect reports, selected items, and diagnostic plots.
#
# It is intended as an editable starting point. The example uses the HEXACO
# structure because it shows how to express multiple dimensions and nested
# facets. For a new construct, replace the object named `factors_hexaco`,
# `scale_name`, `scale_description`, and `items_to_select`.
#
# Important interpretation note
# -----------------------------
# SEMANTICA uses semantic embeddings as an early item-pool screening proxy.
# A selected scale still requires validation with participant response data.
#
# Important security note
# -----------------------
# Never paste API keys into an R script or a GitHub repository. If a key has
# ever appeared in a shared script, message, or repository, revoke it at the
# provider and create a new key before running this example.
#
# How to run this installed example
# ---------------------------------
# After installing SEMANTICA, locate the file with:
#
#   example_file <- system.file(
#     "examples", "hexaco_full_pipeline_example.R", package = "SEMANTICA"
#   )
#   file.show(example_file)
#
# Work from an editable copy of that file in your analysis project.


# ==============================================================================
# 0. Required package and credentials
# ==============================================================================

# Install the development package once, if needed:
# install.packages("remotes")
# remotes::install_github("PedroCieza/SEMANTICA")

library(SEMANTICA)

# This example uses Groq for item generation and OpenAI for embeddings.
# Put keys in your .Renviron file, not below. A convenient interactive method is:
#
#   install.packages("usethis")             # once only
#   usethis::edit_r_environ()               # opens a private environment file
#
# Then add these lines, using your own current keys:
#
#   GROQ_API_KEY=your_groq_key_here
#   OPENAI_API_KEY=your_openai_key_here
#
# Save .Renviron and restart R before running this script.
required_keys <- c("GROQ_API_KEY", "OPENAI_API_KEY")
missing_keys <- required_keys[
  !nzchar(Sys.getenv(required_keys, unset = ""))
]

if (length(missing_keys) > 0L) {
  stop(
    "Before running the example, define these environment variables: ",
    paste(missing_keys, collapse = ", "),
    ". See the credential instructions at the top of this script."
  )
}

# Keys are deliberately not assigned to visible R objects. Because `api_key`
# and `embed_api_key` are left as NULL in the pipeline call below, SEMANTICA
# retrieves GROQ_API_KEY and OPENAI_API_KEY from the environment when needed.


# ==============================================================================
# 1. General item-writing rules reused across every HEXACO dimension
# ==============================================================================

# This text is appended to every dimension-specific prompt. Replace it when
# adapting the example to a different response population, language, age group,
# or item-writing policy.
hexaco_item_rules <- paste(
  "Generate brief first-person declarative items that describe typical behavior, preferences, or reactions.",
  "Write items for the high pole of the target facet only.",
  "Use everyday adult language; avoid clinical, moralizing, extreme, or diagnostic wording.",
  "Avoid double-barrelled items, causal explanations, abstract trait labels, and situationally rare scenarios.",
  "Avoid negations such as 'not', 'never', 'hardly', and 'do not' unless absolutely necessary.",
  "Do not mention the facet name or dimension name directly in the item.",
  "Keep each item focused on one observable tendency.",
  "Be creative and explore different ways of generating items.",
  sep = "\n"
)

# Two small helper constructors make the specification easier to edit. They
# only create ordinary lists; SEMANTICA receives the same structure that could
# be written directly with list(...).
make_facet <- function(description, examples, forbidden) {
  list(
    description = description,
    examples = examples,
    forbidden = forbidden
  )
}

make_dimension <- function(description, forbidden, focus, facets,
                           n_items = 16L) {
  list(
    n_items = n_items,
    description = description,
    forbidden = forbidden,
    extra_instructions = paste(hexaco_item_rules, focus, sep = "\n"),
    facets = facets
  )
}


# ==============================================================================
# 2. Construct specification: six HEXACO dimensions and their facets
# ==============================================================================

# Structure of each entry:
# - `n_items`: initial generated item count for that dimension when dimension
#   counts are respected.
# - `description`: conceptual boundaries of the dimension.
# - `forbidden`: content that should not contaminate that dimension.
# - `extra_instructions`: generation rules sent to the language model.
# - `facets`: named lower-order content domains; each has a description,
#   example stems, and prohibited content.
#
# To use your own scale:
# - rename the dimensions;
# - replace all descriptions and facets with your theoretical specification;
# - retain `forbidden` boundaries to reduce conceptual overlap;
# - keep at least enough generated items per dimension for the planned
#   selection target in `items_to_select`.
factors_hexaco <- list(
  Honesty_Humility = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward fairness, sincerity, modest self-presentation,",
      "and low exploitation of others for personal gain. High scorers are",
      "genuine in interpersonal dealings, avoid cheating or corruption, show",
      "little entitlement to special status, and place limited importance on",
      "wealth, luxury, and prestige."
    ),
    forbidden = c(
      "general politeness or warmth",
      "fear of punishment",
      "religious morality",
      "being shy or socially anxious",
      "work diligence or rule-following for its own sake"
    ),
    focus = paste(
      "For this dimension, emphasize exploitation versus fairness,",
      "status-seeking versus modesty, and genuineness versus manipulation.",
      "Do not turn items into general agreeableness, compassion, or",
      "conscientious rule-following."
    ),
    facets = list(
      Honesty = make_facet(
        description = paste(
          "Genuine and straightforward interpersonal behavior. The person",
          "presents intentions honestly, avoids strategic charm, and does not",
          "manipulate others for advantage."
        ),
        examples = c(
          "I speak plainly about my intentions with other people.",
          "I deal with people in a straightforward way.",
          "I present myself honestly when something is at stake."
        ),
        forbidden = c("general kindness", "conflict avoidance", "being talkative")
      ),
      Modesty = make_facet(
        description = paste(
          "Unassuming self-presentation and low entitlement. The person does",
          "not see themselves as deserving special treatment, admiration, or",
          "elevated social rank."
        ),
        examples = c(
          "I treat my own achievements as no reason to feel above others.",
          "I feel comfortable being seen as an ordinary person.",
          "I avoid acting as though I deserve special treatment."
        ),
        forbidden = c("low confidence", "social anxiety", "humiliation", "self-dislike")
      ),
      Fairness = make_facet(
        description = paste(
          "Avoidance of fraud, corruption, cheating, and exploitation. The",
          "person prefers equitable conduct even when dishonest shortcuts",
          "would bring benefits."
        ),
        examples = c(
          "I keep agreements even when breaking them would benefit me.",
          "I choose fair dealing when I could gain from a shortcut.",
          "I handle shared resources in a way that respects everyone involved."
        ),
        forbidden = c("legal fear", "obedience to authority", "perfectionism", "punctuality")
      ),
      Greed_Avoidance = make_facet(
        description = paste(
          "Limited attraction to wealth, luxury, social status, and expensive",
          "symbols of success. The person is relatively content without",
          "lavish possessions or status-based privileges."
        ),
        examples = c(
          "I stay content without luxury possessions.",
          "I place little personal value on symbols of high status.",
          "I feel satisfied without trying to appear wealthy."
        ),
        forbidden = c("poverty", "financial irresponsibility", "generosity", "charity")
      )
    )
  ),

  Emotionality = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward emotional sensitivity, harm avoidance,",
      "attachment, and need for support under stress. High scorers respond",
      "strongly to danger and stress, value emotional closeness, and form",
      "sentimental bonds."
    ),
    forbidden = c(
      "depression", "anger", "general kindness", "low self-esteem",
      "sociability", "clinical panic symptoms"
    ),
    focus = paste(
      "For this dimension, keep items about emotional sensitivity, threat",
      "response, attachment, and support seeking.",
      "Avoid making items sound pathological or clinically diagnostic."
    ),
    facets = list(
      Fearfulness = make_facet(
        description = paste(
          "Sensitivity to physical danger and situational threat. The person",
          "is alert to possible harm and prefers safety in risky situations."
        ),
        examples = c(
          "I become cautious when a situation could put me in danger.",
          "I notice possible risks in unfamiliar places.",
          "I prefer to stay safe when physical harm is possible."
        ),
        forbidden = c("general worry", "social embarrassment", "planning", "moral caution")
      ),
      Anxiety = make_facet(
        description = paste(
          "Tendency to worry under pressure and experience anticipatory",
          "tension about stressful events or uncertain outcomes."
        ),
        examples = c(
          "I feel tense when an important outcome is uncertain.",
          "I worry about problems before they are fully resolved.",
          "I become uneasy when I expect something stressful to happen."
        ),
        forbidden = c("fear of injury", "sadness", "anger", "perfectionism")
      ),
      Dependence = make_facet(
        description = paste(
          "Need for emotional support, reassurance, and closeness when facing",
          "distress or difficult decisions."
        ),
        examples = c(
          "I seek reassurance from someone close when I feel overwhelmed.",
          "I turn to trusted people when I am emotionally strained.",
          "I feel steadier when someone supportive is near during difficult moments."
        ),
        forbidden = c("submissiveness", "low competence", "social popularity", "agreeableness")
      ),
      Sentimentality = make_facet(
        description = paste(
          "Strength of emotional attachment, tenderness, and sentimental",
          "concern toward close others, memories, and meaningful relationships."
        ),
        examples = c(
          "I feel deeply moved by close emotional bonds.",
          "I become tender when thinking about people I care about.",
          "I value memories that connect me emotionally to others."
        ),
        forbidden = c("general compassion", "romantic jealousy", "fear", "need for attention")
      )
    )
  ),

  Extraversion = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward positive social self-regard, social confidence,",
      "enjoyment of social interaction, and energetic positive affect. High",
      "scorers feel socially capable, comfortable being visible, and lively",
      "in interaction."
    ),
    forbidden = c(
      "dominance over others", "manipulation", "impulsivity", "agreeableness",
      "need for admiration", "work ambition"
    ),
    focus = paste(
      "For this dimension, emphasize social confidence, social approach,",
      "enjoyment of company, and energetic positive affect.",
      "Do not write items about arrogance, status seeking, or controlling others."
    ),
    facets = list(
      Social_Self_Esteem = make_facet(
        description = paste(
          "Positive evaluation of one's social worth. The person feels",
          "accepted, likable, and comfortable with their place among others."
        ),
        examples = c(
          "I feel that I have value in social groups.",
          "I usually feel accepted when I am with other people.",
          "I feel comfortable with the impression I make socially."
        ),
        forbidden = c("superiority", "vanity", "achievement pride", "modesty")
      ),
      Social_Boldness = make_facet(
        description = paste(
          "Confidence in taking social initiative, speaking up, entering",
          "groups, and being visible in interpersonal settings."
        ),
        examples = c(
          "I feel comfortable starting conversations with people I do not know well.",
          "I can speak up in a group without much hesitation.",
          "I take social initiative when a situation calls for it."
        ),
        forbidden = c("aggression", "leadership dominance", "risk taking", "honesty")
      ),
      Sociability = make_facet(
        description = paste(
          "Preference for social contact, conversation, shared activities,",
          "and spending time with other people."
        ),
        examples = c(
          "I enjoy spending time with groups of people.",
          "I seek out opportunities to be with others.",
          "I feel energized by friendly conversation."
        ),
        forbidden = c("social status", "being liked at all costs", "dependence")
      ),
      Liveliness = make_facet(
        description = paste(
          "Energetic enthusiasm and positive affect in daily life. The person",
          "often feels animated, cheerful, and expressive."
        ),
        examples = c(
          "I bring energy into everyday activities.",
          "I often feel cheerful and animated.",
          "I express enthusiasm easily when I am engaged in something."
        ),
        forbidden = c("impulsivity", "mania", "attention seeking", "recklessness")
      )
    )
  ),

  Agreeableness = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward forgiveness, gentleness, flexibility, and",
      "patience in response to interpersonal conflict. In the HEXACO model",
      "this dimension primarily contrasts tolerance and anger control with",
      "irritability, stubbornness, and harshness."
    ),
    forbidden = c(
      "honesty", "fairness", "sentimentality", "emotional dependence",
      "general altruism", "social popularity"
    ),
    focus = paste(
      "For this dimension, focus on reactions to conflict, provocation,",
      "disagreement, and frustration.",
      "Do not write items about honesty, charity, emotional attachment, or",
      "wanting social approval."
    ),
    facets = list(
      Forgiveness = make_facet(
        description = paste(
          "Willingness to let go of resentment after being offended or",
          "treated badly. The person can move past interpersonal slights",
          "without prolonged bitterness."
        ),
        examples = c(
          "I can move on after someone has offended me.",
          "I let go of resentment once a conflict has passed.",
          "I give people room to make amends after they upset me."
        ),
        forbidden = c("trusting everyone", "excusing exploitation", "fairness", "sentimentality")
      ),
      Gentleness = make_facet(
        description = paste(
          "Mild and considerate treatment of others, especially when annoyed",
          "or disagreeing. The person avoids harshness and intimidation."
        ),
        examples = c(
          "I speak to people gently when I disagree with them.",
          "I handle tense conversations without becoming harsh.",
          "I try to keep my tone considerate during disagreements."
        ),
        forbidden = c("submissiveness", "shyness", "fear", "people pleasing")
      ),
      Flexibility = make_facet(
        description = paste(
          "Readiness to compromise, adjust one's position, and avoid rigid",
          "insistence on having things one's own way."
        ),
        examples = c(
          "I can adjust my position when others make a reasonable point.",
          "I look for compromise when plans conflict.",
          "I can change my mind during a disagreement."
        ),
        forbidden = c("lack of standards", "indecision", "obedience", "carelessness")
      ),
      Patience = make_facet(
        description = paste(
          "Control over irritation, anger, and hostile reactions when",
          "delayed, frustrated, criticized, or inconvenienced."
        ),
        examples = c(
          "I stay composed when small frustrations build up.",
          "I keep my temper under control when things take longer than expected.",
          "I remain patient when someone makes a mistake."
        ),
        forbidden = c("anxiety", "fearfulness", "perfectionism", "passivity")
      )
    )
  ),

  Conscientiousness = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward order, sustained effort, careful work, and",
      "deliberation. High scorers organize tasks, persist toward goals,",
      "maintain quality standards, and think carefully before acting."
    ),
    forbidden = c(
      "moral fairness", "social status", "fear of danger", "agreeableness",
      "emotional anxiety", "creativity"
    ),
    focus = paste(
      "For this dimension, emphasize task management, follow-through,",
      "precision, and deliberate action.",
      "Do not write items about moral virtue, pleasing others, or fear-based avoidance."
    ),
    facets = list(
      Organization = make_facet(
        description = paste(
          "Preference for order, structure, planning, and keeping materials",
          "or tasks arranged in a manageable way."
        ),
        examples = c(
          "I keep my tasks arranged in a clear order.",
          "I plan my work so I can keep track of what needs to be done.",
          "I maintain structure in the spaces where I work."
        ),
        forbidden = c("cleanliness obsession", "social control", "perfectionism only")
      ),
      Diligence = make_facet(
        description = paste(
          "Sustained effort, persistence, and responsibility in pursuing",
          "goals or completing demanding tasks."
        ),
        examples = c(
          "I keep working steadily toward goals that matter to me.",
          "I follow through on tasks even when they become demanding.",
          "I put consistent effort into responsibilities I accept."
        ),
        forbidden = c("ambition for status", "competitiveness", "anxiety", "obedience")
      ),
      Perfectionism = make_facet(
        description = paste(
          "Concern for accuracy, completeness, and high standards in one's",
          "work. The person checks details and tries to avoid careless errors."
        ),
        examples = c(
          "I check important details before considering my work finished.",
          "I care about doing tasks accurately.",
          "I notice small errors that could affect the quality of my work."
        ),
        forbidden = c("clinical perfectionism", "fear of failure", "rigidity", "anger")
      ),
      Prudence = make_facet(
        description = paste(
          "Deliberation and impulse control before acting. The person",
          "considers consequences and avoids rash decisions."
        ),
        examples = c(
          "I think through consequences before making important choices.",
          "I pause before acting when a decision could matter later.",
          "I consider possible outcomes before committing to a plan."
        ),
        forbidden = c("fearfulness", "indecision", "low energy", "obedience")
      )
    )
  ),

  Openness_to_Experience = make_dimension(
    n_items = 16L,
    description = paste(
      "A broad tendency toward aesthetic sensitivity, intellectual curiosity,",
      "imagination, creativity, and openness to unconventional ideas. High",
      "scorers seek complexity, novelty, beauty, and original ways of thinking."
    ),
    forbidden = c(
      "sociability", "rebelliousness for attention", "impulsivity",
      "academic achievement", "work diligence", "social status"
    ),
    focus = paste(
      "For this dimension, emphasize curiosity, imagination, aesthetics,",
      "originality, and openness to unusual ideas.",
      "Do not write items about being popular, reckless, disorganized, or",
      "merely well educated."
    ),
    facets = list(
      Aesthetic_Appreciation = make_facet(
        description = paste(
          "Sensitivity to beauty in art, nature, music, design, language, or",
          "other aesthetic experiences."
        ),
        examples = c(
          "I become absorbed in the beauty of art or nature.",
          "I notice aesthetic details that others might overlook.",
          "I feel moved by music, images, or places that have beauty."
        ),
        forbidden = c("luxury", "status taste", "fashion prestige", "sentimentality")
      ),
      Inquisitiveness = make_facet(
        description = paste(
          "Intellectual curiosity and desire to understand ideas, systems,",
          "cultures, or complex topics."
        ),
        examples = c(
          "I enjoy exploring ideas that make me think deeply.",
          "I seek out explanations for topics I do not yet understand.",
          "I like learning about complex subjects for their own sake."
        ),
        forbidden = c("school grades", "work diligence", "expert status", "argumentativeness")
      ),
      Creativity = make_facet(
        description = paste(
          "Imaginative and original thinking. The person enjoys generating",
          "new possibilities, designs, stories, solutions, or perspectives."
        ),
        examples = c(
          "I enjoy coming up with original ways to approach a problem.",
          "I often imagine possibilities beyond the usual approach.",
          "I like creating ideas that feel new or personal."
        ),
        forbidden = c("impulsivity", "carelessness", "attention seeking", "daydreaming only")
      ),
      Unconventionality = make_facet(
        description = paste(
          "Openness to unusual ideas, alternative perspectives, and",
          "nontraditional ways of thinking without adopting them merely to",
          "provoke others."
        ),
        examples = c(
          "I am open to ideas that differ from common opinion.",
          "I consider unusual perspectives before dismissing them.",
          "I feel comfortable questioning conventional assumptions."
        ),
        forbidden = c("antisocial behavior", "rebellion", "rule breaking", "status signaling")
      )
    )
  )
)


# ==============================================================================
# 3. Selection target: how many final items remain in each dimension
# ==============================================================================

# This is not the number of items initially generated. It is the number the
# optimizer should retain in the final shortened form. For a more substantial
# operational scale, use a larger final target after assessing content coverage.
i_per_dimension_full <- c(
  Honesty_Humility = 3L,
  Emotionality = 3L,
  Extraversion = 3L,
  Agreeableness = 3L,
  Conscientiousness = 3L,
  Openness_to_Experience = 3L
)


# ==============================================================================
# 4. Choose a first test run or the complete six-dimension run
# ==============================================================================

# Start with "quick". It exercises the same full pipeline but uses two
# dimensions, fewer generated items, and less expensive optional diagnostics.
# Once it succeeds, change this value to "full" for the complete HEXACO run.
example_mode <- "quick"  # Allowed values: "quick" or "full".
stopifnot(example_mode %in% c("quick", "full"))

if (example_mode == "quick") {
  # A credential-and-workflow smoke test. It still performs generation,
  # embedding, ACO selection, final ESEM fitting, and plotting.
  factors_for_run <- factors_hexaco[c("Honesty_Humility", "Extraversion")]
  generated_items_per_dimension <- 8L
  items_to_select <- i_per_dimension_full[names(factors_for_run)]

  ants_for_run <- 20L
  max_iterations_for_run <- 8L
  esem_top_k_for_run <- 4L

  # This inexpensive setting skips simulation-based cutoff calibration during
  # the first connection test. Change to "semantic_roc_dfi" in serious runs.
  dfi_mode_for_run <- "heuristic_semantic"
  final_dfi_recalibrate_for_run <- FALSE
  semantic_n_sensitivity_for_run <- FALSE
  final_equivtest_for_run <- FALSE
} else {
  # A complete six-dimension reduction modeled after the substantive design.
  # This run makes more API requests and can require substantial computation.
  factors_for_run <- factors_hexaco
  generated_items_per_dimension <- 16L
  items_to_select <- i_per_dimension_full

  ants_for_run <- 90L
  max_iterations_for_run <- 35L
  esem_top_k_for_run <- 14L

  dfi_mode_for_run <- "semantic_roc_dfi"
  final_dfi_recalibrate_for_run <- TRUE
  semantic_n_sensitivity_for_run <- TRUE
  final_equivtest_for_run <- TRUE
}

# Setting `n_per_factor_override = TRUE` in the pipeline call below tells
# SEMANTICA to allocate `generated_items_per_dimension` across the four facets
# in each included dimension. Alternatively, remove `n_per_factor` from the
# call and set `n_per_factor_override = FALSE` to respect each dimension's
# stored `n_items` value independently.
run_plan <- data.frame(
  dimension = names(factors_for_run),
  generated_target = generated_items_per_dimension,
  final_selected_target = unname(items_to_select),
  stringsAsFactors = FALSE
)
print(run_plan)


# ==============================================================================
# 5. Run SEMANTICA: item generation -> embeddings -> ACO-ESEM -> plots
# ==============================================================================

# The random seed makes stochastic search choices easier to reproduce. API
# generation may still vary because remote models can change or be stochastic.
set.seed(2026)

hexaco_reduction <- semantica_full_pipeline(
  # ---------------------------------------------------------------------------
  # Backends and credentials
  # ---------------------------------------------------------------------------
  # Generation and embedding can come from different services. API keys are
  # NULL so that SEMANTICA reads GROQ_API_KEY and OPENAI_API_KEY from .Renviron.
  backend = "groq",
  embed_backend = "openai",
  api_key = NULL,
  embed_api_key = NULL,

  # `NULL` uses SEMANTICA's backend default chat model. Specify a model string
  # here only if it is available from your provider at the time of execution.
  chat_model = NULL,
  embed_model = "text-embedding-3-small",
  embed_batch_size = 64L,

  # ---------------------------------------------------------------------------
  # Construct and item pool
  # ---------------------------------------------------------------------------
  scale_name = "HEXACO-S",
  scale_description = paste(
    "Six-factor personality inventory based on the HEXACO model,",
    "adapted for English-speaking adult populations."
  ),
  factors = factors_for_run,

  # `n_per_factor` is the generated candidate count per included dimension.
  # It differs from `i.per.f`, which is the final selected item count.
  n_per_factor = generated_items_per_dimension,
  n_per_factor_override = TRUE,
  i.per.f = items_to_select,

  # These arguments are forwarded to item generation through `...`.
  language = "English",
  item_style = "first-person declarative sentence",
  response_format = "5-point Likert (1 = Strongly disagree to 5 = Strongly agree)",
  temperature = 0.80,
  overgenerate = 2.0,
  max_retries = 5L,
  global_forbidden_max = 40L,

  # ---------------------------------------------------------------------------
  # Embedding-derived semantic proxy
  # ---------------------------------------------------------------------------
  # "none" uses standard cosine similarity. A later sensitivity analysis can
  # compare mean-centered cosine without changing the primary result.
  cosine_adjustment = "none",
  semantic_calibration = NULL,
  compute_cosine_sensitivity = TRUE,
  retain_embeddings = TRUE,

  # ---------------------------------------------------------------------------
  # ACO search and ESEM guidance
  # ---------------------------------------------------------------------------
  ants = ants_for_run,
  max.iter = max_iterations_for_run,
  esem_every = 10L,
  run_esem_during_search = TRUE,
  esem_weight = 0.30,
  esem_eval_top_k = esem_top_k_for_run,
  elite_k = 10L,
  elite_pareto_rerank = TRUE,
  esem_failure_policy = "stop",

  # ---------------------------------------------------------------------------
  # ESEM model specification and proxy reference sample size
  # ---------------------------------------------------------------------------
  rotation = "target",
  rotation_args = list(),
  data_type = "continuous",
  target_loadings = 0.60,
  esem_sample_size = "auto",
  reference_rmsea_close = 0.05,
  reference_rmsea_poor = 0.06,
  reference_power = 0.80,
  reference_alpha = 0.05,
  reference_max_n = 5000L,

  # ---------------------------------------------------------------------------
  # DFI calibration and reference-N sensitivity
  # ---------------------------------------------------------------------------
  # In "quick" mode, heuristic semantic cutoffs make the first run faster.
  # In "full" mode, semantic ROC DFI performs simulation-based calibration.
  dfi_mode = dfi_mode_for_run,
  dfi_esem_reps = NULL,
  dfi_level = 1,
  dfi_criterion = "Sensitivity",
  dfi_warmup_iters = 5L,
  dfi_roc_misspec_strength = 1.0,
  final_dfi_recalibrate = final_dfi_recalibrate_for_run,
  semantic_n_sensitivity = semantic_n_sensitivity_for_run,
  semantic_n_grid = NULL,
  semantic_n_multipliers = c(0.50, 1.00, 1.50, 2.00),
  semantic_n_iter_max = 800L,
  semantic_esem_score_mode = "structure_weighted",

  # Keep proxy unreliability assumptions neutral in this initial analysis.
  # These can be changed in a planned sensitivity analysis.
  embed_reliability = 1.00,
  residual_inflation = 0.00,

  # ---------------------------------------------------------------------------
  # Sample-free PFA diagnostic and semantic/psychometric safeguards
  # ---------------------------------------------------------------------------
  pfa_mode = "diagnostic",
  pfa_weight = 0.20,
  pfa_extraction = "principal",
  pfa_final_extraction = "ml",
  pfa_rotation = "oblimin",
  pfa_min_loading = 0.40,
  pfa_min_margin = 0.20,
  pfa_unit_diagnostics = TRUE,

  within_similarity_target = 0.30,
  within_similarity_band = 0.06,
  facet_coverage_weight = 0.10,
  psychometric_guard_weight = 0.75,
  psychometric_guard_min_ave = 0.30,
  psychometric_guard_min_loading = 0.40,
  psychometric_guard_min_primary_ge_50 = 0.70,

  redundancy_threshold = 0.85,
  dup_threshold = 0.90,
  htmt_threshold = 0.85,

  # ---------------------------------------------------------------------------
  # Optional respondent-sample planning and validation
  # ---------------------------------------------------------------------------
  # These are disabled in the first test. Enable `validation_n_diagnostic`
  # when you want a Monte Carlo planning diagnostic, and supply
  # `validation_data` only when real respondent data are available.
  validation_n_diagnostic = FALSE,
  validation_n_reps = 20L,
  validation_n_grid = NULL,
  validation_n_max = 2000L,
  validation_n_convergence = 0.90,
  validation_n_max_heywood = 0.05,
  validation_n_min_recovery = 0.90,
  validation_n_max_loading_error = 0.10,
  validation_n_min_dominance = NULL,
  validation_n_max_cross_error = NULL,
  validation_n_max_factor_cor_error = NULL,
  validation_data = NULL,
  validation_ordered = NULL,

  # Companion final diagnostic. It is disabled during the quick test and
  # enabled in full mode through `final_equivtest_for_run`.
  final_dddfi = FALSE,
  final_equivtest = final_equivtest_for_run,

  # ---------------------------------------------------------------------------
  # Computation and plots
  # ---------------------------------------------------------------------------
  pheromone_update = "top_elite",
  use_parallel = TRUE,
  n.cores = 2L,  # SEMANTICA limits parallel work to two workers.
  generate_plots = TRUE,
  interactive_mode = "2d",
  include_interactive_plot = FALSE,
  save_plots = FALSE,
  verbose = TRUE
)


# ==============================================================================
# 6. Examine the results
# ==============================================================================

# The returned object groups all stages so you can revisit generation,
# optimization, and plots without rerunning API calls.
names(hexaco_reduction)

# Read the final optimizer report. It displays selected item IDs, fit
# diagnostics, semantic screening diagnostics, and search metadata.
report_semantica_v2(hexaco_reduction$optimization)

# View generated items and the final selected subset with their dimension/facet
# labels. The selected metadata are especially useful for content review.
semantica_print_items(hexaco_reduction$generation$items_tbl, max_chars = 100L)
print(hexaco_reduction$selected_item_metadata)

# Extract commonly reviewed values into ordinary objects that can be exported
# or used in later analyses.
selected_items <- hexaco_reduction$selected_item_metadata
fit_indices <- hexaco_reduction$fit_indices
semantic_reduction <- hexaco_reduction$semantic_similarity_reduction

print(fit_indices[c("cfi", "rmsea", "srmr", "htmt_max")])
print(semantic_reduction)

# Plots are returned as a named list. Inspect available names first because
# some diagnostics depend on which optional computations were requested.
if (!is.null(hexaco_reduction$plots)) {
  print(names(hexaco_reduction$plots))

  if (!is.null(hexaco_reduction$plots$plot_summary_of_results)) {
    print(hexaco_reduction$plots$plot_summary_of_results)
  }
}


# ==============================================================================
# 7. Optional export: avoid repeating paid API generation and embedding calls
# ==============================================================================

# Set to TRUE only when you want CSV files in your current working directory.
# The export contains the generated item pool and cosine matrix, enabling
# offline re-analysis with ACO settings after generation and embedding.
export_generated_pool <- FALSE

if (export_generated_pool) {
  output_prefix <- paste0("hexaco_semantica_", example_mode)
  semantica_export(hexaco_reduction$generation, prefix = output_prefix)

  # Later, reload the generated/embedded pool without calling either API:
  # reloaded <- semantica_reload(output_prefix, i.per.f = items_to_select)
}


# ==============================================================================
# 8. What users usually replace for their own analysis
# ==============================================================================
#
# 1. Construct content:
#    Replace `factors_hexaco`, `scale_name`, and `scale_description`.
#
# 2. Item-pool size:
#    Increase/decrease `generated_items_per_dimension`. More candidates improve
#    content exploration but increase generation, embedding, and search costs.
#
# 3. Short-form length:
#    Change `items_to_select`. Retaining more items can protect content breadth,
#    but changes the ESEM specification and interpretation.
#
# 4. Backends:
#    Change `backend`, `embed_backend`, and model names according to providers
#    you can access. Keep credentials in environment variables.
#
# 5. Computation budget:
#    Raise `ants_for_run` and `max_iterations_for_run` for more extensive
#    searches; keep a smaller run while developing a construct specification.
#
# 6. Optional response data:
#    Once respondent data exist, set `validation_data` and, for categorical
#    item responses when applicable, `validation_ordered` according to the
#    documented function arguments.
#
# No semantic-only selection should be treated as final psychometric evidence:
# review the wording, evaluate content coverage with subject-matter experts,
# and validate the retained items on response data.
