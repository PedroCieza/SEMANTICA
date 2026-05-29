# ==============================================================================
# SEMANTICA simple full-pipeline example
# ==============================================================================
#
# This script shows the smallest practical use of semantica_full_pipeline().
# It uses the package defaults wherever possible.
#
# In this version, the example remains "simple" because the analysis is run
# through one high-level function call. Several common options are explicit so
# the run is reproducible: Groq is used for item generation, OpenAI is used for
# embeddings, heuristic semantic cutoffs are used for speed, and ESEM is allowed
# to guide the ACO search.
#
# Required inputs used below:
# - scale_name: a short name for the scale.
# - scale_description: a brief description of the construct to measure.
# - factors_hexaco: a named list of the theoretical factors. It contains the
#   prompts for the LLM to generate content.
# - groq_key and openai_key: API credentials for the two model providers used
#   in this example.
#
# The concrete call below overrides some package defaults. For example, it uses
# backend = "groq" and embed_backend = "openai" instead of the single-backend
# default. The following default list applies only when these arguments are not
# supplied by the user.
#
# Important default choices used implicitly in this example:
# - backend = "openai"
# - embed_backend = NULL, so the same backend is used for embeddings.
# - n_per_factor = 15L, so 15 candidate items are generated per factor.
# - i.per.f = NULL, so 3 items are selected per factor.
# - final_dddfi = FALSE, so final DDDFI cutoffs are not computed unless you
#   explicitly request them.
# - generate_plots = TRUE, so diagnostic plot objects are returned.
# - save_plots = FALSE, so plots are not written to disk.
#
# Optional arguments commonly added for a quick test run:
# - n_per_factor: number of candidate items to generate per factor.
# - i.per.f: named vector with the number of items to select per factor.
# - ants and max.iter: ACO search size and number of iterations.
# - generate_plots: set FALSE if you only want the selected items and metrics.
# - verbose: set FALSE for a quieter console.
#
# This example uses the exported function name in the package:
# semantica_full_pipeline().

library(SEMANTICA)


# OBJECTS NEEDED AS IMPUTS

# API keys

groq_key <- "your_groq_key" # if you are using the Groq models
openai_key <- "your_openai_key" # if you are using the OpenAI models

# Prefer storing real keys in .Renviron instead of typing them into scripts.
# Example:
# groq_key <- Sys.getenv("GROQ_API_KEY")
# openai_key <- Sys.getenv("OPENAI_API_KEY")


# Prompts

  # General rules of content creation

hexaco_item_rules <- paste(
  "Generate brief first-person declarative items that describe typical behavior, preferences, or reactions.",
  "Write items for the high pole of the target facet only.",
  "Use everyday adult language; avoid clinical, moralizing, extreme, or diagnostic wording.",
  "Avoid double-barrelled items, causal explanations, abstract trait labels, and situationally rare scenarios.",
  "Avoid negations such as 'not', 'never', 'hardly', and 'do not' unless absolutely necessary.",
  "Do not mention the facet name or dimension name directly in the item.",
  "Keep each item focused on one observable tendency.",
  "Be creative and explore different ways in generating items.",
  sep = "\n"
)

  # Factors assumed by the theoretical model, this object should contain
  # all the information needed as prompts for the model to generate content

factors_hexaco <- list(
  Honesty_Humility = list(
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
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, emphasize exploitation versus fairness, status-seeking versus modesty, and genuineness versus manipulation.",
      "Do not turn items into general agreeableness, compassion, or conscientious rule-following."
    ),
    facets = list(
      Honesty = list(
        description = paste(
          "Genuine and straightforward interpersonal behavior. The person presents",
          "intentions honestly, avoids flattery or strategic charm, and does not",
          "manipulate others for advantage."
        ),
        examples = c(
          "I speak plainly about my intentions with other people.",
          "I deal with people in a straightforward way.",
          "I present myself honestly when something is at stake."
        ),
        forbidden = c("general kindness", "conflict avoidance", "being talkative")
      ),
      Modesty = list(
        description = paste(
          "Unassuming self-presentation and low entitlement. The person does not",
          "see themselves as deserving special treatment, admiration, or elevated",
          "social rank."
        ),
        examples = c(
          "I treat my own achievements as no reason to feel above others.",
          "I feel comfortable being seen as an ordinary person.",
          "I avoid acting as though I deserve special treatment."
        ),
        forbidden = c("low confidence", "social anxiety", "humiliation", "self-dislike")
      ),
      Fairness = list(
        description = paste(
          "Avoidance of fraud, corruption, cheating, and exploitation. The person",
          "prefers equitable conduct even when dishonest shortcuts would bring benefits."
        ),
        examples = c(
          "I keep agreements even when breaking them would benefit me.",
          "I choose fair dealing when I could gain from a shortcut.",
          "I handle shared resources in a way that respects everyone involved."
        ),
        forbidden = c("legal fear", "obedience to authority", "perfectionism", "punctuality")
      ),
      Greed_Avoidance = list(
        description = paste(
          "Limited attraction to wealth, luxury, social status, and expensive symbols",
          "of success. The person is relatively content without lavish possessions",
          "or status-based privileges."
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

  Emotionality = list(
    n_items = 16L,
    description = paste(
      "A broad tendency toward emotional sensitivity, harm avoidance, attachment,",
      "and need for support under stress. High scorers respond strongly to danger",
      "and stress, value emotional closeness, and form sentimental bonds."
    ),
    forbidden = c(
      "depression",
      "anger",
      "general kindness",
      "low self-esteem",
      "sociability",
      "clinical panic symptoms"
    ),
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, keep items about emotional sensitivity, threat response, attachment, and support seeking.",
      "Avoid making items sound pathological or clinically diagnostic."
    ),
    facets = list(
      Fearfulness = list(
        description = paste(
          "Sensitivity to physical danger and situational threat. The person is",
          "alert to possible harm and prefers safety in risky situations."
        ),
        examples = c(
          "I become cautious when a situation could put me in danger.",
          "I notice possible risks in unfamiliar places.",
          "I prefer to stay safe when physical harm is possible."
        ),
        forbidden = c("general worry", "social embarrassment", "planning", "moral caution")
      ),
      Anxiety = list(
        description = paste(
          "Tendency to worry under pressure and experience anticipatory tension",
          "about stressful events or uncertain outcomes."
        ),
        examples = c(
          "I feel tense when an important outcome is uncertain.",
          "I worry about problems before they are fully resolved.",
          "I become uneasy when I expect something stressful to happen."
        ),
        forbidden = c("fear of injury", "sadness", "anger", "perfectionism")
      ),
      Dependence = list(
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
      Sentimentality = list(
        description = paste(
          "Strength of emotional attachment, tenderness, and sentimental concern",
          "toward close others, memories, and meaningful relationships."
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

  Extraversion = list(
    n_items = 16L,
    description = paste(
      "A broad tendency toward positive social self-regard, social confidence,",
      "enjoyment of social interaction, and energetic positive affect. High scorers",
      "feel socially capable, comfortable being visible, and lively in interaction."
    ),
    forbidden = c(
      "dominance over others",
      "manipulation",
      "impulsivity",
      "agreeableness",
      "need for admiration",
      "work ambition"
    ),
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, emphasize social confidence, social approach, enjoyment of company, and energetic positive affect.",
      "Do not write items about arrogance, status seeking, or controlling others."
    ),
    facets = list(
      Social_Self_Esteem = list(
        description = paste(
          "Positive evaluation of one's social worth. The person feels accepted,",
          "likable, and comfortable with their place among others."
        ),
        examples = c(
          "I feel that I have value in social groups.",
          "I usually feel accepted when I am with other people.",
          "I feel comfortable with the impression I make socially."
        ),
        forbidden = c("superiority", "vanity", "achievement pride", "modesty")
      ),
      Social_Boldness = list(
        description = paste(
          "Confidence in taking social initiative, speaking up, entering groups,",
          "and being visible in interpersonal settings."
        ),
        examples = c(
          "I feel comfortable starting conversations with people I do not know well.",
          "I can speak up in a group without much hesitation.",
          "I take social initiative when a situation calls for it."
        ),
        forbidden = c("aggression", "leadership dominance", "risk taking", "honesty")
      ),
      Sociability = list(
        description = paste(
          "Preference for social contact, conversation, shared activities, and",
          "spending time with other people."
        ),
        examples = c(
          "I enjoy spending time with groups of people.",
          "I seek out opportunities to be with others.",
          "I feel energized by friendly conversation."
        ),
        forbidden = c("social status", "being liked at all costs", "dependence")
      ),
      Liveliness = list(
        description = paste(
          "Energetic enthusiasm and positive affect in daily life. The person often",
          "feels animated, cheerful, and expressive."
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

  Agreeableness = list(
    n_items = 16L,
    description = paste(
      "A broad tendency toward forgiveness, gentleness, flexibility, and patience",
      "in response to interpersonal conflict. In the HEXACO model this dimension",
      "primarily contrasts tolerance and anger control with irritability, stubbornness,",
      "and harshness."
    ),
    forbidden = c(
      "honesty",
      "fairness",
      "sentimentality",
      "emotional dependence",
      "general altruism",
      "social popularity"
    ),
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, focus on reactions to conflict, provocation, disagreement, and frustration.",
      "Do not write items about honesty, charity, emotional attachment, or wanting social approval."
    ),
    facets = list(
      Forgiveness = list(
        description = paste(
          "Willingness to let go of resentment after being offended or treated badly.",
          "The person can move past interpersonal slights without prolonged bitterness."
        ),
        examples = c(
          "I can move on after someone has offended me.",
          "I let go of resentment once a conflict has passed.",
          "I give people room to make amends after they upset me."
        ),
        forbidden = c("trusting everyone", "excusing exploitation", "fairness", "sentimentality")
      ),
      Gentleness = list(
        description = paste(
          "Mild and considerate treatment of others, especially when annoyed or",
          "disagreeing. The person avoids harshness and intimidation."
        ),
        examples = c(
          "I speak to people gently when I disagree with them.",
          "I handle tense conversations without becoming harsh.",
          "I try to keep my tone considerate during disagreements."
        ),
        forbidden = c("submissiveness", "shyness", "fear", "people pleasing")
      ),
      Flexibility = list(
        description = paste(
          "Readiness to compromise, adjust one's position, and avoid rigid insistence",
          "on having things one's own way."
        ),
        examples = c(
          "I can adjust my position when others make a reasonable point.",
          "I look for compromise when plans conflict.",
          "I can change my mind during a disagreement."
        ),
        forbidden = c("lack of standards", "indecision", "obedience", "carelessness")
      ),
      Patience = list(
        description = paste(
          "Control over irritation, anger, and hostile reactions when delayed,",
          "frustrated, criticized, or inconvenienced."
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

  Conscientiousness = list(
    n_items = 16L,
    description = paste(
      "A broad tendency toward order, sustained effort, careful work, and deliberation.",
      "High scorers organize tasks, persist toward goals, maintain quality standards,",
      "and think carefully before acting."
    ),
    forbidden = c(
      "moral fairness",
      "social status",
      "fear of danger",
      "agreeableness",
      "emotional anxiety",
      "creativity"
    ),
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, emphasize task management, follow-through, precision, and deliberate action.",
      "Do not write items about moral virtue, pleasing others, or fear-based avoidance."
    ),
    facets = list(
      Organization = list(
        description = paste(
          "Preference for order, structure, planning, and keeping materials or",
          "tasks arranged in a manageable way."
        ),
        examples = c(
          "I keep my tasks arranged in a clear order.",
          "I plan my work so I can keep track of what needs to be done.",
          "I maintain structure in the spaces where I work."
        ),
        forbidden = c("cleanliness obsession", "social control", "perfectionism only")
      ),
      Diligence = list(
        description = paste(
          "Sustained effort, persistence, and responsibility in pursuing goals",
          "or completing demanding tasks."
        ),
        examples = c(
          "I keep working steadily toward goals that matter to me.",
          "I follow through on tasks even when they become demanding.",
          "I put consistent effort into responsibilities I accept."
        ),
        forbidden = c("ambition for status", "competitiveness", "anxiety", "obedience")
      ),
      Perfectionism = list(
        description = paste(
          "Concern for accuracy, completeness, and high standards in one's work.",
          "The person checks details and tries to avoid careless errors."
        ),
        examples = c(
          "I check important details before considering my work finished.",
          "I care about doing tasks accurately.",
          "I notice small errors that could affect the quality of my work."
        ),
        forbidden = c("clinical perfectionism", "fear of failure", "rigidity", "anger")
      ),
      Prudence = list(
        description = paste(
          "Deliberation and impulse control before acting. The person considers",
          "consequences and avoids rash decisions."
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

  Openness_to_Experience = list(
    n_items = 16L,
    description = paste(
      "A broad tendency toward aesthetic sensitivity, intellectual curiosity,",
      "imagination, creativity, and openness to unconventional ideas. High scorers",
      "seek complexity, novelty, beauty, and original ways of thinking."
    ),
    forbidden = c(
      "sociability",
      "rebelliousness for attention",
      "impulsivity",
      "academic achievement",
      "work diligence",
      "social status"
    ),
    extra_instructions = paste(
      hexaco_item_rules,
      "For this dimension, emphasize curiosity, imagination, aesthetics, originality, and openness to unusual ideas.",
      "Do not write items about being popular, reckless, disorganized, or merely well educated."
    ),
    facets = list(
      Aesthetic_Appreciation = list(
        description = paste(
          "Sensitivity to beauty in art, nature, music, design, language, or other",
          "aesthetic experiences."
        ),
        examples = c(
          "I become absorbed in the beauty of art or nature.",
          "I notice aesthetic details that others might overlook.",
          "I feel moved by music, images, or places that have beauty."
        ),
        forbidden = c("luxury", "status taste", "fashion prestige", "sentimentality")
      ),
      Inquisitiveness = list(
        description = paste(
          "Intellectual curiosity and desire to understand ideas, systems, cultures,",
          "or complex topics."
        ),
        examples = c(
          "I enjoy exploring ideas that make me think deeply.",
          "I seek out explanations for topics I do not yet understand.",
          "I like learning about complex subjects for their own sake."
        ),
        forbidden = c("school grades", "work diligence", "expert status", "argumentativeness")
      ),
      Creativity = list(
        description = paste(
          "Imaginative and original thinking. The person enjoys generating new",
          "possibilities, designs, stories, solutions, or perspectives."
        ),
        examples = c(
          "I enjoy coming up with original ways to approach a problem.",
          "I often imagine possibilities beyond the usual approach.",
          "I like creating ideas that feel new or personal."
        ),
        forbidden = c("impulsivity", "carelessness", "attention seeking", "daydreaming only")
      ),
      Unconventionality = list(
        description = paste(
          "Openness to unusual ideas, alternative perspectives, and nontraditional",
          "ways of thinking without adopting them merely to provoke others."
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

# Your desired final scale lenght

i_per_dimension <- c(
  Honesty_Humility = 3L,
  Emotionality = 3L,
  Extraversion = 3L,
  Agreeableness = 3L,
  Conscientiousness = 3L,
  Openness_to_Experience = 3L
)

# SEMANTICA USAGE
#
# Argument guide for the call below:
# - backend and embed_backend choose the providers for generation and embedding.
#   Groq is used here for text generation; OpenAI is used for embeddings.
# - chat_model and embed_model choose the specific models used by those
#   providers.
# - temperature controls generation variability. Higher values usually produce
#   more diverse candidate item wording.
# - factors is the construct specification and prompt material.
# - n_per_factor controls how many candidate items are retained from generation
#   per broad factor before ACO selection.
# - i.per.f controls the final selected item count per factor.
# - dfi_mode = "heuristic_semantic" skips simulated DFI calibration. This is
#   faster and useful for examples or exploratory runs, but less calibrated than
#   "auto" or the ESEM DFI modes.
# - ants and max.iter control the ACO search effort.
# - esem_every is an interval for ESEM search checkpoints, not the exact total
#   number of ESEM fits.
# - esem_weight controls how much the ESEM score contributes to the final
#   search objective when run_esem_during_search = TRUE.
# - n.cores is the requested worker count. SEMANTICA may cap workers for
#   resource control and will warn when it does.

set.seed(123)

result <- semantica_full_pipeline(

  # LLM setup
  backend       = "groq",
  embed_backend = "openai",
  api_key       = groq_key,
  embed_api_key = openai_key,
  chat_model  = "meta-llama/llama-4-scout-17b-16e-instruct", # model for content generation
  embed_model = "text-embedding-3-small", # model for embeddings extraction
  temperature     = 1, # temperature for the generative model

  # Information about your desired scale
  factors      = factors_hexaco, # User prompts
  n_per_factor = 10L, # number of items to be generated per factor
  i.per.f      = i_per_dimension, # number of items to be retained
  scale_name = "HEXACO-AI",
  scale_description = paste(
    "Six-factor personality inventory based on the HEXACO model,",
    "adapted for English-speaking populations."
  ),

  # DFI calibration
  # The heuristic mode avoids the long DFI simulation step. The final ESEM fit
  # is still compared against the active heuristic/search cutoffs.
  dfi_mode              = "heuristic_semantic", # DFI cutoffs mode, leave it as "heuristic_semantic" to skip the estimation, thus a faster run.

  # ACO search
  ants     = 50L, # Ants to be used in the ACO search
  max.iter = 20L, # Max iterations to be tolerated with no improvements
     # ESEM configurations
  # ESEM is used inside the ACO search every `esem_every` iterations/checkpoints.
  # Raising `esem_weight` makes the search rely more on ESEM quality and less
  # on semantic/PFA proposal quality.
  esem_every             = 10L, # Select how many times ESEM calculations are going to run inside the ACO search.
  run_esem_during_search = TRUE,
  esem_weight            = 0.50,

  # Parallelization
  # This requests parallel execution. The package may use fewer workers than
  # requested depending on the active resource controls.
  use_parallel = TRUE,
  n.cores      = 6L
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

# Because generate_plots defaults to TRUE, diagnostic plots are stored here.
names(result$plots)
