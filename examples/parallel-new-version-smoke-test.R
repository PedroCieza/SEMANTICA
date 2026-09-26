# Parallel smoke/regression run for the current SEMANTICA checkout.
#
# Usage from the package root:
#   Rscript examples/parallel-new-version-smoke-test.R
#
# Main environment knobs:
#   SEMANTICA_UNSLOTH_BASE_URL  OpenAI-compatible local origin; default http://localhost:8888
#   SEMANTICA_CHAT_MODEL        Chat/model id; default local-model
#   SEMANTICA_EMBED_MODEL       Embedding model id; default nomic-embed-text
#   NOMIC_EMBED_DIM             Embedding dimension; default 384
#   SEMANTICA_SMOKE_WORKERS     Explicit PSOCK workers; default min(6, physical cores - 1)
#   SEMANTICA_SMOKE_ANTS        ACO ants per iteration; default 24
#   SEMANTICA_SMOKE_MAX_ITER    Hard iteration ceiling; default 14
#   SEMANTICA_SMOKE_ESEM_FITS   Search-time ESEM fit ceiling; default 24

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

find_repo_root <- function(start_dir) {
  cur <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  for (i in seq_len(8L)) {
    if (file.exists(file.path(cur, "DESCRIPTION")) && dir.exists(file.path(cur, "R"))) {
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

detected_cores <- function() {
  physical <- suppressWarnings(tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_))
  logical <- suppressWarnings(tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_))
  out <- physical
  if (!is.finite(out) || out < 1L) out <- logical
  if (!is.finite(out) || out < 1L) out <- 1L
  as.integer(out)
}

extract_worker_count <- function(result) {
  candidates <- list(
    result$resource_plan$effective_workers,
    result$performance$resource$effective_workers,
    result$reproducibility$resolved_config$resources$effective$effective_workers
  )
  for (value in candidates) {
    value <- suppressWarnings(as.integer(value))
    if (length(value) == 1L && is.finite(value)) return(value)
  }
  NA_integer_
}

extract_fallback_count <- function(record_value, count_value = NULL) {
  if (is.data.frame(record_value)) return(nrow(record_value))
  value <- suppressWarnings(as.integer(record_value))
  if (length(value) == 1L && is.finite(value)) return(value)
  value <- suppressWarnings(as.integer(count_value))
  if (length(value) == 1L && is.finite(value)) return(value)
  NA_integer_
}

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- script_args[grepl("^--file=", script_args)]
start_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = FALSE)) else getwd()
repo_root <- find_repo_root(start_dir)
setwd(repo_root)

# Keep parallel workers from multiplying BLAS/OpenMP threads.
Sys.setenv(
  OMP_NUM_THREADS = env_or_default("OMP_NUM_THREADS", "1"),
  OPENBLAS_NUM_THREADS = env_or_default("OPENBLAS_NUM_THREADS", "1"),
  MKL_NUM_THREADS = env_or_default("MKL_NUM_THREADS", "1"),
  VECLIB_MAXIMUM_THREADS = env_or_default("VECLIB_MAXIMUM_THREADS", "1"),
  NUMEXPR_NUM_THREADS = env_or_default("NUMEXPR_NUM_THREADS", "1")
)

set.seed(20260919L)

if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(SEMANTICA)
}

UNSLOTH_ORIGIN <- strip_v1(env_or_default(
  "SEMANTICA_UNSLOTH_BASE_URL",
  env_or_default("UNSLOTH_BASE_URL", "http://localhost:8888")
))
CHAT_MODEL_ID <- env_or_default("SEMANTICA_CHAT_MODEL", "local-model")
EMBED_MODEL_ID <- env_or_default("SEMANTICA_EMBED_MODEL", "nomic-embed-text")
NOMIC_EMBED_DIM <- positive_int_env("NOMIC_EMBED_DIM", 384L)

cores <- detected_cores()
default_workers <- min(6L, max(1L, cores - 1L))
if (cores >= 4L) default_workers <- max(2L, default_workers)
workers <- positive_int_env("SEMANTICA_SMOKE_WORKERS", default_workers)
memory_aware <- flag_env("SEMANTICA_SMOKE_MEMORY_AWARE", FALSE)

pool_per_factor <- positive_int_env("SEMANTICA_SMOKE_POOL", 9L)
selected_per_factor <- positive_int_env("SEMANTICA_SMOKE_SELECTED", 3L)
ants <- positive_int_env("SEMANTICA_SMOKE_ANTS", 24L)
max_iter <- positive_int_env("SEMANTICA_SMOKE_MAX_ITER", 14L)
search_patience <- positive_int_env("SEMANTICA_SMOKE_PATIENCE", 8L)
max_esem_fits <- positive_int_env("SEMANTICA_SMOKE_ESEM_FITS", 24L)
dfi_reps <- positive_int_env("SEMANTICA_SMOKE_DFI_REPS", 80L)
dfi_search_reps <- positive_int_env("SEMANTICA_SMOKE_DFI_SEARCH_REPS", 40L)
temperature <- number_env("SEMANTICA_SMOKE_TEMPERATURE", 0.60, min = 0)
preflight <- flag_env("SEMANTICA_SMOKE_PREFLIGHT", FALSE)

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
  note = "Nomic Embed Text: use the clustering prefix for symmetric psychometric similarity."
)

general_rules <- paste(
  "Generate workplace behavior items for a compact validation smoke test.",
  "Every item must be a short first-person present-tense statement.",
  "Avoid double-barreled wording, trait labels, negations, clinical wording, idioms, and moralizing language.",
  "Keep each item primarily about its assigned factor and facet.",
  "Do not write reverse-keyed items in this run.",
  sep = "\n"
)

smoke_factors <- list(
  Focus = list(
    description = "Sustained attention, task persistence, and resistance to avoidable distraction.",
    forbidden = c("social warmth", "creative novelty", "emotional reassurance"),
    facets = list(
      Attention = list(description = "Maintains attention on the current task."),
      Persistence = list(description = "Continues working through ordinary difficulty.")
    )
  ),
  Collaboration = list(
    description = "Cooperative work with others through listening, coordination, and constructive help.",
    forbidden = c("private concentration", "novel idea generation", "impulsive change"),
    facets = list(
      Listening = list(description = "Takes in other people's views before responding."),
      Support = list(description = "Offers practical help that moves shared work forward.")
    )
  ),
  Adaptability = list(
    description = "Flexible adjustment to changing priorities, feedback, and uncertain work conditions.",
    forbidden = c("social popularity", "perfectionism", "routine persistence"),
    facets = list(
      Flexibility = list(description = "Adjusts plans when conditions change."),
      Learning = list(description = "Uses feedback and new information to improve.")
    )
  )
)

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path("semantica_runs", paste0("parallel_smoke_", run_id))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nSEMANTICA parallel smoke test\n")
cat("Repo root      :", normalizePath(repo_root, winslash = "/", mustWork = FALSE), "\n")
cat("Output dir     :", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
cat("Chat backend   : unsloth @", UNSLOTH_ORIGIN, "\n")
cat("Chat model     :", CHAT_MODEL_ID, "\n")
cat("Embed model    :", EMBED_MODEL_ID, "(", NOMIC_EMBED_DIM, "dim )\n")
cat("Workers request:", workers, "| memory_aware:", memory_aware, "\n")
cat("ACO            :", ants, "ants,", max_iter, "max iterations,", max_esem_fits, "ESEM search fits\n")
cat("DFI            :", dfi_reps, "reps,", dfi_search_reps, "search reps\n\n")

started_at <- Sys.time()

result <- semantica_run_custom(
  scale_name = "SEMANTICA Parallel Smoke Test",
  scale_description = paste(
    "Compact three-factor workplace-behavior scale for testing the current SEMANTICA development version.",
    general_rules,
    sep = "\n\n"
  ),
  factors = smoke_factors,

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
    embed_batch_size = positive_int_env("SEMANTICA_SMOKE_EMBED_BATCH", 32L),
    embedding_cache = TRUE,
    embedding_cache_dir = "semantica_embedding_cache",
    embedding_cache_namespace = paste("parallel-smoke", EMBED_MODEL_ID, NOMIC_EMBED_DIM, sep = "_"),
    retry_max_tries = positive_int_env("SEMANTICA_SMOKE_RETRIES", 3L),
    retry_on_failure = TRUE,
    timeout_s = positive_int_env("SEMANTICA_SMOKE_TIMEOUT", 180L),
    embed_timeout_s = positive_int_env("SEMANTICA_SMOKE_EMBED_TIMEOUT", 180L),
    preflight = preflight
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
    item_style = "first-person present-tense workplace-behavior statement",
    overgenerate = number_env("SEMANTICA_SMOKE_OVERGENERATE", 1.35, min = 1),
    max_retries = positive_int_env("SEMANTICA_SMOKE_GENERATION_RETRIES", 2L),
    global_forbidden_max = 30L,
    temperature = temperature,
    structured_output = "numbered"
  ),

  resources = semantica_resource_config(
    cpu_cores = workers,
    reserve_cpu_cores = 1L,
    max_cpu_cores = workers,
    memory_aware = memory_aware
  ),

  compute = semantica_compute_config(
    cosine_adjustment = "mean_center",
    compute_cosine_sensitivity = TRUE,
    cosine_sensitivity_max_items = 300L,
    cosine_sensitivity_seed = 20260919L,
    compute_device = "cpu",
    gpu_precision = "double",
    retain_embeddings = FALSE
  ),

  quality = semantica_quality_config(
    profile = "standard",
    content_alignment_mode = "guard",
    polarity_action = "guard",
    semantic_objective_mode = "relative_conservative",
    within_target_method = "nonredundant_median",
    htmt_objective_role = "penalty",
    htmt_threshold = 0.90,
    facet_coverage_weight = 0.15,
    psychometric_guard_weight = 0.60,
    psychometric_guard_min_ave = 0.20,
    psychometric_guard_min_loading = 0.35,
    psychometric_guard_min_primary_ge_50 = 0.60,
    psychometric_guard_min_simple_structure = 0.60,
    psychometric_guard_min_dominance = 0.65,
    psychometric_guard_max_cross_loading = 0.40,
    psychometric_guard_include_htmt = TRUE,
    final_structure_repair = TRUE,
    final_structure_repair_max_swaps = 2L,
    final_structure_repair_candidates_per_factor = 3L,
    final_structure_repair_min_delta = 1e-6,
    threshold_mode = "adaptive_pool",
    adaptive_redundancy_quantile = 0.95,
    adaptive_duplicate_quantile = 0.99,
    polarity_screen = TRUE
  ),

  pfa = semantica_pfa_config(
    mode = "objective",
    weight = 0.25,
    failure_policy = "semantic_fallback",
    during_search = TRUE,
    every = 1L,
    extraction = "principal",
    final_extraction = "ml",
    rotation = "promax",
    min_loading = 0.35,
    min_margin = 0.08,
    max_abs_loading = 0.98,
    unit_diagnostics = TRUE
  ),

  esem = semantica_esem_config(
    proxy_reference_n = "auto",
    rotation = "geomin",
    rotation_args = list(geomin.epsilon = 0.50),
    score_mode = "structure_weighted",
    cadence_mode = "adaptive",
    esem_every = 2L,
    run_esem_during_search = TRUE,
    esem_weight = 0.55,
    esem_failure_policy = "semantic_fallback",
    fast_esem = TRUE,
    fast_esem_iter_max = 350L,
    full_esem_iter_max = 900L,
    esem_eval_top_k = 3L
  ),

  fit_calibration = semantica_fit_calibration_config(
    mode = "esem_parametric_dfi",
    reps = dfi_reps,
    esem_reps = dfi_reps,
    search_reps = dfi_search_reps,
    final_recalibrate = FALSE,
    strategy = "fixed",
    fallback_policy = "conservative",
    data_type = "continuous",
    target_loadings = 0.65,
    target_factor_cors = 0.25,
    loading_pattern = "varied",
    embed_reliability = 0.90,
    residual_inflation = 0.02,
    warmup_iters = 2L
  ),

  diagnostics = semantica_diagnostics_config(
    final_fit = "off",
    semantic_stability = FALSE,
    validation_planning = FALSE
  ),

  plots = semantica_plot_config(
    level = "none",
    save = FALSE,
    include_interactive = FALSE,
    progress = FALSE
  ),

  ants = ants,
  search_patience = search_patience,
  max_total_iter = max_iter,
  max_esem_fits = max_esem_fits,
  elite_k = 6L,
  archive_stable_window = 3L,
  structural_archive_stable_window = 2L,
  min_successful_pfa_checkpoints = 1L,
  min_successful_esem_checkpoints = 1L,
  seed = 20260919L,
  verbose = TRUE
)

finished_at <- Sys.time()
elapsed_seconds <- as.numeric(difftime(finished_at, started_at, units = "secs"))

selected_csv <- file.path(output_dir, "selected_items.csv")
if (!is.null(result$selected_items_detail)) {
  utils::write.csv(result$selected_items_detail, selected_csv, row.names = FALSE)
}

snapshot_path <- file.path(output_dir, "parallel_smoke_result_snapshot.rds")
saveRDS(
  list(
    result = result,
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = elapsed_seconds,
    output_dir = output_dir
  ),
  snapshot_path,
  compress = "xz"
)

bundle_path <- file.path(output_dir, "parallel_smoke_bundle.rds")
bundle_ok <- tryCatch({
  semantica_save_bundle(result, path = bundle_path, include_embeddings = FALSE)
  TRUE
}, error = function(e) {
  warning("Could not save SEMANTICA bundle: ", conditionMessage(e), call. = FALSE)
  FALSE
})

effective_workers <- extract_worker_count(result)
expected_items <- length(smoke_factors) * selected_per_factor
actual_items <- length(result$best_items %||% character())
fallbacks <- extract_fallback_count(
  result$evaluation_telemetry$search_esem_parallel_fallbacks,
  result$performance$resource$search_esem_parallel_fallbacks
)
final_fallbacks <- extract_fallback_count(
  result$evaluation_telemetry$final_parallel_fallbacks,
  result$performance$resource$final_parallel_fallbacks
)

checks <- c(
  selected_item_count = actual_items == expected_items,
  resource_config_memory_aware = identical(
    result$reproducibility$resolved_config$resources$requested$memory_aware,
    memory_aware
  ),
  pfa_ml_promax_config = identical(result$reproducibility$resolved_config$pfa$final_extraction, "ml") &&
    identical(result$reproducibility$resolved_config$pfa$rotation, "promax"),
  explicit_parallel_workers = !is.na(effective_workers) &&
    (workers <= 1L || effective_workers >= 2L)
)

cat("\nSmoke test summary\n")
cat("Elapsed minutes :", sprintf("%.2f", elapsed_seconds / 60), "\n")
cat("Selected items  :", actual_items, "/", expected_items, "\n")
cat("Effective workers:", effective_workers, "\n")
cat("Search fallbacks:", fallbacks, "\n")
cat("Final fallbacks :", final_fallbacks, "\n")
cat("Snapshot        :", normalizePath(snapshot_path, winslash = "/", mustWork = FALSE), "\n")
if (bundle_ok) cat("Bundle          :", normalizePath(bundle_path, winslash = "/", mustWork = FALSE), "\n")
if (file.exists(selected_csv)) cat("Selected CSV    :", normalizePath(selected_csv, winslash = "/", mustWork = FALSE), "\n")

if (!is.null(result$performance$timing)) {
  cat("\nTiming detail:\n")
  print(result$performance$timing)
}

cat("\nContract checks:\n")
print(checks)

if (any(!checks)) {
  failed <- names(checks)[!checks]
  stop("SEMANTICA smoke test failed contract check(s): ", paste(failed, collapse = ", "), call. = FALSE)
}

if (is.finite(fallbacks) && fallbacks > 0L) {
  warning("Search-time ESEM parallel dispatch fell back to serial at least once. Inspect result$evaluation_telemetry.", call. = FALSE)
}
if (is.finite(final_fallbacks) && final_fallbacks > 0L) {
  warning("Final parallel dispatch fell back to serial at least once. Inspect result$evaluation_telemetry.", call. = FALSE)
}

cat("\nParallel smoke test completed successfully.\n")
