# ============================================================================
# SEMANTICA main workflow entry point
# ============================================================================
# This file contains interface/configuration logic. The
# analytical engine remains semantica_run_custom(); semantica_run() delegates
# to it so the main and extended interfaces remain on the same pipeline.

.semantica_console_language <- function(console_language = c("English", "Spanish")) {
  match.arg(console_language, c("English", "Spanish"))
}

.semantica_console_text <- function(console_language, key, ...) {
  console_language <- .semantica_console_language(console_language)
  catalog <- list(
    English = list(
      run_unidimensional = paste0(
        "[SEMANTICA] Unidimensional model detected: using the established one-factor branch; ",
        "PFA partition recovery, HTMT, and between-factor separation are not applicable."
      ),
      run_starting = "[SEMANTICA] Starting %s: %d factor(s), %d retained candidates, ~%d raw-generation target, %d final items.",
      run_progress_mode = "[SEMANTICA] Progress mode is concise. Use progress = 'detailed' for component-level telemetry.",
      run_complete = "[SEMANTICA] Complete: %d final item(s) selected.",
      run_restarts = "[SEMANTICA] Optimizer restarts: %s (%d successful start(s)).",
      run_result_surface = "[SEMANTICA] Result surface: $scale | $items | $diagnostics | $plots | $provenance | $advanced",
      run_next = "[SEMANTICA] Next: summary(result) | plot(result) | semantica_save_bundle(result, path)",
      resource_plan_title = "<semantica_resource_plan>\n",
      resource_parallel_disabled = "%s (parallel disabled)",
      resource_requested = "  Requested workers   : %s\n",
      resource_available = "  Available to R      : %s\n",
      resource_coordinator = "  Coordinator budget  : %d\n",
      resource_reserved = "  Reserved (auto)     : %d\n",
      resource_effective = "  Effective workers   : %d\n",
      resource_physical_cap = "  Physical-core cap   : %d worker(s)\n",
      resource_memory_cap = "  Memory-aware cap    : %d (%s)\n",
      resource_auto_cap = "  Auto safety cap     : CPU allocation with coordinator/reserve accounting\n",
      resource_parallel_backend = "  Parallel backend    : %s\n",
      resource_blas_threads = "  Worker BLAS threads : %s\n",
      resource_measured = "measured",
      resource_not_applicable = "not applicable",
      custom_resource_plan = "\n[SEMANTICA] Resolved resource plan before expensive execution:\n",
      custom_cosine_device = "  Cosine device       : %s (lavaan remains on CPU)\n",
      custom_default_items = "[FULL PIPELINE] i.per.f not specified. Defaulting to 3 items per factor.",
      custom_step_generation = "\n[SEMANTICA] Step 1/3: LLM Generation & Semantic Embedding...\n",
      custom_pool_thresholds = "[SEMANTICA] Experimental pool-relative thresholds: redundancy %.3f | duplicate %.3f\n",
      custom_step_optimization = "\n[SEMANTICA] Step 2/3: ACO-ESEM Optimization...\n",
      custom_semantic_summary = "\n[SEMANTICA] Semantic Similarity Change Summary:\n",
      custom_semantic_within = "  Within-factor : %.4f -> %.4f | change = %+.4f",
      custom_semantic_percent = " (%+.2f%%)",
      custom_semantic_target = "  Within target : %.4f +/- %.4f | deviation %.4f -> %.4f | %s\n",
      custom_semantic_between = "  Between-factor: %.4f -> %.4f | change = %+.4f\n",
      custom_semantic_gap = "  Separation gap: %.4f -> %.4f | change = %+.4f\n",
      custom_semantic_composite = "  Composite index: %.4f -> %.4f | change = %+.4f\n",
      custom_semantic_interpretation = "  Interpretation: %s\n",
      custom_target_inside = "inside target band",
      custom_target_outside = "outside target band",
      custom_separation_improved = "Relative within-versus-between semantic separation improved%s.",
      custom_separation_weakened = "Relative within-versus-between semantic separation weakened%s.",
      custom_separation_unchanged = "Relative within-versus-between semantic separation was unchanged%s.",
      custom_cohesion_guard = "; within-factor cohesion remained %s",
      custom_separation_unavailable = "Relative separation could not be computed; inspect the available cohesion diagnostics.",
      custom_separation_insufficient = "Could not compute (insufficient items)",
      custom_facet_pfa_available = "\n[SEMANTICA] Facet/unit PFA: score=%.4f | recovery=%.4f | clarity=%.4f\n",
      custom_facet_pfa_unavailable = "\n[SEMANTICA] Facet/unit PFA %s: %s\n",
      custom_unit_skipped = "skipped",
      custom_unit_unavailable = "unavailable",
      custom_unit_insufficient = "insufficient facet/unit structure",
      custom_generation_provenance = "\n[SEMANTICA] Generation reproducibility/provenance:\n",
      custom_generation_seed = "  Generation master seed : %d\n",
      custom_backend_seed = "  Backend seed control   : %s (%s)\n",
      custom_seed_requested = "requested",
      custom_seed_uncontrolled = "not controlled",
      custom_unavailable = "unavailable",
      custom_seed_not_requested = "  Generation master seed : not requested\n",
      custom_seed_schedule = "  Seed schedule          : %d realized call(s)\n",
      custom_seed_ledger = "  Generation call ledger : %d call(s)\n",
      custom_exact_replay = "  Exact text replay      : not guaranteed by backend/runtime; reuse the realized item pool for exact downstream replay\n",
      custom_replay_fingerprint = "  Replay-plan fingerprint: %s\n",
      custom_pool_fingerprint = "  Item-pool fingerprint  : %s\n",
      custom_screening_state = "  Content-screening state: %s\n",
      custom_unknown = "unknown",
      custom_representation_summary = "\n[SEMANTICA] Representation robustness summary:\n",
      custom_evidence_state = "  Evidence state: %s\n",
      custom_evidence_dependency = "  Evidence dependency: semantic/PFA/ESEM/clustering conclusions share this embedding representation; agreement is dependent corroboration, not independent validation.\n",
      custom_common_direction = "  Common-direction concentration: %.4f (descriptive; no universal cutoff)\n",
      custom_cosine_sensitivity = "  Raw vs mean-centered cosine: offdiag r=%.3f | top %.1f%% pair J=%.3f | random ref=%.3f\n",
      custom_overlap_warning = "  Interpretation: WARNING -- strongest-pair membership is not more stable across preprocessing than its finite-pool random reference; replicate proxy conclusions under a justified alternative representation.\n",
      custom_sensitivity_note = "  Interpretation: sensitivity diagnostic only; SEMANTICA does not automatically choose a more favorable cosine preprocessing.\n",
      custom_step_plots = "\n[SEMANTICA] Step 3/3: Generating Diagnostic Plots...\n",
      custom_complete = "\n[SEMANTICA] Pipeline complete.\n"
    ),
    Spanish = list(
      run_unidimensional = paste0(
        "[SEMANTICA] Modelo unidimensional detectado: se usar\u00e1 la ruta establecida para un factor; ",
        "la recuperaci\u00f3n de particiones PFA, HTMT y la separaci\u00f3n entre factores no son aplicables."
      ),
      run_starting = "[SEMANTICA] Iniciando %s: %d factor(es), %d candidatos retenidos, objetivo aproximado de generaci\u00f3n bruta: %d, %d \u00edtems finales.",
      run_progress_mode = "[SEMANTICA] El modo de progreso es conciso. Use progress = 'detailed' para la telemetr\u00eda de componentes.",
      run_complete = "[SEMANTICA] Completado: %d \u00edtem(s) final(es) seleccionado(s).",
      run_restarts = "[SEMANTICA] Reinicios del optimizador: %s (%d inicio(s) exitoso(s)).",
      run_result_surface = "[SEMANTICA] Superficie de resultados: $scale | $items | $diagnostics | $plots | $provenance | $advanced",
      run_next = "[SEMANTICA] Siguiente: summary(result) | plot(result) | semantica_save_bundle(result, path)",
      resource_plan_title = "<semantica_resource_plan>\n",
      resource_parallel_disabled = "%s (paralelismo desactivado)",
      resource_requested = "  Trabajadores solicitados : %s\n",
      resource_available = "  Disponibles para R       : %s\n",
      resource_coordinator = "  Presupuesto del coordinador: %d\n",
      resource_reserved = "  Reserva (autom\u00e1tica)     : %d\n",
      resource_effective = "  Trabajadores efectivos   : %d\n",
      resource_physical_cap = "  L\u00edmite de n\u00facleos f\u00edsicos: %d trabajador(es)\n",
      resource_memory_cap = "  L\u00edmite seg\u00fan memoria     : %d (%s)\n",
      resource_auto_cap = "  L\u00edmite de seguridad autom\u00e1tico: asignaci\u00f3n de CPU con contabilidad de coordinador/reserva\n",
      resource_parallel_backend = "  Backend paralelo         : %s\n",
      resource_blas_threads = "  Hilos BLAS por trabajador: %s\n",
      resource_measured = "medido",
      resource_not_applicable = "no aplica",
      custom_resource_plan = "\n[SEMANTICA] Plan de recursos resuelto antes de la ejecuci\u00f3n costosa:\n",
      custom_cosine_device = "  Dispositivo de coseno : %s (lavaan permanece en la CPU)\n",
      custom_default_items = "[FULL PIPELINE] i.per.f no se especific\u00f3. Se usar\u00e1n 3 \u00edtems por factor.",
      custom_step_generation = "\n[SEMANTICA] Paso 1/3: Generaci\u00f3n con LLM y embeddings sem\u00e1nticos...\n",
      custom_pool_thresholds = "[SEMANTICA] Umbrales experimentales relativos al conjunto: redundancia %.3f | duplicado %.3f\n",
      custom_step_optimization = "\n[SEMANTICA] Paso 2/3: Optimizaci\u00f3n ACO-ESEM...\n",
      custom_semantic_summary = "\n[SEMANTICA] Resumen de cambios en la similitud sem\u00e1ntica:\n",
      custom_semantic_within = "  Intra-factor  : %.4f -> %.4f | cambio = %+.4f",
      custom_semantic_percent = " (%+.2f%%)",
      custom_semantic_target = "  Objetivo intra-factor : %.4f +/- %.4f | desviaci\u00f3n %.4f -> %.4f | %s\n",
      custom_semantic_between = "  Entre factores: %.4f -> %.4f | cambio = %+.4f\n",
      custom_semantic_gap = "  Brecha de separaci\u00f3n: %.4f -> %.4f | cambio = %+.4f\n",
      custom_semantic_composite = "  \u00cdndice compuesto: %.4f -> %.4f | cambio = %+.4f\n",
      custom_semantic_interpretation = "  Interpretaci\u00f3n: %s\n",
      custom_target_inside = "dentro del rango objetivo",
      custom_target_outside = "fuera del rango objetivo",
      custom_separation_improved = "La separaci\u00f3n sem\u00e1ntica relativa intra-factor frente a entre factores mejor\u00f3%s.",
      custom_separation_weakened = "La separaci\u00f3n sem\u00e1ntica relativa intra-factor frente a entre factores se debilit\u00f3%s.",
      custom_separation_unchanged = "La separaci\u00f3n sem\u00e1ntica relativa intra-factor frente a entre factores no cambi\u00f3%s.",
      custom_cohesion_guard = "; la cohesi\u00f3n intra-factor se mantuvo %s",
      custom_separation_unavailable = "No se pudo calcular la separaci\u00f3n relativa; revise los diagn\u00f3sticos de cohesi\u00f3n disponibles.",
      custom_separation_insufficient = "No se pudo calcular (\u00edtems insuficientes)",
      custom_facet_pfa_available = "\n[SEMANTICA] PFA de faceta/unidad: puntuaci\u00f3n=%.4f | recuperaci\u00f3n=%.4f | claridad=%.4f\n",
      custom_facet_pfa_unavailable = "\n[SEMANTICA] PFA de faceta/unidad %s: %s\n",
      custom_unit_skipped = "omitido",
      custom_unit_unavailable = "no disponible",
      custom_unit_insufficient = "estructura de faceta/unidad insuficiente",
      custom_generation_provenance = "\n[SEMANTICA] Reproducibilidad/procedencia de la generaci\u00f3n:\n",
      custom_generation_seed = "  Semilla maestra de generaci\u00f3n : %d\n",
      custom_backend_seed = "  Control de semilla del backend : %s (%s)\n",
      custom_seed_requested = "solicitado",
      custom_seed_uncontrolled = "sin control",
      custom_unavailable = "no disponible",
      custom_seed_not_requested = "  Semilla maestra de generaci\u00f3n : no solicitada\n",
      custom_seed_schedule = "  Calendario de semillas          : %d llamada(s) realizada(s)\n",
      custom_seed_ledger = "  Registro de llamadas de semillas: %d llamada(s)\n",
      custom_exact_replay = "  Reproducci\u00f3n exacta del texto   : no est\u00e1 garantizada por el backend/entorno; reutilice el conjunto de \u00edtems generado para una reproducci\u00f3n posterior exacta\n",
      custom_replay_fingerprint = "  Huella del plan de reproducci\u00f3n : %s\n",
      custom_pool_fingerprint = "  Huella del conjunto de \u00edtems    : %s\n",
      custom_screening_state = "  Estado del filtrado de contenido: %s\n",
      custom_unknown = "desconocido",
      custom_representation_summary = "\n[SEMANTICA] Resumen de robustez de la representaci\u00f3n:\n",
      custom_evidence_state = "  Estado de la evidencia: %s\n",
      custom_evidence_dependency = "  Dependencia de la evidencia: las conclusiones sem\u00e1nticas/PFA/ESEM/de agrupamiento comparten esta representaci\u00f3n de incrustaci\u00f3n; la concordancia es corroboraci\u00f3n dependiente, no validaci\u00f3n independiente.\n",
      custom_common_direction = "  Concentraci\u00f3n de direcci\u00f3n com\u00fan: %.4f (descriptiva; sin punto de corte universal)\n",
      custom_cosine_sensitivity = "  Coseno bruto frente a centrado en la media: r fuera de la diagonal=%.3f | J del %.1f%% de pares superiores=%.3f | referencia aleatoria=%.3f\n",
      custom_overlap_warning = "  Interpretaci\u00f3n: ADVERTENCIA -- la pertenencia de los pares m\u00e1s fuertes no es m\u00e1s estable entre preprocesamientos que su referencia aleatoria de conjunto finito; replique las conclusiones proxy con una representaci\u00f3n alternativa justificada.\n",
      custom_sensitivity_note = "  Interpretaci\u00f3n: solo diagn\u00f3stico de sensibilidad; SEMANTICA no selecciona autom\u00e1ticamente un preprocesamiento de coseno m\u00e1s favorable.\n",
      custom_step_plots = "\n[SEMANTICA] Paso 3/3: Generando gr\u00e1ficos diagn\u00f3sticos...\n",
      custom_complete = "\n[SEMANTICA] Pipeline completado.\n"
    )
  )
  template <- catalog[[console_language]][[key]]
  if (is.null(template)) {
    stop(sprintf("Unknown SEMANTICA console-text key: '%s'.", key), call. = FALSE)
  }
  values <- list(...)
  if (!length(values)) return(template)
  do.call(sprintf, c(list(template), values))
}

.semantica_run_normalize_factors <- function(factors) {
  if (!is.list(factors) || length(factors) < 1L) {
    stop("'factors' must be a non-empty named list.", call. = FALSE)
  }
  nms <- names(factors)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms))) || anyDuplicated(nms)) {
    stop("'factors' must have unique, non-empty names.", call. = FALSE)
  }

  out <- factors
  for (i in seq_along(out)) {
    x <- out[[i]]
    # semantica_run() accepts the shorthand Factor = "definition" while
    # semantica_run_custom() accepts the same factor-list contract.
    if (is.character(x) && length(x) == 1L && !is.na(x)) {
      x <- list(description = x)
    }
    if (!is.list(x)) {
      stop(sprintf(
        "Factor '%s' must be a description string or a named list containing at least 'description'.",
        nms[[i]]
      ), call. = FALSE)
    }
    description <- x$description %||% NULL
    if (is.null(description) || length(description) != 1L || is.na(description) ||
        !nzchar(trimws(as.character(description)))) {
      stop(sprintf(
        "Factor '%s' needs a non-empty substantive 'description' in semantica_run().",
        nms[[i]]
      ), call. = FALSE)
    }
    x$description <- as.character(description)
    out[[i]] <- x
  }

  out
}

.semantica_run_dimensionality <- function(factors) {
  if (length(factors) == 1L) "unidimensional" else "multidimensional"
}

# The easy interface always records an actual analysis seed.  This makes a
# default run replayable at the optimizer level without claiming that a remote
# LLM will reproduce byte-identical wording.
.semantica_run_resolve_seed <- function(seed) {
  automatic <- is.null(seed) || (is.character(seed) && length(seed) == 1L &&
    !is.na(seed) && identical(tolower(trimws(seed)), "auto"))
  if (automatic) {
    return(list(seed = sample.int(.Machine$integer.max, 1L), source = "auto_generated"))
  }
  list(
    seed = .semantica_assert_nonnegative_integer(seed, "seed"),
    source = "user_supplied"
  )
}

.semantica_run_restart_seeds <- function(seed, restarts, mode) {
  automatic <- is.null(restarts) || (is.character(restarts) && length(restarts) == 1L &&
    !is.na(restarts) && identical(tolower(trimws(restarts)), "auto"))
  n <- if (automatic) switch(mode, fast = 1L, standard = 3L, full = 5L) else {
    .semantica_assert_positive_integer(restarts, "restarts")
  }
  # A fixed stride makes all restart seeds deterministic children of the
  # recorded master seed, without consuming additional global RNG draws.
  seeds <- as.integer((as.double(seed) + 104729 * seq.int(0L, n - 1L)) %% .Machine$integer.max)
  list(seeds = unique(seeds), source = if (automatic) "mode_default" else "user_supplied")
}

.semantica_run_search_space_log10 <- function(pool_items, selected_items, n_factors) {
  selected <- if (length(selected_items) == 1L) rep(selected_items, n_factors) else selected_items
  sum(vapply(selected, function(n) lchoose(pool_items, n) / log(10), numeric(1L)))
}

.semantica_run_adapt_aco_budget <- function(aco_cfg, pool_items, selected_items, n_factors) {
  selected_reference <- if (length(selected_items) == 1L) {
    rep.int(as.integer(selected_items), n_factors)
  } else {
    as.integer(selected_items)
  }
  reference <- list(
    pool_items = as.integer(pool_items), selected_items = unname(selected_reference),
    n_factors = as.integer(n_factors)
  )
  if (isTRUE(aco_cfg$budget_adapted) && identical(aco_cfg$budget_reference, reference)) {
    return(aco_cfg)
  }
  log10_space <- .semantica_run_search_space_log10(pool_items, selected_reference, n_factors)
  # The documented preset remains the floor.  Larger declared form spaces get
  # bounded additional exploration unless the caller explicitly overrode that
  # control.  This is a budget policy, not a claim to exhaustive coverage.
  extra <- max(0L, ceiling(log10_space) - 4L)
  override <- aco_cfg$overrides %||% list()
  if (extra > 0L && !identical(aco_cfg$mode, "fast")) {
    if (!isTRUE(override$ants)) aco_cfg$ants <- as.integer(min(120L, aco_cfg$ants + 5L * extra))
    if (!isTRUE(override$search_patience)) aco_cfg$search_patience <- as.integer(min(80L, aco_cfg$search_patience + 3L * extra))
    if (!isTRUE(override$max_total_iter)) aco_cfg$max_total_iter <- as.integer(min(120L, aco_cfg$max_total_iter + 5L * extra))
  }
  aco_cfg$evaporation <- semantica_evaporation_config(
    mode = "adaptive", rho_start = 0.20, rho_end = 0.05, horizon = aco_cfg$max_total_iter
  )
  aco_cfg$search_space_log10 <- log10_space
  aco_cfg$budget_adaptation <- if (extra > 0L && !identical(aco_cfg$mode, "fast")) "bounded_space_scaled" else "preset_floor"
  aco_cfg$budget_adapted <- TRUE
  aco_cfg$budget_reference <- reference
  aco_cfg
}

.semantica_run_pool_slack <- function(selected_items) {
  as.integer(max(2L, ceiling(max(selected_items) / 2)))
}

.semantica_run_adapt_unidimensional_aco <- function(aco_cfg) {
  out <- aco_cfg
  # PFA factor-recovery/partition objectives are intrinsically comparative and
  # are not informative for a one-factor model. Keep the same ACO effort and
  # fixed ESEM evidence budget, but remove the inapplicable PFA search
  # component. The semantic objective uses target-centered robust cohesion
  # rather than between-factor discrimination.
  out$pfa_mode <- "off"
  out$pfa_during_search <- FALSE
  out$pfa_weight <- 0
  out$description <- switch(
    out$mode %||% "standard",
    fast = "One-factor target-centered semantic + ESEM-guided ACO; PFA partition recovery is not applicable.",
    full = "One-factor target-centered semantic + entropy-responsive ESEM from a 5-iteration base cadence with strict ESEM-parametric DFI; PFA partition recovery is not applicable.",
    "One-factor target-centered semantic + entropy-responsive ESEM from a 10-iteration base cadence; PFA partition recovery is not applicable."
  )
  out
}

.semantica_run_normalize_prompts <- function(prompts, factor_names) {
  empty <- list(global = NULL, by_factor = list())
  if (is.null(prompts)) return(empty)
  if (is.character(prompts) && length(prompts) == 1L && !is.na(prompts)) {
    txt <- trimws(prompts)
    return(list(global = if (nzchar(txt)) txt else NULL, by_factor = list()))
  }
  if (!is.list(prompts)) {
    stop("'prompts' must be NULL, one character string, or a named list with 'global' and/or 'by_factor'.", call. = FALSE)
  }
  if (length(prompts) > 0L && (is.null(names(prompts)) || any(!nzchar(names(prompts))))) {
    stop("'prompts' list entries must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(prompts), c("global", "by_factor"))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "Unknown 'prompts' field(s): %s. Use 'global' and/or 'by_factor'.",
      paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }

  global <- prompts$global %||% NULL
  if (!is.null(global)) {
    if (!is.character(global) || length(global) != 1L || is.na(global)) {
      stop("'prompts$global' must be one character string.", call. = FALSE)
    }
    global <- trimws(global)
    if (!nzchar(global)) global <- NULL
  }

  by_factor <- prompts$by_factor %||% list()
  if (is.character(by_factor) && !is.null(names(by_factor))) {
    by_factor <- as.list(by_factor)
  }
  if (!is.list(by_factor)) {
    stop("'prompts$by_factor' must be a named list or named character vector.", call. = FALSE)
  }
  if (length(by_factor) > 0L) {
    bnm <- names(by_factor)
    if (is.null(bnm) || anyNA(bnm) || any(!nzchar(bnm)) || anyDuplicated(bnm)) {
      stop("'prompts$by_factor' must have unique, non-empty factor names.", call. = FALSE)
    }
    bad <- setdiff(bnm, factor_names)
    if (length(bad) > 0L) {
      stop(sprintf(
        "'prompts$by_factor' contains unknown factor(s): %s.",
        paste(bad, collapse = ", ")
      ), call. = FALSE)
    }
    by_factor <- lapply(by_factor, function(x) {
      if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop("Each factor-specific prompt must be one character string.", call. = FALSE)
      }
      trimws(x)
    })
  }
  list(global = global, by_factor = by_factor)
}

.semantica_run_apply_prompts <- function(factors, prompt_cfg) {
  for (nm in names(factors)) {
    inherited <- factors[[nm]]$extra_instructions %||% NULL
    pieces <- c(inherited, prompt_cfg$global, prompt_cfg$by_factor[[nm]] %||% NULL)
    pieces <- as.character(pieces)
    pieces <- pieces[!is.na(pieces) & nzchar(trimws(pieces))]
    if (length(pieces) > 0L) {
      # Append rather than replace: built-in prompt contracts, factor-specific
      # instructions and user additions remain simultaneously active.
      factors[[nm]]$extra_instructions <- paste(unique(pieces), collapse = "\n")
    }
  }
  factors
}

.semantica_aco_profile_values <- function(mode) {
  mode <- match.arg(mode, c("fast", "standard", "full"))

  # Literature-informed anchors:
  # * ShortForm uses 20 ants as a general default (Raborn & Leite, 2018).
  # * Olaru et al. recommend a sufficiently large colony (often 10-100) and
  #   illustrate 60 ants with 40 no-improvement iterations for psychometric ACO.
  # * Larger recent psychometric applications increase iteration budgets when
  #   the candidate space grows.
  #
  # SEMANTICA keeps one adaptive evaporation policy across presets so changing
  # evidence depth does not silently confound the comparison with a different
  # pheromone model. rho=.20 -> .05 corresponds to retention .80 -> .95, a
  # range spanning published psychometric ACO practice while moving from early
  # exploration toward later exploitation.
  switch(
    mode,
    fast = list(
      mode = "fast",
      ants = 20L,
      search_patience = 20L,
      max_total_iter = 40L,
      evaporation = semantica_evaporation_config(
        mode = "adaptive", rho_start = 0.20, rho_end = 0.05, horizon = 40L
      ),
      elite_k = 10L,
      archive_stable_window = 8L,
      structural_archive_stable_window = 2L,
      min_successful_pfa_checkpoints = 2L,
      min_successful_esem_checkpoints = 2L,
      esem_eval_top_k = 3L,
      esem_every = 10L,
      esem_cadence_mode = "fixed",
      pfa_mode = "diagnostic",
      pfa_during_search = FALSE,
      pfa_every = 5L,
      fit_calibration_mode = "fast",
      pfa_weight = 0.20,
      esem_weight = 0.30,
      description = "Semantic + ESEM-guided ACO; PFA remains a final diagnostic but does not guide search."
    ),
    standard = list(
      mode = "standard",
      ants = 60L,
      search_patience = 40L,
      max_total_iter = 60L,
      evaporation = semantica_evaporation_config(
        mode = "adaptive", rho_start = 0.20, rho_end = 0.05, horizon = 60L
      ),
      elite_k = 10L,
      archive_stable_window = 8L,
      structural_archive_stable_window = 2L,
      min_successful_pfa_checkpoints = 2L,
      min_successful_esem_checkpoints = 2L,
      esem_eval_top_k = 3L,
      esem_every = 10L,
      esem_cadence_mode = "adaptive",
      pfa_mode = "objective",
      pfa_during_search = TRUE,
      pfa_every = 5L,
      fit_calibration_mode = "fast",
      pfa_weight = 0.20,
      esem_weight = 0.30,
      description = "Semantic + PFA-guided ACO every 5 iterations + entropy-responsive ESEM from a 10-iteration base cadence."
    ),
    full = list(
      mode = "full",
      ants = 60L,
      search_patience = 40L,
      max_total_iter = 80L,
      evaporation = semantica_evaporation_config(
        mode = "adaptive", rho_start = 0.20, rho_end = 0.05, horizon = 80L
      ),
      elite_k = 10L,
      archive_stable_window = 8L,
      structural_archive_stable_window = 2L,
      min_successful_pfa_checkpoints = 2L,
      min_successful_esem_checkpoints = 2L,
      esem_eval_top_k = 3L,
      esem_every = 5L,
      esem_cadence_mode = "adaptive",
      pfa_mode = "objective",
      pfa_during_search = TRUE,
      pfa_every = 5L,
      fit_calibration_mode = "strict",
      pfa_weight = 0.20,
      esem_weight = 0.30,
      description = "Semantic + PFA-guided ACO + entropy-responsive ESEM from a 5-iteration base cadence with strict ESEM-parametric DFI calibration."
    )
  )
}

#' Configure an ACO search preset for semantica_run()
#'
#' `semantica_aco_config()` defines the `fast`, `standard`, and `full` search
#' presets used by [semantica_run()]. Additional ACO controls are available in
#' [semantica_run_custom()] and [ACO_with_ESEM()].
#'
#' @param mode One of `"fast"`, `"standard"`, or `"full"`. `"fast"` uses
#'   semantic and ESEM evidence during ACO while retaining PFA as a final
#'   diagnostic. `"standard"` adds objective-mode PFA every 5 iterations and
#'   entropy-responsive ESEM from a 10-iteration base cadence. `"full"` evaluates PFA and entropy-responsive ESEM from a 5-iteration base cadence and
#'   enables strict ESEM-parametric DFI calibration.
#' @param ants Optional positive integer colony-size override.
#' @param search_patience Optional positive integer non-improvement patience
#'   override. This remains independent from pheromone evaporation.
#' @param max_total_iter Optional positive integer hard iteration ceiling.
#'
#' @details
#' No single ACO parameter vector is uniformly optimal across combinatorial
#' problems. The standard preset is therefore literature-informed rather than
#' presented as a universal optimum. It anchors the colony at 60 ants and a
#' 40-iteration no-improvement patience, consistent with psychometric ACO
#' tutorial practice, while bounding total work at 60 iterations. The fast
#' preset uses the 20-ant scale common in the ShortForm implementation. The
#' full preset retains the same 60-ant base but extends the hard ceiling because
#' structural/DFI evidence is evaluated more frequently. Standard and full
#' adapt checkpoint timing to pheromone entropy; all presets use the
#' same adaptive .20-to-.05 evaporation-rate schedule (80% to 95% pheromone
#' retention) so evidence depth is not confounded with a different pheromone
#' model.
#'
#' @return A `semantica_aco_config` object for [semantica_run()].
#'
#' @references
#' Leite, W. L., Huang, I.-C., & Marcoulides, G. A. (2008). Item Selection for
#' the Development of Short Forms of Scales Using an Ant Colony Optimization
#' Algorithm. Multivariate Behavioral Research, 43(3), 411-431.
#' \doi{10.1080/00273170802285743}
#'
#' Olaru, G., Schroeders, U., Hartung, J., & Wilhelm, O. (2019). Ant Colony
#' Optimization and Local Weighted Structural Equation Modeling: A Tutorial on
#' Novel Item and Person Sampling Procedures for Personality Research.
#'
#' Raborn, A. W., & Leite, W. L. (2018). ShortForm: An R Package to Select Scale
#' Short Forms With the Ant Colony Optimization Algorithm. Applied Psychological
#' Measurement, 42(6), 516-517. \doi{10.1177/0146621617752993}
#'
#' @export
semantica_aco_config <- function(
    mode = c("standard", "fast", "full"),
    ants = NULL,
    search_patience = NULL,
    max_total_iter = NULL) {
  mode <- match.arg(mode)
  out <- .semantica_aco_profile_values(mode)
  out$overrides <- list(
    ants = !is.null(ants),
    search_patience = !is.null(search_patience),
    max_total_iter = !is.null(max_total_iter)
  )
  if (!is.null(ants)) {
    out$ants <- .semantica_assert_positive_integer(ants, "ants")
  }
  if (!is.null(search_patience)) {
    out$search_patience <- .semantica_assert_positive_integer(search_patience, "search_patience")
  }
  if (!is.null(max_total_iter)) {
    out$max_total_iter <- .semantica_assert_positive_integer(max_total_iter, "max_total_iter")
  }
  # The adaptive horizon follows a user-overridden hard ceiling, but never the
  # stopping patience. This preserves the 0.3.x optimizer-integrity contract.
  out$evaporation <- semantica_evaporation_config(
    mode = "adaptive", rho_start = 0.20, rho_end = 0.05,
    horizon = out$max_total_iter
  )
  class(out) <- c("semantica_aco_config", "list")
  out
}

.semantica_run_resolve_aco <- function(aco) {
  if (is.character(aco) && length(aco) == 1L) {
    return(semantica_aco_config(aco))
  }
  if (inherits(aco, "semantica_aco_config")) return(aco)
  if (is.list(aco)) {
    mode <- aco$mode %||% "standard"
    allowed <- c("mode", "ants", "search_patience", "max_total_iter")
    unknown <- setdiff(names(aco), allowed)
    if (length(unknown) > 0L) {
      stop(sprintf(
        "Unknown ACO override(s): %s. For additional ACO controls use semantica_run_custom().",
        paste(unknown, collapse = ", ")
      ), call. = FALSE)
    }
    return(semantica_aco_config(
      mode = mode,
      ants = aco$ants %||% NULL,
      search_patience = aco$search_patience %||% NULL,
      max_total_iter = aco$max_total_iter %||% NULL
    ))
  }
  stop("'aco' must be 'fast', 'standard', 'full', a semantica_aco_config(), or a small named override list.", call. = FALSE)
}

.semantica_run_resolve_llm <- function(llm, chat_model = NULL, embed_model = NULL) {
  embedded_chat <- NULL
  embedded_embed <- NULL
  if (is.character(llm) && length(llm) == 1L) {
    cfg <- semantica_llm_config(backend = llm)
  } else if (inherits(llm, "semantica_llm_config")) {
    cfg <- llm
  } else if (is.list(llm)) {
    if (length(llm) > 0L && (is.null(names(llm)) || any(!nzchar(names(llm))))) {
      stop("A list supplied to 'llm' must be fully named.", call. = FALSE)
    }
    embedded_chat <- llm$chat_model %||% NULL
    embedded_embed <- llm$embed_model %||% NULL
    llm$chat_model <- NULL
    llm$embed_model <- NULL
    allowed <- names(formals(semantica_llm_config))
    unknown <- setdiff(names(llm), allowed)
    if (length(unknown) > 0L) {
      stop(sprintf(
        "Unknown LLM field(s): %s. Use semantica_llm_config() for additional provider controls.",
        paste(unknown, collapse = ", ")
      ), call. = FALSE)
    }
    cfg <- do.call(semantica_llm_config, llm)
  } else {
    stop("'llm' must be a backend string, semantica_llm_config(), or named list.", call. = FALSE)
  }

  if (is.null(chat_model)) chat_model <- embedded_chat
  if (is.null(embed_model)) embed_model <- embedded_embed
  list(config = cfg, chat_model = chat_model, embed_model = embed_model)
}

.semantica_run_backend_contract <- function(llm_cfg, resolved, structured_output) {
  generation_backend <- llm_cfg$backend
  embedding_backend <- llm_cfg$embed_backend %||% generation_backend
  generation_spec <- llm_cfg$backend_spec %||% SEMANTICA_BACKENDS[[generation_backend]]
  embedding_spec <- llm_cfg$embed_backend_spec %||% SEMANTICA_BACKENDS[[embedding_backend]] %||% generation_spec
  generation_caps <- .semantica_backend_capabilities(generation_spec)
  embedding_caps <- .semantica_backend_capabilities(embedding_spec)
  if (!isTRUE(generation_caps$can_chat)) {
    stop(sprintf("Generation backend '%s' is not chat-capable.", generation_backend), call. = FALSE)
  }
  if (!isTRUE(embedding_caps$can_embed)) {
    stop(sprintf("Embedding backend '%s' is not embedding-capable.", embedding_backend), call. = FALSE)
  }
  list(
    generation_backend = generation_backend,
    embedding_backend = embedding_backend,
    chat_model = resolved$chat_model %||% generation_spec$default_chat_model %||% NA_character_,
    embed_model = resolved$embed_model %||% embedding_spec$default_embed_model %||% NA_character_,
    output_mode = if (identical(structured_output, "auto")) {
      if (isTRUE(generation_caps$supports_structured_output)) "json" else "numbered"
    } else structured_output,
    structured_output_declared = isTRUE(generation_caps$supports_structured_output),
    language_status = "prompt_requested_not_runtime_verified",
    model_revision_status = "recorded_if_provider_exposes_revision"
  )
}

#' Run a SEMANTICA scale-development workflow
#'
#' `semantica_run()` is the main scale-development interface. It uses the same
#' analysis pipeline as [semantica_run_custom()] with the settings most often
#' needed for a standard run.
#'
#' @param scale_name Non-empty scale name.
#' @param scale_description Substantive description of the overall construct.
#' @param factors Named factor list. Each factor may be either a single
#'   description string or the richer list accepted by
#'   [semantica_run_custom()] (for example `description`, `facets`,
#'   `forbidden`, and `extra_instructions`).
#' @param pool_items Positive integer retained candidate items per factor.
#' @param selected_items Positive integer or factor-specific vector of final
#'   selected items per factor. `NULL` uses a dimensionality-aware default:
#'   4 items for a one-factor model and 3 items per factor otherwise. A
#'   one-factor run requires at least 4 selected items so its one-factor ESEM
#'   proxy is overidentified rather than the vacuously perfect 3-item saturated
#'   model. [semantica_run_custom()] remains available for shorter
#'   forms whose global one-factor fit cannot be assessed.
#' @param overgenerate Positive raw-generation multiplier. `semantica_run()`
#'   defaults to `2`, matching the established full-pipeline default. The larger
#'   raw pool is deterministically curated for lexical diversity before the
#'   retained target is finalized; deficit-aware retries request only the
#'   remaining yield when deduplication leaves too few usable items.
#' @param prompts Optional prompt augmentation. Supply one character string to
#'   append it to every factor, or a list with `global` and/or `by_factor`.
#'   These additions are appended to SEMANTICA's internal prompt contracts; they
#'   do not replace formatting, duplicate-control, or construct instructions.
#' @param aco ACO preset: `"fast"`, `"standard"` (default), or `"full"`;
#'   alternatively use [semantica_aco_config()] or a small named list containing
#'   `mode`, `ants`, `search_patience`, and/or `max_total_iter`.
#' @param llm Generation/provider specification. It may be a backend string such
#'   as `"ollama"`, a [semantica_llm_config()] object, or a named list. Such
#'   named lists may additionally contain `chat_model` and `embed_model`.
#' @param chat_model,embed_model Optional explicit model names. These override
#'   model names nested in an `llm` list.
#' @param seed Non-negative integer seed, or `"auto"` (default) to generate and
#'   record one. `NULL` is treated as `"auto"`.
#' @param restarts `"auto"` (default) uses one frozen-pool optimizer start for
#'   `"fast"`, three for `"standard"`, and five for `"full"`; alternatively a
#'   positive integer requests that many starts. Multiple starts are compared
#'   only when their recorded evidence regimes match.
#' @param verbose Logical compatibility switch. `FALSE` forces quiet execution.
#'   With the default `TRUE`, `progress` chooses concise progress or detailed
#'   component-level messages for troubleshooting.
#' @param workers Parallel worker request for `semantica_run()`. Use `"auto"`
#'   (default) for SEMANTICA's adaptive PSOCK policy, which separately budgets
#'   the main R coordinator and user/OS headroom and applies physical-core and
#'   memory safety caps when detectable; use a positive integer for an explicit
#'   worker ceiling, or `"serial"` for one-process execution.
#'   Explicit numeric requests remain user-authoritative subject to the CPU
#'   allocation visible to R.
#' @param language Language requested for generated questionnaire items.
#' @param response_format Response-scale format stated in the existing item
#'   generation prompt contract, for example `"5-point Likert"` or
#'   `"7-point Likert"`.
#' @param item_style Wording style stated in the existing item-generation prompt
#'   contract.
#' @param temperature Non-negative generation sampling temperature. The easy
#'   interface defaults to `0.4` to prioritize instruction consistency; supply
#'   a higher value deliberately when more wording variation is needed.
#' @param structured_output Generation response contract: `"auto"`,
#'   `"numbered"`, or `"json"`.
#' @param progress User-facing progress level: `"normal"` (concise high-level
#'   stages), `"detailed"` (existing component telemetry), or `"quiet"`.
#' @param console_language Language for console feedback: `"English"` (the
#'   default) or `"Spanish"`. This does not change the language of generated
#'   questionnaire items; use `language` for that setting.
#'
#' @details
#' The three ACO modes alter search effort/evidence cadence without weakening
#' SEMANTICA's integrity safeguards:
#'
#' * `fast`: semantic + ESEM search; PFA stays enabled for final diagnostics.
#' * `standard`: semantic + objective PFA every 5 iterations + entropy-responsive ESEM from a 10-iteration base cadence.
#' * `full`: semantic + objective PFA and entropy-responsive ESEM from a 5-iteration base cadence, with strict
#'   ESEM-parametric DFI calibration.
#'
#' In multidimensional models, final PFA uses ML extraction with oblimin rotation;
#' ESEM uses automatic proxy reference N, geomin rotation, and structure-weighted
#' scoring. When exactly one factor is supplied, `semantica_run()` automatically
#' switches to a unidimensional analysis branch: PFA partition-recovery guidance
#' and HTMT-like separation are marked not applicable, ESEM uses no rotation, and
#' the selected one-factor proxy is summarized with loading, residual-dependence,
#' and eigenvalue-dominance diagnostics. The automatic final-form default becomes
#' 4 items because a three-indicator one-factor covariance model has zero degrees
#' of freedom and therefore cannot provide an informative global-fit check.
#' PFA/ESEM failures use explicit semantic
#' fallback where applicable so unavailable evidence is never silently treated as
#' zero-quality evidence.
#'
#' @references
#' Clark, L. A., & Watson, D. (1995). Constructing validity: Basic issues in
#' objective scale development. *Psychological Assessment, 7*(3), 309-319.
#' \doi{10.1037/1040-3590.7.3.309}
#'
#' Huber, P. J. (1964). Robust estimation of a location parameter. *The Annals
#' of Mathematical Statistics, 35*(1), 73-101. \doi{10.1214/aoms/1177703732}
#'
#' Slocum-Gori, S. L., & Zumbo, B. D. (2011). Assessing the
#' unidimensionality of psychological scales: Using multiple criteria from factor
#' analysis. *Social Indicators Research, 102*, 443-461.
#' \doi{10.1007/s11205-010-9682-8}
#'
#' Cook, K. F., Kallen, M. A., & Amtmann, D. (2009). Having a fit: Impact of
#' number of items and distribution of data on traditional criteria for assessing
#' IRT's unidimensionality assumption. *Quality of Life Research, 18*, 447-460.
#' \doi{10.1007/s11136-009-9464-4}
#'
#' Christensen, K. B., Makransky, G., & Horton, M. (2017). Critical values for
#' Yen's Q3: Identification of local dependence in the Rasch model using residual
#' correlations. *Applied Psychological Measurement, 41*, 178-194.
#' \doi{10.1177/0146621616677520}
#'
#' @return A compact `semantica_run_result` facade with six top-level groups:
#'   `scale`, `items`, `diagnostics`, `plots`, `provenance`, and `advanced`. The
#'   `advanced` field contains the complete canonical
#'   `semantica_full_pipeline_result` (including `run_config`) unchanged. Legacy
#'   direct field access such as `result$optimization` continues to resolve to
#'   the canonical result for compatibility.
#'
#' @examples
#' \dontrun{
#' result <- semantica_run(
#'   scale_name = "Emotion Regulation Scale",
#'   scale_description = "A measure of related emotion-regulation dimensions.",
#'   factors = list(
#'     Awareness = "Noticing and recognizing one's emotional state.",
#'     Clarity = "Understanding and differentiating one's emotional state."
#'   ),
#'   llm = list(
#'     backend = "ollama",
#'     chat_model = "llama3.1:8b",
#'     embed_model = "nomic-embed-text"
#'   ),
#'   aco = "standard",
#'   seed = 20260825L
#' )
#' }
#'
#' @export
semantica_run <- function(
    scale_name,
    scale_description,
    factors,
    pool_items = 15L,
    selected_items = NULL,
    overgenerate = 2,
    prompts = NULL,
    aco = c("standard", "fast", "full"),
    llm = "openai",
    chat_model = NULL,
    embed_model = NULL,
    seed = "auto",
    restarts = "auto",
    verbose = TRUE,
    workers = "auto",
    language = "English",
    response_format = "5-point Likert",
    item_style = "first-person declarative sentence",
    temperature = 0.4,
    structured_output = c("auto", "numbered", "json"),
    progress = c("normal", "detailed", "quiet"),
    console_language = c("English", "Spanish")) {

  if (!is.character(scale_name) || length(scale_name) != 1L || is.na(scale_name) || !nzchar(trimws(scale_name))) {
    stop("'scale_name' must be one non-empty character string.", call. = FALSE)
  }
  if (!is.character(scale_description) || length(scale_description) != 1L || is.na(scale_description) || !nzchar(trimws(scale_description))) {
    stop("'scale_description' must be one non-empty character string.", call. = FALSE)
  }
  verbose <- .semantica_assert_flag(verbose, "verbose")
  progress <- match.arg(progress)
  console_language <- .semantica_console_language(console_language)
  if (!isTRUE(verbose)) progress <- "quiet"
  pipeline_verbose <- identical(progress, "detailed")
  pool_items <- .semantica_assert_positive_integer(pool_items, "pool_items")
  overgenerate <- .semantica_assert_positive_scalar(overgenerate, "overgenerate")
  temperature <- .semantica_assert_nonnegative_scalar(temperature, "temperature")
  structured_output <- match.arg(structured_output)
  for (nm in c("language", "response_format", "item_style")) {
    value <- get(nm, inherits = FALSE)
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop(sprintf("'%s' must be one non-empty character string.", nm), call. = FALSE)
    }
  }

  factors <- .semantica_run_normalize_factors(factors)
  dimensionality <- .semantica_run_dimensionality(factors)
  is_unidimensional <- identical(dimensionality, "unidimensional")
  selected_items_auto <- is.null(selected_items)
  if (selected_items_auto) {
    # Three indicators identify a one-factor covariance model but leave zero
    # degrees of freedom, making global fit perfect by construction. The main
    # interface therefore uses four items for a testable one-factor proxy while
    # retaining the historical three-per-factor default for multidimensional use.
    selected_items <- if (is_unidimensional) 4L else 3L
  }
  selected_items <- .semantica_assert_positive_integer_vector(selected_items, "selected_items")
  if (is_unidimensional) {
    if (length(selected_items) != 1L) {
      stop("A unidimensional semantica_run() requires one scalar 'selected_items' value.", call. = FALSE)
    }
    if (selected_items < 4L) {
      stop(paste(
        "A unidimensional SEMANTICA run requires at least 4 selected items.",
        "With 3 indicators a one-factor covariance model has zero degrees of freedom,",
        "so CFI/RMSEA/SRMR fit can be perfect by construction rather than evidence of unidimensionality.",
        "Use selected_items >= 4, or semantica_run_custom() when you need a shorter form and accept that global one-factor fit cannot be assessed."
      ), call. = FALSE)
    }
  }
  if (any(selected_items > pool_items)) {
    stop("'selected_items' cannot exceed 'pool_items' in semantica_run().", call. = FALSE)
  }
  prompt_cfg <- .semantica_run_normalize_prompts(prompts, names(factors))
  factors_for_run <- .semantica_run_apply_prompts(factors, prompt_cfg)

  if (length(aco) > 1L && is.character(aco)) aco <- aco[[1L]]
  aco_cfg <- .semantica_run_resolve_aco(aco)
  if (is_unidimensional) {
    aco_cfg <- .semantica_run_adapt_unidimensional_aco(aco_cfg)
    if (!identical(progress, "quiet")) {
      message(.semantica_console_text(console_language, "run_unidimensional"))
    }
  }
  aco_cfg <- .semantica_run_adapt_aco_budget(
    aco_cfg, pool_items, selected_items, length(factors)
  )
  seed_cfg <- .semantica_run_resolve_seed(seed)
  restart_cfg <- .semantica_run_restart_seeds(seed_cfg$seed, restarts, aco_cfg$mode)
  llm_resolved <- .semantica_run_resolve_llm(llm, chat_model, embed_model)
  backend_contract <- .semantica_run_backend_contract(
    llm_resolved$config, llm_resolved, structured_output
  )

  item_cfg <- semantica_item_count_config(
    pool = pool_items,
    selected = selected_items,
    override_pool_counts = TRUE
  )
  generation_cfg <- semantica_generation_config(
    response_format = trimws(response_format),
    item_style = trimws(item_style),
    language = trimws(language),
    overgenerate = overgenerate,
    temperature = temperature,
    structured_output = structured_output,
    pool_topup_min_slack = .semantica_run_pool_slack(selected_items)
  )
  resource_cfg <- semantica_resource_config(cpu_cores = workers)

  if (identical(progress, "normal")) {
    n_factors <- length(factors)
    retained_total <- as.integer(pool_items * n_factors)
    raw_target <- as.integer(ceiling(pool_items * overgenerate) * n_factors)
    selected_total <- if (length(selected_items) == 1L) as.integer(selected_items * n_factors) else as.integer(sum(selected_items))
    message(.semantica_console_text(
      console_language, "run_starting",
      trimws(scale_name), n_factors, retained_total, raw_target, selected_total
    ))
    message(.semantica_console_text(console_language, "run_progress_mode"))
  }

  pfa_cfg <- semantica_pfa_config(
    mode = aco_cfg$pfa_mode,
    weight = aco_cfg$pfa_weight,
    failure_policy = "semantic_fallback",
    during_search = aco_cfg$pfa_during_search,
    every = 5L,
    extraction = "ml",
    final_extraction = "ml",
    rotation = if (is_unidimensional) "none" else "oblimin",
    unit_diagnostics = !is_unidimensional
  )
  esem_cfg <- semantica_esem_config(
    proxy_reference_n = "auto",
    cadence_mode = aco_cfg$esem_cadence_mode %||% "fixed",
    # Rotation is indeterminate/irrelevant for a single factor. Avoid imposing
    # geomin on a one-axis solution while preserving the multidimensional default.
    rotation = if (is_unidimensional) "none" else "geomin",
    rotation_args = if (is_unidimensional) list() else list(geomin.epsilon = 0.50),
    score_mode = "structure_weighted"
  )
  fit_cfg <- semantica_fit_calibration_config(
    mode = aco_cfg$fit_calibration_mode,
    strategy = if (identical(aco_cfg$mode, "full")) "adaptive" else "fixed",
    esem_reps = if (identical(aco_cfg$mode, "full")) 500L else NULL,
    adaptive_min_reps = if (identical(aco_cfg$mode, "full")) 250L else NULL,
    adaptive_batch_reps = if (identical(aco_cfg$mode, "full")) 100L else 50L,
    adaptive_tol = if (identical(aco_cfg$mode, "full")) 0.001 else 0.002
  )
  # The main interface is intended to produce a viable candidate scale from
  # theory definitions without requiring expert configuration.  Use the
  # existing conservative contrastive-definition guard. Strict recovery excludes
  # robust factor mismatches/exclusion conflicts; if the factor pool remains
  # infeasible, the logged top-up ladder may relax forbidden conflicts while
  # retaining robust construct mismatch and polarity. The quality-config default remains
  # diagnostic for backward compatibility and method-development workflows.
  quality_cfg <- semantica_quality_config(
    content_alignment_mode = "guard",
    semantic_objective_mode = "relative_conservative"
  )

  result <- semantica_run_custom(
    scale_name = trimws(scale_name),
    scale_description = trimws(scale_description),
    factors = factors_for_run,
    llm = llm_resolved$config,
    chat_model = llm_resolved$chat_model,
    embed_model = llm_resolved$embed_model,
    item_counts = item_cfg,
    generation = generation_cfg,
    resources = resource_cfg,
    ants = aco_cfg$ants,
    search_patience = aco_cfg$search_patience,
    max_total_iter = aco_cfg$max_total_iter,
    evaporation = aco_cfg$evaporation,
    max_esem_fits = NULL,
    elite_k = aco_cfg$elite_k,
    archive_stable_window = aco_cfg$archive_stable_window,
    structural_archive_stable_window = aco_cfg$structural_archive_stable_window,
    min_successful_pfa_checkpoints = aco_cfg$min_successful_pfa_checkpoints,
    min_successful_esem_checkpoints = aco_cfg$min_successful_esem_checkpoints,
    esem_eval_top_k = aco_cfg$esem_eval_top_k,
    esem_every = aco_cfg$esem_every,
    run_esem_during_search = TRUE,
    esem_weight = aco_cfg$esem_weight,
    esem_failure_policy = "semantic_fallback",
    esem = esem_cfg,
    pfa = pfa_cfg,
    fit_calibration = fit_cfg,
    quality = quality_cfg,
    seed = seed_cfg$seed,
    restart_seeds = restart_cfg$seeds,
    verbose = pipeline_verbose,
    console_language = console_language
  )

  run_meta <- list(
    interface = "semantica_run",
    interface_schema = "semantica-run-v4",
    dimensionality = dimensionality,
    unidimensional_adaptation = is_unidimensional,
    unidimensional_semantic_objective = if (is_unidimensional) "huber_target_centered" else NA_character_,
    aco_mode = aco_cfg$mode,
    aco_description = aco_cfg$description,
    semantic_objective_mode = quality_cfg$semantic_objective_mode,
    content_alignment_mode = quality_cfg$content_alignment_mode,
    content_alignment_role = "conservative_feasibility_guard",
    ants = aco_cfg$ants,
    search_patience = aco_cfg$search_patience,
    max_total_iter = aco_cfg$max_total_iter,
    search_space_log10 = aco_cfg$search_space_log10,
    budget_adaptation = aco_cfg$budget_adaptation,
    seed = seed_cfg$seed,
    seed_source = seed_cfg$source,
    restart_seeds = restart_cfg$seeds,
    restart_source = restart_cfg$source,
    restart_status = result$multi_seed_stability$status %||% "unavailable",
    restart_selected_seed = result$multi_seed_stability$selected_seed %||% seed_cfg$seed,
    pfa_during_search = aco_cfg$pfa_during_search,
    pfa_every = if (is_unidimensional) NA_integer_ else 5L,
    pfa_extraction = if (is_unidimensional) NA_character_ else "ml",
    pfa_rotation = if (is_unidimensional) NA_character_ else "oblimin",
    pfa_status = if (is_unidimensional) "not_applicable_unidimensional" else aco_cfg$pfa_mode,
    esem_every = aco_cfg$esem_every,
    esem_cadence_mode = aco_cfg$esem_cadence_mode %||% "fixed",
    esem_proxy_reference_n = "auto",
    esem_rotation = if (is_unidimensional) "none" else "geomin",
    esem_score_mode = "structure_weighted",
    dfi_calibration_mode = aco_cfg$fit_calibration_mode,
    pool_items = pool_items,
    pool_topup_min_slack = generation_cfg$pool_topup_min_slack,
    selected_items = selected_items,
    selected_items_auto = selected_items_auto,
    selected_items_default_rule = if (selected_items_auto) {
      if (is_unidimensional) "4_for_overidentified_one_factor_proxy" else "3_per_factor"
    } else "user_supplied",
    workers_requested = resource_cfg$cpu_cores,
    workers_effective = result$reproducibility$effective_workers %||%
      result$optimization$resource$effective_workers %||%
      result$resource$effective_workers %||% NA_integer_,
    worker_policy = if (identical(resource_cfg$cpu_cores, "auto")) "adaptive_auto_psock_v2" else "explicit_or_serial",
    worker_reproducibility = "deterministic_task_seeds_with_serial_fallback_when_supported",
    overgenerate = overgenerate,
    language = generation_cfg$language,
    response_format = generation_cfg$response_format,
    item_style = generation_cfg$item_style,
    temperature = generation_cfg$temperature,
    structured_output = generation_cfg$structured_output,
    backend_contract = backend_contract,
    progress = progress,
    console_language = console_language,
    generation_provenance_schema = result$generation_provenance$schema %||% NA_character_,
    generation_seed_controlled = result$generation_provenance$generation_seed_controlled %||% FALSE,
    generation_seed_mechanism = result$generation_provenance$generation_seed_mechanism %||% NA_character_,
    exact_text_replay_guaranteed = result$generation_provenance$exact_text_replay_guaranteed %||% FALSE,
    generation_spec_fingerprint = result$generation_provenance$generation_spec_fingerprint %||%
      result$generation_provenance$generation_contract_fingerprint %||% NA_character_,
    generation_replay_plan_fingerprint = result$generation_provenance$generation_replay_plan_fingerprint %||% NA_character_,
    item_pool_fingerprint = result$generation_provenance$item_pool_fingerprint %||% NA_character_,
    prompt_customization = list(
      global = !is.null(prompt_cfg$global),
      factors = names(prompt_cfg$by_factor)
    )
  )
  result$run_config <- run_meta
  if (is.null(result$reproducibility) || !is.list(result$reproducibility)) {
    result$reproducibility <- list()
  }
  result$reproducibility$run_interface <- run_meta
  if (identical(progress, "normal")) {
    selected_n <- length(result$best_items %||% result$optimization$best_items %||% character(0L))
    message(.semantica_console_text(console_language, "run_complete", selected_n))
    restart_status <- result$multi_seed_stability$status %||% "unavailable"
    if (length(restart_cfg$seeds) > 1L) {
      message(.semantica_console_text(
        console_language, "run_restarts", restart_status,
        length(result$multi_seed_stability$successful_seeds %||% integer(0L))
      ))
    }
    message(.semantica_console_text(console_language, "run_result_surface"))
    message(.semantica_console_text(console_language, "run_next"))
  }
  .semantica_wrap_run_result(result)
}
