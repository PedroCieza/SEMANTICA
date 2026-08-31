# Analysis extensions and calibration utilities for SEMANTICA
#
# These helpers intentionally distinguish sample-free semantic-proxy evidence
# from participant-based psychometric evidence. Experimental functions are
# labelled as such in their returned metadata and documentation.

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

.semantica_safe_names <- function(x, prefix = "item") {
  x <- as.character(x)
  bad <- is.na(x) | !nzchar(trimws(x))
  if (any(bad)) x[bad] <- paste0(prefix, which(bad))
  make.unique(x)
}

.semantica_object_md5 <- function(x) {
  tf <- tempfile("semantica-hash-")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf, version = 3L, compress = FALSE)
  unname(tools::md5sum(tf)[[1L]])
}

.semantica_text_cache_key <- function(text, session, normalize = TRUE,
                                      cache_namespace = NULL) {
  policy <- if (exists(".semantica_embedding_policy", mode = "function")) {
    .semantica_embedding_policy(
      session$embed_model %||% "unknown",
      session$embedding_task %||% "auto",
      session$embedding_instruction %||% NULL,
      session$embedding_spec %||% NULL
    )
  } else list(resolved_task = session$embedding_task %||% "auto", prefix = session$embedding_instruction %||% NULL)
  prepared_text <- if (exists(".semantica_prepare_embedding_texts", mode = "function")) {
    unname(.semantica_prepare_embedding_texts(session, as.character(text))[[1L]])
  } else as.character(text)
  endpoint_identity <- session$embed_url %||% session$base_url %||% session$endpoint %||% NULL
  endpoint_fingerprint <- if (is.null(endpoint_identity) || length(endpoint_identity) == 0L ||
                                 is.na(endpoint_identity[[1L]]) || !nzchar(endpoint_identity[[1L]])) {
    NA_character_
  } else {
    endpoint_safe <- if (exists(".semantica_sanitize_url", mode = "function")) {
      .semantica_sanitize_url(as.character(endpoint_identity[[1L]]))
    } else {
      sub("[?#].*$", "", as.character(endpoint_identity[[1L]]), perl = TRUE)
    }
    .semantica_object_md5(list(endpoint = enc2utf8(endpoint_safe), schema = 1L))
  }
  payload <- list(
    text = enc2utf8(prepared_text),
    backend = session$backend %||% "unknown",
    protocol = session$protocol %||% "unknown",
    endpoint_fingerprint = endpoint_fingerprint,
    model = session$embed_model %||% "unknown",
    model_revision = session$model_revision %||% NA_character_,
    model_precision = session$model_precision %||% NA_character_,
    normalize = isTRUE(normalize),
    embedding_task = session$embedding_task %||% "auto",
    embedding_task_resolved = policy$resolved_task %||% NA_character_,
    embedding_instruction = session$embedding_instruction %||% NA_character_,
    embedding_prefix_resolved = policy$prefix %||% NA_character_,
    embedding_capability_fingerprint = policy$capability_fingerprint %||% NA_character_,
    namespace = cache_namespace %||% "default",
    cache_schema = 7L
  )
  .semantica_object_md5(payload)
}

.semantica_default_cache_dir <- function() {
  tryCatch(
    tools::R_user_dir("SEMANTICA", which = "cache"),
    error = function(e) file.path(path.expand("~"), ".cache", "SEMANTICA")
  )
}

.semantica_cache_path <- function(key, cache_dir) {
  file.path(cache_dir, substr(key, 1L, 2L), paste0(key, ".rds"))
}

.semantica_embedding_cache_get <- function(key, cache_dir) {
  path <- .semantica_cache_path(key, cache_dir)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

.semantica_embedding_cache_set <- function(key, value, cache_dir) {
  path <- .semantica_cache_path(key, cache_dir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp-"), tmpdir = dirname(path))
  saveRDS(value, tmp, version = 3L, compress = "xz")
  ok <- file.rename(tmp, path)
  if (!ok) {
    unlink(tmp)
    warning("Could not atomically write SEMANTICA embedding cache entry.", call. = FALSE)
  }
  invisible(path)
}

#' Configure persistent embedding caching
#'
#' Content-addressed caching can eliminate repeated embedding/API work while
#' preserving the exact vectors used in a previous run. Cache keys include the
#' backend, a credential-free hash of the embedding endpoint when available,
#' embedding-model identifier, optional model revision, normalization policy,
#' and item text. Endpoint identity prevents otherwise compatible servers using
#' the same model label from sharing persistent cache entries. Remote providers
#' may still change a model behind a stable endpoint/model identifier; set
#' `cache = FALSE` when the latest provider behavior is required.
#'
#' @param cache Logical; use the persistent embedding cache.
#' @param cache_dir Cache directory. `NULL` uses an OS-appropriate user cache.
#' @param cache_namespace Optional analyst-defined namespace used in cache keys.
#' @return A named list suitable for `semantica_llm_config()`.
#' @export
semantica_embedding_cache_config <- function(cache = TRUE, cache_dir = NULL,
                                              cache_namespace = NULL) {
  cache <- .semantica_assert_flag(cache, "cache")
  list(
    cache = cache,
    cache_dir = cache_dir %||% .semantica_default_cache_dir(),
    cache_namespace = cache_namespace
  )
}

#' Calibrate semantic thresholds to an observed embedding pool
#'
#' Produces model/pool-relative heuristic thresholds rather than assuming that a
#' fixed cosine value has invariant meaning across embedding models. This is an
#' experimental calibration aid, not an empirically validated psychometric
#' cutoff generator.
#'
#' @param cosine_sim_matrix Square cosine-similarity matrix.
#' @param factor_assignment Optional named factor assignment.
#' @param redundancy_quantile Quantile of within-factor similarities used as a
#'   model-relative redundancy alert.
#' @param duplicate_quantile More extreme within-factor quantile for duplicate
#'   screening.
#' @param min_redundancy,max_redundancy Safety bounds.
#' @return A `semantica_threshold_calibration` object.
#' @export
semantica_calibrate_similarity_thresholds <- function(
  cosine_sim_matrix,
  factor_assignment = NULL,
  redundancy_quantile = 0.95,
  duplicate_quantile = 0.99,
  min_redundancy = 0.70,
  max_redundancy = 0.95
) {
  m <- as.matrix(cosine_sim_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L) {
    stop("'cosine_sim_matrix' must be a square matrix with at least two items.")
  }
  if (is.null(rownames(m))) rownames(m) <- colnames(m) <- paste0("item", seq_len(nrow(m)))
  if (is.null(colnames(m))) colnames(m) <- rownames(m)
  m <- (m + t(m)) / 2
  diag(m) <- NA_real_
  all_pairs <- m[upper.tri(m)]
  all_pairs <- all_pairs[is.finite(all_pairs)]
  if (!length(all_pairs)) stop("No finite off-diagonal similarities were found.")

  within <- numeric(0L)
  between <- numeric(0L)
  target <- NULL
  target_source <- NULL
  within_by_factor <- NULL
  if (!is.null(factor_assignment)) {
    fa <- as.character(factor_assignment)
    names(fa) <- names(factor_assignment)
    common <- intersect(names(fa), rownames(m))
    fa <- fa[common]
    f_names <- unique(fa[!is.na(fa) & nzchar(fa)])
    target <- stats::setNames(rep(NA_real_, length(f_names)), f_names)
    target_source <- stats::setNames(rep(NA_character_, length(f_names)), f_names)
    within_by_factor <- stats::setNames(vector("list", length(f_names)), f_names)
    for (f in f_names) {
      ids <- names(fa)[fa == f]
      if (length(ids) >= 2L) {
        block <- m[ids, ids, drop = FALSE]
        vals <- block[lower.tri(block)]
        vals <- vals[is.finite(vals)]
        within_by_factor[[f]] <- vals
        within <- c(within, vals)
      }
    }
    if (length(f_names) >= 2L) {
      for (i in seq_len(length(f_names) - 1L)) {
        for (j in (i + 1L):length(f_names)) {
          a <- names(fa)[fa == f_names[[i]]]
          b <- names(fa)[fa == f_names[[j]]]
          if (length(a) && length(b)) between <- c(between, as.vector(m[a, b, drop = FALSE]))
        }
      }
      between <- between[is.finite(between)]
    }
  }
  reference <- if (length(within) >= 4L) within else all_pairs
  rq <- stats::quantile(reference, redundancy_quantile, na.rm = TRUE, names = FALSE)
  dq <- stats::quantile(reference, duplicate_quantile, na.rm = TRUE, names = FALSE)
  redundancy <- min(max(as.numeric(rq), min_redundancy), max_redundancy)
  duplicate <- min(max(as.numeric(dq), redundancy), 1)

  # Derive cohesion targets from the same calibrated pool rather than imposing
  # the former universal .70 ceiling. For each factor, the target is the
  # typical within-factor relation still below the calibrated redundancy
  # boundary. If a factor has no such pair, the redundancy boundary itself is
  # the only pool-supported upper reference; keep the target infinitesimally
  # inside it rather than inventing another model-independent constant.
  if (!is.null(target)) {
    for (f in names(target)) {
      vals <- within_by_factor[[f]] %||% numeric(0L)
      vals <- vals[is.finite(vals)]
      nonredundant <- vals[vals < redundancy]
      if (length(nonredundant) >= 2L) {
        target[f] <- stats::median(nonredundant)
        target_source[f] <- "median_nonredundant_within_pool"
      } else if (length(nonredundant) == 1L) {
        target[f] <- nonredundant[1L]
        target_source[f] <- "single_nonredundant_within_pair"
      } else if (length(vals)) {
        target[f] <- min(stats::median(vals), redundancy - sqrt(.Machine$double.eps))
        target_source[f] <- "redundancy_boundary_fallback"
      }
    }
    missing_target <- !is.finite(target)
    if (any(missing_target)) {
      available_target <- target[is.finite(target)]
      fallback_target <- if (length(available_target)) {
        stats::median(available_target)
      } else {
        redundancy - sqrt(.Machine$double.eps)
      }
      target[missing_target] <- fallback_target
      target_source[missing_target] <- if (length(available_target)) {
        "borrowed_pool_median"
      } else {
        "redundancy_boundary_fallback"
      }
    }
    target <- pmin(1, pmax(-1, target))
  }

  out <- list(
    redundancy_threshold = redundancy,
    duplicate_threshold = duplicate,
    within_similarity_target = target,
    within_similarity_target_method = if (is.null(target)) NULL else "nonredundant_median",
    within_similarity_target_source = target_source,
    reference = if (length(within) >= 4L) "within_factor_pool" else "all_off_diagonal_pairs",
    redundancy_quantile = redundancy_quantile,
    duplicate_quantile = duplicate_quantile,
    distribution = list(
      n_all = length(all_pairs),
      n_within = length(within),
      n_between = length(between),
      all_median = stats::median(all_pairs),
      all_mad = stats::mad(all_pairs),
      within_median = if (length(within)) stats::median(within) else NA_real_,
      between_median = if (length(between)) stats::median(between) else NA_real_
    ),
    status = "experimental_pool_relative_heuristic",
    warning = paste(
      "Pool-relative thresholds are heuristics, not validated psychometric cutoffs.",
      "Benchmark them against expert and participant data before treating them as decision thresholds."
    )
  )
  class(out) <- c("semantica_threshold_calibration", "list")
  out
}

#' Define a construct blueprint
#'
#' A blueprint makes content representation explicit so semantic optimization
#' does not reduce scale development to item-to-item coherence. Facets can be
#' provided inside `factors` or through `required_facets`; `expected_relations`
#' can encode theoretically expected factor overlap.
#'
#' @param factors Named construct/factor specification list.
#' @param required_facets Optional named list of required facets per factor.
#' @param exclusions Optional named list of construct-irrelevant or neighboring
#'   concepts that should be monitored.
#' @param expected_relations Optional square numeric matrix with factor names.
#' @return A `semantica_construct_blueprint`.
#' @export
semantica_construct_blueprint <- function(factors, required_facets = NULL,
                                           exclusions = NULL,
                                           expected_relations = NULL) {
  if (!is.list(factors) || is.null(names(factors)) || any(!nzchar(names(factors)))) {
    stop("'factors' must be a named list.")
  }
  factor_names <- names(factors)
  inferred <- stats::setNames(vector("list", length(factors)), factor_names)
  for (f in factor_names) {
    spec <- factors[[f]]
    facets <- character(0L)
    if (is.list(spec$facets)) facets <- names(spec$facets) %||% character(0L)
    if (is.list(spec$dimensions)) facets <- unique(c(facets, names(spec$dimensions) %||% character(0L)))
    inferred[[f]] <- facets[nzchar(facets)]
  }
  if (!is.null(required_facets)) {
    if (!is.list(required_facets)) stop("'required_facets' must be a named list.")
    for (f in intersect(names(required_facets), factor_names)) {
      inferred[[f]] <- unique(as.character(required_facets[[f]]))
    }
  }
  inferred_exclusions <- stats::setNames(vector("list", length(factor_names)), factor_names)
  for (f in factor_names) {
    spec <- factors[[f]]
    vals <- spec$forbidden %||% spec$exclusions %||% character(0L)
    inferred_exclusions[[f]] <- unique(as.character(vals))
    inferred_exclusions[[f]] <- inferred_exclusions[[f]][!is.na(inferred_exclusions[[f]]) & nzchar(inferred_exclusions[[f]])]
  }
  if (!is.null(exclusions)) {
    if (!is.list(exclusions)) stop("'exclusions' must be a named list.")
    for (f in intersect(names(exclusions), factor_names)) {
      inferred_exclusions[[f]] <- unique(as.character(exclusions[[f]]))
    }
  }

  if (!is.null(expected_relations)) {
    er <- as.matrix(expected_relations)
    if (nrow(er) != length(factor_names) || ncol(er) != length(factor_names)) {
      stop("'expected_relations' must be square with one row/column per factor.")
    }
    if (is.null(rownames(er))) rownames(er) <- factor_names
    if (is.null(colnames(er))) colnames(er) <- factor_names
    if (!setequal(rownames(er), factor_names) || !setequal(colnames(er), factor_names)) {
      stop("'expected_relations' dimnames must match factor names.")
    }
    er <- er[factor_names, factor_names, drop = FALSE]
    diag(er) <- 1
    expected_relations <- er
  }
  out <- list(
    factor_names = factor_names,
    factor_definitions = stats::setNames(vapply(factors, function(x) as.character(x$description %||% ""), character(1L)), factor_names),
    required_facets = inferred,
    exclusions = inferred_exclusions,
    expected_relations = expected_relations,
    evidence_role = "content_representation_blueprint"
  )
  class(out) <- c("semantica_construct_blueprint", "list")
  out
}

#' Assess construct/facet coverage against a blueprint
#'
#' Content-screening helper that compares item metadata with a
#' construct blueprint. It does not create participant-based validity evidence.
#'
#' @param items_tbl Item metadata table.
#' @param blueprint A `semantica_construct_blueprint`.
#' @param cosine_sim_matrix Optional item cosine matrix used to compare observed
#'   between-factor semantic overlap with `expected_relations`.
#' @param id_col,factor_col,facet_col Metadata column names.
#' @return Coverage diagnostics. These are content-screening diagnostics, not
#'   participant-based validity evidence.
#' @export
semantica_assess_construct_coverage <- function(
  items_tbl,
  blueprint,
  cosine_sim_matrix = NULL,
  id_col = "item_id",
  factor_col = "factor",
  facet_col = "Facet"
) {
  if (!inherits(blueprint, "semantica_construct_blueprint")) {
    stop("'blueprint' must be created by semantica_construct_blueprint().")
  }
  x <- as.data.frame(items_tbl, stringsAsFactors = FALSE)
  if (!id_col %in% names(x) && "ID" %in% names(x)) id_col <- "ID"
  if (!factor_col %in% names(x) && "Dimension" %in% names(x)) factor_col <- "Dimension"
  if (!facet_col %in% names(x) && "facet" %in% names(x)) facet_col <- "facet"
  needed <- c(id_col, factor_col)
  if (length(setdiff(needed, names(x)))) stop("Item metadata lacks ID/factor columns.")
  if (!facet_col %in% names(x)) x[[facet_col]] <- NA_character_

  # Metadata coverage answers only whether declared facet labels are present.
  # If semantic facet diagnostics are available, semantic coverage accepts
  # aligned or ambiguous items and excludes only *clear* pool-relative facet
  # mismatches. This avoids treating tiny top-rank differences as proof that a
  # facet is absent. Older result objects fall back to the binary aligned flag.
  semantic_clear_column <- "semantica_facet_clear_mismatch" %in% names(x)
  semantic_aligned_column <- "semantica_facet_aligned" %in% names(x)
  semantic_column_available <- semantic_clear_column || semantic_aligned_column

  rows <- list(); missing <- list()
  for (f in blueprint$factor_names) {
    req <- unique(as.character(blueprint$required_facets[[f]] %||% character(0L)))
    req <- req[nzchar(req)]
    in_factor <- as.character(x[[factor_col]]) == f
    observed <- unique(as.character(x[[facet_col]][in_factor]))
    observed <- observed[!is.na(observed) & nzchar(observed)]
    semantic_factor_available <- if (semantic_clear_column) {
      any(in_factor & !is.na(x$semantica_facet_clear_mismatch))
    } else if (semantic_aligned_column) {
      any(in_factor & !is.na(x$semantica_facet_aligned))
    } else {
      FALSE
    }
    sem_observed <- observed
    if (semantic_factor_available) {
      ok_sem <- if (semantic_clear_column) {
        in_factor & !is.na(x$semantica_facet_clear_mismatch) &
          !as.logical(x$semantica_facet_clear_mismatch)
      } else {
        in_factor & !is.na(x$semantica_facet_aligned) &
          as.logical(x$semantica_facet_aligned)
      }
      sem_observed <- unique(as.character(x[[facet_col]][ok_sem]))
      sem_observed <- sem_observed[!is.na(sem_observed) & nzchar(sem_observed)]
    }
    if (!length(req)) {
      rows[[length(rows)+1L]] <- data.frame(
        factor=f, required_facets=0L, covered_facets=NA_integer_,
        metadata_coverage=NA_real_, semantic_covered_facets=NA_integer_,
        semantic_coverage=NA_real_, semantic_alignment_available=semantic_factor_available,
        coverage=NA_real_,
        missing_facets=NA_character_, semantic_missing_facets=NA_character_,
        stringsAsFactors=FALSE)
      missing[[f]] <- character(0L)
    } else {
      miss <- setdiff(req, observed)
      sem_miss <- if (semantic_factor_available) setdiff(req, sem_observed) else character(0L)
      metadata_cov <- 1 - length(miss)/length(req)
      semantic_cov <- if (semantic_factor_available) 1 - length(sem_miss)/length(req) else NA_real_
      rows[[length(rows)+1L]] <- data.frame(
        factor=f, required_facets=length(req), covered_facets=length(req)-length(miss),
        metadata_coverage=metadata_cov,
        semantic_covered_facets=if (semantic_factor_available) length(req)-length(sem_miss) else NA_integer_,
        semantic_coverage=semantic_cov, semantic_alignment_available=semantic_factor_available,
        coverage=if (semantic_factor_available) semantic_cov else metadata_cov,
        missing_facets=paste(miss,collapse='; '),
        semantic_missing_facets=if (semantic_factor_available) paste(sem_miss,collapse='; ') else NA_character_,
        stringsAsFactors=FALSE)
      missing[[f]] <- if (semantic_factor_available) sem_miss else miss
    }
  }
  coverage_table <- do.call(rbind, rows)
  finite_cov <- coverage_table$coverage[is.finite(coverage_table$coverage)]
  overall <- if (length(finite_cov)) mean(finite_cov) else NA_real_
  metadata_finite <- coverage_table$metadata_coverage[is.finite(coverage_table$metadata_coverage)]
  metadata_overall <- if (length(metadata_finite)) mean(metadata_finite) else NA_real_
  semantic_finite <- coverage_table$semantic_coverage[is.finite(coverage_table$semantic_coverage)]
  semantic_overall <- if (length(semantic_finite)) mean(semantic_finite) else NA_real_
  semantic_available <- any(coverage_table$semantic_alignment_available %in% TRUE)
  semantic_complete <- if (any(coverage_table$required_facets > 0L))
    all(coverage_table$semantic_alignment_available[coverage_table$required_facets > 0L] %in% TRUE) else FALSE

  relation_diagnostics <- NULL
  er <- blueprint$expected_relations
  if (!is.null(cosine_sim_matrix) && !is.null(er)) {
    m <- as.matrix(cosine_sim_matrix); ids <- as.character(x[[id_col]])
    fac <- as.character(x[[factor_col]]); names(fac) <- ids
    common <- intersect(ids, rownames(m)); fac <- fac[common]
    obs <- matrix(NA_real_, length(blueprint$factor_names), length(blueprint$factor_names),
                  dimnames=list(blueprint$factor_names, blueprint$factor_names))
    diag(obs) <- 1
    for (i in seq_along(blueprint$factor_names)) for (j in seq_along(blueprint$factor_names)) {
      if (i == j) next
      ai <- names(fac)[fac == blueprint$factor_names[i]]; bj <- names(fac)[fac == blueprint$factor_names[j]]
      if (length(ai) && length(bj)) obs[i,j] <- mean(m[ai,bj,drop=FALSE], na.rm=TRUE)
    }
    ok <- upper.tri(er) & is.finite(er) & is.finite(obs)
    residual <- if (any(ok)) obs[ok] - er[ok] else numeric(0L)
    mae <- if (length(residual)) mean(abs(residual), na.rm=TRUE) else NA_real_
    relation_diagnostics <- list(
      observed = obs, observed_semantic = obs, expected = er, residual = residual,
      mae = mae, mean_absolute_residual = mae,
      interpretation = "Semantic overlap compatibility with a theory-specified relation matrix; not an empirical factor-correlation test.",
      evidence_role = "research_track_semantic_domain_relation")
  }
  list(available=TRUE, overall_coverage=overall,
       overall_required_facet_coverage=overall,
       metadata_overall_coverage=metadata_overall,
       semantic_overall_coverage=semantic_overall,
       semantic_alignment_available=semantic_available,
       semantic_alignment_complete=semantic_complete,
       table=coverage_table, coverage_table=coverage_table,
       missing=missing, missing_facets=missing,
       relation_diagnostics=relation_diagnostics, expected_relation_diagnostics=relation_diagnostics,
       evidence_role=if (semantic_available) 'semantic_definition_content_screen' else 'metadata_content_screen',
       note=if (semantic_available)
         'coverage uses semantic item-to-facet definition alignment; metadata coverage is reported separately.'
       else 'semantic facet alignment was unavailable; coverage reflects declared metadata labels only.')
}

#' Detect wording polarity and explicit negation
#'
#' The built-in detector is deliberately conservative and rule-based. A custom
#' `relation_scorer` can add NLI/LLM judgments without making SEMANTICA depend on
#' a specific proprietary model.
#'
#' @param items Character vector or item table.
#' @param text_col Text column when `items` is a data frame.
#' @param relation_scorer Optional function accepting one text string and
#'   returning a named list/vector with fields such as `direction`, `confidence`,
#'   and `method`.
#' @param language Language code used by the conservative negation screen.
#'   `"auto"` searches the built-in English, Spanish, Portuguese, and French
#'   negation lexicons.
#' @return Data frame of polarity diagnostics.
#' @export
semantica_polarity_diagnostics <- function(items, text_col = "item_text",
                                            relation_scorer = NULL,
                                            language = c("auto", "en", "es", "pt", "fr")) {
  texts <- if (is.data.frame(items)) {
    if (!text_col %in% names(items) && "item" %in% names(items)) text_col <- "item"
    if (!text_col %in% names(items)) stop("Could not find item-text column.")
    as.character(items[[text_col]])
  } else as.character(items)
  language_raw <- tolower(trimws(as.character(language[[1L]])))
  language_aliases <- c(
    english = "en", spanish = "es", espanol = "es", "espa\u00f1ol" = "es",
    portuguese = "pt", portugues = "pt", "portugu\u00eas" = "pt",
    french = "fr", francais = "fr", "fran\u00e7ais" = "fr"
  )
  if (language_raw %in% names(language_aliases)) {
    language <- unname(language_aliases[[language_raw]])
  } else {
    language <- language_raw
  }
  if (!language %in% c("auto", "en", "es", "pt", "fr")) {
    warning(sprintf("Unsupported polarity-screen language '%s'; using the conservative multilingual 'auto' lexicon.", language_raw), call. = FALSE)
    language <- "auto"
  }
  lexicons <- list(
    en = c("no", "not", "never", "none", "neither", "nor", "without", "hardly", "rarely", "cannot", "can't", "don't", "doesn't", "didn't", "won't", "wouldn't", "isn't", "aren't", "wasn't", "weren't", "shouldn't", "couldn't", "mustn't"),
    es = c("no", "nunca", "jam\u00e1s", "jamas", "nadie", "ning\u00fan", "ningun", "ninguna", "sin", "tampoco"),
    pt = c("n\u00e3o", "nao", "nunca", "jamais", "ningu\u00e9m", "ninguem", "nenhum", "nenhuma", "sem", "tampouco"),
    fr = c("ne", "pas", "jamais", "aucun", "aucune", "personne", "sans", "ni")
  )
  terms <- if (language == "auto") unique(unlist(lexicons, use.names = FALSE)) else lexicons[[language]]
  esc <- terms  # Built-in lexicon terms contain no regex metacharacters.
  neg_pattern <- paste0("\\b(", paste(esc, collapse = "|"), ")\\b")
  explicit_neg <- grepl(neg_pattern, tolower(texts), perl = TRUE)
  out <- data.frame(
    item_index = seq_along(texts),
    item_text = texts,
    explicit_negation = explicit_neg,
    direction = ifelse(explicit_neg, "potentially_reversed_or_negated", "not_flagged"),
    confidence = ifelse(explicit_neg, 0.70, 0.30),
    method = "rule_based_negation_screen",
    stringsAsFactors = FALSE
  )
  if (is.function(relation_scorer)) {
    scored <- lapply(texts, function(txt) tryCatch(relation_scorer(txt), error = function(e) NULL))
    for (i in seq_along(scored)) {
      s <- scored[[i]]
      if (is.null(s)) next
      if (!is.null(s$direction)) out$direction[[i]] <- as.character(s$direction[[1L]])
      if (!is.null(s$confidence)) out$confidence[[i]] <- as.numeric(s$confidence[[1L]])
      out$method[[i]] <- as.character(s$method %||% "custom_relation_scorer")
    }
  }
  attr(out, "semantica_language") <- language
  attr(out, "semantica_note") <- paste(
    "Polarity is not inferable from cosine similarity alone.",
    "Rule-based flags are screening aids and do not determine empirical reverse",
    "scoring. Treat explicit analyst keying or a separately benchmarked",
    "directional/NLI scorer as authoritative input when direction matters."
  )
  out
}

#' Compare conclusions across semantic representations
#'
#' @param matrices Named list of compatible similarity matrices or SEMANTICA
#'   result objects containing a named `cosine_sim_matrix`.
#' @param selected_sets Optional named list of selected item IDs. When omitted
#'   and result objects are supplied, their `best_items` are used when present.
#' @param high_pair_quantile Quantile defining high-similarity pairs within each
#'   representation for stability analysis.
#' @param item_id_policy Alignment policy. `"exact"` (default) requires the
#'   same named item set in every representation and safely reorders matrices to
#'   a common canonical order. `"intersection"` preserves the legacy
#'   intersection behavior explicitly and warns when items are dropped.
#' @return A `semantica_semantic_robustness` object.
#' @export
semantica_semantic_robustness <- function(matrices, selected_sets = NULL,
                                           high_pair_quantile = 0.95,
                                           item_id_policy = c("exact", "intersection")) {
  if (!is.list(matrices) || length(matrices) < 2L) stop("Provide at least two representations.")
  item_id_policy <- match.arg(item_id_policy)
  if (is.null(names(matrices))) names(matrices) <- paste0("representation_", seq_along(matrices))

  extract_matrix <- function(x, label) {
    candidate <- if (is.matrix(x) || is.data.frame(x)) {
      x
    } else if (is.list(x)) {
      x$cosine_sim_matrix %||% x$generation$cosine_sim_matrix %||%
        x$optimization$cosine_sim_matrix %||% NULL
    } else NULL
    if (is.null(candidate)) stop(sprintf("Representation '%s' does not contain a similarity matrix.", label))
    m <- as.matrix(candidate)
    if (nrow(m) != ncol(m)) stop(sprintf("Representation '%s' must be square.", label))
    if (is.null(rownames(m)) || is.null(colnames(m))) stop(sprintf("Representation '%s' must have explicit row/column item IDs.", label))
    if (anyDuplicated(rownames(m)) || anyDuplicated(colnames(m))) stop(sprintf("Representation '%s' contains duplicated item IDs.", label))
    if (!setequal(rownames(m), colnames(m))) stop(sprintf("Representation '%s' row/column item IDs do not match.", label))
    m <- m[rownames(m), rownames(m), drop = FALSE]
    m
  }
  mats <- Map(extract_matrix, matrices, names(matrices))
  names(mats) <- names(matrices)
  id_sets <- lapply(mats, rownames)
  canonical <- id_sets[[1L]]
  same_sets <- vapply(id_sets[-1L], function(ids) setequal(ids, canonical), logical(1L))
  dropped <- list()
  if (item_id_policy == "exact") {
    if (!all(same_sets)) {
      bad <- names(mats)[c(FALSE, !same_sets)]
      stop(sprintf("Item-ID alignment mismatch across representations: %s. Use item_id_policy='intersection' only when deliberate.", paste(bad, collapse = ", ")))
    }
    common <- canonical
  } else {
    common <- Reduce(intersect, id_sets)
    if (length(common) < 3L) stop("Representations need at least three common named items.")
    for (nm in names(mats)) dropped[[nm]] <- setdiff(rownames(mats[[nm]]), common)
    if (any(lengths(dropped) > 0L)) {
      warning("Representations have non-identical item sets; comparison is restricted to their explicit intersection.", call. = FALSE)
    }
  }
  if (length(common) < 3L) stop("Representations need at least three aligned named items.")
  mats <- lapply(mats, function(m) m[common, common, drop = FALSE])

  # Result objects can carry their selected sets directly. This is only a
  # convenience layer; no consensus/averaged representation is constructed.
  if (is.null(selected_sets)) {
    derived <- lapply(matrices, function(x) if (is.list(x)) x$best_items %||% x$optimization$best_items %||% NULL else NULL)
    if (sum(!vapply(derived, is.null, logical(1L))) >= 2L) selected_sets <- derived
  }

  vecs <- lapply(mats, function(m) m[lower.tri(m)])
  agreement <- matrix(1, length(mats), length(mats), dimnames = list(names(mats), names(mats)))
  high_sets <- list()
  for (i in seq_along(mats)) {
    v <- vecs[[i]]
    thr <- stats::quantile(v, high_pair_quantile, na.rm = TRUE, names = FALSE)
    idx <- which(lower.tri(mats[[i]]) & mats[[i]] >= thr, arr.ind = TRUE)
    high_sets[[i]] <- if (nrow(idx)) {
      apply(idx, 1L, function(z) paste(sort(c(rownames(mats[[i]])[[z[[1L]]]], rownames(mats[[i]])[[z[[2L]]]])), collapse = "||"))
    } else character(0L)
  }
  names(high_sets) <- names(mats)
  pair_jaccard <- agreement
  for (i in seq_along(mats)) for (j in seq_along(mats)) {
    agreement[i, j] <- suppressWarnings(stats::cor(vecs[[i]], vecs[[j]], method = "spearman", use = "pairwise.complete.obs"))
    u <- union(high_sets[[i]], high_sets[[j]])
    pair_jaccard[i, j] <- if (!length(u)) 1 else length(intersect(high_sets[[i]], high_sets[[j]])) / length(u)
  }
  selection_jaccard <- NULL
  if (!is.null(selected_sets) && length(selected_sets) >= 2L) {
    selected_sets <- selected_sets[!vapply(selected_sets, is.null, logical(1L))]
    if (length(selected_sets) >= 2L) {
      if (is.null(names(selected_sets))) names(selected_sets) <- paste0("selection_", seq_along(selected_sets))
      selection_jaccard <- matrix(1, length(selected_sets), length(selected_sets),
                                  dimnames = list(names(selected_sets), names(selected_sets)))
      for (i in seq_along(selected_sets)) for (j in seq_along(selected_sets)) {
        u <- union(selected_sets[[i]], selected_sets[[j]])
        selection_jaccard[i, j] <- if (!length(u)) 1 else length(intersect(selected_sets[[i]], selected_sets[[j]])) / length(u)
      }
    }
  }
  out <- list(
    common_items = common,
    item_id_policy = item_id_policy,
    dropped_items = if (item_id_policy == "intersection") dropped else NULL,
    matrix_rank_agreement = agreement,
    high_similarity_pair_jaccard = pair_jaccard,
    selection_jaccard = selection_jaccard,
    high_pair_quantile = high_pair_quantile,
    evidence_role = "representation_robustness",
    source_family = "embedding_semantic_ensemble",
    participant_based = FALSE,
    dependency_note = "Agreement across embedding representations qualifies semantic robustness but does not constitute participant-response validation.",
    consensus_matrix_constructed = FALSE
  )
  class(out) <- c("semantica_semantic_robustness", "list")
  out
}


#' Exact reference for the semantic-stage constrained selection objective
#'
#' Exhaustively enumerates a factor-stratified item-selection problem when the
#' constrained search space is small enough for an exact reference calculation.
#' This is an optimizer-integrity/benchmark utility: it evaluates the same
#' semantic-stage utility used by SEMANTICA (semantic score, duplicate penalty,
#' and optional facet coverage), but it does not run PFA, ESEM, DFI, or any
#' participant-data analysis. Its purpose is to quantify metaheuristic regret on
#' tractable frozen problems rather than to replace ACO for realistic large
#' search spaces.
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Named vector assigning candidate item IDs to factors.
#' @param select_per_factor Named integer vector giving the exact number selected
#'   from each factor.
#' @param max_combinations Explicit maximum number of complete candidate forms
#'   the caller authorizes for exhaustive enumeration. There is deliberately no
#'   implicit default; exact search begins only after this computational budget
#'   is acknowledged.
#' @param eligible_items Optional item IDs defining a pre-screened eligible pool.
#'   This permits exact comparison with an ACO run after its deterministic guards
#'   have been applied.
#' @param redundancy_threshold,dup_threshold Semantic redundancy-policy
#'   references. They are selection-policy values, not universal psychometric
#'   cutoffs.
#' @param within_similarity_target,within_similarity_band Passed to the semantic
#'   objective. When the target is omitted it is estimated from the frozen
#'   eligible pool with the internal `estimate_within_similarity_targets()` helper.
#' @param semantic_objective_mode Semantic objective used by SEMANTICA.
#' @param expected_factor_relations,nomological_weight Optional declared
#'   semantic nomological hypothesis used by the existing objective.
#' @param item_facet_lookup,facets_by_factor,facet_coverage_weight Optional facet
#'   metadata and weight matching the semantic-stage coverage multiplier.
#' @return A `semantica_exact_semantic_reference` with the exact best utility,
#'   all exactly tied best forms, the constrained search-space size, and the
#'   number of forms evaluated.
#' @references Kilmen, S., & Bulut, O. (2025). Shortening Psychological Scales:
#'   Semantic Similarity Matters. *Educational and Psychological Measurement,
#'   85*(5), 910-934. \doi{10.1177/00131644251319047}
#'
#'   Jung, S.-J., & Seo, J.-W. (2025). A transformer-based embedding approach to
#'   developing short-form psychological measures. *Frontiers in Psychology,
#'   16*, 1640864. \doi{10.3389/fpsyg.2025.1640864}
#' @export
semantica_exact_semantic_reference <- function(
    similarity_matrix, factor_assignment, select_per_factor,
    max_combinations = NULL, eligible_items = NULL,
    redundancy_threshold = 0.85, dup_threshold = 0.90,
    within_similarity_target = NULL, within_similarity_band = 0.08,
    semantic_objective_mode = c("relative_conservative", "legacy_target_burden"),
    expected_factor_relations = NULL, nomological_weight = 0,
    item_facet_lookup = NULL, facets_by_factor = NULL,
    facet_coverage_weight = 0.15) {
  semantic_objective_mode <- match.arg(semantic_objective_mode)
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L || is.null(rownames(m)) || is.null(colnames(m)) ||
      !setequal(rownames(m), colnames(m)) || anyDuplicated(rownames(m))) {
    stop("'similarity_matrix' must be a named square matrix with unique matching item IDs.", call. = FALSE)
  }
  ids <- rownames(m)
  m <- m[ids, ids, drop = FALSE]
  if (any(!is.finite(m)) || !isTRUE(all.equal(m, t(m), tolerance = 1e-8))) {
    stop("'similarity_matrix' must be finite and symmetric.", call. = FALSE)
  }
  if (is.null(names(factor_assignment)) || anyDuplicated(names(factor_assignment))) {
    stop("'factor_assignment' must be uniquely named by item ID.", call. = FALSE)
  }
  if (!all(ids %in% names(factor_assignment))) {
    stop("'factor_assignment' is missing one or more matrix item IDs.", call. = FALSE)
  }
  fa_all <- stats::setNames(as.character(factor_assignment[ids]), ids)
  if (anyNA(fa_all) || any(!nzchar(fa_all))) {
    stop("Every candidate item must have a non-empty factor assignment.", call. = FALSE)
  }
  if (is.null(names(select_per_factor)) || any(!nzchar(names(select_per_factor))) || anyDuplicated(names(select_per_factor))) {
    stop("'select_per_factor' must be uniquely named by factor.", call. = FALSE)
  }
  factors <- names(select_per_factor)
  select_n <- suppressWarnings(as.integer(select_per_factor))
  names(select_n) <- factors
  if (anyNA(select_n) || any(select_n < 2L)) {
    stop("Every 'select_per_factor' value must be an integer of at least 2 for the semantic structural objective.", call. = FALSE)
  }
  if (!all(factors %in% unique(fa_all))) {
    stop("'select_per_factor' names include factors absent from 'factor_assignment'.", call. = FALSE)
  }
  if (is.null(max_combinations)) {
    candidate_counts <- vapply(factors, function(f) sum(fa_all == f), integer(1L))
    ss <- .semantica_constrained_search_space(candidate_counts, select_n)
    stop(sprintf(
      paste0("Exact enumeration requires an explicit 'max_combinations' budget. ",
             "The declared constrained problem has approximately 10^%.3f complete forms."),
      ss$log10_total
    ), call. = FALSE)
  }
  max_combinations <- suppressWarnings(as.numeric(max_combinations[1L]))
  if (!is.finite(max_combinations) || max_combinations < 1 || max_combinations != floor(max_combinations)) {
    stop("'max_combinations' must be a positive finite whole number.", call. = FALSE)
  }
  eligible <- if (is.null(eligible_items)) ids else unique(as.character(eligible_items))
  if (!length(eligible) || any(!eligible %in% ids)) {
    stop("'eligible_items' must be NULL or item IDs contained in the similarity matrix.", call. = FALSE)
  }
  fa <- fa_all[eligible]
  list_items <- lapply(factors, function(f) names(fa)[fa == f])
  names(list_items) <- factors
  candidate_counts <- vapply(list_items, length, integer(1L))
  if (any(candidate_counts < select_n)) {
    bad <- factors[candidate_counts < select_n]
    stop(sprintf("Exact selection is infeasible for factor(s): %s.", paste(bad, collapse = ", ")), call. = FALSE)
  }
  ss <- .semantica_constrained_search_space(candidate_counts, select_n)
  if (!is.finite(ss$log10_total) || ss$log10_total > log10(max_combinations) + 1e-12) {
    stop(sprintf(
      paste0("Exact search not started: approximately 10^%.3f forms exceed the explicitly ",
             "authorized max_combinations=%s."),
      ss$log10_total, format(max_combinations, scientific = FALSE, trim = TRUE)
    ), call. = FALSE)
  }
  target <- estimate_within_similarity_targets(
    list_items, m, factors,
    within_similarity_target = within_similarity_target,
    redundancy_threshold = redundancy_threshold,
    within_similarity_band = within_similarity_band,
    method = "nonredundant_median"
  )
  combinations <- lapply(factors, function(f) {
    utils::combn(list_items[[f]], select_n[[f]], simplify = FALSE)
  })
  names(combinations) <- factors

  best_score <- -Inf
  best_forms <- list()
  evaluated <- 0
  score_form <- function(selected) {
    sub <- m[selected, selected, drop = FALSE]
    sub_fa <- fa_all[selected]
    sem <- compute_semantic_sim_index_v2(
      sub, selected, sub_fa, factors,
      redundancy_threshold = redundancy_threshold,
      within_similarity_target = target,
      within_similarity_band = within_similarity_band,
      expected_factor_relations = expected_factor_relations,
      nomological_weight = nomological_weight,
      semantic_objective_mode = semantic_objective_mode
    )
    dup <- compute_duplicate_penalty(selected, sub_fa, factors, sub, dup_threshold)
    facet <- compute_facet_coverage_multiplier(
      selected, sub_fa, item_facet_lookup, facets_by_factor, select_n,
      weight = facet_coverage_weight
    )
    list(score = sem$sem_score * dup * facet, semantic = sem, duplicate_penalty = dup,
         facet_multiplier = facet)
  }
  recurse <- function(depth, chosen) {
    if (depth > length(factors)) {
      evaluated <<- evaluated + 1
      sc <- score_form(chosen)
      val <- sc$score
      if (is.finite(val) && val > best_score) {
        best_score <<- val
        best_forms <<- list(list(items = chosen, components = sc))
      } else if (is.finite(val) && identical(val, best_score)) {
        best_forms[[length(best_forms) + 1L]] <<- list(items = chosen, components = sc)
      }
      return(invisible(NULL))
    }
    for (pick in combinations[[depth]]) recurse(depth + 1L, c(chosen, pick))
    invisible(NULL)
  }
  recurse(1L, character())
  if (!length(best_forms) || !is.finite(best_score)) {
    stop("Exact semantic reference did not produce a finite candidate utility.", call. = FALSE)
  }
  structure(list(
    status = "exact_complete_semantic_stage_reference",
    objective_scope = "semantic_plus_duplicate_plus_optional_facet_coverage",
    participant_based = FALSE,
    search_space = ss,
    evaluated_forms = as.numeric(evaluated),
    max_combinations_authorized = max_combinations,
    best_score = best_score,
    best_items = best_forms[[1L]]$items,
    tied_best_forms = lapply(best_forms, `[[`, "items"),
    n_tied_best = length(best_forms),
    best_components = best_forms[[1L]]$components,
    within_similarity_target = target,
    semantic_objective_mode = semantic_objective_mode,
    threshold_policy_origin = .semantica_decision_policy()$semantic_thresholds$provenance,
    interpretation = paste(
      "This is an exact oracle only for the declared semantic-stage objective on",
      "the frozen eligible pool. It does not establish that the semantic utility",
      "is a psychometric validity criterion and does not benchmark PFA/ESEM-guided",
      "multi-fidelity objectives. Compare an ACO result with the same frozen inputs",
      "to quantify optimization regret on tractable cases."
    )
  ), class = c("semantica_exact_semantic_reference", "list"))
}

.semantica_extract_pair_vectors <- function(semantic_matrix, response_matrix) {
  s <- as.matrix(semantic_matrix)
  r <- as.matrix(response_matrix)
  common <- intersect(rownames(s), rownames(r))
  common <- intersect(common, intersect(colnames(s), colnames(r)))
  if (length(common) < 3L) stop("Need at least three identically named items in both matrices.")
  s <- s[common, common, drop = FALSE]
  r <- r[common, common, drop = FALSE]
  idx <- lower.tri(s)
  data.frame(
    item_i = rownames(s)[row(s)[idx]],
    item_j = colnames(s)[col(s)[idx]],
    semantic = as.numeric(s[idx]),
    empirical = as.numeric(r[idx]),
    stringsAsFactors = FALSE
  )
}

#' Fit an empirical semantic-to-response calibration
#'
#' This empirical-calibration function estimates the mapping between semantic item
#' relations and participant item correlations. It must be evaluated on held-out
#' instruments or an independent sample before being used for confirmatory
#' inference.
#'
#' @param semantic_matrix Semantic similarity matrix.
#' @param response_data Optional participant-by-item data frame/matrix.
#' @param response_matrix Optional empirical item correlation matrix.
#' @param method `"isotonic"`, `"linear"`, or `"fisher_linear"`.
#' @param cor_method Correlation method if `response_data` is supplied.
#' @return A `semantica_empirical_calibration` model.
#' @export
semantica_fit_empirical_calibration <- function(
  semantic_matrix,
  response_data = NULL,
  response_matrix = NULL,
  method = c("isotonic", "linear", "fisher_linear"),
  cor_method = "pearson"
) {
  method <- match.arg(method)
  if (is.null(response_matrix)) {
    if (is.null(response_data)) stop("Provide 'response_data' or 'response_matrix'.")
    dat <- as.data.frame(response_data)
    response_matrix <- stats::cor(dat, use = "pairwise.complete.obs", method = cor_method)
  }
  pairs <- .semantica_extract_pair_vectors(semantic_matrix, response_matrix)
  pairs <- pairs[is.finite(pairs$semantic) & is.finite(pairs$empirical), , drop = FALSE]
  if (nrow(pairs) < 6L) stop("Too few finite item pairs for calibration.")

  if (method == "isotonic") {
    ord <- order(pairs$semantic)
    iso <- stats::isoreg(pairs$semantic[ord], pairs$empirical[ord])
    model <- list(x = iso$x, y = iso$yf)
  } else if (method == "fisher_linear") {
    sx <- atanh(pmin(pmax(pairs$semantic, -0.999), 0.999))
    ry <- atanh(pmin(pmax(pairs$empirical, -0.999), 0.999))
    model <- stats::lm(ry ~ sx)
  } else {
    model <- stats::lm(empirical ~ semantic, data = pairs)
  }
  fitted_vals <- if (method == "isotonic") {
    stats::approx(model$x, model$y, xout = pairs$semantic, ties = mean, rule = 2)$y
  } else if (method == "fisher_linear") {
    tanh(stats::predict(model, newdata = data.frame(sx = atanh(pmin(pmax(pairs$semantic, -0.999), 0.999)))))
  } else stats::predict(model, newdata = pairs)

  out <- list(
    method = method,
    model = model,
    training_pairs = pairs,
    diagnostics = list(
      n_pairs = nrow(pairs),
      spearman = suppressWarnings(stats::cor(pairs$semantic, pairs$empirical, method = "spearman")),
      pearson = suppressWarnings(stats::cor(pairs$semantic, pairs$empirical, method = "pearson")),
      rmse_training = sqrt(mean((pairs$empirical - fitted_vals)^2)),
      mae_training = mean(abs(pairs$empirical - fitted_vals))
    ),
    evidence_role = "empirical_semantic_to_response_calibration",
    source_families = c("embedding_semantic", "response_data"),
    participant_based = TRUE,
    validation_status = "training_fit_only_requires_independent_validation",
    dependency_note = "This model is fitted to the supplied response sample; its training fit is not independent validation."
  )
  class(out) <- c("semantica_empirical_calibration", "list")
  out
}

#' Predict empirical similarities from a fitted SEMANTICA calibration
#'
#' Applies a fitted semantic-to-response calibration to new
#' semantic similarity values. This does not convert semantic evidence into
#' participant-based validation; independent response-data validation remains
#' required.
#'
#' @param object A fitted `semantica_empirical_calibration` object.
#' @param newdata Numeric semantic similarity values to calibrate.
#' @param ... Additional arguments reserved for S3 method compatibility.
#' @return Numeric calibrated similarity values constrained to `[-0.999, 0.999]`.
#' @method predict semantica_empirical_calibration
#' @export
predict.semantica_empirical_calibration <- function(object, newdata, ...) {
  x <- as.numeric(newdata)
  if (object$method == "isotonic") {
    return(pmin(pmax(stats::approx(object$model$x, object$model$y, xout = x, ties = mean, rule = 2)$y, -0.999), 0.999))
  }
  if (object$method == "fisher_linear") {
    z <- atanh(pmin(pmax(x, -0.999), 0.999))
    return(pmin(pmax(tanh(stats::predict(object$model, newdata = data.frame(sx = z))), -0.999), 0.999))
  }
  pmin(pmax(stats::predict(object$model, newdata = data.frame(semantic = x)), -0.999), 0.999)
}

#' Apply an empirical calibration to a semantic matrix
#'
#' Applies a previously fitted empirical calibration
#' object to a semantic similarity matrix.
#'
#' @param semantic_matrix Square semantic matrix.
#' @param calibration Fitted `semantica_empirical_calibration`.
#' @return Symmetric calibrated matrix with unit diagonal.
#' @export
semantica_apply_empirical_calibration <- function(semantic_matrix, calibration) {
  if (!inherits(calibration, "semantica_empirical_calibration")) stop("Invalid calibration object.")
  m <- as.matrix(semantic_matrix)
  idx <- lower.tri(m)
  vals <- stats::predict(calibration, m[idx])
  out <- diag(1, nrow(m))
  out[idx] <- vals
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  dimnames(out) <- dimnames(m)
  attr(out, "semantica_calibration") <- list(
    method = calibration$method,
    validation_status = calibration$validation_status,
    training_diagnostics = calibration$diagnostics,
    source_families = calibration$source_families %||% c("embedding_semantic", "response_data"),
    participant_based = TRUE,
    validation_status = calibration$validation_status
  )
  out
}

#' Update a semantic proxy using pilot response data
#'
#' Calibration helper for adapting a semantic proxy with pilot
#' responses. Pilot data used here should not be reused as final confirmatory
#' validation evidence.
#'
#' @param semantic_matrix Semantic item matrix.
#' @param pilot_data Participant-by-item pilot data.
#' @param method Calibration method.
#' @return Calibration model and calibrated matrix. The returned object carries
#'   an explicit warning that the pilot sample must not be reused as a final
#'   confirmatory validation sample.
#' @export
semantica_update_with_pilot <- function(semantic_matrix, pilot_data,
                                        method = c("isotonic", "linear", "fisher_linear")) {
  method <- match.arg(method)
  cal <- semantica_fit_empirical_calibration(
    semantic_matrix = semantic_matrix,
    response_data = pilot_data,
    method = method
  )
  list(
    calibration = cal,
    calibrated_matrix = semantica_apply_empirical_calibration(semantic_matrix, cal),
    warning = paste(
      "This calibration was learned from pilot participants.",
      "Use an independent sample for confirmatory psychometric validation and final performance claims."
    ),
    status = "research_track_pilot_update",
    source_families = c("embedding_semantic", "response_data"),
    participant_based = TRUE,
    validation_role = "developmental_pilot_calibration_not_confirmatory_validation"
  )
}

#' Quantify distortion introduced when making a semantic matrix ESEM-usable
#'
#' Matrix-repair diagnostic describing how much a semantic matrix changes under
#' the established repair path used to obtain an ESEM-usable correlation proxy.
#'
#' @param cosine_matrix Raw cosine matrix.
#' @param factor_assignment,factors Optional intended factor information.
#' @param material_change Legacy absolute cell-change threshold retained only for
#'   a descriptive changed-cell proportion. Continuous relative perturbation and
#'   rank-preservation diagnostics are the primary repair evidence.
#' @return Matrix-repair diagnostics.
#' @export
semantica_matrix_repair_diagnostics <- function(cosine_matrix,
                                                 factor_assignment = NULL,
                                                 factors = NULL,
                                                 material_change = 0.02) {
  repaired <- transform_cosine_for_esem(
    cosine_matrix, factor_assignment, factors, material_change = material_change
  )
  diag_info <- attr(repaired, "semantica_matrix_repair", exact = TRUE)
  if (is.null(diag_info)) {
    raw <- as.matrix(cosine_matrix)
    delta <- repaired - raw
    off <- upper.tri(delta)
    diag_info <- list(
      min_eigen_before = min(eigen((raw + t(raw)) / 2, symmetric = TRUE, only.values = TRUE)$values),
      min_eigen_after = min(eigen(repaired, symmetric = TRUE, only.values = TRUE)$values),
      repair_required = any(abs(delta) > sqrt(.Machine$double.eps)),
      matrix_source = if (any(abs(delta) > sqrt(.Machine$double.eps))) "repaired_semantic_proxy" else "raw_semantic_proxy",
      frobenius_change = sqrt(sum(delta^2)),
      relative_frobenius_change = {
        denom <- sqrt(sum((raw - diag(nrow(raw)))^2))
        if (is.finite(denom) && denom > sqrt(.Machine$double.eps)) sqrt(sum(delta^2)) / denom else NA_real_
      },
      max_abs_change = max(abs(delta)),
      mean_abs_offdiag_change = mean(abs(delta[off])),
      offdiag_pearson = suppressWarnings(stats::cor(as.numeric(raw[off]), as.numeric(repaired[off]), method = "pearson")),
      offdiag_spearman = suppressWarnings(stats::cor(as.numeric(raw[off]), as.numeric(repaired[off]), method = "spearman")),
      proportion_materially_changed = mean(abs(delta[off]) >= material_change),
      material_change = material_change,
      threshold_free_primary = TRUE,
      used_nearPD = NA
    )
  }
  diag_info
}

.semantica_evidence_record <- function(status = c("computed", "unavailable", "not_requested", "fallback"),
                                        value = NULL,
                                        reason = NULL,
                                        participant_based = FALSE,
                                        selection_conditioned = FALSE,
                                        evidence_scope = NULL,
                                        source_family = NULL) {
  status <- match.arg(status)
  if (!is.logical(participant_based) || length(participant_based) != 1L || is.na(participant_based)) {
    stop("'participant_based' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(selection_conditioned) || length(selection_conditioned) != 1L || is.na(selection_conditioned)) {
    stop("'selection_conditioned' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(reason)) reason <- as.character(reason)[1L]
  if (!is.null(evidence_scope)) evidence_scope <- as.character(evidence_scope)[1L]
  if (is.null(source_family)) {
    source_family <- if (isTRUE(participant_based)) "response_data" else "embedding_semantic"
  }
  source_family <- as.character(source_family)[1L]
  structure(
    list(
      status = status,
      reason = reason,
      participant_based = participant_based,
      selection_conditioned = selection_conditioned,
      evidence_scope = evidence_scope,
      source_family = source_family,
      evidence_family = source_family,
      value = value
    ),
    class = c("semantica_evidence_record", "list")
  )
}

#' Summarize what evidence a SEMANTICA result can and cannot provide
#'
#' Returns an evidence-family table that separates embedding-derived proxy,
#' content/metadata, and participant-response evidence so diagnostics sharing one
#' representation are not mistaken for independent validation sources.
#'
#' @param result A SEMANTICA pipeline/full-pipeline result.
#' @param labels Status-label representation: `"raw"` preserves stable machine-readable tokens, `"human"` returns readable labels, and `"both"` adds both forms.
#' @return Data frame describing evidence status.
#' @export
semantica_evidence_status <- function(result, labels = c("raw", "human", "both")) {
  labels <- match.arg(labels)
  response_validation <- result$response_validation %||%
    result$optimization$response_validation %||% NULL
  response_available <- !is.null(response_validation) ||
    isTRUE(result$participant_validation_performed)
  response_converged <- if (!is.null(response_validation$result$converged)) {
    isTRUE(response_validation$result$converged)
  } else if (!is.null(result$participant_validation_converged)) {
    isTRUE(result$participant_validation_converged)
  } else NA
  response_admissible <- if (!is.null(response_validation$result$admissible)) {
    isTRUE(response_validation$result$admissible)
  } else NA
  participant_status <- if (!response_available) {
    "not_established"
  } else if (identical(response_converged, FALSE)) {
    "participant_model_attempted_failed"
  } else if (identical(response_converged, TRUE) && identical(response_admissible, FALSE)) {
    "participant_model_converged_inadmissible"
  } else if (identical(response_converged, TRUE) && identical(response_admissible, TRUE)) {
    "participant_model_converged_admissible"
  } else if (identical(response_converged, TRUE)) {
    "participant_model_converged"
  } else "participant_data_supplied"

  coverage <- result$construct_coverage %||% NULL
  alignment_available <- isTRUE(result$content_alignment$available) ||
    (is.data.frame(result$selected_item_metadata) &&
       "semantica_factor_aligned" %in% names(result$selected_item_metadata))
  facet_status <- if (is.null(coverage)) {
    "not_assessed"
  } else if (isTRUE(coverage$semantic_alignment_available)) {
    "semantic_definition_screen"
  } else "metadata_coverage_only"
  dimensionality_mode <- result$dimensionality_mode %||%
    result$optimization$dimensionality_mode %||%
    result$run_config$dimensionality %||% NA_character_
  is_unidimensional <- identical(dimensionality_mode, "unidimensional")
  rep_state <- result$representation_evidence_state %||%
    result$generation$representation_evidence_state %||% NULL
  rep_status <- rep_state$status %||% if (!is.null(result$cosine_diagnostics)) "available_descriptive" else "not_assessed"
  pfa <- result$pfa_diagnostics %||% result$optimization$pfa_diagnostics %||% NULL
  pfa_status <- if (is_unidimensional) {
    "not_applicable_unidimensional"
  } else if (isTRUE(pfa$available)) "available_as_proxy" else "unavailable"
  esem <- result$esem_state %||% result$optimization$esem_state %||% NULL
  esem_status <- if (!is.null(esem)) "available_as_proxy" else "not_assessed"
  cc <- result$semantic_cluster_consensus$selected %||% result$semantic_cluster_consensus$pool %||% NULL
  cc_status <- if (is_unidimensional) "not_applicable_unidimensional" else if (isTRUE(cc$available)) "available_as_proxy" else "not_assessed"

  evidence <- c(
    "embedding_representation",
    "semantic_construct_alignment",
    "semantic_definition_alignment",
    "semantic_redundancy",
    "semantic_construct_separation",
    "sample_free_pfa_proxy",
    "sample_free_esem_proxy",
    "semantic_cluster_consensus",
    "content_facet_coverage",
    "participant_internal_structure",
    "reliability",
    "measurement_invariance_DIF",
    "criterion_or_convergent_validity"
  )
  status <- c(
    rep_status,
    "available_as_proxy",
    if (alignment_available) "available_as_content_screen" else "not_assessed",
    "available_as_proxy",
    if (is_unidimensional) "not_applicable_unidimensional" else "available_as_proxy",
    pfa_status,
    esem_status,
    cc_status,
    facet_status,
    participant_status,
    "not_established_by_semantic_proxy",
    "not_established_by_semantic_proxy",
    "not_established_by_semantic_proxy"
  )
  source_family <- c(
    rep("embedding_semantic", 8L),
    "content_metadata",
    "response_data",
    "not_established",
    "not_established",
    "not_established"
  )
  participant_based <- c(rep(FALSE, 9L), response_available, FALSE, FALSE, FALSE)
  dependency_note <- ifelse(
    source_family == "embedding_semantic",
    "Shares the active embedding representation with other embedding-semantic diagnostics; not independent corroboration.",
    ifelse(source_family == "response_data",
           "Participant-response evidence; distinct from the sample-free embedding proxy when actually supplied.",
           "Not an independent participant-validation source." )
  )
  out <- data.frame(
    evidence = evidence,
    status = status,
    source_family = source_family,
    participant_based = participant_based,
    dependency_note = dependency_note,
    stringsAsFactors = FALSE
  )
  if (identical(labels, "human")) {
    out$status <- .semantica_evidence_human_label(out$status)
  } else if (identical(labels, "both")) {
    out$raw_status <- out$status
    out$status <- .semantica_evidence_human_label(out$status)
    out <- out[c("evidence", "status", "raw_status", "source_family", "participant_based", "dependency_note")]
  }
  out
}

#' Build a construct coverage graph
#'
#' Construct-graph representation for factors, facets, exclusions, and
#' optional item nodes in a construct blueprint.
#'
#' @param blueprint Construct blueprint.
#' @param items_tbl Optional item metadata to attach item nodes.
#' @param factor_col,facet_col,id_col,item_text_col Metadata columns.
#' @return An `igraph` object representing factors, facets, exclusions, and
#'   optionally items.
#' @export
semantica_construct_graph <- function(blueprint, items_tbl = NULL,
                                      factor_col = "factor", facet_col = "Facet",
                                      id_col = "item_id", item_text_col = "item_text") {
  if (!inherits(blueprint, "semantica_construct_blueprint")) stop("Invalid blueprint.")
  edges <- data.frame(from = character(), to = character(), relation = character(), stringsAsFactors = FALSE)
  nodes <- data.frame(name = character(), node_type = character(), label = character(), stringsAsFactors = FALSE)
  add_node <- function(name, type, label = name) {
    if (!name %in% nodes$name) nodes <<- rbind(nodes, data.frame(name = name, node_type = type, label = label, stringsAsFactors = FALSE))
  }
  for (f in blueprint$factor_names) {
    fn <- paste0("factor::", f); add_node(fn, "factor", f)
    for (facet in blueprint$required_facets[[f]] %||% character(0L)) {
      facn <- paste0("facet::", f, "::", facet); add_node(facn, "facet", facet)
      edges <- rbind(edges, data.frame(from = fn, to = facn, relation = "requires_facet", stringsAsFactors = FALSE))
    }
    for (ex in blueprint$exclusions[[f]] %||% character(0L)) {
      exn <- paste0("exclusion::", f, "::", ex); add_node(exn, "exclusion", ex)
      edges <- rbind(edges, data.frame(from = fn, to = exn, relation = "exclude_or_discriminate", stringsAsFactors = FALSE))
    }
  }
  if (!is.null(items_tbl)) {
    x <- as.data.frame(items_tbl, stringsAsFactors = FALSE)
    if (!id_col %in% names(x) && "ID" %in% names(x)) id_col <- "ID"
    if (!factor_col %in% names(x) && "Dimension" %in% names(x)) factor_col <- "Dimension"
    if (!facet_col %in% names(x) && "facet" %in% names(x)) facet_col <- "facet"
    if (!item_text_col %in% names(x) && "item" %in% names(x)) item_text_col <- "item"
    for (i in seq_len(nrow(x))) {
      id <- as.character(x[[id_col]][i]); inn <- paste0("item::", id)
      add_node(inn, "item", as.character(x[[item_text_col]][i] %||% id))
      f <- as.character(x[[factor_col]][i]); facet <- as.character(x[[facet_col]][i])
      if (!is.na(facet) && nzchar(facet)) {
        parent <- paste0("facet::", f, "::", facet)
        add_node(parent, "facet", facet)
      } else parent <- paste0("factor::", f)
      add_node(parent, if (grepl("^facet::", parent)) "facet" else "factor", if (grepl("^facet::", parent)) facet else f)
      edges <- rbind(edges, data.frame(from = parent, to = inn, relation = "represented_by_item", stringsAsFactors = FALSE))
    }
  }
  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
}

#' Run a counterfactual semantic stress test
#'
#' Benchmark helper for comparing controlled wording variants.
#' SEMANTICA intentionally does not impose a universal success threshold here.
#'
#' @param original Character vector of original item texts.
#' @param variants Named list or data frame of controlled variants.
#' @param scorer Function accepting two character vectors (`original`,
#'   `variants`) and returning numeric scores or a data frame.
#' @return Stress-test table. SEMANTICA does not assign a universal success
#'   threshold; the output is intended for benchmark suites.
#' @export
semantica_counterfactual_stress_test <- function(original, variants, scorer) {
  if (!is.function(scorer)) stop("'scorer' must be a function.")
  original <- as.character(original)
  if (is.data.frame(variants)) {
    if (!all(c("original_index", "variant_text", "manipulation") %in% names(variants))) {
      stop("Variant data frame needs original_index, variant_text, and manipulation.")
    }
    v <- variants
  } else if (is.list(variants)) {
    v <- do.call(rbind, lapply(seq_along(variants), function(i) {
      vals <- variants[[i]]
      data.frame(original_index = i, variant_text = as.character(vals), manipulation = names(vals) %||% "variant", stringsAsFactors = FALSE)
    }))
  } else stop("'variants' must be a list or data frame.")
  if (any(v$original_index < 1L | v$original_index > length(original))) stop("Invalid original_index.")
  base <- original[v$original_index]
  scored <- scorer(base, v$variant_text)
  if (is.data.frame(scored)) return(cbind(v, original_text = base, scored))
  cbind(v, original_text = base, score = as.numeric(scored), stringsAsFactors = FALSE)
}

#' Contextualize an observed cosine within its active representation
#'
#' Adds model-relative percentile information to a raw cosine value without
#' changing SEMANTICA thresholds or implying that percentiles are validated
#' psychometric cutoffs.
#'
#' @param cosine_sim_matrix Named square similarity/cosine matrix.
#' @param observed Optional observed cosine/similarity value. If omitted,
#'   `item_i` and `item_j` must identify a matrix pair.
#' @param item_i,item_j Optional item IDs used to obtain `observed`.
#' @param factor_assignment Optional named factor assignment for within/between
#'   reference distributions.
#' @param threshold Optional active fixed/user threshold to display unchanged.
#' @param threshold_source Label describing where `threshold` came from.
#' @param embedding_model Optional model label.
#' @param provenance Optional safe representation provenance.
#' @return A list of raw value plus representation-relative percentiles.
#' @export
semantica_cosine_context <- function(
  cosine_sim_matrix, observed = NULL, item_i = NULL, item_j = NULL,
  factor_assignment = NULL, threshold = NULL,
  threshold_source = "fixed_user_or_default", embedding_model = NULL,
  provenance = NULL
) {
  m <- as.matrix(cosine_sim_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L || any(!is.finite(m))) {
    stop("'cosine_sim_matrix' must be a finite square matrix with at least two items.")
  }
  if (is.null(observed)) {
    if (is.null(item_i) || is.null(item_j) || is.null(rownames(m)) || is.null(colnames(m)) ||
        !item_i %in% rownames(m) || !item_j %in% colnames(m)) {
      stop("Supply a finite 'observed' value or valid 'item_i'/'item_j' names.")
    }
    observed <- m[item_i, item_j]
  }
  observed <- suppressWarnings(as.numeric(observed[1L]))
  if (length(observed) != 1L || !is.finite(observed)) stop("'observed' must be one finite numeric value.")
  pct <- function(ref) {
    ref <- as.numeric(ref[is.finite(ref)])
    if (!length(ref)) return(NA_real_)
    mean(ref <= observed)
  }
  all_pairs <- m[lower.tri(m)]
  within <- between <- numeric(0L)
  if (!is.null(factor_assignment) && !is.null(rownames(m))) {
    fa <- factor_assignment[rownames(m)]
    ok <- !is.na(fa)
    if (sum(ok) >= 2L) {
      mm <- m[ok, ok, drop = FALSE]
      ff <- as.character(fa[ok])
      idx <- which(lower.tri(mm), arr.ind = TRUE)
      same <- ff[idx[, 1L]] == ff[idx[, 2L]]
      vals <- mm[lower.tri(mm)]
      within <- vals[same]
      between <- vals[!same]
    }
  }
  out <- list(
    cosine = observed,
    all_pairs_percentile = pct(all_pairs),
    within_factor_percentile = pct(within),
    between_factor_percentile = pct(between),
    threshold = threshold,
    threshold_source = threshold_source,
    embedding_model = embedding_model %||% "unknown",
    representation_provenance = .semantica_sanitize_config_provenance(provenance %||% list()),
    reference_counts = list(
      all_pairs = length(all_pairs), within_factor = length(within), between_factor = length(between)
    ),
    participant_based = FALSE,
    note = paste(
      "Percentiles are representation-relative context for the active embedding geometry.",
      "They are not validated psychometric cutoffs and do not redefine the raw threshold."
    )
  )
  class(out) <- c("semantica_cosine_context", "list")
  out
}

#' Threshold-free semantic discrimination via stochastic superiority
#'
#' Computes `A = P(S_within > S_between) + 0.5 * P(S_within = S_between)` from
#' the active similarity representation. The statistic is a sample-free semantic
#' discrimination diagnostic, not participant-based construct validity. When
#' calculated after optimizer-driven item selection it is descriptive rather
#' than selection-adjusted; use [semantica_selection_context()] to retain the
#' candidate-pool comparison.
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Named vector mapping item IDs to intended factors.
#' @return A list containing `estimate`, pair counts, orientation, and an explicit
#'   status/reason when the statistic is not computable.
#' @references Vargha, A., & Delaney, H. D. (2000). A critique and improvement
#'   of the CL common language effect size statistics of McGraw and Wong.
#'   *Journal of Educational and Behavioral Statistics, 25*(2), 101-132.
#'   \doi{10.3102/10769986025002101}
#' @export
semantica_semantic_discrimination <- function(similarity_matrix, factor_assignment) {
  m <- as.matrix(similarity_matrix)
  fail <- function(reason, n_within_pairs = 0L, n_between_pairs = 0L,
                   within_mean = NA_real_, between_mean = NA_real_) {
    out <- list(
      estimate = NA_real_, status = "unavailable", reason = reason,
      n_within_pairs = as.integer(n_within_pairs), n_between_pairs = as.integer(n_between_pairs),
      within_mean = within_mean, between_mean = between_mean,
      orientation = "higher = within-factor similarities stochastically exceed between-factor similarities",
      ties = "half credit", participant_based = FALSE,
      evidence_role = "semantic_discrimination"
    )
    class(out) <- c("semantica_semantic_discrimination", "list")
    out
  }
  if (nrow(m) != ncol(m) || nrow(m) < 3L || is.null(rownames(m)) || is.null(colnames(m))) {
    return(fail("similarity_matrix must be a named square matrix with at least three items"))
  }
  if (is.null(names(factor_assignment))) return(fail("factor_assignment must be named by item ID"))
  ids <- intersect(rownames(m), names(factor_assignment))
  if (length(ids) < 3L) return(fail("fewer than three aligned items"))
  m <- m[ids, ids, drop = FALSE]
  fa <- as.character(factor_assignment[ids])
  idx <- which(lower.tri(m), arr.ind = TRUE)
  vals <- m[lower.tri(m)]
  keep <- is.finite(vals) & !is.na(fa[idx[, 1L]]) & !is.na(fa[idx[, 2L]])
  idx <- idx[keep, , drop = FALSE]
  vals <- vals[keep]
  same <- fa[idx[, 1L]] == fa[idx[, 2L]]
  within <- vals[same]
  between <- vals[!same]
  if (!length(within)) return(fail(
    "no finite within-factor item pairs",
    n_within_pairs = 0L, n_between_pairs = length(between),
    between_mean = if (length(between)) mean(between) else NA_real_
  ))
  if (!length(between)) return(fail(
    "no finite between-factor item pairs",
    n_within_pairs = length(within), n_between_pairs = 0L,
    within_mean = mean(within)
  ))
  combined <- c(within, between)
  ranks <- rank(combined, ties.method = "average")
  n_w <- length(within); n_b <- length(between)
  u <- sum(ranks[seq_len(n_w)]) - n_w * (n_w + 1) / 2
  estimate <- u / (n_w * n_b)
  out <- list(
    estimate = max(0, min(1, as.numeric(estimate))), status = "computed", reason = NULL,
    n_within_pairs = n_w, n_between_pairs = n_b,
    within_mean = mean(within), between_mean = mean(between),
    orientation = "higher = within-factor similarities stochastically exceed between-factor similarities",
    ties = "half credit", participant_based = FALSE,
    evidence_role = "semantic_discrimination",
    note = paste(
      "Threshold-free stochastic-superiority diagnostic on the active semantic representation.",
      "It complements mean within-between gap, PFA, and ESEM and is not participant-based validity evidence.",
      "If computed after item selection, interpret it descriptively and retain the pre-selection pool context."
    )
  )
  class(out) <- c("semantica_semantic_discrimination", "list")
  out
}

#' Factor-specific semantic separation diagnostics
#'
#' Computes local, threshold-free semantic separation evidence for each intended
#' factor. For a factor, within-factor item-pair similarities are compared with
#' similarities between that factor's items and all items assigned to other
#' factors. The returned stochastic-superiority value is the same Vargha-Delaney
#' style `A` orientation used by [semantica_semantic_discrimination()].
#'
#' This is a sample-free local diagnostic. It is intended to reveal factors that
#' can be masked by a favorable aggregate statistic; it is not a validated
#' psychometric cutoff or participant-based validity test.
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Named vector mapping item IDs to intended factors.
#' @return A data frame with one row per factor containing item/pair counts,
#'   within and between means, their gap, stochastic superiority, and status.
#' @references Vargha, A., & Delaney, H. D. (2000). *Journal of Educational and
#'   Behavioral Statistics, 25*(2), 101-132. \doi{10.3102/10769986025002101}
#' @export
semantica_factor_semantic_diagnostics <- function(similarity_matrix, factor_assignment) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 3L || is.null(rownames(m)) || is.null(colnames(m))) {
    stop("'similarity_matrix' must be a named square matrix with at least three items.", call. = FALSE)
  }
  if (is.null(names(factor_assignment))) {
    stop("'factor_assignment' must be named by item ID.", call. = FALSE)
  }

  ids <- intersect(rownames(m), names(factor_assignment))
  if (length(ids) < 3L) {
    stop("Fewer than three aligned item IDs are available.", call. = FALSE)
  }
  m <- m[ids, ids, drop = FALSE]
  fa <- as.character(factor_assignment[ids])
  factors <- unique(fa[!is.na(fa) & nzchar(fa)])

  stochastic_A <- function(within, between) {
    within <- within[is.finite(within)]
    between <- between[is.finite(between)]
    if (!length(within) || !length(between)) return(NA_real_)
    combined <- c(within, between)
    ranks <- rank(combined, ties.method = "average")
    n_w <- length(within)
    n_b <- length(between)
    u <- sum(ranks[seq_len(n_w)]) - n_w * (n_w + 1) / 2
    max(0, min(1, as.numeric(u / (n_w * n_b))))
  }

  rows <- lapply(factors, function(f) {
    own <- ids[fa == f]
    other <- ids[fa != f & !is.na(fa)]
    within <- numeric(0L)
    if (length(own) >= 2L) {
      within_m <- m[own, own, drop = FALSE]
      within <- within_m[lower.tri(within_m)]
      within <- within[is.finite(within)]
    }
    between <- if (length(other) >= 1L) as.numeric(m[own, other, drop = FALSE]) else numeric(0L)
    between <- between[is.finite(between)]

    data.frame(
      factor = f,
      n_items = length(own),
      n_within_pairs = length(within),
      n_between_pairs = length(between),
      within_mean = if (length(within)) mean(within) else NA_real_,
      between_mean = if (length(between)) mean(between) else NA_real_,
      gap = if (length(within) && length(between)) mean(within) - mean(between) else NA_real_,
      stochastic_superiority = stochastic_A(within, between),
      status = if (length(within) && length(between)) "computed" else if (length(within)) "within_only" else "unavailable",
      participant_based = FALSE,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "note") <- paste(
    "Factor-specific semantic separation is a local sample-free diagnostic.",
    "For a unidimensional model only within-factor cohesion is defined; between-factor gap and A are not applicable.",
    "No universal cutoff is applied; inspect the available metrics alongside the structural proxy evidence."
  )
  class(out) <- c("semantica_factor_semantic_diagnostics", "data.frame")
  out
}

#' Contextualize semantic discrimination before and after item selection
#'
#' Compares aggregate and factor-specific semantic separation for the input
#' candidate pool with the selected subset. The gain is descriptive and
#' selection-conditioned: optimization can increase apparent separation even
#' when the underlying pool is weak, so no post-selection inferential claim is
#' attached to the selected value.
#'
#' @param similarity_matrix Named square semantic similarity matrix for the
#'   candidate pool.
#' @param factor_assignment Named vector mapping candidate item IDs to intended
#'   factors.
#' @param selected_items Character vector of selected item IDs.
#' @return A list containing pool and selected discrimination objects,
#'   descriptive gains, and factor-specific diagnostics for both stages.
#' @references Berk, R., Brown, L., Buja, A., Zhang, K., & Zhao, L. (2013).
#'   Valid post-selection inference. *Annals of Statistics, 41*(2), 802-837.
#'   \doi{10.1214/12-AOS1077}
#' @export
semantica_selection_context <- function(similarity_matrix, factor_assignment, selected_items) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || is.null(rownames(m)) || is.null(colnames(m))) {
    stop("'similarity_matrix' must be a named square matrix.", call. = FALSE)
  }
  if (is.null(names(factor_assignment))) {
    stop("'factor_assignment' must be named by item ID.", call. = FALSE)
  }
  ids <- intersect(rownames(m), names(factor_assignment))
  if (length(ids) < 3L) stop("Fewer than three aligned candidate items are available.", call. = FALSE)

  selected_items <- unique(as.character(selected_items))
  if (length(selected_items) < 3L) stop("'selected_items' must contain at least three item IDs.", call. = FALSE)
  missing_selected <- setdiff(selected_items, ids)
  if (length(missing_selected)) {
    stop("Selected item IDs are missing from the candidate pool: ",
         paste(missing_selected, collapse = ", "), call. = FALSE)
  }

  pool_m <- m[ids, ids, drop = FALSE]
  pool_fa <- factor_assignment[ids]
  selected_m <- m[selected_items, selected_items, drop = FALSE]
  selected_fa <- factor_assignment[selected_items]

  dimensionality_mode <- if (length(unique(as.character(pool_fa[!is.na(pool_fa)]))) == 1L) {
    "unidimensional"
  } else "multidimensional"
  pool <- semantica_semantic_discrimination(pool_m, pool_fa)
  selected <- semantica_semantic_discrimination(selected_m, selected_fa)
  pool_factors <- semantica_factor_semantic_diagnostics(pool_m, pool_fa)
  selected_factors <- semantica_factor_semantic_diagnostics(selected_m, selected_fa)
  if (identical(dimensionality_mode, "unidimensional")) {
    pool_within <- pool_factors$within_mean[[1L]] %||% NA_real_
    selected_within <- selected_factors$within_mean[[1L]] %||% NA_real_
    pool$within_mean <- pool_within
    selected$within_mean <- selected_within
  }

  pool_gap <- if (is.finite(pool$within_mean %||% NA_real_) && is.finite(pool$between_mean %||% NA_real_)) {
    pool$within_mean - pool$between_mean
  } else NA_real_
  selected_gap <- if (is.finite(selected$within_mean %||% NA_real_) && is.finite(selected$between_mean %||% NA_real_)) {
    selected$within_mean - selected$between_mean
  } else NA_real_

  out <- list(
    status = if (identical(dimensionality_mode, "unidimensional") &&
                 is.finite(pool$within_mean %||% NA_real_) &&
                 is.finite(selected$within_mean %||% NA_real_)) {
      "computed"
    } else if (identical(pool$status, "computed") && identical(selected$status, "computed")) {
      "computed"
    } else "partial",
    dimensionality_mode = dimensionality_mode,
    pool = pool,
    selected = selected,
    stochastic_superiority_gain = if (is.finite(pool$estimate) && is.finite(selected$estimate)) selected$estimate - pool$estimate else NA_real_,
    pool_gap = pool_gap,
    selected_gap = selected_gap,
    gap_gain = if (is.finite(pool_gap) && is.finite(selected_gap)) selected_gap - pool_gap else NA_real_,
    within_cohesion_change = if (is.finite(pool$within_mean %||% NA_real_) && is.finite(selected$within_mean %||% NA_real_)) {
      selected$within_mean - pool$within_mean
    } else NA_real_,
    selected_fraction = length(selected_items) / length(ids),
    pool_scope = "input_similarity_matrix",
    pool_factor_diagnostics = pool_factors,
    selected_factor_diagnostics = selected_factors,
    selection_conditioned = TRUE,
    participant_based = FALSE,
    evidence_role = "selection_context",
    note = if (identical(dimensionality_mode, "unidimensional")) paste(
      "Selected one-factor semantic cohesion is descriptive after optimization and is not selection-adjusted inference.",
      "Between-factor A/gap statistics are not applicable; interpret cohesion alongside one-factor loading and residual diagnostics."
    ) else paste(
      "Selected semantic diagnostics are descriptive after optimization and are not selection-adjusted inference.",
      "Interpret selected values alongside the same-run candidate-pool values and independent PFA/ESEM diagnostics."
    )
  )
  class(out) <- c("semantica_selection_context", "list")
  out
}

.semantica_unidimensional_proxy_diagnostics <- function(esem_result, cor_matrix = NULL,
                                                        selection_context = NULL) {
  esem <- esem_result %||% list()
  sd <- esem$structure_diagnostics %||% list()
  m <- tryCatch(as.matrix(cor_matrix), error = function(e) NULL)
  eig <- numeric(0L)
  if (!is.null(m) && nrow(m) == ncol(m) && nrow(m) >= 2L && all(is.finite(m))) {
    eig <- tryCatch(sort(eigen((m + t(m)) / 2, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE),
                    error = function(e) numeric(0L))
  }
  positive_total <- if (length(eig)) sum(pmax(eig, 0)) else NA_real_
  first <- if (length(eig) >= 1L) eig[[1L]] else NA_real_
  second <- if (length(eig) >= 2L) eig[[2L]] else NA_real_
  ratio <- if (is.finite(first) && is.finite(second) && second > .Machine$double.eps) first / second else NA_real_
  first_share <- if (is.finite(first) && is.finite(positive_total) && positive_total > .Machine$double.eps) first / positive_total else NA_real_

  list(
    status = if (isTRUE(esem$admissible) || length(eig)) "computed" else "unavailable",
    dimensionality_mode = "unidimensional",
    n_items = if (!is.null(m)) nrow(m) else NA_integer_,
    one_factor_esem_admissible = isTRUE(esem$admissible),
    cfi = esem$cfi %||% NA_real_, tli = esem$tli %||% NA_real_,
    rmsea = esem$rmsea %||% NA_real_, srmr = esem$srmr %||% NA_real_,
    ave = esem$ave %||% NA_real_,
    mean_primary_loading = sd$mean_primary_loading %||% NA_real_,
    median_primary_loading = sd$median_primary_loading %||% NA_real_,
    min_primary_loading = sd$min_primary_loading %||% NA_real_,
    primary_ge_40 = sd$primary_ge_40 %||% NA_real_,
    primary_ge_50 = sd$primary_ge_50 %||% NA_real_,
    mean_abs_residual = sd$mean_abs_residual %||% NA_real_,
    q95_abs_residual = sd$q95_abs_residual %||% NA_real_,
    max_abs_residual = sd$max_abs_residual %||% NA_real_,
    mean_residual = sd$mean_residual %||% NA_real_,
    max_centered_residual = sd$max_centered_residual %||% NA_real_,
    q95_centered_residual = sd$q95_centered_residual %||% NA_real_,
    max_abs_centered_residual = sd$max_abs_centered_residual %||% NA_real_,
    top_centered_residual_pairs = sd$top_centered_residual_pairs %||% NULL,
    first_eigenvalue = first, second_eigenvalue = second,
    eigenvalue_ratio_1_to_2 = ratio, first_eigenvalue_share = first_share,
    pool_within_mean = selection_context$pool$within_mean %||% NA_real_,
    selected_within_mean = selection_context$selected$within_mean %||% NA_real_,
    within_cohesion_change = selection_context$within_cohesion_change %||% NA_real_,
    htmt_status = "not_applicable_unidimensional",
    pfa_partition_status = "not_applicable_unidimensional",
    participant_based = FALSE, selection_conditioned = TRUE,
    evidence_role = "unidimensional_structure_proxy",
    note = paste(
      "Unidimensionality is evaluated here only as converging semantic-proxy evidence, not as participant-based dimensionality proof.",
      "The one-factor model is summarized using loading strength, global fit, residual reproduction/dependence-like concentration, and descriptive eigenvalue dominance.",
      "No universal eigenvalue-ratio or residual-dependence cutoff is imposed; empirical dimensionality and local independence require participant response data."
    )
  )
}

#' Summarize disagreement between PFA and ESEM semantic structural proxies
#'
#' Produces a categorical discrepancy state without averaging PFA and ESEM into
#' an omnibus validity score. All inputs remain sample-free semantic proxies.
#'
#' @param pfa_diagnostics Output from SEMANTICA's sample-free PFA diagnostics.
#' @param esem_result Output from SEMANTICA's ESEM scoring/extraction layer.
#' @return A transparent discrepancy summary with underlying metrics.
#' @references Warrens, M. J., & van der Hoef, H. (2022). Understanding the
#'   Adjusted Rand Index and other partition comparison indices based on counting
#'   object pairs. *Journal of Classification, 39*, 487-509.
#'   \doi{10.1007/s00357-022-09413-z}
#' @export
semantica_pfa_esem_discrepancy <- function(pfa_diagnostics, esem_result) {
  pfa <- pfa_diagnostics %||% list()
  esem <- esem_result %||% list()
  if (identical(esem$dimensionality_mode %||% esem$structure_diagnostics$dimensionality_mode %||% "", "unidimensional")) {
    out <- list(
      state = "not_applicable_unidimensional",
      pfa_grouping = "not_applicable",
      pfa_partition_state = "not_applicable",
      esem_separation = "not_applicable",
      structural_flags = logical(0L),
      metrics = list(
        pfa_factor_presence_recovery = NA_real_,
        pfa_partition_agreement_ari = NA_real_,
        pfa_clarity_score = NA_real_,
        esem_admissible = isTRUE(esem$admissible),
        esem_htmt_max = NA_real_, esem_htmt_violations = NA_real_,
        esem_correct_dominance = NA_real_, esem_no_large_cross_loading = NA_real_
      ),
      participant_based = FALSE,
      evidence_role = "pfa_esem_discrepancy",
      note = paste(
        "PFA partition recovery, HTMT, and cross-factor separation require multiple intended factors and are not applicable to a unidimensional model.",
        "Use the one-factor structural-proxy diagnostics instead."
      )
    )
    class(out) <- c("semantica_pfa_esem_discrepancy", "list")
    return(out)
  }
  pfa_available <- isTRUE(pfa$available)
  pfa_grouping <- if (!pfa_available) {
    "unavailable"
  } else if (length(pfa$missing_factors %||% character(0L)) == 0L) {
    "grouping_recoverable"
  } else {
    "grouping_weak"
  }
  esem_admissible <- isTRUE(esem$admissible)
  sd <- esem$structure_diagnostics %||% list()
  structural_flags <- c(
    htmt_overlap = is.finite(esem$htmt_violations %||% NA_real_) && (esem$htmt_violations %||% 0) > 0,
    dominance_mismatch = is.finite(sd$correct_dominance %||% NA_real_) && (sd$correct_dominance %||% 1) < 1,
    cross_loading = is.finite(sd$no_large_cross_loading %||% NA_real_) && (sd$no_large_cross_loading %||% 1) < 1
  )
  esem_separation <- if (is.null(esem_result)) {
    "unavailable"
  } else if (!esem_admissible) {
    "unavailable_or_inadmissible"
  } else if (any(structural_flags, na.rm = TRUE)) {
    "separation_weak_or_mixed"
  } else {
    "separation_comparatively_favorable"
  }
  state <- if (pfa_grouping == "grouping_recoverable" && esem_separation == "separation_weak_or_mixed") {
    "grouping_recoverable__separation_weak"
  } else if (pfa_grouping == "grouping_weak" && esem_separation == "separation_weak_or_mixed") {
    "grouping_weak__esem_weak"
  } else if (pfa_grouping == "grouping_recoverable" && esem_separation == "separation_comparatively_favorable") {
    "grouping_recoverable__esem_favorable"
  } else if (grepl("unavailable", esem_separation, fixed = TRUE)) {
    paste0(pfa_grouping, "__esem_unavailable")
  } else {
    paste0(pfa_grouping, "__", esem_separation)
  }
  ari <- pfa$partition_agreement_ari %||% NA_real_
  partition_state <- if (!is.finite(ari)) {
    "unavailable"
  } else if (abs(ari - 1) <= sqrt(.Machine$double.eps)) {
    "exact"
  } else if (ari <= 0) {
    "at_or_below_chance"
  } else {
    "positive_incomplete"
  }
  out <- list(
    state = state,
    pfa_grouping = pfa_grouping,
    pfa_partition_state = partition_state,
    esem_separation = esem_separation,
    structural_flags = structural_flags,
    metrics = list(
      pfa_factor_presence_recovery = pfa$factor_presence_recovery %||% pfa$recovery_score %||% NA_real_,
      pfa_partition_agreement_ari = ari,
      pfa_clarity_score = pfa$clarity_score %||% NA_real_,
      esem_admissible = esem_admissible,
      esem_htmt_max = esem$htmt_max %||% NA_real_,
      esem_htmt_violations = esem$htmt_violations %||% NA_real_,
      esem_correct_dominance = sd$correct_dominance %||% NA_real_,
      esem_no_large_cross_loading = sd$no_large_cross_loading %||% NA_real_
    ),
    participant_based = FALSE,
    evidence_role = "pfa_esem_discrepancy",
    note = paste(
      "PFA and ESEM are complementary sample-free structural proxies; disagreement is retained rather than averaged away.",
      "ARI is the primary chance-adjusted partition-agreement descriptor; factor-presence recovery is retained as a separate coverage-style diagnostic."
    )
  )
  class(out) <- c("semantica_pfa_esem_discrepancy", "list")
  out
}

#' Profile ESEM evaluation telemetry without changing evaluation decisions
#'
#' Summarizes search/archive/final ESEM accounting recorded by the optimizer.
#' This is performance telemetry only; it does not skip, rerank, or reinterpret
#' any ESEM evaluation.
#'
#' @param result A SEMANTICA result containing `evaluation_telemetry`, or the
#'   telemetry list itself.
#' @return List containing event-level telemetry and stage summaries.
#' @export
semantica_esem_telemetry <- function(result) {
  telemetry <- result$evaluation_telemetry %||% result$optimization$evaluation_telemetry %||% result
  if (!is.list(telemetry)) stop("No ESEM evaluation telemetry is available.")
  events <- telemetry$esem_events %||% data.frame()
  if (!is.data.frame(events)) events <- as.data.frame(events, stringsAsFactors = FALSE)
  stage_summary <- if (nrow(events)) {
    stages <- unique(events$stage)
    do.call(rbind, lapply(stages, function(st) {
      z <- events[events$stage == st, , drop = FALSE]
      data.frame(
        stage = st,
        evaluations = nrow(z),
        cache_hits = sum(z$cache_hit, na.rm = TRUE),
        coalesced_requests = sum(z$coalesced_requests, na.rm = TRUE),
        elapsed_seconds = sum(z$elapsed_seconds, na.rm = TRUE),
        median_elapsed_seconds = if (any(is.finite(z$elapsed_seconds))) stats::median(z$elapsed_seconds[is.finite(z$elapsed_seconds)]) else NA_real_,
        converged = sum(z$converged, na.rm = TRUE),
        admissible = sum(z$admissible, na.rm = TRUE),
        failed_or_inadmissible = sum(!z$admissible, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else data.frame(
    stage=character(), evaluations=integer(), cache_hits=integer(), coalesced_requests=integer(),
    elapsed_seconds=numeric(), median_elapsed_seconds=numeric(), converged=integer(), admissible=integer(),
    failed_or_inadmissible=integer(), stringsAsFactors=FALSE
  )
  list(
    events = events,
    stage_summary = stage_summary,
    accounting = telemetry[setdiff(names(telemetry), "esem_events")],
    analysis_behavior_changed = FALSE,
    note = "Performance telemetry only; no ESEM evaluation is skipped or reranked by this function."
  )
}

.semantica_stochastic_superiority_vectors <- function(within, between) {
  within <- as.numeric(within); between <- as.numeric(between)
  within <- within[is.finite(within)]; between <- between[is.finite(between)]
  if (!length(within) || !length(between)) return(NA_real_)
  combined <- c(within, between)
  ranks <- rank(combined, ties.method = "average")
  n_w <- length(within); n_b <- length(between)
  u <- sum(ranks[seq_len(n_w)]) - n_w * (n_w + 1) / 2
  max(0, min(1, as.numeric(u / (n_w * n_b))))
}

.semantica_semantic_pair_vectors <- function(similarity_matrix, factor_assignment) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L || is.null(rownames(m)) || is.null(colnames(m))) {
    stop("'similarity_matrix' must be a named square matrix with at least two items.", call. = FALSE)
  }
  if (is.null(names(factor_assignment))) {
    stop("'factor_assignment' must be named by item ID.", call. = FALSE)
  }
  ids <- intersect(rownames(m), names(factor_assignment))
  if (length(ids) < 2L) stop("Fewer than two aligned item IDs are available.", call. = FALSE)
  m <- m[ids, ids, drop = FALSE]
  fa <- as.character(factor_assignment[ids])
  idx <- which(lower.tri(m), arr.ind = TRUE)
  vals <- m[lower.tri(m)]
  keep <- is.finite(vals) & !is.na(fa[idx[, 1L]]) & !is.na(fa[idx[, 2L]])
  idx <- idx[keep, , drop = FALSE]; vals <- vals[keep]
  same <- fa[idx[, 1L]] == fa[idx[, 2L]]
  list(
    ids = ids,
    factor_assignment = stats::setNames(fa, ids),
    within = vals[same],
    between = vals[!same]
  )
}

#' Resampling sensitivity for sample-free semantic separation
#'
#' Replaces the legacy random half-pair zeroing heuristic with two transparent
#' descriptive sensitivity analyses: a stratified bootstrap of the observed
#' within- and between-factor pair distributions, and an item jackknife that
#' removes one item at a time. Pairwise similarities share items and therefore
#' are not independent observations; the reported bootstrap intervals are
#' sensitivity intervals for the active semantic representation, not sampling
#' confidence intervals for a respondent population.
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Named vector mapping item IDs to intended factors.
#' @param reps Number of stratified pair bootstrap replicates.
#' @param seed Optional integer seed. The caller's RNG state is restored.
#' @return A list containing observed statistics, bootstrap sensitivity
#'   intervals, item-jackknife ranges, and explicit evidence/provenance notes.
#' @export
semantica_semantic_resampling_stability <- function(
    similarity_matrix, factor_assignment, reps = 1000L, seed = 1L) {
  reps <- suppressWarnings(as.integer(reps[1L]))
  if (!is.finite(reps) || reps < 1L) stop("'reps' must be a positive integer.", call. = FALSE)
  if (!is.null(seed)) {
    seed <- suppressWarnings(as.integer(seed[1L]))
    if (!is.finite(seed) || seed < 0L) stop("'seed' must be NULL or a non-negative integer.", call. = FALSE)
  }
  pv <- .semantica_semantic_pair_vectors(similarity_matrix, factor_assignment)
  within <- pv$within; between <- pv$between
  is_unidimensional <- length(unique(pv$factor_assignment)) == 1L || !length(between)

  observed <- list(
    stochastic_superiority = .semantica_stochastic_superiority_vectors(within, between),
    within_median = if (length(within)) stats::median(within) else NA_real_,
    between_median = if (length(between)) stats::median(between) else NA_real_,
    median_gap = if (length(within) && length(between)) stats::median(within) - stats::median(between) else NA_real_,
    n_within_pairs = length(within),
    n_between_pairs = length(between)
  )

  boot_fun <- function() {
    boot_A <- rep(NA_real_, reps)
    boot_gap <- rep(NA_real_, reps)
    boot_within <- rep(NA_real_, reps)
    for (b in seq_len(reps)) {
      wb <- if (length(within)) sample(within, length(within), replace = TRUE) else numeric(0L)
      bb <- if (length(between)) sample(between, length(between), replace = TRUE) else numeric(0L)
      boot_within[[b]] <- if (length(wb)) stats::median(wb) else NA_real_
      if (length(wb) && length(bb)) {
        boot_A[[b]] <- .semantica_stochastic_superiority_vectors(wb, bb)
        boot_gap[[b]] <- stats::median(wb) - stats::median(bb)
      }
    }
    list(A = boot_A, gap = boot_gap, within = boot_within)
  }
  boot <- if (is.null(seed)) boot_fun() else .semantica_with_task_seed(seed, boot_fun())
  interval <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(c(lower = NA_real_, median = NA_real_, upper = NA_real_))
    stats::setNames(
      as.numeric(stats::quantile(x, probs = c(.025, .50, .975), na.rm = TRUE, names = FALSE, type = 8)),
      c("lower", "median", "upper")
    )
  }

  m <- as.matrix(similarity_matrix)
  jack_rows <- lapply(pv$ids, function(drop_id) {
    keep <- setdiff(pv$ids, drop_id)
    if (length(keep) < 2L) return(NULL)
    sub <- m[keep, keep, drop = FALSE]
    fa <- pv$factor_assignment[keep]
    pair <- tryCatch(.semantica_semantic_pair_vectors(sub, fa), error = function(e) NULL)
    if (is.null(pair)) return(NULL)
    data.frame(
      omitted_item = drop_id,
      stochastic_superiority = .semantica_stochastic_superiority_vectors(pair$within, pair$between),
      within_median = if (length(pair$within)) stats::median(pair$within) else NA_real_,
      between_median = if (length(pair$between)) stats::median(pair$between) else NA_real_,
      median_gap = if (length(pair$within) && length(pair$between)) stats::median(pair$within) - stats::median(pair$between) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  jack_rows <- Filter(Negate(is.null), jack_rows)
  jack <- if (length(jack_rows)) do.call(rbind, jack_rows) else data.frame()
  range_or_na <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) c(min = NA_real_, max = NA_real_, range = NA_real_) else {
      rr <- range(x)
      c(min = rr[[1L]], max = rr[[2L]], range = diff(rr))
    }
  }

  structure(list(
    status = "computed",
    dimensionality_mode = if (is_unidimensional) "unidimensional" else "multidimensional",
    observed = observed,
    pair_bootstrap = list(
      reps = reps,
      stochastic_superiority_interval = interval(boot$A),
      median_gap_interval = interval(boot$gap),
      within_median_interval = interval(boot$within),
      method = "stratified_resampling_within_and_between_pair_distributions"
    ),
    item_jackknife = list(
      estimates = jack,
      stochastic_superiority_range = if (nrow(jack)) range_or_na(jack$stochastic_superiority) else range_or_na(numeric()),
      median_gap_range = if (nrow(jack)) range_or_na(jack$median_gap) else range_or_na(numeric()),
      within_median_range = if (nrow(jack)) range_or_na(jack$within_median) else range_or_na(numeric()),
      method = "leave_one_item_out"
    ),
    seed = seed,
    binary_stability_classification = NA,
    evidence_family = "embedding_semantic",
    participant_based = FALSE,
    note = paste(
      "Intervals quantify sensitivity of the active semantic representation, not respondent-population uncertainty.",
      "No universal stable/unstable cutoff is applied because pair similarities are dependent and the statistic is selection-conditioned."
    )
  ), class = c("semantica_semantic_resampling_stability", "list"))
}

#' Ensemble clustering consensus for semantic scale structure
#'
#' Provides a descriptive structural companion to cosine-based discrimination
#' without adding another ACO objective. Several deterministic agglomerative
#' clustering rules are applied to the same semantic similarity representation;
#' their co-assignment consensus and agreement with the intended factor labels
#' are summarized. Because all clusterings derive from the same embeddings,
#' this remains part of the embedding-semantic evidence family rather than an
#' independent validation source.
#'
#' @param similarity_matrix Named square semantic similarity matrix.
#' @param factor_assignment Named intended factor assignment for each item.
#' @param methods Agglomerative-linkage methods passed to [stats::hclust()].
#' @return A `semantica_cluster_consensus` object with algorithm-level intended
#'   recovery, between-algorithm agreement, and consensus co-assignment
#'   discrimination summaries.
#' @export
semantica_cluster_consensus <- function(
    similarity_matrix, factor_assignment,
    methods = c("average", "complete", "mcquitty")) {
  m <- as.matrix(similarity_matrix)
  if (nrow(m) != ncol(m) || nrow(m) < 2L || is.null(rownames(m)) || is.null(colnames(m))) {
    stop("'similarity_matrix' must be a named square matrix with at least two items.", call. = FALSE)
  }
  if (is.null(names(factor_assignment))) stop("'factor_assignment' must be named by item ID.", call. = FALSE)
  ids <- intersect(rownames(m), names(factor_assignment))
  fa <- as.character(factor_assignment[ids])
  keep <- !is.na(fa) & nzchar(fa)
  ids <- ids[keep]; fa <- fa[keep]
  if (length(ids) < 2L) stop("Fewer than two aligned items are available.", call. = FALSE)
  k <- length(unique(fa))
  if (k < 2L) {
    return(structure(list(
      available = FALSE,
      status = "not_applicable_unidimensional",
      evidence_family = "embedding_semantic",
      participant_based = FALSE,
      note = "Clustering consensus is not informative for a one-factor intended structure."
    ), class = "semantica_cluster_consensus"))
  }
  if (k >= length(ids)) {
    return(structure(list(
      available = FALSE,
      status = "insufficient_items_per_cluster",
      evidence_family = "embedding_semantic",
      participant_based = FALSE,
      note = "The intended number of factors must be smaller than the number of items."
    ), class = "semantica_cluster_consensus"))
  }
  m <- m[ids, ids, drop = FALSE]
  m <- (m + t(m)) / 2
  if (any(!is.finite(m))) stop("'similarity_matrix' contains non-finite aligned values.", call. = FALSE)
  diag(m) <- 1
  # `pmax()` drops matrix dimensions on some R versions.  Preserve the named
  # square structure explicitly because `as.dist()` and the diagonal guard
  # below require a matrix, not a flattened vector.
  dmat <- 1 - m
  dmat[dmat < 0] <- 0
  diag(dmat) <- 0
  d <- stats::as.dist(dmat)
  methods <- unique(as.character(methods))
  methods <- methods[nzchar(methods)]
  if (!length(methods)) stop("At least one clustering method is required.", call. = FALSE)

  clusterings <- lapply(methods, function(method) {
    fit <- stats::hclust(d, method = method)
    stats::cutree(fit, k = k)
  })
  names(clusterings) <- methods

  comb2 <- function(x) ifelse(x >= 2, x * (x - 1) / 2, 0)
  adjusted_rand <- function(a, b) {
    tab <- table(a, b)
    n <- sum(tab)
    if (n < 2L) return(NA_real_)
    total <- comb2(n)
    if (total <= 0) return(NA_real_)
    index <- sum(comb2(tab))
    row_sum <- sum(comb2(rowSums(tab)))
    col_sum <- sum(comb2(colSums(tab)))
    expected <- row_sum * col_sum / total
    max_index <- 0.5 * (row_sum + col_sum)
    denom <- max_index - expected
    if (abs(denom) <= sqrt(.Machine$double.eps)) {
      return(if (abs(index - max_index) <= sqrt(.Machine$double.eps)) 1 else NA_real_)
    }
    (index - expected) / denom
  }

  intended <- stats::setNames(fa, ids)
  algorithm_table <- data.frame(
    method = methods,
    intended_adjusted_rand = vapply(clusterings, adjusted_rand, numeric(1L), b = intended),
    stringsAsFactors = FALSE
  )

  n <- length(ids)
  consensus <- matrix(0, n, n, dimnames = list(ids, ids))
  for (z in clusterings) consensus <- consensus + outer(z, z, `==`)
  consensus <- consensus / length(clusterings)
  diag(consensus) <- 1

  pair <- .semantica_semantic_pair_vectors(consensus, intended)
  within_consensus <- if (length(pair$within)) mean(pair$within) else NA_real_
  between_consensus <- if (length(pair$between)) mean(pair$between) else NA_real_
  consensus_gap <- if (is.finite(within_consensus) && is.finite(between_consensus)) within_consensus - between_consensus else NA_real_
  consensus_A <- .semantica_stochastic_superiority_vectors(pair$within, pair$between)

  algorithm_agreement <- NA_real_
  if (length(clusterings) >= 2L) {
    cmb <- utils::combn(seq_along(clusterings), 2L)
    aris <- apply(cmb, 2L, function(ix) adjusted_rand(clusterings[[ix[1L]]], clusterings[[ix[2L]]]))
    aris <- aris[is.finite(aris)]
    if (length(aris)) algorithm_agreement <- mean(aris)
  }

  structure(list(
    available = TRUE,
    status = "descriptive_embedding_cluster_consensus",
    evidence_family = "embedding_semantic",
    participant_based = FALSE,
    methods = methods,
    n_items = n,
    n_factors = k,
    algorithm_table = algorithm_table,
    mean_intended_adjusted_rand = if (any(is.finite(algorithm_table$intended_adjusted_rand))) mean(algorithm_table$intended_adjusted_rand, na.rm = TRUE) else NA_real_,
    mean_between_algorithm_adjusted_rand = algorithm_agreement,
    consensus_matrix = consensus,
    within_factor_consensus = within_consensus,
    between_factor_consensus = between_consensus,
    within_between_consensus_gap = consensus_gap,
    consensus_stochastic_superiority = consensus_A,
    interpretation = paste(
      "Consensus describes whether multiple deterministic clustering views of the same embedding representation",
      "recover the intended grouping. It is complementary semantic structure evidence, not participant validation",
      "and not independent of the embeddings from which it is derived."
    )
  ), class = "semantica_cluster_consensus")
}


#' Compare frozen-item semantic representations across embedding backends
#'
#' Compares two or more named similarity matrices computed from the exact same
#' item pool. The function is intentionally diagnostic: it does not average
#' embeddings, select a preferred backend, transform the active representation,
#' or alter ACO/PFA/ESEM scores. Its purpose is to expose backend sensitivity
#' while holding item wording fixed.
#'
#' @param similarity_matrices Named list of square, named similarity matrices.
#'   Every matrix must contain the same item identifiers; matrices are reordered
#'   to the first representation before comparison.
#' @param factor_assignment Named vector mapping frozen item IDs to intended
#'   factors.
#' @param selected_items Optional named list of selected item IDs, one vector per
#'   representation. When supplied, pairwise selection Jaccard is reported.
#' @param frozen_selection Optional item-ID vector selected using one designated
#'   representation and then evaluated unchanged across every supplied
#'   representation. This is a semantic holdout/generalization diagnostic; it
#'   never reselects items on the evaluation representations.
#' @param top_fraction Fraction of unique item pairs used for top-pair overlap.
#' @return A `semantica_embedding_representation_comparison` containing
#'   per-representation semantic diagnostics and pairwise geometry agreement.
#' @references Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and
#'   address taxonomic incommensurability in psychological measurement. *Nature
#'   Human Behaviour, 9*, 944-954. \doi{10.1038/s41562-024-02089-y}
#'
#'   Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the CL
#'   common language effect size statistics of McGraw and Wong. *Journal of
#'   Educational and Behavioral Statistics, 25*(2), 101-132.
#'   \doi{10.3102/10769986025002101}
#' @export
semantica_compare_embedding_representations <- function(
    similarity_matrices, factor_assignment, selected_items = NULL,
    top_fraction = 0.05, frozen_selection = NULL) {
  if (!is.list(similarity_matrices) || length(similarity_matrices) < 2L) {
    stop("'similarity_matrices' must be a named list with at least two matrices.", call. = FALSE)
  }
  nm <- names(similarity_matrices)
  if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    stop("'similarity_matrices' must have unique non-empty representation names.", call. = FALSE)
  }
  top_fraction <- suppressWarnings(as.numeric(top_fraction[[1L]]))
  if (!is.finite(top_fraction) || top_fraction <= 0 || top_fraction >= 1) {
    stop("'top_fraction' must be a finite number strictly between 0 and 1.", call. = FALSE)
  }
  if (is.null(names(factor_assignment))) {
    stop("'factor_assignment' must be named by frozen item ID.", call. = FALSE)
  }

  first <- as.matrix(similarity_matrices[[1L]])
  if (nrow(first) != ncol(first) || nrow(first) < 3L ||
      is.null(rownames(first)) || is.null(colnames(first))) {
    stop("Every similarity matrix must be a named square matrix with at least three items.", call. = FALSE)
  }
  ids <- rownames(first)
  if (!setequal(ids, colnames(first)) || anyDuplicated(ids)) {
    stop("Similarity matrix row/column names must identify the same unique items.", call. = FALSE)
  }
  if (!all(ids %in% names(factor_assignment))) {
    stop("'factor_assignment' is missing one or more frozen item IDs.", call. = FALSE)
  }
  fa <- stats::setNames(as.character(factor_assignment[ids]), ids)
  if (anyNA(fa) || any(!nzchar(fa))) {
    stop("Factor assignments for all frozen items must be non-missing and non-empty.", call. = FALSE)
  }
  if (!is.null(frozen_selection)) {
    frozen_selection <- unique(as.character(frozen_selection))
    if (!length(frozen_selection) || anyNA(frozen_selection) || any(!nzchar(frozen_selection))) {
      stop("'frozen_selection' must contain one or more non-empty item IDs.", call. = FALSE)
    }
    if (any(!frozen_selection %in% ids)) {
      stop("'frozen_selection' contains IDs outside the frozen item pool.", call. = FALSE)
    }
    # Each intended factor represented in the frozen selection remains explicit
    # evidence; no missing factor is silently inferred or repaired.
    selected_factors <- unique(fa[frozen_selection])
    if (!length(selected_factors)) {
      stop("'frozen_selection' has no valid factor assignments.", call. = FALSE)
    }
  }

  mats <- lapply(similarity_matrices, function(z) {
    z <- as.matrix(z)
    if (nrow(z) != ncol(z) || is.null(rownames(z)) || is.null(colnames(z)) ||
        !setequal(rownames(z), ids) || !setequal(colnames(z), ids)) {
      stop("All representations must contain exactly the same frozen item IDs.", call. = FALSE)
    }
    z <- z[ids, ids, drop = FALSE]
    if (any(!is.finite(z))) stop("Similarity matrices must contain only finite values.", call. = FALSE)
    if (!isTRUE(all.equal(z, t(z), tolerance = 1e-8))) {
      stop("Similarity matrices must be symmetric.", call. = FALSE)
    }
    z
  })
  names(mats) <- nm

  per_representation <- do.call(rbind, lapply(nm, function(name) {
    m <- mats[[name]]
    diag <- .cosine_diagnostics(m, fa)
    discr <- semantica_semantic_discrimination(m, fa)
    pair <- .semantica_semantic_pair_vectors(m, fa)
    within_median <- if (length(pair$within)) stats::median(pair$within) else NA_real_
    between_median <- if (length(pair$between)) stats::median(pair$between) else NA_real_
    robust_gap <- if (is.finite(within_median) && is.finite(between_median)) {
      within_median - between_median
    } else NA_real_
    data.frame(
      representation = name,
      n_items = nrow(m),
      offdiag_mean = diag$offdiag_mean %||% NA_real_,
      common_direction_strength = diag$common_direction_strength %||% NA_real_,
      effective_rank = diag$effective_rank %||% NA_real_,
      effective_rank_ratio = diag$effective_rank_ratio %||% NA_real_,
      top_eigen_share = diag$top_eigen_share %||% NA_real_,
      stochastic_superiority = discr$estimate %||% NA_real_,
      within_median = within_median,
      between_median = between_median,
      robust_median_gap = robust_gap,
      stringsAsFactors = FALSE
    )
  }))
  rownames(per_representation) <- NULL

  lower_vectors <- lapply(mats, function(m) m[lower.tri(m)])
  n_pairs <- length(lower_vectors[[1L]])
  top_k <- max(1L, min(n_pairs, as.integer(ceiling(top_fraction * n_pairs))))
  top_sets <- lapply(lower_vectors, function(v) order(v, decreasing = TRUE)[seq_len(top_k)])
  jaccard <- function(a, b) {
    u <- union(a, b)
    if (!length(u)) return(NA_real_)
    length(intersect(a, b)) / length(u)
  }

  cmb <- utils::combn(nm, 2L, simplify = FALSE)
  pairwise_geometry <- do.call(rbind, lapply(cmb, function(pair_names) {
    a <- pair_names[[1L]]; b <- pair_names[[2L]]
    va <- lower_vectors[[a]]; vb <- lower_vectors[[b]]
    sel_j <- NA_real_
    if (!is.null(selected_items)) {
      if (!is.list(selected_items) || is.null(names(selected_items))) {
        stop("'selected_items' must be NULL or a named list keyed by representation.", call. = FALSE)
      }
      if (a %in% names(selected_items) && b %in% names(selected_items)) {
        sa <- unique(as.character(selected_items[[a]]))
        sb <- unique(as.character(selected_items[[b]]))
        if (any(!sa %in% ids) || any(!sb %in% ids)) {
          stop("'selected_items' contains IDs outside the frozen item pool.", call. = FALSE)
        }
        sel_j <- jaccard(sa, sb)
      }
    }
    data.frame(
      representation_a = a,
      representation_b = b,
      pairwise_pearson = suppressWarnings(stats::cor(va, vb, method = "pearson")),
      pairwise_spearman = suppressWarnings(stats::cor(va, vb, method = "spearman")),
      top_pair_jaccard = jaccard(top_sets[[a]], top_sets[[b]]),
      selected_item_jaccard = sel_j,
      stringsAsFactors = FALSE
    )
  }))
  rownames(pairwise_geometry) <- NULL

  frozen_selection_evaluation <- NULL
  if (!is.null(frozen_selection)) {
    frozen_selection_evaluation <- do.call(rbind, lapply(nm, function(name) {
      sub <- mats[[name]][frozen_selection, frozen_selection, drop = FALSE]
      sub_fa <- fa[frozen_selection]
      pair <- .semantica_semantic_pair_vectors(sub, sub_fa)
      within_median <- if (length(pair$within)) stats::median(pair$within) else NA_real_
      between_median <- if (length(pair$between)) stats::median(pair$between) else NA_real_
      discr <- if (length(unique(sub_fa)) >= 2L && length(pair$within) && length(pair$between)) {
        .semantica_robust_relative_gap(pair$within, pair$between)
      } else {
        list(stochastic_superiority = NA_real_, median_gap = NA_real_,
             standardized_gap = NA_real_, conservative_score = NA_real_)
      }
      factor_rel <- if (length(unique(sub_fa)) >= 2L) {
        .semantica_relative_semantic_components(sub, sub_fa, unique(as.character(sub_fa)))
      } else NULL
      data.frame(
        representation = name,
        n_selected = length(frozen_selection),
        n_factors_represented = length(unique(sub_fa)),
        within_median = within_median,
        between_median = between_median,
        robust_median_gap = discr$median_gap %||% NA_real_,
        stochastic_superiority = discr$stochastic_superiority %||% NA_real_,
        standardized_robust_gap = discr$standardized_gap %||% NA_real_,
        relative_conservative_score = discr$conservative_score %||% NA_real_,
        weakest_factor_relative_score = factor_rel$weakest_factor_score %||% NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    rownames(frozen_selection_evaluation) <- NULL
  }

  structure(list(
    status = "descriptive_frozen_item_backend_robustness",
    evidence_family = "embedding_semantic",
    participant_based = FALSE,
    frozen_item_required = TRUE,
    item_ids = ids,
    factor_assignment = fa,
    top_fraction = top_fraction,
    top_pairs = top_k,
    per_representation = per_representation,
    pairwise_geometry = pairwise_geometry,
    frozen_selection = frozen_selection,
    frozen_selection_evaluation = frozen_selection_evaluation,
    selection_conditioned = !is.null(frozen_selection),
    interpretation = paste(
      "Use this diagnostic only when item wording is held fixed across representations.",
      if (!is.null(frozen_selection))
        "The frozen selection is evaluated unchanged on every representation, so evaluation representations do not re-optimize the item set."
      else
        "No frozen selected form was supplied; results describe whole-pool representation agreement.",
      "Backend disagreement is robustness evidence; SEMANTICA does not average representations,",
      "choose a preferred backend, or treat cross-backend agreement as participant validation."
    )
  ), class = c("semantica_embedding_representation_comparison", "list"))
}
