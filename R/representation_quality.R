# Internal representation-quality helpers.

#' Describe embedding-model instruction capabilities
#'
#' Defines how SEMANTICA should prepare text for an embedding model without
#' changing downstream psychometric criteria. This is a provider/model I/O
#' contract, not a score-calibration mechanism.
#'
#' @param instruction_mode `"none"` or `"prefix"`. `"prefix"` prepends a
#'   task-specific text instruction; `"none"` leaves text unchanged.
#' @param task_map Optional named character vector mapping SEMANTICA analysis
#'   intents (for example `psychometric_similarity`) to the model's documented
#'   task vocabulary.
#' @param prefix_template Template used when `instruction_mode = "prefix"`.
#'   It must contain `{task}`.
#' @param requires_instruction Logical metadata flag used for diagnostics only.
#' @param note Optional human-readable capability note.
#' @return A `semantica_embedding_spec` object.
#' @export
semantica_embedding_spec <- function(
    instruction_mode = c("none", "prefix"),
    task_map = NULL,
    prefix_template = "{task}: ",
    requires_instruction = FALSE,
    note = NULL) {
  instruction_mode <- match.arg(instruction_mode)
  if (!is.null(task_map)) {
    task_map_names <- names(task_map)
    if (is.null(task_map_names) || any(!nzchar(task_map_names))) {
      stop("'task_map' must be NULL or a named character vector.", call. = FALSE)
    }
    # Preserve the intent -> provider-task mapping explicitly. Some coercion
    # paths drop names from atomic vectors; losing them would silently turn a
    # registered/model-specific capability into the generic task label.
    task_map <- stats::setNames(as.character(task_map), task_map_names)
  }
  prefix_template <- as.character(prefix_template[[1L]])
  if (identical(instruction_mode, "prefix") && !grepl("\\{task\\}", prefix_template)) {
    stop("'prefix_template' must contain '{task}' when instruction_mode='prefix'.", call. = FALSE)
  }
  structure(
    list(
      instruction_mode = instruction_mode,
      task_map = task_map,
      prefix_template = prefix_template,
      requires_instruction = isTRUE(requires_instruction),
      note = if (is.null(note)) NULL else as.character(note[[1L]])
    ),
    class = c("semantica_embedding_spec", "list")
  )
}

.semantica_registered_embedding_spec <- function(model) {
  model_norm <- tolower(trimws(as.character(model %||% "")))
  if (grepl("nomic[-_/]?embed[-_]?text|nomic-embed-text", model_norm)) {
    out <- semantica_embedding_spec(
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
        "Nomic Embed Text uses documented task prefixes. SEMANTICA maps the",
        "symmetric psychometric-similarity intent to its clustering task."
      )
    )
    attr(out, "source") <- "registered_model_capability"
    # Backward-compatible provenance for the concrete instruction itself.
    # `capability_source` remains the richer v0.3 field.
    attr(out, "instruction_source") <- "model_card"
    return(out)
  }
  out <- semantica_embedding_spec(
    instruction_mode = "none",
    requires_instruction = FALSE,
    note = paste(
      "No model-specific instruction contract is registered. SEMANTICA uses",
      "the generic symmetric embedding path without changing score thresholds."
    )
  )
  attr(out, "source") <- "generic_safe_default"
  attr(out, "instruction_source") <- "none"
  out
}

.semantica_embedding_policy <- function(model, task = "auto", instruction = NULL,
                                        embedding_spec = NULL) {
  model <- tolower(trimws(as.character(model %||% "")))
  task <- tolower(trimws(as.character(task %||% "auto")))
  valid <- c("auto", "psychometric_similarity", "clustering", "classification",
             "search_document", "search_query", "none")
  if (!task %in% valid) stop("Unsupported embedding task: ", task)
  if (!is.null(instruction)) {
    instruction <- as.character(instruction[[1L]])
    if (!nzchar(trimws(instruction))) instruction <- NULL
  }

  if (is.null(embedding_spec)) {
    spec <- .semantica_registered_embedding_spec(model)
    capability_source <- attr(spec, "source") %||% "generic_safe_default"
    automatic_instruction_source <- attr(spec, "instruction_source") %||% "none"
  } else {
    if (!inherits(embedding_spec, "semantica_embedding_spec")) {
      stop("'embedding_spec' must be created by semantica_embedding_spec().", call. = FALSE)
    }
    spec <- embedding_spec
    capability_source <- "user_embedding_spec"
    automatic_instruction_source <- "embedding_spec"
  }

  resolved <- if (task == "auto") "psychometric_similarity" else task
  analysis_intent <- if (task %in% c("auto", "psychometric_similarity")) {
    "psychometric_similarity"
  } else {
    task
  }
  mapped_task <- resolved
  if (!is.null(spec$task_map) && resolved %in% names(spec$task_map)) {
    mapped_task <- unname(spec$task_map[[resolved]])
  }

  prefix <- NULL
  source <- "none"
  if (!is.null(instruction)) {
    prefix <- if (grepl("[[:space:]]$", instruction)) instruction else paste0(instruction, " ")
    # Keep the long-standing provenance label used by serialized results/tests.
    # The more detailed origin is available separately as `capability_source`.
    source <- "user"
  } else if (!identical(resolved, "none") && identical(spec$instruction_mode, "prefix")) {
    prefix <- gsub("\\{task\\}", mapped_task, spec$prefix_template)
    source <- automatic_instruction_source
  }

  # `provider_task` reports what SEMANTICA can truthfully resolve from the
  # task contract. An explicit free-form instruction has precedence and is not
  # reverse-engineered into a provider task when the requested task is auto/
  # psychometric_similarity; this preserves the established provenance contract.
  provider_task <- NULL
  if (is.null(instruction)) {
    if (!identical(mapped_task, "psychometric_similarity") &&
        !identical(mapped_task, "none")) {
      provider_task <- mapped_task
    }
  } else if (!task %in% c("auto", "psychometric_similarity", "none")) {
    provider_task <- mapped_task
  }
  instruction_fingerprint <- if (!is.null(prefix) && exists(".semantica_object_md5", mode = "function")) {
    .semantica_object_md5(enc2utf8(prefix))
  } else {
    NA_character_
  }
  capability_fingerprint <- if (exists(".semantica_object_md5", mode = "function")) {
    .semantica_object_md5(unclass(spec))
  } else {
    NA_character_
  }
  list(
    model = model,
    requested_task = task,
    resolved_task = mapped_task,
    analysis_intent = analysis_intent,
    provider_task = provider_task,
    prefix = prefix,
    source = source,
    instruction_applied = !is.null(prefix),
    instruction_fingerprint = instruction_fingerprint,
    model_requires_instruction = isTRUE(spec$requires_instruction),
    capability_source = capability_source,
    capability_fingerprint = capability_fingerprint,
    capability_note = spec$note %||% NULL,
    embedding_spec = spec
  )
}

.semantica_prepare_embedding_texts <- function(session, texts) {
  policy <- .semantica_embedding_policy(
    session$embed_model,
    session$embedding_task %||% "auto",
    session$embedding_instruction %||% NULL,
    session$embedding_spec %||% NULL
  )
  out <- as.character(texts)
  if (!is.null(policy$prefix)) {
    known <- "^(search_document|search_query|clustering|classification):\\s*"
    # A semantic matrix should not silently mix task-specific embedding spaces.
    # Normalize any recognized pre-existing task prefix to the one resolved for
    # this session. Users who need literal pre-instructed text can set task=none.
    out <- sub(known, "", out, ignore.case = TRUE, perl = TRUE)
    out <- paste0(policy$prefix, out)
  }
  # Keep the prepared text vector attribute-free. Embedding-policy provenance is
  # recorded explicitly in session/result diagnostics and cache keys; attaching
  # metadata to a character vector can leak surprising attributes into callers
  # and provider serialization paths.
  out
}

.semantica_cosine_cross <- function(a, b) {
  a <- as.matrix(a); b <- as.matrix(b)
  an <- sqrt(rowSums(a^2)); bn <- sqrt(rowSums(b^2))
  a <- a / pmax(an, .Machine$double.eps)
  b <- b / pmax(bn, .Machine$double.eps)
  a %*% t(b)
}

# Mean-centering sensitivity helper. The active/raw embedding representation
# remains primary everywhere in SEMANTICA. This transformation is used only to
# ask whether an automatic content-exclusion decision survives a plausible
# representation perturbation; it never contributes to the ACO objective.
.semantica_centered_cosine_cross <- function(a, b, centroid) {
  a <- as.matrix(a); b <- as.matrix(b)
  centroid <- as.numeric(centroid)
  if (length(centroid) != ncol(a) || ncol(a) != ncol(b)) {
    stop("Centered cosine sensitivity requires a common embedding dimensionality.", call. = FALSE)
  }
  ac <- sweep(a, 2L, centroid, FUN = "-")
  bc <- sweep(b, 2L, centroid, FUN = "-")
  .semantica_cosine_cross(ac, bc)
}


.semantica_robust_content_guard <- function(
    raw_factor_mismatch, centered_factor_mismatch,
    raw_exclusion_conflict, centered_exclusion_conflict) {
  vals <- list(
    raw_factor_mismatch, centered_factor_mismatch,
    raw_exclusion_conflict, centered_exclusion_conflict
  )
  n <- length(vals[[1L]])
  if (any(vapply(vals, length, integer(1L)) != n)) {
    stop("Content-guard evidence vectors must have matching lengths.", call. = FALSE)
  }
  clean_flag <- function(x) !is.na(x) & as.logical(x)
  raw_factor_mismatch <- clean_flag(raw_factor_mismatch)
  centered_factor_mismatch <- clean_flag(centered_factor_mismatch)
  raw_exclusion_conflict <- clean_flag(raw_exclusion_conflict)
  centered_exclusion_conflict <- clean_flag(centered_exclusion_conflict)

  raw_conflict <- raw_factor_mismatch | raw_exclusion_conflict
  robust_factor_conflict <- raw_factor_mismatch & centered_factor_mismatch
  robust_exclusion_conflict <- raw_exclusion_conflict & centered_exclusion_conflict
  robust_conflict <- robust_factor_conflict | robust_exclusion_conflict

  list(
    raw_pass = !raw_conflict,
    robust_pass = !robust_conflict,
    robust_factor_conflict = robust_factor_conflict,
    robust_exclusion_conflict = robust_exclusion_conflict,
    sensitivity = ifelse(
      raw_conflict & !robust_conflict,
      "raw_exclusion_not_robust_to_centering_retained",
      ifelse(
        robust_conflict,
        "raw_exclusion_confirmed_by_centering",
        "not_triggered"
      )
    )
  )
}

.semantica_classify_alignment_margins <- function(margins, groups = NULL, min_reference = 2L) {
  margins <- as.numeric(margins)
  n <- length(margins)
  if (is.null(groups)) groups <- rep("all", n)
  groups <- as.character(groups)
  out <- list(
    scale = rep(NA_real_, n),
    clear_mismatch = rep(FALSE, n),
    status = rep(NA_character_, n)
  )
  finite <- is.finite(margins)
  out$status[finite & margins >= 0] <- "aligned"
  out$status[finite & margins < 0] <- "ambiguous"

  # Guard calibration uses the typical *positive* separation achieved by items
  # that the assigned construct already wins. If a group has too few positively
  # separated items, SEMANTICA has insufficient evidence to turn a negative
  # top-rank margin into an automatic exclusion, so it remains ambiguous. This
  # is intentionally conservative and avoids universal cosine cutoffs.
  for (g in unique(groups[!is.na(groups)])) {
    ii <- which(groups == g & finite)
    if (!length(ii)) next
    positive <- margins[ii][margins[ii] > sqrt(.Machine$double.eps)]
    if (length(positive) < as.integer(min_reference)) next
    scale_g <- stats::median(positive, na.rm = TRUE)
    if (!is.finite(scale_g) || scale_g <= sqrt(.Machine$double.eps)) next
    out$scale[ii] <- scale_g
    clear <- margins[ii] < -scale_g
    out$clear_mismatch[ii] <- clear
    out$status[ii[clear]] <- "clear_mismatch"
  }
  out
}

.semantica_definition_alignment <- function(items_tbl, embeddings, factors, embed_session,
                                             cache = TRUE, cache_dir = NULL,
                                             cache_namespace = NULL, batch_size = 64L,
                                             exclusions = NULL) {
  fail <- function(note) list(available = FALSE, note = note, table = NULL)
  if (!is.list(factors) || is.null(names(factors))) {
    return(fail("factor definitions unavailable"))
  }

  x <- as.data.frame(items_tbl, stringsAsFactors = FALSE)
  id_col <- if ("item_id" %in% names(x)) "item_id" else if ("ID" %in% names(x)) "ID" else NULL
  factor_col <- if ("factor" %in% names(x)) "factor" else if ("Dimension" %in% names(x)) "Dimension" else NULL
  facet_col <- if ("Facet" %in% names(x)) "Facet" else if ("facet" %in% names(x)) "facet" else NULL
  if (is.null(id_col) || is.null(factor_col)) {
    return(fail("item identifiers/factor labels unavailable"))
  }

  f_names <- names(factors)
  f_desc <- vapply(
    factors,
    function(z) trimws(as.character(z$description %||% "")),
    character(1L)
  )
  if (any(!nzchar(f_desc))) {
    return(fail("one or more factor definitions are empty"))
  }

  refs <- data.frame(
    ref_id = paste0("factor::", f_names),
    ref_text = paste(f_names, f_desc, sep = ". "),
    stringsAsFactors = FALSE
  )
  ref_emb <- semantica_embed(
    refs, embed_session,
    text_col = "ref_text", id_col = "ref_id",
    batch_size = batch_size, normalize = TRUE, cache = cache,
    cache_dir = cache_dir,
    cache_namespace = paste0(cache_namespace %||% "default", "::definitions"),
    verbose = FALSE
  )$embeddings

  ids <- as.character(x[[id_col]])
  common <- intersect(ids, rownames(embeddings))
  if (!length(common)) return(fail("no item embeddings match metadata"))

  item_emb_common <- embeddings[common, , drop = FALSE]
  sim <- .semantica_cosine_cross(item_emb_common, ref_emb)
  colnames(sim) <- f_names
  rownames(sim) <- common
  assigned <- as.character(x[[factor_col]][match(common, ids)])
  top <- f_names[max.col(sim, ties.method = "first")]
  assigned_score <- vapply(seq_along(common), function(i) {
    if (assigned[i] %in% f_names) sim[i, assigned[i]] else NA_real_
  }, numeric(1L))
  competitor <- vapply(seq_along(common), function(i) {
    vals <- sim[i, setdiff(f_names, assigned[i]), drop = TRUE]
    if (!length(vals)) NA_real_ else max(vals, na.rm = TRUE)
  }, numeric(1L))
  factor_margin <- assigned_score - competitor

  # A second view is computed solely for exclusion robustness. We subtract the
  # item-pool centroid from both items and construct prototypes, then recompute
  # relative alignment. Raw geometry remains the authoritative representation;
  # a centered result can only *withhold* an automatic exclusion when the two
  # views disagree. It cannot improve scores or create an exclusion by itself.
  alignment_centroid <- colMeans(item_emb_common)
  sim_centered <- .semantica_centered_cosine_cross(
    item_emb_common, ref_emb, alignment_centroid
  )
  colnames(sim_centered) <- f_names
  rownames(sim_centered) <- common
  top_centered <- f_names[max.col(sim_centered, ties.method = "first")]
  assigned_score_centered <- vapply(seq_along(common), function(i) {
    if (assigned[i] %in% f_names) sim_centered[i, assigned[i]] else NA_real_
  }, numeric(1L))
  competitor_centered <- vapply(seq_along(common), function(i) {
    vals <- sim_centered[i, setdiff(f_names, assigned[i]), drop = TRUE]
    if (!length(vals)) NA_real_ else max(vals, na.rm = TRUE)
  }, numeric(1L))
  factor_margin_centered <- assigned_score_centered - competitor_centered

  out <- data.frame(
    item_id = common,
    semantica_assigned_factor = assigned,
    semantica_top_factor = top,
    semantica_factor_score = assigned_score,
    semantica_factor_margin = factor_margin,
    semantica_factor_margin_scale = NA_real_,
    semantica_factor_aligned = assigned == top,
    semantica_factor_clear_mismatch = FALSE,
    semantica_factor_alignment_status = NA_character_,
    semantica_top_factor_centered = top_centered,
    semantica_factor_score_centered = assigned_score_centered,
    semantica_factor_margin_centered = factor_margin_centered,
    semantica_factor_margin_scale_centered = NA_real_,
    semantica_factor_clear_mismatch_centered = FALSE,
    semantica_factor_alignment_status_centered = NA_character_,
    semantica_factor_guard_agreement = NA_character_,
    semantica_exclusion_score = NA_real_,
    semantica_contrast_margin = NA_real_,
    semantica_exclusion_conflict = FALSE,
    semantica_exclusion_score_centered = NA_real_,
    semantica_contrast_margin_centered = NA_real_,
    semantica_exclusion_conflict_centered = FALSE,
    semantica_content_guard_pass_raw = TRUE,
    semantica_content_guard_pass = TRUE,
    semantica_content_guard_sensitivity = "not_triggered",
    semantica_top_facet = NA_character_,
    semantica_facet_score = NA_real_,
    semantica_facet_margin = NA_real_,
    semantica_facet_margin_scale = NA_real_,
    semantica_facet_aligned = NA,
    semantica_facet_clear_mismatch = NA,
    semantica_facet_alignment_status = NA_character_,
    stringsAsFactors = FALSE
  )

  # Rank-only alignment is useful diagnostically but too brittle for automatic
  # exclusion. Guarding therefore calibrates a negative margin against the
  # typical positive separation achieved by correctly ranked items assigned to
  # the same factor. If that positive reference is not estimable, the mismatch
  # stays ambiguous instead of being excluded.
  factor_class <- .semantica_classify_alignment_margins(
    factor_margin, assigned, min_reference = 2L
  )
  out$semantica_factor_margin_scale <- factor_class$scale
  out$semantica_factor_clear_mismatch <- factor_class$clear_mismatch
  out$semantica_factor_alignment_status <- factor_class$status
  factor_class_centered <- .semantica_classify_alignment_margins(
    factor_margin_centered, assigned, min_reference = 2L
  )
  out$semantica_factor_margin_scale_centered <- factor_class_centered$scale
  out$semantica_factor_clear_mismatch_centered <- factor_class_centered$clear_mismatch
  out$semantica_factor_alignment_status_centered <- factor_class_centered$status
  out$semantica_factor_guard_agreement <- ifelse(
    out$semantica_factor_clear_mismatch & out$semantica_factor_clear_mismatch_centered,
    "clear_mismatch_agreement",
    ifelse(
      xor(out$semantica_factor_clear_mismatch, out$semantica_factor_clear_mismatch_centered),
      "preprocessing_disagreement",
      "no_clear_mismatch_agreement"
    )
  )

  # Contrast the assigned construct with concepts the analyst explicitly says
  # should *not* define it. These are embedded as individual negative
  # prototypes; no universal cosine cutoff is used. A conflict is present only
  # when an excluded concept is closer than the assigned construct definition.
  inferred_exclusions <- stats::setNames(vector("list", length(f_names)), f_names)
  for (f in f_names) {
    z <- factors[[f]]
    vals <- z$forbidden %||% z$exclusions %||% character(0L)
    inferred_exclusions[[f]] <- unique(as.character(vals))
  }
  if (!is.null(exclusions) && is.list(exclusions)) {
    for (f in intersect(names(exclusions), f_names)) {
      inferred_exclusions[[f]] <- unique(c(
        inferred_exclusions[[f]], as.character(exclusions[[f]])
      ))
    }
  }
  for (f in f_names) {
    vals <- inferred_exclusions[[f]]
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    if (!length(vals)) next
    er <- data.frame(
      ref_id = paste0("exclude::", f, "::", seq_along(vals)),
      ref_text = paste(f, "excluded concept", vals, sep = ". "),
      stringsAsFactors = FALSE
    )
    ee <- semantica_embed(
      er, embed_session,
      text_col = "ref_text", id_col = "ref_id",
      batch_size = batch_size, normalize = TRUE, cache = cache,
      cache_dir = cache_dir,
      cache_namespace = paste0(cache_namespace %||% "default", "::definitions"),
      verbose = FALSE
    )$embeddings
    ii <- which(assigned == f)
    if (!length(ii)) next
    es <- .semantica_cosine_cross(item_emb_common[ii, , drop = FALSE], ee)
    ex_score <- apply(es, 1L, max, na.rm = TRUE)
    out$semantica_exclusion_score[ii] <- ex_score
    out$semantica_contrast_margin[ii] <- assigned_score[ii] - ex_score
    out$semantica_exclusion_conflict[ii] <- is.finite(ex_score) &
      is.finite(assigned_score[ii]) & ex_score > assigned_score[ii]

    es_centered <- .semantica_centered_cosine_cross(
      item_emb_common[ii, , drop = FALSE], ee, alignment_centroid
    )
    ex_score_centered <- apply(es_centered, 1L, max, na.rm = TRUE)
    out$semantica_exclusion_score_centered[ii] <- ex_score_centered
    out$semantica_contrast_margin_centered[ii] <-
      assigned_score_centered[ii] - ex_score_centered
    out$semantica_exclusion_conflict_centered[ii] <-
      is.finite(ex_score_centered) & is.finite(assigned_score_centered[ii]) &
      ex_score_centered > assigned_score_centered[ii]
  }

  # Raw alignment remains primary. Automatic exclusion is intentionally more
  # conservative than diagnosis: a raw clear mismatch/exclusion conflict must
  # survive the mean-centered sensitivity view before it can remove an item.
  # This is an uncertainty gate, not an alternative scoring representation.
  guard_decision <- .semantica_robust_content_guard(
    out$semantica_factor_clear_mismatch,
    out$semantica_factor_clear_mismatch_centered,
    out$semantica_exclusion_conflict,
    out$semantica_exclusion_conflict_centered
  )
  out$semantica_content_guard_pass_raw <- guard_decision$raw_pass
  out$semantica_content_guard_pass <- guard_decision$robust_pass
  out$semantica_content_guard_sensitivity <- guard_decision$sensitivity

  if (!is.null(facet_col)) {
    assigned_facet <- as.character(x[[facet_col]][match(common, ids)])
    for (f in f_names) {
      facets <- factors[[f]]$facets %||% NULL
      if (is.null(facets) || !length(facets) || is.null(names(facets))) next
      fn <- names(facets)
      fd <- vapply(
        facets,
        function(z) trimws(as.character(z$description %||% "")),
        character(1L)
      )
      keep <- nzchar(fd)
      if (!any(keep)) next
      fn <- fn[keep]
      fd <- fd[keep]
      fr <- data.frame(
        ref_id = paste0("facet::", f, "::", fn),
        ref_text = paste(f, f_desc[[f]], fn, fd, sep = ". "),
        stringsAsFactors = FALSE
      )
      fe <- semantica_embed(
        fr, embed_session,
        text_col = "ref_text", id_col = "ref_id",
        batch_size = batch_size, normalize = TRUE, cache = cache,
        cache_dir = cache_dir,
        cache_namespace = paste0(cache_namespace %||% "default", "::definitions"),
        verbose = FALSE
      )$embeddings
      ii <- which(assigned == f)
      if (!length(ii)) next
      fs <- .semantica_cosine_cross(embeddings[common[ii], , drop = FALSE], fe)
      colnames(fs) <- fn
      topf <- fn[max.col(fs, ties.method = "first")]
      out$semantica_top_facet[ii] <- topf
      for (k in seq_along(ii)) {
        af <- assigned_facet[ii[k]]
        if (!is.na(af) && af %in% fn) {
          sc <- fs[k, af]
          other <- fs[k, setdiff(fn, af), drop = TRUE]
          out$semantica_facet_score[ii[k]] <- sc
          out$semantica_facet_margin[ii[k]] <- if (length(other)) {
            sc - max(other, na.rm = TRUE)
          } else {
            NA_real_
          }
          out$semantica_facet_aligned[ii[k]] <- identical(af, topf[k])
        }
      }
      jj <- ii[is.finite(out$semantica_facet_margin[ii])]
      if (length(jj)) {
        facet_class <- .semantica_classify_alignment_margins(
          out$semantica_facet_margin[jj],
          rep(f, length(jj)),
          min_reference = 2L
        )
        out$semantica_facet_margin_scale[jj] <- facet_class$scale
        out$semantica_facet_clear_mismatch[jj] <- facet_class$clear_mismatch
        out$semantica_facet_alignment_status[jj] <- facet_class$status
      }
    }
  }

  list(
    available = TRUE,
    table = out,
    factor_similarity = sim,
    evidence_role = "semantic_definition_alignment",
    reference_policy = "single_declared_factor_name_plus_description",
    reference_texts = stats::setNames(refs$ref_text, f_names),
    reference_dependency_note = paste(
      "Alignment is conditional on the analyst-declared factor wording and the",
      "active embedding representation. SEMANTICA does not generate hidden",
      "paraphrase prototypes or aggregate alternate definitions because doing so",
      "would introduce an unvalidated reference-construction policy."
    ),
    guard_rule = paste(
      "Pool-relative conservative guard: retain ambiguous top-rank cases;",
      "a raw clear factor mismatch or explicit exclusion conflict is automatically",
      "excluded only when the same exclusionary conclusion survives the mean-centered",
      "sensitivity view and enough alternatives remain. Centered geometry never enters",
      "the ACO objective. Facet alignment remains diagnostic/coverage evidence."
    ),
    note = paste(
      "Alignment is embedding-based content-screening evidence, not content",
      "validity or participant-based validation."
    )
  )
}

# Representation evidence-state helper -----------------------------------
#
# This helper deliberately avoids universal sentence-embedding cutoffs. It
# records two data/model-relative pieces of evidence that can be interpreted
# without pretending that a single cosine geometry is universally "good":
# (1) whether the active representation preserves its strongest pair ordering
#     better than the finite-pool random-overlap reference under the already
#     computed none-vs-mean-centering sensitivity analysis; and
# (2) whether the Gram spectrum is more concentrated than the upper edge
#     expected from an isotropic random-vector reference with the same item
#     count and embedding dimension. The latter is a descriptive reference,
#     not a validity threshold.
.semantica_representation_evidence_state <- function(
    representation_stability = NULL,
    cosine_diagnostics = NULL,
    embedding_diagnostics = NULL) {
  rs <- representation_stability %||% list()
  cd <- cosine_diagnostics %||% list()
  ed <- embedding_diagnostics %||% list()
  sens <- rs$cosine_adjustment_sensitivity %||% list()

  n_items <- suppressWarnings(as.integer(ed$n_items %||% cd$n_items %||% NA_integer_))
  embed_dim <- suppressWarnings(as.integer(ed$embed_dim %||% NA_integer_))
  top_share <- suppressWarnings(as.numeric(rs$top_eigen_share %||% cd$top_eigen_share %||% NA_real_))

  # Marchenko-Pastur upper-edge share for an n x d matrix of isotropic,
  # unit-variance random vectors. This is used only as an interpretable null
  # reference for concentration; real language embeddings need not be isotropic.
  isotropic_top_share_reference <- if (
    is.finite(n_items) && n_items > 1L && is.finite(embed_dim) && embed_dim > 1L
  ) {
    min(1, ((1 + sqrt(n_items / embed_dim))^2) / n_items)
  } else NA_real_
  concentration_ratio <- if (
    is.finite(top_share) && is.finite(isotropic_top_share_reference) &&
      isotropic_top_share_reference > .Machine$double.eps
  ) top_share / isotropic_top_share_reference else NA_real_
  concentrated_relative_to_isotropic <- if (
    is.finite(top_share) && is.finite(isotropic_top_share_reference)
  ) top_share > isotropic_top_share_reference else NA

  top_pair_vs_random <- sens$top_pair_overlap_vs_random %||% NA_character_
  representation_sensitive <- identical(top_pair_vs_random, "at_or_below_random_reference")

  status <- if (isTRUE(representation_sensitive) && isTRUE(concentrated_relative_to_isotropic)) {
    "representation_sensitive_and_concentrated"
  } else if (isTRUE(representation_sensitive)) {
    "representation_sensitive"
  } else if (isTRUE(concentrated_relative_to_isotropic)) {
    "representation_concentrated"
  } else if (isFALSE(concentrated_relative_to_isotropic) && !isTRUE(representation_sensitive)) {
    "no_automatic_concern_detected"
  } else {
    "insufficient_representation_diagnostics"
  }

  qualifiers <- character(0L)
  if (isTRUE(representation_sensitive)) {
    qualifiers <- c(qualifiers, "top-pair ordering is not more stable than the finite-pool random-overlap reference across cosine preprocessing")
  }
  if (isTRUE(concentrated_relative_to_isotropic)) {
    qualifiers <- c(qualifiers, "Gram-spectrum concentration exceeds an isotropic random-vector reference; this is descriptive and not a universal invalidity cutoff")
  }
  if (!length(qualifiers)) {
    qualifiers <- "no automatic representation qualifier was triggered by the available relative/reference diagnostics"
  }

  sensitivity_axes <- list(
    available = isTRUE(sens$available),
    offdiag_correlation = suppressWarnings(as.numeric(sens$offdiag_correlation %||% NA_real_)),
    q95_abs_delta = suppressWarnings(as.numeric(sens$q95_abs_delta %||% NA_real_)),
    top_pair_jaccard = suppressWarnings(as.numeric(sens$top_pair_jaccard %||% NA_real_)),
    top_pair_random_reference = suppressWarnings(as.numeric(sens$top_pair_jaccard_random_baseline %||% NA_real_)),
    top_pair_excess_over_random = {
      jj <- suppressWarnings(as.numeric(sens$top_pair_jaccard %||% NA_real_))
      rr <- suppressWarnings(as.numeric(sens$top_pair_jaccard_random_baseline %||% NA_real_))
      if (is.finite(jj) && is.finite(rr)) jj - rr else NA_real_
    },
    interpretation = paste(
      "Continuous preprocessing-sensitivity evidence; no universal cutoff is applied.",
      "These diagnostics qualify exclusion confidence and downstream interpretation but do not replace the raw representation."
    )
  )

  structure(list(
    status = status,
    state_schema = "representation-evidence-v2",
    concentration_axis = list(
      concentrated_relative_to_isotropic = concentrated_relative_to_isotropic,
      top_eigen_share = top_share,
      isotropic_top_eigen_share_reference = isotropic_top_share_reference,
      spectral_concentration_ratio = concentration_ratio
    ),
    preprocessing_sensitivity_axis = sensitivity_axes,
    evidence_family = "embedding_semantic",
    participant_based = FALSE,
    calibration_status = "descriptive_reference_not_validity_cutoff",
    representation_sensitive = isTRUE(representation_sensitive),
    concentrated_relative_to_isotropic = concentrated_relative_to_isotropic,
    top_eigen_share = top_share,
    isotropic_top_eigen_share_reference = isotropic_top_share_reference,
    spectral_concentration_ratio = concentration_ratio,
    effective_rank = rs$effective_rank %||% cd$effective_rank %||% NA_real_,
    effective_rank_ratio = rs$effective_rank_ratio %||% cd$effective_rank_ratio %||% NA_real_,
    common_direction_strength = rs$common_direction_strength %||% cd$common_direction_strength %||% NA_real_,
    cosine_adjustment_sensitivity = sens,
    qualifiers = qualifiers,
    note = paste(
      "Representation status qualifies all downstream embedding-derived semantic, PFA, and ESEM evidence.",
      "It does not automatically alter embeddings or choose a more favorable cosine preprocessing."
    )
  ), class = c("semantica_representation_evidence_state", "list"))
}
