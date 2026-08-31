# SEMANTICA visualization helpers.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
}

# Avoid R CMD check notes for non-standard evaluation in ggplot2
utils::globalVariables(c(
  "Item", "Archive_Entry", "Selected", "Factor", "Is_Final", "freq",
  "eval", "value", "Metric", "best_so_far", "from", "to", "weight",
  "name", "is_final", "factor", "label", "loading", "dominant",
  "fill_col", "abs_load", "x", "y", "r", "xend", "yend", "lx", "ly",
  "item", "sim", "type", "mean_sim", "x0", "y0", "x1", "y1", "xmid", "ymid_j",
  "show_label", "load_label", "lwd", "alpha_val", "edge_col", "node_id",
  "n_sel", "zone", "label_col", "dom_loading", "cross_loading", "ratio", "fill_hex",
  "pfa_factor", "intended_factor", "mapped_factor", "is_target",
  "n_obs", "metric", "metric_value", "anchor_kind",
  "xmin", "xmax", "ymin", "ymax", "middle.linewidth", ".data",
  "total", "ci_lo", "ci_hi", ".", "aggregate", "combn", "runif",
  "score", "score_col", "component", "status", "display", "kind",
  "group", "score_label", "before_after", "sim_type", "metric_group",
  "phase", "phase_x", "score_norm", "score_plot", "score_txt", "item_status",
  "centroid_type", "pool_x", "pool_y", "selected_x", "selected_y",
  "ring_x", "ring_y", "edge_width", "edge_alpha", "semantic_overlap"
))

# =================================================================
# SHARED THEME & PALETTES
# =================================================================
.sem_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2, margin = ggplot2::margin(b = 6)),
      plot.subtitle    = ggplot2::element_text(size = base_size - 0.5, colour = "#555555", margin = ggplot2::margin(b = 10)),
      plot.caption     = ggplot2::element_text(size = base_size - 2, colour = "#888888", hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#EEEEEE"),
      axis.title       = ggplot2::element_text(size = base_size - 0.5),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size - 0.5),
      legend.key.size  = grid::unit(0.85, "lines"),
      plot.margin      = ggplot2::margin(10, 12, 8, 10)
    )
}

.factor_colours  <- function(n) {
  palettes  <- list(
    c("#2166AC", "#D6604D", "#4DAC26", "#8E24AA", "#E08214", "#01665E"),
    c("#1B7837", "#C2A5CF", "#D73027", "#4575B4", "#FDAE61", "#762A83")
  )
  grDevices::colorRampPalette(palettes[[1]])(n)
}

.viz_sanitize_name <- function(x) {
  sanitize_fun <- .viz_get_function("sanitize_lavaan_name")
  if (!is.null(sanitize_fun)) {
    sanitize_fun(x)
  } else {
    gsub("[^A-Za-z0-9_]", "_", trimws(x))
  }
}

.viz_get_function <- function(name) {
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }
  ns <- tryCatch(asNamespace("SEMANTICA"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, mode = "function", inherits = FALSE)) {
    return(get(name, envir = ns, mode = "function", inherits = FALSE))
  }
  NULL
}

.viz_core_result <- function(result) {
  if (!is.null(result$optimization) && !is.null(result$optimization$factor_assignment)) {
    result$optimization
  } else {
    result
  }
}

.viz_transform_cosine_for_display <- function(cos_matrix, factor_assignment = NULL, factors = NULL) {
  transform_fun <- .viz_get_function("transform_cosine_for_esem")
  if (!is.null(transform_fun)) {
    out <- tryCatch(transform_fun(cos_matrix, factor_assignment, factors), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  if (!is.matrix(cos_matrix)) cos_matrix <- as.matrix(cos_matrix)
  out <- (cos_matrix + t(cos_matrix)) / 2
  out[!is.finite(out)] <- 0
  out[out > 1] <- 1
  out[out < -1] <- -1
  diag(out) <- 1
  out
}

.viz_factor_col <- function(lambda_mat, factor_name) {
  idx <- which(colnames(lambda_mat) == factor_name)
  if (length(idx) == 0L) {
    idx <- which(.viz_sanitize_name(colnames(lambda_mat)) == .viz_sanitize_name(factor_name))
  }
  if (length(idx) == 0L) NA_integer_ else idx[1L]
}

.viz_lambda_matrix <- function(esem_fit, factor_assignment = NULL,
                               factors = NULL) {
  if (!is.null(factor_assignment) && !is.null(factors)) {
    aligned <- tryCatch(
      extract_aligned_esem_solution(
        esem_fit,
        factor_assignment = factor_assignment,
        factors = factors,
        standardized = TRUE
      ),
      error = function(e) NULL
    )
    # With an intended structure in hand, never silently fall back to raw
    # rotation-axis labels: that can visually assign loadings to the wrong
    # construct.
    return(if (is.null(aligned)) NULL else aligned$lambda)
  }
  # Use standardized loadings for diagnostic plots; fall back only when lavaan
  # cannot provide them for a particular fit.
  out <- tryCatch(lavaan::lavInspect(esem_fit, "std")$lambda, error = function(e) NULL)
  if (is.null(out)) out <- tryCatch(lavaan::lavInspect(esem_fit, "est")$lambda, error = function(e) NULL)
  out
}

.viz_safe_num <- function(x, default = NA_real_) {
  val <- suppressWarnings(tryCatch(as.numeric(x), error = function(e) default))
  if (length(val) == 0L || !is.finite(val[1L])) default else val[1L]
}

.viz_safe01 <- function(x, default = NA_real_) {
  val <- .viz_safe_num(x, default)
  if (!is.finite(val)) return(default)
  max(0, min(1, val))
}

.viz_display_num <- function(x, digits = 3, missing = "NA") {
  val <- .viz_safe_num(x)
  if (!is.finite(val)) missing else sprintf(paste0("%.", digits, "f"), val)
}

.viz_limit_pool_items <- function(items, selected_items, cosine_sim_matrix, max_items) {
  items <- unique(intersect(as.character(items), colnames(cosine_sim_matrix)))
  max_items <- suppressWarnings(as.numeric(max_items[1L]))
  if (length(max_items) != 1L || is.na(max_items) || max_items < 1L) {
    stop("'max_items' must be a positive number or Inf.")
  }
  if (is.infinite(max_items) || length(items) <= floor(max_items)) {
    return(list(items = items, reduced = FALSE, total = length(items)))
  }

  selected_items <- intersect(as.character(selected_items), items)
  cap <- max(as.integer(floor(max_items)), length(selected_items))
  other_items <- setdiff(items, selected_items)
  n_other <- max(0L, cap - length(selected_items))
  if (n_other > 0L && length(selected_items) > 0L && length(other_items) > 0L) {
    proximities <- apply(
      cosine_sim_matrix[other_items, selected_items, drop = FALSE],
      1L,
      function(x) max(x, na.rm = TRUE)
    )
    proximities[!is.finite(proximities)] <- -Inf
    other_items <- other_items[order(-proximities, match(other_items, items))]
  }
  keep <- unique(c(selected_items, utils::head(other_items, n_other)))
  list(items = items[items %in% keep], reduced = TRUE, total = length(items))
}

.viz_factor_label <- function(x, max_chars = 22L) {
  x <- as.character(x)
  out <- gsub("_", "\n", x, fixed = TRUE)
  too_long <- nchar(gsub("\n", "", out, fixed = TRUE)) > max_chars
  if (any(too_long)) {
    out[too_long] <- vapply(
      x[too_long],
      function(label) paste(strwrap(label, width = max(8L, floor(max_chars / 2))), collapse = "\n"),
      character(1L)
    )
  }
  out
}

.viz_factor_correlation_matrix <- function(esem_fit, factors = NULL,
                                           factor_assignment = NULL) {
  if (is.null(esem_fit)) return(NULL)
  psi <- NULL
  if (!is.null(factor_assignment) && !is.null(factors)) {
    aligned <- tryCatch(
      extract_aligned_esem_solution(
        esem_fit,
        factor_assignment = factor_assignment,
        factors = factors,
        standardized = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(aligned) || is.null(aligned$psi)) return(NULL)
    psi <- aligned$psi
  }
  if (is.null(psi)) {
    psi <- tryCatch(lavaan::lavInspect(esem_fit, "est")$psi, error = function(e) NULL)
  }
  if (is.null(psi) || !is.matrix(psi) || nrow(psi) < 2L) {
    psi <- tryCatch(lavaan::lavInspect(esem_fit, "std")$psi, error = function(e) NULL)
  }
  if (is.null(psi) || !is.matrix(psi) || nrow(psi) < 2L) return(NULL)
  d <- sqrt(pmax(diag(psi), .Machine$double.eps))
  cor_lv <- psi / tcrossprod(d)
  diag(cor_lv) <- 1
  cor_lv[!is.finite(cor_lv)] <- NA_real_

  if (!is.null(factors) && length(factors) > 0L) {
    factors <- as.character(factors)
    psi_names <- rownames(cor_lv) %||% colnames(cor_lv)
    if (!is.null(psi_names)) {
      idx <- vapply(factors, function(f) {
        hit <- which(psi_names == f)
        if (length(hit) == 0L) hit <- which(.viz_sanitize_name(psi_names) == .viz_sanitize_name(f))
        if (length(hit) == 0L) NA_integer_ else hit[1L]
      }, integer(1L))
      keep <- !is.na(idx)
      if (any(keep)) {
        out <- matrix(NA_real_, nrow = length(factors), ncol = length(factors),
                      dimnames = list(factors, factors))
        out[keep, keep] <- cor_lv[idx[keep], idx[keep], drop = FALSE]
        diag(out) <- 1
        return(out)
      }
    }
  }
  cor_lv
}

# =================================================================
# PLOT 1 -- PHEROMONE EVOLUTION HEATMAP
# =================================================================
#' Pheromone trail evolution across elite archive solutions
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object (combined heatmap + bar chart).
#' @export
#' @examples
#' \dontrun{
#' plot_pheromone_heatmap(result)
#' }
plot_pheromone_heatmap <- function(result) {
  archive    <- result$elite_archive
  if (length(archive) == 0L) {
    message("[plot_pheromone_heatmap] No elite archive available.")
    return(invisible(NULL))
  }
  fa_final   <- result$factor_assignment
  best_items <- result$best_items
  eligible   <- result$eligible_items

  all_eligible <- if (!is.null(eligible)) {
    unique(unlist(eligible, use.names = FALSE))
  } else {
    names(fa_final)
  }

  fa_all <- character(length(all_eligible))
  names(fa_all) <- all_eligible
  if (!is.null(eligible)) {
    for (f in names(eligible)) fa_all[intersect(eligible[[f]], all_eligible)] <- f
  }
  fa_all[best_items] <- fa_final[best_items]
  fa_all[fa_all == ""] <- "Unassigned"

  factors      <- unique(fa_final)
  n_all        <- length(all_eligible)
  n_archive    <- length(archive)

  mat <- matrix(0L, nrow = n_all, ncol = n_archive,
                dimnames = list(all_eligible, paste0("E", seq_len(n_archive))))
  for (k in seq_len(n_archive)) {
    sel  <- names(archive[[k]]$vec)[archive[[k]]$vec == 1L]
    hit  <- intersect(sel, all_eligible)
    mat[hit, k] <- 1L
  }

  cum_freq <- rowMeans(mat)
  factor_order <- factors
  item_order   <- unlist(lapply(factor_order, function(f) {
    fi <- names(fa_all[fa_all == f])
    fi[order(cum_freq[fi], decreasing = TRUE)]
  }))
  ua <- names(fa_all[fa_all == "Unassigned"])
  item_order <- c(item_order, ua)

  mat_ord  <- mat[item_order, , drop = FALSE]
  mat_long <- do.call(rbind, lapply(colnames(mat_ord), function(cn) {
    data.frame(Item = rownames(mat_ord), Archive_Entry = cn, Selected = mat_ord[, cn], stringsAsFactors = FALSE)
  }))
  mat_long$Item         <- factor(mat_long$Item, levels = rev(item_order))
  mat_long$Factor       <- fa_all[as.character(mat_long$Item)]
  mat_long$Is_Final     <- as.character(mat_long$Item) %in% best_items

  freq_df <- data.frame(
    Item = item_order, freq = cum_freq[item_order], is_final = item_order %in% best_items, stringsAsFactors = FALSE
  )
  fac_cols <- setNames(.factor_colours(length(factors)), factors)
  if ("Unassigned" %in% fa_all) fac_cols["Unassigned"] <- "#CCCCCC"

  p_main  <- ggplot2::ggplot(mat_long, ggplot2::aes(x = Archive_Entry, y = Item, fill = factor(Selected))) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.25) +
    ggplot2::geom_tile(data = mat_long[mat_long$Is_Final, ], ggplot2::aes(x = Archive_Entry, y = Item), fill = NA, colour = "#E83030", linewidth = 0.7, inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = c("0" = "#F0F0F0", "1" = "#1A6BAF"), labels = c("Not selected", "Selected"), name = NULL) +
    ggplot2::facet_grid(rows = ggplot2::vars(Factor), scales = "free_y", space = "free_y") +
    .sem_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6.5, angle = 45, hjust = 1), axis.text.y = ggplot2::element_blank(), strip.text.y = ggplot2::element_text(angle = 0, size = 8, face = "bold"), legend.position = "bottom", panel.spacing = ggplot2::unit(0.15, "lines")) +
    ggplot2::geom_text(data = mat_long[mat_long$Archive_Entry == mat_long$Archive_Entry[1L], ], ggplot2::aes(x = 0.3, y = Item, label = as.character(Item), colour = Is_Final), hjust = 1, size = 2.2, inherit.aes = FALSE) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#C0392B", "FALSE" = "#333333"), guide = "none") +
    ggplot2::labs(title = "Pheromone Trail Evolution -- ALL Eligible Items", subtitle = paste0(sprintf("All %d candidate items shown; red borders = final solution (%d items)\n", n_all, length(best_items)), "Each column = one elite archive entry; blue = selected"), x = "Elite Archive Entry", y = NULL, caption = "SEMANTICA | ACO search")

  freq_plot_df        <- freq_df
  freq_plot_df$Item   <- factor(freq_plot_df$Item, levels = rev(item_order))
  freq_plot_df$Factor <- fa_all[as.character(freq_plot_df$Item)]
  p_bar <- ggplot2::ggplot(freq_plot_df, ggplot2::aes(x = freq, y = Item, fill = Factor)) +
    ggplot2::geom_col(width = 0.7, alpha = 0.85) +
    ggplot2::geom_point(data = freq_plot_df[freq_plot_df$is_final, ], ggplot2::aes(x = freq, y = Item), colour = "#E83030", shape = 18, size = 2.5, inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = fac_cols, guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05), breaks = c(0, 0.5, 1)) +
    ggplot2::facet_grid(rows = ggplot2::vars(Factor), scales = "free_y", space = "free_y") +
    .sem_theme(base_size = 9) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), panel.spacing = ggplot2::unit(0.15, "lines"), panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::labs(x = "Selection\nfreq.", y = NULL)

  patchwork::wrap_plots(p_main, p_bar, ncol = 2, widths = c(5, 1))
}

# =================================================================
# PLOT 2 -- FITNESS EVOLUTION OVER SEARCH
# =================================================================
#' Fitness evolution during ACO search
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_fitness_evolution(result)
#' }
plot_fitness_evolution <- function(result) {
  hist <- result$solution_history
  if (is.null(hist) || length(hist) == 0L) {
    message("[plot_fitness_evolution] No solution history available.")
    return(invisible(NULL))
  }
  df_hist <- data.frame(
    eval      = seq_along(hist),
    sem_score = vapply(hist, function(x) if (is.null(x$sem_score) || is.na(x$sem_score)) NA_real_ else x$sem_score, numeric(1L)),
    esem_score= vapply(hist, function(x) if (is.null(x$esem_score) || is.na(x$esem_score)) NA_real_ else x$esem_score, numeric(1L)),
    total     = vapply(hist, function(x) if (is.null(x$total) || is.na(x$total)) NA_real_ else x$total, numeric(1L)),
    stringsAsFactors = FALSE
  )
  df_hist$best_so_far <- cummax(ifelse(is.na(df_hist$total), -Inf, df_hist$total))
  df_hist$best_so_far[is.infinite(df_hist$best_so_far)] <- NA_real_
  best_idx <- which.max(df_hist$total)

  df_long <- rbind(
    data.frame(eval = df_hist$eval, value = df_hist$sem_score, Metric = "Semantic Score", stringsAsFactors = FALSE),
    data.frame(eval = df_hist$eval, value = df_hist$total, Metric = "Total Score", stringsAsFactors = FALSE)
  )
  esem_rows <- df_hist[!is.na(df_hist$esem_score), ]
  if (nrow(esem_rows) > 0L) {
    df_long <- rbind(df_long, data.frame(eval = esem_rows$eval, value = esem_rows$esem_score, Metric = "ESEM Score", stringsAsFactors = FALSE))
  }
  df_long <- df_long[!is.na(df_long$value), ]
  metric_cols <- c("Semantic Score" = "#4DAC26", "ESEM Score" = "#D6604D", "Total Score" = "#2166AC")

  ggplot2::ggplot(df_long, ggplot2::aes(x = eval, y = value, colour = Metric)) +
    ggplot2::geom_line(alpha = 0.25, linewidth = 0.4) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, span = 0.20, linewidth = 1.1) +
    ggplot2::geom_line(data = df_hist[!is.na(df_hist$best_so_far), ], ggplot2::aes(x = eval, y = best_so_far), colour = "#2166AC", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
    ggplot2::geom_point(data = df_hist[best_idx, ], ggplot2::aes(x = eval, y = total), colour = "#E83030", size = 3.5, shape = 18, inherit.aes = FALSE) +
    ggplot2::annotate("text", x = df_hist$eval[best_idx], y = df_hist$total[best_idx] + 0.02, label = sprintf("Best\n%.4f", df_hist$total[best_idx]), size = 2.8, colour = "#E83030", hjust = 0.5) +
    ggplot2::scale_colour_manual(values = metric_cols, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    .sem_theme() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = "Fitness Evolution During ACO Search", subtitle = "Smoothed trends; dashed = running best; red diamond = best observed objective", x = "Solution Evaluation Number", y = "Score", caption = "SEMANTICA | ACO search history")
}

# =================================================================
# PLOT 3 -- SEMANTIC SIMILARITY NETWORKS
# =================================================================
#' Semantic similarity networks: BEFORE vs AFTER
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix. Optional for a high-level result because the stored matrix is reused.
#' @param edge_threshold_before Threshold for BEFORE network.
#' @param edge_threshold_after Threshold for AFTER network.
#' @param df Optional item metadata dataframe. When supplied, the BEFORE
#'   network represents the generated item pool; otherwise it represents the
#'   screened eligible pool retained in `result`.
#' @param max_before_items Maximum BEFORE-pool items rendered in the network.
#'   Selected items are always retained; additional items nearest the selected
#'   items are shown when the display is capped. Use `Inf` for the full pool.
#' @return A combined `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_semantic_networks(
#'   result, cosine_sim_matrix,
#'   edge_threshold_before = 0.45,
#'   edge_threshold_after = 0.30,
#'   df = item_metadata,
#'   max_before_items = 200L
#' )
#' }
plot_semantic_networks <- function(result, cosine_sim_matrix, edge_threshold_before = 0.45,
                                   edge_threshold_after = 0.30, df = NULL,
                                   max_before_items = 200L) {
  fa_final  <- result$factor_assignment
  best_items <- result$best_items
  all_items  <- colnames(cosine_sim_matrix)
  eligible <- result$eligible_items
  if (is.null(eligible)) eligible <- lapply(unique(fa_final), function(f) names(fa_final[fa_final == f]))

  fa_all <- character(length(all_items))
  names(fa_all) <- all_items
  for (f in names(eligible)) fa_all[intersect(eligible[[f]], all_items)] <- f
  before_items <- intersect(unlist(eligible), all_items)
  before_scope <- "Screened Eligible Pool"
  if (!is.null(df) && is.data.frame(df)) {
    id_col <- intersect(c("item", "item_id", "ID"), names(df))
    factor_col <- intersect(c("factor", "type", "Dimension"), names(df))
    if (length(id_col) > 0L && length(factor_col) > 0L) {
      ids <- as.character(df[[id_col[1L]]])
      fac <- as.character(df[[factor_col[1L]]])
      keep <- nzchar(ids) & nzchar(fac) & ids %in% all_items
      if (any(keep)) {
        fa_all[ids[keep]] <- fac[keep]
        before_items <- intersect(ids[keep], all_items)
        before_scope <- "Generated Item Pool"
      }
    }
  }
  fa_all[fa_all == ""] <- "Unassigned"
  fa_all[best_items]   <- fa_final
  before_display <- .viz_limit_pool_items(
    before_items, best_items, cosine_sim_matrix, max_before_items
  )
  before_items <- before_display$items
  if (isTRUE(before_display$reduced)) {
    before_scope <- sprintf("%s (display subset %d/%d)", before_scope, length(before_items), before_display$total)
  }

  factors    <- unique(fa_final)
  fac_cols   <- setNames(.factor_colours(length(factors)), factors)
  fac_cols["Unassigned"] <- "#CCCCCC"

  make_net_plot <- function(items, fa_sub, cos_sub, threshold, title_txt, subtitle_txt) {
    nodes <- data.frame(name = items, factor = fa_sub[items], is_final = items %in% best_items, stringsAsFactors = FALSE)
    pairs    <- which(upper.tri(cos_sub), arr.ind = TRUE)
    edges_df <- data.frame(from = items[pairs[,1]], to = items[pairs[,2]], weight = cos_sub[pairs])
    edges_df <- edges_df[edges_df$weight >= threshold, ]
    if (nrow(edges_df) == 0L) edges_df <- data.frame(from = character(0), to = character(0), weight = numeric(0))
    g  <- tidygraph::tbl_graph(nodes = nodes, edges = edges_df, directed = FALSE)
    ggraph::ggraph(g, layout = "fr") +
      ggraph::geom_edge_link(ggplot2::aes(alpha = weight, width = weight), colour = "#AAAAAA", show.legend = FALSE) +
      ggraph::scale_edge_width(range = c(0.2, 1.8)) + ggraph::scale_edge_alpha(range = c(0.15, 0.65)) +
      ggraph::geom_node_point(ggplot2::aes(colour = factor, size = is_final), show.legend = c(colour = TRUE, size = FALSE)) +
      ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 2.1, max.overlaps = 20, colour = "#333333") +
      ggplot2::scale_colour_manual(values = fac_cols, name = "Factor", na.value = "#BBBBBB") +
      ggplot2::scale_size_manual(values = c("FALSE" = 2.2, "TRUE" = 4.5)) +
      .sem_theme() + ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "bottom") +
      ggplot2::labs(title = title_txt, subtitle = subtitle_txt)
  }

  cos_before   <- cosine_sim_matrix[before_items, before_items, drop = FALSE]
  p_before <- make_net_plot(before_items, fa_all, cos_before, threshold = edge_threshold_before,
                            title_txt = paste("BEFORE --", before_scope),
                            subtitle_txt = sprintf("%d items; edges >= %.2f", length(before_items), edge_threshold_before))
  cos_after <- cosine_sim_matrix[best_items, best_items, drop = FALSE]
  p_after   <- make_net_plot(best_items, fa_final, cos_after, threshold = edge_threshold_after, title_txt = "AFTER -- Selected Scale Items", subtitle_txt = sprintf("%d items; edges >= %.2f", length(best_items), edge_threshold_after))

  patchwork::wrap_plots(p_before, p_after, ncol = 2) +
    patchwork::plot_annotation(
      title = "Semantic Similarity Networks",
      caption = "SEMANTICA | cosine similarities",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
}

# =================================================================
# PLOT 4 -- ESEM FACTOR LOADING MATRIX
# =================================================================
#' ESEM rotated factor loading matrix heatmap
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_esem_loadings(result)
#' }
plot_esem_loadings <- function(result) {
  esem_fit <- result$esem_fit
  if (is.null(esem_fit)) { message("[plot_esem_loadings] No ESEM fit object."); return(invisible(NULL)) }
  fa <- result$factor_assignment
  factors <- unique(fa)
  lambda_mat <- .viz_lambda_matrix(esem_fit, fa, factors)
  if (is.null(lambda_mat)) { message("[plot_esem_loadings] Could not extract lambda matrix."); return(invisible(NULL)) }

  best_items <- result$best_items
  fac_cols   <- setNames(.factor_colours(length(factors)), factors)
  factor_cols <- vapply(factors, function(f) .viz_factor_col(lambda_mat, f), integer(1L))
  valid_factors <- factors[!is.na(factor_cols)]
  factor_cols <- factor_cols[!is.na(factor_cols)]
  if (length(valid_factors) == 0L) { message("[plot_esem_loadings] Could not match factor columns."); return(invisible(NULL)) }

  df_loads <- do.call(rbind, lapply(seq_along(valid_factors), function(j) {
    f <- valid_factors[j]
    fc <- factor_cols[j]
    data.frame(
      item = rownames(lambda_mat),
      factor = f,
      loading = as.numeric(lambda_mat[, fc]),
      dominant = rownames(lambda_mat) %in% names(fa[fa == f]),
      stringsAsFactors = FALSE
    )
  }))
  df_loads <- df_loads[!is.na(df_loads$loading), ]
  df_loads$factor <- factor(df_loads$factor, levels = factors)

  item_ord  <- unlist(lapply(factors, function(f) {
    fi <- intersect(names(fa[fa == f]), rownames(lambda_mat))
    fc <- .viz_factor_col(lambda_mat, f)
    dom_load  <- sapply(fi, function(it) { if (is.na(fc)) 0 else abs(lambda_mat[it, fc]) })
    fi[order(dom_load, decreasing = TRUE)]
  }))
  df_loads$item <- factor(df_loads$item, levels = rev(item_ord))
  df_loads$fill_col <- ifelse(df_loads$dominant, fac_cols[as.character(df_loads$factor)], "#AAAAAA")
  df_loads$abs_load <- abs(df_loads$loading)

  ggplot2::ggplot(df_loads, ggplot2::aes(x = factor, y = item)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill_col, alpha = abs_load), colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", loading), colour = ifelse(abs_load > 0.35, "white", "#444444")), size = 2.4, fontface = "bold") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() + ggplot2::scale_alpha_continuous(range = c(0.08, 0.95), guide = "none") +
    ggplot2::geom_tile(data = df_loads[df_loads$dominant, ], ggplot2::aes(x = factor, y = item), fill = NA, colour = "#1A1A1A", linewidth = 0.9, inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = seq(0.5, length(factors) + 0.5, 1), colour = "white", linewidth = 0.3) +
    .sem_theme(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 9, angle = 25, hjust = 1), axis.text.y = ggplot2::element_text(size = 8), panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "ESEM Rotated Factor Loading Matrix", subtitle = "Coloured tiles = dominant loadings; grey = cross-loadings; outline = dominant", x = "Factor", y = NULL, caption = paste0("Rotation: ", result$model_info$rotation, " | SEMANTICA"))
}

# =================================================================
# PLOT 5 -- WITHIN-FACTOR LOADING PROFILES
# =================================================================
#' Factor loading profiles with confidence intervals
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_loading_profiles(result)
#' }
plot_loading_profiles <- function(result) {
  esem_fit <- result$esem_fit
  if (is.null(esem_fit)) { message("[plot_loading_profiles] No ESEM fit object."); return(invisible(NULL)) }
  fa <- result$factor_assignment
  factors <- unique(fa)
  lambda_mat <- .viz_lambda_matrix(esem_fit, fa, factors)
  # Standardized loading SEs are not generally available from lavInspect("se");
  # avoid drawing unstandardized CIs around standardized diagnostics.
  se_mat     <- NULL
  if (is.null(lambda_mat)) { message("[plot_loading_profiles] Could not extract lambda."); return(invisible(NULL)) }

  fac_cols <- setNames(.factor_colours(length(factors)), factors)

  df_list <- lapply(factors, function(f) {
    fc <- .viz_factor_col(lambda_mat, f)
    if (is.na(fc)) return(NULL)
    items <- rownames(lambda_mat); dom <- names(fa[fa == f])
    data.frame(item = items, factor = f, loading = lambda_mat[items, fc],
               se = if (!is.null(se_mat)) se_mat[items, fc] else NA_real_,
               dominant = items %in% dom, stringsAsFactors = FALSE)
  })
  df_all <- do.call(rbind, df_list[!sapply(df_list, is.null)])
  if (is.null(df_all) || nrow(df_all) == 0L) { message("[plot_loading_profiles] Could not match factor columns."); return(invisible(NULL)) }
  df_all$factor <- factor(df_all$factor, levels = factors)
  df_all$ci_lo  <- ifelse(is.finite(df_all$se), df_all$loading - 1.96 * df_all$se, NA_real_)
  df_all$ci_hi  <- ifelse(is.finite(df_all$se), df_all$loading + 1.96 * df_all$se, NA_real_)

  ggplot2::ggplot(df_all, ggplot2::aes(x = loading, y = stats::reorder(item, loading))) +
    ggplot2::geom_vline(xintercept = 0, colour = "#CCCCCC", linewidth = 0.8) +
    ggplot2::annotate("rect", xmin = 0.50, xmax = 0.95, ymin = -Inf, ymax = Inf, fill = "#E8F4EA", alpha = 0.5) +
    ggplot2::geom_errorbar(data = df_all[is.finite(df_all$ci_lo) & is.finite(df_all$ci_hi), ],
                           ggplot2::aes(xmin = ci_lo, xmax = ci_hi, y = stats::reorder(item, loading)),
                           orientation = "y", width = 0.25, colour = "#AAAAAA", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = factor, shape = dominant, size = dominant)) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 21), guide = "none") +
    ggplot2::scale_size_manual(values = c("TRUE" = 2.8, "FALSE" = 1.8), guide = "none") +
    ggplot2::scale_colour_manual(values = fac_cols, name = "Factor", guide = "none") +
    ggplot2::facet_wrap(~factor, scales = "free_y", ncol = 3) +
    ggplot2::geom_text(data = df_all[df_all$dominant, ], ggplot2::aes(label = sprintf("%.2f", loading), x = loading + 0.04), size = 2.3, hjust = 0, colour = "#333333") +
    ggplot2::geom_vline(xintercept = c(0.50, 0.95), colour = "#4DAC26", linetype = "dashed", linewidth = 0.5) +
    .sem_theme() + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7), strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA), panel.grid.major.x = ggplot2::element_line(colour = "#EEEEEE")) +
    ggplot2::labs(title = "Factor Loading Profiles (ESEM)", subtitle = "Filled = dominant loading; green band = ideal range [0.50, 0.95]", x = "Standardized Factor Loading", y = NULL, caption = "SEMANTICA")
}

# =================================================================
# PLOT 6 -- DFI CUTOFFS GAUGES
# =================================================================
#' Model fit indices vs DFI cutoffs (gauge chart)
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_dfi_gauges(result)
#' }
plot_dfi_gauges <- function(result) {
  cr <- result$esem_result; ac <- result$active_cutoffs
  if (is.null(cr) || is.null(ac)) { message("[plot_dfi_gauges] Missing results."); return(invisible(NULL)) }

  metrics_df  <- data.frame(metric = c("CFI", "TLI", "RMSEA", "SRMR", "AVE"),
                            observed = c(cr$cfi, cr$tli, cr$rmsea, cr$srmr, cr$ave),
                            cutoff = c(ac$cfi, ac$tli, ac$rmsea, ac$srmr, 0.50),
                            direction = c("high", "high", "low", "low", "high"),
                            label = c("CFI (>=)", "TLI (>=)", "RMSEA (<=)", "SRMR (<=)",
                                      if (is.null(result$response_validation)) "AVE (desc.)" else "AVE (>=)"),
                            stringsAsFactors = FALSE)
  metrics_df  <- metrics_df[!is.na(metrics_df$observed) & is.finite(metrics_df$observed), ]
  metrics_df$pass <- ifelse(metrics_df$direction == "high", metrics_df$observed >= metrics_df$cutoff, metrics_df$observed <= metrics_df$cutoff)
  if (is.null(result$response_validation) && any(metrics_df$metric == "AVE")) {
    metrics_df$pass[metrics_df$metric == "AVE"] <- TRUE
  }

  htmt_max  <- result$htmt_max; htmt_thr <- result$model_info$htmt_threshold
  htmt_pass <- !is.null(htmt_max) && is.finite(htmt_max) && htmt_max <= htmt_thr
  pass_col <- "#4DAC26"; fail_col <- "#D6604D"; grey_col <- "#DDDDDD"
  bar_height <- 0.55

  p_main <- ggplot2::ggplot(metrics_df, ggplot2::aes(y = stats::reorder(label, seq_len(nrow(metrics_df))))) +
    ggplot2::geom_col(ggplot2::aes(x = 1), fill = grey_col, width = bar_height) +
    ggplot2::geom_col(ggplot2::aes(x = pmin(observed / ifelse(direction == "high", max(cutoff * 1.15, 1), cutoff * 1.5), 1), fill = pass), width = bar_height) +
    ggplot2::scale_fill_manual(values = c("TRUE" = pass_col, "FALSE" = fail_col), name = NULL, labels = c("TRUE" = "Reference met", "FALSE" = "Reference not met")) +
    ggplot2::geom_segment(ggplot2::aes(x = ifelse(direction == "high", cutoff / max(cutoff * 1.15, 1), cutoff / (cutoff * 1.5)), xend = ifelse(direction == "high", cutoff / max(cutoff * 1.15, 1), cutoff / (cutoff * 1.5)), y = as.numeric(stats::reorder(label, seq_len(nrow(metrics_df)))) - bar_height/2, yend = as.numeric(stats::reorder(label, seq_len(nrow(metrics_df)))) + bar_height/2), colour = "#1A1A1A", linewidth = 1.1) +
    ggplot2::geom_text(ggplot2::aes(x = pmin(observed / ifelse(direction == "high", max(cutoff * 1.15, 1), cutoff * 1.5), 1) + 0.02, label = sprintf("%.4f", observed)), hjust = 0, size = 3, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(x = ifelse(direction == "high", cutoff / max(cutoff * 1.15, 1), cutoff / (cutoff * 1.5)), label = sprintf("cut: %.3f", cutoff)), vjust = -1.2, size = 2.4, colour = "#444444") +
    ggplot2::scale_x_continuous(limits = c(0, 1.25), breaks = NULL) + .sem_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "bottom") +
    ggplot2::labs(title = "Semantic-Proxy Fit vs Reference Values", subtitle = sprintf("Reference source: %s; not a participant-data validity test", result$cutoff_source), y = NULL)

  if (!is.null(htmt_max) && is.finite(htmt_max)) {
    htmt_df <- data.frame(label = "HTMT max (<=)", observed = htmt_max, cutoff = htmt_thr, pass = htmt_pass)
    p_htmt <- ggplot2::ggplot(htmt_df, ggplot2::aes(y = label)) +
      ggplot2::geom_col(ggplot2::aes(x = 1), fill = grey_col, width = bar_height) +
      ggplot2::geom_col(ggplot2::aes(x = pmin(observed / (htmt_thr * 1.3), 1), fill = pass), width = bar_height) +
      ggplot2::scale_fill_manual(values = c("TRUE" = pass_col, "FALSE" = fail_col), guide = "none") +
      ggplot2::geom_segment(ggplot2::aes(x = htmt_thr / (htmt_thr * 1.3), xend = htmt_thr / (htmt_thr * 1.3), y = 0.7, yend = 1.3), colour = "#1A1A1A", linewidth = 1.1) +
      ggplot2::geom_text(ggplot2::aes(x = pmin(observed / (htmt_thr * 1.3), 1) + 0.02, label = sprintf("%.4f", observed)), hjust = 0, size = 3, fontface = "bold") +
      ggplot2::scale_x_continuous(limits = c(0, 1.25), breaks = NULL) + .sem_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(y = NULL, title = "HTMT-like semantic separation proxy")
    patchwork::wrap_plots(p_main, p_htmt, ncol = 1, heights = c(4, 1)) +
      patchwork::plot_annotation(caption = "SEMANTICA | Semantic-proxy reference comparisons; participant validation is separate")
  } else {
    p_main + ggplot2::labs(caption = "SEMANTICA | Semantic-proxy reference comparisons; participant validation is separate")
  }
}

# =================================================================
# PLOT 7 -- SCORE RADAR
# =================================================================
#' ESEM score decomposition radar chart
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_score_radar(result)
#' }
plot_score_radar <- function(result) {
  decomp <- result$esem_result$score_decomp
  if (is.null(decomp)) { message("[plot_score_radar] No score_decomp."); return(invisible(NULL)) }
  .safe_val <- function(x, default = 0) { v <- tryCatch(as.numeric(x), error = function(e) default); if (length(v) == 0L || is.na(v) || !is.finite(v)) default else v }

  metrics <- c("CFI ratio" = .safe_val(decomp$cfi_s), "RMSEA ratio" = .safe_val(decomp$rmsea_s), "SRMR ratio" = .safe_val(decomp$srmr_s), "AVE score" = .safe_val(decomp$ave_score), "Load quality" = .safe_val(decomp$loading_quality), "HTMT penalty" = .safe_val(decomp$htmt_penalty))
  metric_names <- names(metrics)
  metrics <- pmax(0, pmin(1, metrics))
  names(metrics) <- metric_names
  n <- length(metrics); angs <- seq(0, 2 * pi, length.out = n + 1)[1:n]
  if (n == 0L || length(metric_names) != n) {
    message("[plot_score_radar] No valid score components.")
    return(invisible(NULL))
  }

  pts <- data.frame(x = metrics * cos(angs - pi/2), y = metrics * sin(angs - pi/2), label = names(metrics), value = as.numeric(metrics))
  rings <- lapply(c(0.25, 0.50, 0.75, 1.00), function(r) { th <- seq(0, 2*pi, length.out=100); data.frame(x = r*cos(th-pi/2), y = r*sin(th-pi/2), r = r) })
  rings_df <- do.call(rbind, rings)
  spokes <- data.frame(x = cos(angs-pi/2), y = sin(angs-pi/2), xend = 0, yend = 0)
  pts$lx <- 1.35 * cos(angs - pi/2); pts$ly <- 1.35 * sin(angs - pi/2)

  ggplot2::ggplot() +
    ggplot2::geom_path(data = rings_df, ggplot2::aes(x = x, y = y, group = r), colour = "#DDDDDD", linewidth = 0.4) +
    ggplot2::annotate("text", x = 0, y = c(0.25, 0.50, 0.75, 1.00) + 0.03, label = c("0.25", "0.50", "0.75", "1.00"), size = 2.3, colour = "#AAAAAA", hjust = 0.5) +
    ggplot2::geom_segment(data = spokes, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), colour = "#CCCCCC", linewidth = 0.5) +
    ggplot2::geom_polygon(data = pts, ggplot2::aes(x = x, y = y), fill = "#2166AC", alpha = 0.25, colour = "#2166AC", linewidth = 1) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = x, y = y), colour = "#2166AC", size = 3, fill = "white", shape = 21, stroke = 1.5) +
    ggplot2::geom_text(data = pts, ggplot2::aes(x = lx, y = ly, label = sprintf("%s\n%.3f", label, value)), size = 2.8, hjust = 0.5, colour = "#333333", fontface = "bold") +
    ggplot2::coord_fixed(xlim = c(-1.7, 1.7), ylim = c(-1.7, 1.7)) + .sem_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "ESEM Score Decomposition", subtitle = sprintf("Final score: %.4f  |  Base score: %.4f", .safe_val(decomp$final_score), .safe_val(decomp$base_score)), caption = "SEMANTICA | all components scaled 0-1")
}

# =================================================================
# PLOT 8 -- SEMANTIC DISCRIMINATION
# =================================================================
#' Within vs between-factor semantic similarity violin plot
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix. Optional for a high-level result because the stored matrix is reused.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_semantic_discrimination(result, cosine_sim_matrix)
#' }
plot_semantic_discrimination <- function(result, cosine_sim_matrix) {
  fa <- result$factor_assignment; best_items <- result$best_items; factors <- unique(fa)
  fac_cols <- setNames(.factor_colours(length(factors)), factors)

  rows <- lapply(factors, function(f) {
    f_items <- names(fa[fa == f]); out_items <- setdiff(best_items, f_items)
    within_v <- if (length(f_items) >= 2L) sapply(combn(f_items, 2, simplify=FALSE), function(p) cosine_sim_matrix[p[1], p[2]]) else numeric(0)
    bt_v <- if (length(f_items) > 0L && length(out_items) > 0L) as.vector(cosine_sim_matrix[f_items, out_items, drop=FALSE]) else numeric(0)
    rbind(if (length(within_v)>0L) data.frame(factor=f, type="Within-factor", sim=within_v) else NULL, if (length(bt_v)>0L) data.frame(factor=f, type="Between-factor", sim=bt_v) else NULL)
  })
  df_sim <- do.call(rbind, rows[!sapply(rows, is.null)])
  if (nrow(df_sim) == 0L) { message("[plot_semantic_discrimination] No pairs."); return(invisible(NULL)) }
  df_sim$factor <- factor(df_sim$factor, levels = factors)
  df_sum <- aggregate(sim ~ factor + type, data = df_sim, FUN = mean); colnames(df_sum)[3] <- "mean_sim"

  ggplot2::ggplot(df_sim, ggplot2::aes(x = type, y = sim, fill = factor, colour = factor)) +
    ggplot2::geom_violin(alpha = 0.30, linewidth = 0.5, trim = FALSE) +
    ggplot2::geom_jitter(width = 0.12, size = 0.8, alpha = 0.5) +
    ggplot2::geom_crossbar(data = df_sum, ggplot2::aes(y = mean_sim, ymin = mean_sim, ymax = mean_sim), width = 0.55, linewidth = 0.8, middle.linewidth = 1.5) +
    ggplot2::geom_hline(yintercept = result$model_info$redundancy_threshold, linetype = "dashed", colour = "#E08214", linewidth = 0.6) +
    ggplot2::annotate("text", x = 0.55, y = result$model_info$redundancy_threshold + 0.01, label = "Redundancy threshold", size = 2.5, hjust = 0, colour = "#E08214") +
    ggplot2::scale_fill_manual(values = fac_cols, guide = "none") + ggplot2::scale_colour_manual(values = fac_cols, guide = "none") +
    ggplot2::facet_wrap(~factor, ncol = 3) + .sem_theme() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA), axis.text.x = ggplot2::element_text(size = 8, angle = 15, hjust = 1)) +
    ggplot2::labs(title = "Within vs Between-Factor Semantic Similarities", subtitle = "Bar = mean; orange dashed = redundancy threshold", x = NULL, y = "Cosine Similarity", caption = "SEMANTICA")
}

# =================================================================
# PLOT 9 -- INTERACTIVE SEMANTIC SPACE
# =================================================================
#' Interactive 2D/3D MDS semantic space
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix. Optional for a high-level result because the stored matrix is reused.
#' @param label_all Label all pool items.
#' @param mode `"2d"` (default) or `"3d"`.
#' @param max_pool_items Maximum pool items included in the MDS computation.
#'   Selected items are always retained. Use `Inf` for the complete pool.
#' @return A `plotly` object.
#' @export
#' @examples
#' \dontrun{
#' plot_interactive_semantic_space(
#'   result, cosine_sim_matrix,
#'   label_all = FALSE,
#'   mode = "2d",
#'   max_pool_items = 250L
#' )
#' }
plot_interactive_semantic_space <- function(result, cosine_sim_matrix, label_all = FALSE,
                                            mode = c("2d", "3d"), max_pool_items = 250L) {
  mode <- match.arg(mode)
  if (!requireNamespace("plotly", quietly = TRUE)) { message("[plot_interactive_semantic_space] 'plotly' required."); return(invisible(NULL)) }

  fa <- result$factor_assignment; best_items <- result$best_items; all_items <- colnames(cosine_sim_matrix)
  eligible <- result$eligible_items
  pool_items <- if (!is.null(eligible)) intersect(unlist(eligible), all_items) else all_items
  pool_display <- .viz_limit_pool_items(pool_items, best_items, cosine_sim_matrix, max_pool_items)
  pool_items <- pool_display$items
  fa_pool <- character(length(pool_items)); names(fa_pool) <- pool_items
  for (f in names(eligible)) fa_pool[intersect(eligible[[f]], pool_items)] <- f
  fa_pool[fa_pool == ""] <- "Other"; fa_pool[best_items] <- fa

  cos_pool <- cosine_sim_matrix[pool_items, pool_items, drop=FALSE]
  dist_mat <- 1 - cos_pool; dist_mat[dist_mat < 0] <- 0; diag(dist_mat) <- 0
  k <- if (mode == "3d") 3L else 2L
  mds <- stats::cmdscale(dist_mat, k = k, eig = FALSE)

  df_mds <- data.frame(item = pool_items, x = mds[,1], y = mds[,2], z = if(k==3) mds[,3] else 0, factor = fa_pool[pool_items], selected = pool_items %in% best_items, stringsAsFactors = FALSE)
  df_mds$hover_txt <- sprintf("<b>%s</b><br>Factor: %s<br>Status: %s", df_mds$item, df_mds$factor, ifelse(df_mds$selected, "SELECTED", "not selected"))
  factors <- unique(df_mds$factor[df_mds$selected])
  fac_cols <- setNames(.factor_colours(length(factors)), factors)

  fig <- plotly::plot_ly()
  df_bg <- df_mds[!df_mds$selected, ]
  if (nrow(df_bg) > 0L) {
    if (mode == "3d") {
      fig <- plotly::add_trace(fig, data = df_bg, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", marker = list(size=3, color="#CCCCCC", opacity=0.35, symbol="circle-open", line=list(width=0.8, color="#AAAAAA")), text = ~hover_txt, hoverinfo = "text", name = "Item pool", showlegend = TRUE)
    } else {
      fig <- plotly::add_trace(fig, data = df_bg, x = ~x, y = ~y, type = "scatter", mode = "markers", marker = list(size=5, color="#CCCCCC", opacity=0.4, symbol="circle-open", line=list(width=1, color="#AAAAAA")), text = ~hover_txt, hoverinfo = "text", name = "Item pool", showlegend = TRUE)
    }
  }
  for (f in factors) {
    df_f <- df_mds[df_mds$selected & df_mds$factor == f, ]
    if (nrow(df_f) == 0L) next
    if (mode == "3d") {
      fig <- plotly::add_trace(fig, data = df_f, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers+text", marker = list(size=8, color=fac_cols[f], opacity=0.9, symbol="circle", line=list(width=1.5, color="#333333")), text = ~item, textposition = "top center", textfont = list(size=9, color="#333333"), hovertext = ~hover_txt, hoverinfo = "text", name = f, showlegend = TRUE)
    } else {
      fig <- plotly::add_trace(fig, data = df_f, x = ~x, y = ~y, type = "scatter", mode = "markers+text", marker = list(size=13, color=fac_cols[f], opacity=0.9, symbol="circle", line=list(width=1.5, color="#333333")), text = ~item, textposition = "top center", textfont = list(size=10, color="#333333"), hovertext = ~hover_txt, hoverinfo = "text", name = f, showlegend = TRUE)
    }
  }

  layout_args <- list(
    title = list(text = sprintf(
      "<b>Semantic Space -- %s MDS%s</b>",
      toupper(mode),
      if (isTRUE(pool_display$reduced)) sprintf(" (display subset %d/%d)", length(pool_items), pool_display$total) else ""
    ), font=list(size=14)),
    legend = list(title=list(text="<b>Factor</b>")),
    paper_bgcolor = "#FFFFFF", hoverlabel = list(bgcolor="white", font=list(size=11)),
    annotations = list(list(text = "SEMANTICA | cosine distance MDS | Large = selected", xref="paper", yref="paper", x=0, y=-0.08, showarrow=FALSE, font=list(size=10, color="#888888")))
  )
  if (mode == "3d") {
    layout_args$scene <- list(xaxis=list(title="MDS Dim 1", zeroline=FALSE, showgrid=TRUE), yaxis=list(title="MDS Dim 2", zeroline=FALSE, showgrid=TRUE), zaxis=list(title="MDS Dim 3", zeroline=FALSE, showgrid=TRUE), bgcolor="#FAFAFA")
  } else {
    layout_args$xaxis <- list(title="MDS Dimension 1", zeroline=FALSE, showgrid=TRUE, gridcolor="#EEEEEE")
    layout_args$yaxis <- list(title="MDS Dimension 2", zeroline=FALSE, showgrid=TRUE, gridcolor="#EEEEEE")
    layout_args$plot_bgcolor <- "#FAFAFA"
  }
  do.call(plotly::layout, c(list(p = fig), layout_args))
}

# =================================================================
# PLOT 10 -- ESEM PATH DIAGRAMS (BEFORE/AFTER)
# =================================================================
#' ESEM path diagrams: BEFORE and AFTER ACO selection
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix. Optional for a high-level result because the stored matrix is reused.
#' @param min_loading_show Minimum absolute loading to show.
#' @param show_crossloadings Logical; show cross-loadings.
#' @param df Optional item metadata dataframe used to represent the generated
#'   item pool in the BEFORE diagram. Without metadata, the function falls
#'   back to the screened eligible pool stored in `result`.
#' @param min_factor_cor_show Minimum absolute latent factor correlation to
#'   draw in the side rails.
#' @param max_factor_cor_edges Maximum number of factor-correlation rails.
#' @param before_model How to derive the BEFORE path diagram. `"proxy"` (the
#'   default) uses a fast sample-free PFA/semantic proxy and never estimates a
#'   new full-pool ESEM during plotting. `"refit"` requests a new ESEM fit,
#'   subject to `before_refit_max_items`.
#' @param before_refit_max_items Maximum BEFORE-pool size for an explicitly
#'   requested ESEM refit. This guard prevents plotting from initiating an
#'   unexpectedly expensive full-pool estimation job. Use `Inf` only after
#'   deliberately assessing the computational cost.
#' @param before_proxy_max_items Maximum BEFORE-pool items represented in the
#'   default fast proxy display. Selected items are always retained; use `Inf`
#'   for a complete-pool proxy.
#' @return Named list of two `ggplot` objects (`p10a`, `p10b`).
#' @export
#' @examples
#' \dontrun{
#' paths <- plot_esem_path_diagrams(
#'   result, cosine_sim_matrix,
#'   min_loading_show = 0.10,
#'   show_crossloadings = TRUE,
#'   before_model = "proxy"
#' )
#' paths$p10b
#' }
plot_esem_path_diagrams <- function(result, cosine_sim_matrix, min_loading_show = 0.10,
                                    show_crossloadings = TRUE, df = NULL,
                                    min_factor_cor_show = 0.05,
                                    max_factor_cor_edges = 15L,
                                    before_model = c("proxy", "refit"),
                                    before_refit_max_items = 60L,
                                    before_proxy_max_items = 150L) {
  before_model <- match.arg(before_model)
  before_refit_max_items <- suppressWarnings(as.numeric(before_refit_max_items[1L]))
  if (!is.finite(before_refit_max_items) && !is.infinite(before_refit_max_items)) {
    stop("'before_refit_max_items' must be a positive number or Inf.")
  }
  if (before_refit_max_items < 1) {
    stop("'before_refit_max_items' must be a positive number or Inf.")
  }
  build_esem_syntax_fun <- .viz_get_function("build_esem_syntax_safe")
  run_esem_fun <- .viz_get_function("run_esem_on_matrix")
  transform_cosine_fun <- .viz_get_function("transform_cosine_for_esem")
  prepare_rotation_fun <- .viz_get_function("prepare_esem_rotation_args")
  compute_pfa_fun <- .viz_get_function("compute_pfa_diagnostics")
  can_refit_before <- !is.null(build_esem_syntax_fun) &&
    !is.null(run_esem_fun) &&
    !is.null(transform_cosine_fun) &&
    !is.null(prepare_rotation_fun)

  fa_final <- result$factor_assignment; best_items <- result$best_items
  if (is.null(fa_final) || length(fa_final) == 0L || is.null(best_items)) {
    empty <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "Plot 10\n(result object missing selected items or factor assignment)",
                        size = 5, colour = "#888888") +
      .sem_theme() +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())
    return(list(p10a = empty, p10b = empty))
  }
  fa_final <- as.character(fa_final)
  names(fa_final) <- names(result$factor_assignment)
  factors <- unique(fa_final)
  model_info <- result$model_info %||% list()
  fac_cols <- setNames(.factor_colours(length(factors)), factors)

  .eligible_from_df <- function(df_meta) {
    if (is.null(df_meta) || !is.data.frame(df_meta)) return(NULL)
    id_col <- intersect(c("item", "item_id", "ID"), names(df_meta))
    factor_col <- intersect(c("factor", "type", "Dimension"), names(df_meta))
    if (length(id_col) == 0L || length(factor_col) == 0L) return(NULL)
    ids <- as.character(df_meta[[id_col[1L]]])
    fac <- as.character(df_meta[[factor_col[1L]]])
    keep <- nzchar(ids) & nzchar(fac) & ids %in% colnames(cosine_sim_matrix)
    if (!any(keep)) return(NULL)
    split(ids[keep], fac[keep])
  }

  eligible <- .eligible_from_df(df)
  before_pool_scope <- "Full generated pool"
  if (is.null(eligible) || length(eligible) == 0L) {
    eligible <- result$eligible_items
    before_pool_scope <- "Screened eligible pool"
  }
  if (is.null(eligible) || length(eligible) == 0L) {
    eligible <- lapply(factors, function(f) names(fa_final[fa_final == f]))
    names(eligible) <- factors
    before_pool_scope <- "Selected-item fallback pool"
  } else if (is.null(names(eligible)) && length(eligible) == length(factors)) {
    names(eligible) <- factors
  }
  eligible <- lapply(factors, function(f) {
    unique(as.character(eligible[[f]] %||% character(0L)))
  })
  names(eligible) <- factors

  .df_load_from_fit <- function(fit, fa_assign) {
    if (is.null(fit)) return(NULL)
    lmat <- .viz_lambda_matrix(fit, fa_assign, factors)
    if (is.null(lmat) || !is.matrix(lmat)) return(NULL)
    rows <- lapply(factors, function(f) {
      fc <- .viz_factor_col(lmat, f)
      if (is.na(fc)) return(NULL)
      data.frame(
        item = rownames(lmat),
        factor = f,
        loading = as.numeric(lmat[, fc]),
        dominant = rownames(lmat) %in% names(fa_assign[fa_assign == f]),
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    if (length(rows) == 0L) NULL else do.call(rbind, rows)
  }

  .factor_cor_from_pfa <- function(pfa) {
    phi <- pfa$factor_correlations
    mapping <- suppressWarnings(as.integer(pfa$factor_mapping[factors]))
    if (is.null(phi) || !is.matrix(phi) || any(!is.finite(mapping)) ||
        any(mapping < 1L) || any(mapping > nrow(phi))) {
      return(NULL)
    }
    out <- phi[mapping, mapping, drop = FALSE]
    rownames(out) <- colnames(out) <- factors
    diag(out) <- 1
    out
  }

  .pfa_path_proxy <- function(cos_pool, fa_pool) {
    if (is.null(compute_pfa_fun)) return(NULL)
    pfa <- tryCatch(
      compute_pfa_fun(
        cos_pool, fa_pool, factors,
        extraction = "principal",
        rotation = "target_oblique",
        min_loading = max(0.10, min_loading_show),
        min_margin = NULL
      ),
      error = function(e) NULL
    )
    if (is.null(pfa) || !isTRUE(pfa$available) || is.null(pfa$loadings)) return(NULL)
    loadings <- as.matrix(pfa$loadings)
    mapping <- suppressWarnings(as.integer(pfa$factor_mapping[factors]))
    if (any(!is.finite(mapping))) return(NULL)
    rows <- lapply(seq_along(factors), function(j) {
      f <- factors[j]
      fc <- mapping[j]
      if (!is.finite(fc) || fc < 1L || fc > ncol(loadings)) return(NULL)
      data.frame(
        item = rownames(loadings),
        factor = f,
        loading = as.numeric(loadings[, fc]),
        dominant = rownames(loadings) %in% names(fa_pool[fa_pool == f]),
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    if (length(rows) == 0L) return(NULL)
    list(
      df = do.call(rbind, rows),
      cor = .factor_cor_from_pfa(pfa),
      label = "PFA fallback",
      note = "Full-pool ESEM was unavailable; BEFORE uses sample-free PFA loadings from the semantic proxy."
    )
  }

  .semantic_path_proxy <- function(cos_pool, fa_pool) {
    if (is.null(cos_pool) || nrow(cos_pool) < 2L) return(NULL)
    cor_pool <- .viz_transform_cosine_for_display(cos_pool, fa_pool, factors)
    rows <- lapply(factors, function(f) {
      f_items <- intersect(names(fa_pool[fa_pool == f]), rownames(cor_pool))
      if (length(f_items) == 0L) return(NULL)
      vals <- vapply(rownames(cor_pool), function(item_id) {
        refs <- setdiff(f_items, item_id)
        if (length(refs) == 0L) refs <- f_items
        mean(cor_pool[item_id, refs, drop = TRUE], na.rm = TRUE)
      }, numeric(1L))
      data.frame(
        item = rownames(cor_pool),
        factor = f,
        loading = pmax(-1, pmin(1, vals)),
        dominant = rownames(cor_pool) %in% f_items,
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    if (length(rows) == 0L) return(NULL)
    fc <- matrix(NA_real_, nrow = length(factors), ncol = length(factors),
                 dimnames = list(factors, factors))
    diag(fc) <- 1
    for (a in seq_along(factors)) {
      for (b in seq_along(factors)) {
        if (a >= b) next
        ai <- intersect(names(fa_pool[fa_pool == factors[a]]), rownames(cor_pool))
        bi <- intersect(names(fa_pool[fa_pool == factors[b]]), rownames(cor_pool))
        val <- if (length(ai) > 0L && length(bi) > 0L) mean(cor_pool[ai, bi, drop = TRUE], na.rm = TRUE) else NA_real_
        fc[a, b] <- fc[b, a] <- val
      }
    }
    list(
      df = do.call(rbind, rows),
      cor = fc,
      label = "semantic proxy",
      note = "Full-pool ESEM/PFA was unavailable; BEFORE uses mean semantic association as a visual proxy."
    )
  }

  .make_corr_layers <- function(fac_nodes, factor_cor) {
    if (is.null(factor_cor) || length(factors) < 2L) return(list(layers = list(), labels = NULL))
    fc <- as.matrix(factor_cor)
    if (!all(factors %in% rownames(fc)) || !all(factors %in% colnames(fc))) return(list(layers = list(), labels = NULL))
    fc <- fc[factors, factors, drop = FALSE]
    pair_idx <- which(upper.tri(fc), arr.ind = TRUE)
    if (nrow(pair_idx) == 0L) return(list(layers = list(), labels = NULL))
    corr_edges <- data.frame(
      f1 = rownames(fc)[pair_idx[, 1L]],
      f2 = colnames(fc)[pair_idx[, 2L]],
      r = as.numeric(fc[pair_idx]),
      stringsAsFactors = FALSE
    )
    corr_edges <- corr_edges[is.finite(corr_edges$r) & abs(corr_edges$r) >= min_factor_cor_show, , drop = FALSE]
    if (nrow(corr_edges) == 0L) return(list(layers = list(), labels = NULL))
    corr_edges <- corr_edges[order(abs(corr_edges$r), decreasing = TRUE), , drop = FALSE]
    max_edges <- max(1L, as.integer(max_factor_cor_edges))
    corr_edges <- utils::head(corr_edges, max_edges)

    y_map <- stats::setNames(fac_nodes$y, fac_nodes$factor)
    fac_x <- unique(fac_nodes$x)[1L]
    x_start <- fac_x - 0.085
    max_abs <- max(abs(corr_edges$r), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs <= min_factor_cor_show) max_abs <- min_factor_cor_show + 0.01

    layers <- list()
    label_rows <- vector("list", nrow(corr_edges))
    for (i in seq_len(nrow(corr_edges))) {
      row <- corr_edges[i, ]
      y0 <- y_map[[row$f1]]
      y1 <- y_map[[row$f2]]
      if (!is.finite(y0) || !is.finite(y1)) next
      span <- abs(match(row$f1, factors) - match(row$f2, factors))
      x_ctrl <- max(0.012, x_start - 0.016 * span)
      tt <- seq(0, 1, length.out = 45)
      path_df <- data.frame(
        x = (1 - tt)^2 * x_start + 2 * (1 - tt) * tt * x_ctrl + tt^2 * x_start,
        y = (1 - tt) * y0 + tt * y1,
        stringsAsFactors = FALSE
      )
      line_col <- if (row$r >= 0) "#5E81AC" else "#BF616A"
      line_width <- 0.35 + 1.35 * (abs(row$r) - min_factor_cor_show) / (max_abs - min_factor_cor_show)
      line_alpha <- 0.35 + 0.45 * (abs(row$r) - min_factor_cor_show) / (max_abs - min_factor_cor_show)
      layers[[length(layers) + 1L]] <- ggplot2::geom_path(
        data = path_df,
        ggplot2::aes(x = x, y = y),
        colour = line_col,
        linewidth = line_width,
        alpha = line_alpha,
        inherit.aes = FALSE
      )
      label_rows[[i]] <- data.frame(
        x = x_ctrl - 0.004,
        y = mean(c(y0, y1)),
        label = sprintf("r=%.2f", row$r),
        stringsAsFactors = FALSE
      )
    }
    label_df <- do.call(rbind, label_rows[!vapply(label_rows, is.null, logical(1L))])
    if (!is.null(label_df) && nrow(label_df) > 0L) {
      label_df <- utils::head(label_df, if (nrow(corr_edges) <= 10L) 10L else 8L)
      layers[[length(layers) + 1L]] <- ggplot2::geom_label(
        data = label_df,
        ggplot2::aes(x = x, y = y, label = label),
        size = 1.9,
        label.padding = grid::unit(0.08, "lines"),
        linewidth = 0.15,
        fill = "#FAFAFA",
        colour = "#333333",
        alpha = 0.92,
        inherit.aes = FALSE
      )
    }
    list(layers = layers, labels = label_df)
  }

  .make_path_gg <- function(df_load, fa_assign, phase, n_pool,
                            factor_cor = NULL, fit_label = "ESEM",
                            fit_note = NULL) {
    if (is.null(df_load) || nrow(df_load) == 0L) {
      unavailable_label <- paste0(
        "Plot 10-", substr(phase, 1, 1),
        "\n(", phase, " unavailable)",
        if (!is.null(fit_note) && nzchar(fit_note)) paste0("\n", fit_note) else ""
      )
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0.5, y = 0.5, label = unavailable_label,
                                 size = 5, colour = "#888888", hjust = 0.5) +
               .sem_theme() +
               ggplot2::theme(axis.text = ggplot2::element_blank(),
                              axis.title = ggplot2::element_blank(),
                              panel.grid = ggplot2::element_blank()) +
               ggplot2::labs(title = paste(phase, "-- Path Diagram (unavailable)")))
    }
    items_in <- unique(df_load$item); n_items <- length(items_in); n_factors <- length(factors)
    item_order <- unlist(lapply(factors, function(f) {
      f_items <- intersect(names(fa_assign[fa_assign == f]), items_in)
      dom_loads <- sapply(f_items, function(it) { v <- df_load$loading[df_load$item == it & df_load$factor == f & df_load$dominant]; if(length(v)==0) 0 else abs(v[1]) })
      f_items[order(dom_loads, decreasing=TRUE)]
    }))
    item_order <- c(item_order, setdiff(items_in, item_order))

    fac_y <- seq(0.95, 0.05, length.out = n_factors)
    item_y <- seq(0.97, 0.03, length.out = n_items)
    fac_nodes <- data.frame(node_id = paste0("F_", factors), label = .viz_factor_label(factors),
                            x = 0.21, y = fac_y, type = "factor", factor = factors,
                            stringsAsFactors = FALSE)
    item_nodes <- data.frame(node_id = paste0("I_", item_order), label = item_order,
                             x = 0.88, y = item_y[seq_along(item_order)], type = "item",
                             factor = fa_assign[item_order], stringsAsFactors = FALSE)
    nodes <- rbind(fac_nodes, item_nodes); rownames(nodes) <- nodes$node_id

    edges_raw <- df_load[abs(df_load$loading) >= min_loading_show, ]
    if (!show_crossloadings) edges_raw <- edges_raw[edges_raw$dominant, ]
    edge_list <- lapply(seq_len(nrow(edges_raw)), function(i) {
      row <- edges_raw[i, ]; from_id <- paste0("F_", row$factor); to_id <- paste0("I_", row$item)
      if (!from_id %in% nodes$node_id || !to_id %in% nodes$node_id) return(NULL)
      data.frame(from = from_id, to = to_id, loading = row$loading, dominant = row$dominant, stringsAsFactors = FALSE)
    })
    edges <- do.call(rbind, Filter(Negate(is.null), edge_list))
    if (is.null(edges) || nrow(edges) == 0L) edges <- data.frame(from=character(0), to=character(0), loading=numeric(0), dominant=logical(0))

    edges$abs_load <- abs(edges$loading)
    edges$lwd <- 0.25
    if (any(edges$dominant)) {
      dom_abs <- edges$abs_load[edges$dominant]
      edges$lwd[edges$dominant] <- if (diff(range(dom_abs, na.rm = TRUE)) < 1e-8) {
        rep(1.6, length(dom_abs))
      } else {
        scales::rescale(dom_abs, to = c(0.6, 2.5))
      }
    }
    edges$alpha_val <- ifelse(edges$dominant, 0.92, 0.28)
    edges$edge_col <- ifelse(edges$dominant, fac_cols[sub("^F_", "", edges$from)], "#888888")

    get_xy <- function(id, col) nodes[id, col]
    if (nrow(edges) > 0) {
      edges$x0 <- get_xy(edges$from, "x"); edges$y0 <- get_xy(edges$from, "y")
      edges$x1 <- get_xy(edges$to, "x"); edges$y1 <- get_xy(edges$to, "y")
      edges$xmid <- edges$x0 + 0.70 * (edges$x1 - edges$x0)
      edges$ymid <- edges$y0 + 0.70 * (edges$y1 - edges$y0)
      # Only dominant edges receive labels below, and their prior random jitter was
      # always zero. Keep label geometry deterministic and RNG-neutral.
      edges$ymid_j <- edges$ymid
    }
    label_threshold <- if (phase == "BEFORE") 0.25 else 0.10
    edges$show_label <- edges$dominant & edges$abs_load >= label_threshold
    edges$load_label <- sprintf("%.2f", edges$loading)

    seg_list <- if (nrow(edges) > 0) lapply(seq_len(nrow(edges)), function(i) {
      e <- edges[i, ]
      list(
        ggplot2::annotate("segment", x = e$x0 + 0.055, xend = e$x1 - 0.040, y = e$y0, yend = e$y1, colour = e$edge_col, linewidth = e$lwd, alpha = e$alpha_val, arrow = grid::arrow(length = grid::unit(ifelse(e$dominant, 5, 3), "pt"), type = "closed")),
        if (e$show_label) ggplot2::annotate("label", x = e$xmid, y = e$ymid_j, label = e$load_label, size = ifelse(phase == "AFTER", 2.8, 2.0), colour = e$edge_col, fill = "white", label.padding = grid::unit(0.12, "lines"), fontface = "bold", alpha = 0.9) else NULL
      )
    }) else list()

    use_ggforce <- requireNamespace("ggforce", quietly = TRUE)
    edge_layers <- if (length(seg_list) > 0) unlist(seg_list, recursive = FALSE) else list()
    factor_layer <- if (use_ggforce) {
      ggforce::geom_ellipse(data = fac_nodes, ggplot2::aes(x0 = x, y0 = y, a = 0.075, b = 0.036, angle = 0, fill = factor), colour = "#333333", linewidth = 0.7, alpha = 0.90)
    } else {
      ggplot2::geom_point(data = fac_nodes, ggplot2::aes(x = x, y = y), size = 17, shape = 21, colour = "#333333", stroke = 1, alpha = 0.85)
    }
    corr_layers <- .make_corr_layers(fac_nodes, factor_cor)$layers
    fit_note_txt <- if (!is.null(fit_note) && nzchar(fit_note)) paste0(" | ", fit_note) else ""

    p <- ggplot2::ggplot() +
      corr_layers +
      edge_layers +
      factor_layer +
      ggplot2::scale_fill_manual(values = fac_cols, name = "Factor", guide = ggplot2::guide_legend(override.aes = list(size = 4))) +
      ggplot2::geom_text(data = fac_nodes, ggplot2::aes(x = x, y = y, label = label), size = 2.7, lineheight = 0.82, fontface = "bold", colour = "white") +
      ggplot2::geom_rect(data = item_nodes, ggplot2::aes(xmin = x - 0.038, xmax = x + 0.038, ymin = y - 0.018, ymax = y + 0.018, colour = factor), fill = "white", linewidth = 0.6, inherit.aes = FALSE) +
      ggplot2::scale_colour_manual(values = fac_cols, guide = "none") +
      ggplot2::geom_text(data = item_nodes, ggplot2::aes(x = x, y = y, label = label, colour = factor), size = ifelse(phase == "BEFORE", 1.8, 2.5), fontface = "bold") +
      ggplot2::annotate("text", x = 0.52, y = 1.015, label = sprintf("%s %s -- %s Path Diagram (%d items, %d factors)", ifelse(phase=="BEFORE", "10-A |", "10-B |"), phase, fit_label, n_items, n_factors), size = 4.5, fontface = "bold", colour = ifelse(phase=="BEFORE", "#8B1A1A", "#1A4B8B"), hjust = 0.5) +
      ggplot2::annotate("text", x = 0.50, y = -0.015, label = paste0("Thick coloured = dominant  |  Thin grey = cross-loadings (|lambda| >= ", min_loading_show, ")  |  Side rails = factor correlations (|r| >= ", min_factor_cor_show, ")  |  Rotation: ", model_info$rotation %||% "unknown"), size = 2.35, colour = "#666666", hjust = 0.5) +
      ggplot2::coord_cartesian(xlim = c(0.00, 0.99), ylim = c(-0.04, 1.04), clip = "off") + .sem_theme() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "right", legend.title = ggplot2::element_text(face = "bold", size = 9), plot.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA), panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA), plot.margin = ggplot2::margin(20, 10, 15, 10)) +
      ggplot2::labs(caption = paste0("SEMANTICA | ACO-ESEM | ",
                                     ifelse(phase == "BEFORE", before_pool_scope, "ACO-selected items"),
                                     fit_note_txt))
    p
  }

  after_fit <- result$esem_fit
  df_after <- .df_load_from_fit(after_fit, fa_final)
  cor_after <- .viz_factor_correlation_matrix(
    after_fit, factors, factor_assignment = fa_final
  )

  pool_items <- intersect(unlist(eligible, use.names=FALSE), colnames(cosine_sim_matrix))
  fa_pool <- character(length(pool_items)); names(fa_pool) <- pool_items
  for (f in names(eligible)) fa_pool[intersect(eligible[[f]], pool_items)] <- f
  fa_pool[fa_pool == ""] <- NA_character_
  df_before <- NULL; cor_before <- NULL
  before_label <- "ESEM"; before_note <- NULL
  before_display_items <- pool_items
  before_display_fa <- fa_pool
  requested_before_refit <- identical(before_model, "refit")
  before_refit_too_large <- requested_before_refit &&
    length(pool_items) > before_refit_max_items
  if (before_refit_too_large) {
    message(sprintf(
      "[plot_esem_path_diagrams] Skipping requested BEFORE ESEM refit: %d items exceeds the plotting guard of %s. Using a fast proxy.",
      length(pool_items),
      if (is.infinite(before_refit_max_items)) "Inf" else format(before_refit_max_items, trim = TRUE)
    ))
  }
  if (requested_before_refit && !isTRUE(can_refit_before)) {
    message("[plot_esem_path_diagrams] BEFORE ESEM refit functions are unavailable. Using a fast proxy.")
  }
  if (requested_before_refit && !before_refit_too_large &&
      isTRUE(can_refit_before) && length(pool_items) >= length(factors) + 2L) {
    message(sprintf("[plot_esem_path_diagrams] Running explicitly requested BEFORE ESEM refit for %d items.", length(pool_items)))
    before_fit <- tryCatch({
      cos_pool <- cosine_sim_matrix[pool_items, pool_items, drop=FALSE]
      cor_pool <- transform_cosine_fun(cos_pool, fa_pool, factors)
      syn_pool <- build_esem_syntax_fun(pool_items, fa_pool, factors)
      rot_args <- prepare_rotation_fun(
        model_info$rotation %||% "geomin",
        model_info$rotation_args %||% list(geomin.epsilon = 0.50),
        pool_items, fa_pool, factors
      )
      run_esem_fun(
        syn_pool, cor_pool,
        n_obs = model_info$n_obs %||% 300L,
        estimator = model_info$estimator %||% "ML",
        rotation = model_info$rotation %||% "geomin",
        rotation_args = rot_args,
        iter_max = model_info$full_esem_iter_max %||% 2000L
      )
    }, error = function(e) { message("[plot_esem_path_diagrams] BEFORE model failed: ", e$message); NULL })
    df_before <- .df_load_from_fit(before_fit, fa_pool)
    cor_before <- .viz_factor_correlation_matrix(
      before_fit, factors, factor_assignment = fa_pool
    )
  }
  if ((is.null(df_before) || nrow(df_before) == 0L) && length(pool_items) >= 2L) {
    proxy_display <- .viz_limit_pool_items(
      pool_items, best_items, cosine_sim_matrix, before_proxy_max_items
    )
    before_display_items <- proxy_display$items
    before_display_fa <- fa_pool[before_display_items]
    if (isTRUE(proxy_display$reduced)) {
      before_pool_scope <- sprintf(
        "%s (display subset %d/%d)",
        before_pool_scope, length(before_display_items), proxy_display$total
      )
    }
    cos_pool <- cosine_sim_matrix[before_display_items, before_display_items, drop = FALSE]
    proxy <- .pfa_path_proxy(cos_pool, before_display_fa)
    if (is.null(proxy)) proxy <- .semantic_path_proxy(cos_pool, before_display_fa)
    if (!is.null(proxy)) {
      if (!requested_before_refit) {
        proxy$label <- sub(" fallback$", " proxy", proxy$label)
        proxy$note <- "Fast default: BEFORE uses a sample-free semantic-proxy representation; no new full-pool ESEM is fitted during plotting."
      } else if (before_refit_too_large) {
        proxy$label <- sub(" fallback$", " proxy", proxy$label)
        proxy$note <- "Requested BEFORE ESEM refit exceeded the plotting size guard; a sample-free semantic-proxy representation is shown."
      }
      if (isTRUE(proxy_display$reduced)) {
        proxy$note <- paste(
          proxy$note,
          sprintf("Display is limited to %d of %d pool items; all selected items are retained.",
                  length(before_display_items), proxy_display$total)
        )
      }
      df_before <- proxy$df
      cor_before <- proxy$cor
      before_label <- proxy$label
      before_note <- proxy$note
    }
  }

  list(
    p10a = .make_path_gg(
      df_before, before_display_fa, "BEFORE", length(before_display_items),
      factor_cor = cor_before, fit_label = before_label, fit_note = before_note
    ),
    p10b = .make_path_gg(
      df_after, fa_final, "AFTER", length(best_items),
      factor_cor = cor_after, fit_label = "ESEM", fit_note = NULL
    )
  )
}

# =================================================================
# PLOT 11 -- ITEM SELECTION FREQUENCY
# =================================================================
#' Item selection frequency map across archive or multi-seed runs
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param multi_result Output from `run_multi_seed_semantica()` (optional).
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_item_selection_frequency(result = result)
#' plot_item_selection_frequency(multi_result = multi)
#' }
plot_item_selection_frequency <- function(result = NULL, multi_result = NULL) {
  if (!is.null(multi_result) && !is.null(multi_result$item_frequencies)) {
    freq_raw <- multi_result$item_frequencies; n_runs <- multi_result$n_successful
    freq_df <- data.frame(item = names(freq_raw), n_sel = as.integer(freq_raw), freq = as.numeric(freq_raw)/n_runs, stringsAsFactors = FALSE)
    valid_results <- Filter(Negate(is.null), multi_result$all_results)
    fa_all <- character(0); eligible <- list()
    if (length(valid_results) > 0L) {
      r0 <- valid_results[[1L]]; fa_all <- r0$factor_assignment; eligible <- r0$eligible_items
      for (f in names(eligible)) fa_all[setdiff(eligible[[f]], names(fa_all))] <- f
    }
    consensus_items <- multi_result$consensus_items
    plot_title <- sprintf("Item Selection Frequency -- %d Seeds", n_runs)
    plot_subtitle <- sprintf("Consensus items (>=50%% runs): %d", length(consensus_items))
  } else if (!is.null(result)) {
    archive <- result$elite_archive; fa_final <- result$factor_assignment; eligible <- result$eligible_items; best_items <- result$best_items
    all_eligible <- if (!is.null(eligible)) unique(unlist(eligible, use.names=FALSE)) else names(fa_final)
    fa_all <- character(length(all_eligible)); names(fa_all) <- all_eligible
    for (f in names(eligible)) fa_all[intersect(eligible[[f]], all_eligible)] <- f
    fa_all[best_items] <- fa_final[best_items]; fa_all[fa_all == ""] <- "Unassigned"
    n_runs <- length(archive)
    if (n_runs == 0L) {
      message("[plot_item_selection_frequency] No elite archive entries available.")
      return(invisible(NULL))
    }
    sel_counts <- integer(length(all_eligible)); names(sel_counts) <- all_eligible
    for (k in seq_len(n_runs)) { sel <- names(archive[[k]]$vec)[archive[[k]]$vec == 1L]; hit <- intersect(sel, all_eligible); sel_counts[hit] <- sel_counts[hit] + 1L }
    freq_df <- data.frame(item = all_eligible, n_sel = sel_counts, freq = sel_counts/n_runs, stringsAsFactors = FALSE)
    consensus_items <- best_items
    plot_title <- sprintf("Item Selection Frequency -- %d Elite Archive Entries", n_runs)
    plot_subtitle <- sprintf("Final selected items (n=%d) shown in red", length(best_items))
  } else {
    message("[plot_item_selection_frequency] Supply `result` or `multi_result`."); return(invisible(NULL))
  }

  factors <- unique(fa_all[names(fa_all) != ""]); factors <- factors[factors != "Unassigned"]
  fac_cols <- setNames(.factor_colours(length(factors)), factors); fac_cols["Unassigned"] <- "#CCCCCC"
  freq_df$factor <- fa_all[freq_df$item]; freq_df$factor[is.na(freq_df$factor) | freq_df$factor == ""] <- "Unassigned"
  freq_df$is_final <- freq_df$item %in% consensus_items
  freq_df <- freq_df[order(freq_df$factor, -freq_df$freq), ]
  freq_df$item <- factor(freq_df$item, levels = rev(freq_df$item))
  freq_df$zone <- cut(freq_df$freq, breaks = c(-Inf, 0.10, 0.40, 0.60, 0.90, Inf), labels = c("Never", "Rare", "Swing", "Frequent", "Anchor"))
  freq_df$label_col <- ifelse(freq_df$is_final, "#C0392B", "#333333")

  ggplot2::ggplot(freq_df, ggplot2::aes(x = freq, y = item)) +
    ggplot2::annotate("rect", xmin=0, xmax=0.10, ymin=-Inf, ymax=Inf, fill="#F8F8F8", alpha=0.7) +
    ggplot2::annotate("rect", xmin=0.90, xmax=1.00, ymin=-Inf, ymax=Inf, fill="#E0F0FF", alpha=0.7) +
    ggplot2::geom_col(ggplot2::aes(fill = factor), width = 0.7, alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = item, label = as.character(item), colour = label_col), hjust = 1.05, size = 2.2, inherit.aes = FALSE) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::geom_point(data = freq_df[freq_df$is_final, ], ggplot2::aes(x = freq, y = item), colour = "#C0392B", shape = 18, size = 3.5, inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = 0.50, linetype = "dashed", colour = "#888888", linewidth = 0.7) +
    ggplot2::annotate("text", x=0.05, y=nrow(freq_df)*0.97, label="<10%\nnever", size=2.3, colour="#999999", hjust=0.5) +
    ggplot2::annotate("text", x=0.95, y=nrow(freq_df)*0.97, label=">90%\nanchor", size=2.3, colour="#2166AC", hjust=0.5) +
    ggplot2::scale_fill_manual(values = fac_cols, name = "Factor") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.10, 1.02)) +
    ggplot2::facet_grid(rows = ggplot2::vars(factor), scales = "free_y", space = "free_y") + .sem_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), strip.text.y = ggplot2::element_text(angle=0, size=8, face="bold"), strip.background = ggplot2::element_rect(fill="#F2F2F2", colour=NA), panel.spacing = ggplot2::unit(0.2, "lines"), legend.position = "none") +
    ggplot2::labs(title = plot_title, subtitle = paste0(plot_subtitle, " | Red + diamond = selected; blue zone = anchors"), x = "Selection Frequency", y = NULL, caption = "SEMANTICA")
}

# =================================================================
# PLOT 12 -- CROSS-LOADING SPECIFICITY HEATMAP
# =================================================================
#' Cross-loading specificity ratio heatmap
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param ratio_cap Maximum ratio cap.
#' @param danger_ratio Legacy argument name for the cross-loading ratio review threshold; cells at or below this value receive a review border.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_crossloading_specificity(result, ratio_cap = 10, danger_ratio = 1.5)
#' }
plot_crossloading_specificity <- function(result, ratio_cap = 10, danger_ratio = 1.5) {
  esem_fit <- result$esem_fit
  if (is.null(esem_fit)) { message("[plot_crossloading_specificity] No ESEM fit."); return(invisible(NULL)) }
  fa <- result$factor_assignment
  best_items <- result$best_items
  factors <- unique(fa)
  lambda_mat <- .viz_lambda_matrix(esem_fit, fa, factors)
  if (is.null(lambda_mat)) { message("[plot_crossloading_specificity] No lambda matrix."); return(invisible(NULL)) }

  fac_cols <- setNames(.factor_colours(length(factors)), factors)

  match_factor_col <- function(f) {
    .viz_factor_col(lambda_mat, f)
  }

  dom_col <- sapply(best_items, function(it) match_factor_col(fa[it]))
  ratio_rows <- lapply(best_items, function(it) {
    dom_f <- fa[it]; dom_c <- dom_col[it]; if (is.na(dom_c)) return(NULL)
    dom_lam <- abs(lambda_mat[it, dom_c])
    lapply(factors, function(f2) {
      f2_col <- match_factor_col(f2); if (is.na(f2_col)) return(NULL)
      cross_lam <- abs(lambda_mat[it, f2_col[1L]])
      ratio <- if (f2 == dom_f) NA_real_ else if (cross_lam < 1e-6) ratio_cap else min(dom_lam / cross_lam, ratio_cap)
      type <- if (f2 == dom_f) "dominant" else ifelse(ratio <= danger_ratio, "review", ifelse(ratio <= 3.0, "moderate", "specific"))
      data.frame(item = it, factor = f2, dom_loading = dom_lam, cross_loading = cross_lam, ratio = ratio, type = type, stringsAsFactors = FALSE)
    })
  })
  flat_rows <- Filter(Negate(is.null), do.call(c, ratio_rows))
  if (length(flat_rows) == 0L) { message("[plot_crossloading_specificity] No usable loading rows."); return(invisible(NULL)) }
  df_ratio <- do.call(rbind, flat_rows)
  df_ratio <- df_ratio[!is.na(df_ratio$item), ]

  mean_spec <- tapply(df_ratio$ratio[df_ratio$type != "dominant"], df_ratio$item[df_ratio$type != "dominant"], function(x) mean(x, na.rm=TRUE))
  item_ord <- unlist(lapply(factors, function(f) {
    fi <- intersect(names(fa[fa == f]), best_items); fi[order(mean_spec[fi], decreasing=TRUE)]
  }))
  df_ratio$item <- factor(df_ratio$item, levels = rev(item_ord))
  df_ratio$factor <- factor(df_ratio$factor, levels = factors)

  .ratio_to_hex <- function(ratio, lo = 1, hi = ratio_cap) {
    t <- pmax(0, pmin(1, (ratio - lo) / (hi - lo)))
    grDevices::colorRampPalette(c("#D6604D", "#FFFFBF", "#4DAC26"))(101)[round(t * 100) + 1L]
  }
  df_ratio$fill_hex <- mapply(function(type, f, ratio) {
    if (type == "dominant") {
      fc <- fac_cols[f]
      return(if (length(fc) == 0L || is.na(fc)) "#AAAAAA" else unname(fc))
    }
    if (is.na(ratio)) return("#F0F0F0")
    .ratio_to_hex(ratio)
  }, df_ratio$type, as.character(df_ratio$factor), df_ratio$ratio, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  df_ratio$label <- ifelse(df_ratio$type == "dominant", sprintf("%.2f\n(dom)", df_ratio$dom_loading), ifelse(is.na(df_ratio$ratio), "--", sprintf("%.1fx", df_ratio$ratio)))
  df_ratio$label_col <- ifelse(df_ratio$type == "dominant", "#FFFFFF", ifelse(!is.na(df_ratio$ratio) & df_ratio$ratio <= danger_ratio, "#FFFFFF", "#333333"))

  ggplot2::ggplot(df_ratio, ggplot2::aes(x = factor, y = item)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill_hex), colour = "white", linewidth = 0.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = label_col), size = 2.2, fontface = "bold", lineheight = 0.85) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_tile(data = df_ratio[!is.na(df_ratio$ratio) & df_ratio$type != "dominant" & df_ratio$ratio <= danger_ratio, ], ggplot2::aes(x = factor, y = item), fill = NA, colour = "#E83030", linewidth = 1.2, inherit.aes = FALSE) +
    .sem_theme(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face="bold", size=9, angle=25, hjust=1), axis.text.y = ggplot2::element_text(size=8), panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "Cross-Loading Specificity Heatmap", subtitle = sprintf("Cell = |dominant lambda| / |cross lambda|. Red border = review threshold (<= %.1fx).", danger_ratio), x = "Factor", y = NULL, caption = paste0("SEMANTICA | rotation: ", result$model_info$rotation))
}

# =================================================================
# PLOT 13 -- SAMPLE-FREE PFA LOADING MAP
# =================================================================
#' Plot sample-free PFA loading diagnostics
#'
#' PFA factor axes are sign-indeterminate. SEMANTICA sign-anchors recovered
#' PFA dimensions so intended-factor primary loadings have a positive mean,
#' while preserving negative cross-loadings when they are present.
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object or `NULL`.
#' @export
#' @examples
#' \dontrun{
#' plot_pfa_diagnostics(result)
#' }
plot_pfa_diagnostics <- function(result) {
  pfa <- result$pfa_diagnostics
  if (is.null(pfa) || !isTRUE(pfa$available) || is.null(pfa$loadings)) {
    message("[plot_pfa_diagnostics] No available PFA diagnostics.")
    return(invisible(NULL))
  }
  loadings <- as.matrix(pfa$loadings)
  fa <- result$factor_assignment
  if (is.null(fa) || length(fa) == 0L) {
    message("[plot_pfa_diagnostics] Factor assignment missing.")
    return(invisible(NULL))
  }
  item_order <- unlist(lapply(unique(fa), function(f) names(fa[fa == f])), use.names = FALSE)
  item_order <- intersect(item_order, rownames(loadings))
  if (length(item_order) == 0L) {
    message("[plot_pfa_diagnostics] No matching items in PFA loadings.")
    return(invisible(NULL))
  }
  loadings <- loadings[item_order, , drop = FALSE]
  df_load <- as.data.frame(as.table(loadings), stringsAsFactors = FALSE)
  names(df_load) <- c("item", "pfa_factor", "loading")
  df_load$item <- as.character(df_load$item)
  df_load$pfa_factor <- as.character(df_load$pfa_factor)
  df_load$intended_factor <- unname(fa[df_load$item])
  df_load$abs_loading <- abs(df_load$loading)

  pfa_cols <- colnames(loadings)
  mapped <- rep(NA_character_, length(pfa_cols))
  names(mapped) <- pfa_cols
  mapping <- pfa$factor_mapping
  if (!is.null(mapping) && length(mapping) > 0L) {
    for (f in names(mapping)) {
      col_idx <- suppressWarnings(as.integer(mapping[[f]]))
      if (is.finite(col_idx) && col_idx >= 1L && col_idx <= length(pfa_cols)) {
        mapped[[col_idx]] <- f
      }
    }
  }
  df_load$mapped_factor <- unname(mapped[df_load$pfa_factor])
  df_load$is_target <- !is.na(df_load$mapped_factor) & df_load$intended_factor == df_load$mapped_factor
  df_load$label <- sprintf("%.2f", df_load$loading)
  df_load$item <- factor(df_load$item, levels = rev(item_order))
  df_load$pfa_factor <- factor(df_load$pfa_factor, levels = pfa_cols)
  sign_note <- if (!is.null(pfa$sign_orientation) && nzchar(as.character(pfa$sign_orientation[1L]))) {
    " . axes sign-anchored to intended factors"
  } else {
    ""
  }

  ggplot2::ggplot(df_load, ggplot2::aes(x = pfa_factor, y = item)) +
    ggplot2::geom_tile(ggplot2::aes(fill = loading), colour = "white", linewidth = 0.45) +
    ggplot2::geom_tile(data = df_load[df_load$is_target, , drop = FALSE],
                       fill = NA, colour = "#222222", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.4, colour = "#222222") +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                                  midpoint = 0, limits = c(-1, 1), name = "PFA loading") +
    ggplot2::facet_grid(rows = ggplot2::vars(intended_factor), scales = "free_y", space = "free_y") +
    .sem_theme(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(face = "bold", angle = 25, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0)
    ) +
    ggplot2::labs(
      title = "Sample-Free PFA Loading Map",
      subtitle = sprintf("Score %.3f | recovery %.3f | salience %.3f | clarity %.3f",
                         pfa$score, pfa$recovery_score, pfa$salience_score, pfa$clarity_score),
      x = "Extracted PFA dimension",
      y = NULL,
      caption = paste0("SEMANTICA | cosine-derived semantic correlation proxy | ",
                       pfa$extraction, " extraction, ", pfa$rotation, " rotation",
                       sign_note)
    )
}

# =================================================================
# PLOT 14 -- SEMANTIC PROXY REFERENCE-N SENSITIVITY
# =================================================================
#' Plot semantic proxy reference-N sensitivity
#'
#' The fitted N values are RMSEA-power semantic-proxy fit anchors. They are not
#' observed respondent sample sizes and do not replace the validation-N
#' diagnostic for response-data studies.
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @return A `ggplot` object or `NULL`.
#' @export
#' @examples
#' \dontrun{
#' plot_semantic_n_sensitivity(result)
#' }
plot_semantic_n_sensitivity <- function(result) {
  sns <- result$semantic_n_sensitivity
  grid <- sns$grid_results
  if (is.null(sns) || !isTRUE(sns$available) || is.null(grid) || nrow(grid) == 0L) {
    message("[plot_semantic_n_sensitivity] No semantic proxy reference-N refits available.")
    return(invisible(NULL))
  }

  grid <- as.data.frame(grid, stringsAsFactors = FALSE)
  required <- c("n_obs", "cfi", "rmsea", "srmr", "score")
  if (!all(required %in% names(grid))) {
    message("[plot_semantic_n_sensitivity] Reference-N grid is missing expected fit columns.")
    return(invisible(NULL))
  }
  grid <- grid[is.finite(grid$n_obs), , drop = FALSE]
  if (nrow(grid) == 0L) {
    message("[plot_semantic_n_sensitivity] Reference-N grid has no finite anchors.")
    return(invisible(NULL))
  }

  structural_cols <- intersect(
    c("correct_dominance", "median_primary_loading", "mean_abs_residual"),
    names(grid)
  )
  metric_cols <- c("cfi", "rmsea", "srmr", "score", structural_cols)
  labels <- c(
    cfi = "CFI",
    rmsea = "RMSEA",
    srmr = "SRMR",
    score = "Semantic score",
    correct_dominance = "Dominant-factor recovery",
    median_primary_loading = "Median primary loading",
    mean_abs_residual = "Mean |residual|"
  )

  rows <- lapply(metric_cols, function(metric_name) {
    vals <- suppressWarnings(as.numeric(grid[[metric_name]]))
    keep <- is.finite(vals)
    if (!any(keep)) return(NULL)
    data.frame(
      n_obs = as.numeric(grid$n_obs[keep]),
      metric = unname(labels[[metric_name]] %||% metric_name),
      metric_value = vals[keep],
      anchor_kind = if ("is_reference_n" %in% names(grid)) {
        ifelse(as.logical(grid$is_reference_n[keep]), "RMSEA-power anchor", "Sensitivity anchor")
      } else {
        "Sensitivity anchor"
      },
      stringsAsFactors = FALSE
    )
  })
  plot_df <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(plot_df) || nrow(plot_df) == 0L) {
    message("[plot_semantic_n_sensitivity] Reference-N grid has no plottable metrics.")
    return(invisible(NULL))
  }
  plot_df$metric <- factor(plot_df$metric, levels = unname(labels[metric_cols]))
  plot_df$anchor_kind <- factor(plot_df$anchor_kind, levels = c("Sensitivity anchor", "RMSEA-power anchor"))

  summary <- sns$summary
  subtitle <- if (!is.null(summary) && is.finite(summary$successful_fits) &&
                  is.finite(summary$requested_fits)) {
    sprintf("Selected semantic-proxy ESEM refits: %d/%d successful | structure stable: %s",
            summary$successful_fits, summary$requested_fits,
            ifelse(isTRUE(summary$structurally_stable), "yes", "no"))
  } else {
    "Selected semantic-proxy ESEM refits across nearby RMSEA-power N anchors"
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = n_obs, y = metric_value)) +
    ggplot2::geom_line(colour = "#2166AC", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = anchor_kind, fill = anchor_kind),
                        colour = "#1A1A1A", size = 2.4, stroke = 0.45) +
    ggplot2::scale_shape_manual(values = c("Sensitivity anchor" = 21, "RMSEA-power anchor" = 23)) +
    ggplot2::scale_fill_manual(values = c("Sensitivity anchor" = "#67A9CF", "RMSEA-power anchor" = "#D6604D")) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 3) +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
    .sem_theme(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      strip.background = ggplot2::element_rect(fill = "#F2F2F2", colour = NA)
    ) +
    ggplot2::labs(
      title = "Semantic Proxy Reference-N Sensitivity",
      subtitle = subtitle,
      x = "Semantic proxy reference N",
      y = NULL,
      shape = NULL,
      fill = NULL,
      caption = "SEMANTICA | fit-anchor sensitivity for embedding-derived ESEM; not a respondent sample-size recommendation"
    )
}

# =================================================================
# PLOT 15 -- SUMMARY OF RESULTS
# =================================================================
#' SEMANTICA result dashboard
#'
#' Summarizes the most important SEMANTICA outputs in one compact dashboard:
#' objective scores, ESEM fit checks, structure diagnostics, PFA recovery, and
#' semantic similarity before/after selection when those values are available.
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Optional full cosine similarity matrix. Used only as
#'   a fallback when stored semantic-reduction diagnostics are unavailable.
#' @return A patchwork/`ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_summary_of_results(result, cosine_sim_matrix)
#' }
plot_summary_of_results <- function(result, cosine_sim_matrix = NULL) {
  result <- .viz_core_result(result)
  if (is.null(result) || is.null(result$factor_assignment)) {
    message("[plot_summary_of_results] Missing SEMANTICA result object.")
    return(invisible(NULL))
  }

  pick_num <- function(...) {
    vals <- list(...)
    for (val in vals) {
      out <- .viz_safe_num(val)
      if (is.finite(out)) return(out)
    }
    NA_real_
  }
  safe01_pick <- function(...) {
    val <- pick_num(...)
    if (!is.finite(val)) NA_real_ else max(0, min(1, val))
  }
  blank_panel <- function(title, subtitle = "Diagnostic unavailable for this result object.") {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = subtitle,
                        size = 3.4, colour = "#777777", hjust = 0.5) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      .sem_theme(base_size = 10) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(title = title)
  }

  fa <- result$factor_assignment
  factors <- unique(as.character(fa))
  n_items <- length(result$best_items %||% names(fa))
  n_factors <- length(factors)
  decomp <- result$esem_result$score_decomp %||% list()
  sdg <- result$structure_diagnostics %||% result$esem_result$structure_diagnostics %||% list()
  pfa <- result$pfa_diagnostics %||% list()

  score_df <- data.frame(
    component = c("Optimization utility*", "Proposal utility*", "ESEM structure score",
                  "Semantic objective", "PFA reported", "Loading quality"),
    score = c(
      safe01_pick(result$final_guided_objective_score, result$best_objective),
      safe01_pick(result$proposal_objective_score, result$search_objective_score),
      safe01_pick(result$esem_result$score, decomp$final_score),
      safe01_pick(result$semantic_objective_score, result$semantic_score),
      safe01_pick(result$pfa_score, pfa$score),
      safe01_pick(result$loading_quality, decomp$loading_quality)
    ),
    stringsAsFactors = FALSE
  )
  score_df <- score_df[is.finite(score_df$score), , drop = FALSE]
  score_df$component <- factor(score_df$component, levels = rev(score_df$component))
  score_df$score_label <- sprintf("%.2f", score_df$score)

  p_scores <- if (nrow(score_df) == 0L) {
    blank_panel("The Scoreboard")
  } else {
    ggplot2::ggplot(score_df, ggplot2::aes(x = score, y = component, fill = score)) +
      ggplot2::geom_col(width = 0.62, alpha = 0.95) +
      ggplot2::geom_text(ggplot2::aes(label = score_label, x = pmin(score + 0.045, 1.04)),
                         hjust = 0, size = 3.2, fontface = "bold", colour = "#222222") +
      ggplot2::scale_fill_gradient2(low = "#BF616A", mid = "#EBCB8B", high = "#2E8B57",
                                    midpoint = 0.65, limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = c(0, 0.5, 1),
                                  labels = scales::number_format(accuracy = 0.1)) +
      .sem_theme(base_size = 10) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(
        title = "The Scoreboard",
        subtitle = "*Optimization utilities are within-run and evidence-regime dependent; companions are descriptive proxies",
        x = "Displayed value (0-1)"
      )
  }

  cr <- result$esem_result %||% list()
  ac <- result$active_cutoffs %||% list()
  fit_df <- data.frame(
    component = c("CFI", "TLI", "RMSEA", "SRMR", "AVE", "HTMT"),
    observed = c(
      pick_num(cr$cfi),
      pick_num(cr$tli),
      pick_num(cr$rmsea),
      pick_num(cr$srmr),
      pick_num(cr$ave),
      pick_num(result$htmt_max)
    ),
    cutoff = c(
      pick_num(ac$cfi),
      pick_num(ac$tli),
      pick_num(ac$rmsea),
      pick_num(ac$srmr),
      if (!is.null(result$response_validation)) 0.50 else NA_real_,
      pick_num(result$model_info$htmt_threshold, 0.85)
    ),
    direction = c("high", "high", "low", "low", "high", "low"),
    stringsAsFactors = FALSE
  )
  fit_df$observed <- suppressWarnings(as.numeric(fit_df$observed))
  fit_df$cutoff <- suppressWarnings(as.numeric(fit_df$cutoff))
  fit_df <- fit_df[is.finite(fit_df$observed), , drop = FALSE]
  fit_df$pass <- ifelse(
    is.finite(fit_df$cutoff),
    ifelse(fit_df$direction == "high", fit_df$observed >= fit_df$cutoff, fit_df$observed <= fit_df$cutoff),
    NA
  )
  fit_df$status <- ifelse(is.na(fit_df$pass), "descriptive", ifelse(fit_df$pass, "reference_met", "reference_not_met"))
  fit_df$display <- ifelse(
    is.finite(fit_df$cutoff),
    paste0(fit_df$component, "\n", sprintf("%.3f", fit_df$observed), "\ncut ", sprintf("%.3f", fit_df$cutoff)),
    paste0(fit_df$component, "\n", sprintf("%.3f", fit_df$observed), "\ndesc.")
  )
  fit_df$component <- factor(fit_df$component, levels = c("CFI", "TLI", "RMSEA", "SRMR", "AVE", "HTMT"))

  p_fit <- if (nrow(fit_df) == 0L) {
    blank_panel("Fit Check Tiles")
  } else {
    ggplot2::ggplot(fit_df, ggplot2::aes(x = component, y = 1, fill = status)) +
      ggplot2::geom_tile(width = 0.94, height = 0.82, colour = "white", linewidth = 1) +
      ggplot2::geom_text(ggplot2::aes(label = display), size = 2.8, fontface = "bold",
                         colour = "#1F2933", lineheight = 0.9) +
      ggplot2::scale_fill_manual(values = c(reference_met = "#A7D7C5", reference_not_met = "#F2A7A0", descriptive = "#BFD7EA"),
                                 breaks = c("reference_met", "reference_not_met", "descriptive"),
                                 labels = c("Reference met", "Reference not met", "Descriptive"),
                                 name = NULL) +
      ggplot2::coord_cartesian(ylim = c(0.45, 1.55), clip = "off") +
      .sem_theme(base_size = 10) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom") +
      ggplot2::labs(title = "Semantic-Proxy Reference Tiles",
                    subtitle = result$cutoff_source %||% "Observed fit against available cutoffs")
  }

  structure_df <- data.frame(
    component = c("Mean primary", "Dominance", "Simple structure",
                  "Cross-loading control", "PFA recovery", "PFA clarity"),
    score = c(
      safe01_pick(sdg$mean_primary_loading),
      safe01_pick(sdg$correct_dominance),
      safe01_pick(sdg$simple_structure),
      safe01_pick(sdg$no_large_cross_loading),
      safe01_pick(pfa$recovery_score),
      safe01_pick(pfa$clarity_score)
    ),
    metric_group = c("ESEM", "ESEM", "ESEM", "ESEM", "PFA", "PFA"),
    stringsAsFactors = FALSE
  )
  structure_df <- structure_df[is.finite(structure_df$score), , drop = FALSE]
  structure_df$component <- factor(structure_df$component, levels = rev(structure_df$component))
  structure_cols <- c(ESEM = "#5E81AC", PFA = "#2E8B57")

  p_structure <- if (nrow(structure_df) == 0L) {
    blank_panel("Structure Snapshot")
  } else {
    ggplot2::ggplot(structure_df, ggplot2::aes(y = component, x = score, colour = metric_group)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = score, yend = component),
                            linewidth = 1.6, alpha = 0.45) +
      ggplot2::geom_point(size = 4, alpha = 0.95) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", score), x = pmin(score + 0.055, 1.04)),
                         hjust = 0, size = 3, fontface = "bold", colour = "#333333") +
      ggplot2::geom_vline(xintercept = 0.70, linetype = "dashed", colour = "#999999", linewidth = 0.55) +
      ggplot2::scale_colour_manual(values = structure_cols, name = NULL) +
      ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = c(0, 0.5, 0.7, 1)) +
      .sem_theme(base_size = 10) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     legend.position = "bottom") +
      ggplot2::labs(title = "Structure Snapshot", subtitle = "ESEM loading recovery plus sample-free PFA clarity", x = "Diagnostic score")
  }

  sem_red <- result$semantic_similarity_reduction %||% list()
  sem_rows <- data.frame(
    sim_type = c("Within-factor", "Within-factor", "Between-factor", "Between-factor"),
    before_after = c("Before", "After", "Before", "After"),
    score = c(
      pick_num(sem_red$within_factor_before),
      pick_num(sem_red$within_factor_after, result$mean_within),
      pick_num(sem_red$between_factor_before),
      pick_num(sem_red$between_factor_after, result$mean_between)
    ),
    stringsAsFactors = FALSE
  )
  sem_rows$score <- suppressWarnings(as.numeric(sem_rows$score))
  sem_rows <- sem_rows[is.finite(sem_rows$score), , drop = FALSE]
  if (nrow(sem_rows) == 0L && !is.null(cosine_sim_matrix) && !is.null(result$best_items)) {
    sem_rows <- tryCatch({
      fa2 <- result$factor_assignment
      items <- intersect(result$best_items, rownames(cosine_sim_matrix))
      within_vals <- unlist(lapply(unique(fa2), function(f) {
        fi <- intersect(names(fa2[fa2 == f]), items)
        if (length(fi) < 2L) return(numeric(0L))
        cosine_sim_matrix[fi, fi, drop = FALSE][lower.tri(cosine_sim_matrix[fi, fi, drop = FALSE])]
      }), use.names = FALSE)
      between_vals <- unlist(lapply(seq_len(max(0L, length(unique(fa2)) - 1L)), function(i) {
        facs <- unique(fa2)
        if (i >= length(facs)) return(numeric(0L))
        unlist(lapply((i + 1L):length(facs), function(j) {
          ai <- intersect(names(fa2[fa2 == facs[i]]), items)
          bi <- intersect(names(fa2[fa2 == facs[j]]), items)
          if (length(ai) == 0L || length(bi) == 0L) return(numeric(0L))
          as.vector(cosine_sim_matrix[ai, bi, drop = FALSE])
        }), use.names = FALSE)
      }), use.names = FALSE)
      data.frame(
        sim_type = c("Within-factor", "Between-factor"),
        before_after = "After",
        score = c(mean(within_vals, na.rm = TRUE), mean(between_vals, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
    }, error = function(e) sem_rows)
    sem_rows <- sem_rows[is.finite(sem_rows$score), , drop = FALSE]
  }
  sem_rows$sim_type <- factor(sem_rows$sim_type, levels = c("Within-factor", "Between-factor"))
  sem_rows$before_after <- factor(sem_rows$before_after, levels = c("Before", "After"))
  reduction_label <- if (is.finite(.viz_safe_num(sem_red$percent_reduction))) {
    sprintf("within-factor reduction %.1f%%", sem_red$percent_reduction)
  } else if (is.finite(.viz_safe_num(sem_red$absolute_reduction))) {
    sprintf("within-factor change %.3f", sem_red$absolute_reduction)
  } else {
    "selected-scale semantic separation"
  }

  p_semantic <- if (nrow(sem_rows) == 0L) {
    blank_panel("Semantic Before/After")
  } else {
    sem_y_min <- min(0, min(sem_rows$score, na.rm = TRUE) * 1.15)
    sem_y_max <- max(0.05, max(sem_rows$score, na.rm = TRUE) * 1.20)
    if (!is.finite(sem_y_min)) sem_y_min <- 0
    if (!is.finite(sem_y_max) || sem_y_max <= sem_y_min) sem_y_max <- sem_y_min + 0.10
    ggplot2::ggplot(sem_rows, ggplot2::aes(x = sim_type, y = score, fill = before_after)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.68), width = 0.58, alpha = 0.92) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", score)),
                         position = ggplot2::position_dodge(width = 0.68),
                         vjust = -0.35, size = 2.9, fontface = "bold") +
      ggplot2::scale_fill_manual(values = c(Before = "#D8DEE9", After = "#5E81AC"), name = NULL) +
      ggplot2::scale_y_continuous(limits = c(sem_y_min, sem_y_max),
                                  labels = scales::number_format(accuracy = 0.01)) +
      .sem_theme(base_size = 10) +
      ggplot2::theme(legend.position = "bottom",
                     axis.title.x = ggplot2::element_blank()) +
      ggplot2::labs(title = "Semantic Before/After", subtitle = reduction_label, y = "Mean cosine similarity")
  }

  warnings_obj <- result$summary$warnings %||% character(0L)
  warning_count <- if (length(warnings_obj) == 0L || identical(warnings_obj, "none")) 0L else length(warnings_obj)
  pfa_txt <- if (isTRUE(pfa$available)) sprintf("PFA %.2f", .viz_safe01(pfa$score, 0)) else "PFA unavailable"
  subtitle <- sprintf(
    "%d selected items across %d factors | optimization utility %s | proposal utility %s | %s | warnings %d",
    n_items, n_factors,
    .viz_display_num(pick_num(result$final_guided_objective_score, result$best_objective), 3),
    .viz_display_num(pick_num(result$proposal_objective_score, result$search_objective_score), 3),
    pfa_txt,
    warning_count
  )

  patchwork::wrap_plots(p_scores, p_fit, p_structure, p_semantic, ncol = 2) +
    patchwork::plot_annotation(
      title = "15 | Summary of SEMANTICA Results",
      subtitle = subtitle,
      caption = paste0(
        "SEMANTICA | optimization utilities are not universal scale-quality scores | search guidance: ",
        result$search_guidance_status %||% "legacy/unknown",
        " | objective regime: ", result$objective_context$evidence_regime %||% "legacy/unknown"
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 16, colour = "#1F2933"),
        plot.subtitle = ggplot2::element_text(size = 10, colour = "#4B5563"),
        plot.caption = ggplot2::element_text(size = 8, colour = "#777777", hjust = 0)
      )
    )
}

# =================================================================
# INTERNAL PLOT -- SEMANTIC DISTILLATION MAP
# =================================================================
#' Internal semantic distillation map
#'
#' Creates an internal opt-in semantic distillation map that visualizes how a
#' large semantic item pool is narrowed to a selected factor constellation.
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix used by SEMANTICA.
#' @param max_background_items Maximum number of non-selected candidate items
#'   to display. The most semantically relevant candidates are retained first.
#' @param edge_quantile Quantile used to show only the strongest remaining
#'   selected-factor semantic overlaps.
#' @return A patchwork/`ggplot` object, or `NULL` invisibly if required inputs
#'   are unavailable.
#' @noRd
the_coolest_plot_ever <- function(result, cosine_sim_matrix = NULL,
                                  max_background_items = 260L,
                                  edge_quantile = 0.65) {
  result <- .viz_core_result(result)
  if (is.null(result) || is.null(result$factor_assignment)) {
    message("[the_coolest_plot_ever] Missing SEMANTICA result object.")
    return(invisible(NULL))
  }
  if (is.null(cosine_sim_matrix)) {
    message("[the_coolest_plot_ever] A cosine similarity matrix is required.")
    return(invisible(NULL))
  }

  pick_num <- function(...) {
    vals <- list(...)
    for (val in vals) {
      out <- .viz_safe_num(val)
      if (is.finite(out)) return(out)
    }
    NA_real_
  }
  safe01 <- function(x) {
    val <- .viz_safe_num(x)
    if (!is.finite(val)) NA_real_ else max(0, min(1, val))
  }
  rescale_safe <- function(x, to = c(0, 1), default = mean(to)) {
    out <- rep(default, length(x))
    ok <- is.finite(x)
    if (sum(ok) > 1L && diff(range(x[ok])) > .Machine$double.eps) {
      out[ok] <- scales::rescale(x[ok], to = to, from = range(x[ok]))
    } else if (any(ok)) {
      out[ok] <- default
    }
    out
  }
  short_label <- function(x, width = 13L) {
    vapply(as.character(x), function(z) paste(strwrap(gsub("_", " ", z), width = width), collapse = "\n"), character(1L))
  }

  cos_mat <- tryCatch(as.matrix(cosine_sim_matrix), error = function(e) NULL)
  if (is.null(cos_mat) || nrow(cos_mat) < 3L || ncol(cos_mat) < 3L) {
    message("[the_coolest_plot_ever] Cosine matrix is too small to map.")
    return(invisible(NULL))
  }
  fa <- result$factor_assignment
  best_items <- as.character(result$best_items %||% names(fa))
  item_names <- rownames(cos_mat) %||% colnames(cos_mat)
  if (is.null(item_names) || length(item_names) != nrow(cos_mat)) {
    if (length(names(fa)) == nrow(cos_mat)) {
      item_names <- names(fa)
    } else {
      message("[the_coolest_plot_ever] Cosine matrix needs item row/column names.")
      return(invisible(NULL))
    }
  }
  rownames(cos_mat) <- item_names
  colnames(cos_mat) <- colnames(cos_mat) %||% item_names
  if (!identical(rownames(cos_mat), colnames(cos_mat))) {
    shared <- intersect(rownames(cos_mat), colnames(cos_mat))
    cos_mat <- cos_mat[shared, shared, drop = FALSE]
  }
  cos_mat <- .viz_transform_cosine_for_display(cos_mat)

  eligible <- result$eligible_items
  if (is.null(eligible)) {
    eligible <- split(names(fa), as.character(fa))
  }
  pool_candidates <- unique(c(unlist(eligible, use.names = FALSE), names(fa), best_items))
  pool_items <- intersect(pool_candidates, rownames(cos_mat))
  selected_items <- intersect(best_items, pool_items)
  if (length(pool_items) < 3L || length(selected_items) < 2L) {
    message("[the_coolest_plot_ever] Not enough named pool and selected items to map.")
    return(invisible(NULL))
  }

  fa_pool <- stats::setNames(rep("Unassigned", length(pool_items)), pool_items)
  for (f in names(eligible)) {
    hit <- intersect(as.character(eligible[[f]]), pool_items)
    fa_pool[hit] <- f
  }
  named_fa <- intersect(names(fa), pool_items)
  fa_pool[named_fa] <- as.character(fa[named_fa])
  fa_pool[is.na(fa_pool) | !nzchar(fa_pool)] <- "Unassigned"

  factors <- unique(as.character(fa_pool[selected_items]))
  factors <- factors[!is.na(factors) & nzchar(factors) & factors != "Unassigned"]
  if (length(factors) == 0L) factors <- unique(as.character(fa_pool[selected_items]))
  fac_cols <- stats::setNames(.factor_colours(length(factors)), factors)
  fac_cols["Unassigned"] <- "#B8BDC9"

  max_background_items <- suppressWarnings(as.integer(max_background_items[1L]))
  if (!is.finite(max_background_items) || max_background_items < length(selected_items)) {
    max_background_items <- length(selected_items) + 60L
  }
  non_selected <- setdiff(pool_items, selected_items)
  keep_non <- non_selected
  bg_limit <- max(0L, max_background_items - length(selected_items))
  if (length(non_selected) > bg_limit) {
    proximity <- tryCatch({
      apply(cos_mat[non_selected, selected_items, drop = FALSE], 1L, max, na.rm = TRUE)
    }, error = function(e) rep(0, length(non_selected)))
    proximity[!is.finite(proximity)] <- 0
    keep_non <- names(sort(proximity, decreasing = TRUE))[seq_len(bg_limit)]
  }
  plot_items <- unique(c(selected_items, keep_non))
  plot_items <- intersect(plot_items, rownames(cos_mat))

  cos_sub <- cos_mat[plot_items, plot_items, drop = FALSE]
  dist_sub <- 2 * (1 - cos_sub)
  dist_sub[!is.finite(dist_sub)] <- NA_real_
  dist_sub[dist_sub < 0] <- 0
  dist_sub <- sqrt(dist_sub)
  dist_sub[!is.finite(dist_sub)] <- max(dist_sub[is.finite(dist_sub)], 1)
  diag(dist_sub) <- 0
  mds <- tryCatch(stats::cmdscale(stats::as.dist(dist_sub), k = 2), error = function(e) NULL)
  if (is.null(mds) || ncol(as.matrix(mds)) < 2L) {
    message("[the_coolest_plot_ever] Could not compute semantic map coordinates.")
    return(invisible(NULL))
  }
  coords <- as.data.frame(as.matrix(mds)[, 1:2, drop = FALSE])
  names(coords) <- c("x", "y")
  coords$item <- rownames(as.matrix(mds))
  coords$x <- coords$x - mean(coords$x, na.rm = TRUE)
  coords$y <- coords$y - mean(coords$y, na.rm = TRUE)
  scale_xy <- max(abs(c(coords$x, coords$y)), na.rm = TRUE)
  if (is.finite(scale_xy) && scale_xy > 0) {
    coords$x <- coords$x / scale_xy
    coords$y <- coords$y / scale_xy
  }
  coords$factor <- fa_pool[coords$item]
  coords$selected <- coords$item %in% selected_items
  coords$item_status <- ifelse(coords$selected, "Selected scale", "Candidate pool")
  coords$draw_size <- ifelse(coords$selected, 3.8, 1.35)
  coords$draw_alpha <- ifelse(coords$selected, 0.98, 0.18)
  has_pool_background <- any(!coords$selected)
  pool_shown_n <- sum(!coords$selected)
  selected_shown_n <- sum(coords$selected)
  candidate_counts <- result$candidate_counts
  has_candidate_counts <- is.data.frame(candidate_counts) &&
    all(c("factor", "generated", "eligible", "selected_target") %in% names(candidate_counts))
  generated_n <- if (has_candidate_counts) sum(candidate_counts$generated, na.rm = TRUE) else NA_real_
  eligible_n <- if (has_candidate_counts) sum(candidate_counts$eligible, na.rm = TRUE) else length(pool_items)
  eligible_by_factor <- if (has_candidate_counts) {
    stats::setNames(candidate_counts$eligible, candidate_counts$factor)
  } else {
    vapply(factors, function(f) length(intersect(eligible[[f]] %||% character(0L), pool_items)), integer(1L))
  }

  centroid_rows <- lapply(factors, function(f) {
    pool_f <- coords[coords$factor == f, , drop = FALSE]
    selected_f <- coords[coords$factor == f & coords$selected, , drop = FALSE]
    if (nrow(pool_f) == 0L || nrow(selected_f) == 0L) return(NULL)
    data.frame(
      factor = f,
      pool_x = mean(pool_f$x, na.rm = TRUE),
      pool_y = mean(pool_f$y, na.rm = TRUE),
      selected_x = mean(selected_f$x, na.rm = TRUE),
      selected_y = mean(selected_f$y, na.rm = TRUE),
      n_pool = nrow(pool_f),
      n_eligible = eligible_by_factor[[f]] %||% nrow(pool_f),
      n_selected = nrow(selected_f),
      spread = mean(sqrt((selected_f$x - mean(selected_f$x, na.rm = TRUE))^2 +
                           (selected_f$y - mean(selected_f$y, na.rm = TRUE))^2), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  centroids <- do.call(rbind, centroid_rows[!vapply(centroid_rows, is.null, logical(1L))])
  if (is.null(centroids) || nrow(centroids) == 0L) {
    message("[the_coolest_plot_ever] Could not compute factor centroids.")
    return(invisible(NULL))
  }
  centroids$spread[!is.finite(centroids$spread)] <- 0
  centroids$radius <- rescale_safe(centroids$spread, to = c(0.065, 0.17), default = 0.10)
  centroids$label <- sprintf("%s\n%d/%d eligible", short_label(centroids$factor), centroids$n_selected, centroids$n_eligible)

  theta <- seq(0, 2 * pi, length.out = 120L)
  ring_df <- do.call(rbind, lapply(seq_len(nrow(centroids)), function(i) {
    data.frame(
      factor = centroids$factor[i],
      ring_x = centroids$selected_x[i] + centroids$radius[i] * cos(theta),
      ring_y = centroids$selected_y[i] + centroids$radius[i] * sin(theta),
      stringsAsFactors = FALSE
    )
  }))

  edge_rows <- list()
  if (length(factors) > 1L) {
    pairs <- utils::combn(factors, 2, simplify = FALSE)
    edge_rows <- lapply(pairs, function(pair) {
      a_items <- intersect(selected_items[fa_pool[selected_items] == pair[1]], rownames(cos_mat))
      b_items <- intersect(selected_items[fa_pool[selected_items] == pair[2]], rownames(cos_mat))
      ca <- centroids[centroids$factor == pair[1], , drop = FALSE]
      cb <- centroids[centroids$factor == pair[2], , drop = FALSE]
      if (length(a_items) == 0L || length(b_items) == 0L || nrow(ca) == 0L || nrow(cb) == 0L) return(NULL)
      sim <- mean(as.vector(cos_mat[a_items, b_items, drop = FALSE]), na.rm = TRUE)
      data.frame(
        x = ca$selected_x, y = ca$selected_y,
        xend = cb$selected_x, yend = cb$selected_y,
        semantic_overlap = sim,
        stringsAsFactors = FALSE
      )
    })
  }
  edge_df <- do.call(rbind, edge_rows[!vapply(edge_rows, is.null, logical(1L))])
  if (!is.null(edge_df) && nrow(edge_df) > 0L) {
    edge_df <- edge_df[is.finite(edge_df$semantic_overlap), , drop = FALSE]
    if (nrow(edge_df) > 0L) {
      edge_quantile <- max(0, min(1, .viz_safe_num(edge_quantile, 0.65)))
      keep_cut <- stats::quantile(edge_df$semantic_overlap, probs = edge_quantile, na.rm = TRUE, names = FALSE)
      edge_df <- edge_df[edge_df$semantic_overlap >= keep_cut, , drop = FALSE]
      edge_df$edge_width <- rescale_safe(edge_df$semantic_overlap, to = c(0.45, 2.8), default = 1.3)
      edge_df$edge_alpha <- rescale_safe(edge_df$semantic_overlap, to = c(0.28, 0.72), default = 0.45)
    }
  }

  sem_red <- result$semantic_similarity_reduction %||% list()
  within_txt <- if (is.finite(.viz_safe_num(sem_red$percent_reduction))) {
    sprintf("%.1f%% within-factor redundancy reduction", .viz_safe_num(sem_red$percent_reduction))
  } else if (is.finite(.viz_safe_num(sem_red$absolute_reduction))) {
    sprintf("%.3f within-factor similarity change", .viz_safe_num(sem_red$absolute_reduction))
  } else {
    "semantic redundancy reduction unavailable"
  }
  final_objective_txt <- .viz_display_num(pick_num(result$final_guided_objective_score, result$best_objective), 3)
  proposal_objective_txt <- .viz_display_num(pick_num(result$proposal_objective_score, result$search_objective_score), 3)
  map_subtitle <- if (has_pool_background) {
    sprintf("%s%d eligible; %d selected across %d factors (%d non-selected eligible shown) | utility %s; proposal utility %s | %s",
            if (is.finite(generated_n)) sprintf("%d generated; ", generated_n) else "",
            as.integer(eligible_n), selected_shown_n, length(factors), pool_shown_n,
            final_objective_txt, proposal_objective_txt, within_txt)
  } else {
    sprintf("selected-only view: %d selected across %d factors | utility %s; proposal utility %s | %s",
            selected_shown_n, length(factors), final_objective_txt, proposal_objective_txt, within_txt)
  }
  map_caption <- if (has_pool_background) {
    "Faint dots = non-selected eligible items | bright dots = selected scale | labels = selected/eligible count | X marks = displayed eligible-pool center | arrows = selection shift | rings = selected semantic breadth | purple curves = strongest remaining between-factor overlap"
  } else {
    "Bright dots = selected scale | rings = selected semantic breadth | purple curves = strongest remaining between-factor overlap | no non-selected candidate pool was available in the supplied matrix/result"
  }

  p_map <- ggplot2::ggplot() +
    {if (!is.null(edge_df) && nrow(edge_df) > 0L) ggplot2::geom_curve(
      data = edge_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                   linewidth = edge_width, alpha = edge_alpha),
      colour = "#7C3AED", curvature = 0.18, lineend = "round",
      inherit.aes = FALSE
    )} +
    ggplot2::geom_path(
      data = ring_df,
      ggplot2::aes(x = ring_x, y = ring_y, colour = factor, group = factor),
      linewidth = 1.05, alpha = 0.55, inherit.aes = FALSE
    ) +
    {if (has_pool_background) ggplot2::geom_segment(
      data = centroids,
      ggplot2::aes(x = pool_x, y = pool_y, xend = selected_x, yend = selected_y, colour = factor),
      linewidth = 1.05, alpha = 0.68,
      arrow = grid::arrow(length = grid::unit(0.13, "inches"), type = "closed"),
      inherit.aes = FALSE
    )} +
    {if (has_pool_background) ggplot2::geom_point(
      data = coords[!coords$selected, , drop = FALSE],
      ggplot2::aes(x = x, y = y),
      colour = "#7B8794", size = 1.2, alpha = 0.18, inherit.aes = FALSE
    )} +
    {if (has_pool_background) ggplot2::geom_point(
      data = centroids,
      ggplot2::aes(x = pool_x, y = pool_y, colour = factor),
      shape = 4, stroke = 1.2, size = 3.5, alpha = 0.72, inherit.aes = FALSE
    )} +
    ggplot2::geom_point(
      data = coords[coords$selected, , drop = FALSE],
      ggplot2::aes(x = x, y = y, fill = factor),
      shape = 21, colour = "#111827", stroke = 0.45, size = 4.0, alpha = 0.98,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = centroids,
      ggplot2::aes(x = selected_x, y = selected_y, label = label, fill = factor),
      colour = "white", size = 2.7, fontface = "bold", linewidth = 0,
      alpha = 0.92, lineheight = 0.88, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = fac_cols, guide = "none") +
    ggplot2::scale_fill_manual(values = fac_cols, guide = "none") +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(clip = "off") +
    .sem_theme(base_size = 10) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(colour = "#EEF2F7", linewidth = 0.3),
      plot.background = ggplot2::element_rect(fill = "#FAFBFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FAFBFF", colour = NA),
      plot.margin = ggplot2::margin(8, 12, 4, 12)
    ) +
    ggplot2::labs(
      title = "Semantic Distillation Map",
      subtitle = map_subtitle,
      caption = map_caption
    )

  cr <- result$esem_result %||% list()
  pfa <- result$pfa_diagnostics %||% list()
  sns <- result$semantic_n_sensitivity %||% list()
  # Keep the compact plot from inventing a binary stability score from the
  # retired 0.10 pair-perturbation heuristic. Reference-N structural stability
  # can still be shown when available; semantic resampling remains interval/range
  # evidence in the detailed diagnostics rather than being collapsed to 0-1.
  stable_score <- if (isTRUE(sns$summary$structurally_stable)) {
    1
  } else if (identical(sns$summary$structurally_stable, FALSE)) {
    0.35
  } else {
    NA_real_
  }
  retention_score <- if (is.finite(generated_n) && generated_n > 0 && is.finite(eligible_n)) {
    safe01(eligible_n / generated_n)
  } else {
    NA_real_
  }
  final_label <- if (identical(result$search_guidance_status, "esem_guided")) {
    "Optimization\nutility*"
  } else {
    "Optimization\nutility*"
  }
  phase_df <- data.frame(
    phase = c("Eligible pool\nretained", "Proposal\nutility*", "ESEM structure\nscore",
              final_label, "PFA reported\nscore", "Proxy-N\nstability"),
    score_norm = c(
      retention_score,
      safe01(pick_num(result$proposal_objective_score, result$search_objective_score)),
      safe01(cr$score),
      safe01(pick_num(result$final_guided_objective_score, result$best_objective)),
      safe01(pick_num(pfa$score, result$pfa_score)),
      safe01(stable_score)
    ),
    stringsAsFactors = FALSE
  )
  phase_df$phase_x <- seq_len(nrow(phase_df))
  phase_df$score_plot <- ifelse(is.finite(phase_df$score_norm), phase_df$score_norm, 0.18)
  phase_df$score_txt <- ifelse(is.finite(phase_df$score_norm), sprintf("%.2f", phase_df$score_norm), "NA")
  phase_df$status <- ifelse(!is.finite(phase_df$score_norm), "Missing",
                            ifelse(grepl("objective|utility", phase_df$phase, ignore.case = TRUE), "Optimization",
                            ifelse(phase_df$phase == "Eligible pool\nretained", "Context",
                                   ifelse(phase_df$score_norm >= 0.75, "High signal",
                                          ifelse(phase_df$score_norm >= 0.55, "Moderate", "Review")))))

  p_process <- ggplot2::ggplot(phase_df, ggplot2::aes(x = phase_x, y = 0)) +
    ggplot2::annotate("segment", x = 1, xend = nrow(phase_df), y = 0, yend = 0,
                      linewidth = 2.4, colour = "#D9E2EC") +
    ggplot2::geom_segment(data = phase_df[-nrow(phase_df), ],
                          ggplot2::aes(x = phase_x, xend = phase_x + 1, y = 0, yend = 0),
                          linewidth = 2.4, colour = "#9FB3C8", alpha = 0.45) +
    ggplot2::geom_point(ggplot2::aes(size = score_plot, fill = status),
                        shape = 21, colour = "#1F2933", stroke = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = phase), y = 0.36, size = 3.0,
                       fontface = "bold", lineheight = 0.9, colour = "#1F2933") +
    ggplot2::geom_text(ggplot2::aes(label = score_txt), y = -0.36, size = 3.1,
                       fontface = "bold", colour = "#344054") +
    ggplot2::scale_size_continuous(range = c(7, 17), limits = c(0, 1), guide = "none") +
    ggplot2::scale_fill_manual(values = c("High signal" = "#2E8B57", Moderate = "#E0A42B",
                                          Review = "#C84C4C", Context = "#5E81AC", Missing = "#9AA5B1"),
                               breaks = c("High signal", "Moderate", "Review", "Context", "Missing"),
                               name = NULL) +
    ggplot2::coord_cartesian(xlim = c(0.55, nrow(phase_df) + 0.45), ylim = c(-0.62, 0.62), clip = "off") +
    .sem_theme(base_size = 10) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(0, 14, 6, 14)
    ) +
    ggplot2::labs(title = "Process Signals",
                  subtitle = "Stored metrics shown separately; colours are descriptive visual triage, not validation cutoffs")

  patchwork::wrap_plots(p_map, p_process, ncol = 1, heights = c(4.5, 1.15)) +
    patchwork::plot_annotation(
      title = "Semantic Distillation Map",
      subtitle = "Internal visualization of SEMANTICA's semantic item-selection process",
      caption = paste0("SEMANTICA | presentation-oriented diagnostic | search guidance: ",
                       result$search_guidance_status %||% "legacy/unknown",
                       " | objective regime: ",
                       result$objective_context$evidence_regime %||% "legacy/unknown"),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 18, colour = "#111827"),
        plot.subtitle = ggplot2::element_text(size = 10.5, colour = "#4B5563"),
        plot.caption = ggplot2::element_text(size = 8, colour = "#777777", hjust = 0)
      )
    )
}

# =================================================================
# MASTER WRAPPER
# =================================================================
#' Generate all SEMANTICA diagnostic plots
#'
#' @param result A high-level SEMANTICA result or output from `ACO_with_ESEM()`.
#' @param cosine_sim_matrix Full cosine similarity matrix. Optional for a high-level result because the stored matrix is reused.
#' @param df Item metadata dataframe.
#' @param multi_result Output from `run_multi_seed_semantica()` (optional).
#' @param interactive_mode `"2d"` or `"3d"` for Plot 9.
#' @param before_path_model Passed to [plot_esem_path_diagrams()] for the
#'   BEFORE panel. Defaults to `"proxy"` so the diagnostic plot does not
#'   trigger a new full-pool ESEM fit.
#' @param before_path_refit_max_items Maximum BEFORE-pool size for an
#'   explicitly requested path-diagram ESEM refit.
#' @param network_max_items Maximum candidate-pool items rendered in Plot 3.
#' @param mds_max_items Maximum candidate-pool items included in Plot 9 MDS.
#' @param path_proxy_max_items Maximum candidate-pool items represented in the
#'   fast Plot 10 BEFORE proxy.
#' @param include_interactive Logical; generate the interactive Plot 9 widget.
#' @param progress Logical; print plot progress and completion information. `FALSE` is quiet.
#' @param which `"all"`, one or more plot names, or a diagnostic-family shortcut: `"aco"`, `"esem"`, `"semantic"`, `"pfa"`, or `"dfi"`. See Details.
#' @param save Logical; save to disk.
#' @param out_dir Output directory.
#' @param device Image format (`"png"`, `"pdf"`, etc.).
#' @param width,height,dpi Plot dimensions.
#' @section Side effects:
#' Constructs diagnostic plot objects and, when `save = TRUE`, writes plot files
#' to `out_dir`. An explicitly requested `before_path_model = "refit"` may run
#' an additional guarded full-pool ESEM fit; the default proxy path does not.
#'
#' @section Reproducibility:
#' Plot construction is RNG-neutral. For fixed result/matrix inputs and plotting
#' options, cosmetic geometry does not consume or reseed the caller's RNG stream.
#'
#' @details
#' When a high-level result is supplied, this function only extracts the already
#' computed optimizer result, cosine matrix, and item metadata before calling the
#' established plotting code. Optional plot failures are summarized at the end
#' and stored in the `semantica_plot_failures` attribute. The
#' `semantica_plot_manifest` attribute records generated plots and saved paths.
#' `which` accepts `"pheromone"`, `"fitness"`, `"networks"`, `"loadings"`,
#' `"loading_profiles"`, `"dfi"`, `"radar"`, `"discrimination"`, `"interactive"`,
#' `"paths"`, `"selection_frequency"`, `"specificity"`, `"pfa"`,
#' `"reference_n"`, `"summary"`, or `"all"`. Family shortcuts expand to the corresponding existing plot functions without recomputing diagnostics.
#'
#' @return Named list of `ggplot` and `plotly` objects, including
#'   `p10a_path_before`, `p10b_path_after`, and
#'   `plot_summary_of_results`. Internal opt-in distillation plots are
#'   intentionally not generated by this wrapper.
#' @export
#' @examples
#' \dontrun{
#' plots <- semantica_plot_all(
#'   result = result,
#'   cosine_sim_matrix = cosine_sim_matrix,
#'   df = item_metadata,
#'   include_interactive = TRUE,
#'   before_path_model = "proxy",
#'   save = FALSE
#' )
#' }
semantica_plot_all <- function(result, cosine_sim_matrix = NULL, df = NULL, multi_result = NULL,
                               interactive_mode = c("2d", "3d"), save = FALSE, out_dir = ".",
                               device = "png", width = 12, height = 8, dpi = 180,
                               before_path_model = c("proxy", "refit"),
                               before_path_refit_max_items = 60L,
                               network_max_items = 200L, mds_max_items = 250L,
                               path_proxy_max_items = 150L,
                               include_interactive = TRUE,
                               progress = TRUE,
                               which = "all") {
  interactive_mode <- match.arg(interactive_mode)
  before_path_model <- match.arg(before_path_model)
  progress <- .semantica_assert_flag(progress, "progress")
  allowed_plots <- c("pheromone", "fitness", "networks", "loadings", "loading_profiles", "dfi", "radar", "discrimination", "interactive", "paths", "selection_frequency", "specificity", "pfa", "reference_n", "summary")
  plot_groups <- list(
    aco = c("pheromone", "fitness", "selection_frequency"),
    esem = c("loadings", "loading_profiles", "paths", "specificity"),
    semantic = c("networks", "discrimination", "interactive"),
    pfa = "pfa",
    dfi = c("dfi", "reference_n")
  )
  requested <- unique(as.character(which))
  if (!length(requested) || anyNA(requested) || any(!nzchar(requested))) stop("'which' must contain one or more plot names/groups, or 'all'.", call. = FALSE)
  allowed_selectors <- unique(c("all", allowed_plots, names(plot_groups)))
  bad <- setdiff(requested, allowed_selectors)
  if (length(bad)) stop(sprintf("Unknown plot name/group(s): %s. Available: %s.", paste(bad, collapse = ", "), paste(allowed_selectors, collapse = ", ")), call. = FALSE)
  if ("all" %in% requested) {
    which <- allowed_plots
  } else {
    which <- unique(unlist(lapply(requested, function(z) plot_groups[[z]] %||% z), use.names = FALSE))
  }
  want <- function(name) name %in% which

  # High-level QoL adapter: extract the already-computed optimizer inputs from
  # semantica_run()/semantica_full_pipeline() results. No analysis is rerun.
  if (inherits(result, "semantica_full_pipeline_result")) {
    full_result <- result
    cosine_sim_matrix <- cosine_sim_matrix %||% full_result$generation$cosine_sim_matrix %||% NULL
    df <- df %||% full_result$generation$df %||% NULL
    result <- full_result$optimization %||% full_result
  }
  if (is.null(cosine_sim_matrix)) {
    stop("'cosine_sim_matrix' is required unless 'result' is a high-level semantica_run()/semantica_full_pipeline() result.", call. = FALSE)
  }

  out <- list()
  plot_failures <- character(0L)
  saved_paths <- character(0L)
  if (isTRUE(progress)) cat("\n[SEMANTICA viz] Generating plots...\n")

  run_plot <- function(name, fun) {
    started <- proc.time()[["elapsed"]]
    if (isTRUE(progress)) message(sprintf("  %s: generating...", name))
    value <- tryCatch(fun(), error = function(e) {
      plot_failures <<- c(plot_failures, sprintf("%s: %s", name, e$message))
      if (isTRUE(progress)) message("  ", name, " failed: ", e$message)
      NULL
    })
    if (isTRUE(progress)) {
      message(sprintf("  %s: done (%.2fs)", name, proc.time()[["elapsed"]] - started))
    }
    value
  }

  out$p01_pheromone <- if (want("pheromone")) run_plot("Plot 1", function() plot_pheromone_heatmap(result)) else NULL
  out$p02_fitness <- if (want("fitness")) run_plot("Plot 2", function() plot_fitness_evolution(result)) else NULL
  out$p03_networks <- if (want("networks")) run_plot("Plot 3", function() plot_semantic_networks(
    result, cosine_sim_matrix, df = df, max_before_items = network_max_items
  )) else NULL
  out$p04_loadings_matrix <- if (want("loadings")) run_plot("Plot 4", function() plot_esem_loadings(result)) else NULL
  out$p05_loading_profiles <- if (want("loading_profiles")) run_plot("Plot 5", function() plot_loading_profiles(result)) else NULL
  out$p06_dfi_gauges <- if (want("dfi")) run_plot("Plot 6", function() plot_dfi_gauges(result)) else NULL
  out$p07_score_radar <- if (want("radar")) run_plot("Plot 7", function() plot_score_radar(result)) else NULL
  out$p08_discrimination <- if (want("discrimination")) run_plot("Plot 8", function() plot_semantic_discrimination(result, cosine_sim_matrix)) else NULL
  out$p09_interactive <- if (want("interactive") && isTRUE(include_interactive)) {
    run_plot("Plot 9", function() plot_interactive_semantic_space(
      result, cosine_sim_matrix, mode = interactive_mode, max_pool_items = mds_max_items
    ))
  } else {
    NULL
  }

  p10_result <- if (want("paths")) run_plot("Plot 10", function() plot_esem_path_diagrams(
    result, cosine_sim_matrix, df = df,
    before_model = before_path_model,
    before_refit_max_items = before_path_refit_max_items,
    before_proxy_max_items = path_proxy_max_items
  )) else NULL
  out$p10a_path_before <- if (!is.null(p10_result)) p10_result$p10a else NULL
  out$p10b_path_after  <- if (!is.null(p10_result)) p10_result$p10b else NULL

  out$p11_selection_freq <- if (want("selection_frequency")) run_plot("Plot 11", function() plot_item_selection_frequency(result = result, multi_result = multi_result)) else NULL
  out$p12_specificity <- if (want("specificity")) run_plot("Plot 12", function() plot_crossloading_specificity(result)) else NULL
  out$p13_pfa <- if (want("pfa")) run_plot("Plot 13", function() plot_pfa_diagnostics(result)) else NULL
  out$p14_semantic_n <- if (want("reference_n")) run_plot("Plot 14", function() plot_semantic_n_sensitivity(result)) else NULL
  out$plot_summary_of_results <- if (want("summary")) run_plot("Summary plot", function() plot_summary_of_results(result, cosine_sim_matrix)) else NULL

  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    static_plots <- list(
      "01_pheromone_heatmap_all_items" = out$p01_pheromone,
      "02_fitness_evolution" = out$p02_fitness,
      "03_semantic_networks" = out$p03_networks,
      "04_esem_loading_matrix" = out$p04_loadings_matrix,
      "05_loading_profiles" = out$p05_loading_profiles,
      "06_dfi_gauges" = out$p06_dfi_gauges,
      "07_score_radar" = out$p07_score_radar,
      "08_semantic_discrimination" = out$p08_discrimination,
      "10A_esem_path_diagram_BEFORE" = out$p10a_path_before,
      "10B_esem_path_diagram_AFTER" = out$p10b_path_after,
      "11_item_selection_frequency" = out$p11_selection_freq,
      "12_crossloading_specificity" = out$p12_specificity,
      "13_sample_free_pfa_loadings" = out$p13_pfa,
      "14_semantic_proxy_reference_n_sensitivity" = out$p14_semantic_n,
      "15_summary_of_results" = out$plot_summary_of_results
    )
    static_keys <- c(
      p01_pheromone = "01_pheromone_heatmap_all_items", p02_fitness = "02_fitness_evolution",
      p03_networks = "03_semantic_networks", p04_loadings_matrix = "04_esem_loading_matrix",
      p05_loading_profiles = "05_loading_profiles", p06_dfi_gauges = "06_dfi_gauges",
      p07_score_radar = "07_score_radar", p08_discrimination = "08_semantic_discrimination",
      p10a_path_before = "10A_esem_path_diagram_BEFORE", p10b_path_after = "10B_esem_path_diagram_AFTER",
      p11_selection_freq = "11_item_selection_frequency", p12_specificity = "12_crossloading_specificity",
      p13_pfa = "13_sample_free_pfa_loadings", p14_semantic_n = "14_semantic_proxy_reference_n_sensitivity",
      plot_summary_of_results = "15_summary_of_results"
    )
    path_plot_names <- c("10A_esem_path_diagram_BEFORE", "10B_esem_path_diagram_AFTER")
    for (nm in names(static_plots)) {
      p <- static_plots[[nm]]; if (is.null(p)) next
      fp <- file.path(out_dir, paste0(nm, ".", device))
      w <- width; h <- if (nm %in% path_plot_names) width * 1.4 else height
      tryCatch({ ggplot2::ggsave(fp, plot = p, width = w, height = h, dpi = dpi); saved_key <- names(static_keys)[match(nm, static_keys)]; if (length(saved_key) && !is.na(saved_key)) saved_paths[[saved_key]] <- normalizePath(fp, winslash = "/", mustWork = FALSE); if (isTRUE(progress)) cat(sprintf("  Saved: %s\n", fp)) }, error = function(e) { plot_failures <<- c(plot_failures, sprintf("save %s: %s", nm, e$message)); if (isTRUE(progress)) message("  Could not save ", nm, ": ", e$message) })
    }
    if (!is.null(out$p09_interactive) && requireNamespace("htmlwidgets", quietly = TRUE)) {
      fp9 <- file.path(out_dir, sprintf("09_interactive_semantic_space_%s.html", interactive_mode))
      tryCatch({ htmlwidgets::saveWidget(out$p09_interactive, fp9, selfcontained = TRUE); saved_paths[["p09_interactive"]] <- normalizePath(fp9, winslash = "/", mustWork = FALSE); if (isTRUE(progress)) cat(sprintf("  Saved: %s\n", fp9)) }, error = function(e) { plot_failures <<- c(plot_failures, sprintf("save Plot 9: %s", e$message)); if (isTRUE(progress)) message("  Could not save Plot 9: ", e$message) })
    }
  }
  n_generated <- sum(!vapply(out, is.null, logical(1L)))
  if (isTRUE(progress)) {
    if (length(plot_failures)) {
      cat(sprintf("\n[SEMANTICA viz] Done: %d plot object(s) generated; %d optional plot/save operation(s) failed or were skipped.\n", n_generated, length(plot_failures)))
      for (z in plot_failures) cat("  - ", z, "\n", sep = "")
    } else {
      cat(sprintf("\n[SEMANTICA viz] Done: %d plot object(s) generated successfully.\n", n_generated))
    }
  }
  attr(out, "semantica_plot_failures") <- plot_failures
  manifest <- data.frame(plot = names(out), generated = !vapply(out, is.null, logical(1L)), stringsAsFactors = FALSE)
  manifest$saved_path <- NA_character_
  if (length(saved_paths)) {
    for (nm in names(saved_paths)) {
      idx <- match(nm, manifest$plot)
      if (!is.na(idx)) manifest$saved_path[[idx]] <- saved_paths[[nm]]
    }
  }
  attr(out, "semantica_plot_manifest") <- manifest
  invisible(out)
}
