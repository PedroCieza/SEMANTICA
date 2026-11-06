# Feasibility of domain/facet quotas and global duplicate groups is a bounded
# circulation problem. No quality cutoffs, subset enumeration or new dependency.
.semantica_constraint_priority_order <- function(weights) {
  weights <- as.numeric(weights)
  weights[!is.finite(weights) | weights <= 0] <- .Machine$double.eps
  order(-log(pmax(stats::runif(length(weights)), .Machine$double.eps)) / weights)
}

.semantica_joint_pool_selection <- function(
    eligible_items, i_per_f, duplicate_cluster_id = NULL,
    item_facet_lookup = NULL, facets_by_factor = NULL, mode = "soft",
    min_per_facet = 1L, max_imbalance = 1L, probs = NULL, priority = NULL) {
  items <- unlist(eligible_items[names(i_per_f)], use.names = FALSE)
  cid <- if (is.null(duplicate_cluster_id)) rep(NA_character_, length(items)) else duplicate_cluster_id[items]
  grouped <- !is.na(cid) & nzchar(cid)
  keys <- ifelse(grouped, paste0("cluster:", cid), paste0("item:", items))
  groups <- match(keys, unique(keys))
  # Source, sink, duplicate groups, then domain/facet nodes.
  n_vertices <- 2L + max(c(0L, groups))
  from <- to <- lower <- upper <- numeric(0L)
  edge <- function(a, b, lo = 0, hi = 1) {
    from <<- c(from, a); to <<- c(to, b)
    lower <<- c(lower, lo); upper <<- c(upper, hi)
    length(from)
  }
  vertex <- function() { n_vertices <<- n_vertices + 1L; n_vertices }
  for (g in unique(groups)) edge(g + 2L, 2L)
  edge(2L, 1L, hi = sum(i_per_f))
  item_edges <- stats::setNames(integer(length(items)), items)
  domain_nodes <- unfilled <- stats::setNames(integer(length(i_per_f)), names(i_per_f))
  balance <- list()
  for (f in names(i_per_f)) {
    fnode <- vertex(); quota <- i_per_f[[f]]
    domain_nodes[f] <- fnode
    edge(1L, fnode, quota, quota)
    ids <- eligible_items[[f]]
    facets <- facets_by_factor[[f]] %||% character(0L)
    facets <- unique(facets[!is.na(facets) & nzchar(facets)])
    hard <- mode != "soft" && !is.null(item_facet_lookup) && length(facets) > 0L
    targets <- stats::setNames(rep(fnode, length(ids)), ids)
    if (hard) {
      facet_edges <- integer(length(facets))
      credited <- as.character(item_facet_lookup[ids])
      for (j in seq_along(facets)) {
        node <- vertex()
        facet_edges[j] <- edge(fnode, node, min_per_facet, quota)
        targets[ids[!is.na(credited) & credited == facets[j]]] <- node
      }
      unknown <- is.na(credited) | !credited %in% facets
      if (mode == "balanced") {
        ids <- ids[!unknown]
        m <- length(facets)
        first <- max(min_per_facet, ceiling((quota - (m - 1L) * max_imbalance) / m))
        last <- floor(quota / m)
        if (first > last) return(list(feasible = FALSE, items = character(0L)))
        bases <- seq.int(first, last)
        balance[[f]] <- list(edges = facet_edges, bases = bases)
      }
    }
    for (id in ids) item_edges[id] <- edge(targets[[id]], groups[match(id, items)] + 2L)
  }
  solve <- function() {
    if (any(upper < lower)) return(NULL)
    demand <- numeric(n_vertices)
    for (i in which(lower > 0)) {
      demand[to[i]] <- demand[to[i]] + lower[i]
      demand[from[i]] <- demand[from[i]] - lower[i]
    }
    positive <- which(demand > 0); negative <- which(demand < 0)
    endpoints <- rbind(cbind(from, to), cbind(n_vertices + 1L, positive),
      cbind(negative, n_vertices + 2L))
    graph <- igraph::make_empty_graph(n_vertices + 2L, directed = TRUE)
    graph <- igraph::add_edges(graph, as.vector(t(endpoints)))
    fit <- igraph::max_flow(graph, n_vertices + 1L, n_vertices + 2L,
      capacity = c(upper - lower, demand[positive], -demand[negative]))
    if (fit$value < sum(demand[positive])) {
      remaining <- stats::setNames(demand[positive] - fit$flow[length(from) + seq_along(positive)], positive)
      missing <- remaining[as.character(domain_nodes)]; missing[is.na(missing)] <- 0
      unfilled <<- stats::setNames(as.integer(missing), names(i_per_f))
      return(NULL)
    }
    unfilled[] <<- 0L
    fit$flow[seq_along(from)] + lower
  }
  # Usually the balanced baseline is unique. Wider user-specified imbalance
  # bounds are searched exactly, pruning branches by the relaxed flow problem.
  search <- function(k = 1L) {
    witness <- solve()
    if (is.null(witness) || k > length(balance)) return(witness)
    b <- balance[[k]]; saved <- list(lower[b$edges], upper[b$edges])
    on.exit({ lower[b$edges] <<- saved[[1L]]; upper[b$edges] <<- saved[[2L]] })
    for (base in b$bases) {
      lower[b$edges] <<- base; upper[b$edges] <<- base + max_imbalance
      answer <- search(k + 1L)
      if (!is.null(answer)) return(answer)
    }
    NULL
  }
  flow <- search()
  if (!is.null(flow) && (!is.null(probs) || !is.null(priority))) {
    # Retain a weighted choice only when it admits a complete feasible form.
    # Rejected choices cannot become feasible after forcing additional items.
    chosen <- 0L
    ordered <- if (is.null(priority)) items[.semantica_constraint_priority_order(probs[items])] else intersect(priority, items)
    for (id in ordered) {
      e <- item_edges[[id]]
      if (e == 0L) next
      lower[e] <- 1
      witness <- search()
      if (is.null(witness)) lower[e] <- 0 else {
        flow <- witness; chosen <- chosen + 1L
        if (chosen == sum(i_per_f)) break
      }
    }
  }
  usable <- item_edges > 0L
  selected <- if (is.null(flow)) character(0L) else names(item_edges)[usable][flow[item_edges[usable]] > 0.5]
  list(feasible = !is.null(flow), items = selected, method = "joint_lower_bound_flow",
    unfilled_domain_slots = unfilled)
}
