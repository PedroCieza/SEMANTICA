test_that("exploratory PFA recovers a dense planted structure with either extractor", {
  skip_if_not_installed("psych")
  withr::local_seed(173L)
  group <- rep(LETTERS[1:6], each = 4L)
  ids <- paste0("i", seq_along(group))
  matrix <- matrix(.70, length(group), length(group), dimnames = list(ids, ids))
  matrix[outer(group, group, "==")] <- .85
  diag(matrix) <- 1
  labels <- setNames(group, ids)

  for (method in c("ml", "minres")) {
    rotation <- if (method == "minres") "oblimin" else "promax"
    fit <- semantica_structure_probe(matrix, 6L, method, rotation)
    expect_identical(fit$status, "observed")
    expect_identical(fit$extraction, method)
    expect_identical(fit$rotation, rotation)
    expect_equal(fit$occupied_factors, 6)
    expect_equal(.semantica_adjusted_rand_index(labels, fit$components)$value, 1)
    expect_lt(fit$rms_offdiag, 1e-6)
    expect_true(all(fit$uniquenesses > 0))
    native <- compute_pfa_diagnostics(matrix, labels, unique(labels), method, rotation)
    expect_identical(native$structure_disposition, "advance")
  }
  # High common similarity masks six dimensions under the Kaiser count rule.
  expect_equal(semantica_structure_probe(matrix)$n_factors, 1)
  expect_equal(semantica_structure_probe(diag(8))$n_factors, 0)
  expect_error(semantica_structure_probe(matrix[, rev(ids)]), "symmetric|same order")
  config <- semantica_pfa_config(extraction = "minres", final_extraction = "minres")
  expect_identical(config$extraction, config$final_extraction)
})

test_that("dense matrices with absent trait contrast cannot advance through native PFA", {
  ids <- paste0("i", 1:12)
  labels <- setNames(rep(c("A", "B"), each = 6L), ids)
  matrix <- matrix(.70, 12L, 12L, dimnames = list(ids, ids))
  diag(matrix) <- 1
  native <- compute_pfa_diagnostics(matrix, labels, c("A", "B"), "ml")
  expect_true(native$structure_disposition %in% c("defer", "unavailable"))
})

test_that("a favorable primary fit stays uncorroborated until a distinct source agrees", {
  ids <- paste0("i", 1:12)
  groups <- setNames(rep(c("A", "B", "C"), each = 4L), ids)
  make_matrix <- function(within) {
    x <- matrix(.02, 12L, 12L, dimnames = list(ids, ids))
    x[outer(groups, groups, "==")] <- within
    diag(x) <- 1
    x
  }
  x <- make_matrix(.60)
  pfa <- compute_pfa_diagnostics(x, groups, unique(groups), "ml")
  pfa$input_geometry_id <- .semantica_structure_geometry_id(x)
  result <- list(best_items = ids, factor_assignment = groups, pfa_diagnostics = pfa)
  expect_identical(semantica_corroborate_structure(result)$status, "uncorroborated")
  same <- semantica_corroborate_structure(result, x[rev(ids), rev(ids)], "same", "synthetic")
  expect_identical(same$reason, "same_geometry")
  expect_identical(same$status, "uncorroborated")
  weak <- semantica_corroborate_structure(result, make_matrix(.08), "weak", "synthetic")
  expect_identical(weak$status, "defer")
  strong <- semantica_corroborate_structure(result, make_matrix(.65), "strong", "synthetic")
  expect_identical(strong$status, "supported_by_both")
  expect_error(semantica_corroborate_structure(result, x[-1, -1], "partial", "synthetic"),
    "same unique selected item IDs")
})

test_that("the borderline gate admits only one narrowly weak item with enough strong peers", {
  pfa <- list(structure_disposition = "defer", available = TRUE,
    min_uniqueness = .20, min_loading = .40, min_margin = .20,
    item_diagnostics = data.frame(factor = rep(c("A", "B"), each = 4L),
      primary_loading = c(.37, rep(.6, 7L)), loading_margin = .25,
      dominant_pfa_component = rep(c("a", "b"), each = 4L)))
  expect_identical(semantica_pfa_gate(pfa), "defer")
  expect_identical(semantica_pfa_gate(pfa, "one_borderline"), "advance")
  pfa$item_diagnostics$item <- paste0("i", seq_len(nrow(pfa$item_diagnostics)))
  details <- semantica_pfa_gate(pfa, "one_borderline", details = TRUE)
  expect_identical(details$disposition, "advance")
  expect_true(details$borderline_used)
  expect_identical(details$below_loading, "i1")
  expect_identical(details$below_margin, character())
  incomplete <- pfa
  incomplete$item_diagnostics$loading_margin[2L] <- NA_real_
  expect_identical(semantica_pfa_gate(incomplete, details = TRUE)$unresolved_items, "i2")
  boundary <- pfa
  boundary$item_diagnostics$primary_loading[1L] <- .35
  expect_identical(semantica_pfa_gate(boundary, "one_borderline"), "advance")
  for (change in list(list(row = 1L, key = "primary_loading", value = .34),
      list(row = 5L, key = "primary_loading", value = .37),
      list(row = 3L, key = "loading_margin", value = .19),
      list(row = 1L, key = "dominant_pfa_component", value = NA_character_))) {
    altered <- pfa
    altered$item_diagnostics[change$row, change$key] <- change$value
    expect_identical(semantica_pfa_gate(altered, "one_borderline"), "defer")
  }
  pfa$item_diagnostics <- pfa$item_diagnostics[-2L, ]
  expect_identical(semantica_pfa_gate(pfa, "one_borderline"), "defer")
  pfa$structure_disposition <- "unavailable"
  expect_identical(semantica_pfa_gate(pfa, "one_borderline"), "unavailable")
})

test_that("an embedding view binds the selected wording and declares a different encoder", {
  ids <- paste0("i", 1:12)
  assignment <- setNames(rep(c("A", "B", "C"), each = 4L), ids)
  wording <- setNames(paste("Question", seq_along(ids)), ids)
  gram <- function(within) {
    x <- matrix(.02, 12L, 12L, dimnames = list(ids, ids))
    x[outer(assignment, assignment, "==")] <- within
    diag(x) <- 1
    x
  }
  vectors <- t(chol(gram(.65)))
  rownames(vectors) <- ids
  view <- semantica_embedding_view(vectors[rev(ids), ], wording, "alternate", "r1")
  expect_equal(view$matrix, gram(.65))
  primary <- gram(.6)
  opt <- list(best_items = ids, factor_assignment = assignment,
    pfa_diagnostics = compute_pfa_diagnostics(primary, assignment, unique(assignment), "ml"))
  opt$pfa_diagnostics$input_geometry_id <- .semantica_structure_geometry_id(primary)
  result <- list(optimization = opt,
    selected_item_metadata = data.frame(ID = rev(ids), item = unname(wording[rev(ids)])),
    reproducibility = list(models = list(resolved_embedding_model = "primary")))
  checked <- semantica_corroborate_structure(list(advanced = result), view)
  expect_identical(checked$status, "supported_by_both")
  expect_identical(checked$source, "alternate")
  expect_identical(checked$source_family, "alternate_encoder")
  expect_identical(checked$revision, "r1")
  expect_identical(checked$view_id, view$id)

  same <- semantica_embedding_view(vectors, wording, "PRIMARY", "r2")
  expect_identical(semantica_corroborate_structure(result, same)$reason, "same_encoder")
  identical_view <- semantica_embedding_view(t(chol(primary)), wording, "other encoder")
  expect_identical(semantica_corroborate_structure(result, identical_view)$reason, "same_geometry")
  wrong <- wording
  wrong[1L] <- "Reworded item"
  expect_error(semantica_corroborate_structure(result,
    semantica_embedding_view(vectors, wrong, "alternate")), "wording or item IDs differ")
  expect_error(semantica_corroborate_structure(result,
    semantica_embedding_view(vectors[-1L, ], wording[-1L], "alternate")), "wording or item IDs differ")
  expect_error(semantica_embedding_view(vectors, wording, ""), "nonempty model")
  expect_error(semantica_embedding_view(vectors[c(1L, 1:11), ], wording, "alternate"), "unique")
  view$matrix[1L, 1L] <- 0
  expect_error(semantica_corroborate_structure(result, view), "changed after construction")
})
