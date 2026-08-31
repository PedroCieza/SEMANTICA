test_that("content guard requires preprocessing agreement before automatic exclusion", {
  z <- SEMANTICA:::.semantica_robust_content_guard(
    raw_factor_mismatch = c(TRUE, TRUE, FALSE, FALSE),
    centered_factor_mismatch = c(FALSE, TRUE, TRUE, FALSE),
    raw_exclusion_conflict = c(FALSE, FALSE, FALSE, TRUE),
    centered_exclusion_conflict = c(FALSE, FALSE, FALSE, FALSE)
  )

  # Raw-only exclusions are retained; raw+centered agreement is exclusionary.
  expect_equal(z$raw_pass, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(z$robust_pass, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(
    z$sensitivity,
    c(
      "raw_exclusion_not_robust_to_centering_retained",
      "raw_exclusion_confirmed_by_centering",
      "not_triggered",
      "raw_exclusion_not_robust_to_centering_retained"
    )
  )
})

test_that("representation evidence exposes continuous sensitivity axes without changing legacy status", {
  st <- SEMANTICA:::.semantica_representation_evidence_state(
    representation_stability = list(
      top_eigen_share = 0.70,
      cosine_adjustment_sensitivity = list(
        available = TRUE,
        offdiag_correlation = 0.67,
        q95_abs_delta = 0.95,
        top_pair_jaccard = 0.52,
        top_pair_jaccard_random_baseline = 0.03,
        top_pair_overlap_vs_random = "above_random_reference"
      )
    ),
    embedding_diagnostics = list(n_items = 45L, embed_dim = 1024L)
  )

  expect_identical(st$state_schema, "representation-evidence-v2")
  expect_identical(st$status, "representation_concentrated")
  expect_equal(st$preprocessing_sensitivity_axis$offdiag_correlation, 0.67)
  expect_equal(st$preprocessing_sensitivity_axis$top_pair_jaccard, 0.52)
  expect_equal(st$preprocessing_sensitivity_axis$top_pair_excess_over_random, 0.49)
  expect_match(st$preprocessing_sensitivity_axis$interpretation, "no universal cutoff")
})

test_that("frozen-item representation comparison holds item IDs fixed and reports backend disagreement", {
  ids <- paste0("i", 1:6)
  fa <- setNames(c("A", "A", "A", "B", "B", "B"), ids)

  a <- matrix(c(
    1,.8,.7,.2,.1,.2,
    .8,1,.75,.2,.15,.1,
    .7,.75,1,.1,.2,.15,
    .2,.2,.1,1,.8,.7,
    .1,.15,.2,.8,1,.75,
    .2,.1,.15,.7,.75,1
  ), 6, 6, byrow = TRUE, dimnames = list(ids, ids))
  b <- a
  b[1,4] <- b[4,1] <- .55
  b[2,5] <- b[5,2] <- .50

  out <- semantica_compare_embedding_representations(
    list(model_a = a, model_b = b),
    factor_assignment = fa,
    selected_items = list(
      model_a = c("i1", "i2", "i4", "i5"),
      model_b = c("i1", "i3", "i4", "i6")
    )
  )

  expect_identical(out$status, "descriptive_frozen_item_backend_robustness")
  expect_false(out$participant_based)
  expect_true(out$frozen_item_required)
  expect_equal(nrow(out$per_representation), 2L)
  expect_equal(nrow(out$pairwise_geometry), 1L)
  expect_true(out$pairwise_geometry$pairwise_spearman < 1)
  expect_true(out$pairwise_geometry$selected_item_jaccard < 1)
})

test_that("frozen-item comparison rejects non-identical item pools", {
  ids <- paste0("i", 1:4)
  a <- diag(4); dimnames(a) <- list(ids, ids)
  b <- diag(4); dimnames(b) <- list(c(ids[1:3], "x"), c(ids[1:3], "x"))
  fa <- setNames(c("A", "A", "B", "B"), ids)

  expect_error(
    semantica_compare_embedding_representations(list(a = a, b = b), fa),
    "same frozen item IDs"
  )
})

test_that("definition alignment records raw and centered guard evidence without using centered scores as primary", {
  items <- data.frame(
    item_id = c("a1", "a2", "b1", "b2"),
    factor = c("A", "A", "B", "B"),
    item_text = c("A one", "A two", "B one", "B two"),
    stringsAsFactors = FALSE
  )
  emb <- rbind(
    a1 = c(1.0, 0.0),
    a2 = c(0.9, 0.1),
    b1 = c(0.0, 1.0),
    b2 = c(0.1, 0.9)
  )
  factors <- list(
    A = list(description = "Construct A"),
    B = list(description = "Construct B")
  )

  local_mocked_bindings(
    semantica_embed = function(x, ...) {
      refs <- as.character(x$ref_id)
      out <- if (all(grepl("^factor::", refs))) {
        rbind(`factor::A` = c(1, 0), `factor::B` = c(0, 1))
      } else {
        stop("unexpected reference request")
      }
      list(embeddings = out)
    },
    .package = "SEMANTICA"
  )

  z <- SEMANTICA:::.semantica_definition_alignment(
    items, emb, factors, embed_session = list(), cache = FALSE
  )
  expect_true(z$available)
  expect_true(all(c(
    "semantica_factor_margin_centered",
    "semantica_factor_alignment_status_centered",
    "semantica_content_guard_pass_raw",
    "semantica_content_guard_sensitivity"
  ) %in% names(z$table)))
  expect_true(all(z$table$semantica_content_guard_pass))
  expect_match(z$guard_rule, "never enters the ACO objective")
})
