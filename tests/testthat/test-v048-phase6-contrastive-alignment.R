test_that("casual interface uses the existing conservative construct-definition guard", {
  captured <- NULL
  local_mocked_bindings(
    semantica_full_pipeline = function(...) {
      captured <<- list(...)
      structure(list(reproducibility = list()), class = c("semantica_full_pipeline_result", "list"))
    },
    .package = "SEMANTICA"
  )
  out <- semantica_run(
    scale_name = "Contrastive test",
    scale_description = "Two theoretically distinct constructs.",
    factors = list(A = "Definition of A.", B = "Definition of B."),
    llm = "ollama", verbose = FALSE
  )
  expect_identical(captured$quality$content_alignment_mode, "guard")
  expect_identical(out$run_config$content_alignment_role, "conservative_feasibility_guard")
})

test_that("construct-definition margins only break equal relative heuristic ranks", {
  ids <- c("a1", "a2", "a3", "b1", "b2", "b3")
  sim <- matrix(0.20, 6, 6, dimnames = list(ids, ids)); diag(sim) <- 1
  sim[1:3, 1:3] <- 0.75
  sim[4:6, 4:6] <- 0.75
  diag(sim) <- 1
  items <- list(A = ids[1:3], B = ids[4:6])
  margin <- setNames(c(.40, .10, .20, .30, .20, .10), ids)
  h <- compute_item_heuristics(
    items, sim, c("A", "B"),
    within_similarity_target = c(A = .75, B = .75),
    semantic_objective_mode = "relative_conservative",
    content_alignment_margin = margin
  )
  # All A items have exactly the same within/between geometry; only the
  # contrastive definition margin orders the otherwise tied proposal heuristic.
  expect_gt(unname(h$A["a1"]), unname(h$A["a3"]))
  expect_gt(unname(h$A["a3"]), unname(h$A["a2"]))
})
