test_that("ESEM feasibility preflight flags collapsed high-pressure search spaces", {
  items <- paste0("i", seq_len(12L))
  sim <- matrix(0.92, nrow = 12L, ncol = 12L, dimnames = list(items, items))
  diag(sim) <- 1
  assignment <- stats::setNames(rep(c("F1", "F2", "F3"), each = 4L), items)
  pool_health <- data.frame(
    factor = c("F1", "F2", "F3"),
    cohesion_eligible = c(4L, 4L, 6L),
    selected_target = c(4L, 4L, 4L),
    selection_pressure_eligible = c(1, 1, 0.75),
    operational_status = c("adequate_capacity", "content_constrained", "adequate_capacity"),
    stringsAsFactors = FALSE
  )
  duplicate_feasibility <- data.frame(
    factor = c("F1", "F2", "F3"),
    feasible = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  out <- SEMANTICA:::.semantica_assess_esem_search_feasibility(
    cosine_sim_matrix = sim,
    factor_assignment = assignment,
    factors = c("F1", "F2", "F3"),
    i.per.f = c(F1 = 4L, F2 = 4L, F3 = 4L),
    pool_health = pool_health,
    duplicate_feasibility = duplicate_feasibility,
    run_esem_during_search = TRUE
  )

  expect_identical(out$risk, "high")
  expect_match(paste(out$reasons, collapse = " "), "effective rank")
  expect_match(paste(out$reasons, collapse = " "), "duplicate guard infeasible")
  expect_identical(out$recommended_action, "generate_more_candidates_before_relaxing_guards")
  expect_equal(out$top_up_recommendation$factor, c("F1", "F2"))
  expect_match(out$note, "does not alter embeddings", fixed = TRUE)
})

test_that("ESEM feasibility preflight is explicit when ESEM search is not requested", {
  out <- SEMANTICA:::.semantica_assess_esem_search_feasibility(
    cosine_sim_matrix = diag(3L),
    factor_assignment = stats::setNames(c("F1", "F1", "F1"), paste0("i", 1:3)),
    factors = "F1",
    i.per.f = c(F1 = 3L),
    run_esem_during_search = FALSE
  )

  expect_identical(out$status, "not_requested")
  expect_identical(out$risk, "none")
})
