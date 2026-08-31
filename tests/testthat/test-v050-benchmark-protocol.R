test_that("selection-method benchmark enforces common budget and seed design metadata", {
  seen <- new.env(parent = emptyenv())
  seen$rows <- list()
  make_method <- function(label) {
    force(label)
    function(problem, budget, seed) {
      seen$rows[[length(seen$rows) + 1L]] <- list(label = label, budget = budget, seed = seed)
      list(value = problem + seed)
    }
  }
  out <- semantica_compare_selection_methods(
    methods = list(aco = make_method("aco"), baseline = make_method("baseline")),
    problem = 10,
    budget = list(evaluations = 25L),
    evaluator = function(x) c(external_score = x$value),
    seeds = c(3L, 7L)
  )
  expect_equal(length(seen$rows), 4L)
  expect_true(all(vapply(seen$rows, function(z) identical(z$budget, list(evaluations = 25L)), logical(1))))
  expect_setequal(vapply(seen$rows, `[[`, integer(1), "seed"), c(3L, 7L))
  expect_identical(out$evidence_role, "selection_method_external_benchmark")
  expect_true("independent_evaluator" %in% out$benchmark_requirements)
  expect_match(out$weighting_policy, "held-out participant")
})
