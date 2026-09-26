test_that("fitness history plot shows regimes without connecting raw candidates", {
  hist <- list(
    list(sem_score = 0.50, total = 0.50, stage = "semantic_iteration"),
    list(sem_score = 0.54, total = 0.53, stage = "semantic_iteration"),
    list(sem_score = 0.51, total = 0.52, stage = "semantic_iteration"),
    list(sem_score = 0.49, esem_score = 0.26, total = 0.45, stage = "esem_guided_iteration"),
    list(sem_score = 0.53, esem_score = 0.31, total = 0.48, stage = "esem_guided_iteration"),
    list(sem_score = 0.55, esem_score = 0.29, total = 0.47, stage = "esem_guided_iteration")
  )

  p <- plot_fitness_evolution(list(solution_history = hist))

  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(layer) class(layer$geom)[[1L]], character(1L))
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomStep" %in% geoms)
  expect_true(inherits(p$facet, "FacetWrap"))
  expect_no_error(ggplot2::ggplot_build(p))
})
