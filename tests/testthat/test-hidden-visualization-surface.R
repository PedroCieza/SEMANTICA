test_that("internal distillation plot stays hidden from regular exports", {
  ns <- asNamespace("SEMANTICA")

  expect_false("the_coolest_plot_ever" %in% getNamespaceExports("SEMANTICA"))
  expect_true(exists("the_coolest_plot_ever", envir = ns, inherits = FALSE))
})
