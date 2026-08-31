test_that("objective schema groups proxy structure without pretending it is independent evidence", {
  # Inspect the function body because this phase intentionally changes schema
  # semantics without introducing an unvalidated new optimizer weight.
  txt <- paste(deparse(body(ACO_with_ESEM)), collapse = "\n")
  expect_match(txt, "evidence_grouping")
  expect_match(txt, "shared_embedding_representation")
  expect_match(txt, "held_out_empirical_calibration")
})

test_that("PFA and ESEM remain configurable rather than receiving new hard-coded evidence weights", {
  f <- formals(ACO_with_ESEM)
  expect_true("pfa_weight" %in% names(f))
  expect_true("esem_weight" %in% names(f))
  expect_equal(eval(f$pfa_weight), 0.20)
  # Direct advanced ACO default is retained; casual presets continue to resolve
  # their own established values through semantica_aco_config().
  expect_true(is.numeric(eval(f$esem_weight)))
})
