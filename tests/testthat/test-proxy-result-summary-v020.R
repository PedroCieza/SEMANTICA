test_that("full-pipeline summary makes semantic-only evidence status explicit", {
  x <- structure(list(
    semantic_score=.82,
    pfa_score=.77,
    optimization=list(response_validation=NULL),
    fit_indices=list(cfi=.95,rmsea=.05,srmr=.04),
    construct_coverage=list(overall_required_facet_coverage=.75),
    polarity_diagnostics=data.frame(direction=c("not_flagged","potentially_reversed_or_negated")),
    participant_validation_performed=FALSE,
    interpretation_notice="proxy only"
  ), class=c("semantica_full_pipeline_result","list"))
  s <- summary(x)
  expect_s3_class(s, "summary.semantica_full_pipeline_result")
  expect_equal(s$participant_validation, "NOT PERFORMED")
  txt <- capture.output(print(s))
  expect_true(any(grepl("NOT PERFORMED", txt, fixed=TRUE)))
})
