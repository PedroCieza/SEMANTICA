test_that("human-oriented summary propagates the representation evidence state", {
  obj <- structure(list(
    representation_evidence_state = list(
      status = "representation_sensitive_and_concentrated",
      qualifiers = "synthetic qualifier"
    ),
    representation_stability = list(
      common_direction_strength = .9,
      cosine_adjustment_sensitivity = list(
        top_pair_overlap_vs_random = "at_or_below_random_reference",
        top_pair_jaccard = .1
      )
    ),
    cosine_diagnostics = list(),
    run_config = list(dimensionality = "multidimensional"),
    best_items = c("i1", "i2")
  ), class = c("semantica_full_pipeline_result", "list"))
  sec <- .semantica_diagnostic_sections(obj)
  expect_identical(sec$representation$evidence_state, "representation_sensitive_and_concentrated")
  expect_match(sec$representation$downstream_dependency, "qualifies semantic, PFA, ESEM")
})
