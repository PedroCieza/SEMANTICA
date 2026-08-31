test_that("ESEM telemetry exposes stage, cache, elapsed, and state fields", {
  tel <- list(esem_events = data.frame(
    stage = c("search","search","final"), candidate_key = c("a","b","c"),
    cache_hit = c(FALSE,TRUE,FALSE), coalesced_requests=c(1L,1L,0L),
    elapsed_seconds=c(.2,.01,.5), solver_method=c("x","x","x"),
    converged=c(TRUE,TRUE,FALSE), admissible=c(TRUE,TRUE,FALSE),
    reason=c(NA,NA,"failed"), fallback_used=c(FALSE,FALSE,TRUE),
    stringsAsFactors=FALSE
  ), esem_fits_started=2L)
  out <- semantica_esem_telemetry(tel)
  expect_true(all(c("stage","candidate_key","cache_hit","elapsed_seconds","converged","admissible","reason","fallback_used") %in% names(out$events)))
  expect_true(all(c("search","final") %in% out$stage_summary$stage))
  expect_false(out$analysis_behavior_changed)
})

test_that("empty ESEM telemetry remains inspectable", {
  out <- semantica_esem_telemetry(list(esem_events=data.frame(), esem_fits_started=0L))
  expect_equal(nrow(out$stage_summary), 0L)
})
