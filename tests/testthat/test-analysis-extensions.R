test_that("adaptive semantic thresholds are explicitly experimental and bounded", {
  ids <- paste0("i", 1:6)
  m <- matrix(c(
    1,.7,.6,.2,.1,.2,
    .7,1,.65,.2,.15,.2,
    .6,.65,1,.25,.1,.2,
    .2,.2,.25,1,.72,.66,
    .1,.15,.1,.72,1,.69,
    .2,.2,.2,.66,.69,1
  ), 6, 6, byrow = TRUE, dimnames = list(ids, ids))
  fa <- setNames(c(rep("A",3), rep("B",3)), ids)
  x <- semantica_calibrate_similarity_thresholds(m, fa)
  expect_s3_class(x, "semantica_threshold_calibration")
  expect_true(x$redundancy_threshold >= .70 && x$redundancy_threshold <= .95)
  expect_true(x$duplicate_threshold > x$redundancy_threshold)
  expect_match(x$status, "experimental")
})

test_that("construct blueprints expose missing facet coverage", {
  factors <- list(A = list(description = "A", facets = list(one=list(), two=list())),
                  B = list(description = "B", facets = list(three=list())))
  bp <- semantica_construct_blueprint(factors)
  items <- data.frame(item_id=c("a1","b1"), factor=c("A","B"), Facet=c("one","three"))
  out <- semantica_assess_construct_coverage(items, bp)
  expect_true("two" %in% out$missing_facets$A)
  expect_equal(out$coverage_table$coverage[out$coverage_table$factor == "B"], 1)
})

test_that("polarity screening flags overt negation conservatively", {
  x <- data.frame(item_id=c("a","b"), item_text=c("I feel capable", "I do not feel capable"))
  out <- semantica_polarity_diagnostics(x)
  expect_false(out$explicit_negation[1])
  expect_true(out$explicit_negation[2])
  expect_match(attr(out, "semantica_note"), "screening aids")
})

test_that("empirical calibration predicts a bounded symmetric matrix", {
  ids <- paste0("i",1:5)
  s <- diag(1,5); s[lower.tri(s)] <- seq(.1,.7,length.out=10); s[upper.tri(s)] <- t(s)[upper.tri(s)]
  r <- diag(1,5); r[lower.tri(r)] <- .05 + .8*s[lower.tri(s)]; r[upper.tri(r)] <- t(r)[upper.tri(r)]
  dimnames(s) <- dimnames(r) <- list(ids,ids)
  fit <- semantica_fit_empirical_calibration(s, response_matrix=r, method="linear")
  out <- semantica_apply_empirical_calibration(s, fit)
  expect_s3_class(fit, "semantica_empirical_calibration")
  expect_equal(unname(diag(out)), rep(1,5))
  expect_equal(out, t(out), tolerance=1e-12)
  expect_true(all(out >= -.999 & out <= 1))
})

test_that("representation ensemble and signed matrix preserve geometry contracts", {
  ids <- paste0("i",1:4)
  m1 <- matrix(c(1,.8,.2,.1,.8,1,.3,.2,.2,.3,1,.7,.1,.2,.7,1),4,4,byrow=TRUE,dimnames=list(ids,ids))
  m2 <- m1; m2[1,2] <- m2[2,1] <- .75
  e <- semantica_ensemble_similarity(list(a=m1,b=m2), "rank_mean")
  expect_equal(e, t(e))
  expect_equal(unname(diag(e)), rep(1,4))
  signed <- semantica_signed_semantic_matrix(m1, setNames(c(1,-1,1,1),ids))
  expect_lt(signed[1,2], 0)
  expect_gt(signed[3,4], 0)
})

test_that("leave-one-scale-out calibration never trains on held-out pairs", {
  make_case <- function(offset) {
    ids <- paste0("i",1:5)
    s <- diag(1,5); s[lower.tri(s)] <- seq(.1,.7,length.out=10); s[upper.tri(s)] <- t(s)[upper.tri(s)]
    r <- diag(1,5); r[lower.tri(r)] <- pmin(.95, offset + .75*s[lower.tri(s)]); r[upper.tri(r)] <- t(r)[upper.tri(r)]
    dimnames(s) <- dimnames(r) <- list(ids,ids)
    list(semantic_matrix=s,response_matrix=r)
  }
  z <- semantica_leave_one_scale_out_calibration(list(S1=make_case(.05), S2=make_case(.08)), method="linear")
  expect_equal(nrow(z$per_scale), 2)
  expect_equal(z$validation_status, "out_of_scale_external_cross_validation")
  expect_true(all(z$per_scale$n_train_pairs == 10))
})

test_that("polarity language aliases work for normal user-facing language names", {
  es <- semantica_polarity_diagnostics(c("Me siento capaz", "No me siento capaz"), language = "Spanish")
  expect_equal(attr(es, "semantica_language"), "es")
  expect_false(es$explicit_negation[[1L]])
  expect_true(es$explicit_negation[[2L]])
  en <- semantica_polarity_diagnostics("I do not agree", language = "English")
  expect_equal(attr(en, "semantica_language"), "en")
  expect_true(en$explicit_negation[[1L]])
})

test_that("structured generation prompts do not contradict their JSON contract", {
  p_json <- .build_factor_prompt("A", "Construct A", 3L, output_mode = "json")
  p_num <- .build_factor_prompt("A", "Construct A", 3L, output_mode = "numbered")
  expect_match(p_json, "valid JSON")
  expect_false(grepl("numbered 1 to", p_json, fixed = TRUE))
  expect_match(p_num, "numbered 1 to 3")
  parsed <- .parse_items_json('{"items":["Item alpha wording", "Item beta wording", "Item gamma wording"]}', 3L, "A")
  expect_length(parsed, 3L)
})

test_that("contrastive construct alignment computes intended margins", {
  items <- rbind(i1 = c(1, 0), i2 = c(0, 1))
  constructs <- rbind(A = c(1, 0), B = c(0, 1))
  out <- semantica_contrastive_construct_alignment(items, c(i1 = "A", i2 = "B"), constructs)
  expect_s3_class(out, "semantica_construct_alignment")
  expect_true(all(out$item_diagnostics$contrastive_margin > 0))
})

test_that("semantic fingerprints and scale comparison are descriptive", {
  ids <- paste0("i", 1:4)
  m <- matrix(c(1,.8,.2,.1,.8,1,.3,.2,.2,.3,1,.7,.1,.2,.7,1),4,4,byrow=TRUE,dimnames=list(ids,ids))
  fp <- semantica_scale_fingerprint(m, setNames(c("A","A","B","B"), ids))
  expect_s3_class(fp, "semantica_scale_fingerprint")
  expect_true("semantic_separation" %in% names(fp$features))
  m2 <- m; m2[1,2] <- m2[2,1] <- .75
  cmp <- semantica_compare_scale_versions(m, m2)
  expect_s3_class(cmp, "semantica_scale_version_comparison")
  expect_equal(cmp$n_mapped_items, 4L)
  expect_gt(cmp$pairwise_spearman, .9)
})

test_that("cross-language equivalence and expert queue expose interpretation boundaries", {
  a <- rbind(i1=c(1,0), i2=c(0,1), i3=c(.7,.7))
  b <- a
  x <- semantica_cross_language_equivalence(a,b)
  expect_equal(x$mean_paired_cosine, 1, tolerance=1e-12)
  expect_match(x$note, "does not establish")
  m1 <- tcrossprod(.sem_norm_rows(a)); dimnames(m1) <- list(rownames(a),rownames(a))
  m2 <- m1; m2[1,2] <- m2[2,1] <- .4
  q <- semantica_expert_review_queue(list(one=m1,two=m2), top_n=2)
  expect_true(nrow(q) >= 1L)
  expect_equal(q$review_type[[1L]], "representation_disagreement")
})

test_that("coverage prompts and preregistration manifest are explicit scaffolds", {
  factors <- list(A=list(description="A", facets=list(one=list(), two=list())))
  bp <- semantica_construct_blueprint(factors)
  cov <- semantica_assess_construct_coverage(data.frame(item_id="a1",factor="A",Facet="one"), bp)
  prompts <- semantica_suggest_coverage_prompts(cov,bp,2L)
  expect_length(prompts,1L)
  expect_match(prompts[[1L]], "expert and empirical review")
  man <- semantica_preregistration_manifest(list(seed=123), inputs=data.frame(item="x"))
  expect_equal(man$schema,"semantica-preregistration-manifest-1")
  expect_match(man$interpretation_boundary,"does not")
})
