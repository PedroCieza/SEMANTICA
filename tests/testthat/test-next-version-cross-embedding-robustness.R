test_that("cross-embedding robustness aligns identical item sets by explicit IDs", {
  m1 <- matrix(c(1,.8,.2,.8,1,.3,.2,.3,1), 3, 3,
               dimnames = list(c("a","b","c"), c("a","b","c")))
  m2 <- m1[c("c","a","b"), c("c","a","b")]
  out <- semantica_semantic_robustness(list(one=m1, two=m2))
  expect_equal(out$common_items, c("a","b","c"))
  expect_equal(out$matrix_rank_agreement[1,2], 1)
  expect_equal(out$high_similarity_pair_jaccard[1,2], 1)
  expect_false(out$participant_based)
})

test_that("strict cross-embedding comparison rejects missing or extra item IDs", {
  m1 <- diag(4); dimnames(m1) <- list(letters[1:4], letters[1:4])
  m2 <- diag(4); dimnames(m2) <- list(letters[2:5], letters[2:5])
  expect_error(semantica_semantic_robustness(list(a=m1,b=m2)), "Item-ID alignment mismatch")
  expect_warning(out <- semantica_semantic_robustness(list(a=m1,b=m2), item_id_policy="intersection"), "intersection")
  expect_equal(out$common_items, letters[2:4])
})

test_that("monotonic similarity transforms preserve rank and top-pair agreement", {
  set.seed(3)
  x <- matrix(runif(25),5,5); x <- (x+t(x))/2; diag(x)<-1
  dimnames(x)<-list(paste0("i",1:5),paste0("i",1:5))
  y <- exp(x)
  out <- semantica_semantic_robustness(list(raw=x, transformed=y), high_pair_quantile=.8)
  expect_equal(out$matrix_rank_agreement[1,2], 1, tolerance=1e-12)
  expect_equal(out$high_similarity_pair_jaccard[1,2], 1)
})

test_that("compatible SEMANTICA result objects are accepted directly", {
  m <- diag(3); dimnames(m)<-list(c("a","b","c"),c("a","b","c")); m[lower.tri(m)] <- c(.2,.3,.8); m[upper.tri(m)] <- t(m)[upper.tri(m)]
  r1 <- list(cosine_sim_matrix=m, best_items=c("a","b"))
  r2 <- list(cosine_sim_matrix=m[c("b","c","a"),c("b","c","a")], best_items=c("a","c"))
  out <- semantica_semantic_robustness(list(r1=r1,r2=r2))
  expect_true(is.matrix(out$selection_jaccard))
  expect_false(out$consensus_matrix_constructed)
})
