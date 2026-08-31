test_that("embedding policy is model-aware rather than globally hard-coded", {
  p_nomic <- SEMANTICA:::.semantica_embedding_policy("nomic-embed-text", "auto")
  expect_equal(p_nomic$resolved_task, "clustering")
  expect_equal(p_nomic$prefix, "clustering: ")
  p_generic <- SEMANTICA:::.semantica_embedding_policy("text-embedding-3-small", "auto")
  expect_null(p_generic$prefix)
})

test_that("embedding preparation does not double-prefix instructed text", {
  s <- structure(list(embed_model="nomic-embed-text", embedding_task="auto", embedding_instruction=NULL), class=c("semantica_session","list"))
  x <- SEMANTICA:::.semantica_prepare_embedding_texts(s, c("alpha", "clustering: beta"))
  expect_equal(unname(x), c("clustering: alpha", "clustering: beta"))
})

test_that("embedding-only session does not invent a required chat model", {
  s <- semantica_connect("ollama", embed_model="nomic-embed-text", purpose="embed", preflight=FALSE, verbose=FALSE)
  expect_null(s$chat_model)
  expect_equal(s$purpose, "embed")
})

test_that("semantic facet coverage is separated from metadata coverage", {
  factors <- list(A=list(description="Factor A", facets=list(x=list(description="facet x"), y=list(description="facet y"))))
  bp <- semantica_construct_blueprint(factors)
  d <- data.frame(item_id=c("i1","i2"), factor="A", Facet=c("x","y"),
                  semantica_facet_aligned=c(TRUE,FALSE), stringsAsFactors=FALSE)
  z <- semantica_assess_construct_coverage(d,bp)
  expect_equal(z$metadata_overall_coverage,1)
  expect_equal(z$semantic_overall_coverage,0.5)
  expect_equal(z$overall_required_facet_coverage,0.5)
})

test_that("cache keys depend on embedding instruction policy", {
  s1 <- list(backend="ollama",protocol="ollama",embed_model="nomic-embed-text",embedding_task="auto",embedding_instruction=NULL)
  s2 <- s1; s2$embedding_instruction <- "classification: "
  k1 <- SEMANTICA:::.semantica_text_cache_key("same text",s1)
  k2 <- SEMANTICA:::.semantica_text_cache_key("same text",s2)
  expect_false(identical(k1,k2))
})

test_that("legacy construct coverage falls back to metadata when semantic alignment is absent", {
  factors <- list(A = list(description = "Factor A", facets = list(x = list(description = "facet x"))))
  bp <- semantica_construct_blueprint(factors)
  d <- data.frame(item_id = "i1", factor = "A", Facet = "x", stringsAsFactors = FALSE)
  z <- semantica_assess_construct_coverage(d, bp)
  expect_false(z$semantic_alignment_available)
  expect_equal(z$metadata_overall_coverage, 1)
  expect_true(is.na(z$semantic_overall_coverage))
  expect_equal(z$overall_required_facet_coverage, 1)
  expect_identical(z$coverage_table, z$table)
})

test_that("quality defaults are conservative across arbitrary wording", {
  q <- semantica_quality_config()
  expect_equal(q$content_alignment_mode, "diagnostic")
  # Negation is not synonymous with reverse-keying, so automatic removal is
  # deliberately not the default.
  expect_equal(q$polarity_action, "diagnostic")
})

test_that("direct ACO defaults match full-pipeline QA defaults", {
  f <- formals(ACO_with_ESEM)
  expect_identical(eval(f$content_alignment_mode), c("diagnostic", "guard", "off"))
  expect_identical(eval(f$polarity_action), c("diagnostic", "guard", "off"))
})

test_that("explicit embedding instruction overrides model-card policy", {
  p <- SEMANTICA:::.semantica_embedding_policy(
    "nomic-embed-text",
    task = "auto",
    instruction = "classification: "
  )
  expect_equal(p$prefix, "classification: ")
  expect_equal(p$source, "user")
})

test_that("cosine diagnostics no longer apply a universal anisotropy cutoff", {
  m <- matrix(c(
    1, .7, .6,
    .7, 1, .65,
    .6, .65, 1
  ), 3, 3, byrow = TRUE)
  d <- SEMANTICA:::.cosine_diagnostics(m)
  expect_true(is.finite(d$common_direction_strength))
  expect_gte(d$common_direction_strength, 0)
  expect_lte(d$common_direction_strength, 1)
  expect_true(is.na(d$possible_anisotropy))
  expect_match(d$possible_anisotropy_note, "No universal anisotropy cutoff")
})

test_that("unknown embedding models remain unmodified under auto policy", {
  s <- structure(
    list(embed_model = "vendor/new-general-embedding-model", embedding_task = "auto", embedding_instruction = NULL),
    class = c("semantica_session", "list")
  )
  x <- SEMANTICA:::.semantica_prepare_embedding_texts(s, "plain item text")
  expect_equal(unname(x), "plain item text")
})

test_that("semantic facet coverage is factor-specific when definitions are only partially evaluable", {
  factors <- list(
    A = list(description = "Factor A", facets = list(x = list(description = "facet x"))),
    B = list(description = "Factor B", facets = list(y = list()))
  )
  bp <- semantica_construct_blueprint(factors)
  d <- data.frame(
    item_id = c("i1", "i2"),
    factor = c("A", "B"),
    Facet = c("x", "y"),
    semantica_facet_aligned = c(TRUE, NA),
    stringsAsFactors = FALSE
  )
  z <- semantica_assess_construct_coverage(d, bp)
  a <- z$table[z$table$factor == "A", ]
  b <- z$table[z$table$factor == "B", ]
  expect_true(a$semantic_alignment_available)
  expect_false(b$semantic_alignment_available)
  expect_equal(a$semantic_coverage, 1)
  expect_true(is.na(b$semantic_coverage))
  expect_equal(b$coverage, 1) # metadata fallback for the unevaluable factor
  expect_true(z$semantic_alignment_available)
  expect_false(z$semantic_alignment_complete)
})

test_that("recognized pre-existing task prefixes are normalized within a session", {
  s <- structure(
    list(embed_model = "nomic-embed-text", embedding_task = "auto", embedding_instruction = NULL),
    class = c("semantica_session", "list")
  )
  x <- SEMANTICA:::.semantica_prepare_embedding_texts(
    s,
    c("search_query: alpha", "classification: beta", "clustering: gamma")
  )
  expect_equal(
    unname(x),
    c("clustering: alpha", "clustering: beta", "clustering: gamma")
  )
})

test_that("cache keys coalesce equivalent normalized task-prefixed text", {
  s <- list(
    backend = "ollama", protocol = "ollama", embed_model = "nomic-embed-text",
    embedding_task = "auto", embedding_instruction = NULL
  )
  k1 <- SEMANTICA:::.semantica_text_cache_key("alpha", s)
  k2 <- SEMANTICA:::.semantica_text_cache_key("search_query: alpha", s)
  expect_identical(k1, k2)
})
