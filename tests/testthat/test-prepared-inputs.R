prepared_fixture <- function(scenario="C01") {
  stopifnot(identical(scenario,"C01"))
  readRDS(test_path("fixtures","benchmark-prepared-C01.rds"))
}

test_that("preparation source identity ignores metadata but detects code changes", {
  source_id <- SEMANTICA:::.semantica_preparation_source_id
  original <- source_id()
  wrap <- semantica_wrap
  expression <- body(wrap)
  metadata <- new.env(parent = emptyenv())
  attr(expression, "srcref") <- list(metadata)
  body(wrap) <- expression
  local_mocked_bindings(semantica_wrap = wrap, .package = "SEMANTICA")
  expect_identical(source_id(), original)
  metadata$runtime_state <- "changed after execution"
  expect_identical(source_id(), original)
  body(wrap) <- quote(stop("changed implementation"))
  local_mocked_bindings(semantica_wrap = wrap, .package = "SEMANTICA")
  expect_false(identical(source_id(), original))
  body(wrap) <- substitute({ VALUE }, list(VALUE = 1.2345678901234567))
  local_mocked_bindings(semantica_wrap = wrap, .package = "SEMANTICA")
  precise <- source_id()
  body(wrap) <- substitute({ VALUE }, list(VALUE = 1.2345678901234569))
  local_mocked_bindings(semantica_wrap = wrap, .package = "SEMANTICA")
  expect_false(identical(source_id(), precise))
})
test_that("prepared construction isolates metadata and rejects altered evidence", {
  x<-prepared_fixture();x$selection$items$gold_loading<-.99;x$selection$items$semantica_content_guard_pass<-TRUE
  p<-semantica_prepare_inputs(x$selection$items,x$selection$embeddings,x$task$factors,x$selection$references)
  expect_false("gold_loading" %in% names(p$items));expect_false("semantica_content_guard_pass" %in% names(p$items))
  altered<-p;altered$items$item_text[1]<-"changed"
  expect_error(SEMANTICA:::.semantica_validate_prepared(altered),"changed")
  altered<-x$selection$embeddings;rownames(altered)[1]<-"unknown"
  expect_error(semantica_prepare_inputs(x$selection$items,altered,x$task$factors,x$selection$references),"exactly")
  refs<-x$selection$references;refs$texts[1]<-"changed definition"
  expect_error(semantica_prepare_inputs(x$selection$items,x$selection$embeddings,x$task$factors,refs),"reference|alignment|match")
  local_mocked_bindings(.call_chat=function(...) stop("PROHIBITED HTTP"),semantica_embed=function(...) stop("PROHIBITED HTTP"),.package="SEMANTICA")
  g<-SEMANTICA:::.semantica_prepared_generation(p,p$factors,p$compute)
  expect_true("semantica_content_guard_pass" %in% names(g$df))
})
test_that("native live and prepared paths agree for passing and rejecting pools", {
  x<-prepared_fixture();items<-x$selection$items;vectors<-x$selection$embeddings;refs<-x$selection$references
  provider_calls<-0L
  local_mocked_bindings(
    semantica_connect=function(...) structure(list(embed_model="recorded-vectors",protocol="offline"),class="semantica_session"),
    semantica_generate_items=function(...) items,
    semantica_embed=function(items_tbl,...,id_col="item_id",text_col="item_text") {
      provider_calls<<-provider_calls+1L
      ids<-as.character(items_tbl[[id_col]])
      list(embeddings=if(identical(id_col,"ref_id")) refs$vectors[ids,,drop=FALSE] else vectors[ids,,drop=FALSE])
    },.call_chat=function(...) stop("PROHIBITED HTTP"),.package="SEMANTICA")
  for(workers in c(1L,2L)) {
    p<-semantica_prepare_inputs(items,vectors,x$task$factors,refs)
    args<-x$arguments
    if(workers==2L) args$calibration_rng_state<-x$calibration_state
    args$resources<-semantica_resource_config(cpu_cores=workers);args$seed<-401L;args$checkpoint_dir<-NULL
    live<-suppressWarnings(do.call(semantica_run_custom,args))$advanced
    before<-provider_calls;args$prepared_inputs<-p
    saved<-suppressWarnings(do.call(semantica_run_custom,args))$advanced
    expect_identical(provider_calls,before)
    expect_identical(saved$best_items,live$best_items)
    expect_equal(saved$structural_guard,live$structural_guard,tolerance=1e-10)
    expect_equal(saved$optimization$search_calibration,live$optimization$search_calibration,tolerance=1e-10)
    expect_true(isTRUE(saved$objective_context$final_esem_admissible))
    expect_identical(saved$optimization$reproducibility$calibration_rng_entry_state,args$calibration_rng_state)
  }
  # Fewer candidates than selected positions is an unambiguous rejection.
  keep<-items$factor!="F01" | seq_len(nrow(items))==1L
  items<-items[keep,];vectors<-vectors[items$item_id,,drop=FALSE]
  p<-suppressWarnings(semantica_prepare_inputs(items,vectors,x$task$factors,refs))
  args$prepared_inputs<-NULL
  a<-tryCatch(suppressWarnings(do.call(semantica_run_custom,args)),error=identity)
  args$prepared_inputs<-p;before<-provider_calls
  b<-tryCatch(suppressWarnings(do.call(semantica_run_custom,args)),error=identity)
  expect_s3_class(a,"error");expect_s3_class(b,"error")
  if(inherits(a,"error") && inherits(b,"error")) {
    # Origin is deliberately explicit; all counts and scientific reasons agree.
    expect_match(conditionMessage(a),"Top-up status: disabled[.]$")
    expect_match(conditionMessage(b),"Top-up status: immutable_prepared_pool[.]$")
    expect_identical(sub("Top-up status: .*","",conditionMessage(a)),sub("Top-up status: .*","",conditionMessage(b)))
  }
  expect_identical(provider_calls,before)
})
test_that("successful generation is checkpointed before an embedding failure", {
  x<-prepared_fixture();saved<-NULL
  local_mocked_bindings(
    semantica_connect=function(...) structure(list(embed_model="offline"),class="semantica_session"),
    semantica_generate_items=function(...) x$selection$items,
    semantica_embed=function(...) stop("injected embedding failure"),.package="SEMANTICA")
  expect_error(semantica_pipeline(backend="ollama",scale_name="offline",scale_description="offline",
    factors=x$task$factors,n_per_factor=40L,pool_topup=FALSE,verbose=FALSE,
    generation_checkpoint=function(items,metadata) saved<<-list(items=items,metadata=metadata)),"injected embedding failure")
  expect_identical(saved$items$item_text,x$selection$items$item_text)
  path<-tempfile();saveRDS(saved,path);expect_identical(readRDS(path),saved)
})
