test_that("empirical calibration accepts frozen group exclusions and fingerprints them", {
  make<-function(id) {
    m<-matrix(.2,6,6);diag(m)<-1;m[1,2]<-m[2,1]<-.4
    dimnames(m)<-list(paste0("i",1:6),paste0("i",1:6))
    r<-.7*m;diag(r)<-1
    list(semantic_matrix=m,response_matrix=r,item_ids=paste0(id,"_",1:6),
      paraphrase_groups=paste0(id,"_",1:6),respondent_ids=paste0(id,"_r",1:50))
  }
  data<-list(A=make("A"),B=make("B"),C=make("C"))
  splits<-list(A="C",B="C",C=c("A","B"))
  x<-suppressWarnings(semantica_leave_one_scale_out_calibration(data,method="linear",splits=splits))
  expect_identical(x$per_scale$training_scales,c("C","C","A | B"))
  expect_true(all(x$split_manifest$overlap_status=="declared_disjoint"))
  expect_equal(x$macro_scale$rmse,mean(x$per_scale$rmse))
  y<-suppressWarnings(semantica_leave_one_scale_out_calibration(data,method="linear"))
  expect_false(identical(x$split_fingerprint,y$split_fingerprint))
  bad<-splits;bad$A<-c("A","B")
  expect_error(semantica_leave_one_scale_out_calibration(data,splits=bad),"Invalid training")
  data$C$item_ids<-data$A$item_ids
  expect_error(semantica_leave_one_scale_out_calibration(data,splits=splits),"shared items")
})
