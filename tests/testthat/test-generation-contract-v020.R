test_that("JSON factor prompt and system prompt agree", {
  sys <- .build_system_prompt("Scale", "Description", "Likert", "brief", "English", output_mode = "json")
  usr <- .build_factor_prompt("Factor", "Definition", 4L, output_mode = "json")
  expect_match(sys, "Return ONLY valid JSON")
  expect_match(usr, "Output valid JSON only")
  expect_false(grepl("numbered 1 to", usr, fixed = TRUE))
})

test_that("numbered generation remains backward compatible", {
  usr <- .build_factor_prompt("Factor", "Definition", 4L, output_mode = "numbered")
  expect_match(usr, "numbered 1 to 4")
})
