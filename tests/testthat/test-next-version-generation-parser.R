parse_case <- function(text, mode = "numbered", n = 3L) {
  suppressWarnings(SEMANTICA:::.semantica_parse_generation_response(
    text, n, "Factor", minimum_n = 1L, output_mode = mode
  ))
}

test_that("generation parser handles structured and fenced JSON", {
  plain <- parse_case('{"items":["Alpha item text","Beta item text","Gamma item text"]}', "json")
  fenced <- parse_case('```json\n{"items":["Alpha item text","Beta item text","Gamma item text"]}\n```', "json")
  expect_identical(plain$items, fenced$items)
  expect_identical(plain$metadata$retained, 3L)
})

test_that("generation parser normalizes numbered, bullet, CRLF, and one-line lists", {
  numbered <- parse_case("1. Alpha item text\n2. Beta item text\n3. Gamma item text")
  bullet <- parse_case("- Alpha item text\n- Beta item text\n- Gamma item text")
  crlf <- parse_case("1. Alpha item text\r\n2. Beta item text\r\n3. Gamma item text")
  one_line <- parse_case("1. Alpha item text 2. Beta item text 3. Gamma item text")
  expect_identical(numbered$items, bullet$items)
  expect_identical(crlf$items, numbered$items)
  expect_identical(one_line$items, numbered$items)
  expect_false(any(grepl("^n[0-9]+[.]", numbered$items)))
})

test_that("generation parser ignores prose around an actual list", {
  x <- parse_case("Here are the requested items:\n1. Alpha item text\n2. Beta item text\n3. Gamma item text\nThese should help your scale.")
  expect_identical(x$items, c("Alpha item text", "Beta item text", "Gamma item text"))
})

test_that("generation parser reports duplicates and short responses", {
  x <- parse_case("1. Alpha item text\n2. Alpha item text\n3. Beta item text")
  expect_lte(x$metadata$retained, 2L)
  expect_true("duplicates_removed" %in% x$metadata$rejection_reasons || x$metadata$parsed == 2L)

  short <- parse_case("1. Alpha item text", n = 3L)
  expect_true("fewer_than_requested" %in% short$metadata$rejection_reasons)
})

test_that("generation parser does not invent items from empty/refusal responses", {
  empty <- parse_case("")
  expect_length(empty$items, 0L)
  expect_true("empty_response" %in% empty$metadata$rejection_reasons)

  refusal <- parse_case("Sorry, I cannot provide those items.")
  expect_length(refusal$items, 0L)
  expect_true("refusal_or_non_item_response" %in% refusal$metadata$rejection_reasons)
})

test_that("generation parser supports malformed JSON fallback and unicode", {
  malformed <- parse_case("not-json\n1. Alpha item text\n2. Beta item text", "json", 2L)
  expect_identical(malformed$items, c("Alpha item text", "Beta item text"))

  unicode <- parse_case("1. Me siento capaz de actuar con autonomía.\n2. Puedo expresar mis ideas sin temor.\n3. 我能清楚表达自己的想法。")
  expect_length(unicode$items, 3L)
})

test_that("generation parser tolerates repeated numbering and quoted punctuation", {
  x <- parse_case("1. \"I can focus, even when distracted.\"\n1. 'I adapt when plans change.'\n2. I notice what's important.")
  expect_length(x$items, 3L)
})
