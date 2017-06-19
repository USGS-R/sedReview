context("check_tss")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  check_tssOut <- readRDS("data/check_tssOut.rds")
  check_tssOutTest <- check_tss(exampleData)
  expect_equal(check_tssOut, check_tssOutTest)
})
