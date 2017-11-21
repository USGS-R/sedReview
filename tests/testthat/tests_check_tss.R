context("check_tss")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_tssOut <- check_tss(exampleData)
  # saveRDS(check_tssOut, file = "tests/testthat/data/check_tssOut.rds")
  
  check_tssOut <- readRDS("data/check_tssOut.rds")
  check_tssOutTest <- check_tss(exampleData)
  expect_equal(check_tssOut, check_tssOutTest)
})
