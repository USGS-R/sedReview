context("check_commentsNoResults")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_commentsNoResultOut <- check_commentsNoResult(exampleData)
  # saveRDS(check_commentsNoResultOut, "tests/testthat/data/check_commentsNoResultOut.rds")
  # 
  # check_commentsNoResultOut <- readRDS("tests/testthat/data/check_commentsNoResultOut.rds")
  
  check_commentsNoResultOut <- readRDS("data/check_commentsNoResultOut.rds")
  check_commentsNoResultOutTest <- check_commentsNoResult(exampleData)
  expect_equal(check_commentsNoResultOut, check_commentsNoResultOutTest)
})