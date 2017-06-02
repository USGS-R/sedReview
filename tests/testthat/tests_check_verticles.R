context("check_verticles")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  #check_verticlesOut <- check_verticles(exampleData)
  #saveRDS(check_verticlesOut,"data/check_verticlesOut.rds")
  
  check_verticlesOut <- readRDS("data/check_verticlesOut.rds")
  check_verticlesOutTest <- check_verticles(exampleData)
  expect_equal(check_verticlesOut, check_verticlesOutTest)
})
