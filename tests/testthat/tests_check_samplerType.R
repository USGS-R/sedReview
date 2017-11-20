context("check_samplerType")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  #check_samplerTypeOut <- check_samplerType(exampleData)
  #saveRDS(check_samplerTypeOut,"tests/testthat/data/check_samplerTypeOut.rds")
  
  check_samplerTypeOut <- readRDS("data/check_samplerTypeOut.rds")
  check_samplerTypeOutTest <- check_samplerType(exampleData)
  expect_equal(check_samplerTypeOut, check_samplerTypeOutTest)
})
