context("check_bagIE")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_bagIEOut <- check_bagIE(exampleData)
  # saveRDS(check_bagIEOut, file = "tests/testthat/data/check_bagIEOut.rds")
  # check_bagIEOut <- readRDS("tests/testthat/data/check_bagIEOut.rds")
  
  check_bagIEOut <- readRDS("data/check_bagIEOut.rds")
  check_bagIEOutTest <- check_bagIE(exampleData)
  expect_equal(check_bagIEOut, check_bagIEOutTest)
})
