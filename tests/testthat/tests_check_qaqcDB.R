context("check_qaqcDB")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_qaqcDBOut <- check_qaqcDB(exampleData)
  # saveRDS(check_qaqcDBOut, "tests/testthat/data/check_qaqcDBOut.rds")
  # check_qaqcDBOut <- readRDS("tests/testthat/data/check_bagIEOut.rds")
  
  check_qaqcDBOut <- readRDS("data/check_qaqcDBOut.rds")
  check_qaqcDBOutTest <- check_qaqcDB(exampleData)
  expect_equal(check_qaqcDBOut, check_qaqcDBOutTest)
})