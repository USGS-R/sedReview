context("check_qaqcDB")

test_that("Return values check", {
  data("exampleData2", package = "sedReview")
  
  # check_qaqcDBOut <- check_qaqcDB(exampleData2)
  # saveRDS(check_qaqcDBOut, "tests/testthat/data/check_qaqcDBOut.rds")
  # check_qaqcDBOut <- readRDS("tests/testthat/data/check_qaqcDBOut.rds")
  
  check_qaqcDBOut <- readRDS("data/check_qaqcDBOut.rds")
  check_qaqcDBOutTest <- check_qaqcDB(exampleData2)
  expect_equal(check_qaqcDBOut, check_qaqcDBOutTest)
})
