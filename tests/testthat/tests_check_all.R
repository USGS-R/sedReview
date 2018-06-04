context("check_all")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_allOut <- check_all(exampleData, returnAllTables = TRUE)
  # saveRDS(check_allOut, "tests/testthat/data/check_allOut.rds")
  # 
  # check_allOut <- readRDS("tests/testthat/data/check_allOut.rds")

  check_allOut <- readRDS("data/check_allOut.rds")
  check_allOutTest <- check_all(exampleData, returnAllTables = TRUE)
  expect_equal(check_allOut, check_allOutTest)
})
