context("find_outliers")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  #find_outliersOut <- find_outliers(exampleData)
  #saveRDS(find_outliersOut,"tests/testthat/data/find_outliersOut.rds")
  
  find_outliersOut <- readRDS("data/find_outliersOut.rds")
  find_outliersOutTest <- find_outliers(exampleData)
  expect_equal(find_outliersOut, find_outliersOutTest)
})
