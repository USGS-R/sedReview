context("find_boxcoef")

test_that("Return values check", {
  data("exampleData2", package = "sedReview")
  
  # find_boxcoefOut <- find_boxcoef(exampleData2, site_no = "09163500")
  # saveRDS(find_boxcoefOut, "tests/testthat/data/find_boxcoefOut.rds")
  
  find_boxcoefOut <- readRDS("data/find_boxcoefOut.rds")
  find_boxcoefOutTest <- find_boxcoef(exampleData2, site_no = "09163500")
  expect_equal(find_boxcoefOut, find_boxcoefOutTest)
})
