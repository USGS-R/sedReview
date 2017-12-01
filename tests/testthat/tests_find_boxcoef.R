context("find_boxcoef")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # find_boxcoefOut <- find_boxcoef(x, site_no = "05586300")
  # saveRDS(find_boxcoefOut, "tests/testthat/data/find_boxcoefOut.rds")
  
  find_boxcoefOut <- readRDS("data/find_boxcoefOut.rds")
  find_boxcoefOutTest <- find_boxcoef(x, site_no = "05586300")
  expect_equal(find_boxcoefOut, find_boxcoefOutTest)
})
