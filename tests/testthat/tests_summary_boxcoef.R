context("summary_boxcoef")

test_that("Return values check", {
  data("exampleData2", package = "sedReview")
  
  # summary_boxcoefOut <- summary_boxcoef(exampleData2)
  # saveRDS(summary_boxcoefOut, "tests/testthat/data/summary_boxcoefOut.rds")
  
  summary_boxcoefOut <- readRDS("data/summary_boxcoefOut.rds")
  summary_boxcoefOutTest <- summary_boxcoef(exampleData2)
  expect_equal(summary_boxcoefOut, summary_boxcoefOutTest)
})
