context("calc_summaryStats")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # calc_summaryStatsOut <- calc_summaryStats(exampleData)
  # saveRDS(calc_summaryStatsOut,"tests/testthat/data/calc_summaryStatsOut.rds")
  
  calc_summaryStatsOut <- readRDS("data/calc_summaryStatsOut.rds")
  calc_summaryStatsOutTest <- calc_summaryStats(exampleData)
  expect_equal(calc_summaryStatsOut, calc_summaryStatsOutTest)
})
