context("make_wideTable")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # make_wideTableOut <- make_wideTable(exampleData)
  # saveRDS(make_wideTableOut,"tests/testthat/data/make_wideTableOut.rds")
  
  make_wideTableOut <- readRDS("data/make_wideTableOut.rds")
  make_wideTableOutTest <- make_wideTable(exampleData)
  expect_equal(make_wideTableOut, make_wideTableOutTest)
})
