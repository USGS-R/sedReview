context("calc_concSandFiine")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # calc_concSandFineOut <- calc_concSandFine(exampleData)
  # saveRDS(calc_concSandFineOut, "tests/testthat/data/calc_concSandFineOut.rds")
  # 
  # calc_concSandFineOut <- readRDS("tests/testthat/data/calc_concSandFineOut.rds")
  
  calc_concSandFineOut <- readRDS("data/calc_concSandFineOut.rds")
  calc_concSandFineOutTest <- calc_concSandFine(exampleData)
  expect_equal(calc_concSandFineOut, calc_concSandFineOutTest)
})