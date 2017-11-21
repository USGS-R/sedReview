context("check_verticals")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_verticalsOut <- check_verticals(exampleData)
  # saveRDS(check_verticalsOut, file = "tests/testthat/data/check_verticalsOut.rds")
  
  check_verticalsOut <- readRDS("data/check_verticalsOut.rds")
  check_verticalsOutTest <- check_verticals(exampleData)
  expect_equal(check_verticalsOut, check_verticalsOutTest)
})
