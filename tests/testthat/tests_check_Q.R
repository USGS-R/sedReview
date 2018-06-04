context("check_missingQ")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_missingQOut <- check_Q(exampleData)
  # saveRDS(check_missingQOut, file = "tests/testthat/data/check_missingQOut.rds")
  
  check_missingQOut <- readRDS("data/check_missingQOut.rds")
  check_missingQOutTest <- check_Q(exampleData)
  expect_equal(check_missingQOut, check_missingQOutTest)
})
