context("check_sedMass")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # check_sedMassOut <- check_sedMass(exampleData)
  # saveRDS(check_sedMassOut, file = "tests/testthat/data/check_sedMassOut.rds")
  
  check_sedMassOut <- readRDS("data/check_sedMassOut.rds")
  check_sedMassOutTest <- check_sedMass(exampleData)
  expect_equal(check_sedMassOut, check_sedMassOutTest)
})
