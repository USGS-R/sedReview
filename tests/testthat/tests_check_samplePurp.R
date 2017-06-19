context("check_samplePurp")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  check_samplePurpOut <- readRDS("data/check_samplePurpOut.rds")
  check_samplePurpOutTest <- check_samplePurp(exampleData)
  expect_equal(check_samplePurpOut, check_samplePurpOutTest)
})
