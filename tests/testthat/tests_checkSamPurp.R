context("checkSamPurp")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkSamPurpOut.rda")
  checkSamPurpOutTest <- checkSamPurp(testData)
  expect_equal(checkSamPurpOut, checkSamPurpOutTest)
})

