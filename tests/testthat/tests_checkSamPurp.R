context("checkSamPurp")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/checkSamPurpOut.rda")
  longTable <- exampleData$longTable
  checkSamPurpOutTest <- checkSamPurp(longTable)
  expect_equal(checkSamPurpOut, checkSamPurpOutTest)
})