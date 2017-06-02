context("getSedOutliers")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/getSedOutliersOut.rda")
  getSedOutliersOutTest <- getSedOutliers(testData)
  expect_equal(getSedOutliersOut, getSedOutliersOutTest)
})
