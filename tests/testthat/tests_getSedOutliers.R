context("getSedOutliers")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/getSedOutliersOut.rda")
  longTable <- exampleData$longTable
  getSedOutliersOutTest <- getSedOutliers(longTable)
  expect_equal(getSedOutliersOut, getSedOutliersOutTest)
})