context("countMethodsBySite")

test_that("Return values check", {
  ###load the data upfront#
  data("testData", package = "sedReview")
  load("data/countMethodsBySiteOut.rda")
  countMethodsBySiteOutTest <- countMethodsBySite(testData)
  expect_equal(countMethodsBySiteOut,countMethodsBySiteOutTest)
})

