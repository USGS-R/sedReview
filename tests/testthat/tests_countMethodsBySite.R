context("countBySite")

test_that("Return values check", {
  ###load the data upfront#
  data("exampleData",package="sedReview")
  load("data/countBySiteOut.rda")
  longTable <- exampleData$longTable
  countBySiteOutTest <- countMethodsBySite(longTable)
  expect_equal(countBySiteOut,countBySiteOutTest)
})

