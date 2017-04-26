context("checkSamVert")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/checkSamVertOut.rda")
  longTable <- exampleData$longTable
  checkSamVertOutTest <- checkSamVert(longTable)
  expect_equal(checkSamVertOut, checkSamVertOutTest)
})