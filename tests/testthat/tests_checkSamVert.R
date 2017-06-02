context("checkSamVert")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkSamVertOut.rda")
  checkSamVertOutTest <- checkSamVert(testData)
  expect_equal(checkSamVertOut, checkSamVertOutTest)
})

