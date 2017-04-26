context("checkSamplerTyp")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/checkSamplerTypOut.rda")
  longTable <- exampleData$longTable
  checkSamplerTypOutTest <- checkSamplerTyp(longTable)
  expect_equal(checkSamplerTypOut, checkSamplerTypOutTest)
})