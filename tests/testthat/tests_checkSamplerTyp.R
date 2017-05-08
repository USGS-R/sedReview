context("checkSamplerTyp")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkSamplerTypOut.rda")
  checkSamplerTypOutTest <- checkSamplerTyp(testData)
  expect_equal(checkSamplerTypOut, checkSamplerTypOutTest)
})
