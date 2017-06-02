context("checkOSW2001_03")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkOSW2001_03Out.rda")
  checkOSW2001_03OutTest <- checkOSW2001_03(testData)
  expect_equal(checkOSW2001_03Out, checkOSW2001_03OutTest)
})
