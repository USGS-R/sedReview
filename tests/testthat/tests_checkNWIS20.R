context("checkNWIS20")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkNWIS20Out.rda")
  checkNWIS20OutTest <- checkNWIS20(testData)
  expect_equal(checkNWIS20Out, checkNWIS20OutTest)
})