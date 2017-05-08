context("checkCalcBagIE")

test_that("Return values check", {
  data("testData", package = "sedReview")
  load("data/checkCalcBagIEOut.rda")
  checkCalcBagIEOutTest <- checkCalcBagIE(testData)
  expect_equal(checkCalcBagIEOut, checkCalcBagIEOutTest)
})
