context("checkOSW2001_03")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/checkOSW2001_03Out.rda")
  longTable <- exampleData$longTable
  checkOSW2001_03OutTest <- checkOSW2001_03(longTable)
  expect_equal(checkOSW2001_03Out, checkOSW2001_03OutTest)
})
