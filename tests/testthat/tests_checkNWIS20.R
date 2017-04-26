context("checkNWIS20")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  load("data/checkNWIS20Out.rda")
  longTable <- exampleData$longTable
  checkNWIS20OutTest <- checkNWIS20(longTable)
  expect_equal(checkNWIS20Out, checkNWIS20OutTest)
})