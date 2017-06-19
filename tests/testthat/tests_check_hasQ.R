context("check_hasQ")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  check_hasQOut <- readRDS("data/check_hasQOut.rds")
  check_hasQOutTest <- check_hasQ(exampleData)
  expect_equal(check_hasQOut, check_hasQOutTest)
})
