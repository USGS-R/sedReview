context(find_provisional)

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # find_provisionalOut <- find_provisional(exampleData, view = FALSE)
  # saveRDS(find_provisionalOut, file = "tests/testthat/data/find_provisionalOut.rds")
  
  find_provisionalOut <- readRDS("data/find_provisionalOut.rds")
  find_provisionalOutTest <- find_provisional(exampleData, view = FALSE)
  expect_equal(find_provisionalOut, find_provisionalOutTest)
})
