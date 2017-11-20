context("count_methodsBySite")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  #count_methodsBySiteOut <- count_methodsBySite(exampleData)
  #saveRDS(count_methodsBySiteOut,"tests/testthat/data/count_methodsBySiteOut.rds")
  
  count_methodsBySiteOut <- readRDS("data/count_methodsBySiteOut.rds")
  count_methodsBySiteOutTest <- count_methodsBySite(exampleData)
  expect_equal(count_methodsBySiteOut, count_methodsBySiteOutTest)
})
