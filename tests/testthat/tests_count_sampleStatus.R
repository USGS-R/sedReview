context("count_sampleStatus")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  # count_sampleStatusOut <- count_sampleStatus(exampleData)
  # saveRDS(count_sampleStatusOut,"tests/testthat/data/count_sampleStatusOut.rds")

  count_sampleStatusOut <- readRDS("data/count_sampleStatusOut.rds")
  count_sampleStatusOutTest <- count_sampleStatus(exampleData)
  expect_equal(count_sampleStatusOut, count_sampleStatusOutTest)
})
