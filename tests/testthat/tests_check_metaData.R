context("check_metaData")

test_that("Return values check", {
  data("exampleData", package = "sedReview")
  
  #check_metaDataOut <- check_metaData(exampleData)
  #saveRDS(check_metaDataOut,"data/check_metaDataOut.rds")
  
  check_metaDataOut <- readRDS("data/check_metaDataOut.rds")
  check_metaDataOutTest <- check_metaData(exampleData)
  expect_equal(check_metaDataOut, check_metaDataOutTest)
})
