context('plot_sedFlow')

test_that("Return values check", {
  data('exampleData', package = "sedReview")
  
  # plot_sedFlowOut <- plot_sedFlow(exampleData, siteSelect = "06934500")
  # saveRDS(plot_sedFlowOut, "tests/testthat/data/plot_sedFlowOut.rds")
  
  plot_sedFlowOut <- readRDS("data/plot_sedFlowOut.rds")
  plot_sedFlowOutTest <- plot_sedFlow(exampleData, siteSelect = "06934500")
  expect_equal(plot_sedFlowOut, plot_sedFlowOutTest)
})
