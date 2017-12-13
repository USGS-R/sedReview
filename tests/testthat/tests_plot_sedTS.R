context('plot_sedTS')

test_that("Return values check", {
  data('exampleData', package = "sedReview")
  
  # plot_sedTSOut <- plot_sedTS(exampleData, siteSelect = "05586300")
  # saveRDS(plot_sedTSOut, "tests/testthat/data/plot_sedTSOut.rds")
  
  plot_sedTSOut <- readRDS("data/plot_sedTSOut.rds")
  plot_sedTSOutTest <- plot_sedTS(exampleData, siteSelect = "05586300")
  expect_equal(plot_sedTSOut, plot_sedTSOutTest)
})
