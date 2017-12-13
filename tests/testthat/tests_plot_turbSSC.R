context('plot_turbSSC')

test_that("Return values check", {
  data('exampleData2', package = "sedReview")
  
  # plot_turbSSC <- plot_sedTS(exampleData2, siteSelect = "09163500")
  # saveRDS(plot_turbSSC, "tests/testthat/data/plot_turbSSC.rds")
  
  plot_turbSSC <- readRDS("data/plot_turbSSC.rds")
  plot_turbSSCTest <- plot_sedTS(exampleData2, siteSelect = "09163500")
  expect_equal(plot_turbSSC, plot_turbSSCTest)
})
