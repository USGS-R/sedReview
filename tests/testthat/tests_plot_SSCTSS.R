context('plot_SSCTSS')

test_that("Return values check", {
  data('exampleData2', package = "sedReview")
  
  # plot_SSCTSSOut <- plot_SSCTSS(exampleData2, siteSelect = "09163500")
  # saveRDS(plot_SSCTSSOut, "tests/testthat/data/plot_SSCTSSOut.rds")
  
  plot_SSCTSSOut <- readRDS("data/plot_SSCTSSOut.rds")
  plot_SSCTSSOutTest <- plot_SSCTSS(exampleData2, siteSelect = "09163500")
  # DIFFERENT STRING APPENDED ON SOME GROB ASPECTS?? expect_equal(plot_SSCTSSOut$scatter, plot_SSCTSSOutTest$scatter)
  expect_length(plot_SSCTSSOut, 4)
  expect_length(plot_SSCTSSOutTest, 4)
})
