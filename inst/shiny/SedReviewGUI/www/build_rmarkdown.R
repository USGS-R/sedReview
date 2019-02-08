rmarkdown::render('inst/shiny/SedReviewGUI/www/sedReview_manual.Rmd',
                  output_format = 'github_document',
                  output_file = 'sedReview_manual.md')
rmarkdown::render('inst/shiny/SedReviewGUI/www/sedReview_manual.Rmd',
                  output_format = 'html_document',
                  output_file = 'sedReview_manual.html')
