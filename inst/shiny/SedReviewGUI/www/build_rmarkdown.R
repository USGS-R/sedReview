rmarkdown::render('inst/shiny/SedReviewGUI/www/sedReview_manual.Rmd',
                  output_format = 'github_document',
                  output_file = 'sedReview_manual.md')
#note, did not put in path to screenshots, other weird formatting issues that don't match the HTML document launched from GUI
rmarkdown::render('inst/shiny/SedReviewGUI/www/sedReview_manual.Rmd',
                  output_format = 'html_document',
                  output_file = 'sedReview_manual.html')
