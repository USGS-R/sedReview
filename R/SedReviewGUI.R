#' Function to open the sedReview GUI
#' @examples
#' \dontrun{ 
#' SedReviewGUI()
#' }
#' @export
#' @import shiny
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
SedReviewGUI <- function() {
  appDir <- system.file("shiny","SedReviewGUI",package = "sedReview")
  if (appDir == "") {
    stop("Could not find GUI directory. Try re-installing `sedReview`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}