#' Function to open the sedReview GUI
#' @examples
#' \dontrun{ 
#' SedReviewGUI()
#' }
#' @export
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @import shiny
#' @import shinyBS
#' @import ggplot2
#' @import gghighlight
#' @import dplyr
#' @import stringr
#' @import DT
#' @import broom
#' @import smwrBase
#' @import dataRetrieval
#' @import leaflet
#' @import shinycssloaders
#' @import plotly
SedReviewGUI <- function() {
  appDir <- system.file("shiny","SedReviewGUI",package = "sedReview")
  if (appDir == "") {
    stop("Could not find GUI directory. Try re-installing `sedReview`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}