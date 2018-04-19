#' plot_sedTS. Create timeseries plots for sediment parameters.
#' 
#' @description Function to output timeseries plots for sediment parameters. Output is a list of plots or write to PDF.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param log.P80154 Logical, if \code{TRUE}, SSC y axis will be log10. Default is \code{FALSE}.
#' @param log.P70331 Logical, if \code{TRUE}, sand/silt break y axis will be log10. Default is \code{FALSE}.
#' @param log.P80155 Logical, if \code{TRUE}, SSL y axis will be log10. Default is \code{FALSE}.
#' @param log.P80225 Logical, if \code{TRUE}, bedload y axis will be log10. Default is \code{FALSE}.
#' @param log.P91145 Logical, if \code{TRUE}, bedload mass y axis will be log10. Default is \code{FALSE}.
#' @param siteSelect Character, site number to create plots for if \code{x} contains multiple sites. Default is \code{NULL}.
#' @param PDFout Character. File or full path name of file for plots. If \code{NULL}, the default, a list of the plots will be returned in R instead.
#' @details Timeseries plots of SSC (P80154), sand/silt break (P70331), SSL (P80155), bedload (P80255) and bedload mass (P91145).
#' Not all parameters need to be present in \code{x} for plots to be created (eg. if only SSC present than only SSC timeseries plot returned)
#' @details If PDFout is not specified, than a list of the plots is returned. Plots (if applicable) are specified above. See example for more details.
#' @details Portions of code modified from \code{WQReview::qwtsPlot}.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' sedTS <- plot_sedTS(exampleData, siteSelect = "05586300")
#' \dontrun{
#' # view plot in Rstudio
#' sedTS$SSC
#' 
#' # output to file on D drive
#' plot_sedTS(exampleData, siteSelect = "05586300", PDFout = "D:/timeseries.pdf")
#' }
#' 
#' @import ggplot2
#' @export
#' @return If \code{PDFout = NULL}, list containing ggplot elements. If \code{PDFout} specified, a PDF document containing the plots.

plot_sedTS <- function(x,
                       log.P80154 = FALSE,
                       log.P70331 = FALSE,
                       log.P80155 = FALSE,
                       log.P80225 = FALSE,
                       log.P91145 = FALSE,
                       siteSelect = NULL,
                       PDFout = NULL){
  # sediment pcodes to plot
  pcodes <- c('80154','70331','80155','80225','91145')
  
  # subset data
  if(is.null(siteSelect)){
    x <- x[x$PARM_CD %in% pcodes,]
  }else{x <- x[x$SITE_NO %in% siteSelect & x$PARM_CD %in% pcodes,]}
  
  # if more than 1 site in file, quit
  if(length(unique(x$SITE_NO)) > 1){
    stop("More than one site in input dataframe x. Subset data to one site_no with siteSelect.")
  }
  
  # internal plotting function
  plotfun <- function(y, ylabel, ptcolor){
    p1 <- ggplot(data = y, aes(x = SAMPLE_START_DT, y = RESULT_VA))
    p1 <- p1 + geom_point(size = 3, color = ptcolor)
    p1 <- p1 + xlab("Date") + ylab(paste(ylabel,"\n")) + labs(caption = paste(unique(y$PARM_CD),unique(y$PARM_DS)))
    p1 <- p1 + labs(title = paste(unique(y$SITE_NO),"\n",unique(y$STATION_NM)))
    return(p1)
  }
  
  # create plots
  if(nrow(x[x$PARM_CD == '80154',]) > 0){
    sscPlot <- plotfun(y = x[x$PARM_CD == '80154',], ylabel = 'Concentration (mg/L)', ptcolor = 'blue')
    if(log.P80154 == TRUE){
      sscPlot <- sscPlot + scale_y_log10()
    }
  }else{warning("No SSC data to plot")}
  
  if(nrow(x[x$PARM_CD == '70331',]) > 0){
    ssbreakPlot <- plotfun(y = x[x$PARM_CD == '70331',], ylabel = 'Percent (%)', ptcolor = 'tan')
    if(log.P70331 == TRUE){
      ssbreakPlot <- ssbreakPlot + scale_y_log10()
    }
  }else{warning("No Sand/Silt break data to plot")}
  
  if(nrow(x[x$PARM_CD == '80155',]) > 0){
    sslPlot <- plotfun(y = x[x$PARM_CD == '80155',], ylabel = 'Discharge (tons/day)', ptcolor = 'brown')
    if(log.P80155 == TRUE){
      sslPlot <- sslPlot + scale_y_log10()
    }
  }else{warning("No SSL data to plot")}
  
  if(nrow(x[x$PARM_CD == '80225',]) > 0){
    bedloadPlot <- plotfun(y = x[x$PARM_CD == '80225',], ylabel = 'Discharge (tons/day)', ptcolor = 'green')
    if(log.P80225 == TRUE){
      bedloadPlot <- bedloadPlot + scale_y_log10()
    }
  }else{warning("No bedload data to plot")}
  
  if(nrow(x[x$PARM_CD == '91145',]) > 0){
    bedmassPlot <- plotfun(y = x[x$PARM_CD == '91145',], ylabel = 'Mass (g)', ptcolor = 'grey')
    if(log.P91145 == TRUE){
      bedmassPlot <- bedmassPlot + scale_y_log10()
    }
  }else{warning("No bedload mass data to plot")}
  
  # return list of plots or output PDF document
  if(is.null(PDFout)){
    plotList <- list(sscPlot)
    names(plotList) <- 'SSC'  
    if(exists('ssbreakPlot')){plotList$ssbreak <- ssbreakPlot}
    if(exists('sslPlot')){plotList$SSL <- sslPlot}
    if(exists('bedloadPlot')){plotList$bedload <- bedloadPlot}
    if(exists('bedmassPlot')){plotList$bedmass <- bedmassPlot}
    return(plotList)
  }else{
    pdf(file = PDFout)
    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, unique(x$SITE_NO))
    text(5, 7, unique(x$STATION_NM))
    text(5, 6, "Sediment Time Series Plots")
    if(exists('sscPlot')){print(sscPlot)}
    if(exists('ssbreakPlot')){print(ssbreakPlot)}
    if(exists('sslPlot')){print(sslPlot)}
    if(exists('bedloadPlot')){print(bedloadPlot)}
    if(exists('bedmassPlot')){print(bedmassPlot)}
    dev.off()
  }
}
