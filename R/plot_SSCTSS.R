#' plot_SSCTSS. Plot individual and side-by-side boxplots for SSC and TSS, and SSC vs. TSS scatterplot at a site.
#' 
#' @description Function to output individual and side-by-side boxplots, and scatterplot, for SSC and TSS at a site. Output is a list of plots or write to PDF.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param siteSelect Character, site number to create plots for if \code{x} contains multiple sites. Default is \code{NULL}.
#' @param PDFout Character. File or full path name of file for plots. If \code{NULL}, the default, a list of the plots will be returned in R instead.
#' @details Boxplots of SSC (P80154) and TSS (P00530). Box contains lower, median, and upper quartile. Whiskers extend to farthest point
#' within +/- 1.5 IQR. Any points outside of +/- 1.5 IQR are plotted as outlier points.
#' Both parameters do not need to be present in \code{x} for plots to be created (eg. if only SSC present then only a boxplot of SSC is returned) 
#' @details If PDFout is not specified, than a list of the plots is returned. Plots (if applicable) are boxplot of SSC, boxplot of TSS,
#' side-by-side boxplot of SSC and TSS, and a SSC vs. TSS scatterplot of samples with both analyses. See example for more details.
#' @details Portions of code modified from \code{WQReview::qwparmBoxPlot}.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData2
#' ssctss <- plot_SSCTSS(exampleData2, siteSelect = "09163500")
#' \dontrun{
#' # view plot in Rstudio
#' ssctss$combined
#' 
#' # output to file on D drive
#' plot_ssctss(exampleData2, siteSelect = "09163500", PDFout = "D:/ssctss.pdf")
#' }
#' 
#' @import ggplot2
#' @import cowplot
#' @importFrom dplyr left_join
#' @export
#' @return If \code{PDFout = NULL}, list containing ggplot elements. If \code{PDFout} specified, a PDF document containing the plots.

plot_SSCTSS <- function(x,
                        siteSelect = NULL,
                        PDFout = NULL){
  if(!(is.null(siteSelect))){
    x <- x[x$SITE_NO == siteSelect,]
  }
  # if more than 1 site in file, quit
  if(length(unique(x$SITE_NO)) > 1){
    stop("More than one site in input dataframe x. Subset data to one site_no with siteSelect.")
  }
  
  # SSC plot
  plotData1 <- x[x$PARM_CD %in% c('80154') & !(x$MEDIUM_CD == 'OAQ'),c("UID","PARM_CD","PARM_NM","RESULT_VA")]
  if(nrow(plotData1) == 0){warning("No SSC data to plot")
  }else{
    p1 <- ggplot(data = plotData1, aes(x=PARM_NM,y=RESULT_VA))
    p1 <- p1 + geom_boxplot(fill = 'grey',colour = "black", outlier.colour = 'red') + 
      xlab("SSC_80154") + ylab("Concentration (mg/L)")
  }
  
  # TSS plot
  plotData2 <- x[x$PARM_CD %in% c('00530') & !(x$MEDIUM_CD == 'OAQ'),c("UID","PARM_CD","PARM_NM","RESULT_VA")]
  if(nrow(plotData2) == 0){warning("No TSS data to plot")
  }else{
    p2 <- ggplot(data = plotData2, aes(x=PARM_NM,y=RESULT_VA))
    p2 <- p2 + geom_boxplot(fill = 'grey',colour = "black", outlier.colour = 'red') + 
      xlab("TSS_00530") + ylab(NULL)
  }
  
  # if no data to plot, quit function
  if(nrow(plotData1) == 0 & nrow(plotData2) == 0){stop("No SSC or TSS data to plot")}
  
  # if both SSC and TSS, create side-by-side plot
  if(exists('p1') & exists('p2')){
    p_final <- cowplot::plot_grid(p1,p2,ncol = 2)
    title <- cowplot::ggdraw() + cowplot::draw_label(paste(unique(x$SITE_NO),unique(x$STATION_NM)), fontface = 'bold')
    p_final <- cowplot::plot_grid(title, p_final, ncol = 1, rel_heights = c(0.1,1))
    p1 <- p1 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))
    p2 <- p2 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM))) + ylab("Concentration (mg/L")
  }else{
    if(exists('p1')){p1 <- p1 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))}
    if(exists('p2')){p2 <- p2 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM))) + ylab("Concentration (mg/L")}
  }
  
  # create SSC vs. TSS scatterplot. RESULT_VA.x is SSC, RESULT_VA.y is TSS
  scatterdata <- plotData1[plotData1$UID %in% plotData2$UID,]
  scatterdata <- dplyr::left_join(scatterdata, plotData2, by = "UID")
  scatterdata <- scatterdata[!(is.na(scatterdata$RESULT_VA.x)) & !(is.na(scatterdata$RESULT_VA.y)),]
  if(nrow(scatterdata) == 0){warning("No TSS/SSC pairs for scatterplot. Check input dataframe x.")
  }else{
    p3 <- ggplot(data = scatterdata, aes(x = RESULT_VA.x, y = RESULT_VA.y))
    p3 <- p3 + geom_point(size = 3)
    p3 <- p3 + xlab("TSS_00530 (mg/L)") + ylab(paste('SSC_80154 (mg/L)',"\n"))
    p3 <- p3 + labs(title = paste(unique(scatterdata$SITE_NO),"\n",unique(scatterdata$STATION_NM)))
  }
  
  if(is.null(PDFout)){
    plotList <- list(p1)
    names(plotList) <- 'SSC'  
    if(exists('p2')){plotList$TSS <- p2}
    if(exists('p_final')){plotList$combined <- p_final}
    if(exists('p3')){plotList$scatter <- p3}
    return(plotList)
  }else{
    pdf(file = PDFout)
    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, unique(x$SITE_NO))
    text(5, 7, unique(x$STATION_NM))
    text(5, 6, "SSC and TSS Boxplots")
    if(exists('p1')){print(p1)}
    if(exists('p2')){print(p2)}
    if(exists('p_final')){print(p_final)}
    if(exists('p3')){print(p3)}
    dev.off()
  }
}