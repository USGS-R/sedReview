#' plot_turbSSC. Create plots of sediment vs. turbidity.
#' 
#' @description Function to output scatterplots of SSC vs. turbidity. Output is a list of plots or write to PDF.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param siteSelect Character, site number to create plots for if \code{x} contains multiple sites. Default is \code{NULL}.
#' @param PDFout Character. File or full path name of file for plots. If \code{NULL}, the default, a list of the plots will be returned in R instead.
#' @details Scatterplots of SSC (P80154) vs. Turbidity (Parameter codes 00076, 61028, 63675, 63676, 63677, 63679, 63680, 63681, 
#' 63682, 63683, 63684, 72188, 72208, 72209, 72213, 82079, 99872). A scatterplot is created for each unique turbidity parameter code present.
#' @details If PDFout is not specified, than a list of the plots is returned. Plots (if applicable) are specified above. See example for more details.
#' @examples 
#' data("exampleData2",package="sedReview")
#' x <- exampleData2
#' turbSSC <- plot_turbSSC(exampleData2, siteSelect = "09163500")
#' \dontrun{
#' # view plot in Rstudio
#' turbSSC$Turbidity_00076
#' 
#' # output to file on D drive
#' plot_turbSSC(exampleData2, siteSelect = "09163500", PDFout = "D:/turbidity.pdf")
#' }
#' 
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#' @return If \code{PDFout = NULL}, list containing ggplot elements. If \code{PDFout} specified, a PDF document containing the plots.

plot_turbSSC <- function(x,
                         siteSelect = NULL,
                         PDFout = NULL){
  # subset data
  if(!(is.null(siteSelect))){
    x <- x[x$SITE_NO == siteSelect,]
  }
  # if more than 1 site in file, quit
  if(length(unique(x$SITE_NO)) > 1){
    stop("More than one site in input dataframe x. Subset data to one site_no with siteSelect.")
  }
  
  turb_pcodes <- c('00076', '61028', '63675', '63676', '63677', '63679', '63680', '63681', '63682',
                   '63683', '63684', '72188', '72208', '72209', '72213', '82079', '99872')
  
  # subset turbidity and ssc data
  x_turb <- x[x$PARM_CD %in% turb_pcodes,]
  if(nrow(x_turb) == 0){stop("No Turbidity data. Check input dataframe x.")}
  x_ssc <- x[x$PARM_CD == '80154', c('UID','RESULT_VA')]
  names(x_ssc) <- c('UID', 'ssc')
  if(nrow(x_ssc) == 0){stop("No SSC data. Check input dataframe x.")}
  
  # join ssc to turbidity
  x_turb <- dplyr::left_join(x_turb, x_ssc, by = 'UID')
  x_turb <- x_turb[!(is.na(x_turb$RESULT_VA)) & !(is.na(x_turb$ssc)),]
  if(nrow(x_turb) == 0){stop("No Turbidity/SSC pair data. Check input dataframe x.")}
  
  # internal plotting function
  plotfun <- function(y,plotdata){
    z <- plotdata[plotdata$PARM_CD == y,]
    p1 <- ggplot(data = z, aes(x = RESULT_VA, y = ssc))
    p1 <- p1 + geom_point(size = 3)
    p1 <- p1 + xlab("Turbidity") + ylab(paste('80154_SSC (mg/L)',"\n")) + labs(caption = paste(unique(z$PARM_CD),unique(z$PARM_NM)))
    p1 <- p1 + labs(title = paste(unique(z$SITE_NO),"\n",unique(z$STATION_NM)))
    return(p1)
  }
  
  # apply plotfun to all unique turbidity pcodes in input data
  plotList <- lapply(unique(x_turb$PARM_CD),plotfun, plotdata = x_turb)
  names(plotList) <- paste0('Turbidity_',unique(x_turb$PARM_CD))
  
  # return plot list or output PDF file
  if(is.null(PDFout)){
    return(plotList)
  }else{
    pdf(file = PDFout)
    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, unique(x$SITE_NO))
    text(5, 7, unique(x$STATION_NM))
    text(5, 6, "SSC vs. Turbidity Scatterplots")
    for(i in 1:length(plotList)){
      print(plotList[[i]])
    }
    dev.off()
  }
}
