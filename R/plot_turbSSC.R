#' plot_turbSSC. Create plots of sediment vs. turbidity.
#' 
#' @description Function to output scatterplot of SSC vs. turbidity. Output is a single plot or write to PDF.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param log.SSC Logical, if \code{TRUE}, SSC y axis will be log10. Default is \code{FALSE}.
#' @param log.turb Logical, if \code{TRUE}, Turbidity parameter x axis will be log10. Default is \code{FALSE}.
#' @param siteSelect Character, site number to create plots for if \code{x} contains multiple sites. Default is \code{NULL}.
#' @param PDFout Character. File or full path name of file for plot. If \code{NULL}, the default, a plot will be returned in R instead.
#' @details Scatterplot of SSC (P80154) vs. Turbidity (Parameter codes 00076, 61028, 63675, 63676, 63677, 63679, 63680, 63681, 
#' 63682, 63683, 63684, 72188, 72208, 72209, 72213, 82079, 99872).
#' @examples 
#' data("exampleData2",package="sedReview")
#' x <- exampleData2
#' turbSSC <- plot_turbSSC(exampleData2, siteSelect = "09163500")
#' \dontrun{
#' # view plot in Rstudio
#' print(turbSSC)
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
                         log.SSC = FALSE,
                         log.turb = FALSE,
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
  turb.colors <- c('white','black','orange','orangered','orangered4','orchid','red','palegoldenrod','palegreen',
                   'paleturquoise','palevioletred','peachpuff','peru','blue','salmon','sienna','grey')
  names(turb.colors) <- turb_pcodes
  
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
  
  
  # make plot
  p1 <- ggplot(data = x_turb, aes(x = RESULT_VA, y = ssc, fill = PARM_CD))
  p1 <- p1 + geom_point(size = 3, shape = 'circle filled') +
    scale_fill_manual("Turbidity P-codes",values = turb.colors)
  p1 <- p1 + xlab("Turbidity (units P-code specific)") + ylab(paste('80154_SSC (mg/L)',"\n"))
  p1 <- p1 + labs(title = paste(unique(x_turb$SITE_NO),"\n",unique(x_turb$STATION_NM))) +
    theme_bw()
  if(log.SSC == TRUE){
    p1 <- p1 + scale_x_log10()
  }
  if(log.turb == TRUE){
    p1 <- p1 + scale_y_log10()
  }
  
  # return plot list or output PDF file
  if(is.null(PDFout)){
    return(p1)
  }else{
    pdf(file = PDFout)
    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, unique(x$SITE_NO))
    text(5, 7, unique(x$STATION_NM))
    text(5, 6, "SSC vs. Turbidity Scatterplot")
    print(p1)
    dev.off()
  }
}
