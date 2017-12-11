# modified from WQReview::qwtsPlot
library(ggplot2)
library(cowplot)
x <- monumentBijou
siteSelect <- '07104905'
pcodes <- c('80154','70331','80155','80225','91145')
x <- x[x$SITE_NO %in% siteSelect & x$PARM_CD %in% pcodes,]

# if more than 1 site in file, quit
if(length(unique(x$SITE_NO)) > 1){
  stop("More than one site in input dataframe x. Subset data to one site_no.")
}

plotfun <- function(y, ylabel, ptcolor){
  p1 <- ggplot(data = y, aes(x = SAMPLE_START_DT, y = RESULT_VA))
  p1 <- p1 + geom_point(size = 3, color = ptcolor)
  p1 <- p1 + xlab("Date") + ylab(paste(ylabel,"\n")) + labs(caption = paste(unique(y$PARM_CD),unique(y$PARM_DS)))
  p1 <- p1 + labs(title = paste(unique(y$SITE_NO),"\n",unique(y$STATION_NM)))
  return(p1)
}

if(nrow(x[x$PARM_CD == '80154',]) > 0){
  sscPlot <- plotfun(y = x[x$PARM_CD == '80154',], ylabel = 'Concentration (mg/L)', ptcolor = 'blue')
}

if(nrow(x[x$PARM_CD == '70331',]) > 0){
  ssbreakPlot <- plotfun(y = x[x$PARM_CD == '70331',], ylabel = 'Percent (%)', ptcolor = 'tan')
}

if(nrow(x[x$PARM_CD == '80155',]) > 0){
  sslPlot <- plotfun(y = x[x$PARM_CD == '80155',], ylabel = 'Discharge (tons/day)', ptcolor = 'brown')
}

if(nrow(x[x$PARM_CD == '80225',]) > 0){
  bedloadPlot <- plotfun(y = x[x$PARM_CD == '80225',], ylabel = 'Discharge (tons/day)', ptcolor = 'green')
}

if(nrow(x[x$PARM_CD == '91145',]) > 0){
  bedmassPlot <- plotfun(y = x[x$PARM_CD == '91145',], ylabel = 'Mass (g)', ptcolor = 'grey')
}

pdf(file = "TimeSeries.pdf")
plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(5, 8, unique(x$SITE_NO))
text(5, 7, unique(x$STATION_NM))
text(5, 6, "Sediment Time Series Plots")
if(exists('sscPlot')){sscPlot}
if(exists('ssbreakPlot')){ssbreakPlot}
if(exists('sslPlot')){sslPlot}
if(exists('bedloadPlot')){bedloadPlot}
if(exists('bedmassPlot')){bedmassPlot}
dev.off()

