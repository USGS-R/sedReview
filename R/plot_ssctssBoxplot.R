# modified from WQReview::qwparmBoxPlot
library(ggplot2)
library(cowplot)
x <- monumentBijou
siteSelect <- '07104905'
x <- x[x$SITE_NO == siteSelect,]

# if more than 1 site in file, quit
if(length(unique(x$SITE_NO)) > 1){
  stop("More than one site in input dataframe x. Subset data to one site_no.")
}

plotData1 <- x[x$PARM_CD %in% c('80154') & !(x$MEDIUM_CD == 'OAQ'),c("PARM_CD","PARM_NM","RESULT_VA")]
plotData2 <- x[x$PARM_CD %in% c('00530') & !(x$MEDIUM_CD == 'OAQ'),c("PARM_CD","PARM_NM","RESULT_VA")]

p1 <- ggplot(data = plotData1, aes(x=PARM_NM,y=RESULT_VA))
p1 <- p1 + geom_boxplot(fill = 'grey',colour = "black", outlier.colour = 'red') + 
  xlab("SSC_80154") + ylab("Concentration (mg/L)")

p2 <- ggplot(data = plotData2, aes(x=PARM_NM,y=RESULT_VA))
p2 <- p2 + geom_boxplot(fill = 'grey',colour = "black", outlier.colour = 'red') + 
  xlab("TSS_00530") + ylab("")

if(exists('p1') & exists('p2')){
  p_final <- cowplot::plot_grid(p1,p2,ncol = 2)
  title <- ggdraw() + draw_label(paste(unique(x$SITE_NO),unique(x$STATION_NM)), fontface = 'bold')
  p_final <- cowplot::plot_grid(title, p_final, ncol = 1, rel_heights = c(0.1,1))
  p1 <- p1 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))
  p2 <- p2 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))
}else{
  p1 <- p1 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))
  p2 <- p2 + labs(title = paste(unique(x$SITE_NO),"\n",unique(x$STATION_NM)))
}

pdf(file = "BoxPlots.pdf")
plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
text(5, 8, unique(x$SITE_NO))
text(5, 7, unique(x$STATION_NM))
text(5, 6, "SSC and TSS Boxplots")
if(exists('p1')){p1}
if(exists('p2')){p2}
if(exists('p_final')){p_final}
dev.off()
