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
  xlab("SSC_80154") + ylab("Concentration [mg/L]")

p2 <- ggplot(data = plotData2, aes(x=PARM_NM,y=RESULT_VA))
p2 <- p2 + geom_boxplot(fill = 'grey',colour = "black", outlier.colour = 'red') + 
  xlab("TSS_00530") + ylab("")

p <- cowplot::plot_grid(p1,p2,ncol = 2)
title <- ggdraw() + draw_label(paste(unique(x$SITE_NO),unique(x$STATION_NM)), fontface = 'bold')
p_final <- cowplot::plot_grid(title, p, ncol = 1, rel_heights = c(0.1,1))
print(p_final)
