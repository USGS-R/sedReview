

plot_sedFlow <- function(x,
                         siteSelect = NULL,
                         PDFout = NULL){
  # get flow data
  if(is.null(siteSelect)){
    qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"), ]
  }else{qRecords <- x[x$SITE_NO %in% siteSelect & 
                        x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"), ]}
  
  # convert all flow data to common units
  qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "30209"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30209"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "50042"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "50042"] * 0.133681 / 60 #gal/min to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "72243"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "72243"] / 24 / 60 / 60 #cfd to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "99060"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "99060"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "99061"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "99061"] * 35.3147 #cms to cfs
  qRecords <- unique(qRecords[c("UID", "RESULT_VA")])
  
  # sediment pcodes to plot
  pcodes <- c('80154','70331','80225')
  
  # subset data to site
  if(is.null(siteSelect)){
    x <- x[x$PARM_CD %in% pcodes,]
  }else{x <- x[x$SITE_NO %in% siteSelect & x$PARM_CD %in% pcodes,]}
  
  # if more than 1 site in file, quit
  if(length(unique(x$SITE_NO)) > 1){
    stop("More than one site in input dataframe x. Subset data to one site_no with siteSelect.")
  }
  
  # SSC plot
  plotData1 <- x[x$PARM_CD %in% c('80154') & !(x$MEDIUM_CD == 'OAQ'),c("UID","PARM_CD","PARM_NM","RESULT_VA")]
  plotData1 <- dplyr::left_join(plotData1, qRecords, by = "UID")
  plotData1 <- plotData1[!(is.na(plotData1$RESULT_VA.y)),]
  if(nrow(plotData1) == 0){warning("No SSC data to plot")
  }else{
    p1 <- ggplot(data = plotData1, aes(x=RESULT_VA.y, y=RESULT_VA.x))
    p1 <- p1 + geom_point() + 
      xlab("Discharge (cfs)") + ylab("SSC_80154 (mg/L)")
  }
  
  # Sand/Silt break plot
  plotData2 <- x[x$PARM_CD %in% c('70331') & !(x$MEDIUM_CD == 'OAQ'),c("UID","PARM_CD","PARM_NM","RESULT_VA")]
  plotData2 <- dplyr::left_join(plotData2, qRecords, by = "UID")
  plotData2 <- plotData2[!(is.na(plotData2$RESULT_VA.y)),]
  if(nrow(plotData2) == 0){warning("No Sand/Silt break data to plot")
  }else{
    p2 <- ggplot(data = plotData2, aes(x=RESULT_VA.y, y=RESULT_VA.x))
    p2 <- p2 + geom_point() + 
      xlab("Discharge (cfs)") + ylab("70331 Sus Sed % smaller than 0.0625mm")
  }
  
  # bedload plot
  plotData3 <- x[x$PARM_CD %in% c('80225') & !(x$MEDIUM_CD == 'OAQ'),c("UID","PARM_CD","PARM_NM","RESULT_VA")]
  plotData3 <- dplyr::left_join(plotData3, qRecords, by = "UID")
  plotData3 <- plotData3[!(is.na(plotData3$RESULT_VA.y)),]
  if(nrow(plotData3) == 0){warning("No bedload data to plot")
  }else{
    p3 <- ggplot(data = plotData3, aes(x=RESULT_VA.y, y=RESULT_VA.x))
    p3 <- p3 + geom_point() + 
      xlab("Discharge (cfs)") + ylab("Bedload_80225 (mg/L)")
  }
  
  # output
  if(is.null(PDFout)){
    plotList <- list(p1)
    names(plotList) <- 'SSC'  
    if(exists('p2')){plotList$ssbreak <- p2}
    if(exists('p3')){plotList$bedload <- p3}
    return(plotList)
  }else{
    pdf(file = PDFout)
    plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    text(5, 8, unique(x$SITE_NO))
    text(5, 7, unique(x$STATION_NM))
    text(5, 6, "Sediment vs. Discharge Scatterplots")
    if(exists('p1')){print(p1)}
    if(exists('p2')){print(p2)}
    if(exists('p3')){print(p3)}
    dev.off()
  }
  
}
  