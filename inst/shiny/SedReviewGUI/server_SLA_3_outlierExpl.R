#### Site-Level Assessment: Outlier explorer ####

outlier <- reactive({
  outliers <<- NULL
  outliers <<- find_outliers(siteData, site_no = input$varSite, 
                             lowThreshold = as.numeric(input$percentile), 
                             highThreshold = (1-as.numeric(input$percentile)))
  importInfo$outlierPercentile <<- input$percentile
  return(outliers)
})


siteData_wide <- eventReactive(list(input$dataPull, input$loadRData), {
  siteData_nonRej <- siteData[!(siteData$DQI_CD %in% c("X","Q")),]
  make_wideTable(siteData_nonRej)
})

######### Make GUI displayed table ###########

OutlierData <- reactive({
  OutWide <-siteData_wide()
  if(length(OutWide$Discharge..instant. != 0)) {OutWide$Qcfs <- as.numeric(OutWide$Discharge..instant.)}
  else{OutWide$Qcfs <- NA}  
  
  if(length(OutWide$Turbidity..Form.Neph != 0)) {OutWide$TurbFNU <- as.numeric(OutWide$Turbidity..Form.Neph)}
  else{OutWide$TurbFNU <- NA}
  
  if(length(OutWide$Specific.cond.at.25C != 0)) {OutWide$SC <- as.numeric(OutWide$Specific.cond.at.25C)}
  else{OutWide$SC <- NA}
  
  if(length(OutWide$Suspnd.sedmnt.conc != 0)) {OutWide$SSC <- as.numeric(OutWide$Suspnd.sedmnt.conc)}
  else{OutWide$SSC <- NA}
  
  if(length(OutWide$Suspnd.sedmnt.disch != 0)) {OutWide$SSL <- as.numeric(OutWide$Suspnd.sedmnt.disch)}
  else{OutWide$SSL <- NA}
  
  if(length(OutWide$Loss.on.ignition..bs != 0)) {OutWide$bedSedLOI <- as.numeric(OutWide$Loss.on.ignition..bs)}
  else{OutWide$bedSedLOI <- NA}
  
  if(length(OutWide$LOI.of.susp..solids != 0)) {OutWide$susSedLOI <- as.numeric(OutWide$LOI.of.susp..solids)}
  else{OutWide$susSedLOI <- NA}
  
  if(length(OutWide$Sus.sed..0.0625mm.sd != 0)) {OutWide$SandSilt <- as.numeric(OutWide$Sus.sed..0.0625mm.sd)}
  else{OutWide$SandSilt <- NA}
  
  if(length(OutWide$Bedload.sediment != 0)) {OutWide$Bedload <- as.numeric(OutWide$Bedload.sediment)}
  else{OutWide$Bedload <- NA}
  
  if(length(OutWide$Suspended.solids != 0)) {OutWide$TSS <- as.numeric(OutWide$Suspended.solids)}
  else{OutWide$TSS <- NA}
  
  OutWide <- OutWide[,c("RECORD_NO", "SITE_NO", "STATION_NM", "SAMPLE_START_DT", 
                        "Qcfs", "TurbFNU", "SC", "SSC", "SSL",
                        "bedSedLOI", "susSedLOI",
                        "SandSilt","Bedload", "TSS")]
  OutWide <- dplyr::left_join(OutWide, dplyr::select(outlier(), RECORD_NO, SITE_NO, STATION_NM, SAMPLE_START_DT, dplyr::contains('flag')),
                         by = c("RECORD_NO", "SITE_NO", "STATION_NM", "SAMPLE_START_DT"))
  
  return(OutWide)
})


##############################################
# Outlier Plot
OutlierData2<-reactive({
  dfplot2<-OutlierData()
  dfplot2$xplot2<- OutlierData()[,as.character(input$varxOut)]
  dfplot2$yplot2<- OutlierData()[,as.character(input$varyOut)]
  return(dfplot2)
})
OutlierDataLOW <- reactive({
  df <- OutlierData2()[OutlierData2()$yplot2 < quantile(OutlierData2()$yplot2, as.numeric(input$percentile), na.rm = TRUE),]
  return(df)
})
OutlierDataHI <- reactive({
  df <- OutlierData2()[OutlierData2()$yplot2 > quantile(OutlierData2()$yplot2, (1-as.numeric(input$percentile)), na.rm = TRUE),]
  return(df)
})
output$outlierPlot <- renderPlotly({
  p <- ggplot(data = OutlierData2()) +
    geom_point(aes(x=xplot2, y=yplot2,
                   text = paste0("X: ", xplot2,"\n",
                                 "Y: ", yplot2,"\n",
                                 "RECORD_NO: ",RECORD_NO)), color = 'grey') +
    geom_point(data = OutlierDataLOW(), 
               aes(x=xplot2, y=yplot2,
                   text = paste0("X: ", xplot2,"\n",
                                 "Y: ", yplot2,"\n",
                                 "RECORD_NO: ",RECORD_NO)), color = 'red', size = 3) +
    geom_point(data = OutlierDataHI(), 
               aes(x=xplot2, y=yplot2,
                   text = paste0("X: ", xplot2,"\n",
                                 "Y: ", yplot2,"\n",
                                 "RECORD_NO: ",RECORD_NO)), color = 'red', size = 3) +
    xlab(as.character(input$varxOut)) +
    ylab(as.character(input$varyOut))
  if(input$outlierlabel == TRUE){
    p <- p +
      geom_text(data = OutlierDataLOW(), aes(x = xplot2, y = yplot2, label = RECORD_NO, text = NULL)) +
      geom_text(data = OutlierDataHI(), aes(x = xplot2, y = yplot2, label = RECORD_NO, text = NULL))
  }
  p <- ggplotly(p, tooltip = "text")
  return(p)
})

# Outlier Table info and Table

output$outlierTable <- DT::renderDataTable(
  datatable({OutlierData()},
            extensions = 'Buttons', 
            rownames = FALSE,
            options = list(dom = 'Bfrtip',
                           buttons = 
                             list('colvis', list(
                               extend = 'collection',
                               buttons = list(list(extend ='csv',
                                                   filename = 'OutlierDataTable'),
                                              list(extend ='excel',
                                                   filename = 'OutlierDataTable'),
                                              list(extend ='pdf',
                                                   pageSize = 'A1',
                                                   orientation = 'landscape',
                                                   filename = 'OutlierDataTable')),
                               text = 'Download'
                             )),
                           scrollX = TRUE,
                           scrollY = "600px",
                           order = list(list(3, 'asc')),
                           pageLength = nrow({OutlierData()}),
                           selection = 'single')
            
  ))
