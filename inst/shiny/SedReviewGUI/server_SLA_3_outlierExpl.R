#### Site-Level Assessment: Outlier explorer ####

outlier <- reactive({
  find_outliers(siteData(), site_no = input$varSite, lowThreshold = as.numeric(input$percentile), highThreshold = (1-as.numeric(input$percentile)))
})

#join flag table eith SiteData Table for plotting ????????????????????????????

JoinTable <- eventReactive(input$dataPull, {
  make_wideTable(siteData())
})

######### Make placeholder columns ###########

JoinTable2 <- reactive({
  J2 <-JoinTable()
  if(length(J2$Turbidity..Form.Neph != 0)) {J2$TurbFNU <- as.numeric(J2$Turbidity..Form.Neph)}
  else{J2$TurbFNU <- NA}
  
  if(length(J2$Specific.cond.at.25C != 0)) {J2$SC <- as.numeric(J2$Specific.cond.at.25C)}
  else{J2$SC <- NA}
  
  if(length(J2$Discharge..instant. != 0)) {J2$Qcfs <- as.numeric(J2$Discharge..instant.)}
  else{J2$Qcfs <- NA}
  
  if(length(J2$Suspnd.sedmnt.conc != 0)) {J2$SSC <- as.numeric(J2$Suspnd.sedmnt.conc)}
  else{J2$SSC <- NA}
  
  if(length(J2$Sus.sed..0.0625mm.sd != 0)) {J2$SandSilt <- as.numeric(J2$Sus.sed..0.0625mm.sd)}
  else{J2$SandSilt <- NA}
  
  if(length(J2$Bedload.sediment != 0)) {J2$Bedload <- as.numeric(J2$Bedload.sediment)}
  else{J2$Bedload <- NA}
  
  if(length(J2$Suspended.solids != 0)) {J2$TSS <- as.numeric(J2$Suspended.solids)}
  else{J2$TSS <- NA}
  
  return(J2)
})


##############################################
JoinTableSelect <- reactive({
  select(JoinTable2(), c("RECORD_NO", "SITE_NO", "STATION_NM", "SAMPLE_START_DT", "TurbFNU", "SC", "Qcfs", "SSC", "SandSilt","Bedload", "TSS")) #, "Turbidity..Form.Neph",
})

OutlierData <- reactive({
  left_join(JoinTableSelect(), outlier())
})

# Outlier Plot
OutlierData2<-reactive({
  dfplot2<-OutlierData()
  dfplot2$xplot2<- OutlierData()[,as.numeric(input$varx2)]
  dfplot2$yplot2<- OutlierData()[,as.numeric(input$vary2)]
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
  ggplotly(
    ggplot(data = OutlierData2()) +
      geom_point(aes(x=xplot2, y=yplot2), color = 'grey') +
      geom_point(data = OutlierDataLOW(), 
                 aes(x=xplot2, y=yplot2), color = 'red', size = 3) +
      geom_point(data = OutlierDataHI(), 
                 aes(x=xplot2, y=yplot2), color = 'red', size = 3) +
      xlab(names(OutlierData()[as.numeric(input$varx2)])) +
      ylab(names(OutlierData()[as.numeric(input$vary2)]))
  )
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
