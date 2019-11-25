#### Science-Center Review: Map of sites by parameter code ####

PcodeSearch <- eventReactive(input$mapPcode, {
  readNWISdata(stateCd = as.character(input$StateCd), parameterCd = as.character(input$searchWhat),
               service="site", seriesCatalogOutput=TRUE)
})

PcodeSiteList <-eventReactive(input$mapPcode, {
  unique(PcodeSearch()$site_no)
})

PcodeSites <- reactive({
  PcodeSites <-unique.data.frame(select(PcodeSearch(), c("site_no", "station_nm", "dec_long_va", "dec_lat_va")))
  PcodeSites$SITE_NO_STATION_NM<- paste0(PcodeSites$site_no, " - ", PcodeSites$station_nm)
  return(PcodeSites)
})

PcodeData <- eventReactive(input$mapPcode, {
  readNWISqw(siteNumbers = as.character(PcodeSiteList()),
             parameterCd = as.character(input$searchWhat),
             startDate = as.character(input$reviewBeginDT))
})

SitesDataSummary <- eventReactive(input$mapPcode, {
  renameNWISColumns(PcodeData())%>%
    group_by(site_no) %>%
    summarise(count=n(),
              start=min(startDateTime),
              end=max(startDateTime),
              mean=signif(mean(result_va, na.rm = TRUE), digits = 3),
              min=min(result_va, na.rm = TRUE),
              max=max(result_va, na.rm = TRUE),
              median=median(result_va, na.rm = TRUE))
})


PcodeSitesMap <- eventReactive(input$mapPcode, {
  left_join(SitesDataSummary(), PcodeSites())
})

PcodeSitesMapFiltered <- reactive({
  filter(PcodeSitesMap(), as.numeric(PcodeSitesMap()$count) >= 1)
})


PcodeMap <- reactive({
  
  map2<- leaflet(data=PcodeSitesMapFiltered()) %>%
    addProviderTiles("CartoDB.Positron",
                     options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(~dec_long_va,~dec_lat_va,
                     fillColor = "orange",
                     radius = ~(get(input$sizeWhat))*(0.01*as.numeric(input$scale)),
                     fillOpacity = 0.8, opacity = 0.8,stroke=FALSE,
                     popup=~SITE_NO_STATION_NM)
  return(map2)
})

output$PcodeMap <- renderLeaflet({
  PcodeMap()
})

output$map2Sitetable <- DT::renderDataTable(
  datatable({PcodeSitesMapFiltered()},
            extensions = 'Buttons', 
            rownames = FALSE,
            options = list(dom = 'Bfrtip',
                           buttons = 
                             list('colvis', list(
                               extend = 'collection',
                               buttons = list(list(extend ='csv',
                                                   filename = 'PcodeSitesMapTable'),
                                              list(extend ='excel',
                                                   filename = 'PcodeSitesMapTable'),
                                              list(extend ='pdf',
                                                   pageSize = 'A3',
                                                   orientation = 'landscape',
                                                   filename = 'PcodeSitesMapTable')),
                               text = 'Download'
                             )),
                           scrollX = TRUE,
                           scrollY = "600px",
                           order = list(list(0, 'asc')),
                           pageLength = nrow({PcodeSitesMapFiltered()}),
                           selection = 'single')
            
  ))

# output$map2Sampletable <- DT::renderDataTable({
#   PcodeData()
# })
