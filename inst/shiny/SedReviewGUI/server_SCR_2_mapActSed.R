#### Science-Center Review: Map of active sediment sites ####

StateSites <- eventReactive(input$mapPlot, {
  readNWISsite(SitesCount$SITE_NO)
})


ActiveSedSites <- eventReactive(input$mapPlot, {
  left_join(SitesCount, unique(select(StateSites(), c(site_no, station_nm, dec_lat_va, dec_long_va, alt_va))), by = c("SITE_NO" = "site_no"))
})

MapActiveSites <- reactive({
  filter(ActiveSedSites(), get(input$mapWhat) >= 1) 
})

Sedmap <- reactive({ 
  
  leaflet(data=MapActiveSites()) %>%
    addProviderTiles("CartoDB.Positron",
                     options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(~dec_long_va,~dec_lat_va,
                     fillColor = "orange",
                     radius = 3,
                     fillOpacity = 0.8, opacity = 0.8,stroke=FALSE,
                     popup=~SITE_NO_STATION_NM)
})

Map1table <- reactive({ #https://blog.exploratory.io/filter-with-text-data-952df792c2ba
  ActiveSedSites()%>%
    select("SITE_NO", "STATION_NM", "WY", "SSC_80154", "SandSilt_70331", "TSS_00530", "bedload_80225")#%>%
  #filter(str_detect(SITE_NO, as.character(input$map1SiteId))) 
})

output$activeMap <- renderLeaflet({
  Sedmap()
})

output$map1Sitetable <- DT::renderDataTable(
  datatable({Map1table()},
            extensions = 'Buttons', 
            rownames = FALSE,
            options = list(dom = 'Bfrtip',
                           buttons = 
                             list('colvis', list(
                               extend = 'collection',
                               buttons = list(list(extend ='csv',
                                                   filename = 'MapActiveTable'),
                                              list(extend ='excel',
                                                   filename = 'MapActiveTable'),
                                              list(extend ='pdf',
                                                   pageSize = 'A4',
                                                   orientation = 'portrait',
                                                   filename = 'MapActiveTable')),
                               text = 'Download'
                             )),
                           scrollX = TRUE,
                           scrollY = "600px",
                           order = list(list(0, 'asc')),
                           pageLength = nrow({Map1table()}),
                           selection = 'single')
            
  ))
