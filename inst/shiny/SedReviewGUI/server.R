server <- function(input, output, session) {
  
  # Data Pull routine
  siteData <- eventReactive(input$dataPull, {
    
    # import data for your site using get_localNwis.
    get_localNWIS(DSN = input$DBName,            # NWIS server 
                  env.db = input$env.db,           # environmental database number  
                  qa.db = input$qa.db,            # QA database number 
                  STAIDS = input$varSite,             
                  begin.date = input$beginDT, # WY 2017
                  end.date = input$endDT)
  })
  
  sumStats <- eventReactive(input$dataPull, {
    sumStats <- calc_summaryStats(siteData())
    return(sumStats)
  })
  
  output$sumtable <- DT::renderDataTable(
    datatable({sumStats()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SiteSummaryTable'),
                                                list(extend ='excel',
                                                     filename = 'SiteSummaryTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'landscape',
                                                     filename = 'SiteSummaryTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'desc'), list(2, 'asc')),
                             pageLength = nrow({sumStats()}),
                             selection = 'single')
              
    ))
  
  # Center-level data summary pull routine  
  CenterReviewData <- eventReactive(input$reviewPull, {
    # import data for your site using get_localNwis.
    SitesCount <- count_activeSed(DSN = input$DBName2,            # NWIS server
                                  env.db = input$env.db2,           # environmental database number
                                  begin.date = input$reviewBeginDT, 
                                  end.date = input$reviewEndDT)
    SitesCount$SITE_NO <- trimws(SitesCount$SITE_NO)
    SitesCount$SITE_NO_STATION_NM <- paste0(SitesCount$SITE_NO, " - ", SitesCount$STATION_NM)
    return(SitesCount)
  })
  
  # output$centerSumtable <- DT::renderDataTable({datatable(
  #   CenterReviewData(), rownames = FALSE, options = list(scrollX = TRUE, scrollY = "600px"))
  # })
  ########################################################################################################################
  output$centerSumtable <- DT::renderDataTable(
    datatable({CenterReviewData()[, -c(8)]}, 
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLSummaryTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLSummaryTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'portrait',
                                                     filename = 'SCLSummaryTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'desc'), list(2, 'asc')),
                             pageLength = nrow({CenterReviewData()}),
                             selection = 'single')
              
    ))
  ########################################################################################################################
  ##### SCL Data Flags
  
  SCLsiteData <- eventReactive(input$reviewPull, {
    get_localNWIS(DSN = input$DBName2,            # Colorado NWIS server 
                  env.db = input$env.db2,
                  qa.db = input$qa.db2,
                  STAIDS = CenterReviewData()$SITE_NO,             
                  begin.date = input$reviewBeginDT, # WY 2016-2017
                  end.date = input$reviewEndDT,
                  approval = "All")
  })
  
  ############## Science-Center level Box Coeff Summary
  
  SCLBoxCoeff<- eventReactive(input$reviewLoad, {
    summary_boxcoef(SCLsiteData(), timediff = input$searchInterval2)
    
  })
  
  
  output$SCLBoxCoefftable <- DT::renderDataTable(
    datatable({SCLBoxCoeff()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLBoxCoeffTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLBoxCoeffTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'portrait',
                                                     filename = 'SCLBoxCoeffTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(0, 'desc'), list(2, 'asc')),
                             pageLength = nrow({SCLBoxCoeff()}),
                             selection = 'single')
              
    ))
  
  ##############
  
  
  SCLbagIE <- eventReactive(input$reviewPull, {
    
    check_bagIE(SCLsiteData(), reviewSummary = TRUE)
    
  })
  
  SCLhasQ <- eventReactive(input$reviewPull, {
    
    check_Q(SCLsiteData(), reviewSummary = TRUE)
    
  })
  
  # SCLmetaData <- eventReactive(input$reviewPull, {
  # 
  #   check_metaData(SCLsiteData(), reviewSummary = TRUE)
  # 
  # })
  
  SCLqaqc <- eventReactive(input$reviewPull, {
    
    check_qaqcDB(SCLsiteData(), as.character(input$qa.db2), reviewSummary = TRUE)
    
  })
  
  # SCLpurp <- eventReactive(input$reviewPull, {
  # 
  #   check_samplePurp(SCLsiteData(), reviewSummary = TRUE)
  # 
  # })
  # 
  # SCLsampler <- eventReactive(input$reviewPull, {
  # 
  #   check_samplerType(SCLsiteData(), reviewSummary = TRUE)
  # 
  # })
  
  SCLsedMass <- eventReactive(input$reviewPull, {
    
    check_sedMass(SCLsiteData(), reviewSummary = TRUE)
    
  })
  
  SCLunpairedTSS <- eventReactive(input$reviewPull, {
    
    check_tss(SCLsiteData(), reviewSummary = TRUE)
    
  })
  
  SCLverticals <- eventReactive(input$reviewPull, {
    
    check_verticals(SCLsiteData(), reviewSummary = TRUE)
    
  })
  
  SCLmethods <- eventReactive(input$reviewPull, {
    
    count_methodsBySite(SCLsiteData())
    
  })
  
  SCLstatus <- eventReactive(input$reviewPull, {
    
    count_sampleStatus(SCLsiteData())
    
  })
  
  output$SCLflagtable1 <- DT::renderDataTable(
    datatable({SCLbagIE()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLbagIETable'),
                                                list(extend ='excel',
                                                     filename = 'SCLbagIETable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'portrait',
                                                     filename = 'SCLbagIETable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLbagIE()}),
                             selection = 'single')
              
    ))
  
  output$SCLflagtable2 <- DT::renderDataTable(
    datatable({SCLhasQ()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLmissingQTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLmissingQTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'portrait',
                                                     filename = 'SCLmissingQTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLhasQ()}),
                             selection = 'single')
              
    ))
  
  # output$SCLflagtable3 <- DT::renderDataTable({
  #   SCLmetaData()
  # })
  
  output$SCLflagtable4 <- DT::renderDataTable(
    datatable({SCLqaqc()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLqaqcTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLqaqcTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'landscape',
                                                     filename = 'SCLqaqcTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLqaqc()}),
                             selection = 'single')
              
    ))
  
  # output$SCLflagtable5 <- DT::renderDataTable({
  #   SCLpurp()
  # })
  # 
  # output$SCLflagtable6 <- DT::renderDataTable({
  #   SCLsampler()
  # })
  
  output$SCLflagtable7 <- DT::renderDataTable(
    datatable({SCLsedMass()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLsedMassTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLsedMassTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'portrait',
                                                     filename = 'SCLsedMassTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLsedMass()}),
                             selection = 'single')
              
    ))
  
  output$SCLflagtable8 <- DT::renderDataTable(
    datatable({SCLunpairedTSS()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLunpairedTSSTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLunpairedTSSTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'portrait',
                                                     filename = 'SCLunpairedTSSTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLunpairedTSS()}),
                             selection = 'single')
              
    ))
  
  output$SCLflagtable9 <- DT::renderDataTable(
    datatable({SCLverticals()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLverticalsTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLverticalsTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'SCLverticalsTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLverticals()}),
                             selection = 'single')
              
    ))
  
  output$SCLflagtable10 <- DT::renderDataTable(
    datatable({SCLmethods()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLmethodsTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLmethodsTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A0',
                                                     orientation = 'landscape',
                                                     filename = 'SCLmethodsTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLmethods()}),
                             selection = 'single')
              
    ))
  
  output$SCLflagtable11 <- DT::renderDataTable(
    datatable({SCLstatus()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'SCLstatusTable'),
                                                list(extend ='excel',
                                                     filename = 'SCLstatusTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'portrait',
                                                     filename = 'SCLstatusTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(0, 'asc')),
                             pageLength = nrow({SCLstatus()}),
                             selection = 'single')
              
    ))
  
  ################## Map of active sediment sites
  StateSites <- eventReactive(input$mapPlot, {
    readNWISsite(CenterReviewData()$SITE_NO)
  })
  
  
  ActiveSedSites <- eventReactive(input$mapPlot, {
    left_join(CenterReviewData(), unique(select(StateSites(), c(site_no, station_nm, dec_lat_va, dec_long_va, alt_va))), by = c("SITE_NO" = "site_no"))
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
  
  ##################
  
  ################## Map of sites by pcode
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
  
  # withProgress - sets view of run-time on process window
  
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
  
  ##################
  
  # Flagging routine
  checkAll <- eventReactive(input$dataPull, {
    
    check_all(subset.data.frame(siteData(), SAMPLE_START_DT>=as.POSIXct(input$analysisBeginDT, tz = input$tz)), qa.db = as.character(input$qa.db), returnAllTables = FALSE)
    
  })
  
  bagIE <- eventReactive(input$dataPull, {
    
    check_bagIE(siteData())
    
  })
  
  hasQ <- eventReactive(input$dataPull, {
    
    check_Q(siteData())
    
  })
  
  metaData <- eventReactive(input$dataPull, {
    
    check_metaData(siteData())
    
  })
  
  qaqc <- eventReactive(input$dataPull, {
    
    check_qaqcDB(siteData(), as.character(input$qa.db))
    
  })
  
  purp <- eventReactive(input$dataPull, {
    
    check_samplePurp(siteData())
    
  })
  
  sampler <- eventReactive(input$dataPull, {
    
    check_samplerType(siteData())
    
  })
  
  sedMass <- eventReactive(input$dataPull, {
    
    check_sedMass(siteData())
    
  })
  
  unpairedTSS <- eventReactive(input$dataPull, {
    
    check_tss(siteData())
    
  })
  
  verticals <- eventReactive(input$dataPull, {
    
    check_verticals(siteData())
    
  })
  
  methods <- eventReactive(input$dataPull, {
    
    count_methodsBySite(siteData())
    
  })
  
  status <- eventReactive(input$dataPull, {
    
    count_sampleStatus(siteData())
    
  })
  
  noResult <- eventReactive(input$dataPull, {
    
    check_commentsNoResult (siteData())
    
  })
  
  outlier <- reactive({
    
    find_outliers(siteData(), site_no = input$varSite, lowThreshold = as.numeric(input$percentileLow), highThreshold = as.numeric(input$percentileHigh))
    
  })
  
  
  output$flagtablesum <- DT::renderDataTable(
    datatable({checkAll()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'checkAllTable'),
                                                list(extend ='excel',
                                                     filename = 'checkAllTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'checkAllTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({checkAll()}),
                             selection = 'single')
              
    ))
  
  output$flagtable1 <- DT::renderDataTable(
    datatable({bagIE()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'bagIETable'),
                                                list(extend ='excel',
                                                     filename = 'bagIETable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'bagIETable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({bagIE()}),
                             selection = 'single')
              
    ))
  
  output$flagtable2 <- DT::renderDataTable(
    datatable({hasQ()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'missingQTable'),
                                                list(extend ='excel',
                                                     filename = 'missingQTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'missingQTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({hasQ()}),
                             selection = 'single')
              
    ))
  
  output$flagtable3 <- DT::renderDataTable(
    datatable({metaData()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'metaDataTable'),
                                                list(extend ='excel',
                                                     filename = 'metaDataTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A1',
                                                     orientation = 'landscape',
                                                     filename = 'metaDataTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({metaData()}),
                             selection = 'single')
              
    ))
  
  output$flagtable4 <- DT::renderDataTable(
    datatable({qaqc()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'qaqcTable'),
                                                list(extend ='excel',
                                                     filename = 'qaqcTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'landscape',
                                                     filename = 'qaqcTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({qaqc()}),
                             selection = 'single')
              
    ))
  
  output$flagtable5 <- DT::renderDataTable(
    datatable({purp()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'purpTable'),
                                                list(extend ='excel',
                                                     filename = 'purpTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'purpTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({purp()}),
                             selection = 'single')
              
    ))
  
  output$flagtable6 <- DT::renderDataTable(
    datatable({sampler()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'samplerTable'),
                                                list(extend ='excel',
                                                     filename = 'samplerTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'samplerTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({sampler()}),
                             selection = 'single')
              
    ))
  
  output$flagtable7 <- DT::renderDataTable(
    datatable({sedMass()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'sedMassTable'),
                                                list(extend ='excel',
                                                     filename = 'sedMassTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'landscape',
                                                     filename = 'sedMassTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({sedMass()}),
                             selection = 'single')
              
    ))
  
  output$flagtable8 <- DT::renderDataTable(
    datatable({unpairedTSS()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'unpairedTSSTable'),
                                                list(extend ='excel',
                                                     filename = 'unpairedTSSTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'landscape',
                                                     filename = 'unpairedTSSTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({unpairedTSS()}),
                             selection = 'single')
              
    ))
  
  output$flagtable9 <- DT::renderDataTable(
    datatable({verticals()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'verticalsTable'),
                                                list(extend ='excel',
                                                     filename = 'verticalsTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'landscape',
                                                     filename = 'verticalsTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(4, 'asc')),
                             pageLength = nrow({verticals()}),
                             selection = 'single')
              
    ))
  
  output$flagtable10 <- DT::renderDataTable(
    datatable({methods()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'methodsTable'),
                                                list(extend ='excel',
                                                     filename = 'methodsTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A2',
                                                     orientation = 'landscape',
                                                     filename = 'methodsTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc')),
                             pageLength = nrow({methods()}),
                             selection = 'single')
              
    ))
  
  output$flagtable11 <- DT::renderDataTable(
    datatable({status()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'statusTable'),
                                                list(extend ='excel',
                                                     filename = 'statusTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A3',
                                                     orientation = 'landscape',
                                                     filename = 'statusTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(2, 'desc'), list(3, 'desc')),
                             pageLength = nrow({status()}),
                             selection = 'single')
              
    ))
  
  output$flagtable12 <- DT::renderDataTable(
    datatable({noResult()},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'noResultTable'),
                                                list(extend ='excel',
                                                     filename = 'noResultTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A1',
                                                     orientation = 'landscape',
                                                     filename = 'noResultTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(7, 'asc')),
                             pageLength = nrow({noResult()}),
                             selection = 'single')
              
    ))
  
  
  #join flag table eith SiteData Table for plotting
  
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
  # autofillXplot2<-eventReactive(input$dataPull, {
  #   colnames(JoinTable(), do.NULL = FALSE)
  # })
  
  JoinTableSelect <- reactive({
    select(JoinTable2(), c("RECORD_NO", "SITE_NO", "STATION_NM", "SAMPLE_START_DT", "TurbFNU", "SC", "Qcfs", "SSC", "SandSilt","Bedload", "TSS")) #, "Turbidity..Form.Neph",
  })
  
  OutlierData <- reactive({
    left_join (JoinTableSelect(), outlier())
  })
  
  
  # Plotting routine
  
  
  
  # Outlier Plot ################################
  
  # x2 <- reactive({
  #   OutlierData()[,as.numeric(input$varx2)]
  # })
  # y2 <- reactive({
  #   OutlierData()[,as.numeric(input$vary2)]
  # })
  
  OutlierData2<-reactive({
    dfplot2<-OutlierData()
    dfplot2$xplot2<- OutlierData()[,as.numeric(input$varx2)]
    dfplot2$yplot2<- OutlierData()[,as.numeric(input$vary2)]
    return(dfplot2)
  })
  
  output$plot2 <- renderPlot({
    ggplot(data = OutlierData2(), aes(x=xplot2, y=yplot2)) + geom_point() + gghighlight((yplot2) > (quantile(yplot2, as.numeric(input$percentileHigh), na.rm = TRUE)), use_direct_label = input$outlierlabel, label_key = RECORD_NO) +
      xlab("X-axis Variable") +
      ylab("Y-axis Variable")
    
  })
  
  output$plot3 <- renderPlot({
    ggplot(data= OutlierData2(), aes(x=xplot2, y=yplot2)) + geom_point() + gghighlight((yplot2) < (quantile(yplot2, as.numeric(input$percentileLow), na.rm = TRUE)), use_direct_label = input$outlierlabel, label_key = RECORD_NO) +
      xlab("X-axis Variable") +
      ylab("Y-axis Variable")
  })
  
  # output$plot2info <-renderPrint({
  #   nearPoints(OutlierData(), input$plot2_dblclick, threshold = 10, maxpoints = 5, addDist = TRUE, allRows = TRUE)
  # })
  
  # Outlier Table info and Table
  
  # output$header <- renderText({
  #   summary(x2())
  # })
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
  
  
  ################################################ 
  # Time Series plots
  sedTS <- eventReactive(input$dataPull, {
    
    plot_sedTS(siteData())
    
  })
  
  output$TSplot1 <- renderPlot(sedTS()$SSC)
  
  output$TSplot2 <- renderPlot(sedTS()$ssbreak)
  
  output$TSplot3 <- renderPlot(sedTS()$SSL)
  
  output$TSplot4 <- renderPlot(sedTS()$bedload)
  
  output$TSplot5 <- renderPlot(sedTS()$bedmass)
  
  output$TSplot6 <- renderPlot(sedTS()$TSS)
  
  # Scatter Plot
  sedFlow <- eventReactive(input$dataPull, {
    
    plot_sedFlow(siteData())
    
  })
  
  turbSSC <- eventReactive(input$dataPull, {
    
    plot_turbSSC(siteData())
    
  })
  
  SSCTSS <- eventReactive(input$dataPull, {
    
    plot_SSCTSS(siteData())
    
  })
  
  output$Splot1 <- renderPlot(sedFlow()$SSC)
  
  output$Splot2 <- renderPlot(sedFlow()$ssbreak)
  
  output$Splot3 <- renderPlot(sedFlow()$bedload)
  
  output$Splot4 <- renderPlot(turbSSC()) #check with Colin about update call
  
  output$Splot5 <- renderPlot(sedFlow()$TSS)
  
  output$Splot6 <- renderPlot(SSCTSS()$scatter)
  
  
  # Boxplot
  
  ssctss <- eventReactive(input$dataPull, {
    
    plot_SSCTSS(siteData())
    
  })
  
  output$Bplot1 <- renderPlot(ssctss()$combined)
  
  output$Bplot2 <- renderPlot(ssctss()$SSC)
  
  output$Bplot3 <- renderPlot(ssctss()$TSS)
  
  #Rejected data pull routine  
  rejectedData <- eventReactive(input$dataPull, {
    # import data for your site using get_localNwis.
    rejDat <- get_localNWIS(DSN = input$DBName,            # NWIS server
                            env.db = input$env.db,           # environmental database number
                            qa.db = input$qa.db,            # QA database number
                            STAIDS = input$varSite,
                            begin.date = input$analysisBeginDT, # WY 2017
                            end.date = input$endDT,
                            approval = "Rejected")
    rejDat <- rejDat[rejDat$PARM_CD %in% c("80154",
                                           "70331",
                                           "00530",
                                           "80155",
                                           "80225",
                                           "91145"),]
    return(rejDat)
  })
  
  output$rejectedtable <- DT::renderDataTable(
    datatable({rejectedData()[, -c(1, 3, 6:7, 11, 15, 17:35, 43:47, 49:52, 54:56, 58:65, 70:72, 74:79)]},
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'rejectedDataTable'),
                                                list(extend ='excel',
                                                     filename = 'rejectedDataTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A1',
                                                     orientation = 'landscape',
                                                     filename = 'rejectedDataTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(1, 'asc')),
                             pageLength = nrow({rejectedData()}),
                             selection = 'single')
              
    ))
  
  # Merge streamflow from unit values
  # Build q dataset, reformat flow as numberic, format column names
  discharge <- eventReactive(input$qPull, {  
    raw.discharge <- readNWISdata(sites = input$varSite, # input$varSite
                                  service = "iv",
                                  parameterCd = "00060",
                                  startDate = as.POSIXct(input$analysisBeginDT, tz = input$tz),
                                  endDate = as.POSIXct(input$endDT, tz = input$tz),
                                  tz = input$tz)
    
    raw.discharge$X_00060_00000 <- as.numeric(raw.discharge$X_00060_00000)
    raw.discharge$FLOW <- raw.discharge$X_00060_00000
    raw.discharge$DATES <- raw.discharge$dateTime
    raw.discharge <-subset.data.frame(raw.discharge)
    return(raw.discharge)
  })
  
  
  MergeTable <- eventReactive(input$qPull, {
    mergeNearest(left = siteData(), dates.left = "SAMPLE_START_DT", right = discharge(), dates.right = "DATES", max.diff = "15 mins")
  })
  
  output$qtable <- DT::renderDataTable(
    datatable({MergeTable()}, #[which(MergeTable()$PARM_CD == "00061"), ] #c("RECORD_NO", "SAMPLE_START_DT", "PARM_NM", "RESULT_VA", "DQI_CD", "FLOW") 
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'MergeTableTable'),
                                                list(extend ='excel',
                                                     filename = 'MergeTableTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A0',
                                                     orientation = 'landscape',
                                                     filename = 'MergeTableTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             #order = list(list(4, 'desc'), list(2, 'asc')),
                             pageLength = nrow({MergeTable()}),
                             selection = 'single')
              
    ))
  
  # match cross section and point samples by time Box Coeff
  boxcoef <- eventReactive(input$boxPull, {
    raw.boxcoef <- find_boxcoef(siteData(), site_no = input$varSite, timediff = input$searchInterval, methods_NX = as.array(input$methods_NX), methods_X = as.array(input$methods_X))
    # raw.boxcoef$BoxCoef <- round((raw.boxcoef$RESULT_VA_xsection/raw.boxcoef$RESULT_VA_nonXS), 2)
    # raw.boxcoef$Date <- as.character(raw.boxcoef$SAMPLE_START_DT_xsection)
    raw.boxcoef1 <- raw.boxcoef[, c("RESULT_VA_nonXS", "method_nonXS", "RESULT_VA_xsection", "method_xsection",  "calc_box_coef", "QW_flow_cfs_xsection", "SAMPLE_START_DT_xsection")]
    return(raw.boxcoef1)
  })
  
  boxcoeftrim <- eventReactive(input$boxPull, {
    raw.boxcoef2 <- boxcoef()
    boxcoeftrim <- filter(raw.boxcoef2,SAMPLE_START_DT_xsection > as.POSIXct(input$analysisBeginDT, tz = input$tz))
    return(boxcoeftrim)
  })
  
  # boxcoefdisplay <- eventReactive(input$boxPull, {
  #   raw.boxcoef3 <- boxcoeftrim()
  #   boxcoefdisplay <- select(raw.boxcoef3, c(Site = SITE_NO, Date, BoxCoef , SSC_xsect = RESULT_VA_xsection, SSC_point = RESULT_VA_nonXS, streamflow = QW_flow_cfs_xsection))
  #   return(boxcoefdisplay)
  # })
  
  x <- reactive({
    boxcoef()[,as.numeric(input$varx)]
  })
  y <- reactive({
    boxcoef()[,as.numeric(input$vary)]
  })
  
  x1 <- reactive({
    serverTable$bx_data[,as.numeric(input$varx)]
  })
  y1 <- reactive({
    serverTable$bx_data[,as.numeric(input$vary)]
  })
  s <- reactive({
    as.numeric(input$abline)
  })
  output$plot1 <- renderPlot({
    ggplot() + geom_point(data = boxcoef(), aes(x(), y()), color = "black", size = 2) +
      geom_point(data = serverTable$bx_data, aes(x1(), y1()), color = "red", size = 2) +
      # geom_smooth(data = serverTable$bx_data, aes(x1(), y1()), method = lm, formula = y ~ 0 + x, fullrange = TRUE, color = "red", se = TRUE) +
      # geom_abline(slope = s(), intercept = 0, col = "black", lty = 2) +
      xlab("X-axis Variable") +
      ylab("Y-axis Variable")
    
  })
  
  output$plot1b <- renderPlot({
    ggplot() + geom_point(data = boxcoef(), aes(x = RESULT_VA_nonXS, y = RESULT_VA_xsection), color = "black", size = 2) +
      geom_point(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), color = "red", size = 2) +
      geom_smooth(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), method = lm, formula = y ~ 0 + x, fullrange = TRUE, color = "red", se = TRUE) +
      geom_abline(slope = s(), intercept = 0, col = "black", lty = 2) +
      xlab("Non-Cross Section SSC, mg/L") +
      ylab("Cross Section SSC, mg/L")
    
  })
  
  ######## Box Coeff Explorer Plot Interactions
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  #   }
  # 
  #   output$bxe_datadblclickinfo <- renderPrint({
  #     nearPoints(serverTable$bx_data, input$bxe_plot_dblclick, xvar = x1(), yvar = y1(), allRows = FALSE)
  #   })
  #   
  #   paste0(
  #     # "click: ", xy_str(input$plot_click),
  #     #"dblclick: ", xy_str(input$plot_dblclick),
  #     #"hover: ", xy_str(input$plot_hover),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  # })
  
  # output$boxtable <- DT::renderDataTable({
  #   boxcoefdisplay()
  #   })
  ############### Delete Row in table and Plot for Box Coeff Data Pull
  
  serverTable <- reactiveValues(bx_data = NULL)
  
  observeEvent(input$boxPull, {
    serverTable$bx_data <- boxcoeftrim()[, c("RESULT_VA_nonXS", "method_nonXS", "RESULT_VA_xsection", "method_xsection",  "calc_box_coef", "QW_flow_cfs_xsection", "SAMPLE_START_DT_xsection")] #need to add the rest that are needed and to change the plot number for Explorer in the input area - done
  })
  
  output$bx_datadblclickinfo <- renderPrint({
    nearPoints(serverTable$bx_data, input$bx_plot_dblclick, allRows = FALSE)
  })
  
  output$bx_datadblbrushinfo <- renderPrint({
    brushedPoints(serverTable$bx_data, input$bx_plot_brush)
  })
  
  output$mytable <- DT::renderDataTable(
    datatable(serverTable$bx_data,
              extensions = 'Buttons', 
              rownames = FALSE,
              options = list(dom = 'Bfrtip',
                             buttons = 
                               list('colvis', list(
                                 extend = 'collection',
                                 buttons = list(list(extend ='csv',
                                                     filename = 'BoxCoeffTable'),
                                                list(extend ='excel',
                                                     filename = 'BoxCoeffTable'),
                                                list(extend ='pdf',
                                                     pageSize = 'A4',
                                                     orientation = 'landscape',
                                                     filename = 'BoxCoeffTable')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,
                             scrollY = "600px",
                             order = list(list(6, 'asc')),
                             pageLength = nrow(serverTable$bx_data),
                             selection = 'single')
              
    ))
  
  observeEvent(input$delete_rows, {
    temp_bx <- serverTable$bx_data[-input$mytable_rows_selected,]
    serverTable$bx_data <- temp_bx
  })
  
  output$DelBoxPlot <- renderPlot({
    ggplot() + geom_point(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), color = "red", size = 2) ####need to remove the column callout and add variables - done
  })
  
  ####### Help text in Site-Level Assessment
  output$site <- renderText({
    c("Site:", input$varSite, "  Reference Period:", input$beginDT, "-", input$endDT, "    Analysis Period:",input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
    
  })
  
  ####### Help text in Science Center Assessment
  output$site2 <- renderText({
    c("State:", input$StateCd, "    Review Period:",input$reviewBeginDT, "-", input$reviewEndDT)
    
  })
  
  ####### Regression line
  output$model <- renderText({
    c("Site:", input$varSite, " Box Coeff",round(coef(lm(RESULT_VA_xsection~ 0+ RESULT_VA_nonXS, boxcoeftrim())), 2), "  Adjusted R^2:",round(summary(lm(RESULT_VA_xsection~ 0+ RESULT_VA_nonXS, boxcoeftrim()))$adj.r.squared, 3), "    Analysis Period:", input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
    
  })
  ###### stop app after exit
  session$onSessionEnded(function() {
    stopApp()
  })
}
