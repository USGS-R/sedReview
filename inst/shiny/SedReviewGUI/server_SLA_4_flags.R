#### Site-Level Assessment: Data flags and flag summary ####


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
