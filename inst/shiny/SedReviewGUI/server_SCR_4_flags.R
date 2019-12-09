#### Science-Center Review: Data flags summary ####

SCLbagIE <- eventReactive(list(input$reviewPull, input$loadRData), {check_bagIE(siteData_SCR, reviewSummary = TRUE)})

SCLhasQ <- eventReactive(list(input$reviewPull, input$loadRData), {check_Q(siteData_SCR, reviewSummary = TRUE)})

# SCLmetaData <- eventReactive(list(input$reviewPull, input$loadRData), {check_metaData(siteData_SCR, reviewSummary = TRUE)})

SCLqaqc <- eventReactive(list(input$reviewPull, input$loadRData), {
  check_qaqcDB(siteData_SCR, as.character(input$qa.db2), reviewSummary = TRUE)})

# SCLpurp <- eventReactive(list(input$reviewPull, input$loadRData), {check_samplePurp(siteData_SCR, reviewSummary = TRUE)})
# 
# SCLsampler <- eventReactive(list(input$reviewPull, input$loadRData), {check_samplerType(siteData_SCR, reviewSummary = TRUE)})

SCLsedMass <- eventReactive(list(input$reviewPull, input$loadRData), {check_sedMass(siteData_SCR, reviewSummary = TRUE)})

SCLunpairedTSS <- eventReactive(list(input$reviewPull, input$loadRData), {check_tss(siteData_SCR, reviewSummary = TRUE)})

SCLverticals <- eventReactive(list(input$reviewPull, input$loadRData), {check_verticals(siteData_SCR, reviewSummary = TRUE)})

SCLmethods <- eventReactive(list(input$reviewPull, input$loadRData), {count_methodsBySite(siteData_SCR)})

SCLstatus <- eventReactive(list(input$reviewPull, input$loadRData), {count_sampleStatus(siteData_SCR)})

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