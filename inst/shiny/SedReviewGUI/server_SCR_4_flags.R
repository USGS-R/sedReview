#### Science-Center Review: Data flags summary ####

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