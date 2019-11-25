#### Science-Center Review: Data import and summary ####

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

SCLsiteData <- eventReactive(input$reviewPull, {
  get_localNWIS(DSN = input$DBName2,            # Colorado NWIS server 
                env.db = input$env.db2,
                qa.db = input$qa.db2,
                STAIDS = CenterReviewData()$SITE_NO,             
                begin.date = input$reviewBeginDT, # WY 2016-2017
                end.date = input$reviewEndDT,
                approval = "All")
})