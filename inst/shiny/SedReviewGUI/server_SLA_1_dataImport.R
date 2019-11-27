#### Site-Level Assessment: Data import and import summary ####
# Data Pull routine
siteData <- eventReactive(input$dataPull, {
  
  # import data for your site using get_localNwis.
  get_localNWIS(DSN = input$DBName,            # NWIS server 
                env.db = input$env.db,          # environmental database number  
                qa.db = input$qa.db,            # QA database number 
                STAIDS = input$varSite,             
                begin.date = input$beginDT, 
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
