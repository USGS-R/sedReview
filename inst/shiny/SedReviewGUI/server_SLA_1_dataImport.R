#### Site-Level Assessment: Data import and import summary ####
# Save/load database info
observeEvent(input$loadDBinfo,{
  appDir <- system.file("shiny", "SedReviewGUI",package = "sedReview")
  DBInfo <- readRDS(file = paste0(appDir,"/DBinfo.RDS"))
  updateTextInput(session, inputId = "DBName", value = DBInfo$DSN)
  updateTextInput(session, inputId = "env.db", value = DBInfo$env.db)
  updateTextInput(session, inputId = "qa.db", value = DBInfo$qa.db)
})
observeEvent(input$saveDBinfo,{
  DBInfo <- list()
  DBInfo$DSN <- input$DBName
  DBInfo$env.db <- input$env.db          
  DBInfo$qa.db <- input$qa.db
  appDir <- system.file("shiny", "SedReviewGUI",package = "sedReview")
  saveRDS(DBInfo, file = paste0(appDir,"/DBinfo.RDS"))
})
# Load DB info from SCR module
observeEvent(input$loadDB_SCR, {
  updateTextInput(session, inputId = "DBName", value = input$DBName2)
  updateTextInput(session, inputId = "env.db", value = input$env.db2)
  updateTextInput(session, inputId = "qa.db", value = input$qa.db2)
})

# Data Pull routine
observeEvent(input$dataPull, {
  
  siteData <<- NULL
  importInfo <<- list()
  sumStats <<- NULL
  
  # import data for your site using get_localNwis.
  siteData <<- get_localNWIS(DSN = input$DBName, # NWIS server 
                             env.db = input$env.db,           # environmental database number  
                             qa.db = input$qa.db,             # QA database number 
                             STAIDS = input$varSite,             
                             begin.date = input$beginDT, 
                             end.date = input$endDT)
  importInfo$DSN <<- input$DBName
  importInfo$env.db <<- input$env.db          
  importInfo$qa.db <<- input$qa.db
  importInfo$STAIDS <<- input$varSite      
  importInfo$begin.date <<- input$beginDT
  importInfo$analysis.begin.date <<- input$analysisBeginDT
  importInfo$end.date <<- input$endDT
  
  sumStats <<- calc_summaryStats(siteData)
  output$sumtable <- DT::renderDataTable(
    datatable({sumStats},
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
                             pageLength = nrow({sumStats}),
                             selection = 'single')
              
    ))
})

