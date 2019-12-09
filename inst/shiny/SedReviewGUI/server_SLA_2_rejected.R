#### Site-Level Assessment: Rejected data ####
#Rejected data pull routine  
observeEvent(input$dataPull, {
  # import data for your site using get_localNwis.
  rejectedData <<- NULL
  rejectedData <<- get_localNWIS(DSN = input$DBName,            # NWIS server
                                 env.db = input$env.db,           # environmental database number
                                 qa.db = input$qa.db,            # QA database number
                                 STAIDS = input$varSite,
                                 begin.date = input$analysisBeginDT, # WY 2017
                                 end.date = input$endDT,
                                 approval = "Rejected")
  rejectedData <<- rejectedData[rejectedData$PARM_CD %in% c("80154",
                                                            "70331",
                                                            "00530",
                                                            "80155",
                                                            "80225",
                                                            "91145"),]
  output$rejectedtable <- DT::renderDataTable(
    datatable({rejectedData[, -c(1, 3, 6:7, 11, 15, 17:35, 43:47, 49:52, 54:56, 58:65, 70:72, 74:79)]},
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
                             pageLength = nrow({rejectedData}),
                             selection = 'single')
              
    ))
})


