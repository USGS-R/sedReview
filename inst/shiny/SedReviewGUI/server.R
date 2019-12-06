server <- function(input, output, session) {
  
  #### Site-Level Assessment ####
  # data import
  source("server_SLA_1_dataImport.R", local = TRUE)$value
  # rejected data
  source("server_SLA_2_rejected.R", local = TRUE)$value
  # outlier explorer
  source("server_SLA_3_outlierExpl.R", local = TRUE)$value
  # data flags
  source("server_SLA_4_flags.R", local = TRUE)$value
  # plots
  source("server_SLA_5_plots.R", local = TRUE)$value
  # box coefficient pairs
  source("server_SLA_6_boxCoef.R", local = TRUE)$value
  # box coefficient explorer
  source("server_SLA_7_boxCoefExpl.R", local = TRUE)$value
  
  
  #### Science-Center Review ####
  # data import
  source("server_SCR_1_dataImport.R", local = TRUE)$value
  # map of active sediment sites
  source("server_SCR_2_mapActSed.R", local = TRUE)$value
  # map parameter code
  source("server_SCR_3_mapPcode.R", local = TRUE)$value
  # data flag summary
  source("server_SCR_4_flags.R", local = TRUE)$value
  # box coefficient summary
  source("server_SCR_5_boxCoefSumm.R", local = TRUE)$value
  

  #### SedReview Save/Load Session Data ####
  # save all current data
  observeEvent(input$savePrep,{
    sessionvalues <<- lapply(reactiveValuesToList(input), unclass)
    if(!is.null(sessionvalues)){
      output$saveInfo <- renderText({c("Session Prepared. OK to save.")})
    }else{output$saveInfo <- renderText({c("ERROR collecting user inputs. Please report issue.")})}
  })
  
  output$saveRData <- downloadHandler(
    filename = function() {paste0("SedReview_",Sys.time(),".rda")},
    content = function(file) {
    save.image(file = file)
    }
  )
  observeEvent(input$loadRData, {
    loadRDataFile <- input$loadRDataFile
    load(loadRDataFile$datapath,envir = .GlobalEnv)
    # load session info to app and re-render tables/plots
    # SLA
    ## Data Import
    updateTextInput(session, inputId = "DBName", value = sessionvalues$DBName)
    updateTextInput(session, inputId = "env.db", value = sessionvalues$env.db)
    updateTextInput(session, inputId = "qa.db", value = sessionvalues$qa.db)
    updateTextInput(session, inputId = "varSite", value = sessionvalues$varSite)
    updateTextInput(session, inputId = "beginDT", value = sessionvalues$beginDT)
    updateTextInput(session, inputId = "analysisBeginDT", value = sessionvalues$analysisBeginDT)
    updateTextInput(session, inputId = "endDT", value = sessionvalues$endDT)
    updateSelectInput(session, inputId = "tz", selected = sessionvalues$tz)
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
    ## Rejected Data
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
    
    ## Outlier Explorer
    updateSelectInput(session, inputId = "varxOut", selected = sessionvalues$varxOut)
    updateSelectInput(session, inputId = "varyOut", selected = sessionvalues$varyOut)
    updateSelectInput(session, inputId = "percentile", selected = sessionvalues$percentile)
    
    ## Flags - tables will populate on load session, no text options to update in this module tab
    ## Plots - plots will populate on load session, no text options to update in this module tab
    
    ## Box Coef data pull
    updateNumericInput(session, inputId = "searchInterval", value = sessionvalues$searchInterval)
    updateSelectInput(session, inputId = "methods_NX", selected = sessionvalues$methods_NX)
    updateSelectInput(session, inputId = "methods_X", selected = sessionvalues$methods_X)
    updateRadioButtons(session, inputId = "abline2", selected = sessionvalues$abline2)
    
    if(exists("boxcoef.trim")){
      serverTable$bx_data <- boxcoef.trim[, c("RESULT_VA_nonXS", 
                                              "method_nonXS", 
                                              "RESULT_VA_xsection", 
                                              "method_xsection",  
                                              "calc_box_coef", 
                                              "QW_flow_cfs_xsection", 
                                              "SAMPLE_START_DT_xsection")]
      names(serverTable$bx_data) <- c("NonXS.Pt.SSC",
                                      "NonXS.Pt.method",
                                      "X.sect.SSC",
                                      "X.sect.method",
                                      "calc.box.coef",
                                      "X.sect.Q.cfs",
                                      "SAMPLE_START_DT.X.sect")
    }
    
    
    # SCR
    ## Data Import
    updateTextInput(session, inputId = "DBName2", value = sessionvalues$DBName2)
    updateTextInput(session, inputId = "env.db2", value = sessionvalues$env.db2)
    updateTextInput(session, inputId = "qa.db2", value = sessionvalues$qa.db2)
    updateTextInput(session, inputId = "StateCd", value = sessionvalues$StateCd)
    updateTextInput(session, inputId = "reviewBeginDT", value = sessionvalues$reviewBeginDT)
    updateTextInput(session, inputId = "reviewEndDT", value = sessionvalues$reviewEndDT)
    updateTextInput(session, inputId = "endDT", value = sessionvalues$endDT)
    updateSelectInput(session, inputId = "tz", selected = sessionvalues$tz)
    output$centerSumtable <- DT::renderDataTable(
      datatable({SitesCount[, -c(8)]}, 
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
                               pageLength = nrow({SitesCount}),
                               selection = 'single')
                
      ))
    
    ## Map of active sed sites
    updateSelectInput(session, inputId = "mapWhat", sessionvalues$mapWhat)
    
    ## Pcode map
    updateTextInput(session, inputId = "searchWhat", value = sessionvalues$searchWhat)
    updateSelectInput(session, inputId = "sizeWhat", selected = sessionvalues$sizeWhat)
    updateSelectInput(session, inputId = "scale", selected = sessionvalues$scale)
    
    ## Data flag summary - tables will populate on load session, no text options to update in this module tab
    
    ## Box Coef summary
    updateNumericInput(session, inputId = "searchInterval2", value = sessionvalues$searchInterval2)
    
    if(exists("BoxCoeff.summary")){
      output$SCLBoxCoefftable <- DT::renderDataTable(
        datatable({BoxCoeff.summary},
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
                                 pageLength = nrow({BoxCoeff.summary}),
                                 selection = 'single')
                  
        ))
    }
    
    
    # Load message
    if(!is.null(sessionvalues)){
      output$loadInfo <- renderText({c("Session Loaded.")})
    }else{output$saveInfo <- renderText({c("ERROR loading. Please report issue.")})}
  })
  

  

  
  
  
  
  
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

  ###### stop app after exit
  session$onSessionEnded(function() {
    stopApp()
  })
}
