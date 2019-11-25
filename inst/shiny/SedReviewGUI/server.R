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
