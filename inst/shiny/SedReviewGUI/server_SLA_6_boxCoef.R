#### Site-Level Assessment: Box Coefficient data pull ####

# match cross section and point samples by time Box Coeff
boxcoef <- eventReactive(input$boxPull,
                         {
                           boxcoef.all <<- NULL
                           boxcoef.all <<- find_boxcoef(siteData, 
                                                       site_no = input$varSite, 
                                                       timediff = input$searchInterval, 
                                                       methods_NX = as.array(input$methods_NX), 
                                                       methods_X = as.array(input$methods_X))
                           boxcoef.all <<- boxcoef.all[, c("RESULT_VA_nonXS", 
                                                           "method_nonXS", 
                                                           "RESULT_VA_xsection", 
                                                           "method_xsection",  
                                                           "calc_box_coef", 
                                                           "QW_flow_cfs_xsection", 
                                                           "SAMPLE_START_DT_xsection")]
                           
                           return(boxcoef.all)
                         })

boxcoeftrim <- eventReactive(input$boxPull,
                             {
                               boxcoef.trim <<- NULL
                               boxcoef.trim <<- boxcoef()
                               boxcoef.trim <<- filter(boxcoef.trim,
                                                     SAMPLE_START_DT_xsection > as.POSIXct(input$analysisBeginDT, tz = input$tz))
                               return(boxcoef.trim)
                             })


serverTable <- reactiveValues(bx_data = NULL)

observeEvent(input$boxPull, 
             {
               serverTable$bx_data <- boxcoeftrim()[, c("RESULT_VA_nonXS", 
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
             })

# output$bx_datadblclickinfo <- renderPrint({
#   nearPoints(serverTable$bx_data, input$bx_plot_dblclick, allRows = FALSE)
# })
# 
# output$bx_datadblbrushinfo <- renderPrint({
#   brushedPoints(serverTable$bx_data, input$bx_plot_brush)
# })

output$boxtable <- DT::renderDataTable(
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
                           order = list(list(0, 'asc')),
                           pageLength = nrow(serverTable$bx_data),
                           selection = 'single')
            
  ))

observeEvent(input$delete_rows, {
  temp_bx <- serverTable$bx_data[-input$boxtable_rows_selected,]
  serverTable$bx_data <- temp_bx
  boxcoef.trim <<- serverTable$bx_data
  names(boxcoef.trim) <<- names(boxcoef.all)
})

output$DelBoxPlot <- renderPlotly(
  {
    tryCatch({
      ggplotly(
        ggplot() + 
          geom_point(data = serverTable$bx_data, aes(NonXS.Pt.SSC, X.sect.SSC), color = "red", size = 2) +
          xlab("Non-Cross Section/Point sample SSC, mg/L") +
          ylab("Cross Section sample SSC, mg/L") 
      )
    }, error = function(e){
      return(message(simpleError("Generate Box Coefficient Pairs plot and table with time interval and method criteria on the left")))
    })
    
  })

