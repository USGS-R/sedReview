#### Site-Level Assessment: Box Coefficient explorer ####

######## Box Coeff Explorer Plot Interactions
# output$info <- renderText({
#   xy_str <- function(e) {
#     if(is.null(e)) return("NULL\n")
#     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
#   }
#   xy_range_str <- function(e) {
#     if(is.null(e)) return("NULL\n")
#     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
#            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
#   }
# 
#   output$bxe_datadblclickinfo <- renderPrint({
#     nearPoints(serverTable$bx_data, input$bxe_plot_dblclick, xvar = x1(), yvar = y1(), allRows = FALSE)
#   })
#   
#   paste0(
#     # "click: ", xy_str(input$plot_click),
#     #"dblclick: ", xy_str(input$plot_dblclick),
#     #"hover: ", xy_str(input$plot_hover),
#     "brush: ", xy_range_str(input$plot_brush)
#   )
# })

# output$boxtable <- DT::renderDataTable({
#   boxcoefdisplay()
#   })
############### Delete Row in table and Plot for Box Coeff Data Pull

serverTable <- reactiveValues(bx_data = NULL)

observeEvent(input$boxPull, {
  serverTable$bx_data <- boxcoeftrim()[, c("RESULT_VA_nonXS", "method_nonXS", "RESULT_VA_xsection", "method_xsection",  "calc_box_coef", "QW_flow_cfs_xsection", "SAMPLE_START_DT_xsection")] #need to add the rest that are needed and to change the plot number for Explorer in the input area - done
})

output$bx_datadblclickinfo <- renderPrint({
  nearPoints(serverTable$bx_data, input$bx_plot_dblclick, allRows = FALSE)
})

output$bx_datadblbrushinfo <- renderPrint({
  brushedPoints(serverTable$bx_data, input$bx_plot_brush)
})

output$mytable <- DT::renderDataTable(
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
                           order = list(list(6, 'asc')),
                           pageLength = nrow(serverTable$bx_data),
                           selection = 'single')
            
  ))

observeEvent(input$delete_rows, {
  temp_bx <- serverTable$bx_data[-input$mytable_rows_selected,]
  serverTable$bx_data <- temp_bx
})

output$DelBoxPlot <- renderPlot({
  ggplot() + geom_point(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), color = "red", size = 2) ####need to remove the column callout and add variables - done
})

####### Help text in Site-Level Assessment
output$site <- renderText({
  c("Site:", input$varSite, "  Reference Period:", input$beginDT, "-", input$endDT, "    Analysis Period:",input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
  
})

####### Help text in Science Center Assessment
output$site2 <- renderText({
  c("State:", input$StateCd, "    Review Period:",input$reviewBeginDT, "-", input$reviewEndDT)
  
})

####### Regression line
output$model <- renderText({
  c("Site:", input$varSite, " Box Coeff",round(coef(lm(RESULT_VA_xsection~ 0+ RESULT_VA_nonXS, boxcoeftrim())), 2), "  Adjusted R^2:",round(summary(lm(RESULT_VA_xsection~ 0+ RESULT_VA_nonXS, boxcoeftrim()))$adj.r.squared, 3), "    Analysis Period:", input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
  
})