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


####### Help text in Site-Level Assessment
output$site <- renderText({
  c("Site:", input$varSite, "  Reference Period:", input$beginDT, "-", input$endDT, "    Analysis Period:",input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
  
})

####### Help text in Science Center Assessment
output$site2 <- renderText({
  c("State:", input$StateCd, "    Review Period:",input$reviewBeginDT, "-", input$reviewEndDT)
  
})

####### Regression line
boxlm <- reactive({
  boxlinreg <<- NULL
  boxlinreg <<- lm(X.sect.SSC ~ 0 + NonXS.Pt.SSC, data = serverTable$bx_data)
  return(boxlinreg)
})

output$model <- renderText({
  c("Site:", input$varSite, " Box Coeff:",round(summary(boxlm())$coefficients[1], 2), "  Adjusted R^2:", round(summary(boxlm())$adj.r.squared, 3), "    Analysis Period:", input$analysisBeginDT, "-", input$endDT, "(", input$tz, ")" )
  
})

x <- reactive({
  boxcoef()[,as.numeric(input$varx)]
})
y <- reactive({
  boxcoef()[,as.numeric(input$vary)]
})

x1 <- reactive({
  serverTable$bx_data[,as.numeric(input$varx)]
})
y1 <- reactive({
  serverTable$bx_data[,as.numeric(input$vary)]
})
s <- reactive({
  as.numeric(input$abline)
})

output$plot1_static <- renderPlotly({
  ggplotly(
    ggplot() + 
      geom_point(data = boxcoef(), aes(x = RESULT_VA_nonXS, y = RESULT_VA_xsection), color = "black", size = 2) +
      geom_point(data = serverTable$bx_data, aes(NonXS.Pt.SSC, X.sect.SSC), color = "red", size = 2) +
      geom_smooth(data = serverTable$bx_data, aes(NonXS.Pt.SSC, X.sect.SSC), method = lm, formula = y ~ 0 + x, fullrange = TRUE, color = "red", se = TRUE) +
      geom_abline(slope = s(), intercept = 0, col = "black", lty = 2) +
      xlab("Non-Cross Section/Point Sample SSC, mg/L") +
      ylab("Cross Section Sample SSC, mg/L")
  )
})


output$plot1 <- renderPlotly({
  ggplotly(
    ggplot() + 
      geom_point(data = boxcoef(), aes(x(), y()), color = "black", size = 2) +
      geom_point(data = serverTable$bx_data, aes(x1(), y1()), color = "red", size = 2) +
      xlab(names(serverTable$bx_data[as.numeric(input$varx)])) +
      ylab(names(serverTable$bx_data[as.numeric(input$vary)]))
  )
})

