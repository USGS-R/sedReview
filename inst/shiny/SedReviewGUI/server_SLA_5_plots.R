#### Site-Level Assessment: Plots ####

# Time Series plots
sedTS <- eventReactive(input$dataPull, {
  
  plot_sedTS(siteData)
  
})

output$TSplot1 <- renderPlotly(sedTS()$SSC)

output$TSplot2 <- renderPlotly(sedTS()$ssbreak)

output$TSplot3 <- renderPlotly(sedTS()$SSL)

output$TSplot4 <- renderPlotly(sedTS()$bedload)

output$TSplot5 <- renderPlotly(sedTS()$bedmass)

output$TSplot6 <- renderPlotly(sedTS()$TSS)

# Scatter Plot
sedFlow <- eventReactive(input$dataPull, {
  
  plot_sedFlow(siteData)
  
})

turbSSC <- eventReactive(input$dataPull, {
  
  plot_turbSSC(siteData)
  
})

SSCTSS <- eventReactive(input$dataPull, {
  
  plot_SSCTSS(siteData)
  
})

output$Splot1 <- renderPlotly(sedFlow()$SSC)

output$Splot2 <- renderPlotly(sedFlow()$ssbreak)

output$Splot3 <- renderPlotly(sedFlow()$bedload)

output$Splot4 <- renderPlotly(turbSSC()) #check with Colin about update call

output$Splot5 <- renderPlotly(sedFlow()$TSS)

output$Splot6 <- renderPlotly(SSCTSS()$scatter)


# Boxplot

ssctss <- eventReactive(input$dataPull, {
  
  plot_SSCTSS(siteData)
  
})

output$Bplot1 <- renderPlot(ssctss()$combined)

output$Bplot2 <- renderPlot(ssctss()$SSC)

output$Bplot3 <- renderPlot(ssctss()$TSS)
