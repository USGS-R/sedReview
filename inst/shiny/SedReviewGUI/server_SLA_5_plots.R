#### Site-Level Assessment: Plots ####

# Time Series plots
sedTS <- eventReactive(input$dataPull, {
  
  plot_sedTS(siteData())
  
})

output$TSplot1 <- renderPlot(sedTS()$SSC)

output$TSplot2 <- renderPlot(sedTS()$ssbreak)

output$TSplot3 <- renderPlot(sedTS()$SSL)

output$TSplot4 <- renderPlot(sedTS()$bedload)

output$TSplot5 <- renderPlot(sedTS()$bedmass)

output$TSplot6 <- renderPlot(sedTS()$TSS)

# Scatter Plot
sedFlow <- eventReactive(input$dataPull, {
  
  plot_sedFlow(siteData())
  
})

turbSSC <- eventReactive(input$dataPull, {
  
  plot_turbSSC(siteData())
  
})

SSCTSS <- eventReactive(input$dataPull, {
  
  plot_SSCTSS(siteData())
  
})

output$Splot1 <- renderPlot(sedFlow()$SSC)

output$Splot2 <- renderPlot(sedFlow()$ssbreak)

output$Splot3 <- renderPlot(sedFlow()$bedload)

output$Splot4 <- renderPlot(turbSSC()) #check with Colin about update call

output$Splot5 <- renderPlot(sedFlow()$TSS)

output$Splot6 <- renderPlot(SSCTSS()$scatter)


# Boxplot

ssctss <- eventReactive(input$dataPull, {
  
  plot_SSCTSS(siteData())
  
})

output$Bplot1 <- renderPlot(ssctss()$combined)

output$Bplot2 <- renderPlot(ssctss()$SSC)

output$Bplot3 <- renderPlot(ssctss()$TSS)
