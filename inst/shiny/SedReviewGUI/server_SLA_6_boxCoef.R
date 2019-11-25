#### Site-Level Assessment: Box Coefficient data pull ####

# match cross section and point samples by time Box Coeff
boxcoef <- eventReactive(input$boxPull, {
  raw.boxcoef <- find_boxcoef(siteData(), site_no = input$varSite, timediff = input$searchInterval, methods_NX = as.array(input$methods_NX), methods_X = as.array(input$methods_X))
  # raw.boxcoef$BoxCoef <- round((raw.boxcoef$RESULT_VA_xsection/raw.boxcoef$RESULT_VA_nonXS), 2)
  # raw.boxcoef$Date <- as.character(raw.boxcoef$SAMPLE_START_DT_xsection)
  raw.boxcoef1 <- raw.boxcoef[, c("RESULT_VA_nonXS", "method_nonXS", "RESULT_VA_xsection", "method_xsection",  "calc_box_coef", "QW_flow_cfs_xsection", "SAMPLE_START_DT_xsection")]
  return(raw.boxcoef1)
})

boxcoeftrim <- eventReactive(input$boxPull, {
  raw.boxcoef2 <- boxcoef()
  boxcoeftrim <- filter(raw.boxcoef2,SAMPLE_START_DT_xsection > as.POSIXct(input$analysisBeginDT, tz = input$tz))
  return(boxcoeftrim)
})

# boxcoefdisplay <- eventReactive(input$boxPull, {
#   raw.boxcoef3 <- boxcoeftrim()
#   boxcoefdisplay <- select(raw.boxcoef3, c(Site = SITE_NO, Date, BoxCoef , SSC_xsect = RESULT_VA_xsection, SSC_point = RESULT_VA_nonXS, streamflow = QW_flow_cfs_xsection))
#   return(boxcoefdisplay)
# })

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
output$plot1 <- renderPlot({
  ggplot() + geom_point(data = boxcoef(), aes(x(), y()), color = "black", size = 2) +
    geom_point(data = serverTable$bx_data, aes(x1(), y1()), color = "red", size = 2) +
    # geom_smooth(data = serverTable$bx_data, aes(x1(), y1()), method = lm, formula = y ~ 0 + x, fullrange = TRUE, color = "red", se = TRUE) +
    # geom_abline(slope = s(), intercept = 0, col = "black", lty = 2) +
    xlab("X-axis Variable") +
    ylab("Y-axis Variable")
  
})

output$plot1b <- renderPlot({
  ggplot() + geom_point(data = boxcoef(), aes(x = RESULT_VA_nonXS, y = RESULT_VA_xsection), color = "black", size = 2) +
    geom_point(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), color = "red", size = 2) +
    geom_smooth(data = serverTable$bx_data, aes(RESULT_VA_nonXS, RESULT_VA_xsection), method = lm, formula = y ~ 0 + x, fullrange = TRUE, color = "red", se = TRUE) +
    geom_abline(slope = s(), intercept = 0, col = "black", lty = 2) +
    xlab("Non-Cross Section SSC, mg/L") +
    ylab("Cross Section SSC, mg/L")
  
})
