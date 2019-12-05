#### Science-Center Review: box coefficient summary ####

SCLBoxCoeff<- eventReactive(input$reviewLoad, {
  summary_boxcoef(siteData_SCR, timediff = input$searchInterval2)
  
})


output$SCLBoxCoefftable <- DT::renderDataTable(
  datatable({SCLBoxCoeff()},
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
                           pageLength = nrow({SCLBoxCoeff()}),
                           selection = 'single')
            
  ))
