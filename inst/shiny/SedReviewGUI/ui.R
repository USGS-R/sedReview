ui <- navbarPage(
  titlePanel(title = NULL, windowTitle = "SedReview v1.0"),
  tabPanel(title = img(src="Logo.png", width="60px",height = "20px"),
           h3("Welcome to SedReview v1.0: Discrete sediment data review and exploration toolbox."),
           h4("Please use the 'Site-Level Assessment Module' to review and plot data from a specific site."),
           h4("Please use the 'Science-Center Review Module' to perform high-level data reviews for an entire Water Science Center."),
           h4(helpText(a('User Guide', href="sedReview_manual.html",target="_blank"))),
           h4(helpText(a("Additional Info: SedReview 1.0 GitHub",
                         href="https://github.com/USGS-R/sedReview",target="_blank")))
  ),
  
  
  tabPanel("Site-Level Assessment Module", helpText(h4(verbatimTextOutput("site"))),
           source("ui_SLA.R", local = TRUE)$value),
  
  tabPanel("Science-Center Review Module", helpText(h4(verbatimTextOutput("site2"))),
           source("ui_SCR.R", local = TRUE)$value)
)
