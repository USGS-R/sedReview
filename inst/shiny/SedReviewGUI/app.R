# Draft Version 0.16_14 of BoxCoeff tool. Author Cory A. Williams, 2019-02-06
################
library(shiny)
library(shinyBS)
library(ggplot2)
library(gghighlight)
library(sedReview)
library(dplyr)
library(stringr)
library(DT)
library(broom)
library(smwrBase)
library(dataRetrieval)
library(leaflet)
library(shinycssloaders)
################

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
  tabsetPanel(  
    
    tabPanel(title = "User Input and Summary",
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "DBName", label = "Please enter your ODBC Database connection name", value = "NWISCO"),
                 textInput(inputId = "env.db", label = "Please enter your database number of environmental samples", value = "01"),
                 textInput(inputId = "qa.db", label = "Please enter your database number of QA samples", value = "02"),
                 textInput(inputId = "varSite", label = "Please enter your 8- or 15-digit USGS station ID", value = "385626107212000"),
                 textInput(inputId = "beginDT", label = "Please enter starting date for reference period", value = "2012/10/01"),
                 textInput(inputId = "analysisBeginDT", label = "Please enter starting date for analysis period", value = "2016/10/01"),
                 bsTooltip("beginDT", "Reference period - Period of historical data. Sample results during the reference period will be shown on plots for comparison with sample results during the analysis period.", "right","hover", options = list(container = "body")),
                 bsTooltip("analysisBeginDT", "Analysis period - Period of interest for the site level assessment. Sample results from the analysis period will be shown on summary stat, sample count, and data flag tables as well as in plots.", "right","hover", options = list(container = "body")),
                 textInput(inputId = "endDT", label = "Please enter ending date for reference and analysis periods", value = "2017/09/30"),
                 selectInput(inputId = "tz", label = "Please select local time zone", list( "GMT", "America/New_York", "America/Chicago","America/Denver", "America/Phoenix", "America/Los_Angeles", "America/Anchorage", "America/Adak", "Pacific/Honolulu")),
                 actionButton(inputId = "dataPull", label = "Get data!")
               ),
               
               mainPanel(
                 h4("Summary Stats", align="center"), 
                 helpText("Data available for download using NWIS interface, below:"), helpText( a("NWIS link", target= "_blank", href= "https://nwis.waterdata.usgs.gov/nwis/qwdata?search_criteria=search_site_no&submitted_form=introduction")),
                 withSpinner(DT::dataTableOutput("sumtable"))
             )
           )
    ),

    
    tabPanel(title = "Rejected Sediment Data",
             h4("Rejected Sediment Data Table", align="center"),
             withSpinner(DT::dataTableOutput("rejectedtable"))
    ),
    
    tabPanel(title = "Outlier Explorer",
             sidebarLayout(
               sidebarPanel(
                 #numericInput(inputId = "searchInterval", label = "Please enter your search interval in hrs", value = "0.50"),
                 # actionButton(inputId = "outlierPull", label = "Plot Data!"),
                 #selectInput(inputId = "varx2", label = "Please select X variable from the dataset", choices=autofillXplot2),
                 selectInput(inputId = "varx2", label = "Please select X variable from the dataset", choices=c("Streamflow" = 7, "Date-Time" = 4, "Turbidity_FNU" = 5, "Specific conductance" = 6, "Suspended Sediment Concentration" = 8)),
                 selectInput(inputId = "vary2", label = "Please select Y variable from the dataset", choices=c("Suspended Sediment Concentration" = 8, "Sand/Silt-break percent finer" = 9, "Bedload" = 10)), #, "Total Suspended Solids" = 11
                 selectInput(inputId = "percentileHigh", label = "Please select 'greater than' percentile for outlier", choices=c("0.99", "0.95", "0.90", "0.85", "0.80")),
                 selectInput(inputId = "percentileLow", label = "Please select 'less than' percentile for outlier", choices=c("0.01", "0.05", "0.10", "0.15", "0.20")),
                 checkboxInput(inputId = "outlierlabel", label = "Add label to plot: RECORD_NO (labels are record#_database#).", value = FALSE)
               ),
               
               mainPanel(
                 #h4("Regression Fit, Y-intercept as zero", align="center"),
                 #verbatimTextOutput("model"),
                 h3("Outlier Plots", align="center"),
                 h5("These plots show potential outliers in the analysis period. Currently, outliers are defined as values above and below the selected percentiles for the y-axis variables."),
                 h4("Outlier greater than threshold", align="center"),
                 withSpinner(plotOutput("plot2", dblclick = "plot2_dblclick", height = 250)),
                 h4("Outlier less than threshold", align="center"),
                 withSpinner(plotOutput("plot3", dblclick = "plot2_dblclick", height = 250)),
                 
                 # verbatimTextOutput("plot2info"),
                 # verbatimTextOutput("header"),
                 
                 #h4("Plot info", align="center"),
                 #verbatimTextOutput("info"),
                 h4("Outlier Table", align="center"),
                 helpText("Tests whether sample results are outliers. Flags if an outlier. Currently looks at SSC, SSL, bedload loss on ignition, suspended sediment loss on ignition, and sand/silt break."),
                 withSpinner(DT::dataTableOutput("outlierTable"))
               )
              )
    ),

    navbarMenu("Data Flags and Summary",
    
    tabPanel(title = "Data flag summary",
             h4("Data flag summary", align="center"),
             helpText("This table summarizes samples that had one or more flags in the following data quality tests:"),
             helpText("bagIEFlags: Tests whether required  intake efficiency test parameters are reported when bag samplers are used (required as of June 2013 per OSW Memo 2013.03). Flags if missing."),
             helpText("CommentNoResultFlags: Returns a table with sample or analytical result comments and tests whether an analytical result is missing for a sample."),
             helpText("QFlags: Tests whether some measure of discharge is provided as metadata for a sample. Flags if missing."),
             helpText("metaDataFlags: Tests whether samples are missing required sample metadata, such as sample purpose, sampler type, sampling method, and type of associated QA with sample (if applicable). Includes tests in NWIS 20.xx level checks.For more information, see:", a("link to NWIS 20.xx level checks", target= "_blank", href= "http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check20-sql.html")),
             helpText("samplePurpFlags: Tests whether a sample purpose code (71999) differs from other samples collected at a site (there are certainly valid reasons for this, but just provides a reminder to check that these are correct)."),
             helpText("samplerTypeFlags: Tests whether a sampler type code (84164) differs from others reported. Flags if sampler type code is used 3 or fewer times (again, there are valid reasons for this, so just a check)."),
             helpText("sedMassFlags: Tests whether a sample has a sediment mass less than 2 mg. Flags if so. (Note: this is a new piece of metadata (pcode 91157) reported by USGS sediment laboratories so will not be available for historical data)."),
             helpText("tssFlags: Tests whether there is a TSS result without an accompanying SSC result, per OSW memo 01.03. Flags if TSS stands alone."),
             helpText("verticalsFlags: Tests whether sufficient verticals were sampled and reported (criteria are between 4 and 9 verticals for an EDI sample and between 10 and 20 verticals for an EWI samples)."),
             helpText("qaqcFlags: Tests whether samples coded as SSC, bedload, or bedload mass are stored in the QA database. Flags if a sediment sample is in the QA database. This is just a check to see if any sediment samples have been entered (manually or otherwise) into the QA database."),
             withSpinner(DT::dataTableOutput("flagtablesum"))
    ),
    
    tabPanel(title = "Sampling method summary",
             h4("Count of SSC sampling method, sampler type and bedload method", align="center"),
             helpText("[SSC_method_10 = EWI; SSC_method_15 = multiple verticals, non-isokinetic, equal widths and transit rates; SSC_method_20 = EDI;  SSC_method_40 = multiple verticals; SSC_method_70 = grab sample; SSC_sampler_100 = Van Dorn; SSC_sampler_3044 = DH-81; SSC_sampler_3045 = DH-81 with Teflon cap and nozzle; SSC_sampler_3051 = DH-95 Teflon bottle; SSC_sampler_3052 = DH-95 plastic bottle; SSC_sampler_3054 = D-95 plastic bottle; SSC_sampler_3055 = D-96; SSC_sampler_3071 = open-mouth bottle; SSC_sampler_8000 = none.]"), helpText( a("link to additional sampling codes", target= "_blank", href= "https://help.waterdata.usgs.gov/code/fixed_parms_query?fmt=html")),
             withSpinner(DT::dataTableOutput("flagtable10"))
    ),
    
    tabPanel(title = "Bag-sampler IE check",
             h4("Missing bag-sampler intake-efficency data", align="center"),
             helpText("Tests whether required  intake efficiency test parameters are reported when bag samplers are used (required as of June 2013 per OSW Memo 2013.03). Flags if missing."),
             withSpinner(DT::dataTableOutput("flagtable1"))
    ),
    
    tabPanel(title = "Sample/Result comments & Missing result check",
             h4("Table of Sample/Result comments and flags for missing analytical results", align="center"),
             helpText("Returns a table with sample or analytical result comments and tests whether an analytical result is missing for a sample."),
             withSpinner(DT::dataTableOutput("flagtable12"))
    ),
    
    tabPanel(title = "Streamflow check",
             h4("Missing associated streamflow data", align="center"),
             helpText("Tests whether some measure of discharge is provided as metadata for a sample. Flags if missing."),
             withSpinner(DT::dataTableOutput("flagtable2"))
    ),
    
    tabPanel(title = "Metadata check",
             h4("Flags of metadata fields", align="center"),
             helpText("Tests whether samples are missing required sample metadata, such as sample purpose, sampler type, sampling method, and type of associated QA with sample (if applicable). Includes tests in NWIS 20.xx level checks. For more information, see:", a("link to NWIS 20.xx level checks", target= "_blank", href= "http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check20-sql.html")),
             withSpinner(DT::dataTableOutput("flagtable3"))
    ),
    
    tabPanel(title = "Sample purpose check",
             h4("Samples not collected with most common purpose code", align="center"),
             helpText("Tests whether a sample purpose code (71999) differs from other samples collected at a site (there are certainly valid reasons for this, but just provides a reminder to check that these are correct)."),
             withSpinner(DT::dataTableOutput("flagtable5"))
    ),
    
    tabPanel(title = "Sampler type check",
             h4("Samples not collected with most common sampler type", align="center"),
             helpText("Tests whether a sampler type code (84164) differs from others reported. Flags if sampler type code is used 3 or fewer times (again, there are valid reasons for this, so just a check)."),
             withSpinner(DT::dataTableOutput("flagtable6"))
    ),
    
    tabPanel(title = "Sediment mass check",
             h4("Flag samples with sediment less than 2 milligrams", align="center"),
             helpText("Tests whether a sample has a sediment mass less than 2 mg. Flags if so. Note: this is a new piece of metadata (pcode 91157) reported by USGS sediment laboratories so will not be available for historical data."),
             withSpinner(DT::dataTableOutput("flagtable7"))
    ),
    
    tabPanel(title = "Unpaired-TSS samples",
             h4("Samples with TSS analysis without associated SSC data", align="center"),
             helpText("Tests whether there is a TSS result without an accompanying SSC result, per OSW memo 01.03. Flags if TSS stands alone."),
             withSpinner(DT::dataTableOutput("flagtable8"))
    ),
    
    tabPanel(title = "EWI/EDI verticals check",
             h4("EWI/EDI samples low/high/missing verticals", align="center"),
             helpText("Tests whether sufficient verticals were sampled and reported (criteria are between 4 and 9 verticals for an EDI sample and between 10 and 20 verticals for an EWI samples)"),
             withSpinner(DT::dataTableOutput("flagtable9"))
    ),
    
    tabPanel(title = "DQI status summary",
             h4("Summary of counts of sediment-associated data with DQI codes of A (historical data), S (presumed satisfactory), I (awaiting review), R (reviewed and accepted), Q (reviewed and rejected), P (proprietary, not reviewed), O (proprietary, reviewed and approved), X (proprietary, reviewed and rejected), and U (research/unapproved method or lab) when present in Reference and Analysis periods.", align="center"),
             #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
             withSpinner(DT::dataTableOutput("flagtable11"))
    ),
    tabPanel(title = "QAQC check",
             h4("Check for sediment samples that are in the QAQC database", align="center"),
             helpText("Tests whether samples coded as SSC, bedload, or bedload mass are stored in the QA database. Flags if a sediment sample is in the QA database.  Note: most qa/qc samples for sediment are NOT stored in the qa database because they are considered environmental samples. Storage of sediment replicates is coded by default in SedLogin in the main environmental database. This is just a check to see if any sediment samples have been entered (manually or otherwise) into the QA database."),
             withSpinner(DT::dataTableOutput("flagtable4"))
    )
    ),
    
    # tabPanel(title = "Merge Q",
    #          h4("Streamflow matching", align="left"),
    #          sidebarLayout(
    #            sidebarPanel(
    #              actionButton(inputId = "qPull", label = "Get streamflow!"),
    #              helpText("Typically completed in 1 to 3-minutes")
    #            ),
    #               
    #            mainPanel(
    #              h4("Streamflow Matching", align="center"),
    #              DT::dataTableOutput("qtable")
    #          )
    #        )
    # ),
    
    ##
    navbarMenu("Plots",
               
               tabPanel(title = "Time Series Plots",
                        h4("Time series plot of SSC (80154)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("TSplot1", height = 400)),
                        
                        h4("Time series plot of sand/silt break (70331)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("TSplot2", height = 400)),
                        
                        h4("Time series plot of SSL (80155)", align="center"),
                        helpText("Samples without an accompanying discharge will not appear in this plot"),
                        withSpinner(plotOutput("TSplot3", height = 250)),

                        h4("Time series plot of bedload (80225)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("TSplot4", height = 250)),

                        h4("Time series plot of bedload mass (91145)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("TSplot5", height = 250)),
                        
                        h4("Time series plot of total suspended solids (00530)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("TSplot6", height = 250))
                        
               ),
               
               tabPanel(title = "Scatter Plots",
                        h4("Scatter plot of SSC (80154) by discharge (00060, 00061, 30208, or 30209) - if more than one discharge parameter is available, plot by default 00060 or 00061", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot1", height = 400)),
                        
                        h4("Scatter plot of sand/silt break (70331) by discharge", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot2", height = 400)),
                        
                        h4("Scatter plot of bedload (80225) by discharge", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot3", height = 400)),
                        
                        h4("Scatter plot of SSC (80154) by turbidity (00076, 61028, 63675, 63676, 63677, 63679, 63680, 63681, 63682, 63683, 63684, 72188, 72208, 72209, 72213, 82079, OR 99872, whatever is available).", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot4", height = 400)),
                        
                        h4("Scatter plot of total suspended solids (00530) by discharge", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot5", height = 400)),
                        
                        h4("Scatter plot of total suspended solids (00530) by SSC (80154)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Splot6", height = 400))
               ),
               
               tabPanel(title = "Box Plots",
                        h4("Boxplots of TSS (00530) and SSC (80154)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Bplot1", height = 250)),
                        
                        h4("Boxplots of SSC (80154)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Bplot2", height = 250)),
                        
                        h4("Boxplots of TSS (00530)", align="center"),
                        #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                        withSpinner(plotOutput("Bplot3", height = 250))
               )
    ),
    

    ##
    tabPanel(title = "Box Coeff Data Pull",
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "searchInterval", label = "Please enter your search interval in hrs", value = "0.50", step = 0.25),
                 selectInput(inputId = "methods_NX", label = "Please enter Vector of Sampling Method for non-cross section/point samples", choices = c(30,40,50,55,70,100,900,920,940,4033,4080, "missing"), multiple = TRUE, selected = c(30,40,50,55,70,100,900,920,940,4033,4080)),
                 selectInput(inputId = "methods_X", label = "Please enter Vector of Sampling Method for cross section samples", choices = c(10,15,20), multiple = TRUE, selected = c(10,15,20)),
                 actionButton(inputId = "boxPull", label = "Get Box Coeff!"),
                 actionButton(inputId = "delete_rows", label = "Remove selected samples!"),
                 bsTooltip("methods_NX", "30 (single vertical), 40 (multiple verticals), 50 (point sample), 55 (composite - multiple point samples), 60 (weighted bottle), 70 (grab sample - dip), 100 (Van Dorn), 900 (SS pumping), 920 (SS BSV DI att), 930 (SS partial depth), 940 (SS partial width), 4033 (suction lift peristaltic), 4080 (peristaltic pump).", "right","hover", options = list(container = "body")),
                 bsTooltip("methods_X", "10 (EWI), 15 (multiple verticals non-isokinetic EWT), or 20 (EDI).", "right","hover", options = list(container = "body"))
                 
               ),
               
               mainPanel(
                 h4("Sample Pairs Box Coeff Table for the Analysis Period", align="center"),
                 plotOutput("DelBoxPlot", brush ="bx_plot_brush", dblclick = "bx_plot_dblclick", height = 400),
                 helpText("Point selection information (double, left-click):", align="center"),
                 verbatimTextOutput("bx_datadblclickinfo"),
                 helpText("Point selection information (left-click, and drag rectangle):", align="center"),
                 verbatimTextOutput("bx_datadblbrushinfo"),
                 withSpinner(DT::dataTableOutput("mytable"))
                 
               )
             )
  ),    
    
    tabPanel(title = "Box Coeff Explorer",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "varx", label = "Please select X variable from the dataset", choices=c("Streamflow" = 6, "Sample Date" = 7, "SSC xsection" = 3, "SSC nonXS" = 1)), #"RESULT_VA_nonXS", "method_nonXS", "RESULT_VA_xsection", "method_xsection",  "calc_box_coef", "QW_flow_cfs_xsection", "SAMPLE_START_DT_xsection"
      selectInput(inputId = "vary", label = "Please select Y variable from the dataset", choices=c("BoxCoef" = 5, "SSC xsection" = 3, "SSC nonXS" = 1)),
      radioButtons(inputId = "abline", label = "Add a 1:1 line to plot:", c("None" = "", "1:1" = "1"))
          ),

    mainPanel(
      h4("Box Coefficient Plot with Best-Fit Line", align="center"),
      helpText("Best-fit line (shown as red line) and 95th confidence-interval (shown as grey shading) shown in plot based on samples retained from within the analysis period (shown as red dots)."),
      helpText("Reference period samples and 'removed' samples from analysis period (shown as black dots)."),
      h6("Regression Fit, with Y-intercept set to zero", align="center"),
      verbatimTextOutput("model"),
      withSpinner(plotOutput("plot1b", height = 250)),

  # h4("double-Click info", align="center"),
  # verbatimTextOutput("bxe_datadblclickinfo"),
  # 
  # 
  # h4("Plot info", align="center"),
  # verbatimTextOutput("info"),
  
  h4("Exploratory Scatter Plot", align="center"),
  helpText("Samples retained from within the analysis period (shown as red dots) and samples from the reference period samples and 'removed' samples from analysis period (shown as black dots)."),
  
  withSpinner(plotOutput("plot1", height = 250))
  
      )
    )
  )
  )
  ),

tabPanel("Science-Center Review Module", helpText(h4(verbatimTextOutput("site2"))),
tabsetPanel(  
  
  tabPanel(title = "Reviewer Input and Summary",
           sidebarLayout(
             sidebarPanel(
               textInput(inputId = "DBName2", label = "Please enter your ODBC Database connection Name", value = "NWISCO"),
               textInput(inputId = "env.db2", label = "Please enter your database number of environmental samples", value = "01"),
               textInput(inputId = "qa.db2", label = "Please enter your database number of QA samples", value = "02"),
               textInput(inputId = "StateCd", label = "Please enter your State Code", placeholder = "CO"),
               #textInput(inputId = "beginDT2", label = "Please enter starting date for reference period", value = "2012/10/01"),
               textInput(inputId = "reviewBeginDT", label = "Please enter starting date for review period", value = "2016/10/01"),
               textInput(inputId = "reviewEndDT", label = "Please enter ending date for review period", value = "2017/09/30"),
               #selectInput(inputId = "tz2", label = "Please select local time zone", list( "GMT", "America/New_York", "America/Chicago","America/Denver", "America/Phoenix", "America/Los_Angeles", "America/Anchorage", "America/Adak", "Pacific/Honolulu")),
               actionButton(inputId = "reviewPull", label = "Review data!")
               
             ),
             
             mainPanel(
               h4("Science-Center Summary", align="center"),
               helpText("Table containing overview of sites with sediment data, how much, and review status organized by site, year, and data type, below"),
               helpText("Data available for download using NWIS interface, below:"), helpText( a("NWIS link", target= "_blank", href= "https://nwis.waterdata.usgs.gov/nwis/qwdata?search_criteria=search_site_no&submitted_form=introduction")),
               withSpinner(DT::dataTableOutput("centerSumtable"))
             )
           )
  ),
  
  tabPanel(title = "Map of active sediment sites",
           sidebarLayout(
             sidebarPanel(
               h4("Active Sediment Sites", align="center"),
               helpText("Map"),
           selectInput(inputId = "mapWhat", label = "Select data type to map", list("SSC_80154", "SandSilt_70331", "TSS_00530", "bedload_80225")),
           actionButton(inputId = "mapPlot", label = "Map sites!")#,
           #textInput(inputId = "map1SiteId", label = "Please enter your 8- or 15-digit USGS station id for table retrieval", placeholder = "07106500")
           
           ),
           
           mainPanel(
             withSpinner(leafletOutput("activeMap")),
           DT::dataTableOutput("map1Sitetable")
           )
           )
  ),
  
  tabPanel(title = "Map search of sites by pcode",
           sidebarLayout(
             sidebarPanel(
               h4("Pcode Site Search", align="center"),
               helpText( a("link to NWIS Pcodes", target= "_blank", href= "https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes")),
               textInput(inputId = "searchWhat", label = "Select pcode to map", value = "80154"),
               selectInput(inputId = "sizeWhat", label = "Marker size by:", list("count","mean", "median", "min", "max")),
               selectInput(inputId = "scale", label = "Scale marker size by", selected = "0", list ("0.0001", "0.001", "0.01", "0.1", "0", "1", "10", "100", "1000")),
               bsTooltip("scale", "Values less than zero decrease symbol size; Values greater than zero increase symbol size", "right","hover", options = list(container = "body")),
               actionButton(inputId = "mapPcode", label = "Map pcode sites!")
               # textInput(inputId = "map2SiteId", label = "Please enter your 8- or 15-digit USGS station id for table retrieval", placeholder = "07106500")
             ),
             
             mainPanel(
               withSpinner(leafletOutput("PcodeMap")),
               DT::dataTableOutput("map2Sitetable")
               #DT::dataTableOutput("map2Sampletable")
             )
  )
  ),
  
  ############################### SCL Data Flags
  navbarMenu("Data Flags and Summary",
             
             # tabPanel(title = "Data flag summary",
             #          h4("Data flag summary", align="center"),
             #          helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
             #          DT::dataTableOutput("flagtablesum")
             # ),
             
             tabPanel(title = "Bag-sampler IE check",
                      h4("Missing bag-sampler intake-efficency data", align="center"),
                      helpText("Tests whether required  intake efficiency test parameters are reported when bag samplers are used (required as of June 2013 per OSW Memo 2013.03). Flags if missing."),
                      withSpinner(DT::dataTableOutput("SCLflagtable1"))
             ),
             
             tabPanel(title = "Streamflow check",
                      h4("Missing associated streamflow data", align="center"),
                      helpText("Tests whether some measure of discharge is provided as metadata for a sample. Flags if missing."),
                      withSpinner(DT::dataTableOutput("SCLflagtable2"))
             ),
             
             # tabPanel(title = "Metadata check",
             #          h4("Flags of metadata fields", align="center"),
             #          helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
             #          DT::dataTableOutput("SCLflagtable3")
             # ),
             
             tabPanel(title = "QAQC check",
                      h4("Check for sediment samples that are in the QAQC database", align="center"),
                      helpText("Tests whether samples coded as SSC, bedload, or bedload mass are stored in the QA database. Flags if a sediment sample is in the QA database.  (Note: most qa/qc samples for sediment are NOT stored in the qa database because they are considered environmental samples. Storage of sediment replicates is coded by default in SedLogin in the main environmental database. This is just a check to see if any sediment samples have been entered (manually or otherwise) into the QA database."),
                      withSpinner(DT::dataTableOutput("SCLflagtable4"))
             ),
             
             # tabPanel(title = "Sample purpose check",
             #          h4("Samples not collected with most common purpose code", align="center"),
             #          #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
             #          DT::dataTableOutput("SCLflagtable5")
             # ),
             # 
             # tabPanel(title = "Sampler type check",
             #          h4("Samples not collected with most common sampler type", align="center"),
             #          #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
             #          DT::dataTableOutput("SCLflagtable6")
             # ),
             
             tabPanel(title = "Sediment mass check",
                      h4("Flag samples with sediment less than 2 milligrams", align="center"),
                      helpText("Tests whether a sample has a sediment mass less than 2 mg. Flags if so. (Note: as of September 2018, sediment mass data (pcode 91157) are not yet reported by USGS sediment laboratories. Future samples will show in this table if mass is less than 2 mg)"),
                      withSpinner(DT::dataTableOutput("SCLflagtable7"))
             ),
             
             tabPanel(title = "Unpaired-TSS sample check",
                      h4("Samples with TSS analysis without associated SSC data", align="center"),
                      helpText("Tests whether there is a TSS result without an accompanying SSC result, per OSW memo 01.03. Flags if TSS stands alone."),
                      withSpinner(DT::dataTableOutput("SCLflagtable8"))
             ),
             
             tabPanel(title = "EWI/EDI verticals check",
                      h4("EWI/EDI samples low/high/missing verticals", align="center"),
                      helpText("Tests whether sufficient verticals were sampled and reported (criteria are between 4 and 9 verticals for an EDI sample and between 10 and 20 verticals for an EWI sample)."),
                      withSpinner(DT::dataTableOutput("SCLflagtable9"))
             ),
             
             tabPanel(title = "Sampling method summary",
                      h4("Count of SSC sampling method, sampler type and bedload method", align="center"),
                      helpText("[SSC_method_10 = EWI; SSC_method_15 = multiple verticals, non-isokinetic, equal widths and transit rates; SSC_method_20 = EDI;  SSC_method_40 = multiple verticals; SSC_method_70 = grab sample; SSC_sampler_100 = Van Dorn; SSC_sampler_3044 = DH-81; SSC_sampler_3045 = DH-81 with Teflon cap and nozzle; SSC_sampler_3051 = DH-95 Teflon bottle; SSC_sampler_3052 = DH-95 plastic bottle; SSC_sampler_3054 = D-95 plastic bottle; SSC_sampler_3055 = D-96; SSC_sampler_3071 = open-mouth bottle; SSC_sampler_8000 = none.]"), helpText( a("link to additional sampling codes", target= "_blank", href= "https://help.waterdata.usgs.gov/code/fixed_parms_query?fmt=html")),
                      withSpinner(DT::dataTableOutput("SCLflagtable10"))
             ),
             
             tabPanel(title = "DQI status summary",
                      h4("Summary of counts of sediment-associated data with DQI codes of Q (reviewed and rejected), R (reviewed and accepted), S (presumed satisfactory), and U (research/unapproved method or lab) when present in Review period.", align="center"),
                      #helpText("# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html"),
                      withSpinner(DT::dataTableOutput("SCLflagtable11"))
             )
  ),
  
  
  
  ###############################
  
  tabPanel(title = "Summary of Box Coef",
           h4("Box Coeff Data Summary", align="center"),
           helpText("Summary of data by site, and year for SSC box coefficients"),
           numericInput(inputId = "searchInterval2", label = "Please enter your search interval in hrs", value = "0.50", step = 0.25),
           actionButton(inputId = "reviewLoad", label = "Pull Summary!"),
           withSpinner(DT::dataTableOutput("SCLBoxCoefftable"))
  )
)
)
)



server <- function(input, output, session) {

  # Data Pull routine
 siteData <- eventReactive(input$dataPull, {
    
    # import data for your site using get_localNwis.
  get_localNWIS(DSN = input$DBName,            # NWIS server 
                  env.db = input$env.db,           # environmental database number  
                  qa.db = input$qa.db,            # QA database number 
                  STAIDS = input$varSite,             
                  begin.date = input$beginDT, # WY 2017
                  end.date = input$endDT)
   })

 sumStats <- eventReactive(input$dataPull, {
   sumStats <- calc_summaryStats(siteData())
   return(sumStats)
 })
 
   output$sumtable <- DT::renderDataTable(
     datatable({sumStats()},
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
                    pageLength = nrow({sumStats()}),
     selection = 'single')
     
   ))

       # Center-level data summary pull routine  
     CenterReviewData <- eventReactive(input$reviewPull, {
       # import data for your site using get_localNwis.
      SitesCount <- count_activeSed(DSN = input$DBName2,            # NWIS server
                     env.db = input$env.db2,           # environmental database number
                     begin.date = input$reviewBeginDT, 
                     end.date = input$reviewEndDT)
      SitesCount$SITE_NO <- trimws(SitesCount$SITE_NO)
      SitesCount$SITE_NO_STATION_NM <- paste0(SitesCount$SITE_NO, " - ", SitesCount$STATION_NM)
      return(SitesCount)
     })
     
     # output$centerSumtable <- DT::renderDataTable({datatable(
     #   CenterReviewData(), rownames = FALSE, options = list(scrollX = TRUE, scrollY = "600px"))
     # })
     ########################################################################################################################
     output$centerSumtable <- DT::renderDataTable(
       datatable({CenterReviewData()[, -c(8)]}, 
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
                                pageLength = nrow({CenterReviewData()}),
                                selection = 'single')
                 
       ))
     ########################################################################################################################
     ##### SCL Data Flags
     
     SCLsiteData <- eventReactive(input$reviewPull, {
       get_localNWIS(DSN = input$DBName2,            # Colorado NWIS server 
                     env.db = input$env.db2,
                     qa.db = input$qa.db2,
                     STAIDS = CenterReviewData()$SITE_NO,             
                     begin.date = input$reviewBeginDT, # WY 2016-2017
                     end.date = input$reviewEndDT,
                     approval = "All")
     })
     
     ############## Science-Center level Box Coeff Summary
     
     SCLBoxCoeff<- eventReactive(input$reviewLoad, {
       summary_boxcoef(SCLsiteData(), timediff = input$searchInterval2)
     
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
     
     ##############
     
     
     SCLbagIE <- eventReactive(input$reviewPull, {
       
       check_bagIE(SCLsiteData(), reviewSummary = TRUE)
       
     })
     
     SCLhasQ <- eventReactive(input$reviewPull, {

       check_Q(SCLsiteData(), reviewSummary = TRUE)

     })

     # SCLmetaData <- eventReactive(input$reviewPull, {
     # 
     #   check_metaData(SCLsiteData(), reviewSummary = TRUE)
     # 
     # })

     SCLqaqc <- eventReactive(input$reviewPull, {

       check_qaqcDB(SCLsiteData(), as.character(input$qa.db2), reviewSummary = TRUE)

     })

     # SCLpurp <- eventReactive(input$reviewPull, {
     # 
     #   check_samplePurp(SCLsiteData(), reviewSummary = TRUE)
     # 
     # })
     # 
     # SCLsampler <- eventReactive(input$reviewPull, {
     # 
     #   check_samplerType(SCLsiteData(), reviewSummary = TRUE)
     # 
     # })

     SCLsedMass <- eventReactive(input$reviewPull, {

       check_sedMass(SCLsiteData(), reviewSummary = TRUE)

     })

     SCLunpairedTSS <- eventReactive(input$reviewPull, {

       check_tss(SCLsiteData(), reviewSummary = TRUE)

     })

     SCLverticals <- eventReactive(input$reviewPull, {

       check_verticals(SCLsiteData(), reviewSummary = TRUE)

     })

     SCLmethods <- eventReactive(input$reviewPull, {

       count_methodsBySite(SCLsiteData())

     })

     SCLstatus <- eventReactive(input$reviewPull, {

       count_sampleStatus(SCLsiteData())

     })

     output$SCLflagtable1 <- DT::renderDataTable(
       datatable({SCLbagIE()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLbagIETable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLbagIETable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A2',
                                                        orientation = 'portrait',
                                                        filename = 'SCLbagIETable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLbagIE()}),
                                selection = 'single')
                 
       ))

     output$SCLflagtable2 <- DT::renderDataTable(
       datatable({SCLhasQ()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLmissingQTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLmissingQTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A4',
                                                        orientation = 'portrait',
                                                        filename = 'SCLmissingQTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLhasQ()}),
                                selection = 'single')
                 
       ))

     # output$SCLflagtable3 <- DT::renderDataTable({
     #   SCLmetaData()
     # })

     output$SCLflagtable4 <- DT::renderDataTable(
       datatable({SCLqaqc()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLqaqcTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLqaqcTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A3',
                                                        orientation = 'landscape',
                                                        filename = 'SCLqaqcTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLqaqc()}),
                                selection = 'single')
                 
       ))

     # output$SCLflagtable5 <- DT::renderDataTable({
     #   SCLpurp()
     # })
     # 
     # output$SCLflagtable6 <- DT::renderDataTable({
     #   SCLsampler()
     # })

     output$SCLflagtable7 <- DT::renderDataTable(
       datatable({SCLsedMass()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLsedMassTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLsedMassTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A2',
                                                        orientation = 'portrait',
                                                        filename = 'SCLsedMassTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLsedMass()}),
                                selection = 'single')
                 
       ))

     output$SCLflagtable8 <- DT::renderDataTable(
       datatable({SCLunpairedTSS()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLunpairedTSSTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLunpairedTSSTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A3',
                                                        orientation = 'portrait',
                                                        filename = 'SCLunpairedTSSTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLunpairedTSS()}),
                                selection = 'single')
                 
       ))

     output$SCLflagtable9 <- DT::renderDataTable(
       datatable({SCLverticals()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLverticalsTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLverticalsTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A2',
                                                        orientation = 'landscape',
                                                        filename = 'SCLverticalsTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLverticals()}),
                                selection = 'single')
                 
       ))

     output$SCLflagtable10 <- DT::renderDataTable(
       datatable({SCLmethods()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLmethodsTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLmethodsTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A0',
                                                        orientation = 'landscape',
                                                        filename = 'SCLmethodsTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLmethods()}),
                                selection = 'single')
                 
       ))

     output$SCLflagtable11 <- DT::renderDataTable(
       datatable({SCLstatus()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'SCLstatusTable'),
                                                   list(extend ='excel',
                                                        filename = 'SCLstatusTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A3',
                                                        orientation = 'portrait',
                                                        filename = 'SCLstatusTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(2, 'desc'), list(0, 'asc')),
                                pageLength = nrow({SCLstatus()}),
                                selection = 'single')
                 
       ))
     
     ################## Map of active sediment sites
     StateSites <- eventReactive(input$mapPlot, {
       readNWISsite(CenterReviewData()$SITE_NO)
     })
     
     
     ActiveSedSites <- eventReactive(input$mapPlot, {
       left_join(CenterReviewData(), unique(select(StateSites(), c(site_no, station_nm, dec_lat_va, dec_long_va, alt_va))), by = c("SITE_NO" = "site_no"))
     })
     
    MapActiveSites <- reactive({
      filter(ActiveSedSites(), get(input$mapWhat) >= 1) 
    })
     
     Sedmap <- reactive({ 
       
      leaflet(data=MapActiveSites()) %>%
         addProviderTiles("CartoDB.Positron",
                          options = providerTileOptions(noWrap = TRUE)) %>%
         addCircleMarkers(~dec_long_va,~dec_lat_va,
                          fillColor = "orange",
                          radius = 3,
                          fillOpacity = 0.8, opacity = 0.8,stroke=FALSE,
                          popup=~SITE_NO_STATION_NM)
     })
     
     Map1table <- reactive({ #https://blog.exploratory.io/filter-with-text-data-952df792c2ba
       ActiveSedSites()%>%
         select("SITE_NO", "STATION_NM", "WY", "SSC_80154", "SandSilt_70331", "TSS_00530", "bedload_80225")#%>%
         #filter(str_detect(SITE_NO, as.character(input$map1SiteId))) 
     })
     
     output$activeMap <- renderLeaflet({
       Sedmap()
     })
     
     output$map1Sitetable <- DT::renderDataTable(
       datatable({Map1table()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'MapActiveTable'),
                                                   list(extend ='excel',
                                                        filename = 'MapActiveTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A4',
                                                        orientation = 'portrait',
                                                        filename = 'MapActiveTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(0, 'asc')),
                                pageLength = nrow({Map1table()}),
                                selection = 'single')
                 
       ))
     
     ##################
     
     ################## Map of sites by pcode
     PcodeSearch <- eventReactive(input$mapPcode, {
       readNWISdata(stateCd = as.character(input$StateCd), parameterCd = as.character(input$searchWhat),
                    service="site", seriesCatalogOutput=TRUE)
     })
     
     PcodeSiteList <-eventReactive(input$mapPcode, {
       unique(PcodeSearch()$site_no)
     })
     
     PcodeSites <- reactive({
       PcodeSites <-unique.data.frame(select(PcodeSearch(), c("site_no", "station_nm", "dec_long_va", "dec_lat_va")))
       PcodeSites$SITE_NO_STATION_NM<- paste0(PcodeSites$site_no, " - ", PcodeSites$station_nm)
       return(PcodeSites)
     })
     
     PcodeData <- eventReactive(input$mapPcode, {
       readNWISqw(siteNumbers = as.character(PcodeSiteList()),
                             parameterCd = as.character(input$searchWhat),
                             startDate = as.character(input$reviewBeginDT))
     })
    
     SitesDataSummary <- eventReactive(input$mapPcode, {
       renameNWISColumns(PcodeData())%>%
         group_by(site_no) %>%
         summarise(count=n(),
                    start=min(startDateTime),
                    end=max(startDateTime),
                    mean=signif(mean(result_va, na.rm = TRUE), digits = 3),
                    min=min(result_va, na.rm = TRUE),
                    max=max(result_va, na.rm = TRUE),
                    median=median(result_va, na.rm = TRUE))
     })

        
     PcodeSitesMap <- eventReactive(input$mapPcode, {
       left_join(SitesDataSummary(), PcodeSites())
     })
     
     PcodeSitesMapFiltered <- reactive({
       filter(PcodeSitesMap(), as.numeric(PcodeSitesMap()$count) >= 1)
     })
     
     # withProgress - sets view of run-time on process window
     
     PcodeMap <- reactive({
       
       map2<- leaflet(data=PcodeSitesMapFiltered()) %>%
         addProviderTiles("CartoDB.Positron",
                          options = providerTileOptions(noWrap = TRUE)) %>%
         addCircleMarkers(~dec_long_va,~dec_lat_va,
                          fillColor = "orange",
                          radius = ~(get(input$sizeWhat))*(0.01*as.numeric(input$scale)),
                          fillOpacity = 0.8, opacity = 0.8,stroke=FALSE,
                          popup=~SITE_NO_STATION_NM)
       return(map2)
     })

     output$PcodeMap <- renderLeaflet({
       PcodeMap()
     })
     
     output$map2Sitetable <- DT::renderDataTable(
       datatable({PcodeSitesMapFiltered()},
                 extensions = 'Buttons', 
                 rownames = FALSE,
                 options = list(dom = 'Bfrtip',
                                buttons = 
                                  list('colvis', list(
                                    extend = 'collection',
                                    buttons = list(list(extend ='csv',
                                                        filename = 'PcodeSitesMapTable'),
                                                   list(extend ='excel',
                                                        filename = 'PcodeSitesMapTable'),
                                                   list(extend ='pdf',
                                                        pageSize = 'A3',
                                                        orientation = 'landscape',
                                                        filename = 'PcodeSitesMapTable')),
                                    text = 'Download'
                                  )),
                                scrollX = TRUE,
                                scrollY = "600px",
                                order = list(list(0, 'asc')),
                                pageLength = nrow({PcodeSitesMapFiltered()}),
                                selection = 'single')
                 
       ))
     
     # output$map2Sampletable <- DT::renderDataTable({
     #   PcodeData()
     # })
     
     ##################
   
   # Flagging routine
 checkAll <- eventReactive(input$dataPull, {
    
   check_all(subset.data.frame(siteData(), SAMPLE_START_DT>=as.POSIXct(input$analysisBeginDT, tz = input$tz)), qa.db = as.character(input$qa.db), returnAllTables = FALSE)
   
   })
     
  bagIE <- eventReactive(input$dataPull, {
       
       check_bagIE(siteData())
       
     })
  
  hasQ <- eventReactive(input$dataPull, {
    
    check_Q(siteData())
    
  })
  
  metaData <- eventReactive(input$dataPull, {
    
    check_metaData(siteData())
    
  })
  
  qaqc <- eventReactive(input$dataPull, {
    
    check_qaqcDB(siteData(), as.character(input$qa.db))
    
  })
  
  purp <- eventReactive(input$dataPull, {
    
    check_samplePurp(siteData())
    
  })
  
  sampler <- eventReactive(input$dataPull, {
    
    check_samplerType(siteData())
    
  })
  
  sedMass <- eventReactive(input$dataPull, {
    
    check_sedMass(siteData())
    
  })
  
  unpairedTSS <- eventReactive(input$dataPull, {
    
    check_tss(siteData())
    
  })
  
  verticals <- eventReactive(input$dataPull, {
    
    check_verticals(siteData())
    
  })
  
  methods <- eventReactive(input$dataPull, {
    
    count_methodsBySite(siteData())
    
  })
  
  status <- eventReactive(input$dataPull, {
    
    count_sampleStatus(siteData())
    
  })
  
  noResult <- eventReactive(input$dataPull, {
    
    check_commentsNoResult (siteData())
    
  })
  
  outlier <- reactive({
    
    find_outliers(siteData(), site_no = input$varSite, lowThreshold = as.numeric(input$percentileLow), highThreshold = as.numeric(input$percentileHigh))
    
  })

  
   output$flagtablesum <- DT::renderDataTable(
     datatable({checkAll()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'checkAllTable'),
                                                 list(extend ='excel',
                                                      filename = 'checkAllTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'checkAllTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({checkAll()}),
                              selection = 'single')
               
     ))
   
   output$flagtable1 <- DT::renderDataTable(
     datatable({bagIE()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'bagIETable'),
                                                 list(extend ='excel',
                                                      filename = 'bagIETable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'bagIETable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({bagIE()}),
                              selection = 'single')
               
     ))
   
   output$flagtable2 <- DT::renderDataTable(
     datatable({hasQ()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'missingQTable'),
                                                 list(extend ='excel',
                                                      filename = 'missingQTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'missingQTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({hasQ()}),
                              selection = 'single')
               
     ))
   
   output$flagtable3 <- DT::renderDataTable(
     datatable({metaData()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'metaDataTable'),
                                                 list(extend ='excel',
                                                      filename = 'metaDataTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A1',
                                                      orientation = 'landscape',
                                                      filename = 'metaDataTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({metaData()}),
                              selection = 'single')
               
     ))
   
   output$flagtable4 <- DT::renderDataTable(
     datatable({qaqc()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'qaqcTable'),
                                                 list(extend ='excel',
                                                      filename = 'qaqcTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A4',
                                                      orientation = 'landscape',
                                                      filename = 'qaqcTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({qaqc()}),
                              selection = 'single')
               
     ))
   
   output$flagtable5 <- DT::renderDataTable(
     datatable({purp()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'purpTable'),
                                                 list(extend ='excel',
                                                      filename = 'purpTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'purpTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({purp()}),
                              selection = 'single')
               
     ))
   
   output$flagtable6 <- DT::renderDataTable(
     datatable({sampler()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'samplerTable'),
                                                 list(extend ='excel',
                                                      filename = 'samplerTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'samplerTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({sampler()}),
                              selection = 'single')
               
     ))
   
   output$flagtable7 <- DT::renderDataTable(
     datatable({sedMass()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'sedMassTable'),
                                                 list(extend ='excel',
                                                      filename = 'sedMassTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A4',
                                                      orientation = 'landscape',
                                                      filename = 'sedMassTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({sedMass()}),
                              selection = 'single')
               
     ))
   
   output$flagtable8 <- DT::renderDataTable(
     datatable({unpairedTSS()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'unpairedTSSTable'),
                                                 list(extend ='excel',
                                                      filename = 'unpairedTSSTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A4',
                                                      orientation = 'landscape',
                                                      filename = 'unpairedTSSTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({unpairedTSS()}),
                              selection = 'single')
               
     ))
   
   output$flagtable9 <- DT::renderDataTable(
     datatable({verticals()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'verticalsTable'),
                                                 list(extend ='excel',
                                                      filename = 'verticalsTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A4',
                                                      orientation = 'landscape',
                                                      filename = 'verticalsTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(4, 'asc')),
                              pageLength = nrow({verticals()}),
                              selection = 'single')
               
     ))
   
   output$flagtable10 <- DT::renderDataTable(
     datatable({methods()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'methodsTable'),
                                                 list(extend ='excel',
                                                      filename = 'methodsTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A2',
                                                      orientation = 'landscape',
                                                      filename = 'methodsTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(2, 'desc')),
                              pageLength = nrow({methods()}),
                              selection = 'single')
               
     ))
   
   output$flagtable11 <- DT::renderDataTable(
     datatable({status()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'statusTable'),
                                                 list(extend ='excel',
                                                      filename = 'statusTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A3',
                                                      orientation = 'landscape',
                                                      filename = 'statusTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(2, 'desc'), list(3, 'desc')),
                              pageLength = nrow({status()}),
                              selection = 'single')
               
     ))
   
   output$flagtable12 <- DT::renderDataTable(
     datatable({noResult()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'noResultTable'),
                                                 list(extend ='excel',
                                                      filename = 'noResultTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A1',
                                                      orientation = 'landscape',
                                                      filename = 'noResultTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(7, 'asc')),
                              pageLength = nrow({noResult()}),
                              selection = 'single')
               
     ))
   
   
      #join flag table eith SiteData Table for plotting
   
   JoinTable <- eventReactive(input$dataPull, {
     make_wideTable(siteData())
   })
   
   ######### Make placeholder columns ###########
   
   JoinTable2 <- reactive({
     J2 <-JoinTable()
     if(length(J2$Turbidity..Form.Neph != 0)) {J2$TurbFNU <- as.numeric(J2$Turbidity..Form.Neph)}
                    else{J2$TurbFNU <- NA}
     
     if(length(J2$Specific.cond.at.25C != 0)) {J2$SC <- as.numeric(J2$Specific.cond.at.25C)}
                    else{J2$SC <- NA}
     
     if(length(J2$Discharge..instant. != 0)) {J2$Qcfs <- as.numeric(J2$Discharge..instant.)}
                    else{J2$Qcfs <- NA}
     
     if(length(J2$Suspnd.sedmnt.conc != 0)) {J2$SSC <- as.numeric(J2$Suspnd.sedmnt.conc)}
                    else{J2$SSC <- NA}
     
     if(length(J2$Sus.sed..0.0625mm.sd != 0)) {J2$SandSilt <- as.numeric(J2$Sus.sed..0.0625mm.sd)}
                    else{J2$SandSilt <- NA}
     
     if(length(J2$Bedload.sediment != 0)) {J2$Bedload <- as.numeric(J2$Bedload.sediment)}
                    else{J2$Bedload <- NA}
     
     if(length(J2$Suspended.solids != 0)) {J2$TSS <- as.numeric(J2$Suspended.solids)}
                    else{J2$TSS <- NA}
 
     return(J2)
   })
   
   
   ##############################################
   # autofillXplot2<-eventReactive(input$dataPull, {
   #   colnames(JoinTable(), do.NULL = FALSE)
   # })
   
   JoinTableSelect <- reactive({
     select(JoinTable2(), c("RECORD_NO", "SITE_NO", "STATION_NM", "SAMPLE_START_DT", "TurbFNU", "SC", "Qcfs", "SSC", "SandSilt","Bedload", "TSS")) #, "Turbidity..Form.Neph",
   })
   
   OutlierData <- reactive({
     left_join (JoinTableSelect(), outlier())
   })
   

   # Plotting routine
   
   
   
   # Outlier Plot ################################
   
   # x2 <- reactive({
   #   OutlierData()[,as.numeric(input$varx2)]
   # })
   # y2 <- reactive({
   #   OutlierData()[,as.numeric(input$vary2)]
   # })
   
   OutlierData2<-reactive({
     dfplot2<-OutlierData()
     dfplot2$xplot2<- OutlierData()[,as.numeric(input$varx2)]
     dfplot2$yplot2<- OutlierData()[,as.numeric(input$vary2)]
     return(dfplot2)
   })

   output$plot2 <- renderPlot({
     ggplot(data = OutlierData2(), aes(x=xplot2, y=yplot2)) + geom_point() + gghighlight((yplot2) > (quantile(yplot2, as.numeric(input$percentileHigh), na.rm = TRUE)), use_direct_label = input$outlierlabel, label_key = RECORD_NO) +
     xlab("X-axis Variable") +
     ylab("Y-axis Variable")

   })
   
   output$plot3 <- renderPlot({
     ggplot(data= OutlierData2(), aes(x=xplot2, y=yplot2)) + geom_point() + gghighlight((yplot2) < (quantile(yplot2, as.numeric(input$percentileLow), na.rm = TRUE)), use_direct_label = input$outlierlabel, label_key = RECORD_NO) +
       xlab("X-axis Variable") +
       ylab("Y-axis Variable")
   })
   
    # output$plot2info <-renderPrint({
    #   nearPoints(OutlierData(), input$plot2_dblclick, threshold = 10, maxpoints = 5, addDist = TRUE, allRows = TRUE)
    # })

  # Outlier Table info and Table

    # output$header <- renderText({
    #   summary(x2())
    # })
   output$outlierTable <- DT::renderDataTable(
     datatable({OutlierData()},
               extensions = 'Buttons', 
               rownames = FALSE,
               options = list(dom = 'Bfrtip',
                              buttons = 
                                list('colvis', list(
                                  extend = 'collection',
                                  buttons = list(list(extend ='csv',
                                                      filename = 'OutlierDataTable'),
                                                 list(extend ='excel',
                                                      filename = 'OutlierDataTable'),
                                                 list(extend ='pdf',
                                                      pageSize = 'A1',
                                                      orientation = 'landscape',
                                                      filename = 'OutlierDataTable')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,
                              scrollY = "600px",
                              order = list(list(3, 'asc')),
                              pageLength = nrow({OutlierData()}),
                              selection = 'single')
               
     ))
   
   
  ################################################ 
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
   
   #Rejected data pull routine  
 rejectedData <- eventReactive(input$dataPull, {
   # import data for your site using get_localNwis.
     rejDat <- get_localNWIS(DSN = input$DBName,            # NWIS server
                   env.db = input$env.db,           # environmental database number
                   qa.db = input$qa.db,            # QA database number
                   STAIDS = input$varSite,
                   begin.date = input$analysisBeginDT, # WY 2017
                   end.date = input$endDT,
                   approval = "Rejected")
     rejDat <- rejDat[rejDat$PARM_CD %in% c("80154",
                                            "70331",
                                            "00530",
                                            "80155",
                                            "80225",
                                            "91145"),]
     return(rejDat)
   })
   
   output$rejectedtable <- DT::renderDataTable(
     datatable({rejectedData()[, -c(1, 3, 6:7, 11, 15, 17:35, 43:47, 49:52, 54:56, 58:65, 70:72, 74:79)]},
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
                              pageLength = nrow({rejectedData()}),
                              selection = 'single')
               
     ))
   
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
  ###### stop app after exit
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)