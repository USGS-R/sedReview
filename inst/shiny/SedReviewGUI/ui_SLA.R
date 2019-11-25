#### UI for Site-Level Assessment Module ####
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
