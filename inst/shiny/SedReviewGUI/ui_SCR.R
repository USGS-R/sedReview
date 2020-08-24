#### UI for Science Center Review Module ####

tabsetPanel(  
  
  tabPanel(title = "Reviewer Input and Summary",
           sidebarLayout(
             sidebarPanel(
               actionButton(inputId = "loadDBinfo2", label = "Load previously saved NWIS DB info"),
               actionButton(inputId = "loadDB_SLA", label = "Load DB Info from SLA Module"),br(),
               actionButton(inputId = "saveDBinfo2", label = "Save NWIS DB info (must enter values in top 3 boxes first)"),br(),br(),
               textInput(inputId = "DBName2", label = "Please enter your ODBC Database connection Name", placeholder =  "NWISCO"),
               textInput(inputId = "env.db2", label = "Please enter your database number of environmental samples", placeholder = "01"),
               textInput(inputId = "qa.db2", label = "Please enter your database number of QA samples", placeholder = "02"),
               textInput(inputId = "StateCd", label = "Please enter your State Code", placeholder = "CO"),
               textInput(inputId = "reviewBeginDT", label = "Please enter starting date for review period", placeholder = "YYYY/MM/DD"),
               textInput(inputId = "reviewEndDT", label = "Please enter ending date for review period", placeholder = "YYYY/MM/DD"),
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
                      h4("Flag samples with Sediment Mass less than 2 milligrams", align="center"),
                      helpText("Tests whether a sample has a SEDIMENT MASS (p91157) less than 2 mg. Flags if so. (Note: as of September 2018, sediment mass data (pcode 91157) are not yet reported by USGS sediment laboratories. Future samples will show in this table if mass is less than 2 mg, see: ", 
                               a("WMA Tech Note 41)", target="_blank", href="https://doimspp.sharepoint.com/sites/usgs-water-mission-area/Lists/news_technote/Flat.aspx?RootFolder=%2Fsites%2Fusgs%2Dwater%2Dmission%2Darea%2FLists%2Fnews%5Ftechnote%2FWMA%20Technical%20Note%20Number%2041%20Availability%20of%20new%20metadata%20for%20sediment%20analyses%20%28sample%20mass%29&FolderCTID=0x01200200501EAAFB86995C4DA98D3E9D5C759CFE")),
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