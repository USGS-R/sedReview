# load the sedReview R package
library(sedReview)
data("exampleData2", package = "sedReview")

# This portion of the code is intended for Project Chiefs who want to review data for specific sites under their control.
########################################
## Project Chief Review
## for review of a single site or several sites operated by a project chief
## for any created dataframe (table of data or results), click in the upper left Environment tab on the dataset to view it.
## to see the help page for any function, type ?sedReview:: then the function of interest into the console.
## to run section of code, highlight and hit Run, or Ctrl+Enter.
## to run an individual line, make sure the cursor is anywhere in that line and hit Ctrl+Enter or Run.
########################################

# Edit the script directly with your Center’s NWIS server name, database numbers (env.db and qa.db), and the sites you wish to review. These commands are retrieving data for the identified sites within the date range you specify and putting them into a dataframe called siteData.
# import data for your site(s) using get_localNwis.
siteData <- get_localNWIS(DSN = 'nwisco',            # Colorado NWIS server 
                          env.db = '01',
                          qa.db = '02',
                          STAIDS = c(
                            '07104905',              # Monument Creek blw Bijou St., Colo.Springs
                            '09163500',              # CO River at UT state line
                            '06741510',              # BigT at Loveland
                            '07106500'),             # Fountain Creek at Pueblo
                          begin.date = '2015-10-01', # WY 2016-2017
                          end.date = '2017-09-30',
                          approval = 'Non-rejected')

# view site data in wide format
siteData2 <- make_wideTable(siteData)

# to run checks for only one site, rerun get_localNWIS with only one STAIDS, or subset your data in R with.
# subset siteData to only Fountain Creek
# To view a dataframe, you can click on the table icon next to the dataframe in the environment window in the upper right corner of Rstudio, or you can type View(dataframe name) in the console at lower left. 
# You can name the datasets below (e.g. “fountain”, “monument”) whatever makes sense to you for your sites. The commands below are needed only if you want to subset the original siteData dataframe by site for later review. 
fountain <- siteData[siteData$SITE_NO == '07106500', ]
monument <- siteData[siteData$SITE_NO == '07104905', ]

# run check_all function to see which sites/samples have flags
# This function runs through the regular and QA database (identify your QA database number below (qa.db) to look for samples that are flagged for one or more issues according to what is currently coded in the SedReview script. Current flag tests are:
# bagIEFlags: Tests whether required intake efficiency test parameters are reported when bag samplers are used. Flags if missing.
# Qflags: Tests whether some measure of discharge is provided as metadata for a sample. Flags if missing.
# metaDataFlags: Tests whether samples are missing required sample metadata, such as sample purpose, sampler type, sampling method, and type of associated QA with sample (if applicable). Includes tests in NWIS 20.xx level checks. For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check20-sql.html
# samplePurpFlags: Tests whether a sample purpose code (71999) differs from other samples collected at site (there are certainly valid reasons for this, but just provides a reminder to check that these are correct.
# samplerTypeFlags: Tests whether a sampler type code (84164) differs from others reported. Flags if sampler type code is used 3 or fewer times (again, there are valid reasons for this, so just a check).
# sedMassFlags: Tests whether a sample has a sediment mass less than 2 mg. Flags if so. (Note: this is a relatively new piece of metadata (pcode 91157) reported by USGS sediment laboratories so will not be available for historical data).
# tssFlags: Tests whether there is a TSS result without an accompanying SSC result, per OSW memo 01.03. Flags if TSS stands alone. 
# verticalsFlags: Tests whether sufficient verticals were sampled and reported (criteria are between 4 and 9 verticals for an EDI sample and between 10 and 20 verticals for an EWI samples)
# qaqcFlags: Tests whether samples coded as SSC, bedload, or bedload mass are stored in the QA database. Flags if a sediment sample is in the QA database.  (Note: most qa/qc samples for sediment are NOT stored in the qa database because they are considered environmental samples of their own. Storage of sediment replicates is coded by default in SedLogin in the main environmental database. This is just a check to see any sediment samples have been entered (manually or otherwise) into the QA database.)
# outliers: Tests whether sample results are outliers. Flags if an outlier. Currently looks at SSC, SSL, bedload loss on ignition, suspended sediment loss on ignition, and sand/silt break. Default threshold settings are at the 0.1 and 0.9 percentile level. 
# # If a sample meets the criteria of a flag, you will see the message “flags present” under the column for that test in the checkAll table.
# # checkAll also runs a few commands that do not generate a “flag”, but that prep datasets for future calculations and displays, including:
# sampleStatus: For reviewing which samples have DQI codes of R (reviewed and approved), S (provisional), and U (research/unapproved method or lab).
# boxcoeff: For evaluating whether cross section (EWI/EDI) sample are being collected to evaluate the representativeness of pumped sampler intakes.
# provisional: For determining which sediment samples need to have their DQI codes changed from ‘S’ to ‘R’ after review. 
# concSandFine: For determining sand and fines concentrations based on SSC and sand/silt break data reported in samples for all data in the siteData dataframe.
# summaryStats: For determining summary statistics for SSC, SSL, bedload, sand/silt break, and TSS data
checkAll <- check_all(siteData, qa.db = '02', returnAllTables = FALSE)

# run check_all function with returnAll option to return an R list with the check_all summary and all the check datasets
# This function displays all the tests run as part of checkAll as well as the characteristics of the dataframes that were created in each test. 
checkAll_list <- check_all(siteData, qa.db = '02', returnAllTables = TRUE)

# extract the various checks/counts/calc/finds from the list with $ notation. For example:
concSandFines1 <- checkAll_list$concSandFine
outliers_09163500_1 <- checkAll_list$outliers$`09163500`

# alternately, you can run each individual function
### now go through each project level review function to view individual calculations, checks, counts, finds, and plots
# Over the next several steps in the script, you are drilling down deeper into the results of the checkAll function. When the functions below are run, they create dataframes with the results. The dataframes can be viewed as tables by clicking on the table icon next to the dataframe in the environment window in the upper right corner of Rstudio, or by typing View(dataframe name) in the console at lower left. 

# calculate sand and fines concentrations and output quick plots. Use arrows in Plots tab to scroll through output plots
# This function will calculate sand and fines concentrations based on SSC and sand/silt break data reported in samples for all data in the siteData dataframe (pulled at beginning of workflow).
concSandFines2 <- calc_concSandFine(siteData)

# calculate summary statistics table using default sediment parameters
# This function calculates summary statistics for all data in the siteData dataframe. Summary is displayed by site, parameter (SSC, SSL, bedload, sand/silt break, TSS), and water year. Currently, included statistics are minimum, maximum, median, mean, standard deviation, and # non-detects.
sumStats <- calc_summaryStats(siteData)

# check bag sampler intake efficiency. There are no bag samples in my siteData so a dataframe of 0 observations was returned
# If the sampler type indicates a bag sampler was used, this will report a list of samples with intake efficiency test results and metadata. If the results and metadata are missing, the sample will be marked with a flag as “missing bag IE test results”. 
bagIE <- check_bagIE(siteData)

# check and flag samples that don't have associated discharge
# This function creates a dataframe of all the samples that were missing discharge data.
missingQ <- check_missingQ(siteData)

# check metadata and flag samples that are missing NWIS 20.xx level checks
# For more information on NWIS 20.xx level checks, see http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check10-sql.html
metaData <- check_metaData(siteData)

# check for sediment samples that are in the QAQC database (for NWISCO that is DB 02)
# As before, edit the number in single quotes based on your QA database number. Again, most qa/qc samples for sediment are NOT stored in the qa database because they are considered environmental samples of their own. Storage of sediment replicates is coded by default in SedLogin in the main environmental database. This is just a check to see any sediment samples have been entered (manually or otherwise) into the QA database.
qaqc <- check_qaqcDB(siteData, qa.db = '02')

# check and flag samples not collected with most common purpose
# The most commonly used sample purpose is provided in this dataframe for comparison.
purp <- check_samplePurp(siteData)

# check and flag samples not collected with most common sampler type
# The most commonly used sampler type is provided in this dataframe for comparison.
sampler <- check_samplerType(siteData)

# check and flag sediment mass samples < 2mg
# Again, suspended sediment mass reporting is relatively new, so this function will not return any results for historical data. 
sedMass <- check_sedMass(siteData)

# check and flag TSS samples without paired SSC sample
unpairedTSS <- check_tss(siteData)

# check and flag EWI/EDI samples low/high/missing verticals
verticals <- check_verticals(siteData)

# count of SSC sampling method, sampler type and bedload method
# This provides a summary of sample counts for SSC, bedload, and bedload mass as well as methods and sampler types used, organized by site and water year for all data pulled in the siteData dataframe. 
methods <- count_methodsBySite(siteData)

# count DQI code status
# This provides a summary of counts of sediment-associated data with DQI codes of R (reviewed and approved), S (provisional), and U (research/unapproved method or lab), organized by site. Useful for seeing how many samples need to be reviewed/approved and set to a DQI code or R. 
status <- count_sampleStatus(siteData)

# find box coefficient pairs at individual sites
# This function reports the occurrences and values of suspended sediment sample result pairs when there is a pumped sample and an EWI or EDI sample within plus or minus 1 hour of the pumped sample time. You can compare the pumped/cross section results at a glance. This is to determine how often the pumped sample is being check against the cross section (and how often box coefficients are being determined to evaluate the representativeness of the pumped sampler intake location). You can also specify the time window with timediff.
# Add and adapt these functions for your specific site numbers.
boxcoef_09163500 <- find_boxcoef(siteData, site_no = '09163500', timediff = 3)
boxcoef_07106500 <- find_boxcoef(siteData, site_no = '07106500') # or you can run the function on just the fountain data set
boxcoef_fountain <- find_boxcoef(fountain)

# find outliers at individual sites
# Default threshold settings are: below the 0.1 percentile level (flag low) and above the 0.9 percentile level (flag high), calculated for all data pulled for that site and date range. Note you may see odd results if you have few samples for the site and date range you are viewing. 
outliers_09163500_2 <- find_outliers(siteData, site_no = '09163500')
outliers_07106500 <- find_outliers(siteData, site_no = '07106500') # or you can run the function on just the fountain data set
outliers_fountain <- find_outliers(fountain)

# find provisional record numbers and output tab delimited files to D: drive. 
# Output file can be opened in excel, or used as input to QWDATA (this part needs testing)
# This function is intended to aid you in determining which sediment samples need to have their DQI codes changed from ‘S’ to ‘R’ after review. Modify the database numbers in the function as needed.
provisional <- find_provisional(siteData,
                                env.db = '01',
                                qa.db = '02',
                                env.fileout = 'D:/ex_sediment_records_env.txt')

# The following functions create several plots to facilitate review of your sediment data. Currently, plots available are:
# Side by side boxplots of TSS (00530) and SSC (80154), where available 
# Scatter plot of TSS (00530) vs SSC (80154), where available
# Time series plot of SSC (80154)
# Time series plot of sand/silt break (70331)
# Time series plot of SSL (80155)
# Time series plot of bedload (80225)
# Time series plot of bedload mass (91145)
# Scatter plot of SSC (80154) by flow (00060, 00061, 30208, or 30209) - if more than one flow parameter is available, 00060 or 00061 selected by default.
# Scatter plot of sand/silt break (70331) by flow
# Scatter plot of bedload (80225) by flow
# If turbidity is available, scatter plot of SSC (80154) by turbidity (00076, 61028, 63675, 63676, 63677, 63679, 63680, 63681, 63682, 63683, 63684, 72188, 72208, 72209, 72213, 82079, OR 99872, whatever is available). Assumes 63680 or 72209 will most likely be the available turbidity codes.

# Adapt and add functions as needed based on the sites you are reviewing. 
# plot sediment versus streamflow
sedFlow_09163500 <- plot_sedFlow(siteData, siteSelect = '09163500')
sedFlow_fountain <- plot_sedFlow(fountain)
###### to view a plot select from the plot list using $ operator
sedFlow_09163500$SSC
sedFlow_09163500$ssbreak
sedFlow_fountain$SSC
sedFlow_fountain$ssbreak
# To plot bedload by flow, simply type bedload after the $ operator and run the command. 
################## or output to a PDF file with all available plots
plot_sedFlow(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_sedFlow.pdf')
plot_sedFlow(fountain, PDFout = 'D:/ex_fountain_sedFlow.pdf')
# This creates a nice pdf packet of sediment~flow plots for your site.

# plot timeseries of sediment data
sedTS_09163500 <- plot_sedTS(siteData, siteSelect = '09163500', log.P80154 = TRUE)
sedTS_09163500$SSC
sedTS_09163500$ssbreak
plot_sedTS(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_TS.pdf')
# This creates a nice pdf packet of sediment~time plots for your site.

sedTS_fountain <- plot_sedTS(fountain)
sedTS_fountain$SSC
sedTS_fountain$ssbreak
plot_sedTS(fountain, PDFout = 'D:/ex_fountain_TS.pdf')

# plot SSC and TSS boxplots and scatterplot
ssctss_09163500 <- plot_SSCTSS(siteData, siteSelect = '09163500')
ssctss_09163500$SSC
plot_SSCTSS(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_ssctss.pdf')
# This creates a nice pdf packet of SSC~TSS comparison plots for your site.

ssctss_monument <- plot_SSCTSS(monument)
ssctss_monument$SSC
ssctss_monument$TSS
ssctss_monument$combined
ssctss_monument$scatter
plot_SSCTSS(monument, PDFout = 'D:/ex_monument_ssctss.pdf')

# plot turb versus SSC
turbSSC_09163500 <- plot_turbSSC(siteData, siteSelect = '09163500', log.turb = T)
turbSSC_09163500
# Note the number after turbidity is the turbidity parameter code; it should be edited depending on what you are using to measure turbidity. See parameter codes listed at: https://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search&pm_search=turbidity&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units

turbSSC_monument <- plot_turbSSC(monument)
turbSSC_monument
turbSSC_monument
plot_turbSSC(monument, PDFout = 'D:/ex_monument_turbSSC.pdf')
# This creates a nice pdf packet of SSC~TSS comparison plots for your site.


# This portion of the code is intended for Section Chiefs, Supervisors, Center Managers, and/or Technical Reviewers who want to perform a higher-level review than what is typical for a Project Chief. The main goal is to get a status report on the number of types of samples that are collected in the WSC as well as to look for major problems or noncompliance with policy. 
########################################
## Center Level Review
## for review of mulitiple sediment sites/projects in a WSC. Some overlapping functions with Project Chief Review.
## for any created dataframe (table of data or results), click in the upper left Environment tab on the dataset to view it.
## to see the help page for any function, type ?sedReview:: then the funtion of interest into the console.
## to run section of code, highlight and hit Run, or Ctrl+Enter.
## to run an individual line, make sure the cursor is anywhere in that line and hit Ctrl+Enter or Run.
########################################

# get a count of all active sediment sites/samples in a database in a given time frame
# Results are organized by site and water year. Edit database server and dates as needed.
activeSed <- count_activeSed(DSN = 'nwisco',
                             env.db = '01',
                             begin.date = '2015-10-01',
                             end.date = '2017-09-30')

# extract a list of all the site numbers containing WY2016-2017 sediment data from activeSed
# use this to pull NWIS data for these sites. this could take several minutes depending on how many sites, timespan, and network connection
sedSites <- unique(activeSed$SITE_NO)
centerData <- get_localNWIS(DSN = 'nwisco',
                            env.db = '01',
                            qa.db = '02',
                            STAIDS = sedSites,
                            begin.date = '2015-10-01',
                            end.date = '2017-09-30')
# You can quickly scan and review the data as needed.


# count DQI code status
status2 <- count_sampleStatus(centerData)

# check and flag sediment mass samples < 2mg
sedMass2 <- check_sedMass(centerData)
sedMassSummary <- check_sedMass(centerData, reviewSummary = TRUE)
# get summary of boxcoefficient pairs at all the sites
# This helps determine how often the WSC performs a comparison between pumped samples and cross section (EWI/EDI) samples, to evaluate the representativeness of the pumped sampler intake location. Reports the number of paired samples collected, grouped by site and water year. 
boxcoefSum1 <- summary_boxcoef(centerData)
boxcoefSum3 <- summary_boxcoef(centerData, timediff = 3)

#### you can also output the boxcoefficient summary and the data for all sites as an R list. Access list elements using $ operator
boxcoefSum_all <- summary_boxcoef(centerData, returnAllTables = TRUE)
cherrycrk_boxcoef <- boxcoefSum_all$`06713500`

# check EWI/EDI sample verticals
# Reports a summary of sediment samples that were flagged with # verticals outside the normal ranges for EWI and EDI or where # verticals was missing in the metadata.
verticals2 <- check_verticals(centerData)
verticalsSummary <- check_verticals(centerData, reviewSummary = TRUE)

# check bag intake efficiency
# Reports a summary of samples that were missing intake efficiency test results if a bag suspended sediment sampler was used. 
bagIE2 <- check_bagIE(centerData)
bagIESummary <- check_bagIE(centerData, reviewSummary = TRUE)
bagIESummary2 <- check_bagIE(exampleData2, reviewSummary = TRUE)

# check discharge accompanies sediment sample
# Reports a summary of sediment samples that were missing discharge data. 
missingQ2 <- check_Q(centerData)
missingQ3 <- check_Q(centerData, reviewSummary = TRUE)
# check for TSS samples without SSC per OSW memo 01.03
# Reports a summary of sediment samples that had TSS data but no accompanying SSC data. 
unpairedTSS2 <- check_tss(centerData)
unpairedTSSsummary <- check_tss(centerData, reviewSummary = TRUE)

# count methods and total number of samples
# Reports a summary of the total number of sediment samples collected by type and method used, organized by site and water year.  
methods2 <- count_methodsBySite(centerData)

# check for sediment samples that have been coded into the QAQC database
# As discussed above, this checks for any sediment samples in the QA database. This indicates samples were entered by some means other than SedLogin. Edit the QA database number as needed.
qaqc2 <- check_qaqcDB(centerData, qa.db = '02')
qaqcSummary <- check_qaqcDB(centerData, qa.db = '02', reviewSummary = TRUE)

#########################################
#########################################
#########################################
# any dataframe or dataframe from a list can be output as a csv or tab delimited file using write.table

# write unpaired TSS samples to a csv
write.table(unpairedTSS2, file = 'D:/ex_centerRev_unpairedTSS.csv', quote = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)

# write verticals check to tab-delimited file
write.table(verticals2, file = 'D:/ex_centerRev_verticals.txt', quote = FALSE, sep = "\t",
            row.names = FALSE, col.names = TRUE)


