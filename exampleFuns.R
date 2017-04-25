###Joe commented this out because all code in this directory needs to run as-is, so basically just functions
###If this needs to be retained it needs to be moved to the top level directory

# #x is the plotTable dataframe generated from readNWISodbc
# x <- qw.data$PlotTable
# threshold <- c(0.1,0.9)
# exampleFun <- function(x,threshold) {
#   SSC <- x[x$PARM_CD == "80154",]
#   perctiles <- quantile(SSC$RESULT_VA,probs=threshold)
#   outliers <- SSC[SSC$RESULT_VA < perctiles[1] |
#                     SSC$RESULT_VA > perctiles[2],]
# }
# 
# AnotherExmaple <- function(x) {
#   ###First I need to count the number of samples per sampler type code
#   ###Then I need to flag unusuall sampler type codes based off some criteria
# }