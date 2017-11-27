data('exampleData', package = "sedReview")
x <- exampleData[1:5,]
x$PARM_CD <- '91157'
x$RESULT_VA <- c(0.001,200,1000,0.002,0.0005)

#records for sediment mass pcode 91157
sedMass <- x[x$PARM_CD == '91157',]
sedMass <- unique(sedMass[c('UID','RESULT_VA')])

# flag samples with sediment mass less than 2mg (0.002 g)
sedMass$flag[sedMass$RESULT_VA < 0.002] <- paste0("flag, low sediment mass ", sedMass$RESULT_VA[sedMass$RESULT_VA < 0.002], "g")

