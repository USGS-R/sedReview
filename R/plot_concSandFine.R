

# get suspended sediment concentration
SSC <- x[x$PARM_CD == "80154", ]

# get sand/fines split (% finer than 62.5 microns)
split <- x[x$PARM_CD == "70331", ]

# get flow data
