# function to test EDI sample collected correctly. Number of sampling verticles 4-9.

sampleEDI <- function(x){
  EDI <- x[x$PARM_CD == "82398" | x$PARM_CD == "00063", ]
  
  
  
  
  
  
  
  verts <- x[x$PARM_CD == "00063", ]
}