

checkBagIE <- function(x, returnAll = FALSE){
  ### Find records where sampler type code (P84164) has bag sampler value (3055,3056,3057,3058)
  bagSamp <- x[x$PARM_CD == "84164" & x$RESULT_VA %in% c(3055,3056,3057,3058), ]
  
  ### Pull intake efficiency test results Pcodes
  # 72217 - Duration sampler collected water, seconds
  duration <- x[x$PARM_CD == "72217" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
  # 72218 - Sample volume to compute isokinetic transit rate, milliliters
  volume <- x[x$PARM_CD == "72218" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
  # 72196 - Velocity to compute isokinetic transit rate, feet per second
  velocity <- x[x$PARM_CD == "72196" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
  # 72219 - Sampler nozzle material, code
  nozzle <- x[x$PARM_CD == "72219" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
  # 72220 - Sampler nozzle diameter, code
  diameter <- x[x$PARM_CD == "72220" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
  # 00010 or 00011 - water temp
  waterTemp <- x[x$PARM_CD %in% c("00010","00011") & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  
}