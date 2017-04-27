

checkBagIE <- function(x, returnAll = FALSE){
  ### Find records where sampler type code (P84164) has bag sampler value (3055,3056,3057,3058)
  bagSamp <- x[x$PARM_CD == "84164" & x$RESULT_VA %in% c(3055,3056,3057,3058,3054), ]
  bagSamp <- unique(bagSamp[c("RECORD_NO", "PARM_CD", "PARM_NM", "RESULT_VA")])
  
  ### Pull intake efficiency test results Pcodes
  # 72217 - Duration sampler collected water, seconds
  duration <- x[x$PARM_CD == "72217" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  duration <- unique(duration[c("RECORD_NO", "RESULT_VA")])
  # 72218 - Sample volume to compute isokinetic transit rate, milliliters
  volume <- x[x$PARM_CD == "72218" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  volume <- unique(volume[c("RECORD_NO", "RESULT_VA")])
  # 72196 - Velocity to compute isokinetic transit rate, feet per second
  velocity <- x[x$PARM_CD == "72196" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  velocity <- unique(velocity[c("RECORD_NO", "RESULT_VA")])
  # 72219 - Sampler nozzle material, code
  nozzle <- x[x$PARM_CD == "72219" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  nozzle <- unique(nozzle[c("RECORD_NO", "RESULT_VA")])
  # 72220 - Sampler nozzle diameter, code
  diameter <- x[x$PARM_CD == "72220" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  diameter <- unique(diameter[c("RECORD_NO", "RESULT_VA")])
  # 00010 or 00011 - water temp
  waterTemp <- x[x$PARM_CD %in% c("00010","00011") & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  waterTemp <- unique(waterTemp[c("RECORD_NO", "RESULT_VA")])
  
  ### append IE test results to bag sampler records
  bagSamp <- dplyr::left_join(bagSamp, duration, by = "RECORD_NO")
  bagSamp <- dplyr::left_join(bagSamp, volume, by = "RECORD_NO")
  
  
}

