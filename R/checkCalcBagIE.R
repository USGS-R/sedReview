#' checkCalcBagIE
#' @description Function to check and flag bag intake efficiency parameters, and calculate and flag the bag efficiency
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details checks and calculated bag intake efficiency under OSW Tech Memo 2013.03 (https://water.usgs.gov/admin/memo/SW/sw13.03.pdf)
#' @details P72217 - Duration sampler collected water, seconds
#' @details P72218 - Sample volume to compute isokinetic transit rate, milliliters
#' @details P72196 - Velocity to compute isokinetic transit rate, feet per second
#' @details P72219 - Sampler nozzle material, code: Plastic=2, TFE=3
#' @details P72220 - Sampler nozzle diameter, code: 3/16"=3, 1/4"=4, 5/16"=5
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' bagIEflags <- checkCalcBagIE(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

checkCalcBagIE <- function(x, returnAll = FALSE){
  ### Find records where sampler type code (P84164) has bag sampler value (3055,3056,3057,3058)
  bagSamp <- x[x$PARM_CD == "84164" & x$RESULT_VA %in% c(3055,3056,3057,3058), ]
  bagSamp <- unique(bagSamp[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA")])
  
  ### Pull intake efficiency test results Pcodes
  # 72217 - Duration sampler collected water, seconds
  duration <- x[x$PARM_CD == "72217" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  duration <- unique(duration[c("UID", "RESULT_VA")])
  # 72218 - Sample volume to compute isokinetic transit rate, milliliters
  volume <- x[x$PARM_CD == "72218" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  volume <- unique(volume[c("UID", "RESULT_VA")])
  # 72196 - Velocity to compute isokinetic transit rate, feet per second
  velocity <- x[x$PARM_CD == "72196" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  velocity <- unique(velocity[c("UID", "RESULT_VA")])
  # 72219 - Sampler nozzle material, code: Plastic=2, TFE=3
  nozzle <- x[x$PARM_CD == "72219" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  nozzle <- unique(nozzle[c("UID", "RESULT_VA")])
  # 72220 - Sampler nozzle diameter, code: 3/16"=3, 1/4"=4, 5/16"=5
  diameter <- x[x$PARM_CD == "72220" & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  diameter <- unique(diameter[c("UID", "RESULT_VA")])
  # 00010 or 00011 - water temp
  waterTemp <- x[x$PARM_CD %in% c("00010","00011") & x$RECORD_NO %in% bagSamp$RECORD_NO,]
  waterTemp <- unique(waterTemp[c("UID", "RESULT_VA")])
  
  ### append IE test results to bag sampler records and flag those with missing results
  bagSamp <- dplyr::left_join(bagSamp, duration, by = "UID")
  bagSamp <- dplyr::left_join(bagSamp, volume, by = "UID")
  bagSamp <- dplyr::left_join(bagSamp, velocity, by = "UID")
  bagSamp <- dplyr::left_join(bagSamp, nozzle, by = "UID")
  bagSamp <- dplyr::left_join(bagSamp, diameter, by = "UID")
  bagSamp <- dplyr::left_join(bagSamp, waterTemp, by = "UID")
  names(bagSamp) <- c("UID", "PARM_CD", "PARM_NM", "RESULT_VA",
                      "P72217_Dur", "P72218_Vol", "P72196_Vel", "P72219_Nozz", "P72220_Dia", "P00010_00011_waterTemp")
  bagSamp$IEflag[is.na(bagSamp$P72217_Dur)==TRUE |
                   is.na(bagSamp$P72218_Vol)==TRUE |
                   is.na(bagSamp$P72196_Vel)==TRUE |
                   is.na(bagSamp$P72219_Nozz)==TRUE |
                   is.na(bagSamp$P72220_Dia)==TRUE |
                   is.na(bagSamp$P00010_00011_waterTemp)==TRUE] <- paste("flag missing bag IE test results")
  
  ### Calculate bag intake efficiency according to OSW Tech Memo 2013.03
  ## if duration, volume, velocity and diameter are available calculate intake efficiency
  # assign K value based on indexed nozzle diameter
  bagSamp$K[bagSamp$P72220_Dia == 3] <- 0.1841
  bagSamp$K[bagSamp$P72220_Dia == 4] <- 0.1036
  bagSamp$K[bagSamp$P72220_Dia == 5] <- 0.0663
  # calculate intake efficieny
  bagSamp$calcIE <- bagSamp$K * (bagSamp$P72218_Vol/bagSamp$P72217_Dur) / bagSamp$P72196
  # flag intake efficiency where 0.75<IE<1.25
  bagSamp$calcIEflag[bagSamp$calcIE < 0.75] <- paste("flag low calc IE")
  bagSamp$calcIEflag[bagSamp$calcIE > 1.25] <- paste("flag high calc IE")
  bagSamp$calcIEflag[is.na(bagSamp$calcIE)==TRUE] <- paste("flag missing bag IE test results")

  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, bagSamp, by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$IEflag)==FALSE | is.na(flaggedSamples$calcIEflag)==FALSE, ]
  }
  
  
  return(flaggedSamples)
}

