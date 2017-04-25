# Master queries:
#   Ability to retrieve total number of samples and names of sites sampled for:
#   SSC (80154)
# Bedload (80225)
# Bedload mass (91145)
# Ability to retrieve counts (number of samples) on sample method code, particularly:
#   If SSC (80154) value is present:
#   Sample method code (82398) = 10, 20, or 60 (provide counts for each code)
# Sampler type (84164) = 3060, 3070, 3071 (provide counts for each code)
# If bedload (80225) value is present:
#   Sample method code (82398) = 1000, 1010, 1020 (provide counts for each code)

#' countSamplesByMethod
#' 
#' @description Function to provide counts of samples collected using various methods for 
#' sample method codes 82398 and 82398 and for sampler type code 84164 
#' @param x A list generated from \code{getLocalNWIS}
#' @return A data.frame tabular summary of counts of samples by site
#' @examples
#' x <- data("exampleData",package="sedReview")
#' countSamplesByMethod(x)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
countSamplesByMethod <- function(x) {
  x <- x[c("RECORD_NO","SITE_NO","STATION_NM","PARM_CD","METH_CD","RESULT_VA")]
  x <- unique(x)
  
  sumX <- dplyr::summarise(dplyr::group_by(x,SITE_NO,STATION_NM),
                           SSC_80154 = length(PARM_CD[PARM_CD == "80154"]),
                           bedload_80225 = length(PARM_CD[PARM_CD == "80225"]),
                           bedloadMass_91145 = length(PARM_CD[PARM_CD == "91145"])
  )
  
  return(sumX)
  
}