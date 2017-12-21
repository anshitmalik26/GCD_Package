#' Great Circle Distance
#'
#'Takes in 2 pairs of latitute and longitude and gives the distance between them in kms.
#'@param lat1 Latitute of 1st pair
#'@param long1 Longitude of 1st pair
#'@param lat2 Latitude of 2nd pair
#'@param long2 Longitude of 2nd pair
#'@export
gcd <- function(lat1,long1,lat2,long2){

  lat1 <- lat1/180
  lat2 <- lat2/180
  long1 <- long1/180
  long2 <- long2/180

  return(6378*acos(sinpi(lat1)*sinpi(lat2) + cospi(lat1)*cospi(lat2)*cospi(long2-long1)))


}

