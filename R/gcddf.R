#' Great Circle Distance
#'
#'Takes in a dataframe with latitute and longitude columns and gives another column with distance between 2 consecutive pairs of location
#'@param data a dataframe with latitude and longitude information
#'@param lat Index of latitude column
#'@param long Index of longitude column
#'@export

GCD <- function(data,lat,long){

  number_col <- ncol(data)


  if (nrow(data)==1){

    data[1,number_col+1] <- 0

  }

  else{

    data[1,number_col+1] <- 0

    for (b in 2:nrow(data)){

      r <- 6378   # Earth's radius in km

      lat1 <- data[b-1,lat]/180
      lat2 <- data[b,lat]/180
      long1 <- data[b-1,long]/180
      long2 <- data[b,long]/180

      gcd <- r*acos(sinpi(lat1)*sinpi(lat2) + cospi(lat1)*cospi(lat2)*cospi(long2-long1))

      if(is.nan(gcd)==TRUE){

        data[b,number_col+1] <- 0

      }

      else {
        data[b,number_col+1] <- gcd

      }

    }

  }

  names(data)[number_col+1] <- "GCD"
  return(data)

}
