impute_weather <- function(w,
                           Tm,
                           rain,
                           RH){
   if("epiphy.weather" %in% class(w)){print("to do")}

}


#' Impute daily temperatures
#'
#' @param start_h numeric, first hour in the day to return the imputed temperature
#' @param end_h numeric, last hour in the day to return the imputed temperature.
#'  If no `end_h` is supplied `impute_temperature()` will return one value for the
#'  `start_h`
#' @param max_Tm numeric, maximum daily temperature
#' @param min_Tm numeric, minimum daily temperature
#'
#' @return numeric, vector of imputed times for each hour between (and including)
#'  `start_h` and `end_h`
#' @export
#'
#' @examples
impute_temperature <- function(start_h = 0,
                               end_h = NULL,
                               max_Tm = 15,
                               min_Tm = 25){

   if(is.null(end_h)){
      end_h <- start_h
      }

   # (2*pi)/24
   hour_rad <- 0.2417994

   # find start in radians for cos function
   start_rad <- (start_h * hour_rad) + (0.2417994 * 11)
   # find start in radians for cos function
   end_rad <- (end_h * hour_rad) + (0.2417994 * 11)

   tm_range <-
      ((cos(seq(from = start_rad,
                to = end_rad,
                by = hour_rad))/2)+
         0.5) * (max_Tm - min_Tm)
   #adjust
   tm_range <- tm_range + min_Tm

   return(tm_range)
}
