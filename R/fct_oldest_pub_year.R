#' oldest_pub_year 
#'
#' @description A fct function
#'
#' @return Returns the publication year for the oldest pub in pubs_trend_all_years dataset.
#'
#' @noRd

oldest_pub_year <- function(){
  x <- min(pubs_trend_all_years$year)
  return(x)
}