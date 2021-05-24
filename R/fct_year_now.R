#' year_now 
#'
#' @description A fct function
#'
#' @return Returns the current year as numeric value.
#'
#' @noRd

year_now <- function(){
  x <- stringr::str_sub(string = Sys.Date(), start = 1, end = 4)
  x <- as.numeric(x)
  return(x)
}