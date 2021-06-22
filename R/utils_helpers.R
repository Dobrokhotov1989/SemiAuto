#' Current year  
#'
#' @return Returns a current year as numeric value.
#'
#' @noRd
#' 
#' @examples
#' year_now()

year_now <- function(){
  x <- stringr::str_sub(string = Sys.Date(), start = 1, end = 4)
  x <- as.numeric(x)
  return(x)
}

#' Year of oldest publication at EuropePMC
#'
#' @return Returns a year of the oldest publication deposited at EuropePMC
#' as numeric value
#'
#' @noRd
#' 
#' @examples
#' oldest_pub_year()

oldest_pub_year <- function(){
  x <- min(pubs_trend_all_years$year)
  return(x)
}

#' Remove html tags from text
#'
#' @return Returns character string with all tags removed
#'
#' @noRd
#' 
#' @examples
#' remove_tags("some random text with <italic>html tags</italic>")


#Function to remove html tags from the text of paper
remove_tags <- function(htmlString) {
  return(stringr::str_replace_all(string = htmlString,
                                  pattern = "<.*?>",
                                  replacement = ""))
}
