#' abbr_find_coappearance 
#'
#' @description A fct function that takes a list of PMC IDs and two or more
#' synonyms dictionaries
#' as an input and searches for #' the co-appearance of the words from each
#' dictionary in the same sentence
#'
#' @return Return value is a tibble with sentences where words co-appear and
#' additional columns "pmcid", "title", "authorString", "firstPublicationDate",
#' "doi"
#'
#' @noRd

abbr_find_coappearance <- function(records, dictionaries = list()){
  
  
  
}