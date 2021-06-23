#' abbr_term_to_pattern 
#'
#' @description Convert search term into pattern that match the common pattern
#'  of abbreviations in scientific literature. For additional explanations
#'  see Abbrevimate.Rmd
#'
#' @return Returns the pattern as a string 
#'
#' @noRd
#' 
#' @examples
#' abbr_term_to_pattern(term = "Calyculin A", derivatives = FALSE)
#' abbr_term_to_pattern(term = "Y-27632", derivatives = TRUE)

abbr_term_to_pattern <- function(term,
                                 derivatives = FALSE){
  
  usable_term <- term %>%
    tolower() %>%
    stringr::str_replace_all(string = .,
                             pattern = " ",
                             replacement = ".?")
  
  if(derivatives == TRUE){
    
    search_pattern <- sprintf("(\\S*%s\\S*[ ][(].*?[)])", usable_term)
    return(search_pattern)
    
  } else if (derivatives == FALSE) {
    
    search_pattern <- sprintf("(\\b%s\\b[ ][(].*?[)])", usable_term)
    return(search_pattern)
    
  }
}