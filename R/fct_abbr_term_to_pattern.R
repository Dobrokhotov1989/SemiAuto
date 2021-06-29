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
  
  # If special characters appears in the search query
  # they may lead to error due to the incorrect regex.
  # Hence, all special characters are replaced with \\[\\^\\\\w\\\\s\\]
  # which means "any character that is not alphanumeric and not white-space"
  special_characters <- c(".", "+", "*", "?", "^", "$", "(", ")",
                          "[", "]", "{", "}", "|", "\\")
  
  special_characters_pattern <- 
    stringr::str_c( "\\", special_characters, collapse = "|") %>%
    stringr::str_c("(", ., ")")
  
  usable_term <- term %>%
    tolower() %>%
    stringr::str_replace_all(string = .,
                             pattern = " ",
                             replacement = ".?") %>%
    stringr::str_replace_all(string = .,
                             pattern = special_characters_pattern,
                             replacement = "\\[\\^\\\\w\\\\s\\]")

  if(derivatives == TRUE){
    
    search_pattern <- sprintf("(\\S*%s\\S*[ ][(].*?[)])", usable_term)
    return(search_pattern)
    
  } else if (derivatives == FALSE) {
    
    search_pattern <- sprintf("(\\b%s\\b[ ][(].*?[)])", usable_term)
    return(search_pattern)
    
  }
}
