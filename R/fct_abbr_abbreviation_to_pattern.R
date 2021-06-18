#' abbr_abbreviation_to_pattern 
#'
#' @description Convert potential abbreviation to pattern to check whether it is 
#' abbreviation or false match. For additional explanations see Abbrevimate.Rmd
#'
#' @return Return pattern of abbreviation as a character string
#'
#' @noRd
abbr_abbreviation_to_pattern <- function(x){
  pattern <- x %>%
    stringr::str_to_lower(locale = "en") %>%
    stringr::str_split(., pattern = "", simplify = TRUE) %>%
    stringr::str_c(., collapse = ".*") %>%
    stringr::str_c("\\b", ., sep = "") %>%
    # Two following modifications needed to avoid evaluating parenthesis
    # in the context of regex where they have special meaning 
    stringr::str_replace_all(.,
                             pattern = "\\(",
                             replacement = "\\\\(") %>%
    stringr::str_replace_all(.,
                             pattern = "\\)",
                             replacement = "\\\\)") %>%
    
    return(pattern)
}