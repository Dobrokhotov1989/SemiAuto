#' abbr_dictionaries_to_patterns
#'
#' @description A fct function that reads csv files with dictionaries and 
#' convert them into the patterns for co-appearance search
#'
#' @return Function returns vector of patterns that could be used in the 
#' search of patterns co-appearance
#'
#' @noRd
abbr_dictionaries_to_patterns <- function(files, header){
  
  dictionaries <- purrr::map(files,
                             ~ readr::read_csv(.x, col_names = header))

  patterns <- purrr::map_chr(.x = dictionaries,
                             .f = function(x){
                               x %>%
                                 dplyr::pull(1) %>%
                                 stringr::str_replace_all(pattern = " ",
                                                          replacement = "\\s?")%>%
                                 stringr::str_c(collapse = "\\b|\\b") %>%
                                 stringr::str_c("\\b", ., "\\b")
                               
                             } 
  )
  
  return(patterns)
}