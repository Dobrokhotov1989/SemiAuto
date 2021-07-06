#' abbr_extract_pattern_from_paper 
#'
#' @description Function searches though the paper to extract a pattern of 
#' abbreviation. For additional explanations see Abbrevimate.Rmd
#'
#' @return Returns a vector with all strings that match pattern
#'
#' @noRd
abbr_extract_pattern_from_paper <- function(x, pattern, derivatives = FALSE){
  
  # No derivatives - search directly on xml
  if(rlang::is_false(derivatives)){
    
    results <- stringr::str_extract_all(
      string = x,
      pattern = stringr::regex(pattern,
                               ignore_case = TRUE)
    ) %>%
      purrr::flatten_chr()
    
    return(results)
    
  }
  
  # Derivatives - convert xml to list
  if (rlang::is_true(derivatives)) {
    
    results <- stringr::str_extract_all(
      string = stringr::str_c(
        unlist(xml2::as_list(x)),
        collapse = " "),
      pattern = stringr::regex(pattern,
                               ignore_case = TRUE)
    ) %>%
      purrr::flatten_chr()
    
    return(results)
  }
}