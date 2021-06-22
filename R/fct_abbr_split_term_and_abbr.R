#' abbr_split_term_and_abbr 
#'
#' @description Takes a vector of search results from
#' abbr_extract_pattern_from_paper() and splits full term and abbreviation. For
#' additional explanations see Abbrevimate.Rmd
#'
#' @return Returns tibble with column "full" and "abbr" there full term and 
#' abbreviation from each element of vector are returned.
#'
#' @noRd
abbr_split_term_and_abbr <- function(x){
  
  # Split part before parenthesis and in parenthesis
  abbr_tbl <- x %>%
    purrr::map(~ stringr::str_split_fixed(string = (.x),
                                 pattern = " \\(", n = 2)) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(.f = function(x){c("full", "abbr")})
  
  # Remove parenthesis
  abbr_tbl$abbr <- purrr::map_chr(abbr_tbl$abbr,
                                  .f = function(x){stringr::str_sub(x, 1, -2)})
  
  # If "extra text" present inside parenthesis then everything after comma, 
  # semicolon or period will be removed
  abbr_tbl$abbr <- purrr::map_chr(abbr_tbl$abbr, .f = function(x){
    if(stringr::str_detect(x, pattern = ",|[.]|;")){
      return(stringr::str_replace(x, pattern = "(,|[.]|;).*", replacement = ""))
    } else {
      return(x)
    }
    
  })
  
  return(abbr_tbl)
}