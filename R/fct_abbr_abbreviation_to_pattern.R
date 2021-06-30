#' abbr_abbreviation_to_pattern 
#'
#' @description Convert potential abbreviation to pattern to check whether it is 
#' abbreviation or false match. For additional explanations see Abbrevimate.Rmd
#'
#' @return Return pattern of abbreviation as a character string
#'
#' @noRd
#' 
#' @examples
#' abbr_abbreviation_to_pattern("text")
#' abbr_abbreviation_to_pattern("(with.special*characters)")
abbr_abbreviation_to_pattern <- function(x){
  pattern <- x %>%
    stringr::str_to_lower(locale = "en") %>%
    stringr::str_split(., pattern = "", simplify = TRUE) %>%
    stringr::str_c(., collapse = ".*") %>%
    stringr::str_c("\\b", ., sep = "") 
  
  special_characters <- c(".", "+", "*", "?", "^", "$", "(", ")",
                          "[", "]", "{", "}", "|", "\\")
  
  
  special_characters_in_context <- 
    stringr::str_c( "\\", special_characters, "\\.\\*", collapse = "|") %>%
    stringr::str_c("(", ., ")")
  
  
  
  special_characters_in_context_2 <- 
    stringr::str_c( "\\", special_characters, collapse = "|") %>%
    stringr::str_c("((", ., ")$)")
  
  special_characters_full <- stringr::str_c(special_characters_in_context,
                                            special_characters_in_context_2,
                                            sep = "|")
  
  
  complete_pattern <- purrr::map_chr(
    pattern,
    function(x){
      if(stringr::str_detect(x, pattern = special_characters_full)){
        # locate start positions of the special characters to be escaped 
        positions <- stringr::str_locate_all(x, pattern = special_characters_full)
        start_pos <- positions[[1]][,1]
        
        # Adjust start position to insert chracters in -1 position
        start_pos <- start_pos - 1
        
        # Convert to the number of characters in each group
        groups <- c(start_pos[[1]], (start_pos - dplyr::lag(start_pos))[-1])
        
        # Make a pattern to match number of characters
        groups_patterns <- purrr::map_chr(groups,
                                          ~stringr::str_c("(.{", .x, "})"))
        
        # Complete the full pattern
        full_groups <- stringr::str_c("^",
                                      stringr::str_c(groups_patterns, collapse = ""),
                                      "(.+)$")
        
        # Create a replacement pattern
        n_of_groups <- length(start_pos)+1
        replacement_pattern_immature <-
          stringr::str_c("\\", seq_along(start_pos), collapse = "\\\\")
        replacement_pattern <- stringr::str_c(replacement_pattern_immature, n_of_groups,
                                              sep = "\\\\\\")
        
        # Add escape characters to the text
        complete_pattern <- stringr::str_replace_all(string = x,
                                                     pattern = full_groups,
                                                     replacement = replacement_pattern)
        return(complete_pattern)} else {
          return(x)
        }
      
    }) %>%
    purrr::map_chr(~ stringr::str_replace_all(.x,
                                             pattern = "-",
                                             replacement = "-?"))
  
  return(complete_pattern)
}