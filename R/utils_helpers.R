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
#' @return Returns character string with all or selected tags removed
#'
#' @noRd
#' 
#' @examples
#' remove_tags("some random text with <italic>html tags</italic>")

remove_tags <- function(htmlString, tags = NULL) {
  require(magrittr)
  if (rlang::is_null(tags)){
    
    new_string <- stringr::str_replace_all(string = htmlString,
                                           pattern = "<.*?>",
                                           replacement = " ") %>%
      stringr::str_squish()
    return(new_string)
    
  } else {
    
    pattern <- stringr::str_c(tags, collapse = "|")
    
    new_string <- stringr::str_replace_all(string = htmlString,
                                           pattern = pattern,
                                           replacement = " ") %>%
      stringr::str_squish()
    return(new_string)
    
  }
}

#' Make datatable rows expandable. Taken from:
#' https://github.com/rasmusab/tidyverse-in-a-table
#'
#' @return Returns JavaScript code
#'
#' @noRd

datatable_callback <- function(){
  return(
    DT::JS("
  var format = function(d) {
    return '<div style=\"padding: .5em;\">' +
           '<p>' + d[3] + '</p>' +
           '<p>' + d[4] + '</p>' +
           '<p>' + d[5] + '</p>' + 
           '<p>' + d[6] + '</p>' +
           '<p>' + '</p>' +
           '<p>' + d[7] + '</p>' +
           '</div>';
  };
  table.on('click', 'td.expand-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
    )
  )
}

#' Add highlight tags to the words matching pattern 
#'
#' @return Returns character string with highlight tags added
#'
#' @noRd

add_highlight <- function(sentence, patterns, color = "#D1D0DE"){
  # Detect positions of words/phrases matching patterns
  positions_init <- tibble::as_tibble((stringr::str_locate_all(sentence,
                                                               pattern = stringr::regex(stringr::str_c(patterns, collapse = "|"),
                                                                                        ignore_case = TRUE))
  )[[1]]) %>%
    # Adjust the position of tag insertion
    dplyr::mutate(start = start - 1,
                  end = end)
  # Calculate length of character lengths between insertions
  start_groups <- c(positions_init$start[[1]],
                    (positions_init$start - dplyr::lag(positions_init$start))[-1])
  
  
  # Make a pattern to match number of characters
  start_groups_pattern <- purrr::map_chr(start_groups,
                                         ~stringr::str_c("(.{", .x, "})")) %>%
    stringr::str_c(collapse = "") %>%
    stringr::str_c("^", ., "(.+)$")
  
  
  # Create a replacement pattern
  start_tag <- sprintf("<mark style = 'background-color:%s'>", color)
  
  start_replacement <- stringr::str_c(
    "\\", seq_along(positions_init$start), collapse = start_tag
  ) %>%
    stringr::str_c(., nrow(positions_init)+1, sep = stringr::str_c(start_tag, "\\"))
  
  
  # Add tags to the text
  intermediate_sentence <- sentence %>%
    stringr::str_replace_all(pattern = start_groups_pattern,
                             replacement = start_replacement) 
  
  # Repeat same for the closing tag
  positions_second <- tibble::as_tibble((stringr::str_locate_all(intermediate_sentence,
                                                                 pattern = stringr::regex(stringr::str_c(patterns, collapse = "|"),
                                                                                          ignore_case = TRUE))
  )[[1]]) %>%
    # Adjust the position of tag insertion
    dplyr::mutate(start = start - 1,
                  end = end)
  
  end_groups <- c(positions_second$end[[1]],
                  (positions_second$end - dplyr::lag(positions_second$end))[-1])
  
  end_groups_pattern <- purrr::map_chr(end_groups,
                                       ~stringr::str_c("(.{", .x, "})")) %>%
    stringr::str_c(collapse = "") %>%
    stringr::str_c("^", ., "(.+)$")
  
  end_tag <- "</mark>"
  
  end_replacement <- stringr::str_c(
    "\\", seq_along(positions_second$end), collapse = end_tag
  ) %>%
    stringr::str_c(., nrow(positions_second)+1, sep = stringr::str_c(end_tag, "\\"))
  
  new_sentence <- intermediate_sentence %>%
    stringr::str_replace_all(pattern = end_groups_pattern,
                             replacement = end_replacement) 
  
  return(new_sentence) 
}
