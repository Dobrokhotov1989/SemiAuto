#' abbr_find_coappearance_epmc 
#'
#' @description A fct function searches co-appearance of patterns in papers from
#' EuropePMC
#'
#' @return The return value is tibble with sentences there patterns co-appear. 
#' Additionally tibble contains columns "pmcid" and "paragraphs".
#'
#' @noRd
abbr_find_coappearance_epmc <- function(patterns,
                                        pmcid,
                                        highlight = "#D1D0DE"){
  
  require(magrittr)

  # This helper expression allows dynamically evaluate appearance of each 
  # pattern in the sentences
  # (https://community.rstudio.com/t/dynamically-add-columns-to-tibble/108233)
  #
  # Because of parse_expr (probably) one \ from \\b is "used" therefore each
  # \\ should be replaced by \\\\
  mutator_patterns <- purrr::map_chr(patterns,
                             ~ stringr::str_replace_all(
                               .x,
                               pattern = "\\\\",
                               replacement = "\\\\\\\\"
                             ))
  (mutator_expressions <- purrr::map(
    mutator_patterns,
    ~rlang::parse_expr(
      glue::glue("stringr::str_detect(sentences, stringr::regex(pattern = '{.x}', ignore_case = TRUE))"))
  ) %>%
      rlang::set_names(glue::glue("word_{seq_along(mutator_patterns)}")
      )
  )
  
  # Downloads papers from the pmcid vector & searches for the patterns
  # co-appearance 
  purrr::map_dfr(pmcid,
                 function(x){
                   # Download
                   paper <- attempt::attempt({
                     europepmc::epmc_ftxt(ext_id = x)
                   })
                   
                   if(attempt::is_try_error(paper)){
                     return(NULL)
                   } else {
                     # Xml to character vector (titles and paragraphs only)
                     trimed <- c(trimws(xml2::xml_find_all(paper, xpath = "//title")),
                                 trimws(xml2::xml_find_all(paper, xpath = "//p")))
                     
                     # Split into sentences
                     tokens <- tidytext::unnest_tokens(
                       tbl = tibble::tibble(paragraphs = trimed),
                       output = sentences,
                       input = paragraphs,
                       token = "sentences",
                       to_lower = FALSE,
                       drop = FALSE,
                       collapse = FALSE)
                     
                     # Evaluate co-appearance of patterns
                     tokens <- tokens %>% 
                       dplyr::mutate(!!!mutator_expressions,
                                     pmcid = x) %>%
                       dplyr::filter(
                         rlang::eval_tidy(rlang::parse_expr(
                           stringr::str_c(
                             glue::glue("word_{seq_along(patterns)} == TRUE"),
                             collapse = " & ")
                         )))%>%
                       dplyr::select(pmcid, paragraphs, sentences) %>%
                       unique()
                     browser()
                     # Highlight found words
                     tokens <- tokens %>%
                       dplyr::mutate(
                         dplyr::across(
                           .cols = c(paragraphs, sentences),
                           .fns = ~ purrr::map(.x,
                                               ~ add_highlight(.x,
                                                               patterns = patterns,
                                                               color = highlight)
                             )
                         ),
                         dplyr::across(
                           .cols = c(paragraphs, sentences),
                           .fns = ~ purrr::map(.x,
                                          ~ remove_tags(.x,
                                                        tags = c("<title>",
                                                                 "</title>",
                                                                 "<p>",
                                                                 "</p>")
                                                        )
                                          )
                           )
                         )
                       
                   }
                 })

}