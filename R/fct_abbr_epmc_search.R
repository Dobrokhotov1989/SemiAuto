#' abbr_epmc_search 
#'
#' @description Function shapes query for EuropePMC and make a search.
#' For additional explanations see Abbrevimate.Rmd
#'
#' @return Returns tibble with four columns: pmid, pmcid, isOpenAccess,
#'  and firstPublicationDate
#'
#' @noRd
#' 
#' @examples
#' ## Not run: 
#' abbr_epmc_search(query = '(METHODS:"blebbistatin")', limit = 10, 
#' date_range = as.Date(c("2015-01-01", "2020-01-01")), precise = FALSE)
#' 
#' ## End(Not run)

abbr_epmc_search <- function(query, limit = NULL, date_range = NULL, precise = FALSE){
  
  #Define full query
  #If date range is not defined by user
  if(is.null(date_range)){
    
    query_full <- query
    
    # If specific dates selected by user  
  } else if (precise == TRUE){
    
    query_full <- sprintf("%s AND (FIRST_PDATE:[%s TO %s])",
                          query, date_range[[1]], date_range[[2]])
    
    # If only year selected by user
  } else if (precise == FALSE){
    
    start_d <- sprintf("%s-01-01", format(date_range[[1]], "%Y"))
    end_d <- sprintf("%s-12-31", format(date_range[[2]], "%Y"))
    query_full <- sprintf("%s AND (FIRST_PDATE:[%s TO %s])",
                          query, start_d, end_d)
    
  }
  
  # limit == 0 means "unlimited" number of publications
  # "unlimited' == maximum number of hits
  if (limit == 0) {
    # epmc_hits() somethimes returns less records than epmc_search()
    # for details see https://github.com/ropensci/europepmc/issues/39
    # limit <- europepmc::epmc_hits(query = query_full)
    
    # as a temporary measure
    my_hits_synonym <- europepmc::epmc_search(query = query_full,
                                              limit = 1,
                                              synonym = TRUE)
    limit <- attr(my_hits_synonym, "hit_count")
  } 

  search_results <- europepmc::epmc_search(
      query = query_full,
      limit = limit, 
      verbose = FALSE #verbose is error-prone
    ) %>%
      dplyr::select(pmid, pmcid, isOpenAccess, 
                    firstPublicationDate, inEPMC, inPMC,
                    title, authorString, doi)
    
    return(search_results)

}
