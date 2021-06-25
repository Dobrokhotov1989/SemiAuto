#' text_sieve_results_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_sieve_results_box_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    title = "Analysis results",
    width = 12,
    collapsible = TRUE,
    icon = shiny::icon("folder-plus"),
    DT::dataTableOutput(outputId = ns("results"))
  )
}

#' text_sieve_results_box Server Functions
#'
#' @noRd 
mod_text_sieve_results_box_server <- function(id, tbl){
  moduleServer( id, function(input, output, session){
    require(magrittr)
    
    ns <- session$ns
    
    output$results <- DT::renderDataTable({
      
      if(tibble::is_tibble(tbl())){
        if(nrow(tbl()) > 0){
          #browser()
          results_tbl <- tbl() %>% dplyr::select(
            pmcid, sentences, doi, title,
            authorString, firstPublicationDate,
            paragraphs
          ) %>%
            dplyr::mutate(" " = '&oplus;')
          
        } else if(nrow(tbl()) == 0){
          
          results_tbl <- tibble::tibble(Results = "Nothing found")
          
        }
      } else {
        results_tbl <- tibble::tibble(Results = "Nothing to show")
      }

      results_dt <- DT::datatable(
        data = results_tbl,
        escape = FALSE,
        filter = "top",
        callback = datatable_callback(),
        options = list(
          # Show 5 rows by default
          pageLength = 5,
          # But it will be possible to show up to a 100 rows
          lengthMenu = c(5, 10, 25, 50, 100),
          # Some column specific settings
          columnDefs = list(
            # column 0 (row numbers) and all others 
            # except pmcid, sentences, and "+" are hidden
            list(visible = FALSE, targets = c(0, 3, 4, 5, 6, 7)),
            # The special column with (+) gets the expand-control class so that it
            # triggers the callback code
            list(orderable = FALSE, className = 'expand-control', targets = 8)
          )
        )
        
        #selection = list(
        #  mode = "multiple",
        #  target = "row",
        #  selectable = TRUE
        #)
      ) %>% 
        # Column specific formatting
        DT::formatStyle(8, `font-size` = "20px", cursor = "pointer")
      
    })
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
