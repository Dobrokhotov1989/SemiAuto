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
  
  results_box <- tagList(
    shinydashboardPlus::box(
      title = "Analysis results",
      width = 12,
      collapsible = TRUE,
      icon = shiny::icon("receipt"),
      
      htmlOutput(outputId = ns("bttns")),
      br(),
      br(),
      DT::dataTableOutput(outputId = ns("results"))
    )
  )
  
  return(results_box)

}

#' text_sieve_results_box Server Functions
#'
#' @noRd 
mod_text_sieve_results_box_server <- function(id, tbl){
  moduleServer( id, function(input, output, session){
    require(magrittr)
    
    rv <- reactiveValues()
    
    ns <- session$ns
    
    output$bttns <- renderUI({
      tagList(
        div(
          shinyBS::tipify(
            shinyWidgets::downloadBttn(
              outputId = ns("download"),
              label = "",
              style = "material-flat",
              size = "sm"
            ),
            title = "Download results as html",
            placement = "bottom"
          ),
          
          style="float:right"
        )
      )
    })
    
    output$results <- DT::renderDataTable({

      if(tibble::is_tibble(tbl())){
        if(nrow(tbl()) > 0){
          results_tbl <- tbl() %>% dplyr::select(
            pmcid, sentences, doi, title,
            authorString, firstPublicationDate,
            paragraphs
          ) %>%
            dplyr::mutate(" " = '&oplus;',
                          doi = glue::glue(
                            "<a href='https://www.doi.org/{doi}'>{doi}</a>"
                          ),
                          title = glue::glue("<b>{title}</b>"),
                          authorString = glue::glue(
                            "<small>{authorString}</small>"
                            ),
                          firstPublicationDate = glue::glue(
                            "<small>{firstPublicationDate}</small>"
                          )
                          )
          
          results_dt <- DT::datatable(
            data = results_tbl,
            escape = FALSE,
            filter = "none",
            callback = datatable_callback(),
            selection = "none",
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
          ) %>% 
            # Column specific formatting
            DT::formatStyle(1, `vertical-align` = "top") %>%
            DT::formatStyle(8, `font-size` = "20px", cursor = "pointer")
          
        } else if(nrow(tbl()) == 0){
          
          results_tbl <- tibble::tibble(Results = "Nothing found")
          
          results_dt <- DT::datatable(
            data = results_tbl,
            escape = FALSE,
            selection = "none",
            rownames = FALSE)
          
        }
      } else {
        results_tbl <- tibble::tibble(Results = "Nothing to show")
        
        results_dt <- DT::datatable(
          data = results_tbl,
          escape = FALSE,
          selection = "none",
          rownames = FALSE)
      }

      rv$results_dt <- results_dt
      
      return(results_dt)
      
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste('TS_results_', Sys.Date(), '.html', sep='')
      },
      content = function(file) {
        htmlwidgets::saveWidget(rv$results_dt, file)
      }
    )
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
