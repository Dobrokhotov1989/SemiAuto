#' abbrevimate_neg_hits_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_neg_hits_box_ui <- function(id){
  ns <- NS(id)
  
  neg_hits_box <- tagList(
    shinydashboardPlus::box(
      title = "Negative hits",
      width = 4,
      collapsible = TRUE,
      icon = shiny::icon("folder-minus"),
      
      htmlOutput(outputId = ns("bttns_neg_hits")),
      
      br(),
      br(),
      
      DT::dataTableOutput(outputId = ns("neg_hits"))
    )
  )
  return(neg_hits_box)
}

#' abbrevimate_neg_hits_box Server Functions
#'
#' @noRd 
mod_abbrevimate_neg_hits_box_server <- function(id, hits){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$bttns_neg_hits <- renderUI({
      tagList(
        div(
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("select_all_neg"),
              label = shiny::icon("check"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Select all",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("deselect_all_neg"),
              label = shiny::icon("remove"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Clear selection",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("add_to_lib_neg"),
              label = shiny::icon("book-medical"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Add selected to the library",
            placement = "bottom"
          ),
          
          style="float:right"
        )
      )
    })
    
    output$neg_hits <- DT::renderDataTable({
      
      if(not_null(hits()$false_abbr)){
        results_tbl <- tibble::tibble(Results = hits()$false_abbr)
      } else {
        results_tbl <- tibble::tibble(Results = "Nothing to show")
      }
      
      DT::datatable(
        data = results_tbl,
        rownames = FALSE,
        selection = list(
          mode = "multiple",
          selected = NULL,
          target = "row",
          selectable = TRUE
        )
      )
    })
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
