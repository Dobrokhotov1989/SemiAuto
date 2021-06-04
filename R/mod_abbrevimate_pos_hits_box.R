#' abbrevimate_pos_hits_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_pos_hits_box_ui <- function(id){
  ns <- NS(id)
  
  pos_hits_box <- tagList(
    shinydashboardPlus::box(
      title = "Positive hits",
      width = 4,
      collapsible = TRUE,
      icon = shiny::icon("folder-plus"),
      
      htmlOutput(outputId = ns("bttns_pos_hits")),

      br(),
      br(),
      
      DT::dataTableOutput(outputId = ns("pos_hits"))
    )
  )
  
  return(pos_hits_box)
}

#' abbrevimate_pos_hits_box Server Functions
#'
#' @noRd 
mod_abbrevimate_pos_hits_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$bttns_pos_hits <- renderUI({
      tagList(
        div(
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("select_all"),
              label = shiny::icon("check"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Select all",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("deselect_all"),
              label = shiny::icon("remove"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Clear selection",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("add_to_lib"),
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
    
    output$pos_hits <- DT::renderDataTable({
      tibble::tibble(a = 1:3,
                     b = LETTERS[1:3])
    })
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
