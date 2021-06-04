#' abbrevimate_library_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_library_box_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    title = "Abbreviation labrary",
    width = 4,
    collapsible = TRUE,
    icon = shiny::icon("book-medical"),
    
    htmlOutput(outputId = ns("bttns_lib")),
    br(),
    br(),
    
    shinyWidgets::searchInput(
      inputId = ns("manual_add"), 
      placeholder = "Add abbreviation manually",
      btnSearch = shiny::icon("plus"), 
      btnReset = shiny::icon("remove"),
      width = "100%"
    ),
    
    br(),
    br(),
    
    DT::dataTableOutput(outputId = ns("abb_library"))
  )
}

#' abbrevimate_library_box Server Functions
#'
#' @noRd 
mod_abbrevimate_library_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$bttns_lib <- renderUI({
      tagList(
        div(
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("remove_selected"),
              label = shiny::icon("minus"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Remove selected from the library",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("clear_all"),
              label = shiny::icon("remove"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Clear library",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("download_lib"),
              label = shiny::icon("download"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Download library",
            placement = "bottom"
          ),
          
          style="float:right"
        )
      )
    })
    
    output$abb_library <- DT::renderDataTable({
      tibble::tibble(a = 1:3,
                     b = LETTERS[1:3])
    })
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
