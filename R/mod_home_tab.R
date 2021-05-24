#' home_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_tab_ui <- function(id){
  ns <- NS(id)
  home_tab = shinydashboard::tabItem(
    tabName = "home",
    fluidRow(
      mod_home_motivation_box_ui(ns("home_motivation_box_ui_1")),
      
      shinydashboardPlus::box(
        width = 4,
        title = "Text sieve", 
        icon = shiny::icon("file-alt"),
        collapsible = TRUE,
        "Text sieve description"
      )),
    fluidRow(
      mod_home_reading_time_box_ui(ns("home_reading_time_box_ui_1")),

      shinydashboardPlus::box(
        width = 4,
        title = "Abbrevimate",
        icon = shiny::icon("font"),
        collapsible = TRUE,
        "Abbrevimate Description"
      )
    )
  )
  return(home_tab)
}

#' home_tab Server Functions
#'
#' @noRd 
mod_home_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_home_motivation_box_server("home_motivation_box_ui_1")
    mod_home_reading_time_box_server("home_reading_time_box_ui_1")
  })
}


## To be copied in the UI
# 

## To be copied in the server
# 
