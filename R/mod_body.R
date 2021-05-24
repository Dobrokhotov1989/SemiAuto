#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_ui <- function(id){
  ns <- NS(id)
  
  body = shinydashboard::dashboardBody(
    id = ns("body"),
    shinydashboard::tabItems(
      mod_home_tab_ui(ns("home_tab_ui_1")),
      mod_text_sieve_tab_ui("text_sieve_tab_ui_1"),
      mod_abbrevimate_tab_ui("abbrevimate_tab_ui_1"),
      mod_about_tab_ui("about_tab_ui_1")
    )
  )

}

#' body Server Functions
#'
#' @noRd 
mod_body_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_home_tab_server("home_tab_ui_1")
    mod_text_sieve_tab_server("text_sieve_tab_ui_1")
    mod_abbrevimate_tab_server("abbrevimate_tab_ui_1")
    mod_about_tab_server("about_tab_ui_1")
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
