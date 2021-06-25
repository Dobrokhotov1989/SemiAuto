#' text_sieve_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_sieve_tab_ui <- function(id){
  ns <- NS(id)
  text_sieve = shinydashboard::tabItem(
    tabName = "text_sieve",
    mod_text_sieve_settings_box_ui(ns("text_sieve_settings_box_ui_1")),
    mod_text_sieve_results_box_ui(ns("text_sieve_results_box_ui_1"))
  )
  return(text_sieve)
}
    
#' text_sieve_tab Server Functions
#'
#' @noRd 
mod_text_sieve_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    text_sieve_return_values <- mod_text_sieve_settings_box_server("text_sieve_settings_box_ui_1")
    mod_text_sieve_results_box_server("text_sieve_results_box_ui_1",
                                      tbl = text_sieve_return_values)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
