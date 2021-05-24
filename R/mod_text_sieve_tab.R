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
    h2("Text sieve")
  )
  return(text_sieve)
}
    
#' text_sieve_tab Server Functions
#'
#' @noRd 
mod_text_sieve_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
