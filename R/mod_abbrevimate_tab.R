#' abbrevimate_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_tab_ui <- function(id){
  ns <- NS(id)
  abbrevimate = shinydashboard::tabItem(
    tabName = "abbrevimate",
    h2("abbrevimate")
  )
  return(abbrevimate)
}
    
#' abbrevimate_tab Server Functions
#'
#' @noRd 
mod_abbrevimate_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
