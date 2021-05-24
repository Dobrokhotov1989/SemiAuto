#' about_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_tab_ui <- function(id){
  ns <- NS(id)
  about = shinydashboard::tabItem(
    tabName = "about",
    h2("about")
  )
  return(about)
}
    
#' about_tab Server Functions
#'
#' @noRd 
mod_about_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
