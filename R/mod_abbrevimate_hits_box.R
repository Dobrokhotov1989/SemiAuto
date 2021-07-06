#' abbrevimate_hits_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_hits_box_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' abbrevimate_hits_box Server Functions
#'
#' @noRd 
mod_abbrevimate_hits_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_abbrevimate_hits_box_ui("abbrevimate_hits_box_ui_1")
    
## To be copied in the server
# mod_abbrevimate_hits_box_server("abbrevimate_hits_box_ui_1")
