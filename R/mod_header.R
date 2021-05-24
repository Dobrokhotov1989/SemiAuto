#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_header_ui <- function(id){
  ns <- NS(id)
  header = shinydashboardPlus::dashboardHeader(
    title = tagList(span(class = "logo-lg", "SemiAuto"),
                    img(src = "https://global-uploads.webflow.com/5e157547d6f791d34ea4e2bf/6087f2b060c7a92408bac811_logo.svg" )
    )
  )
  
}
    
#' header Server Functions
#'
#' @noRd 
mod_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
