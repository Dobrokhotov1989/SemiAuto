#' footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_footer_ui <- function(id){
  ns <- NS(id)
  footer = shinydashboardPlus::dashboardFooter(
    left = "By Oleg Dobrokhotov",
    right = div(fontawesome::fa("creative-commons",
                                height = "2em",
                                margin_right = "0.2em"),
                fontawesome::fa("creative-commons-by",
                                height = "2em",
                                margin_left = "0.2em",
                                margin_right = "0.2em"),
                a("CC BY 4.0",
                  href = "https://creativecommons.org/licenses/by/4.0/"))
  )
  return(footer)
}
    
#' footer Server Functions
#'
#' @noRd 
mod_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
