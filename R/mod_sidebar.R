#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  sidebar = shinydashboardPlus::dashboardSidebar(
    id = ns("sidebar"),
    minified = TRUE,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(text = "Home", 
                               tabName  = "home", 
                               icon = shiny::icon("home")),
      shinydashboard::menuItem(text = "Text sieve", 
                               tabName  = "text_sieve", 
                               icon = shiny::icon("file-alt")),
      shinydashboard::menuItem(text = "Abbrevimate",
                               tabName  = "abbrevimate",
                               icon = shiny::icon("font")),
      shinydashboard::menuItem(text = "About",
                               tabName  = "about",
                               icon = shiny::icon("address-card"))
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
