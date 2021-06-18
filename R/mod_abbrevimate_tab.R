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
    
    mod_abbrevimate_settings_box_ui(ns("abbrevimate_settings_box_ui_1")),
    mod_abbrevimate_library_box_ui(ns("abbrevimate_library_box_ui_1")),
    mod_abbrevimate_pos_hits_box_ui(ns("abbrevimate_pos_hits_box_ui_1")),
    mod_abbrevimate_neg_hits_box_ui(ns("abbrevimate_neg_hits_box_ui_1"))
    
  )
  return(abbrevimate)
}

#' abbrevimate_tab Server Functions
#'
#' @noRd 
mod_abbrevimate_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_abbrevimate_settings_box_server("abbrevimate_settings_box_ui_1")
    mod_abbrevimate_library_box_server("abbrevimate_library_box_ui_1")
    mod_abbrevimate_pos_hits_box_server("abbrevimate_pos_hits_box_ui_1",
                                        pos_hits = abbr_return_values$true_abbr)
    mod_abbrevimate_neg_hits_box_server("abbrevimate_neg_hits_box_ui_1")
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
