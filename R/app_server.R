#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_header_server("header_ui_1")
  mod_sidebar_server("sidebar_ui_1")
  mod_footer_server("footer_ui_1")
  mod_body_server("body_ui_1")
}
