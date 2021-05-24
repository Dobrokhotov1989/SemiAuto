#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      title = "Semi_Auto",
      md = FALSE,
      scrollToTop = TRUE,
      #preloader = list(html = waiter::spin_4(), color = "#FFAAAA"),
      controlbar = shinydashboardPlus::dashboardControlbar(
        shinydashboardPlus::skinSelector()
        ),
      
      header = mod_header_ui("header_ui_1"),
      sidebar = mod_sidebar_ui("sidebar_ui_1"),
      footer = mod_footer_ui("footer_ui_1"),
      body = mod_body_ui("body_ui_1")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Semi_auto'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

