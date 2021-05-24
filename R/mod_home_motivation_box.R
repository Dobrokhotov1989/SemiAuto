#' home_motivation_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_motivation_box_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    width = 8,
    title = "Motivation",
    icon = shiny::icon("question"),
    collapsible = TRUE,
    sidebar = shinydashboardPlus::boxSidebar(
      id = ns("motivation_side"),
      width = 30,
      shinyWidgets::knobInput(
        ns("n_of_years"), 
        label = "Number of years:",
        min = 20, 
        max = floor((year_now()-oldest_pub_year())/20)*20, 
        step = 20,
        value = 100,
        displayPrevious = TRUE, 
        lineCap = "round",
        immediate = FALSE,
        post = "yr",
        displayInput = TRUE,
        width = "90%"
      )),
    column(
      width = 7,
      
      "Over the last 100 years number of publication in scientific journals grows rapidly.
      It becomes more difficult (if not impossible) to read all new papers issue even in 
      relatively narrow field. There is number of projects that working on the fully 
      automated text analysis, however the available tools requires considerable 
      amount of experties to use properly. The purpose of this app is to help you
      separate the wheat from the chaff.",
      br(),
      br(),
      "Plot shows number of documents (indexed in EuropePMC) published each year.
      Click on the gear-icon to adjust the years range."
      
    ),
    column (
      width = 5,
      plotOutput(outputId = ns("pubs_pl"))
    )
  )
}
    
#' home_motivation_box Server Functions
#'
#' @noRd 
mod_home_motivation_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$pubs_pl <- renderPlot({
      
      end_date <- year_now()
      
      n_years <- input$n_of_years
      
      pubs_sub <- dplyr::filter(pubs_trend_all_years,
                                dplyr::between(year, (end_date - n_years), (end_date - 1)))
      
      
      pubs_plot <- ggplot2::ggplot(data = pubs_sub,
                                   ggplot2::aes(x = year,
                                                y = query_hits))+
        ggplot2::geom_point(fill = "grey15") +
        ggplot2::geom_line(stat = "smooth",
                           method = "loess",
                           formula = y ~ poly(x),
                           span = 0.5,
                           arrow = ggplot2::arrow(angle = 10,
                                                  length = ggplot2::unit(0.7, "cm"),
                                                  type = "closed"),
                           size = 1,
                           color = "red") +
        ggplot2::scale_x_continuous(limits = c(end_date - n_years - 1,
                                               end_date),
                                    breaks = seq(from = end_date - n_years - 1,
                                                 to = end_date-1,
                                                 length.out = 5))+ 
        ggplot2::ggtitle(label = sprintf("Pubs over last %s years", input$n_of_years))+
        ggplot2::theme_void()+
        ggplot2::theme(axis.text.x = ggplot2::element_text(),
                       plot.title = ggplot2::element_text(margin = ggplot2::margin(b = -15),
                                                          size = 14))
      
      return(pubs_plot)
      
    }, bg="transparent", )
    
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
