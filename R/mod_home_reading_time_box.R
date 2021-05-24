#' home_reading_time_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_reading_time_box_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    width = 8,
    title = "How long would you read abstracts?",
    icon = shiny::icon("search-plus"),
    collapsible = TRUE,
    sidebar = shinydashboardPlus::boxSidebar(
      id = ns("motivation_side"),
      width = 30,
      textOutput(outputId = ns("info_text"))
      ),
    
    fluidRow(
      column(width = 6,
             style='padding-left:50px;', #to prevent it escaping the box
             fluidRow(
               shinyWidgets::searchInput(
                 inputId = ns("search"), 
                 label = "Subject of interest",
                 placeholder = "crispr",
                 value = "crispr",
                 btnSearch = icon("search"), 
                 btnReset = icon("remove"),
                 width = "100%"
               )
             ),
             fluidRow(
               column(width = 6,
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("reader"),
                        label = "English proficiency",
                        choiceNames = c("Non-native", "Native", "Reading champ"),
                        choiceValues = c(200, 500, 4700), #reading spead in wpm
                        justified = TRUE,
                        direction = "vertical"
                      )
               ),
               column(width = 6,
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("units"),
                        label = "Units",
                        choices = c("Anna Karenina", "Flight London-NY", "Game of Thrones"),
                        justified = TRUE,
                        direction = "vertical"
                      )
               )
             )
      ),
      column(width = 6, style='padding-right:25px;',
             h4("Abstracts reading is equivalent of"),
             br(),
             htmlOutput(outputId = ns("icons"))
      )
    )
  )
}

#' home_reading_time_box Server Functions
#'
#' @noRd 
mod_home_reading_time_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    values <- reactiveValues()
    
    output$icons <- renderUI({
      
      n_of_hits <- europepmc::epmc_hits(query = input$search,
                                        verbose = FALSE)
      
      reading_speed <- as.numeric(input$reader)
      
      show_icon <- dplyr::case_when(input$units == "Game of Thrones" ~ "play-circle",
                                    input$units == "Flight London-NY" ~ "plane-departure",
                                    input$units == "Anna Karenina" ~ "book")
      #See details in Reading_time.Rmd
      duration <- dplyr::case_when(input$units == "Game of Thrones" ~ 70,
                                   input$units == "Flight London-NY" ~ 8,
                                   input$units == "Anna Karenina" ~ round(349736/reading_speed/60, digits = 1))
      
     
      #See details in Reading_time.Rmd
      abstract_avg_length = 210
      
      #See details in Reading_time.Rmd
      values$n_of_rep <-
        ceiling((n_of_hits*abstract_avg_length/reading_speed + n_of_hits*0.25)/60/duration)
      
      
      
      # Use logic from fontawesome::fa() to make a multiple icons in a row
      svg <- paste(rep(fontawesome::fa(name = show_icon,
                                       height = "2em",
                                       margin_left = "0.2em",
                                       margin_right = "0.2em"),
                       times = values$n_of_rep),
                   collapse = "")
      svg <- HTML(svg)
      svg <- structure(svg, class = c("fontawesome", "svg", class(svg)))
      
    })
    
    output$info_text <- renderText({
      
      srch_q <- input$search
      
      verb <- dplyr::case_when(input$units == "Game of Thrones" ~ 
                                 "watch all 8 seasons of the Game of Thrones series",
                               input$units == "Flight London-NY" ~
                                 "flight from London to New York",
                               input$units == "Anna Karenina" ~ 
                                 "read novel 'Anna Karenina' by Leo Tolstoy")
      
     sprintf("Time to read all abstracts for the search query %s is equivalent
              to the time required to %s aproximately %s times",
             srch_q, verb, values$n_of_rep)
      
    })
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
