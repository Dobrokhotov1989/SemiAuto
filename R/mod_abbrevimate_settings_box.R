#' abbrevimate_settings_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_settings_box_ui <- function(id){
  
  ns <- NS(id)
  
  shinydashboardPlus::box(
    title = "Settings",
    width = 12,
    collapsible = TRUE,
    icon = shiny::icon("search-plus"),
    column(
      width = 4,
      shinyWidgets::materialSwitch(
        inputId = ns("batch"),
        label = "Batch mode",
        value = FALSE, 
        status = "info"
      ),
      br(),
      
      htmlOutput(outputId = ns("search_input"))
      
    ),
    
    column(
      width = 4,
      shinyWidgets::radioGroupButtons(
        inputId = ns("source"),
        choices = c("EuropePMC", 
                    "Local files"),
        selected = "EuropePMC",
        justified = TRUE
      ),
      br(),
      htmlOutput(outputId = ns("source_input"))
      
    ),
    
    column(
      width = 4,
      shinyWidgets::materialSwitch(
        inputId = ns("advanced"),
        label = "Advanced settings",
        value = FALSE, 
        status = "info"
      ),
      htmlOutput(outputId = ns("advanced_set")),
      textOutput(outputId = ns("text"))
      
    )
  )

}

#' abbrevimate_settings_box Server Functions
#'
#' @noRd 
mod_abbrevimate_settings_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues()
    
    #### Remember typed in the searchInput field text ####
    rv$search_term <- ""
    rv$query <- ""
    rv$deriv <- FALSE
    rv$precise <- FALSE
    rv$max <- 0
    rv$dates <- NULL
    
    
    observeEvent(input$search_term_search, {
      
      rv$search_term <- input$search_term
    })
    
    observeEvent(input$search_term_reset, {
      rv$search_term <- ""
    })
    
    observeEvent(input$epmc_query_search, {
      
      rv$query <- input$epmc_query
    })
    
    observeEvent(input$epmc_query_reset, {
      rv$query <- ""
    })
    
    observeEvent(input$derivatives, {
      rv$deriv <- input$derivatives
    })
    
    observeEvent(input$precise, {
      rv$precise <- input$precise
    })
    
    max_pubs <- reactive({
      if (is.null(input$max_pubs)) { return(0)} else {return(input$max_pubs)}
    })


   
    observeEvent(
      eventExpr = {
        input$advanced
        input$precise
      },
      handlerExpr = {
        rv$dates <- input$dates
      })
    
    #### Render UI based on the conditions ####
    output$search_input <- renderUI({
      
      if(isTRUE(input$batch)){
        field <- tagList(
          column(width = 8,
                 style= "padding:0px;",
                 shiny::fileInput(
                   inputId = ns("search_batch"),
                   label = "Select csv file",
                   accept = ".csv"
                 )
          ),
          column(width = 4,
                 style= "padding:0px; margin-top:32px; padding-left:5px",
                 shinyWidgets::prettySwitch(
                   inputId = ns("header"),
                   label = "Header",
                   fill = TRUE,
                   status = "info"
                 )
          )
        )
        return(field)
      } else {
        field <- shinyWidgets::searchInput(
          inputId = ns("search_term"), 
          label = "Enter term here",
          placeholder = "...",
          value = rv$search_term,
          btnSearch = shiny::icon("save"), 
          btnReset = shiny::icon("remove"),
          width = "100%"
        )
        return(field)
      }
    })
    
    output$source_input <- renderUI({
      
      if (input$source == "EuropePMC"){
        
        field <- shinyWidgets::searchInput(
          inputId = ns("epmc_query"), 
          label = "Enter query here",
          placeholder = "...",
          value = rv$query,
          btnSearch = shiny::icon("save"), 
          btnReset = shiny::icon("remove"),
          width = "100%"
        )
        return(field)
      } else {
        
        field <- shiny::fileInput(
          inputId = ns("source_local"),
          label = "Select papers in .pdf format",
          accept = "application/pdf",
          multiple = TRUE
        )
        return(field)
      }
    })
    
    output$advanced_set <- renderUI({
      
      rv$max <- shiny::restoreInput(id = ns("max_pubs"), default = 0)
      
      if (isTRUE(input$advanced)){
        if (input$source == "EuropePMC") {
          
          field <- tagList(
            shinyWidgets::materialSwitch(
              inputId = ns("derivatives"),
              label = "Include derivatives",
              value = rv$deriv, 
              status = "info"
            ),
            shinyWidgets::searchInput(inputId = ns("max_pubs"), 
                                      label = "Max number of publication (0 for unlimited)",
                                      value = max_pubs(),
                                      width = "100%"),
            shinyWidgets::materialSwitch(
              inputId = ns("precise"),
              label = "Precise date range",
              value = rv$precise, 
              status = "info"
            ),
            htmlOutput(outputId = ns("date_range_ui"))
          )
          
        } else {
          field <- tagList(
            shinyWidgets::materialSwitch(
              inputId = ns("derivatives"),
              label = "Include derivatives",
              value = rv$deriv, 
              status = "info"
            )
          )
        }
      }
      
    })
    
    # If precise is selected then user can define date range as range between
    # specific days, otherwise as year range
    output$date_range_ui <- renderUI({
      
      if(isTRUE(input$precise)){
        field <- shinyWidgets::airDatepickerInput(
          inputId = ns("dates"),
          label = "Select years range",
          value = rv$dates,
          range = TRUE,
          minDate = paste0(oldest_pub_year(), "-01-01"),
          maxDate = Sys.Date(),
          clearButton = TRUE,
          view = "day",
          minView = "day",
          dateFormat = "yyyy-mm-dd")
        return(field)
      } else {
        field <- shinyWidgets::airDatepickerInput(
          inputId = ns("dates"),
          label = "Select years range",
          value = rv$dates,
          range = TRUE,
          minDate = paste0(oldest_pub_year(), "-01-01"),
          maxDate = Sys.Date(),
          clearButton = TRUE,
          view = "years",
          minView = "years",
          dateFormat = "yyyy")
        return(field)
      }
    })

  })
  
  
}

## To be copied in the UI
# 

## To be copied in the server
# 
