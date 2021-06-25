#' text_sieve_settings_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_sieve_settings_box_ui <- function(id){
  ns <- NS(id)
  
  shinydashboardPlus::box(
    title = "Settings",
    width = 12,
    collapsible = TRUE,
    icon = shiny::icon("search-plus"),
    column(
      width = 4,
      shinyWidgets::radioGroupButtons(
        inputId = ns("source"),
        choices = c("EuropePMC", 
                    "Local files"),
        selected = "EuropePMC",
        justified = TRUE
      ),
      
      htmlOutput(outputId = ns("source_input")),
      
      tagList(
        column(width = 8,
               style= "padding:0px;",
               shiny::fileInput(
                 inputId = ns("dic"),
                 label = "Select two or more dictionaries",
                 accept = ".csv",
                 multiple = TRUE
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
    ),
    
    column(
      width = 4,
      htmlOutput(outputId = ns("advanced_set")),
      textOutput(outputId = ns("text"))
      
    ),
    
    column(
      width = 4,
      shiny::actionButton(inputId = ns("search"),
                          label = "Search",
                          icon = shiny::icon("search"))
      
    )
  )
}

#' text_sieve_settings_box Server Functions
#'
#' @noRd 
mod_text_sieve_settings_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues()
    
    #### Render UI based on the conditions ####
    output$source_input <- renderUI({
      
      if (input$source == "Local files"){
        
        field <- shiny::fileInput(
          inputId = ns("source_local"),
          label = "Select papers in .pdf format",
          accept = "application/pdf",
          multiple = TRUE
        )
        return(field)
      } else {
        field <- shiny::textInput(
          inputId = ns("search_term"), 
          label = "Enter query here",
          placeholder = "...",
          value = NULL,
          width = "100%"
        )
        return(field)
      }
    })
    
    output$advanced_set <- renderUI({
      if (input$source == "EuropePMC") {
        
        field <- tagList(
          numericInput(inputId = ns("max_pubs"), 
                       label = "Max number of pubs (0 = inf)",
                       value = 0,
                       width = "100%"),
          shinyWidgets::materialSwitch(
            inputId = ns("precise"),
            label = "Precise date range",
            status = "info"
          ),
          htmlOutput(outputId = ns("date_range_ui"))
        )
        
      } else {
        field <- tagList(
        )
      }
      
      
    })
    
    # If precise is selected then user can define date range as range between
    # specific days, otherwise as year range
    output$date_range_ui <- renderUI({
      
      if(isTRUE(input$precise)){
        field <- shinyWidgets::airDatepickerInput(
          inputId = ns("dates"),
          label = "Select years range",
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
    
    observeEvent(input$search, {
      
      patterns <- abbr_dictionaries_to_patterns(files = input$dic$datapath,
                                                header = input$header)
      
      if(input$source == "EuropePMC"){
        
        # Limit set at x5 of max_pubs limit to ensure max_pubs number of open
        # access publication.
        pubs_list <- abbr_epmc_search(query = input$search_term,
                                      limit = input$max_pubs*5,
                                      date_range = input$dates,
                                      precise = input$precise)
        
        #Split pubs_list on the "searchable" (open) and unsearchable (closed) papers
        open_pubs_list <- pubs_list %>%
          dplyr::filter(isOpenAccess == "Y" &
                          not_na(pmcid)) 
        if (nrow(open_pubs_list) > input$max_pubs & input$max_pubs != 0){
          open_pubs_list <- open_pubs_list[1:input$max_pubs, ]
        }
        
        closed_pubs_list <- pubs_list %>%
          dplyr::filter(isOpenAccess == "N" | is.na(pmcid))
        
        coapp_tbl <- abbr_find_coappearance_epmc(patterns = patterns,
                                                 pmcid = open_pubs_list$pmcid)
        
        analysis_results <- dplyr::left_join(x = coapp_tbl,
                                             y = open_pubs_list,
                                             by = "pmcid")

        return(
          rv$analysis_results <- analysis_results
        )
      }
    })
    
    return(reactive(rv$analysis_results))
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
