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
      htmlOutput(outputId = ns("source_input")),
      br(),
      shiny::actionButton(inputId = ns("search"),
                          label = "Search",
                          icon = shiny::icon("search"))
      
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
    
    
    
    observeEvent(
      eventExpr = {
        
        input$search_term_search
        input$search
      },
      handlerExpr = {
        
        rv$search_term <- input$search_term
      })
    
    observeEvent(input$search_term_reset, {
      rv$search_term <- ""
    })
    
    observeEvent(
      eventExpr = {
        
        input$epmc_query_search
        input$search
      }, 
      
      handlerExpr = {
        
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
    
    rv$max <- max_pubs
    
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
      
      if (input$source == "EuropePMC" &
          rlang::is_false(input$batch)){
        
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
      } else if (input$source == "Local files"){
        
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
    
    observeEvent(input$search,
                 {
                   require(magrittr)
                   #For batch mode - read uploaded csv file 
                   if(rlang::is_true(input$batch)){
                     terms_list = readr::read_csv(file = input$search_batch,
                                                  col_names = input$header)
                     #tibble to vector; if more then one column - use only first
                     terms_list = dplyr::pull(terms_list, 1)
                   }
                   
                   if(input$source == "EuropePMC" & rlang::is_false(input$batch)){
                     
                     
                     
                     pubs_list <- abbr_epmc_search(query = input$epmc_query,
                                                   limit = rv$max,
                                                   date_range = rv$dates,
                                                   precise = rv$precise)
                     
                     pattern <- abbr_term_to_pattern(term = input$search_term,
                                                     derivatives = rv$deriv)
                     
                     #Split pubs_list on the "searchable" (open) and unsearchable (closed) papers
                     open_pubs_list <- pubs_list %>%
                       dplyr::filter(isOpenAccess == "Y" &
                                       not_na(pmcid)) 
                     
                     open_pubs_list <- open_pubs_list[1:10,]
                     
                     closed_pubs_list <- pubs_list %>%
                       dplyr::filter(isOpenAccess == "N" | is.na(pmcid))
                     
                     abbrs_vec <- c()
                     
                     abbrs_vec <- purrr::map(open_pubs_list$pmcid,
                                             function(x){
                                               
                                               paper <- europepmc::epmc_ftxt(ext_id = x)
                                               
                                               abbrs_vec <- append(abbrs_vec,
                                                                   abbr_extract_pattern_from_paper(x = paper,
                                                                                                   pattern = pattern,
                                                                                                   derivatives = rv$deriv)
                                               )
                                               
                                               return(abbrs_vec)
                                             })
                     
                     abbrs_vec <- unlist(abbrs_vec)
                     
                     abbrs_tbl <- abbr_split_term_and_abbr(abbrs_vec)
                     
                     abbrs_tbl$abbr_pattern <- purrr::map_chr(abbrs_tbl$abbr, abbr_abbreviation_to_pattern)
                     
                     abbrs_tbl$is_abbr <- purrr::map2_lgl(.x = abbrs_tbl$full,
                                                   .y = abbrs_tbl$abbr_pattern,
                                                   .f = ~ stringr::str_detect(string = .x,
                                                                              pattern = .y))
                     
                     abbrs_true <- abbrs_tbl %>%
                       dplyr::filter(is_abbr == TRUE) %>%
                       dplyr::select(abbr)
                     
                     abbrs_false <- abbrs_tbl %>%
                       dplyr::filter(is_abbr != TRUE) %>%
                       dplyr::select(abbr)
                     
                     abbr_return_values <- list(
                       true_abbr = abbrs_true,
                       false_abbr = abbrs_false,
                       closed_papers = closed_pubs_list
                     )
                     
                     return(abbr_return_values)
                     
                     
                   }
                   
                 })
    
  })
  
 
}

## To be copied in the UI
# 

## To be copied in the server
# 
