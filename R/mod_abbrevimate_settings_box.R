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
      
      htmlOutput(outputId = ns("search_input")),
      
      br(),
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

#' abbrevimate_settings_box Server Functions
#'
#' @noRd 
mod_abbrevimate_settings_box_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues()
    
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
        field <- shiny::textInput(
          inputId = ns("search_term"), 
          label = "Enter term here",
          placeholder = "...",
          value = NULL,
          width = "100%"
        )
        return(field)
      }
    })
    
    output$source_input <- renderUI({
      
      if (input$source == "Local files"){
        
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
      if (input$source == "EuropePMC") {
        
        field <- tagList(
          shinyWidgets::materialSwitch(
            inputId = ns("derivatives"),
            label = "Include derivatives",
            status = "info"
          ),
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
          shinyWidgets::materialSwitch(
            inputId = ns("derivatives"),
            label = "Include derivatives",
            status = "info"
          )
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
    
    #### Search functionality ####
    observeEvent(input$search, {
      
      if(!is.numeric(input$max_pubs)){
        
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "Max number of pubs should be a number",
          type = "warning"
        )
        
      } else if(input$search_term == "") {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "Enter term",
          type = "warning"
        )
      } else {
        
        require(magrittr)
        
        shinyWidgets::progressSweetAlert(
          session = session, id = "myprogress",
          title = "Loading list of papers",
          display_pct = TRUE, value = 0
        )
        
        #For batch mode - read uploaded csv file 
        if(rlang::is_true(input$batch)){
          terms_list = readr::read_csv(file = input$search_batch,
                                       col_names = input$header)
          #tibble to vector; if more then one column - use only first
          terms_list = dplyr::pull(terms_list, 1)
        }
        
        if(input$source == "EuropePMC" & rlang::is_false(input$batch)){
          
          # Limit set at x5 of max_pubs limit to ensure max_pubs number of open
          # access publication.
          pubs_list <- abbr_epmc_search(query = input$search_term,
                                        limit = input$max_pubs*5,
                                        date_range = input$dates,
                                        precise = input$precise)
          
          pattern <- abbr_term_to_pattern(term = input$search_term,
                                          derivatives = input$derivatives)
          
          #Split pubs_list on the "searchable" (open) and unsearchable (closed) papers
          open_pubs_list <- pubs_list %>%
            dplyr::filter(isOpenAccess == "Y" &
                            not_na(pmcid)) 
          if (nrow(open_pubs_list) > input$max_pubs & input$max_pubs != 0){
            open_pubs_list <- open_pubs_list[1:input$max_pubs, ]
          }
          
          closed_pubs_list <- pubs_list %>%
            dplyr::filter(isOpenAccess == "N" | is.na(pmcid))
          
          abbrs_vec <- c()
          
          shinyWidgets::updateProgressBar(
            session = session,
            id = "myprogress",
            title = sprintf("Work in progress (total %s papers to analyze)",
                            nrow(open_pubs_list)),
            value = 0,
            total = nrow(open_pubs_list)
          )
          
          abbrs_vec <- purrr::map2(
            open_pubs_list$pmcid,
            seq_along(open_pubs_list$pmcid),
            function(x, y){
              
              shinyWidgets::updateProgressBar(
                session = session,
                id = "myprogress",
                value = y,
                total = nrow(open_pubs_list)
              )
              
              paper <- attempt::attempt({
                europepmc::epmc_ftxt(ext_id = x)
              })
              
              if(attempt::is_try_error(paper)){
                return(NULL)
              } else {
                abbrs_vec <- append(abbrs_vec,
                                    abbr_extract_pattern_from_paper(x = paper,
                                                                    pattern = pattern,
                                                                    derivatives = input$derivatives)
                )
                return(abbrs_vec)
              }
              
            })
          
          abbrs_vec <- unlist(abbrs_vec)
          
          if(length(abbrs_vec) == 0){
            abbrs_true <- "Nothing found"
            abbrs_false <- "Nothing found"
          } else {
            
            abbrs_vec <- remove_tags(abbrs_vec)
            abbrs_tbl <- abbr_split_term_and_abbr(abbrs_vec)
            
            ## Clean abbreviations
            
            abbrs_tbl$abbr <- purrr::map_chr(~ stringr::str_replace_all(string = .x,
                                                                        pattern = "^(\\+|-|hereafter)\\s?",
                                                                        replacement = ""))
            
            abbrs_tbl$abbr <- purrr::map_chr(~ stringr::str_replace_all(string = .x,
                                                                        pattern = '^"(.*)"$',
                                                                        replacement = "\\1"))
            
            abbrs_tbl$abbr_pattern <- purrr::map_chr(abbrs_tbl$abbr, abbr_abbreviation_to_pattern)
            browser()
            abbrs_tbl$is_abbr <- purrr::map2_lgl(
              .x = abbrs_tbl$full,
              .y = abbrs_tbl$abbr_pattern,
              .f = ~ stringr::str_detect(
                string = .x,
                pattern = stringr::regex(.y,
                                         ignore_case = TRUE)) &
                # This necessary to exclude single characters
                # > 3 because nchar("\\b") = 2 and pattern of 
                # single character is "\\b_"
                nchar(.y) > 3)
            
            abbrs_true <- abbrs_tbl %>%
              dplyr::filter(is_abbr == TRUE) %>%
              dplyr::select(abbr) %>%
              dplyr::mutate(abbr = stringr::str_to_lower(abbr)) %>%
              unique()
            
            
            abbrs_false <- abbrs_tbl %>%
              dplyr::filter(is_abbr != TRUE) %>%
              dplyr::select(abbr) %>%
              dplyr::mutate(abbr = stringr::str_to_lower(abbr)) %>%
              unique()
            
          }
          rv$analysis_results <- list(
            true_abbr = abbrs_true,
            false_abbr = abbrs_false,
            closed_papers = closed_pubs_list
          )
          shinyWidgets::closeSweetAlert(session = session)
          return(rv$analysis_results)
        }}
    })
    
    return(reactive(rv$analysis_results))
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
