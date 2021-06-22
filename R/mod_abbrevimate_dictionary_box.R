#' abbrevimate_dictionary_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_abbrevimate_dictionary_box_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    title = "Abbreviation dictionary",
    width = 4,
    collapsible = TRUE,
    icon = shiny::icon("book-medical"),
    
    htmlOutput(outputId = ns("bttns_lib")),
    br(),
    br(),
    
    shinyWidgets::searchInput(
      inputId = ns("manual_add"), 
      placeholder = "Add abbreviation manually",
      btnSearch = shiny::icon("plus"), 
      btnReset = shiny::icon("remove"),
      width = "100%"
    ),
    
    br(),
    br(),
    
    DT::dataTableOutput(outputId = ns("abb_dic"))
  )
}

#' abbrevimate_dictionary_box Server Functions
#'
#' @noRd 

mod_abbrevimate_dictionary_box_server <- function(id, pos, neg){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues()
    
    output$bttns_lib <- renderUI({
      tagList(
        div(
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("remove_selected"),
              label = shiny::icon("minus"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Remove selected from the library",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("clear_all"),
              label = shiny::icon("remove"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Clear library",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::downloadBttn(
              outputId = ns("download_lib"),
              label = "",
              style = "material-flat",
              size = "sm"
            ),
            title = "Download library",
            placement = "bottom"
          ),
          
          style="float:right"
        )
      )
    })
    
    
    
    
    output$abb_dic <- DT::renderDataTable({
      
      if(not_null(pos()) | not_null(neg())){
        
        rv$dictionary <- unique(dplyr::bind_rows(rv$dictionary, pos(), neg()))
        
      }
      
      if(not_null(rv$dictionary)){
        
        results_tbl <- rv$dictionary
        colnames(results_tbl) <- "Results"
        
      } else {
        results_tbl <- tibble::tibble(Results = "Nothing to show")
      }
      
      DT::datatable(
        data = results_tbl,
        rownames = FALSE,
        selection = list(
          mode = "multiple",
          selected = NULL,
          target = "row",
          selectable = TRUE
        )
      )
      
    })
    
    observeEvent(input$manual_add_search, {
      
      rv$dictionary <- dplyr::bind_rows(rv$dictionary, 
                                        tibble::tibble(abbr = input$manual_add))
      
      rv$dictionary <- unique(rv$dictionary)
      
      
    })
    
    
    observeEvent(input$remove_selected, {
      if(not_null(input$abb_dic_rows_selected)){
        rv$dictionary <- rv$dictionary[-c(input$abb_dic_rows_selected),]
      }
    })
    
    observeEvent(input$clear_all, {
      rv$dictionary <- NULL
    })
    
    output$download_lib <- downloadHandler(
      filename = function() {
        paste('dictionary_', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(rv$dictionary, file, row.names = FALSE)
      }
    )
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 

