#' abbrevimate_pos_hits_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_pos_hits_box_ui <- function(id){
  ns <- NS(id)
  
  pos_hits_box <- tagList(
    shinydashboardPlus::box(
      title = "Positive hits",
      width = 4,
      collapsible = TRUE,
      icon = shiny::icon("folder-plus"),
      
      htmlOutput(outputId = ns("bttns_pos_hits")),
      
      br(),
      br(),
      
      DT::dataTableOutput(outputId = ns("pos_hits"))
    )
  )
  
  return(pos_hits_box)
}

#' abbrevimate_pos_hits_box Server Functions
#'
#' @noRd 
mod_abbrevimate_pos_hits_box_server <- function(id, hits){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues()
    
    output$bttns_pos_hits <- renderUI({
      tagList(
        div(
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("select_all"),
              label = shiny::icon("check"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Select all",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("deselect_all"),
              label = shiny::icon("remove"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Clear selection",
            placement = "bottom"
          ),
          
          shinyBS::tipify(
            shinyWidgets::actionBttn(
              inputId = ns("add_to_lib"),
              label = shiny::icon("book-medical"),
              style = "material-flat",
              size = "sm"
            ),
            title = "Add selected to the library",
            placement = "bottom"
          ),
          
          style="float:right"
        )
      )
    })
    
    
    output$pos_hits <- DT::renderDataTable({
      
      if(not_null(hits()$true_abbr)){
        
        if(is.vector(hits()$true_abbr)){
          
          results_tbl <- tibble::tibble(Results = "Nothing found")
          
          selection <- list(
            mode = "none"
          )
          
        } else if(is.data.frame(hits()$true_abbr) & nrow(hits()$true_abbr) > 0){
          
          results_tbl <- tibble::tibble(Results = hits()$true_abbr)
          selected <- seq_along(results_tbl$abbr)
          selection <- list(
            mode = "multiple",
            selected = selected,
            target = "row",
            selectable = TRUE
          )
          
        } else {
         
          results_tbl <- tibble::tibble(Results = "Nothing found")
          
          selection <- list(
            mode = "none"
          )
          
        }
      } else {
        results_tbl <- tibble::tibble(Results = "Nothing to show")
        
        selection <- list(
          mode = "none"
        )
        
      }
      
      DT::datatable(
        data = results_tbl,
        rownames = FALSE,
        selection = selection
      )
      
    })
    
    #This part is necessary to update table selection
    proxy <- DT::dataTableProxy("pos_hits")
    
    observeEvent(input$select_all, {
      DT::selectRows(proxy = proxy,
                     selected = input$pos_hits_rows_all)
    })
    
    observeEvent(input$deselect_all, {
      DT::selectRows(proxy = proxy,
                     selected = NULL)
    })
    
    #Add to library
    observeEvent(input$add_to_lib, {
      if(not_null(hits())){
        rv$pos_to_dictionaty <- hits()$true_abbr[input$pos_hits_rows_selected,]
        return(rv$pos_to_dictionaty)
      }
    })
    
    return(reactive(rv$pos_to_dictionaty))
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
