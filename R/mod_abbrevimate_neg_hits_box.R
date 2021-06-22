#' abbrevimate_neg_hits_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_abbrevimate_neg_hits_box_ui <- function(id){
  ns <- NS(id)
  
  neg_hits_box <- tagList(
    shinydashboardPlus::box(
      title = "Negative hits",
      width = 4,
      collapsible = TRUE,
      icon = shiny::icon("folder-minus"),
      
      htmlOutput(outputId = ns("bttns_neg_hits")),
      
      br(),
      br(),
      
      DT::dataTableOutput(outputId = ns("neg_hits"))
    )
  )
  return(neg_hits_box)
}

#' abbrevimate_neg_hits_box Server Functions
#'
#' @noRd 
mod_abbrevimate_neg_hits_box_server <- function(id, hits){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues()
    
    output$bttns_neg_hits <- renderUI({
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
    
    output$neg_hits <- DT::renderDataTable({
      
      if(not_null(hits()$false_abbr)){
        #nrow() part necessary because sometimes empty tibble appears in hits()
        if(hits()$false_abbr != "Nothing found" & nrow(hits()$false_abbr) > 0){
          results_tbl <- tibble::tibble(Results = hits()$false_abbr)
        } else {
          results_tbl <- tibble::tibble(Results = "Nothing found")
          selected <- NULL
        }
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
    
    #This part is necessary to update table selection
    proxy <- DT::dataTableProxy("neg_hits")
    
    observeEvent(input$select_all, {
      DT::selectRows(proxy = proxy,
                     selected = input$neg_hits_rows_all)
    })
    
    observeEvent(input$deselect_all, {
      DT::selectRows(proxy = proxy,
                     selected = NULL)
    })
    
    #Add to library
    observeEvent(input$add_to_lib, {

      if(not_null(hits()$false_abbr)){
        rv$neg_to_dictionaty <- hits()$false_abbr[input$neg_hits_rows_selected,]
        return(rv$neg_to_dictionaty)
      }
    })
    
    return(reactive(rv$neg_to_dictionaty))
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
