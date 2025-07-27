nvcFlorTabsUI <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("floristicTablesTable"))
        # reactable.extras::reactable_extras_ui(id = ns("floristicTablesTable"))
      )
      
    )
  )
  
}
