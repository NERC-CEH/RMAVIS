floristicTablesUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 6,
      
      shiny::h5("Composed Floristic Table"),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("floristicTables_composed"))
      )
      
    ),
    shiny::column(
      width = 6,
      
      shiny::h5("NVC Floristic Table"),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("floristicTables_nvc"))
      )
      
    )
  )
  
}