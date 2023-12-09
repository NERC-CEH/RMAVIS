floristicTablesUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 6,
      
      shiny::h5("Composed Floristic Table"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("floristicTables_composed"))
        #, style = "margin-right: 5px !important;"
      )
      
    ),
    shiny::column(
      width = 6,
      
      shiny::h5("NVC Floristic Table"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("floristicTables_nvc"))
        #, style = "margin-right: 5px !important;"
      )
      
    )
  )
  
}