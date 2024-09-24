nmDataInputUI <- function(id) {
  ns <- NS(id)

# Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Variable Input Table"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("nmDataInput"))
      )
      
    )
  )
  
}
