surveyTableUI <- function(id) {
  ns <- NS(id)

# Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("surveyTable"))
      )
    )
  )
  
}
