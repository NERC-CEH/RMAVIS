resultsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("resultsTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}