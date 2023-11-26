calcAvgEIVsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Average Hill-Ellenberg Values"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("avgEIVsTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}