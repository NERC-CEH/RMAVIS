calcAvgEIVsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Cover-weighted Mean Hill-Ellenberg Values"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("avgWeightedEIVsTable"))
        #, style = "margin-right: 5px !important;"
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Mean Hill-Ellenberg Values"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("avgUnweightedEIVsTable"))
        #, style = "margin-right: 5px !important;"
      )
      
    )
  )
  
}