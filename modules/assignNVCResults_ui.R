assignNVCResultsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Results: Pseudo-Quadrat Similarities"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("resultsTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}