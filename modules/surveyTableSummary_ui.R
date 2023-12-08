surveyTableSummaryUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Survey Table Check"),
      
      shiny::h5("Survey Table Summary"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("surveySampleSummaryTable"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}