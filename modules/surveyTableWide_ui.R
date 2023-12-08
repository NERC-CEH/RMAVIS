surveyTableWideUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(
        # rhandsontable::rHandsontableOutput(outputId = ns("surveyTableWide"))
        #, style = "margin-right: 5px !important;"
      )
    )
  )
  
}
