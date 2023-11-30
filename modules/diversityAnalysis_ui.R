diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Diversity Metrics - by ID & Quadrat"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("metricsTableIDQuad"))
        #, style = "margin-right: 5px !important;"
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Diversity Metrics - by ID"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("metricsTableID"))
        #, style = "margin-right: 5px !important;"
      )
      
    )
  )
  
}