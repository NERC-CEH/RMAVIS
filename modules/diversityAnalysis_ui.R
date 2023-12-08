diversityAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Alpha Diversity"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("alphaDiversityQuadratTable"))
      ),
      
      shiny::h5("Beta Diversity"),
      
      shiny::h5("Gamma Diversity"),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("gammaDiversitySiteTable"))
      )
      
    )
  )
  
}