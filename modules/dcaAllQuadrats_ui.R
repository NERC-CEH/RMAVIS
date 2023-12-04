dcaAllQuadratsUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::div(
        
        shiny::h5("All Quadrats"),
        
        plotly::plotlyOutput(outputId = ns("dcaAllQuadratsPlot"), height = "600px")
        
      )
      
    )
  )
}