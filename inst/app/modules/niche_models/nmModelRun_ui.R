nmModelRunUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    shiny::column(
      
      width = 8,
      
      shiny::div(
        
        shiny::h5("Model Prediction Breakdown"),
        
        plotly::plotlyOutput(outputId = ns("modelPredBreakdown_plot"), height = "75vh")
        
      )
      
    )
    
  )
  
}