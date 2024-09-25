nmModelDisplayUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    shiny::column(
      
      width = 7,
      
      shiny::div(
        
        shiny::h5("Performance Metrics"),
        
        reactable::reactableOutput(outputId = ns("modelEvalMetricsTable"))
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        shiny::h5("Feature Importance"),
        
        plotly::plotlyOutput(outputId = ns("feature_importance_plot"), height = "50vh")
        
      )
      
    ),
    
    shiny::column(
      
      width = 5,
      
      shiny::div(
        
        shiny::h5("Accumulated Local Effects"),
        
        plotly::plotlyOutput(outputId = ns("ale_plot"), height = "75vh")
        
      )
      
    )
    
  )
  
}