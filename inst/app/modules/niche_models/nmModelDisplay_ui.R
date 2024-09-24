nmModelDisplayUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 6,
      
      shiny::div(
        
        shiny::h5("Feature Importance"),
        
        plotly::plotlyOutput(outputId = ns("feature_importance_plot"), height = "600px")
        
      )
      
    )
  )
}