ordinationAnalysisUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Detrended Correspondence Analysis"),
      
      shiny::div(
        plotly::plotlyOutput(outputId = ns("dcaPSvsSamples"))
        # shiny::plotOutput(outputId = ns("dcaPSvsSamples"))
      )
      
    )
  )
  
}