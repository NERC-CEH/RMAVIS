mvaNationalRefUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
        
      shiny::div(
        
        shiny::h5("National Reference"),
        
        plotly::plotlyOutput(outputId = ns("mvaNationalRefPlot"), height = "600px")
        
      )
    )
  )
}
