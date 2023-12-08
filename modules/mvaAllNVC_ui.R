mvaAllNVCUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,

      shiny::div(
        
        shiny::h5("Global Reference"),
        
        plotly::plotlyOutput(outputId = ns("mvaAllNVCPlot"), height = "600px")
        
      )

    )
  )
}
